     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller)
     H*****************************************************************
     H/TITLE W7417R - CREATE WCPOLS FOR ELECTRONIC TRANSFER TO NCCI
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*   NONE                                                        *
     H*                                                               *
     H* PROGRAMMER:
     H*                                                               *
     H* INDICATORS:                                                   *
     H*   50  GENERAL CHAIN INDICATOR                                 *
     H*   60  GENERAL CHAIN INDICATOR                                 *
     H*   LR  LAST RECORD INDICATOR                                   *
     a*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/19/25 C:333 Support: Phw#119374 (Amwins)                   *
     H*                WCPOLS/WCSTAT - 0277 WCRIBMA ARAP Premium not  *
     H*                 reported FW: MA ARAP - USR                    *
     H*                - Adding ARAP factor to record type 4 for MA.  *
     H*                                                               *
     H* 08/11/25 C:65  Project: Phw#119519 (Vital)                    *
     H*                Change the multiplication of exposure rate to  *
     H*                use 10 instead of 100 since data changed       *
     H*                because of WC2952 having 3 decimal places.     *
     H*                                                               *
     H* 07/28/25 C:65  Project: Phw#113872 (AmWins)                   *
     H*                Populate change dates on exposure records if   *
     H*                the payroll amount changes.                    *
     H*                                                               *
     H* 07/23/25 C:300 Project: 148WFQ (AmWins)                       *
     H*                Add Large Deductible Form WC990660VAA.         *
     H*                                                               *
     H* 07/01/25 C:65  Project: 119112 (Sunz)                         *
     H*                When generating new policy data for MTEs, only *
     H*                generate the data once - set all bureau fields *
     H*                on the endorsement and policy records.  This   *
     H*                will speed up processing of extracts.          *
     H*                                                               *
     H* 06/24/25 C:65  Project: 119254 (Vital)                        *
     H*                Clear the industry code (SIC) field on the     *
     H*                carrier and agent address records.             *
     H*                                                               *
     H* 04/21/25 C:65  Project: 118868 (AmWins)                       *
     H*                If a policy reports a cancel/re-write trans    *
     H*                then prevent CX trans on original policy from  *
     H*                generating for NCCI submission files.          *
     H*                                                               *
     H* 03/25/25 C:333 Project: 148WFL                                *
     H*                - SN WC990660IL Endorsement                    *
     H*                                                               *
     H* 02/03/25 C:65  Support: 118187 (Sunz)                         *
     H*                Adjust split standard premium since prorated   *
     H*                amounts are now being reported.                *
     H*                                                               *
     H* 01/15/25 C:300 Support: 118142 (SUNZ)                         *
     H*                WCPOLS-WC370401 should not be reported in record44
     H*                                                               *
     H* 12/18/24 C:65  Project: 118197 (Prescient)                    *
     H*                Allow the WXNHP records to update the NHSADD   *
     H*                field to reflect when a state CX has been      *
     H*                generated.  This will prevent the state CX     *
     H*                from constantly generating and allow policy    *
     H*                transactions to generate.  Generate submission *
     H*                detail record to support this as well.         *
     H*                                                               *
     H* 12/17/24 C:65  Project: 118197 (Prescient)                    *
     H*                Zero out change dates only when not currently  *
     H*                deleting a name or address record for CA. If a *
     H*                name or address is no longer deleted then      *
     H*                populate the new change dates as well.         *
     H*                                                               *
     H* 11/26/24 C:291 Project: 148WFA (AmWins)                       *
     H*                Add State National forms WC990660OK and        *
     H*                WC990661OK.                                    *
     H*                                                               *
     H* 10/31/24 C:65  Project: 117940 (AmWins)                       *
     H*                Zero out change dates for new/renewal trans    *
     H*                for CA in case fields are populated in WXNHP.  *
     H*                                                               *
     H* 09/09/24 C:340 Project: 148WFG (AmWins)                       *
     H*                Add Large Deductible Form WC990660VA.          *
     H*                                                               *
     H* 09/06/24 C:340 Project: 148WFB (AmWins)                       *
     H*                Add Large Deductible Form WC990660TX,          *
     H*                WC990660GA, WC990660AZ, WC990660MA, WC990660TN,*
     H*                WC990660KY, WC990660AR, WC990660ME, WC990660OR,*
     H*                WC990660RI and WC990660AK.                     *
     H*                                                               *
     H* 08/28/24 C:340 Project: 150WGA (Sunz)                         *
     H*                WC995410: XML and Type 43 Rcds.                *
     H*                                                               *
     H* 08/22/24 C:65  Project: 117380 (AmWins)                       *
     H*                Update to Phw#114934 to check on modifier date *
     H*                instead of modifier for split state premium    *
     H*                records to cover unrated splits - linked to    *
     H*                WC2918 project which allows for more than 1st  *
     H*                modifier to be unrated.                        *
     H*                                                               *
     H* 08/13/24 C:65  Project: 148WEZ (AmWins)                       *
     H*                Change the subroutine writing type 43 records  *
     H*                to report WC99* forms under carrier version ID *
     H*                for Amwins.                                    *
     H*                                                               *
     H* 08/09/24 C:65  Project: WC2978                                *
     H*                Correct seq# for DK record type so it          *
     H*                increments for multiple records.               *
     H*                                                               *
     H* 07/30/24 C:340 Project: 148WEX (AmWins)                       *
     H*                Add SN Large Deductible Form WC990635FL.       *
     H*                                                               *
     H* 07/29/24 C:340 Project: 148WEZ (AmWins)                       *
     H*                Add SN Large Deductible Form WC990003 and      *
     H*                WC990003NY.                                    *
     H*                                                               *
     H* 07/25/24 C:333 Project: WC2978                                *
     H*                WCPOLS WC040314B, WC040366, WC040602B.         *
     H*                                                               *
     H* 07/23/24 C:300 Support: 115864 (AmWins)                       *
     H*                WCPOLS - NJ RT AH question on mapping Project  *
     H*                148WEM - Large Deductible                      *
     H*                1) Fields 8-10 correspond to form WC290601     *
     H*                   Fields 11-14 correspond to form WC290605    *
     H*                2) Remove AH record for WC290604 form          *
     H*                                                               *
     H* 07/16/24 C:340 Project: 148WEW (AmWins)                       *
     H*                Add SN Large Deductible Form WC990660.         *
     H*                                                               *
     H* 06/07/24 C:65  Project: Phw#115720 (Amwins)                   *
     H*                If an endorsement is set up with same name in  *
     H*                state/national form # field then report it     *
     H*                under the carrier version ID field only.       *
     H*                                                               *
     H* 05/29/24 C300  Support: 115512 (LUBA) Project: WC2951         *
     H*                Report the merit rating status                 *
     H*                                                               *
     H* 05/28/24 C:300 Support: 114990 (AmWins)                       *
     H*                CA WCPOLS - new TXNS showing policy change     *
     H*                effective date (P05EFD) not equal to 000000    *
     H*                                                               *
     H* 04/17/24 C:300 Support: 113848 (AmWins)                       *
     H*                WCPOLS DB not produced when form WC040302D     *
     H*                attached (QA) T30230633                        *
     H*                Correct typo from WC2608 changes               *
     H*                                                               *
     H* 04/10/24 C:300 Support: 114934 (AmWins)                       *
     H*                WCPOLS - CA split mod reporting RT04           *
     H*                Correctly show the effective and expiration    *
     H*                dates of each period that the mod is effective *
     H*                                                               *
     H* 04/02/24 C:344 Project: Phw#115270 (Sunz)                     *
     H*                Remove extra spaces from CRNAM1 and CRNAM2     *
     H*                when concatening both field during the WCPOLS  *
     H*                extract for cancellation and reinstatement     *
     H*                executions record type 08.                     *
     H*                                                               *
     H* 03/08/24 C:65  Project: Phw#114908 (Sunz)                     *
     H*                Change UE# reporting to only report multiple   *
     H*                UE# values for NCCI.  Independent bureaus      *
     H*                will receive single UE# value from that state. *
     H*                                                               *
     H* 03/04/24 C:079 Project: WC2951                                *
     H*                Use new Experience Rating field                *
     H*                                                               *
     H* 02/14/24 C:333 Project: WC2957                                *
     H*                Suppress WC99* Forms for WCPOLS by State.      *
     H*                                                               *
     H* 01/16/24 C:65  Project: Phw#114898 (Sunz)                     *
     H*                Zero out the standard premium in state premium *
     H*                record for Sunz when standard is negative.     *
     H*                                                               *
     H* 01/15/24 C:344 Project: WC2937                                *
     H*                Update WCPOLS extract (W7417R) to populate the *
     H*                first 42 characters of the email address into  *
     H*                P03MAIL & the remainder into the P03FU3 field. *
     H*                                                               *
     H* 01/02/24 C:342 Project: WC2938                                *
     H*                WC2938 - WCPOLS ED Record Type                 *
     H*                                                               *
     H* 12/12/23 C:65  Project: Phw#114739 (Sunz)                     *
     H*                Clear policy transaction record formats        *
     H*                between each policy transaction.               *
     H*                                                               *
     H* 12/11/23 C:65  Project: Phw#114652 (Sunz)                     *
     H*                Populate a 'N' value for foreign address indi- *
     H*                cator for type 1 addresses on non MI/NCCI/NJ   *
     H*                data submission files.                         *
     H*                                                               *
     H* 09/21/23 C:344 Project: WC2923                                *
     H*                Add EJ type for Endorsement WC480323           *
     H*                                                               *
     H* 08/29/23 C:65  Support: 108955 (AmWins)                       *
     H*                Clear out first address line if it has a value *
     H*                of <No Specific Address> to properly report    *
     H*                when a client has entered no address.          *
     H*                                                               *
     H* 08/11/23 C:65  Project: Phw#113186 (Sunz)                     *
     H*                Check eff/exp dates on the approved/excluded   *
     H*                states on endorsements when determining if     *
     H*                a national endorsement needs to report for     *
     H*                a subset of the policy's states.               *
     H*                                                               *
     H* 07/30/23 C:62  Project: WC2918                                *
     H*                Apply non rating logic for exp mod 2 and 3     *
     H*                                                               *
     H* 05/01/23 C:65  Project: Phw#107632 (Amwins)                   *
     H*                Remove unwanted characters from waiver data    *
     H*                as these are populated from text areas.        *
     H*                                                               *
     H* 03/20/23 C:65  Project: Phw#107627 (Amwins)                   *
     H*                Remove 150WAD change to include cx/re trans-   *
     H*                actions from header count in NJ submissions.   *
     H*                                                               *
     H* 03/10/23 C:65  Project: Phw#107576 (Sunz)                     *
     H*                Restrict WC2841 to Amwins only.                *
     H*                                                               *
     H* 02/27/23 C:291 Project: WC2897                                *
     H*                -Electronically report WC150603                *
     H*                                                               *
     H* 01/30/23 C:65  Project: Phw#105980 (Sunz)                     *
     H*                For MI submission files remove DBA from name   *
     H*                records due to a specific policy that has it   *
     H*                in the name for Sunz.                          *
     H*                                                               *
     H* 01/23/23 C:344 Project: Phw#105589 (Luba)                     *
     H*                Write a type 43 rec for WC350602D endorsement. *
     H*                                                               *
     H* 12/12/22 C:65  Project: 153WAC                                *
     H*                Report WC991011* under carrier version ID for  *
     H*                Vital Services.  This applies to both the type *
     H*                7 endorsement rcd and the type 43 deduc rcd.   *
     H*                                                               *
     H* 12/05/22 C:291 Project: C153WCA                               *
     H*                Generare Type 43 records for Vital Services    *
     H*                Large Deductible endorsements.                 *
     H*                                                               *
     H* 11/15/22 C:291 Project: WC2891                                *
     H*                Report type 13 transactions (Policy            *
     H*                Period Endorsement).                           *
     H*                                                               *
     H* 11/04/22 C:65  Project: Phw#104100 (Sunz)                     *
     H*                Change the checks for transaction 15 and       *
     H*                creating a state CX record to find the index   *
     H*                using the current state array.  There is no    *
     H*                direct correlation between the states in the   *
     H*                prior array compared to current array.         *
     H*                                                               *
     H* 10/25/22 C:344 Project: 103957                                *
     H*                Add EI type for Endorsement WC480322           *
     H*                                                               *
     H* 10/14/22 C:65  Project: Phw#103927WC2814 (Sunz)               *
     H*                Add electronic reporting to WC480314A and      *
     H*                WC480315A endorsements.                        *
     H*                                                               *
     H* 09/21/22 C:291 Project: 150WFT                                *
     H*                Add WC990403 and WC992003                      *
     H*                                                               *
     H* 07/06/22 C:079 Project: WC2870                                *
     H*                -Electronically report WC290605                *
     H*                                                               *
     H* 05/24/22 C:062 Project: WC2862                                *
     H*                -Use EMrsk# to populate P01IRI instead of      *
     H*                 P04IR#.                                       *
     H*                -Populate P04IR# with value from EDI           *
     H*                                                               *
     H* 03/11/22 C:340 Project: WC2843                                *
     H*                Use 2 New Fields & popolate on ETR records     *
     H*                                                               *
     H* 05/09/22 C:65  Project: 148WCY    (Amwins)                    *
     H*                Update to report endorsements using optional   *
     H*                form number field as MUFORM may not be the     *
     H*                actual form needed.                            *
     H*                                                               *
     H* 02/17/22 C:62  Project: WC2846                                *
     H*                Electronic Report for WC470602                 *
     H*                                                               *
     H* 02/09/22 C:62  Project: 150WFQ                                *
     H*                Add WC995203 and WC990678                      *
     H*                                                               *
     H* 02/04/22 C:65  Project: WC2841                                *
     H*                Turn on indicator 66 for CA 06 transactions    *
     H*                to populate the "original" policy fields.      *
     H*                                                               *
     H* 01/31/22 C:65  Project: Phw#100685 (Amwins)                   *
     H*                Move the cleanup of wmmup to end of entire     *
     H*                section for policy as its current location     *
     H*                was getting skipped due to polSkip coding.     *
     H*                                                               *
     H* 12/22/21 C:79  Project: WC2840                                *
     H*                Add email logic                                *
     H*                Fix issue with WC2841 due to data decimal error*
     H*                                                               *
     H* 11/19/21 C:65  Project: WC2841                                *
     H*                For CA policy resubmissions, use transaction   *
     H*                code 06 instead of 14, unless all the events   *
     H*                which triggered the resubmission are mid-term  *
     H*                endorsements which generated a WC890600 form.  *
     H*                Reset req# after processing policy record.     *
     H*                                                               *
     H* 11/16/21 C:65  Project: 148WCY    (Amwins)                    *
     H*                Update to trigger electronic reporting records *
     H*                for Amwins specific versions of WC000406.      *
     H*                                                               *
     H* 09/16/21 C:65  Project: 148WDC    (Amwins)                    *
     H*                Report carrier version dec page/schedules as   *
     H*                such on endorsement record when a national/    *
     H*                state form is indicated on template setup.     *
     H*                                                               *
     H* 06/18/21 C:65  Project: WC2815                                *
     H*                Prevent DD and DE records from reporting.      *
     H*                                                               *
     H* 05/25/21 C:65  Project: Phw#96028 (Amwins) - WC2592           *
     H*                Update the WC2592 coding to allow endorsements *
     H*                to be reported only on states they apply to    *
     H*                whether they are set up on an excluded or      *
     H*                an approved basis.                             *
     H*                                                               *
     H* 04/08/21 C:65  Project: Phw#95285 (Sunz)                      *
     H*                Trigger electronic records for any WC990603*   *
     H*                deductible endorsement for Sunz.               *
     H*                                                               *
     H* 03/16/21 C:65  Project: Phw#93467 (Sunz)                      *
     H*                Trigger electronic records for three TX deduc  *
     H*                endorsements.                                  *
     H*                                                               *
     H* 03/18/21 C:65  Project: Phw#94990 (Amwins)                    *
     H*                If processing a state CX record for a policy   *
     H*                to an independent bureau then prevent the      *
     H*                policy transaction from sending in same file.  *
     H*                The transaction will still be updated as if it *
     H*                had been sent.                                 *
     H*                                                               *
     H* 02/17/21 C:062 Project: WC2761                                *
     H*                Add report.                                    *
     H*                                                               *
     H* 11/18/20 C:180 Project WC2757                                 *
     H*                Recompiled only.                               *
     H*                                                               *
     H* 10/23/20 C:180 Project: WC2752                                *
     H*                Add endorsement WC480314.  When reporting a    *
     H*                master policy to WI, prevent all client names  *
     H*                and addresses from reporting to WI as name/    *
     H*                address records.  Instead they are reported    *
     H*                under the above endorsement.                   *
     H*                                                               *
     H* 09/24/20 C:062 Project: 150WEW                                *
     H*                -Remove the following endorsements             *
     H*                 WC990616                                      *
     H*                 WC990617                                      *
     H*                 WC990704                                      *
     H*                 WC990704                                      *
     H*                -Add the following endorsements                *
     H*                 WC994103A                                     *
     H*                 WC995402                                      *
     H*                                                               *
     H* 09/15/20 C:65  Project: Phw#90900 (LUBA)                      *
     H*                When checking for prior submission entity code *
     H*                for prior deleted address status, pull it from *
     H*                a wxnap record with name populated in case the *
     H*                address is on a record with a blanked name.    *
     H*                Add name link counter to SRDEL coding as well  *
     H*                in case policy has more than 1000 names.       *
     H*                                                               *
     H* 08/27/20 C:136 Project: WC2608 (not commented)                *
     H*                Add WC040302D and WC040303C to electronic      *
     H*                reporting logic                                *
     H*                                                               *
     H* 07/02/20 C:65  Project: Phw#89815 (Sunz)                      *
     H*                Flag WXNHP record with a 'D'/'P' when flag is  *
     H*                set to generate state CX record to ensure only *
     H*                one state CX is generated.                     *
     H*                                                               *
     H* 06/16/20 C:65  Support: 150-88137                             *
     H*                Add logic for form WC200304D.                  *
     H*                                                               *
     H* 05/12/20 C:65  Project: Phw#88982 (Sunz)                      *
     H*                Override WMMULP/L7 within the program here     *
     H*                after policy page resubmissions are complete.  *
     H*                                                               *
     H* 05/07/20 C:65  Project: Phw#88982 (Sunz)                      *
     H*                Clear out the endorsement table fields when    *
     H*                a record is not found so the prior form's      *
     H*                values are not used.                           *
     H*                                                               *
     H* 05/01/20 C:65  Project: Phw#88889 (LUBA)                      *
     H*                Subtract out standard premium from the policy  *
     H*                header total for states that are deleting or   *
     H*                have previously reported as deleted.           *
     H*                                                               *
     H* 04/01/20 C:136 Project: 150WEI (88039)                        *
     H*                Add more LD forms for Sunz to electronically   *
     H*                report.                                        *
     H*                                                               *
     H* 03/31/20 C:65  Project: 150WAD/Phw#70357                      *
     H*                Update cancellation type code on state based   *
     H*                CX transaction.                                *
     H*                                                               *
     H* 11/26/19 C:79  Support: 150-85231                             *
     H*                Add logic for form WC200304C                   *
     H*                                                               *
     H* 11/21/19 C:65  Project: 150WAD/Phw#85139                      *
     H*                Populate nature of business on state cx rcds   *
     H*                as was done with regular records.              *
     H*                                                               *
     H* 11/11/19 C:267 Project: WC2695                                *
     H*                Write endorsement data record for WC370604     *
     H*                PA Employer Assessment Endorsement form.       *
     H*                                                               *
     H* 10/03/19 C:65  Project: 150WAD/Phw#82826 (SUNZ)               *
     H*                Along with change in W7413R to remove bureaus  *
     H*                that no longer belong on a policy during a     *
     H*                resubmission policy data set run (83127),      *
     H*                remove any bureaus not found on policy record  *
     H*                from endorsement transaction record.  This     *
     H*                will ensure they do not get orphaned and never *
     H*                removed/processed.                             *
     H*                                                               *
     H* 08/27/19 C:65  Project: 150WAD/Phw#82472 (SUNZ)               *
     H*                Ensure type 07 and 43 records do not report    *
     H*                on a state that has previously deleted and is  *
     H*                no longer being reported.                      *
     H*                                                               *
     H* 08/19/19 C:65  Project: Phw#82227 (Amwins)                    *
     H*                Check employer detail for state premium rcd    *
     H*                exp mod status since most states have a value  *
     H*                of 1 in header record.  Check flag in WXNHP    *
     H*                when setting the save status.                  *
     H*                                                               *
     H* 08/02/19 C:65  Project: 150WAD/Phw#81198 (SUNZ)               *
     H*                Set the experience rating code in header       *
     H*                record based on a division written within      *
     H*                the bureau being reported to.                  *
     H*                                                               *
     H* 07/29/19 C:65  Project: 148WBE                                *
     H*                Add processing for request wxpep for WC000406. *
     H*                                                               *
     H* 07/18/19 C:65  Project: Phw#79807 (CareWest)                  *
     H*                Clear the corresponding cx date for cx rcds.   *
     H*                                                               *
     H* 06/03/19 C:65  Project: Phw# 78844 (LUBA)                     *
     H*                Change coding in SRDEL to ensure name entity   *
     H*                code is taken into account when checking if    *
     H*                address record is previously reported.         *
     H*                                                               *
     H* 05/28/19 C:65  Project: 150WAD/Phw#79757 (SUNZ)               *
     H*                Correct address concatenation for state cx rcd.*
     H*                                                               *
     H* 05/24/19 C:65  Project: 150WAD/Phw# 78844 (LUBA)              *
     H*                Revert change which allowed subsequent report- *
     H*                ing of deleted states/exposure records for     *
     H*                NCCI submissions.  If a state deletes then all *
     H*                records will not continue reporting for NCCI.  *
     H*                                                               *
     H* 04/03/19 C:65  Project: 150WAD/Phw#73594 (SUNZ)               *
     H*                Pull NHDRSK for the P04IRR value for each state*
     H*                header record - so the value is not pulled     *
     H*                from state reporting division only in extract. *
     H*                                                               *
     H* 02/05/19 C:65  Project: 150WAD/Phw#76211                      *
     H*                Populate prorate minimum premium field for WI. *
     H*                                                               *
     H* 02/04/19 C:65  Project: 150WAD/Phw#75449/WC2646               *
     H*                Clear the modifier eff date field in state     *
     H*                premium record to prevent carry over data.     *
     H*                                                               *
     H* 02/01/19 C:291 Project: WC2206                                *
     H*                Report the first modifier is not rated if      *
     H*                ELnoexm1 flag is set to 'Y'.                   *
     H*                                                               *
     H* 01/09/19 C:291 Project: WC2636                                *
     H*                Add missing fields to WCPOLSDO record type.    *
     H*                                                               *
     H* 12/07/18 C:65  Project: 150WAD                                *
     H*                Restrict subsequent reporting of deleted names *
     H*                and addresses to non-NCCI bureaus.             *
     H*                                                               *
     H* 11/30/18 C:267 Project: WC2636                                *
     H*                Add missing fields to WCPOLSDM, WCPOLSDO       *
     H*                output data: name of insured, Endorsement      *
     H*                effective and expiration dates.                *
     H*                                                               *
     H* 10/22/18 C:65  Project: 150WAD/Phw#70091/WC2646               *
     H*                Enable the split rating coding for CA to occur *
     H*                on all non-NCCI submissions.  Add in coding    *
     H*                to ensure premium fields are properly          *
     H*                populated - by splitting the premium amounts   *
     H*                in WDPPP file for modifiers.                   *
     H*                                                               *
     H* 10/11/18 C:291 Project: WC2636                                *
     H*                Add coding to trigger electronic reporting     *
     H*                for WC040342B form.                            *
     H*                                                               *
     H* 10/10/18 C:65  Project: 150WAD/Phw#70357/WC2647               *
     H*                Prevent transaction 15 for state deletion on   *
     H*                states flagged to produce the state CX rcd.    *
     H*                                                               *
     H* 10/03/18 C:65  Project: 133WQQ                                *
     H*                Write endorsement data record for Luba         *
     H*                deductible endorsements.                       *
     H*                                                               *
     H* 09/26/18 C:62  Ticket: 73514(Inhouse)                        *
     H*                When accessing Submission History don't use    *
     H*                division                                       *
     H*                                                               *
     H* 09/18/18 C:267 Project: WC2634                                *
     H*                Move BLANKS to Name of Insured in pos 255 of   *
     H*                WCPOLSDK work file - request per client SUNZ.  *
     H*                                                               *
     H* 09/11/18 C:65  Project: 150WAD/Phw#72882                      *
     H*                Default deductible flags on DR record to N if  *
     H*                they are blank.                                *
     H*                                                               *
     H* 09/06/18 C:291 Project: WC2602                                *
     H*                Modify WrtStateCxRcd procedure to process      *
     H*                state CX Transaction based on new flag in WTXCP*
     H*                                                               *
     H* 08/30/18 C:65  Project: WC2607/150WAD/Phw#72882               *
     H*                Write endorsement data record for more         *
     H*                Sunz deductible endorsements.                  *
     H*                                                               *
     H* 08/21/18 C:267 Project: WC2634                                *
     H*                Add coding to trigger electronic reporting     *
     H*                for WC040315B form.                            *
     H*                                                               *
     H* 07/18/18 C:65  Project: 150WAD                                *
     H*                Add two fields to hold the name and address    *
     H*                effective date.  Independent bureaus will      *
     H*                report the full coverage date range so the     *
     H*                begin date will need to be retained.  NCCI     *
     H*                will continue reporting deletions with the     *
     H*                same date in policy change eff/exp date fields.*
     H*                All bureaus will report deleted data on        *
     H*                subsequent transactions.                       *
     H*                                                               *
     H* 07/06/18 C:62  Ticket: 70603(150WCO)                          *
     H*                Populate carrier version form # into           *
     H*                endorsement record when state/national form    *
     H*                replaces it.                                   *
     H*                                                               *
     H* 06/25/18 C:65  Project: 150WAD                                *
     H*                Move the coding to populate state status info  *
     H*                to prior the coding to write out endorsement   *
     H*                information on state basis when needed.  Do    *
     H*                not consider states prior deleted when         *
     H*                splitting up the endorsement info.             *
     H*                                                               *
     H* 05/31/18 C:65  Project: WC2607/150WAD                         *
     H*                Write endorsement data record for several      *
     H*                Sunz deductible endorsements.                  *
     H*                                                               *
     H* 05/30/18 C:65  Project: WC2607                                *
     H*                Populate additional fields in type 43 record   *
     H*                for deductibles and write type 43 records for  *
     H*                additional endorsements.  Update the NJ record *
     H*                format PAH as well.  Add subroutines for the   *
     H*                BD, CE, and DR record types as well for state  *
     H*                deductible endorsements.                       *
     H*                                                               *
     H* 05/07/18 C:65  Project: 150WAD                                *
     H*                Do not populate 'A' for Add on renewal         *
     H*                transaction.                                   *
     H*                                                               *
     H* 03/26/18 C:65  Project: 150WAD                                *
     H*                Populate a 'C' for type of corporation on BI   *
     H*                type records.                                  *
     H*                                                               *
     H* 03/13/18 C:65  Project: 150WAD                                *
     H*                Populate nature of business on NJ cx/re rcds   *
     H*                with type of business when nature is blank.    *
     H*                                                               *
     H* 03/07/18 C:65  Project: 150WAD                                *
     H*                Ensure key change status is properly saved or  *
     H*                cleared when generating resubmission data.     *
     H*                                                               *
     H* 03/06/18 C:65  Project: 150WAD                                *
     H*                Prevent array error on excluded states array.  *
     H*                                                               *
     H* 03/05/18 C:65  Project: 150WAD                                *
     H*                If submitting a file to NJ then include cx and *
     H*                rein transactions in total header count.       *
     H*                                                               *
     H* 03/02/18 C:65  Project: 150WAD                                *
     H*                Use 1st position in SWCPF/WCSC04 to indicate   *
     H*                if the CA exclusion of privacy notices listing *
     H*                on endorsement records should be overridden.   *
     H*                                                               *
     H* 02/26/18 C:65  Project: 150WAD                                *
     H*                Load name record legal nature from table.      *
     H*                                                               *
     H* 02/23/18 C:65  Project: 150WAD/WC2592                         *
     H*                WMMUL7 is overriden to QTEMP to allow          *
     H*                endorsements to be copied over and rebuilt     *
     H*                before reporting on type 07 records.  The      *
     H*                rebuilding is to report national forms on      *
     H*                every state record when a national form is     *
     H*                excluded from at least one state on the policy.*
     H*                                                               *
     H* 02/20/18 C:291 Project: WC2521                                *
     H*                - Add Type of Address 6 for Non-Specific       *
     H*                  Address.                                     *
     H*                                                               *
     H* 01/31/18 C:62  Project: 150WCO                                *
     H*                - Exclude forms if marked to exclude           *
     H*                - Use national/state form#                     *
     H*                                                               *
     H* 01/19/18 C:65  Project: 150WAD/WC2587                         *
     H*                If a state is added to a policy after that     *
     H*                policy has been submitted then instead of an   *
     H*                new business transaction report the new state  *
     H*                as an Add state transaction.                   *
     H*                                                               *
     H* 01/15/18 C:136 Project: 150WCJ                                *
     H*                Correct concatenation of the cancellation and  *
     H*                reinstatement names (CRNAM1 || CRNAM2)         *
     H*                                                               *
     H* 01/10/18 C:65  Project: 150WAD/WC2587                         *
     H*                If generating a submission file for WI then    *
     H*                check if policy has multiple legal nature of   *
     H*                entities and if so then report policy under    *
     H*                type 99 for other and concatenate the types    *
     H*                under "other text" field.                      *
     H*                                                               *
     H* 01/05/18 C:65  Project: 150WAD                                *
     H*                Change ANC endorsement records to report       *
     H*                "Estimated Annual Premium" for the basis.      *
     H*                                                               *
     H* 12/26/17 C:267 Project: C150WCI                               *
     H*                Populate P01BSI new field                      *
     H*                                                               *
     H* 11/06/17 C:136 Project: 150WCC                                *
     H*                Don't report endorsement flagged to 'Exclude   *
     H*                from schedule'                                 *
     H*                                                               *
     H* 07/17/17 C:65  PROJECT: 150WAP                                *
     H*                ADD CODING TO RECOGNIZE THE 'C' VALUE FOR PEO. *
     H*                                                               *
     H* 05/08/17 C:180 Project: 93WLH/WC2481                          *
     H*                Add coding to trigger electronic reporting     *
     H*                for WC000424 and WC370401 forms.               *
     H*                                                               *
     H* 05/02/17 C:180 Project: WC2493                                *
     H*                Add coding to trigger electronic reporting     *
     H*                for WC040316B form.                            *
     H*                                                               *
     H* 05/01/17 C:65  Project: Amwins testing                        *
     H*                Do not report CA privacy notices to anyone.    *
     H*                                                               *
     H* 04/24/17 C:65  Project: Amwins testing/Phw#46864              *
     H*                Report increased limits/IL balance to minimum  *
     H*                percentages as factors to WI.                  *
     H*                                                               *
     H* 04/21/17 C:180 Project: WC2488                                *
     H*                Add coding to trigger electronic reporting     *
     H*                for WC480315 form.                             *
     H*                                                               *
     H* 04/12/17 C:65  Project: WC2483                                *
     H*                Add coding to trigger electronic reporting     *
     H*                for WC200304B form.                            *
     H*                                                               *
     H* 03/23/17 C:65  Project: C114WAA/Amwins testing                *
     H*                Correct the rewrite of the endorsement         *
     H*                inclusion coding - it had improper logic.      *
     H*                In addition, only report WC890600B for CA on   *
     H*                non new/renewal policy transactions.           *
     H*                                                               *
     H* 11/03/16 C:302 Support: WC2466                                *
     H*                Add coding to trigger electronic reporting     *
     H*                for WC040302C, WC040303B, and WC040318B forms. *
     H*                                                               *
     H* 10/13/16 C:65  Support: WC2415                                *
     H*                Add coding to trigger electronic reporting     *
     H*                for WC040303A form.                            *
     H*                                                               *
     H* 09/22/16 C:308 Support: PHW43487                              *
     H*                Correct form filters                           *
     H*                                                               *
     H* 09/07/16 C:308 Project: C114WAA                               *
     H*                Do not write records for WI and UWIC group for *
     H*                WC990601, WC990604, WC990605, WC990609A.       *
     H*                                                               *
     H* 08/24/16 C:136 Project: WC2451                                *
     H*                Report CA LLC Exclusion                        *
     H*                (WC040318A)                                    *
     H*                                                               *
     H* 08/15/16 C:65  Project: WC2446                                *
     H*                Updates for CA reporting: resubmission records *
     H*                should only populate the change dates on       *
     H*                records which changed data from the prior      *
     H*                submission.  Write endorsements to new work    *
     H*                file to track when they have changed.  Prevent *
     H*                type 06 records from reporting to CA.          *
     H*                                                               *
     H* 07/22/16 C:136 Project: WC2450                                *
     H*                Report CA Waiver Charge                        *
     H*                (WC040306)                                     *
     H*                                                               *
     H* 07/15/16 C:136 Project: WC2449                                *
     H*                Report CA Voluntary Labor Charge               *
     H*                (WC040305)                                     *
     H*                                                               *
     H* 07/14/16 C:65  Support: Phase#42306 (CareWest)                *
     H*                Split state premium records on both experience *
     H*                modifiers and anniversary rating date for CA   *
     H*                submissions, and populate the exp mod eff      *
     H*                dates into their own field instead of ARD.     *
     H*                                                               *
     H* 06/06/16 C:136 Project: WC2423 Priss: 41800                   *
     H*                Report WC890600B for CA submission             *
     H*                                                               *
     H* 04/27/16 C:306 Project: WC2424                                *
     H*                Experience modifier Not final status flag.     *
     H*                                                               *
     H* 03/17/16 C:65  Support: Phase#40177 (SCS)                     *
     H*                Prevent empty type 07 record from generating   *
     H*                when policy has exactly 11 endorsements.       *
     H*                                                               *
     H* 12/03/15 C:65  Project: 45WKP                                 *
     H*                If processing a policy effective prior to      *
     H*                1/1/2016 for LCTA then use old Carrier #.      *
     H*                                                               *
     H* 11/06/15 C:65  Support: Phase#38230 (CSI)                     *
     H*                Change reporting of assessments to PCRB to     *
     H*                report a decimal value instead of a percentage *
     H*                value.                                         *
     H*                                                               *
     H* 10/23/15 C:300 Support 25640 - Xref 80779                     *
     H*                Reporting of the Credit for the Wisconsin      *
     H*                Contractors Premium Adjustment Program.        *
     H*                                                               *
     H* 08/24/15 C:65  Project: Phw# 36693 (WC2130)                   *
     H*                Prevent exposure records from reporting if the *
     H*                state has been marked as previously deleted.   *
     H*                                                               *
     H* 08/20/15 C:65  Project: Phw# 36693 (WC2130)                   *
     H*                Skip state record creation if the state is     *
     H*                flagged as previously deleted regardless of if *
     H*                the current transaction is add/delete or not.  *
     H*                                                               *
    H* 08/17/15 C:295 Project :WC2332 Priss# 35799                   *
    H*                To handle the existing records with different  *
    H*                data type  as per previous WCPOLS BI Record    *
     H*                                                               *
    H* 08/03/15 C:295 Project :WC2382                                *
    H*                Update WCPOLS Policy Transfer to populate the  *
    H*                corresponding CX date on reinstatement records *
    H*                in WCPOLS reporting for CA.                    *
    H*                                                               *
     H* 06/16/15 C:65  Project: Phase# 114/35364                      *
     H*                Replace the field by field renaming of the     *
     H*                two submission history logicals with prefix    *
     H*                replacement - state/status slot #8 and #9      *
     H*                were missing renames which caused errors       *
     H*                in this support item.                          *
     H*                                                               *
     H* 06/01/15 C:180 Support 28762                                  *
     H*                Fix reporting of endorsement as policy         *
     H*                resubmission.                                  *
     H*                                                               *
    H* 05/25/15 C:295 Project :WC2332                                *
    H*                Delete all references to XXS#1 and XXS#2       *
    H*                Changes for to WCPOLS BI Record                *
    H*                                                               *
     H* 04/16/15 C:180 Support 32046                                  *
     H*                Increase states to be handled from 15 TO 51    *
    H*                                                               *
     H* 04/10/15 C:302 Project: WC2357                                *
     H*                Write a state specific WCPOLS cancellation     *
     H*                record for Wisconsin if that state is deleted  *
     H*                from the policy.                               *
     H*                - Correct error in SRState subroutine          *
     H*                   (Outside the scope of this project).        *
     H*                                                               *
     H* 04/02/15 C:279 Project: WC2356                                *
     H*                Modified to write the CX/Rein transaction for  *
     H*                any CA only (non-multi state) policy to report *
     H*                using an '04' for the state code field instead *
     H*                of '99'.                                       *
     H*                                                               *
     H* 02/27/15 C:65  Project: Phase# 093/30436 (WC2130)             *
     H*                Allow non-rating resubmissions to be converted *
     H*                to add/delete state transactions.              *
     H*                                                               *
     H* 02/26/15 C:180 Support 31837-86995                            *
     H*                Fix risk id for state of PA.                   *
     H*                                                               *
     H* 01/14/15 C:282 Project: WC2334                                *
     H*                Process to read the state code table to        *
     H*                determine if a state code would be reported    *
     H*                if there is no exposure amount. Added the      *
     H*                assessment, domestic, and foreign terrorism    *
     H*                for PA state. Removed the hardcoded assessment *
     H*                and terrorism handling for NJ and NY state.    *
     H*                                                               *
     H* 06/19/14 C:65  Project: Supp# WC114/85854 (WC2130)            *
     H*                Add logic to avoid index issue in add/delete   *
     H*                state premium record coding.                   *
     H*                                                               *
     H* 06/16/14 C:65  Project: 136WGJ                                *
     H*                Populate class suffix field on all records for *
     H*                CA with '00' if no value is already specified. *
     H*                Report a name link identifier for all exposure *
     H*                records as well for CA - default to 001 if no  *
     H*                name link identifier is already specified.     *
     H*                                                               *
     H* 05/19/14 C:65  Project: 136WGJ                                *
     H*                Copy exposure records to new temporary work    *
     H*                file and accumulate if needed for NCCI prior   *
     H*                to writing the exposure records.               *
     H*                                                               *
     H* 05/14/14 C:300 Project: WC2251                                *
     H*                NY Assessment - Reporting when zero            *
     H*                Report an exposure record for New York assessment
     H*                0932 even when there is no exposure amount and *
     H*                reporting to New York in Policy Transfer.      *
     H*                                                               *
     H* 05/08/14 C:279 Project: WC2287                                *
     H*                Modified to include the class suffix field     *
     H*                when writing exposure records for CA payroll   *
     H*                records that have an extension.                *
     H*                                                               *
     H* 05/02/14 C:295 PROJECT: WC2265                                *
     H*                Recompile for changes in the file WCPOLS01     *
     H*                                                               *
     H* 02/27/14 C:65  Project: WC2130 (WA114/84917)                  *
     H*                Correct coding for deletion of state on policy *
     H*                to only invoke if effective and expiration     *
     H*                dates are equal AND not zero.                  *
     H*                                                               *
     H* 01/14/14 C:136 Project: WC2034                                *
     H*                October 2013 Changes                           *
     H*                                                               *
     H* 11/14/13 C:282 Project: WC2233                                *
     H*                Added the code to interpret the new submission *
     H*                code of 'N' and set the transaction code equal *
     H*                to '10' for non-rating changes resubmission.   *
     H*                Include mid-term endorsment forms on the type  *
     H*                07 record for PA/MA/DE once each and only if   *
     H*                it is not the 'MID-YEAR' form.                 *
     H*                                                               *
     H* 10/31/13 C:65  Project: WC2086/Supp# WA147/84011              *
     H*                Populate Reinstatement reason code with 00     *
     H*                for all cancellation records.                  *
     H*                                                               *
     H* 10/23/13 C:65 Project: Supp# WA147/83828                      *
     H*               Expand size of CAP array to ensure nothing is   *
     H*               lost when building the hyphen free policy #.    *
     H*                                                               *
     H* 08/12/13 C:190 Project: WC2130                                *
     H*                Report type 15 transactions (Policy            *
     H*                resubmission with Add/Delete of state).        *
     H*                                                               *
     H* 05/29/13 C:180 Support  WC093 82397                           *
     H*                Assign NAIC instead of SIC for locations       *
     H*                                                               *
     H* 05/15/13 C:180 SUPPORT WA093 82211                            *
     H*                Trans code 14 not valid for WI.                *
     H*                                                               *
     H* 04/22/13 C:180 SUPPORT WC045 82146                            *
     H*                Prevent CPF5032 record lock error.             *
     H*                                                               *
     H*                                                               *
     H* 12/19/12 C:191 Project: WC2126 FMIC reported bug              *
     H*                New file added under this project has same     *
     H*                same fieldnames as existing cancellation file  *
     H*                This is conflicting with logic. Renaming fields*
     H*
     H* 09/06/12 C:282 Project: WC2126 Priss 16                       *
     H*                Change the keylist Cxky by adding additional   *
     H*                key field CREFDT to retrieve the correct       *
     H*                cancellation detail record.                    *
     H*                                                               *
     H* 06/21/12 C:282 Project: WC2086                                *
     H*                Reinstatement Reason Code added in system      *
     H*                table value.                                   *
     H*                                                               *
     H* 03/13/12 C:180 SUPPORT 79494
     H*                Populate exp rating code with '5' when no
     H*                EXP MOD present in pol.
     H*
     H* 02/20/12 C:190 Project: WC2126                                *
     H*                Check the new field for determining what to    *
     H*                set the cancellation ID code to.               *
     H*                                                               *
     H* 01/25/12 C:136 Support: WA089 78956                           *
     H*                Allow non-NCCI states to extract when          *
     H*                client is not set up for state reporting.      *
     H*                                                               *
     H* 01/17/12 C:65 PROJECT: WA139/77336 (114WGH/SUPP#64386)        *
     H*               Change coding to report 0 terrorism charges to  *
     H*               NY for submissions to NY (was previously only   *
     H*               activating for submissions to NCCI).            *
     H*                                                               *
     H* 08/06/11 C:190 Project: WC2062                                *
     H*                Modify program to generate HC record into the  *
     H*                data submission file from WXPEP.               *
     H*                                                               *
     H* 05/30/11 C:189 Support: WC114 72000                           *
     H*                Report the WC370310C ENDORSEMENT electronically.
     H*                                                               *
     H* 04/20/11 C:65 PROJECT: SUPP#76102                             *
     H*               REMOVE THE PEO FLAG CHECK ON CALLING SRPEOS     *
     H*               SINCE YOU CAN ONLY (CURRENTLT) SET THE FLAG     *
     H*               ON FOR PEO'S.  THE OTHER PROGRAMMING FOR AAE    *
     H*               CHECKS THE PEO FLAG ALREADY WITHIN.             *
     H*                                                               *
     H* 03/30/11 C:65 PROJECT: SUPP#76283                             *
     H*               THE STATE CODE FIELD ON CX/REIN RECORDS SHOULD  *
     H*               BE SET TO '99' NOW INSTEAD OF BLANKS.           *
     H*                                                               *
     H* 11/10/10 C:65 PROJECT: 139WPL                                 *
     H*               DO NOT MULTIPLY EXPOSURE RATE BY 100 FOR HCN    *
     H*               CREDIT, SINCE IT IS A FACTOR NOT A PERCENTAGE.  *
     H*                                                               *
     H* 11/03/10 C:65 PROJECT: SUPP#74548/114WQC                      *
     H*               POPULATE NUMERIC STATE VALUE FOR PEEP STATE CD. *
     H*                                                               *
     H* 08/17/10 C:190 PROJECT: WC1899                                *
     H*                Delete changes of PROJECT: 114WGH/SUPP#57718   *
     H*                                                               *
     H* 07/07/10 C:65 PROJECT: SUPP#73417                             *
     H*               CHECK STATE SUBMISSION STATUS FOR REISSUES AS   *
     H*               MULTI-STATE POLICIES WILL NOT FLAG AS REISSUE   *
     H*               UNTIL ALL STATES HAVE SUBMITTED TO BUREAUS.     *
     H*                                                               *
     H* 02/25/10 C:33  Project: 25WRC  Priss #13                      *
     H*                Process Retro independently of Large Deductible*
     H*                                                               *
     H* 12/03/09 C:232 Support: WC139 70681                           *
     H*                Include all states to count the "Electronic    *
     H*                Transmittal Record".                           *
     H*                                                               *
     H* 07/15/09 C:65 PROJECT: 139WLR                                 *
     H*               CHANGE THE SERVICING OFFICE ADDRESS FOR ULLICO  *
     H*               AND CA TO OFFICE MAILING ADDRESS TO MATCH THE   *
     H*               POLICY DECLARATION PAGE.                        *
     H*                                                               *
     H* 06/26/09 C:65 PROJECT: 139WLR                                 *
     H*               REPORT THE TYPE 3 ADDRESS RECORD FOR ULLICO     *
     H*               USING THE SERVICING ADDRESS FROM OFFICE.        *
     H*                                                               *
     H* 01/29/09 C:65 PROJECT: 146WAP                                 *
     H*               REMOVE 12/11/08 CHANGE, APROVAL WAS RECEIVED.   *
     H*                                                               *
     H* 12/23/08 C:65  PROJECT: SUPP#66104/WC1633                     *
     H*                ONLY FLAG NAMES/ADDRESSES AS PREVIOUSLY        *
     H*                DELETED IF THEIR DATES INDICATE THEY ARE NOW   *
     H*                DELETED.                                       *
     H*                                                               *
     H* 12/11/08 C:65 PROJECT: 146WAP                                 *
     H*               DISABLE REVISIONS AND REWRITES FOR SCS.         *
     H*                                                               *
     H* 09/15/08 C:65 PROJECT: 114WGH/SUPP#64386                      *
     H*               REPORT THE TERRORISM CHARGES WHEN ZERO TO NY.   *
     H*               THIS WILL BE DONE IF THE POLICY HAS ANY NY      *
     H*               COVERAGE SINCE NY REPORTS THOUGH NCCI.          *
     H*                                                               *
     H* 08/12/08 C:65 PROJECT: 146WAP                                 *
     H*               REPORT THE WC000412 ENDORSEMENT ELECTRONICALLY. *
     H*               DO NOT REPORT TO DE,MI,NJ,NY, OR PA.            *
     H*                                                               *
     H* 07/18/08 C:65  PROJECT: WC1249/SUPPORT                        *
     H*                ALLOW THE MID-TERM ENDORSEMENT RESUBMISSION    *
     H*                PROCESS TO PROCEED IF THE POLICY RECORD IS     *
     H*                IN A '3' STATUS BUT HAS ALREADY SUBMITTED      *
     H*                PREVIOUSLY.                                    *
     H*                                                               *
     H* 07/09/08 C:65  PROJECT: SUPP#64177/WC1633                     *
     H*                REWRITE THE LOGIC TO REPORT DELETED NAMES AND  *
     H*                ADDRESSES TO INSTEAD FLAG THE WXNAP RECORD     *
     H*                WHEN A NAME OR ADDRESS IS ACTUALLY REPORTED.   *
     H*                THEN, A NEW SUBMISSION HISTORY DETAIL FILE     *
     H*                WILL BE CHECKED TO ACCESS THOSE PRIOR WXNAP    *
     H*                RECORDS TO VERIFY IF THE NAME/ADDRESS HAS BEEN *
     H*                REPORTED YET OR NOT.                           *
     H*                                                               *
     H* 05/20/08 C:65  PROJECT: WC1249/SUPP#63561                     *
     H*                ONLY PROCESS ENDORSEMENT AS POLICY RESUBMISSION*
     H*                IF THE STATE IS PENDING TO SEND ON 'E' RECORD. *
     H*                                                               *
     H* 04/04/08 C:189 114WQC                                         *
     H*                POPULATING EXTRA FIELDS FOR WCPOLS             *
     H*                                                               *
     H* 05/06/08 C:65  PROJECT: SUPP#63265                            *
     H*                MOVE THE CODING TO PROCESS ENDORSEMETNS AS     *
     H*                RESUBMISSIONS INSIDE THE GROUP LOOP FOR        *
     H*                MULTIPLE GROUP SUBMISSIONS.                    *
     H*                                                               *
     H* 05/01/08 C:65  PROJECT: 114WGH/SUPP#63021                     *
     H*                CLEAR THE NAME EFF/EXP CHANGE DATE FIELDS FOR  *
     H*                EACH RECORD TO PREVENT DATA CARRYOVER.         *
     H*                                                               *
     H* 02/29/08 C:65  PROJECT: 135WGP                                *
     H*                REMOVE BLOCK ON RESUBMISSIONS FOR NCCI FOR PPC.*
     H*                                                               *
     H* 02/22/08 C:65  PROJECT: WC1633/SUPP#62127/SUPP#62380          *
     H*                ADD SAME CODING FOR DELETED ADDRESSES AS WELL. *
     H*                                                               *
     H* 02/04/08 C:65  PROJECT: WC1633/SUPP#62127                     *
     H*                DO NOT REPORT DELETED NAMES IF NO PRIOR        *
     H*                SUBMISSION HAS BEEN COMPLETED.                 *
     H*                                                               *
     H* 12/15/07 C:65  PROJECT: 114WGH                                *
     H*                REPORT NCCI SUBMISSIONS FOR COMPWEST UNDER     *
     H*                CARRIER # 12985 FOR GROUP 5.                   *
     H*                                                               *
     H* 12/14/07 C:65  PROJECT: 114WGH                                *
     H*                DISABLE THE CODING TO SPLIT INCR LIMITS CHARGE *
     H*                FOR NJ, TO ALLOW THE BALANCE TO INCR LIMITS    *
     H*                MINIMUM CODE 9848 TO REPORT PROPERLY.          *
     H*                                                               *
     H* 12/13/07 C:183 PROJECT: 139WCI                                *
     H*                WHEN REPORTING THE TYPE '88' NAME CHANGE       *
     H*                RECORDS CHECK THE NEW EXPANDED NAME FIELD. IF  *
     H*                THIS FIELD IS POPULATED USE THAT FIELD INSTEAD *
     H*                OF NERNAME AND WRITE MULTIPLE RECORDS AS       *
     H*                NECESSARY IN ORDER TO REPORT THE FULL NAME     *
     H*                IF GREATER THAN 90 CHARACTERS.                 *
     H*                                                               *
     H* 12/04/07 C:65  PROJECT: 114WGH                                *
     H*                REPORT ADDITIONAL NAMED INSURED UNDER THE      *
     H*                ENTITY CODE OF THE MAIN NAME (001 USUALLY)     *
     H*                USING THE CONTINUATION SEQUENCE # FIELD.       *
     H*                                                               *
     H* 11/29/07 C:65  PROJECT: 135WGP                                *
     H*                PREVENT RESUBMISSIONS FROM SUBMITTING TO NCCI  *
     H*                FOR PPC.                                       *
     H*                                                               *
     H* 10/22/07 C:65  PROJECT: WC1633                                *
     H*                VERIFY IF A NAME/ADDRESS RECORD IS DELETED AND *
     H*                IF SO THEN ONLY REPORT IT IF THE LAST          *
     H*                SUBMISSION DATE/TIME FOR THE STATE BEING       *
     H*                PROCESSED IS EARLIER THAN THE DELETION DATE    *
     H*                AND TIME.  ADDITIONALLY,  PROCESS THE SUBMIS-  *
     H*                SION CODE STATUS FROM THE NEW STATE SUBMISSION *
     H*                CODE FIELDS.                                   *
     H*                                                               *
     H* 09/20/07 C:65  PROJECT: 114WGH                                *
     H*                RECOMPILE SINCE SPECIAL STATE FIELD FOR WXPEP  *
     H*                IN WWCPOLDAT3/4 HAS CHANGED TO ALPHA.          *
     H*                                                               *
     H* 09/20/07 C:65  PROJECT: 114WGH                                *
     H*                ENABLE REISSUE/RESUBMIT TRANSACTIONS FOR CSI   *
     H*                AND NJ.                                        *
     H*                                                               *
     H* 09/12/07 C:65  PROJECT: 114WGH                                *
     H*                REPORT REISSUES FOR NON-PA/MA/DE AS TYPE 14.   *
     H*                                                               *
     H* 09/06/07 C:65  PROJECT: 114WGH                                *
     H*                ENABLE MID-TERM ENDORSEMENTS AND REWRITE       *
     H*                TRANSACTIONS FOR NJ FOR CSI/CLIENT 114.        *
     H*                                                               *
     H* 08/03/07 C:183 PROJECT WC1520                                 *
     H*                RE-SUBMIT POLS IF ENDORSED AND USING AEE NAMING*
     H*                                                               *
     H* 07/23/07 C:65 PROJECT SUPP#58457                              *
     H*               SET PEO COMPANY CODE ON TYPE 88 NAME RECORDS.   *
     H*                                                               *
     H* 07/16/07 C:65 PROJECT 102WDC/114WGH                           *
     H*               REMOVE ENDORSEMENT SEQ# FROM DEFINITION, FIELD  *
     H*               DOES NOT EXIST IN WCPOLS LAYOUT.                *
     H*                                                               *
     H* 07/12/07 C:65  PROJECT: SUPPORT                               *
     H*                ADD STATE AND STATUS FIELDS TO PSH02 FIELD     *
     H*                RENAMES TO PREVENT DATA FROM COPYING OVER TO   *
     H*                OTHER RECORD READS.                            *
     H*                                                               *
     H* 07/11/07 C:65  PROJECT: WC1249                                *
     H*          C:53  CHECK GROUP LEVEL FLAG (FNPEOS) THAT WILL      *
     H*                CONTROL WHETHER MIDTERM ENDORSEMENTS ARE       *
     H*                INCLUDED IN WCPOLS DATA TRANSMISSION OR NOT.   *
     H*                                                               *
     H* 07/09/07 C:65  PROJECT: 114WGH/SUPP#57718                     *
     H*                IF ANNIVERSARY RATED THEN REPORT TWO RECORDS   *
     H*                ON SPLIT POLICIES TO PA FOR ASSESSMENTS AND    *
     H*                TERRORISM CHARGES.  NUMBERS ARE PULLED FROM    *
     H*                WDPPP FILE.                                    *
     H*                                                               *
     H* 05/11/07 C:65  PROJECT: SUPP# 57502                           *
     H*                ONLY ATTEMPT LOADING SIC/NAICS CODE IF NASIC   *
     H*                IS NOT EQUAL TO BLANKS.                        *
     H*                                                               *
     H*  03/21/07 C:65 PROJECT: 114WGH/114WFI                         *
     H*                WHEN SENDING PEO/CLIENT NAME DATA, DO NOT      *
     H*                SEND THE SECOND NAME RECORD FOR THE PRIMARY    *
     H*                NAME TO NJ AS NJ DOES NOT CARE ABOUT DIFFERENT *
     H*                FEIN NUMBERS.  REPORT ANY ADDRESSES FOR THAT   *
     H*                SECOND NAME UNDER NAME LINK CODE 001. IF       *
     H*                REPORTING FOR NJ, DO NOT MARK PRIMARY NAME     *
     H*                AS A PEO NAME.                                 *
     H*                                                               *
     H*  01/09/07 C:65 PROJECT: 114WGH/114WFI                         *
     H*                REMOVE CONDITIONS PREVENTING ADDRESSES FROM    *
     H*                BEGIN WRITTEN BASED ON CONTINUATION SEQ #.     *
     H*                CODE NO LONGER APPLICABLE DUE TO 09/02/06 CHG. *
     H*                                                               *
     H* 12/13/06 C:65  Seq#: 51590                                    *
     H*                IF SUBMITTING DATA TO NC, ENSURE STATE CODE    *
     H*                LINK ON ADDRESS RECORD MATCHES MAILING STATE.  *
     H*                                                               *
     H* 12/05/06 C:65  Seq#: 53079                                    *
     H*                -STRIP THE HYPHENS OUT OF THE POLICY #'S FOR   *
     H*                 ALL CLIENTS.                                  *
     H*                                                               *
     H*  10/24/06 C:65 PROJECT: 114WGH/114WFI                         *
     H*                REPEAT THE TAX ID'S ON THE NAME CONTINUATION   *
     H*                RECORDS INSTEAD OF BLANKING THEM OUT.          *
     H*                                                               *
     H*  10/13/06 C:65 PROJECT: 114WGH                                *
     H*                POPULATE THE HAZARD GROUP ON THE TYPE 15       *
     H*                RECORD FOR NJ SUBMISSIONS.                     *
     H*                                                               *
     H*  09/02/06 C:65 PROJECT: 114WGH/114WFI                         *
     H*                ALTER PROCESSING TO RECOGNIZE IS NANAM4 FIELD  *
     H*                IS POPULATED AND PULL THE NAME FROM THERE      *
     H*                INSTEAD.  THIS FIELD CONTAINS THE FULL NAME    *
     H*                AND MUST BE SPLIT INTO UP TO 4 NAME RECORDS,   *
     H*                USING THE NAME CONTINUATION SEQ #. AS THESE    *
     H*                NAMES ARE GENERATED, THE 3 TAX ID'S WILL BE    *
     H*                POPULATED ON THE FIRST NAME RECORD AND THE     *
     H*                OTHER RECORDS WILL HAVE ZERO VALUES. THE       *
     H*                NAME RECORDS WILL BE DUPLICATED FOR ADDITIONAL *
     H*                TAX ID'S.  NOTE:  DUE TO RECENT CHANGES WE     *
     H*                WILL NEVER USE AN ENTITY CODE FOR MULTIPLE     *
     H*                NAMES SO THE ORIGINAL CONTINUATION SEQ #       *
     H*                CODE CAN BE DISABLED.                          *
     H*                                                               *
     H*  08/25/06 C:65 PROJECT: 114WGH                                *
     H*                ADD PROCESSING TO CREATE RECORD INFORMATION    *
     H*                FOR THE WC290303 ENDORSEMENT.                  *
     H*                                                               *
     H* 08/04/06 C:33  Seq#: WA129/53290                              *
     H*                Implement 129WLS for CLT129 only.              *
     H*                -STRIP THE HYPHENS OUT OF THE PRIOR POLICY #   *
     H*                                                               *
     H* 07/20/06 C:65  PROJECT: 133WDY                                *
     H*                USE A NEW LOGICAL OVER WMMUP TO READ IN THE    *
     H*                ENDORSEMENTS FOR THE TYPE 7 RECORDS TO ORDER   *
     H*                THEM BY THE ENDORSEMENT STATE, INSTEAD OF      *
     H*                PARSING BY THE FORM #.                         *
     H*                                                               *
     H*  06/15/06 C:65 PROJECT: 139WBR                                *
     H*                SET PEO/COMPANY CLIENT NAME FIELD IN THE NAME  *
     H*                RECORD.  THIS WILL BE FLAGGED FROM THE PERSONAL*
     H*                NAME FIELD IN THE WXNAP FILE.                  *
     H*                                                               *
     H* 05/15/06 C:65  PROJECT: 114WGH                                *
     H*                DISABLE CODE THAT PREVENTS SHORT TERM/ANNIV    *
     H*                RATED POLICIES FROM SENDING TO NJ.             *
     H*                                                               *
     H*  04/25/06 C:65 PROJECT: 132WAY                                *
     H*                SET NAME TYPE ON NAME RECORDS ACCORDING TO THE *
     H*                NEW PERSONAL NAME FIELD.  ADDITIONALLY, IF A   *
     H*                '$' VALUE IS SET IN THE LEGAL NATURE FIELD     *
     H*                THEN MARK THE NAME AS A TRADE NAME(DBA).       *
     H*                NEXT, STRIP THE HYPHENS OUT OF THE PRIOR       *
     H*                POLICY # AS WELL FOR CA SUBMISSIONS.           *
     H*                FINALLY, MARK THE DATA RECORD FOR WC040303     *
     H*                CORRECTLY AS A 'DC' TYPE RECORD.               *
     H*                                                               *
     H* 04/06/06 C:168 PROJECT: 114WLK                                *
     H*                ADDED CODE FFROM CLIENT 114, RETRIEVE THE CLTID*
     H*                TO CONDITION THIS IMPORTED CODE                *
     H*                                                               *
     H* 04/06/06 C:65  PROJECT: SUPP# 52133/WC1250                    *
     H*                DO NOT MULTIPLY RATE FOR DOMESTIC TERRORISM    *
     H*                BY 100.  ADDITIONALLY, REPORT DOMESTIC CHARGE  *
     H*                IF ZERO ONLY TO NJ.                            *
     H*                                                               *
     H* 03/28/06 C:65  PROJECT: 102WDC                                *
     H*                IF THERE IS NO EXPERIENCE MOD, OR ITS EQUAL TO *
     H*                1, THEN REPORT STATUS 5 FOR NOT RATED ON HEADER*
     H*                                                               *
     H* 12/07/05 C:65  PROJECT: 114WGH                                *
     H*                ADD BALANCE TO MINIMUM TO STANDARD PREMIUM FOR *
     H*                FIRST STATE PREMIUM RECORD FOR NJ/DE/PA.       *
     H*                                                               *
     H* 10/20/05 C:65  PROJECT: SUPP# 49426                           *
     H*                IF REPORTING TO NCCI AND A SIC CODE IS BEING   *
     H*                USED, THEN APPEND 'SC' TO THE END OF THE CODE. *
     H*                                                               *
     H* 10/12/05 C:65  PROJECT: 132WAY                                *
     H*                REPORT NATURE OF BUSINESS FOR CA NAME RECORDS. *
     H*                                                               *
     H* 10/07/05 C:65  PROJECT: 132WAY                                *
     H*                DO NOT REPORT THE POLICY HOLDER NOTICES TO CA. *
     H*                                                               *
     H* 10/06/05 C:65  PROJECT: 132WAY                                *
     H*                REFINE TYPE OF BUSINESS FOR CA NAME RECORDS.   *
     H*                                                               *
     H* 10/04/05 C:65  PROJECT: 132WAY                                *
     H*                STRIP THE HYPHENS OUT OF THE POLICY CARRIER #  *
     H*                FOR CA SUBMISSIONS.                            *
     H*                                                               *
     H* 09/23/05 C:65  PROJECT: 102WDC                                *
     H*                ADD 1 TO RECORD COUNT FOR TRANSMITTAL RECORD   *
     H*                FOR NC AS WELL.                                *
     H*                                                               *
     H* 09/21/05 C:65  PROJECT: 114WGH                                *
     H*                REPORT WC290601A ENDORSEMENT ELECTRONICALLY.   *
     H*                                                               *
     H* 09/14/05 C:65  PROJECT: 114WGH/WC1298                         *
     H*                REPORT TWO RECORDS FOR INCREASED LIMITS ON NJ  *
     H*                SPLIT POLICIES TO ALLOW TOTALS FOR EACH PERIOD *
     H*                TO BE ACCURATE.  NUMBERS ARE PULLED FROM WDPPP.*
     H*                                                               *
     H* 09/13/05 C:65  PROJECT: 114WGH                                *
     H*                REMOVE THE 05/16/05 CHANGE WHERE WE SUBTRACTED *
     H*                THE AMOUNT OF THE MCO CREDIT BACK OUT FROM     *
     H*                MINIMUM PREMIUM.  NJ CHANGED ALGORITHM TO      *
     H*                ADJUST FOR IT CORRECTLY.                       *
     H*                                                               *
     H* 08/17/05 C:33 PROJECT: 114WGH                                 *
     H*              -Load 'Exposure Link' code.                      *
     H*              -If running for 'NJ', load the state code for    *
     H*                name records.                                  *
     H*                                                               *
     H* 08/09/05 C:65  PROJECT: 114WGH                                *
     H*                REPORT ADDITIONAL NAMED INSUREDS USING UNIQUE  *
     H*                NAME LINK IDENTIFIER FOR ALL STATES.           *
     H*                                                               *
     H* 08/05/05 C:65  PROJECT: 132WAY                                *
     H*                ADD DATA PROVIDER TYPE CODE AND TPE FEIN TO    *
     H*                THE SUBMISSION HEADER RECORD.                  *
     H*                                                               *
     H* 07/26/05 C:65  PROJECT: 114WGH                                *
     H*                REPORT MULTIPLE TAX ID'S FOR EACH NAME RECORD. *
     H*                CODE TO SAVE THE TAX ID FOR NJ IS NO LONGER    *
     H*                NECESSARY AS EACH WXNAP RECORD WILL CONTAIN    *
     H*                ALL OF THE TAX ID'S AND WILL GENERATE RECORDS. *
     H*                                                               *
     H* 06/23/05 C:61  PROJECT 133WBS                                 *
     H*                Change to process Form LUB0020199.             *
     H*                                                               *
     H* 05/23/05 C:65  PROJECT: 114WAK                                *
     H*                REPORT WC370310B ENDORSEMENT ELECTRONICALLY.   *
     H*                                                               *
     H* 05/18/05 C:65  PROJECT: 114WGH                                *
     H*                REPORT TERRORISM RECORD IF ZERO ONLY TO NJ.    *
     H*                                                               *
     H* 05/17/05 C:65  PROJECT: 114WGH/WC1298                         *
     H*                PULL EST STANDARD PREMIUM AND MCO CREDIT FROM  *
     H*                WDPPP PREMIUM PERIODS FILE FOR ACCURATE SPLIT. *
     H*                                                               *
     H* 05/16/05 C:65  PROJECT: 114WGH                                *
     H*                SUBTRACT THE MANAGED CARE CREDIT BACK OUT FROM *
     H*                BALANCE TO MINIMUM FIGURE FOR NJ.              *
     H*                                                               *
     H* 05/12/05 C:65  PROJECT: 132WAY                                *
     H*                REPORT A STATE PREMIUM RECORD FOR EACH EXP     *
     H*                MODIFIER FOR THE STATE OF CA.  THE EXP MOD     *
     H*                EFFECTIVE DATE WILL REPLACE THE ANNIV DATE.    *
     H*                ADD SUBROUTINES TO REPORT WC04302A AND         *
     H*                WC040303 CA ENDORSEMENTS AS WELL.              *
     H*                                                               *
     H* 05/11/05 C:136 PROJECT: WC1249                                *
     H*                CHECK GROUP LEVEL FLAG (FNPEOS) THAT WILL      *
     H*                CONTROL WHETHER MIDTERM ENDORSEMENTS ARE       *
     H*                INCLUDED IN WCPOLS DATA TRANSMISSION OR NOT.   *
     H*                                                               *
     H* 03/01/05 C:65  PROJECT: 114WGH                                *
     H*                REPORT TWO EXPOSURE RECORDS FOR MANAGED CARE   *
     H*                CREDIT FOR SPLIT NJ SUBMISSIONS. ALSO, DO NOT  *
     H*                REPORT ARD ENDORSEMENT RECORD FOR SPLIT NJ.    *
     H*                                                               *
     H* 02/14/05 C:65  PROJECT: 114WIY                                *
     H*                STORE RECORD COUNTS FOR DISPLAY ON THE         *
     H*                FINAL SCREEN OF SUBMISSION PROCESS.            *
     H*                                                               *
     H* 02/01/05 C:65  PROJECT: WC1272                                *
     H*                ADD PARM FOR CONTACT ADDRESS INFORMATION.      *
     H*                                                               *
     H* 02/01/05 C:65  PROJECT: 114WGH                                *
     H*                REPORT ADDRESS RECORD FOR ADDITIONAL NAMED     *
     H*                INSUREDS AS AN OTHER WORK LOCATION, A '2'.     *
     H*                                                               *
     H* 01/18/05 C:65  PROJECT: WC1253                                *
     H*                CREATE TRANSMITTAL RECORD WHENEVER THERE IS    *
     H*                CONTACT DATA ENTERED.                          *
     H*                                                               *
     H* 01/13/05 C:65  PROJECT: 114WGH                                *
     H*                IF A NAME HAS ALREADY BEEN REPORTED FOR        *
     H*                ANOTHER STATE AND THAT NAME ALSO EXISTS IN     *
     H*                NJ, THEN REPORT THE NJ TAX ID# UNLESS THERE IS *
     H*                ANOTHER NJ NAME IT CAN BE REPORTED UNDER.      *
     H*                                                               *
     H* 01/05/05 C:65  PROJECT: 114WGH                                *
     H*                REPORT ADDITIONAL NAMED INSUREDS WITH A UNIQUE *
     H*                NAME LINK CODE (NAENCD) FOR NJ.  WRITE A       *
     H*                MATCHING ADDRESS RECORD WITH THE NAME LINK     *
     H*                CODE AS WELL, PULLED FROM STATE RPT DIVISION.  *
     H*                                                               *
     H* 01/04/05 C:33  Seq#: WA114/                                   *
     H*                If the last space of the first name field is   *
     H*                 blank, keep the spacing when concatinating.   *
     H*                                                               *
     H* 11/29/04 C:33  Seq#: WA122/44732                              *
     H*                State codes must be reported as numeric.       *
     H*                                                               *
     H* 11/03/04 C:65  PROJECT: SUPP#44164                            *
     H*                REPORT EXP MODIFIER AS 1.000 OR 0.000 AS       *
     H*                APPROPRIATE FOR TYPE 10 RECORDS.               *
     H*                                                               *
     H* 10/22/04 C:65  PROJECT: SUPPORT                               *
     H*                ONLY REPORT ENDORSEMENTS WHICH ARE INCLUDED.   *
     H*                                                               *
     H* 10/21/04 C:65  PROJECT: SUPPORT                               *
     H*                IF SECOND EXP MODIFIER IS ZERO, REPORT 1ST     *
     H*                MODIFIER IN THE ANNIV STATE PREM RECORD.       *
     H*                                                               *
     H* 10/20/04 C:65 PROJECT: 114WAK/SUPPORT                         *
     H*               REPORT THE WC000402 ENDORSEMENT ELECTRONICALLY. *
     H*                                                               *
     H* 10/06/04 C:65  PROJECT: SUPPORT                               *
     H*                ONLY CHECK STATE STATUS IN CHKPOL SUBROUTINE   *
     H*                IF WE ARE PROCESSING STATE SUBMISSION LISTS.   *
     H*                                                               *
     H* 10/04/04 C:33  Support:                                       *
     H*                Report continuation seq #'s for all divisions. *
     H*                                                               *
     H* 09/24/04 C:65  PROJECT: SUPPORT                               *
     H*                RESTRICT CHKPOL CHECK FOR MAIN POLICY RECORD   *
     H*                IF THE STATE LIST MOD IS ENABLED.              *
     H*                                                               *
     H* 09/23/04 C:65  PROJECT: SUPP#43907                            *
     H*                REPORT THE PROPER POLICY CHANGES EFF/EXP DATES *
     H*                ON ALL RECORD TYPES.  THIS WILL BE DONE BY     *
     H*                LOADING IN THE APPROPRIATE POLICY DATES INTO   *
     H*                WXNHP, WXNAP, AND WXNXP.                       *
     H*                                                               *
     H* 09/10/04 C:33  Project: WC1242                                *
     H*                Add 'Name Link Counter Identifier' field.      *
     H*                                                               *
     H* 08/31/04 C:65  PROJECT: 114WGH/SUPP                           *
     H*                ONLY REPORT NJ TAX ID'S FOR NJ NAME/ADDRESS.   *
     H*                                                               *
     H* 08/31/04 C:65  PROJECT: 114WGH/SUPP                           *
     H*                CHANGE THE 2ND NAME FOR EXCLUSION ENDORSEMENT  *
     H*                TO MOVE LEFT SINCE ITS STORED IN WXPEP AS 61   *
     H*                CHARACTERS INSTEAD OF 60.                      *
     H*                                                               *
     H* 08/31/04 C:33  Project: WC1202                                *
     H*                If the last space of the first name field is   *
     H*                 blank, keep the spacing when concatinating.   *
     H*                                                               *
     H* 08/06/04 C:65  PROJECT: 114WGH/SUPP                           *
     H*                EXPAND THE CHECK FOR EARLIEST AND LATEST       *
     H*                TRANSACTION DATES TO INCLUDE CX/REIN/ENDORS.   *
     H*                                                               *
     H* 08/04/04 C:65  PROJECT: 114WGH/SUPP                           *
     H*                SPLIT THE ESTIMATED PREMIUM FIELD IN THE STATE *
     H*                PREMIUM RECORD WHEN POLICY IS ANNIVERSARY RATED.
     H*                                                               *
     H* 08/03/04 C:65  PROJECT: 114WGH/SUPP                           *
     H*                ENSURE THE NJ TAX ID IS REPORTED ON THE NAME   *
     H*                RECORD WHEN THE NJ LOCATION HAS SAME NAME AS   *
     H*                A PREVIOUSLY REPORTED NAME.                    *
     H*                                                               *
     H* 08/02/04 C:65  PROJECT: 114WGH/SUPP                           *
     H*                IF REPORTING TO NCCI, DO NOT REPORT 2 STATE    *
     H*                PREMIUM RECORDS FOR SPLIT POLICIES.  THE FIRST *
     H*                RECORD WHEN REPORTED SHOULD ALSO HAVE A ZERO   *
     H*                ANNIV DATE AND THE ESTIMATED PREMIUM POPULATED.*
     H*                                                               *
     H* 07/22/04 C:33 Seq#: WA091/40129                               *
     H*               Only write the first endorsement record for a   *
     H*                policy with each submission.                   *
     H*                                                               *
     H* 06/24/04 C:65 PROJECT: 114WHO                                 *
     H*               WRITE AN ENDORSEMENT RECORD FOR WC290503B.      *
     H*                                                               *
     H* 06/21/04 C:65 PROJECT: SUPPORT/114WAK                         *
     H*               RECHAIN TO EMPLOYER MASTER FOR THE NAME OF      *
     H*               EMPLOYER ON THE TYPE 88 AND 89 RECORDS.         *
     H*                                                               *
     H* 06/18/04 C:65  PROJECT: 114WGH/SUPP                           *
     H*                DO NOT PROCESS ANY RECORDS THAT HAVE A STATE   *
     H*                STATUS OF '4' FOR CX, REINS, OR ENDORSMENTS.   *
     H*                                                               *
     H* 06/16/04 C:65  PROJECT: 114WGH/SUPP                           *
     H*                IF A POLICY IS ANNIVERSARY SPLIT THEN REPORT   *
     H*                TWO STATE PREMIUM RECORDS FOR NJ/PA/DE. ALSO   *
     H*                IF THERE IS NO EXPERIENCE MODIFIER FOR NJ      *
     H*                THEN REPORT ZERO INSTEAD OF ONE.               *
     H*                                                               *
     H* 06/10/04 C:65 PROJECT: 114WAK/SUPP#42709                      *
     H*               DO NOT PROCESS A POLICY SUBMISSION IF THE       *
     H*               STATE'S STATUS IS SET TO '4' ALREADY.           *
     H*                                                               *
     H* 06/10/04 C:65 PROJECT: SUPPORT/114WGH                         *
     H*               REPORT A TYPE 07 RECORD FOR EACH STATE'S        *
     H*               ENDORSEMENTS.                                   *
     H*                                                               *
     H* 06/09/04 C:65 PROJECT: SUPPORT/114WGH                         *
     H*               READ DATA FOR WC290409A ENDORSEMENT FROM THE    *
     H*               WXPEP FILE.                                     *
     H*                                                               *
     H* 05/14/04 C:65 PROJECT: WC1202                                 *
     H*               REPORT EXTENDED EMPLOYER NAMES FOR NAME RECORDS.*
     H*                                                               *
     H* 04/20/04 C:65 PROJECT: SUPPORT/114WGH                         *
     H*               INCLUDE TRANSMITTAL RCD IN RECORD COUNT FOR NJ. *
     H*                                                               *
     H* 03/23/04 C:65 PROJECT: SUPPORT/114WGH                         *
     H*               REMOVE RESTRICTION FROM WRITING ADDRESS RECORDS *
     H*               FOR ADDITIONAL LOCATIONS(TYPE = 2).  THE NAME   *
     H*               RECORD CHANGE FOR NOT REPORTING IDENTICAL NAMES *
     H*               WAS PREVENTING ADDRESS FOR WORKPLACES FROM      *
     H*               BEING WRITTEN.                                  *
     H*                                                               *
     H* 03/22/04 C:65 PROJECT: SUPPORT/114WAK                         *
     H*               COPIED P06REC DATA INTO THE P06 FORMAT BEFORE   *
     H*               WRITING EXCLUDED STATES RECORD.  STATES WERE    *
     H*               NOT BEING COPIED IN DUE TO THIS MISSING.        *
     H*                                                               *
     H* 02/25/04 C:65 PROJECT: SUPPORT/114WAK                         *
     H*               INCLUDE PDS ADDRESS IN WCPOLS TRANSMITTAL RCD.  *
     H*               IF NJ AND UNEMPLOYMENT # IS BLANK THEN REPORT   *
     H*               '29' IN STATE FIELD FOLLOWED BY 12 ZEROS.       *
     H*               CHANGE THE 'AI' RECORD TYPE TO USE THE PROPER   *
     H*               DATA STRUCTURE.  WRITE THE FEDERAL NAME TO      *
     H*               TYPE 88 AND 89 RECORDS.                         *
     H*                                                               *
     H* 02/24/04 C:65 PROJECT: SUPPORT/WC1060                         *
     H*               ADD A CHAIN AT BEGINNING OF PROCESSING A POLICY *
     H*               TO ENSURE MEDICAL EXCLUSION FLAG IS POPULATED.  *
     H*                                                               *
     H* 02/12/04 C:65 PROJECT: 114WGH                                 *
     H*               WHEN WRITING THE TYPE 06 RECORD FOR EXCLUDED    *
     H*               STATES, INCLUDE ANY STATES ALSO ON THE POLICY.  *
     H*                                                               *
     H* 02/11/04 C:65 PROJECT: 114WAK/SUPP#40711                      *
     H*               DO NOT MULTIPLY THE EXPOSURE RATE BY 100 IF IT  *
     H*               HAS ALREADY BEEN DONE WITHIN W7413R.  CLEAR     *
     H*               THE WORKFIELD FOR THE P07 RECORD AS WELL.       *
     H*                                                               *
     H* 02/06/04 C:65 PROJECT: 114WAK/SUPP#40711                      *
     H*               ADD CODE TO PREVENT DECIMAL DATA ERRORS WHEN    *
     H*               ENDORSEMENTS DO NOT UPDATE WXPEP RECORD WITH    *
     H*               ANY DATA.                                       *
     H*                                                               *
     H* 01/30/04  C:35   PROJECT: 114WGH                              *
     H*                  CONCATENATE THE SECOND ADDRESS LINE ONTO     *
     H*                  THE FIRST WHEN LOADING THE ADDRESS RECORD    *
     H*                  FOR THE GROUP.                               *
     H*                  LOAD INDEPENDENT RATING BUREAU RISK ID FOR   *
     H*                  PA/DE/NJ.                                    *
     H*                  ZERO EXPERIENCE MOD IS VALID FOR PA,DE,CA.   *
     H*                  ONLY LOAD POLICY PAGE ENDORSEMENTS (END# = 0)*
     H*                  TO RECORD TYPE 7.                            *
     H*                                                               *
     H* 01/16/04 C:65 PROJECT: 114WAK/SUPP#40711                      *
     H*                CREATE A NEW TRANSMITTAL RECORD IF THE SUBM-   *
     H*                ISSION STATE IS NJ AS WELL.                    *
     H*                                                               *
     H* 11/13/03  C:35   PROJECT: SUPPORT WA081 #37326                *
     H*                  CHANGED TO USE LARGER FIELD FOR MINIMUM      *
     H*                  PREMIUM IN WXNHP.                            *
     H*                                                               *
     H* 06/12/03  C:35   PROJECT: SUPPORT WA081 #37326                *
     H*                  RECORD TYPE 06 (EXCLUDED/INCLUDED STATES)    *
     H*                  WAS BEING WRITTEN EVEN WHEN NO RECORDS WERE  *
     H*                  FOUND IN WDXSP.                              *
     H*                  LEGAL NATURE DESCRIPTION IN WMEMP IS LONGHER *
     H*                  THAN NCCI'S FIELD, SO IT NEEDS TO BE LOADED  *
     H*                  USING MOVEL.                                 *
     H*                                                               *
     H* 06/11/03  C:35   PROJECT: SUPPORT WA093 #37318                *
     H*                  VALUE '08' FOR POLICY TYPE ID CODE NON-      *
     H*                  STANDARD IS NOT VALID FOR NORTH CAROLINA.    *
     H*                                                               *
     H* 05/27/03  C:35   PROJECT: SUPPORT WA093 #37318                *
     H*                  CHANGE MADE ON 09/12/00 WAS SETTING          *
     H*                  INCORRECT FLAG.                              *
     H*                                                               *
     H* 05/02/03 C:132 PROJECT: WC996                                 *
     H*                Add code for second name field for additional  *
     H*                named insureds in files WXANP                  *
     H*                                                               *
     H* 04/08/03  C:35   PROJECT: SUPPORT WA126 #36603                *
     H*                  CHANGE MADE ON 03/31 CAUSED PRODUCER ADDRESS *
     H*                  TO BE SKIPPED.                               *
     H*                                                               *
     H* 03/31/03  C:35   PROJECT: SUPPORT WA126 #36603                *
     H*                  DO NOT WRITE ADDRESS RECORD FOR ADDITIONAL   *
     H*                  NAMES ON ENTITY CODE 001.                    *
     H*                                                               *
     H* 03/19/03  C:35  PROJECT: SUPPORT WA090 #36379                 *
     H*                 CLEAR UNEMPLOYMENT STATE CODE IF UNEMPLOYMENT *
     H*                 NUMBER IS BLANK.                              *
     H*                                                               *
     H* 01/22/03  C:35   PROJECT: SUPPRT WA108 #35598                 *
     H*                  DO NOT REPORT NAME RECORD IF NAME IS BLANK.  *
     H*                                                               *
     H* 10/03/02 C:65  PROJECT 114WAK                                 *
     H*                CREATE A NEW TRANSMITTAL RECORD IF THE SUBM-   *
     H*                ISSION STATE IS PA OR DE.  THIS RECORD WILL    *
     H*                CONTAIN CONTACT INFORMATION AND WILL NOT BE    *
     H*                INCLUDED IN THE RECORD COUNT.                  *
     H*                                                               *
     H* 06/28/02  C:35   PROJECT: SUPPORT WA072 #33255                *
     H*                  PAD ##ADDR FIELD WITH BLANKS.                *
     H*                                                               *
     H* 04/24/02  C:35   PROJECT 114WBK/SUPPORT WA081 #32233          *
     H*                  ENDORSEMENT ARRAY WAS NEVER BEING LOADED     *
     H*                  INTO OUTPUT RECORD.                          *
     H*                                                               *
     H* 02/15/02  C:35   PROJECT: 114WAK/SUPPORT WA096 #31284         *
     H*                  ONLY UPDATE STATE PARM TO 'XX' IF WCFL18 =   *
     H*                  'Y'.                                         *
     H*                                                               *
     H* 01/17/02  C:35   PROJECT 114WAK/SUPPORT WA081 #30855          *
     H*                  STATE PARM WAS NOT BEING USED TO EXCLUDE/    *
     H*                  INCLUDE TRANSACTIONS FOR SUBMISSION.         *
     H*                                                               *
     H* 12/20/01 C:09 PROJECT: SUPPORT WA095 #29580                   *
     H*               LOAD EXP MOD INFO DIRECTLY FROM WDELP TO GET    *
     H*               FULL VALUE (X.XXX) UNTIL NEXT BASE TAPE WHEN    *
     H*               THE FIELD NEMOD CAN BE EXPANDED FROM 3.2 P TO   *
     H*               4.3 P.                                          *
     H*                                                               *
     H* 12/12/01  C:53   PROJECT: WC1060
     H*                  ADD EXCLUDE MEDICAL PROCESSING.
     H*                                                               *
     H* 10/17/01 C:35 PROJECT: SUPPORT WA090 #29249                   *
     H*               ONLY LOAD UNEMPLOYMENT STATE IF UNEMPLOYMENT    *
     H*               NUMBER IS ASSIGNED.                             *
     H*                                                               *
     H* 09/24/01 C:65 PROJECT: 114WAK                                 *
     H*               PROCESS CX/REIN/ENDOR'S IF THE POLICY RECORD    *
     H*               IS STATUS '4' OR IF THE STATE'S STATUS IS '4'.  *
     H*                                                               *
     H* 08/03/01 C:35 PROJECT: SUPPORT WA113 #28494                   *
     H*               TRANSACTION DATE WAS NOT BEING LOADED FOR       *
     H*               CANCELLATIONS.                                  *
     H*                                                               *
     H* 07/23/01 C:65 PROJECT: 114WBK                                 *
     H*               ADD REPORTING REQUIREMENTS FOR NJ.              *
     H*                                                               *
     H* 07/17/01 C:35 PROJECT: SUPPORT WA091 #28275                   *
     H*               ERROR WAS INTRODUCED ON 04/29/99 THAT CAUSED    *
     H*               ALL SUBSEQUENT POLICY YEAR DATA FOR A GIVEN     *
     H*               EMPLOYER TO BE SKIPPED WHEN A CANCELLATION      *
     H*               OR REINSTATEMENT RECORD WAS ENCOUNTERED FOR     *
     H*               A POLICY YEAR FOR WHICH NO POLICY PAGE HAD      *
     H*               BEEN SUBMITTED.                                 *
     H*                                                               *
     H* 06/27/01 C:35 PROJECT: SUPPORT WA112 #27937                   *
     H*               EXPOSURE ACT WAS NOT BEING LOADED WITH THE      *
     H*               PROPER VALUE.                                   *
     H*                                                               *
     H* 06/13/01 C:61 PROJECT WC1038                                  *
     H*              - Remove code that creates unique transaction    *
     H*                dates.                                         *
     H*              - Load CX/Rein Transaction Sequence # (P08TS#).  *
     H*                                                               *
     H* 06/01/01 C:35 PROJECT: WC878                                  *
     H*               LOAD REVISED NUMBER OF EMPLOYEES AND REVISED    *
     H*               SIC CODE IN RECORD TYPE 89.                     *
     H*                                                               *
     H* 04/30/01 C:65 PROJECT: 114WAK                                 *
     H*               ADD REPORTING REQUIREMENTS FOR PA/DE.  THIS     *
     H*               WILL INCLUDE REPORTING BY STATE/DIVISION AS     *
     H*               WELL AS WRITING REPORTING RECORDS FOR ENDRSMTS. *
     H*                                                               *
     H* 02/23/01 C:35 PROJECT: WC577                                  *
     H*               ADD CODE TO REPORT STATE UNEMPLOYMENT           *
     H*               NUMBER.                                         *
     H*                                                               *
     H* 01/16/01 C:61 PROJECT WC996                                   *
     H*               For each record written with link code 001      *
     H*                (NAENCD=1), start continuation sequence number *
     H*                (P02CSQ) at 1 and increment by one for each    *
     H*                submission history record processed.           *
     H*                                                               *
     H* 12/20/00 C:35  PROJECT: SUPPORT WA095 #25426                  *
     H*                REVISED NAME WAS BEING LOADED BOTH LEFT-       *
     H*                JUSTIFIED AND RIGHT-JUSTIFIED TO SAME FIELD.   *
     H*                                                               *
     H* 12/15/00 C:35  PROJECT: WC577                                 *
     H*                CHANGE TO USE TRANSACTION CODE 14 FOR NON-     *
     H*                KEY CHANGES AND CODE 6 FOR KEY CHANGES THAT    *
     H*                ARE NOT REWRITES.                              *
     H*                                                               *
     H* 12/07/00 C:35  PROJECT: SUPPORT                               *
     H*                CHANGE TO USE NEW ALPHA FIELD FOR PRIOR        *
     H*                POLICY NUMBER.                                 *
     H*                                                               *
     H* 10/27/00 C:61  PROJECT 95WFG                                  *
     H*              - Load Nature field EMNAOB -> P08NAE.            *
     H*                                                               *
     H* 10/11/00  C:35   PROJECT: SUPPORT WA095 #24588                *
     H*                  CONCATENATE THE SECOND ADDRESS LINE ONTO     *
     H*                  THE FIRST WHEN LOADING THE ADDRESS RECORD.   *
     H*                                                               *
     H* 09/12/00  C:35   PROJECT: WC935/SUPPORT WA096 #24259          *
     H*                  ZERO EXP MOD IS NOT VALID FOR NCCI.  IF NO   *
     H*                  EXP MOD, REPORT AS 1.000 AND SET FLAG TO     *
     H*                  INDICATE NO EXP MOD APPLICABLE.              *
     H*                                                               *
     H* 03/21/00  C:35   PROJECT: SUPPORT WA033 #22289                *
     H*                  LOAD A ZERO TO THE CANCEL CODE IN THE CASE   *
     H*                  OF A NON-RENEWAL.                            *
     H*                                                               *
     H* 11/03/99  C:35   PROJECT: SUPPORT WA091                       *
     H*                  LOAD STATE FROM EXPOSURE TRANSACTION RECORD  *
     H*                  TO STATE FIELD IN EXPOSURE RECORD (RECORD    *
     H*                  TYPE '05).                                   *
     H*                                                               *
     H* 10/20/99  C:35   PROJECT: WC947                               *
     H*                  REPORT PRIOR CARRIER NUMBER, POLICY NUMBER   *
     H*                  AND EFFECTIVE DATE IN THE CASE OF A REWRITE. *
     H*                                                               *
     H* 06/24/99  C:35   PROJECT: SUPPORT WA096 #18378                *
     H*                  CONVERT SIC CODE FROM LEFT-JUSTIFIED         *
     H*                  CHARACTER FIELD TO RIGHT-JUSTIFIED NUMERIC   *
     H*                  FIELD.                                       *
     H*                                                               *
     H* 05/25/99  C:35   PROJECT: SUPPORT WA096 #16779                *
     H*                  IF THE POLICY TRANSACTION IS A REISSUE, LOAD *
     H*                  FIELDS FOR ORIGINAL CARRIER NUMBER, ORIGINAL *
     H*                  POLICY NUMBER AND ORIGINAL POLICY EFFECTIVE  *
     H*                  DATE.                                        *
     H*                                                               *
     H* 04/29/99  C:35   PROJECT: SUPPORT WA096 #17258                *
     H*                  CHANGE CANCELLATION/REINSTATEMENT RECORD     *
     H*                  PROCESS TO ENSURE NO TWO TRANSACTIONS ARE    *
     H*                  REPORTED WITH THE SAME TRANSACTION DATE.     *
     H*                  ALSO, FIX CODE SO THAT CORRECT AREA CODE IS  *
     H*                  LOADED TO ADDRESS RECORD (P03).              *
     H*                                                               *
     H* 02/05/99  C:87   PROJECT: WC878                               *
     H*                  Load P03NOE with the value in NAEMPY.  Load  *
     H*                  P03SIC with the value in NASIC.              *
     H*                                                               *
     H* 10/21/98  C:35   PROJECT: SUPPORT WA090 14265                 *
     H*                  CALL TO SRJULC WAS USING INCORRECT DATE      *
     H*                  FORMAT.                                      *
     H*                                                               *
     H* 09/03/98  C:35   PROJECT: SUPPORT WA091 13352                 *
     H*                  USE NEW FIELD EMRSK# FOR NCCI RISK# INSTEAD  *
     H*                  OF EMCOM3.                                   *
     H*                                                               *
     H* 08/29/98  C:35   PROJECT: WC862                               *
     H*                  REPORT TO NCCI USING THE CARRIER POLICY      *
     H*                  NUMBER IF THERE IS ONE,  OTHERWISE USE PDS   *
     H*                  NEW-FORMAT POLICY NUMBER.  (IF A POLICY WAS  *
     H*                  PREVIOUSLY REPORTED TO NCCI USING THE OLD-   *
     H*                  FORMAT POLICY NUMBER, THAT NUMBER WILL BE IN *
     H*                  THE CARRIER POLICY NUMBER FIELD.             *
     H*                                                               *
     H* 03/30/98  C:35  PROJECT: WC829                                *
     H*                 REPORT POLICY NUMBER IN OLD FORMAT IF POLICY  *
     H*                 EFFECTIVE DATE < F2DT01 (DATE CLIENT IS LIVE  *
     H*                 ON V3).                                       *
     H*                                                               *
     H*****************************************************************
      *///POLICY ELECTRONIC TRANSFER FILE
     FWCPOLS    O    E             DISK
      *///POLICY HEADER TRANSACTION
     fWXNHL     UF   E           K DISK
      *///POLICY EXPOSURE TRANSACTION
     FWXNXL     IF   E           K DISK
     FWXNX2L    UF A E           K DISK    rename(PNX01:PNX02)
     F                                     INFDS(DBFDB2)
     FWXNXP2    IF   E             DISK    rename(PNX01:PNX02P)
      *///POLICY ENDORSEMENTS TRANSACTIONS
     FWXNEL     IF   E           K DISK
      *///POLICY NAME/ADDRESS TRANSACTIONS
     FWXNAL     IF   E           K DISK
     F                                     INFDS(DBFDB)
     FWXNAP     UF   E             DISK
     F                                     RENAME(PNA01:PNA02)
     FWXNAL1    IF   E           K DISK
     F                                     RENAME(PNA01:PNA03)
      *///CANCELLATION TRANSACTIONS
     FWXC#L1    IF   E           K DISK
      *///SUBMISSION HISTORY
     FWMSHL5    UF   E           K DISK
     FWMSHL18   UF   E           K DISK
     F                                     RENAME(PSH01:PSH18)
     FWMSHL7    IF   E           K DISK
     F                                     RENAME(PSH01:PSH02)
     F                                     Prefix(s2:2)
     FWMEML     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWDF2L     IF   E           K DISK
     FWMRPL     IF   E           K DISK
      *///GENERIC TABLE FILE
     FWTGTL     IF   E           K DISK
     FWMSHL15   UF   E           K DISK
     F                                     RENAME(PSH01:PSH04)
     FWMSHL1    UF   E           K DISK
     F                                     RENAME(PSH01:PSHL1)
     F                                     Prefix(ph:2)
     FWZEXL     IF   E           K DISK
     F*** begin add ***  04/21/25  ************************************
     Fwzexl1    if   e           k disk
     F                                     rename(pex01:pex02)
     F*** end add *****  04/21/25  ************************************
     FWDELL     IF   E           K DISK
     FWMMUL5    IF   E           K DISK
     FWMMUL6    IF   E           K DISK
     F                                     RENAME(PMU01:PMU06)
     FWMMUL7    UF A E           K DISK    USROPN
     F                                     RENAME(PMU01:PMU02)
     FWDXSL     IF   E           K DISK
     FSTSTL3    IF   E           K DISK
     FWXPEL     IF   E           K DISK
     FSWCPF     IF   E           K DISK
     FWDRRL     IF   E           K DISK
     FSTSTL4    IF   E           K DISK
     F                                     RENAME(PST01:PST02)
     FWDELL1    IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
      *** Beg Add ***** 08/19/25 ******************************
     FWDELL1Q   IF   E           K DISK    Prefix(ma:2)
     F                                     RENAME(PEL01:PEL01Q)
      *** End Add ***** 08/19/25 ******************************
     FWMRSL     IF   E           K DISK
     Fwdcxl10   if   e           k disk
     Fwtcrl     if   e           k disk
     F                                     rename(pcr01:pcr02)
     F                                     Prefix(ct:2)
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PNA01)
     FWDPPL     IF   E           K DISK
     FWMOFL     IF   E           K DISK
     FWXPGL     IF   E           K DISK
     FWDEHL6    IF   E           K DISK
     FWDSHL1    IF A E           K DISK
     FWMSHP     IF A E           K DISK
     F                                     Rename(PSH01:PSH0P)
     F                                     Prefix(P0)
     FWMHCL     IF   E           K DISK
     Fwtstl     IF   E           K DISK    rename(pst01:pst00)
     FWXMUL     IF A E           K DISK    rename(pmu01:xmu00)
     F                                     Prefix(xu:2)
     FWXA6L     IF A E           K DISK    rename(pa601:pa600)
     FWTLML1    IF   E           K DISK
     fwtenl     if   e           k disk    rename(pen01:pen00)
     fwtesl     if   e           k disk    rename(pes01:pes00)
     Fwtxcl     if   e           k disk
     Fsmorl     if   e           k disk    Rename(por01:por00)
     Fwdf3l     if   e           k disk    Rename(pf301:pf300)
      *** Beg Add ***** 02/14/24 *******************************
     Fwmnxl     if   e           k disk    Rename(pnx01:pnx00)
      *** End Add ***** 02/14/24 *******************************
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)
      *
     D SIC1            S              1    DIM(6) ASCEND                        SIC CODE LEFT JST
     D SIC2            S              1    DIM(6) ASCEND                        SIC CODE RGHT JST
     D CAP             S              1    DIM(25)
     D CAP2            S              1    DIM(18)
     D splitDt         S              7S 0 DIM(4) ASCEND
     d priorSTATE      s              2    DIM(51)
     d deletionFLAG    s              1    DIM(51)
     d currentSTATE    s              2    DIM(51)
     d currentSTATUS   s              1    DIM(51)
     d xxTR15          s              1
     d xxRENW          s              1
     d counter1        s              3  0
     d counter2        s              3  0
     d index           s              3  0
     d xxStatus        s              1
     d writeExclude    s              1
     d xxApproved      s              1
     D ##co#           s              3
     D ##fnd           s              3
     D ##emp#          s              9
     D ##div           s              5
     D ##fyr           s              3
     D ##Mode          s              1
     D ##Rrn           s              9  0 Inz
     D ##Mod2          s              1
     D EndrPgm         s             10
     D isProcessEnd    s               n
     d xxEAPR          s             11  2
     d xxEMPR          s             11  2
     d svState         s                   like(STstate)
     d svStCd          s                   like(STcode)
     d svPrtZrFlg      s                   like(STflag)
     d svStatus        s                   like(STstate)
     d svDate          s                   like(NHpcdt)
     d XXpos1          s              9  0
     d xxCAWvrSeq#     s              2  0 inz(0)
     d svP01LNI        s              2  0
     d svP01TON        s             20
     d svP01NOP        s             30
     d svP02TON        s              1  0
     d svP03PH#        s             10  0
     d newName         s              1
     d newAddr         s              1
     d newExpo         s              1
     d newEndr         s              1
     d xxAType         s              1  0
     d x5St            s                   like(NXSt)
     d x5Rt            s                   like(NXRt)
     d x5Clas          s                   like(NXClas)
     d x5Efdt          s                   like(NXEfdt)
     d x5Desc          s                   like(NXDesc)
     d x5Expp          s                   like(NXExpp)
     d x5Csuf          s                   like(NXCsuf)
     d*** begin add ***  07/28/25  ***************************************
     d x5Eapr          s                   like(NXEapr)
     d*** end add *****  07/28/25  ***************************************
     d svaNHEXM1       s              5  4 DIM(51)
     d svaNHEXM2       s              5  4 DIM(51)
     d svaNHEXM3       s              5  4 DIM(51)
     d svaNHEMD1       s              7  0 DIM(51)
     d svaNHEMD2       s              7  0 DIM(51)
     d svaNHEMD3       s              7  0 DIM(51)
     d svaNHANDT       s              7  0 DIM(51)
     d svaP04EMS       s              1  0 DIM(51)
     d flgGenCX        s              1a
     d intNamOpLen     s              3  0
     D PDAT            S              7  0 DIM(8) ASCEND                        Period Dates
     D PRO             S              5  4 DIM(8)                               Prorata Factors
     D DYS             S              3  0 DIM(8)                               Days in each Period
     D PREM            S             11  0 DIM(8)                               Premium Amounts
     D svt             s              2    dim(15)
     d flatDel         s              1a
     d company#        s                   like(SHco#)
     d group#          s                   like(SHfnd)
     d employer#       s                   like(SHemp#)
     d policyYear      s                   like(SHfyr)
     d polEffDate      s              8  0
     d subTranDate     s              8  0
     d state           s                   like(SHst01)
     d subType         s                   like(SHtype)
     d companyName     s             40a
     d userName        s             50a
      *** Beg Add ***** 02/14/24 *******************************
     d xxExcl99        s              1
      *** End Add ***** 02/14/24 *******************************
      *
     D                 DS
     D  PST                    1     30
     D                                     DIM(15)
     D  PHST01                 1      2
     D  PHST02                 3      4
     D  PHST03                 5      6
     D  PHST04                 7      8
     D  PHST05                 9     10
     D  PHST06                11     12
     D  PHST07                13     14
     D  PHST08                15     16
     D  PHST09                17     18
     D  PHST10                19     20
     D  PHST11                21     22
     D  PHST12                23     24
     D  PHST13                25     26
     D  PHST14                27     28
     D  PHST15                29     30
     D                 DS
     D  PSS                    1     15
     D                                     DIM(15)
     D  PHSS01                 1      1
     D  PHSS02                 2      2
     D  PHSS03                 3      3
     D  PHSS04                 4      4
     D  PHSS05                 5      5
     D  PHSS06                 6      6
     D  PHSS07                 7      7
     D  PHSS08                 8      8
     D  PHSS09                 9      9
     D  PHSS10                10     10
     D  PHSS11                11     11
     D  PHSS12                12     12
     D  PHSS13                13     13
     D  PHSS14                14     14
     D  PHSS15                15     15
     D                 DS
     D  PSD                    1    105  0
     D                                     DIM(15)
     D  PHDT01                 1      7  0
     D  PHDT02                 8     14  0
     D  PHDT03                15     21  0
     D  PHDT04                22     28  0
     D  PHDT05                29     35  0
     D  PHDT06                36     42  0
     D  PHDT07                43     49  0
     D  PHDT08                50     56  0
     D  PHDT09                57     63  0
     D  PHDT10                64     70  0
     D  PHDT11                71     77  0
     D  PHDT12                78     84  0
     D  PHDT13                85     91  0
     D  PHDT14                92     98  0
     D  PHDT15                99    105  0
      *
     D                 DS
     D  PSC                    1     15
     D                                     DIM(15)
     D  PHCD01                 1      1
     D  PHCD02                 2      2
     D  PHCD03                 3      3
     D  PHCD04                 4      4
     D  PHCD05                 5      5
     D  PHCD06                 6      6
     D  PHCD07                 7      7
     D  PHCD08                 8      8
     D  PHCD09                 9      9
     D  PHCD10                10     10
     D  PHCD11                11     11
     D  PHCD12                12     12
     D  PHCD13                13     13
     D  PHCD14                14     14
     D  PHCD15                15     15
     D                 DS
     D  PTI                    1     90  0
     D                                     DIM(15)
     D  PHTM01                 1      6  0
     D  PHTM02                 7     12  0
     D  PHTM03                13     18  0
     D  PHTM04                19     24  0
     D  PHTM05                25     30  0
     D  PHTM06                31     36  0
     D  PHTM07                37     42  0
     D  PHTM08                43     48  0
     D  PHTM09                49     54  0
     D  PHTM10                55     60  0
     D  PHTM11                61     66  0
     D  PHTM12                67     72  0
     D  PHTM13                73     78  0
     D  PHTM14                79     84  0
     D  PHTM15                85     90  0
      *
     D                 DS
     D  SSD                    1    105  0
     D                                     DIM(15)
     D  SHDT01                 1      7  0
     D  SHDT02                 8     14  0
     D  SHDT03                15     21  0
     D  SHDT04                22     28  0
     D  SHDT05                29     35  0
     D  SHDT06                36     42  0
     D  SHDT07                43     49  0
     D  SHDT08                50     56  0
     D  SHDT09                57     63  0
     D  SHDT10                64     70  0
     D  SHDT11                71     77  0
     D  SHDT12                78     84  0
     D  SHDT13                85     91  0
     D  SHDT14                92     98  0
     D  SHDT15                99    105  0
     D                 DS
     D  STI                    1     90  0
     D                                     DIM(15)
     D  SHTM01                 1      6  0
     D  SHTM02                 7     12  0
     D  SHTM03                13     18  0
     D  SHTM04                19     24  0
     D  SHTM05                25     30  0
     D  SHTM06                31     36  0
     D  SHTM07                37     42  0
     D  SHTM08                43     48  0
     D  SHTM09                49     54  0
     D  SHTM10                55     60  0
     D  SHTM11                61     66  0
     D  SHTM12                67     72  0
     D  SHTM13                73     78  0
     D  SHTM14                79     84  0
     D  SHTM15                85     90  0
     D                 DS
     D  SSC                    1     15
     D                                     DIM(15)
     D  SHCD01                 1      1
     D  SHCD02                 2      2
     D  SHCD03                 3      3
     D  SHCD04                 4      4
     D  SHCD05                 5      5
     D  SHCD06                 6      6
     D  SHCD07                 7      7
     D  SHCD08                 8      8
     D  SHCD09                 9      9
     D  SHCD10                10     10
     D  SHCD11                11     11
     D  SHCD12                12     12
     D  SHCD13                13     13
     D  SHCD14                14     14
     D  SHCD15                15     15
     D*
     D*
     D P00           E DS                  EXTNAME(WCPOLS00)
     D P01           E DS                  EXTNAME(WCPOLS01)
     D P02           E DS                  EXTNAME(WCPOLS02)
     D P03           E DS                  EXTNAME(WCPOLS03)
     D P04           E DS                  EXTNAME(WCPOLS04)
     D P05           E DS                  EXTNAME(WCPOLS05)
     D P06           E DS                  EXTNAME(WCPOLS06)
     D P07           E DS                  EXTNAME(WCPOLS07)
     D P08           E DS                  EXTNAME(WCPOLS08)
     D P09           E DS                  EXTNAME(WCPOLS09)
     D P10           E DS                  EXTNAME(WCPOLS10)
     D P42           E DS                  EXTNAME(WCPOLS42)
     D P87           E DS                  EXTNAME(WCPOLS87)
     D P88           E DS                  EXTNAME(WCPOLS88)
     D P89           E DS                  EXTNAME(WCPOLS89)
     D P11           E DS                  EXTNAME(WCPOLS11)
     D P12           E DS                  EXTNAME(WCPOLS12)
      *** Beg Add ***** 11/15/22 ****************************
     D P13           E DS                  EXTNAME(WCPOLS13)
      *** End Add ***** 11/15/22 ****************************
     D P15           E DS                  EXTNAME(WCPOLS15)
     D P19           E DS                  EXTNAME(WCPOLS19)
     D P21           E DS                  EXTNAME(WCPOLS21)
     D P24           E DS                  EXTNAME(WCPOLS24)
     D P25           E DS                  EXTNAME(WCPOLS25)
     D P27           E DS                  EXTNAME(WCPOLS27)
     D P29           E DS                  EXTNAME(WCPOLS29)
     D P30           E DS                  EXTNAME(WCPOLS30)
     D P36           E DS                  EXTNAME(WCPOLS36)
     D P37           E DS                  EXTNAME(WCPOLS37)
     D P38           E DS                  EXTNAME(WCPOLS38)
     D P39           E DS                  EXTNAME(WCPOLS39)
     D P41           E DS                  EXTNAME(WCPOLS41)
     D P43           E DS                  EXTNAME(WCPOLS43)
     D PBI           E DS                  EXTNAME(WCPOLSBI)
     D PAG           E DS                  EXTNAME(WCPOLSAG)
     D PAH           E DS                  EXTNAME(WCPOLSAH)
     D PBJ           E DS                  EXTNAME(WCPOLSBJ)
     D PBD           E DS                  EXTNAME(WCPOLSBD)
     D PCE           E DS                  EXTNAME(WCPOLSCE)
     D PDR           E DS                  EXTNAME(WCPOLSDR)
     D PDB           E DS                  EXTNAME(WCPOLSDB)
     D PDC           E DS                  EXTNAME(WCPOLSDC)
     D PDD           E DS                  EXTNAME(WCPOLSDD)
     D PDE           E DS                  EXTNAME(WCPOLSDE)
     D PDK           E DS                  EXTNAME(WCPOLSDK)
     D PDM           E DS                  EXTNAME(WCPOLSDM)
     D PDO           E DS                  EXTNAME(WCPOLSDO)
     D PDQ           E DS                  EXTNAME(WCPOLSDQ)
     D PFA           E DS                  EXTNAME(WCPOLSFA)
     D PEC           E DS                  EXTNAME(WCPOLSEC)
      *** Begin Add *** 01/02/24 *** WC2938 **************************
     D PED           E DS                  EXTNAME(WCPOLSED)
      *** End   Add *** 01/02/24 *** WC2938 **************************
      *** Begin Add *** 10/25/22 *** 103957 **************************
     D PEI           E DS                  EXTNAME(WCPOLSEI)
      *** End   Add *** 10/25/22 *** 103957 **************************
      *** Begin Add *** 09/21/23 *** WC2923 **************************
     D PEJ           E DS                  EXTNAME(WCPOLSEJ)
      *** End   Add *** 09/21/23 *** WC2923 **************************
     D PDL           E DS                  EXTNAME(WCPOLSDL)
     D P44           E DS                  EXTNAME(WCPOLS44)
     D PHC           E DS                  EXTNAME(WCPOLSHC)
     D/COPY WWCPOLDAT4
     D PAI           E DS                  EXTNAME(WCPOLSAI)
     D X1FORM          C                   CONST('WC290409A')
     D X1DESC          C                   CONST('First Managed Care-
     D                                      Option')
     D XXFEIN          C                   CONST('391833695')
     D PN4992          C                   CONST('PN049902B')
     D P07REC          DS
     D  P7LNK                  1     43
     D  P7ST                  44     45
     D  P7RTP                 46     47
     D  P7FU1                 48     50
     D  PPA                   51    270
     D                                     DIM(11)
     D                 DS
     D  XXEND                  1     20
     D  XXEND#                 1      8
     D  XXENDB                 9      9
     D  XXENDE                10     20
     D                 DS
     D  @#FORM                 1     15
     D  XXSTID                 3      4
     D*
     D R99             DS
     D  R99SPC                 1     45
     D  R99RTP                46     47
     D  R99T#R                48     57  0
     D  R99THR                58     65  0
     D  R99TDE                66     73  0
     D  R99TDL                74     81  0
     D  R99SP2                82    300
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D                 DS
     D  ##P2L                  1     30
     D  ##ACC                  1     10  0
     D  ##AGG                 11     20  0
     D  ##DIS                 21     30  0
     D                 DS
     D  ##LNK                  1     43
     D  ##CARR                 1      5  0
     D  ##POL#                 6     23
     D  ##CRT#                24     30
     D  ##PFDT                31     36  0
     D  ##TRDT                37     41  0
     D  ##TRCD                42     43  0
     D                 DS
     D  ##ADRS                 1    101
     D  ##ADDR                 1     60
     D  ##CITY                61     90
     D  ##ST                  91     92
     D  ##ZIP1                93     97
     D  ##ZIP2                98    101
     D                 DS
     D  ##NEL                  1     10
     D  ##ENCD                 1      3  0
     D  ##ST1                  4      5  0
     D  ##ELCD                 6     10  0
     D                 DS
     D  #1POL#                 1     18
     D  NHFND                  1      3  0
     D  NHEMP#                 4     12  0
     D  NHFYR                 13     15  0
     D                 DS
     D  #2POL#                 1     18
     D  CRFND                  1      3  0
     D  CREMP#                 4     12  0
     D  CRFYR                 13     15  0
     D                 DS
     D  #SHPOL                 1     18
     D  SHFND                  1      3  0
     D  SHEMP#                 4     12  0
     D  SHFYR                 13     15  0
     D                 DS
     D  #RVPOL                 1     18
     D  NEPLC1                 1      3  0
     D  NEPLC2                 4     12  0
     D  NEPLC3                13     15  0
     D                 DS
     D  #OPOL                  1     18
     D  EXOFND                 1      3  0
     D  EXOEMP                 4     12  0
     D  ELFYR                 13     15  0
     D                 DS                  INZ
     D  #18POL                 1     18
     D  #18FND                 1      3  0
     D  #18EMP                 4     12  0
     D  #18FYR                13     15  0
     D*
     D                 DS
     D  #RADR                  1    101
     D  NEMA12                 1     40
     D  BLK201                41     60
     D  NEMCT2                61     85
     D  BLK011                86     90
     D  NEMST2                91     92
     D  NEMZP2                93    101  0
     D*
     D                 DS
     D  #NAEL                  1     10
     D  NEENCD                 1      3  0
     D  NEST                   4      5
     D  NEELCD                 6     10  0
     D*
     D                 DS
     D  #PHN#                  1     10
     D  NEAREA                 1      3  0
     D  NEPHN#                 4     10  0
     D*
     D                 DS
     D  NHPFDT                 1      7  0
     D  XXXYR                  1      3  0
     D  XXXMD                  4      7  0
     D*
     D                 DS
     D  NHPTDT                 1      7  0
     D  XX2YR                  1      3  0
     D  XX2MD                  4      7  0
     D                 DS
     D  XXZIP                  1      9  0
     D  XXZIP1                 1      5  0
     D  XXZIP2                 6      9  0
     D W29503          C                   CONST('WC290503B')
     D W00503          C                   CONST('WC000503A')
     D W00106          C                   CONST('WC000106A')
     D W00201          C                   CONST('WC000201A')
     D W00301          C                   CONST('WC000301A')
     D W00311          C                   CONST('WC000311A')
     D W00406          C                   CONST('WC000406A')
     D LUBWV           C                   CONST('LUB0020199')
     D W00401          C                   CONST('WC000401A')
     D W04302          C                   CONST('WC040302A')
     D W37310          C                   CONST('WC370310B')
     D W37311          C                   CONST('WC370310C')
     D W29601          C                   CONST('WC290601A')
     D P06REC          DS
     D  XXDTA1                 1     48
     D  EST                   49    168
     D                                     DIM(60)                              EXCLUDED STATES
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
     D                 DS
     D  S2T                    1     30
     D                                     DIM(15)
     D  S2ST01                 1      2
     D  S2ST02                 3      4
     D  S2ST03                 5      6
     D  S2ST04                 7      8
     D  S2ST05                 9     10
     D  S2ST06                11     12
     D  S2ST07                13     14
     D  S2ST08                15     16
     D  S2ST09                17     18
     D  S2ST10                19     20
     D  S2ST11                21     22
     D  S2ST12                23     24
     D  S2ST13                25     26
     D  S2ST14                27     28
     D  S2ST15                29     30
     D                 DS
     D  S2S                    1     15
     D                                     DIM(15)
     D  S2SS01                 1      1
     D  S2SS02                 2      2
     D  S2SS03                 3      3
     D  S2SS04                 4      4
     D  S2SS05                 5      5
     D  S2SS06                 6      6
     D  S2SS07                 7      7
     D  S2SS08                 8      8
     D  S2SS09                 9      9
     D  S2SS10                10     10
     D  S2SS11                11     11
     D  S2SS12                12     12
     D  S2SS13                13     13
     D  S2SS14                14     14
     D  S2SS15                15     15
     D                 DS
     D  XX0ADR                 1    101
     D  XPSTR                  1     60
     D  XPCITY                61     90
     D  XPST                  91     92
     D  XPZIP                 93    101
     D                 DS
     D  XXDATE                 1      8  0
     D  XXMD                   1      4  0
     D  XXYR                   5      8  0
     D  XXADH8               113    128
     D  XXADH9               129    144
     D XXFTPH          C                   CONST('$!+WORKCOMP+!$')
     D ancBasis        C                   CONST('Estimated Annual Premium')
     d arrType         s              1    DIM(10)
     d arrAcro         s             10    DIM(10)
     d othType         s              1
     d othText         s                   like(P01TON)
     d saveText        s             30
     d strCommand      s            255
     d S3EnCd          s                   like(NAencd) Inz
     d S3namP          s                   like(NAnamp) Inz
     d reportData      s               n   inz(*Off)
     d kkCo#           s                   like(NAco#)  Inz
     d kkFnd           s                   like(NAfnd)  Inz
     d kkEmp#          s                   like(NAemp#) Inz
     d kkFyr           s                   like(NAfyr)  Inz
     d kkUnId          s                   like(NAunId) Inz
     d kkNlKc          s                   like(NAnlkc) Inz
     d QCmdExc         pr                  extpgm('QCMDEXC')
     d command                             const like(strCommand)
     d length                        15  5 const
     D*
     D NANAM4          DS           360
     D  NMA                    1    360
     D                                     DIM(4)
     D  XXNM01                 1     90
     D  XXNM02                91    180
     D  XXNM03               181    270
     D  XXNM04               271    360
     D*
     D*
     D NERNM4          DS           360
     D  RMA                    1    360
     D                                     DIM(4)
     D  XXRM01                 1     90
     D  XXRM02                91    180
     D  XXRM03               181    270
     D  XXRM04               271    360
     D*
     D X1NAM4          DS           360
     D DBFDB           DS
     D  DBRRB                397    400B 0
     D DBFDB2          DS
     D  DBRRB2               397    400B 0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     d/copy sprCompany
     d/copy wprUsrPrms
     d/copy wprEmplr
     d/copy wprGroup
     d*** begin add ***  05/01/23  *******************************************
     d/copy sprString
     d*** end add *****  05/01/23  *******************************************

     D*--------------------------------------------------
     D* Procedure name: WrtStateCxRcd
     D* Purpose:        Write the WI cancelletion record when a WI division...
     D*                 is deleted.
     D*--------------------------------------------------
     D WrtStateCxRcd   PR

     C*
     C*   KEY LIST
     C*
     C     NKEY          KLIST
     C                   KFLD                    SHCO#
     C                   KFLD                    SHFND
     C                   KFLD                    SHEMP#
     C                   KFLD                    SHFYR
     C                   KFLD                    SHUNID
     C*
     C     keyWXNX2L     KLIST
     C                   KFLD                    NXCO#
     C                   KFLD                    NXFND
     C                   KFLD                    NXEMP#
     C                   KFLD                    NXFYR
     C                   KFLD                    NXUNID
     C                   KFLD                    NXST
     C                   KFLD                    NXCLAS
     C                   KFLD                    NXRT
     C                   KFLD                    NXEFDT
     C                   KFLD                    NXEXPP
     C                   KFLD                    NXDESC
     C                   KFLD                    NXCSUF
     C*
     C     EMKEY         KLIST
     C                   KFLD                    SHCO#
     C                   KFLD                    SHFND
     C                   KFLD                    SHEMP#
     C                   KFLD                    SHDIV
     C     EMKY1         KLIST
     C                   KFLD                    SHCO#
     C                   KFLD                    SHFND
     C                   KFLD                    SHEMP#
     C*
     C     SHKEY         KLIST
     C                   KFLD                    XXSTAT            1
     C                   KFLD                    XXTYPE
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     S1KEY         KLIST
     C                   KFLD                    X1STAT            1
     C                   KFLD                    X1TYPE            1
     C                   KFLD                    SHCO#
     C                   KFLD                    SHFND
     C                   KFLD                    SHFYR
     C                   KFLD                    SHEMP#
     C*
     C     SHKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
     C     SHKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C     SHKYL1        KLIST
     C                   KFLD                    SHCO#
     C                   KFLD                    SHFND
     C                   KFLD                    SHEMP#
     C                   KFLD                    SHFYR
     C                   KFLD                    XXTYPE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    SHCO#
     C                   KFLD                    SHFND
     C*
     C*  GENERIC TABLE KLIST FOR RETRIEVAL OF LEGAL NATURE OF INSURED
     C*
     C     GTKY          KLIST
     C                   KFLD                    SHCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
      *
      * KEY LIST FOR THE FUND AUXILIARY FILE
      *
     C     F2KY          KLIST
     C                   KFLD                    EXCO#
     C                   KFLD                    EXOFND
      *
      * KEY LIST FOR THE CARRIER/STATE REPORTING FILE
      *
     C     RPKY          KLIST
     C                   KFLD                    SHCO#
     C                   KFLD                    SHFND
     C                   KFLD                    SHEMP#
     C                   KFLD                    SHDIV
     C                   KFLD                    SHFYR
      *
      * KEY LIST FOR THE EMPLOYER CROSS-REFERENCE FILE
      *
     C     EXKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
      *
      * KEY LIST FOR THE EMPLOYER DETAIL FILE
      *
     C     ELKY          KLIST
     C                   KFLD                    EXCO#
     C                   KFLD                    EXOFND
     C                   KFLD                    EXOEMP
     C                   KFLD                    EXODIV
      *
     C     ELKY2         KLIST
     C                   KFLD                    NECO#
     C                   KFLD                    NEFND
     C                   KFLD                    NEEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    NEFYR
     C     ELKY1         KLIST
     C                   KFLD                    SHCO#
     C                   KFLD                    SHFND
     C                   KFLD                    SHFYR
     C                   KFLD                    SHEMP#
      *
      * RATE SET CODE FILE
      *
     C     RSKY          KLIST
     C                   KFLD                    SHCO#
     C                   KFLD                    ELRTCD
      *
      * KEY LIST FOR THE CARRIER/STATE REPORTING FILE
      *
     C     RPKY0         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C     MUKY          KLIST
     C                   KFLD                    SHCO#
     C                   KFLD                    SHFND
     C                   KFLD                    SHFYR
     C                   KFLD                    SHEMP#
     C                   KFLD                    XXFORM
     C     MUKY1         KLIST
     C                   KFLD                    SHCO#
     C                   KFLD                    SHFND
     C                   KFLD                    SHFYR
     C                   KFLD                    SHEMP#
     C     XUKY          KLIST
     C                   KFLD                    SHCO#
     C                   KFLD                    SHFND
     C                   KFLD                    SHFYR
     C                   KFLD                    SHEMP#
     C                   KFLD                    SHDIV
     C                   KFLD                    XXUNID
     C                   KFLD                    XXFORM
     C     XUKY1         KLIST
     C                   KFLD                    SHCO#
     C                   KFLD                    SHFND
     C                   KFLD                    SHFYR
     C                   KFLD                    SHEMP#
     C                   KFLD                    SHDIV
     C                   KFLD                    XXUNID
     C     keyWXA6L      KLIST
     C                   KFLD                    SHCO#
     C                   KFLD                    SHFND
     C                   KFLD                    SHFYR
     C                   KFLD                    SHEMP#
     C                   KFLD                    XXUNID
     C                   KFLD                    xxAType
     C*
     C     XSKY          KLIST
     C                   KFLD                    SHCO#
     C                   KFLD                    SHFND
     C                   KFLD                    SHEMP#
     C                   KFLD                    SHFYR
     C*
     C     STKY          KLIST
     C                   KFLD                    XXKEY
     C*
     C*  KEY LIST FOR ENDORSEMENT TRANSACTION FILE
     C*
     C     PEKY          KLIST
     C                   KFLD                    SHCO#
     C                   KFLD                    SHFND
     C                   KFLD                    SHEMP#
     C                   KFLD                    SHFYR
     C                   KFLD                    SHUNID
     C*
     C     PEKY1         KLIST
     C                   KFLD                    SHCO#
     C                   KFLD                    SHFND
     C                   KFLD                    SHEMP#
     C                   KFLD                    SHFYR
     C                   KFLD                    SHUNID
     C                   KFLD                    XXFORM
     C*
     C*  KEY LIST FOR STATISTICAL RPT/RATING FILE
     C*
     C     RRKY          KLIST
     C                   KFLD                    SHCO#
     C                   KFLD                    SHFND
     C                   KFLD                    XXST              2
     C                   KFLD                    XXEFDT            7 0
     C*
     C     RRKY1         KLIST
     C                   KFLD                    SHCO#
     C                   KFLD                    SHFND
     C                   KFLD                    XXST
     C*
     C     STKY2         KLIST
     C                   KFLD                    XXNST             2
     C*
     C*
     C     TBKY          KLIST
     C                   KFLD                    SHCO#
     C                   KFLD                    XXKEY
     C*
     C*  KEY LIST FOR LIMITS FILE
     C*
     C     LMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    RSJRST
     C                   KFLD                    XXILCD           10
     C                   KFLD                    XXILEF            7 0
     C*
     C     LMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    RSJRST
     C                   KFLD                    XXILCD
     C*
     C*
     C*  OFFICE MASTER
     C*
     C     OFKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    EMOFF#
     C*
     C* GROUP SELECTION FILE KEY LIST
     C*
     C     PGKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C* EMPLOYER HISTORY FILE KEY LIST
     C*
     C     EHKY          KLIST
     C                   KFLD                    SHCO#
     C                   KFLD                    SHFND
     C                   KFLD                    SHEMP#
     C                   KFLD                    SHDIV
     C                   KFLD                    SHFYR
     C                   KFLD                    SHTRDT
     C     EHKY1         KLIST
     C                   KFLD                    SHCO#
     C                   KFLD                    SHFND
     C                   KFLD                    SHEMP#
     C                   KFLD                    SHDIV
     C                   KFLD                    SHFYR
     C*
     C     ELKY3         KLIST
     C                   KFLD                    SHCO#
     C                   KFLD                    SHFND
     C                   KFLD                    SHEMP#
     C                   KFLD                    SHDIV
     C                   KFLD                    SHFYR
     C     ELKY4         KLIST
     C                   KFLD                    SHCO#
     C                   KFLD                    SHFND
     C                   KFLD                    SHEMP#
     C                   KFLD                    PHDIV
     C                   KFLD                    SHFYR
     C*
     C* SUBMISSION HISTORY DETAIL KEY LIST
     C*
     C     S1KY          KLIST
     C                   KFLD                    SHCO#
     C                   KFLD                    SHFND
     C                   KFLD                    SHEMP#
     C                   KFLD                    SHFYR
     C                   KFLD                    X2TYPE            1
     C                   KFLD                    X2ST              2
     C                   KFLD                    X2DATE            7 0
     C                   KFLD                    X2TIME            6 0
     C*
     C     S1KY1         KLIST
     C                   KFLD                    SHCO#
     C                   KFLD                    SHFND
     C                   KFLD                    SHEMP#
     C                   KFLD                    SHFYR
     C                   KFLD                    X2TYPE
     C                   KFLD                    X2ST
     C*
     C     NAKY          KLIST
     C                   KFLD                    SHCO#
     C                   KFLD                    SHFND
     C                   KFLD                    SHEMP#
     C                   KFLD                    SHFYR
     C                   KFLD                    XXUNID
     C*** begin add ***  03/08/24  **************************************
     C*
     C     NAKY1         KLIST
     C                   KFLD                    SHCO#
     C                   KFLD                    SHFND
     C                   KFLD                    SHEMP#
     C                   KFLD                    SHFYR
     C                   KFLD                    SHUNID
     C                   KFLD                    svnlkc
     C                   KFLD                    svencd
     C*** end add *****  03/08/24  **************************************
     C     crky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxst
     C                   kfld                    xxkey1
     C                   kfld                    cxcdat
     C*
     C     crky1         klist
     C                   kfld                    xxco#
     C                   kfld                    xxst
     C                   kfld                    xxkey1
      *
     C     cxky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    shemp#
     C                   kfld                    shdiv
     C                   kfld                    shfyr
     C                   kfld                    crefdt
      *
     C     cxky1         klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    shemp#
     C                   kfld                    shdiv
     C                   kfld                    shfyr
     C*   Key List State Code file for Reporting
     C*
     C     keyWTSTL      KLIST
     C                   KFLD                    xxCO#
     C                   KFLD                    svState
     C                   KFLD                    svStCd
     C*
     C*
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         XXKEY
     C     *LIKE         DEFINE    SHEMP#        XXEMP#
     C     *LIKE         DEFINE    SHTRDT        XXTRDH
     C     *LIKE         DEFINE    SHTRDT        XXWKDT
     C     *LIKE         DEFINE    MUFORM        XXFORM
     C*** begin add ***  05/09/22  ************************************
     C     *LIKE         DEFINE    MUFORM        svFORM
     C*** end add *****  05/09/22  ************************************
     C     *LIKE         DEFINE    NAUE#         XXUE#
     C     *LIKE         DEFINE    SHEMP#        SVEMP#
     C     *LIKE         DEFINE    SHFYR         SVFYR
     C     *LIKE         DEFINE    SHUNID        XXUNID
     C     *LIKE         DEFINE    NANAME        X1NAME
     C     *LIKE         DEFINE    NANAM2        X1NAM2
     C     *LIKE         DEFINE    NAFEIN        X1FEIN
     C     *LIKE         DEFINE    NAMAD1        X1MAD1
     C     *LIKE         DEFINE    NAMAD2        X1MAD2
     C     *LIKE         DEFINE    NAMCTY        X1MCTY
     C     *LIKE         DEFINE    NAMST         X1MST
     C     *LIKE         DEFINE    NAMZIP        X1MZIP
      *
      **************************************************************************
      *
     C                   Z-ADD     0             Q                 3 0
     C                   Z-ADD     0             QNXT              3 0
     C                   Z-ADD     500           XXMAXQ            3 0
     C                   Z-ADD     0             XXDAYS            5 0
     C                   Z-ADD     0             X                 1 0
     C                   Z-ADD     0             Y                 1 0
      *
     C                   MOVE      *BLANKS       XXSIC             6
     C*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    XXCO#             3 0
     C                   PARM                    XXFND             3 0
     C                   PARM                    QQTOTR            9
     C                   PARM                    QQTOTH            9
     C                   PARM                    QQER              1
     C                   PARM                    QQST              2
     C                   PARM                    QQMAIL           31
     C                   PARM                    QQNAME           25
     C                   PARM                    QQPHN            10
     C                   PARM                    QQEXT             6
     C                   PARM                    QQFAX            10
     C                   PARM                    QQADR           101
     C                   PARM                    QQTLNP            9
     C                   PARM                    QQTLRP            9
     C                   PARM                    QQTLCX            9
     C                   PARM                    QQTLRE            9
     C                   PARM                    QQTLEN            9
     C                   PARM                    QQPEEP            1
     C                   PARM                    QQOLDCAR          1
     c*** Beg   Chg *** 03/11/22 *******************************************
     C                   PARM                    qqDtaPvdr         1
     C                   PARM                    qqFein            9
     c*** End   Chg *** 03/11/22 *******************************************
     C*
     C     POLPL         PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQFYR             3
     C                   PARM      'Y'           QQREIS            1
     C                   PARM      *BLANKS       QQQUE            10
     C                   PARM      '0'           QQSTEP            1
     C                   PARM      'N'           QQDART            1
     C                   PARM      'N'           QQHARD            1
     C                   PARM      'N'           QQAGTO            1
     C                   PARM      '01'          QQCPY             2
     C                   PARM      'B'           QQRNB             1
     C                   PARM      *BLANKS       QQDMBR           10
     C                   PARM      *BLANKS       QQFINA            1
     C                   PARM      'N'           QQPDF             1
     C                   PARM      *BLANKS       QQPRT            10
     C                   PARM      *BLANKS       QQPRT1           10
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
     C     DTCYM3        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*YMD'        SRTFMT
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
     C*
     C     DTMDY2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   CLEAR                   P00
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
     C                   CLEAR                   P01
     C                   CLEAR                   P02
     C                   CLEAR                   P03
     C                   CLEAR                   P04
     C                   CLEAR                   P05
     C                   CLEAR                   P06
     C                   CLEAR                   P07
     C                   CLEAR                   P08
     C                   CLEAR                   P10
     C                   CLEAR                   P42
     C                   CLEAR                   P87
     C                   CLEAR                   P88
     C                   CLEAR                   P89
     C                   CLEAR                   R99
     C                   CLEAR                   DSDTTM
     C                   CLEAR                   ##P2L
     C                   CLEAR                   ##LNK
     C                   CLEAR                   ##ADRS
     C                   CLEAR                   ##NEL
     C                   CLEAR                   #1POL#
     C                   CLEAR                   #2POL#
     C                   CLEAR                   #SHPOL
     C                   CLEAR                   #RVPOL
     C                   CLEAR                   #NAEL
     C                   CLEAR                   #PHN#
     C*** begin add ***  03/08/24  ******************************************
     C                   Clear                   svUESt            2
     C                   If        QQSt <> ' '
     C                   Movel(P)  QQST          XXKEY
     C     STKY          Chain     PST01                              60
     C     *IN60         Ifeq      *OFF
     C                   Move      STNST         svUESt
     C                   Endif
     C                   Endif
     C*** end add *****  03/08/24  ******************************************
     C                   Z-ADD     *HIVAL        XXTDE             7 0
     C                   Z-ADD     *LOVAL        XXTDL             7 0
     C     Q1PRMS        IFGT      17
     C                   MOVEL     QQPEEP        XXPEEP            1
     C                   ENDIF
     C                   Z-ADD     0             XX#ST             2 0
     C* Get the branding information
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*
     C                   MOVE      '1'           *INOA
     C*
        // Get the company#
        company# = UserCo#(q1user);

        // Get the CompanyName
        companyName = getCompNam(company#);

        // Get the User Name
        userName = getUsrName(q1user);
     C*
     C* SET NUMERIC STATE CODE FOR PEEP SUBMISSIONS TO STATE BUREAUS.
     C*
     C     XXPEEP        IFEQ      'Y'
     C     QQST          ANDNE     *BLANKS
     C*
     C                   MOVEL(P)  QQST          XXKEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      STNST         XX#ST
     C                   ENDIF
     C*
     C                   ENDIF
     C                   Z-ADD     0             ##TLNP            9 0
     C                   Z-ADD     0             ##TLRP            9 0
     C                   Z-ADD     0             ##TLCX            9 0
     C                   Z-ADD     0             ##TLRE            9 0
     C                   Z-ADD     0             ##TLEN            9 0
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      *BLANKS       BLK201
     C                   MOVE      *BLANKS       BLK011
     C                   Z-ADD     XXFND         SVFND             3 0
     C*
     C                   MOVE      *OFF          *IN59
     C                   READ      SWCPF                                  60
     C     *IN60         IFEQ      *OFF
     C     WCFL18        ANDEQ     'Y'
     C                   MOVE      *ON           *IN59
     C                   ENDIF
     C                   Clear                   pnPrint           1
     C                   If        %subst(WCSC04:1:1) = 'Y'
     C                   Eval      pnPrint = 'Y'
     C                   Endif
      *
      *   CHANGE PEO SUBMISSION RECORDS IF FLAGGED TO DO SO AT GROUP LEVEL
      *
      **********
      * CHAIN TO FUND AUX FILE TO GET V3 LIVE DATE (F2DT01)
      **********
     C*
     C* WRITE ELECTRONIC TRANSMITTAL RECORD IF PA OR DE SUBMITTAL.
     C     QQST          IFNE      *BLANK
     C     QQMAIL        ORNE      *BLANK
     C*
     C                   MOVE      XXFTPH        P00TFT
     C                   MOVE      QQMAIL        P00EML
     C                   MOVE      'WCP'         P00DTP
      *
     C                   MOVEL(P)  QQST          XXKEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      STNST         P00DRC
     C                   ENDIF
     C     SVFND         IFEQ      *ZERO
      *
     C     PGKY          CHAIN     XPG01                              60
     C     *IN60         DOWEQ     *OFF
     C     PGFLAG        ANDEQ     'E'
     C     PGKY          READE     XPG01                                  60
     C                   ENDDO
      *
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     PGGRP         XXFND
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   ENDIF                                                  SVFND IFEQ
      *
     C     SHKY1         CHAIN     PF201                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     F2CAR#        P00DPC
     C                   ENDIF                                                              OFF
     C                   If        xxClt# = '045' and QQOldCar = 'Y'
     C                   Z-add     33057         P00DPC
     C                   Endif
     C     QQST          IFEQ      *BLANKS
     C     XXCLT#        ANDEQ     '132'
     C     XXFND         ANDEQ     5
     C                   Z-ADD     12985         P00DPC
     C                   ENDIF                                                  QQST IFEQ
      *
     C                   MOVE      'S'           P00STC
     C                   MOVE      'E'           P00TRC
     C                   MOVE      QQNAME        P00DPN
     C                   MOVE      QQPHN         P00CP#
     C                   MOVE      QQEXT         P00CPX
     C                   MOVE      QQFAX         P00FAX
     C                   MOVE      QQPHN         P00CP#
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         XXDATE            8 0
      *
     C                   MOVE      XXDATE        QQFDAT            8
     C                   MOVE      *BLANKS       QQJULD            5
     C                   CALL      'SRJULC'
     C                   PARM                    QQFDAT
     C                   PARM                    QQJULD
     C                   PARM      ' '           QQRTNC            1
     C*
     C     QQRTNC        IFNE      '1'
     C                   MOVEL     QQJULD        P00TVI
     C                   END
     C*
     C                   MOVE      'V01'         P00TVI
     C                   MOVEL     XXYR          P00PDT
     C                   MOVE      XXMD          P00PDT
     C*
     C                   MOVE      QQADR         P00ADR
     C*
     C
                         P00dtc = %Trim(qqDtaPvdr);
                         Monitor;
                            P00fei = %Dec(qqFein:9:0);
                         On-error;
                            P00fei = 0;
                         EndMon;
     C
     C*
     C                   MOVEL     P00           RDATA
     C                   WRITE     RPOLS
     C                   CLEAR                   RDATA
     C                   ADD       1             ##TOTR           10 0
     C*
     C                   ENDIF                                                  QQST IFEQ
     C*
     C*** GET ALL POLICY SUBMISSION REQUESTS ***
     C*
     C                   MOVE      *ON           *IN58
      *
     C     SVFND         IFEQ      *ZERO
      *
     C     PGKY          CHAIN     XPG01                              60
     C     *IN60         DOWEQ     *OFF
     C     PGFLAG        ANDEQ     'E'
     C     PGKY          READE     XPG01                                  60
     C                   ENDDO
      *
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     PGGRP         XXFND
     C                   MOVE      *OFF          *IN58
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   ELSE
     C                   MOVE      *OFF          *IN58
     C                   ENDIF                                                  SVFND IFEQ
      *
     C     *IN58         DOWEQ     *OFF
      *
      *   CHANGE PEO SUBMISSION RECORDS IF FLAGGED TO DO SO AT GROUP LEVEL
      *
     C     SHKY1         CHAIN     PFN01                              60
     C     *IN60         IFEQ      *OFF
     C                   Exsr      ChkEndorse
     C                   EXSR      SRPEOS
     C                   ENDIF
     C*
     C                   MOVE      'P'           XXTYPE            1
     C                   MOVE      '3'           XXSTAT            1
     C*
     C     SHKEY         CHAIN     PSH01                              50
     C*
     C     *IN50         DOWEQ     '0'
     C*** begin add ***  12/12/23  ******************************************
     C* Clear each policy transaction record type before processing policy.
     C                   CLEAR                   P01
     C                   CLEAR                   P02
     C                   CLEAR                   P03
     C                   CLEAR                   P04
     C                   CLEAR                   P05
     C                   CLEAR                   P06
     C                   CLEAR                   P07
     C*** end add *****  12/12/23  ******************************************
     C                   Move      *on           xxProcess         1
     C
     C                   If        xxClt# = '045' and QQOldCar = 'Y'
     C                             and SHPFDT >= 1160101
     C                   Move      *off          xxProcess
     C                   Endif
     C
     C                   If        xxClt# = '045' and QQOldCar <> 'Y'
     C                             and SHPFDT < 1160101
     C                   Move      *off          xxProcess
     C                   Endif
     C
     C                   If        xxProcess = *on
     C     *IN59         IFEQ      *ON
     C     QQST          IFNE      *BLANKS
     C                   MOVE      QQST          XZST              2
     C                   ELSE
     C                   MOVE      'XX'          XZST
     C                   ENDIF
     C                   ENDIF                                                  *IN59 IFEQ *ON
      *
     C                   Z-ADD     1             I
     C     XZST          LOOKUP    SST(I)                                 60
     C     *IN60         IFEQ      *ON
     C     *IN59         IFEQ      *ON
     C                   EXSR      CHKPOL
     C                   ENDIF
      *
     C     *IN69         IFEQ      *ON
     C     *IN59         OREQ      *OFF
     C     *IN59         IFEQ      *ON
     C                   Z-ADD     SSD(I)        XXSBDT            7 0
     C                   Z-ADD     STI(I)        XXSBTM            6 0
     C                   ELSE
     C                   Z-ADD     SHSBDT        XXSBDT            7 0
     C                   Z-ADD     SHSBTM        XXSBTM            6 0
     C                   ENDIF                                                  *IN59 IFEQ
     C*
     C                   MOVE      SHCODE        XXCODE            1
     C     *IN59         IFEQ      *ON
     C     SSC(I)        ANDNE     *BLANK
     C                   MOVE      SSC(I)        XXCODE
     C                   ENDIF                                                  *IN59 IFEQ
     C     QQER          IFNE      *BLANK
     C*
     C*** Beg Chg *** 03/04/24 *****************************************
     C***     EMKEY         CHAIN     PEM01                              60
     C***  N60QQER          CABNE     EMEXRT        NEXTSH
     C     elky3         Chain     pel01                              60
     C  N60QQER          CabNe     ELexRt        nextSh
     C*** End Chg *** 03/04/24 *****************************************
     C                   ENDIF
     C*
     C                   MOVE      *OFF          *IN64
     C*
     C     QQST          IFEQ      'NJ'
     C     XXCLT#        ANDEQ     '114'
     C*
     C  N64
     CAN 64              EXSR      ANNV
     C*
     C*
     C                   ENDIF                                                  QQST IFEQ
     C*
     C*
     C*
     C*
     C*
     C     *IN64         IFEQ      *OFF
     C                   Z-ADD     *ZERO         CSQCNT            4 0
     C                   Z-ADD     *ZERO         CS8CNT            4 0
     C     *LIKE         DEFINE    NAENCD        S2ENCD
     C                   CLEAR                   S2ENCD
     C                   MOVE      *BLANK        XXNY              1
     C                   MOVE      *BLANK        xxWIShort         1
     C                   Z-add     0             xxBurRate         1 0
     C                   Eval      xxTBID = 'NCCI'
     C*
     C     ELKY1         CHAIN     PEL02                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     RSJRST        IFEQ      'NY'
     C                   MOVE      'Y'           XXNY
     C                   ENDIF                                                  RSJRST IFEQ
     C     RSJRST        IFEQ      'WI'
     C     ELCOFL        ANDEQ     'Y'
     C                   MOVE      'Y'           xxWIShort
     C                   Endif
     C*
     C* Determine experience rating for header record based on a division
     C* written within the bureau being reported to.
     C*
     C                   If        xxBurRate = 0
     C*
     C                   Eval      xxKey = RSJRST
     C     GTKY          CHAIN     pgt01                              60
     C*
     C* Division is written within state of bureau, or NCCI.
     C*
     C                   If        RSJRST = QQST or QQST = '  ' and *in60 = *off
     C*
     C* Set to not-rated if no modifier, intrastate rated otherwise.
     C*
     C*** Beg Chg *** 03/04/24 *****************************************
     C*************      If        ELexm1 = 0 or ELnoexm1 = 'Y'
     C*************      Z-add     5             xxBurRate
     C*************      Else
     C*************      Z-add     3             xxBurRate
     C*************      Endif
       If ELexRt <> ' ';
          xxBurRate = %Dec(ELexRt:1:0);
       Endif;
     C*** End Chg *** 03/04/24 *****************************************
     C*
     C                   Endif
     C*
     C                   Endif
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     ELKY1         READE     PEL02                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C****** TRANSLATE TODAY TO A JULIAN DATE ******
     C*
     C                   Z-ADD     SHTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         #@FDAT            8 0
     C                   MOVE      #@FDAT        QQFDAT            8
     C                   MOVE      *BLANKS       QQJULD            5
     C                   CALL      'SRJULC'
     C                   PARM                    QQFDAT
     C                   PARM                    QQJULD
     C                   PARM      ' '           QQRTNC            1
     C*
     C     QQRTNC        IFNE      '1'
     C                   MOVE      QQJULD        XXDTJ             5 0
     C                   ELSE
     C                   MOVE      *ZEROS        XXDTJ
     C                   END
     C*
     C     RPKY          CHAIN     PEL01                              60

      /free
       //Retrieve Generate state CX transaction on state removal flag
       flgGenCX = *Blanks;
       flatDel = *blank;
       If QQst <> *Blanks;
          Setgt (SHCo#:SHfnd:QQst:SHpfdt) pxc01;
          Readpe (SHCo#:SHfnd:QQst) pxc01;
          If not %eof and XCstcx = 'Y';
             flgGenCX = 'Y';
          Endif;
       Endif;
      /end-free
      /free
       // Initilize variables & arrays. Call subroutine SRSTATE.
       Clear xxTR15;
       Clear xxRENW;
       Clear priorSTATE;
       Clear deletionFLAG;
       Clear currentSTATE;
       Clear currentSTATUS;
       Exsr SRstate;
      /end-free
     C*
     C* Copy policy's endorsements over to WMMUL7 for type 07 records.
     C* If any national forms are to be excluded from a state on the
     C* policy, write state versions for every other state to the file
     C* instead of the single "all states" record.
     C*
      /free

          strCommand = 'SYDUPFILE FROM(WMMUP) TO(QTEMP/WMMUP) DATA(*NO) +
           LGL(*NO)';
       //*** begin add ***  01/31/22  ************************************
          Monitor;
       //*** end add *****  01/31/22  ************************************
          Callp QCMDEXC(strCommand:%len(strCommand));
       //*** begin add ***  01/31/22  ************************************
          On-error;
          Endmon;
       //*** end add *****  01/31/22  ************************************

          Monitor;
             clear strCommand;
             strCommand = 'OVRDBF FILE(WMMUP) TOFILE(QTEMP/WMMUP)';
             Callp QCMDEXC(strCommand:%len(strCommand));
          On-error;
          Endmon;

          strCommand = 'SYDUPFILE FROM(WMMUL7) TO(QTEMP/WMMUL7) DATA(*NO) +
           LGL(*NO)';
       //*** begin add ***  01/31/22  ************************************
          Monitor;
       //*** end add *****  01/31/22  ************************************
          Callp QCMDEXC(strCommand:%len(strCommand));
       //*** begin add ***  01/31/22  ************************************
          On-error;
          Endmon;
       //*** end add *****  01/31/22  ************************************

          Monitor;
             clear strCommand;
             strCommand = 'OVRDBF FILE(WMMUL7) TOFILE(QTEMP/WMMUL7)';
             Callp QCMDEXC(strCommand:%len(strCommand));
          On-error;
          Endmon;

          Open wmmul7;

          //* Clear temporary file of records before writing to it.
          Setll muky1 pmu02;
          Reade muky1 pmu02;
          Dow not %eof;
             Delete pmu02;
             Reade muky1 pmu02;
          Enddo;

          Setll muky1 pmu01;
          Reade(n) muky1 pmu01;
          Dow not %eof;

             Clear writeExclude;

          //* Process if form is national.
             If MUEnst = '  ';

                Setll  (MUco#:MUform) pes00;
                Reade  (MUco#:MUform) pes00;
             //* Flag if form is set to excluded on any state on policy.
                If not %eof;
                   xxApproved = ESapex;
                   Setll NKEY PNH01;
                   Reade(n) NKEY PNH01;
                   Dow not %eof(WXNHL);
                         Chain (MUCo#:MUForm:NHSt) pes00;
                      //*** begin chg ***  08/11/23  **********************************
                      //*****   If (%found and xxApproved = 'E') or
                      //*****    (not %found and xxApproved = 'A');
                         If ESExdt = 0;
                            ESExdt = *hival;
                         Endif;
                         //* State is excluded if form is exclusion basis and the
                         //* exclusion basis begins on/before state's eff date and is
                         //* not expired as of state's eff date, or if form is on
                         //* approval basis and either state is not approved or
                         //* state is approved but approval is after state's eff date
                         //* or state's approval expires before state's eff date.

                         If (%found and xxApproved = 'E' and ESADte <= NHPfdt
                          and NHPfdt <= ESExdt) or (not %found and xxApproved
                          = 'A') or (%found and xxApproved = 'A' and (ESADte
                          > NHPfdt or NHPfdt > ESExdt));
                      //*** end chg *****  08/11/23  **********************************
                            writeExclude = 'Y';
                            Leave;
                         Endif;
                      Reade(n) NKEY PNH01;
                   Enddo;
                Endif;

                If writeExclude = 'Y';

                //* Write record for each state not flagged as excluded.
                   Setll NKEY PNH01;
                   Reade(n) NKEY PNH01;
                   Dow not %eof(WXNHL);
                         Chain (MUCo#:MUForm:NHSt) pes00;
                      //*** begin chg ***  08/11/23  **********************************
                      //*****   If (not %found and NHSt <> '**' and xxApproved = 'E')
                      //*****    or (%found and NHSt <> '**' and xxApproved = 'A');
                         If ESExdt = 0;
                            ESExdt = *hival;
                         Endif;
                         //* Write record if on exclusion basis and no record for
                         //* the state, or there is a record and it is either not
                         //* adopted yet or its adoption has expired, or if on
                         //* approved basis and there is a record and its both
                         //* in effect and not expired yet.

                         If (not %found and NHSt <> '**' and xxApproved = 'E')
                          or (%found and NHSt <> '**' and xxApproved = 'E'
                          and (ESADte > NHPfdt or NHPfdt > ESExdt))
                          or (%found and NHSt <> '**' and xxApproved = 'A' and
                          ESADte <= NHPfdt and NHPfdt <= ESExdt);
                      //*** end chg *****  08/11/23  **********************************
                            MUEnst = NHSt;
                            Write pmu02;
                         Endif;
                      Reade(n) NKEY PNH01;
                   Enddo;

                Endif;

             Endif;

          //* Write national or state form record.
             If writeExclude <> 'Y';
                Write pmu02;
             Endif;

             Reade(n) muky1 pmu01;
          Enddo;
      /end-free
     C*
     C* Check if there are multiple legal natures and concatenate their
     C* acronyms into the "Text for Other" field in the header record.
      /free
        Clear othType;
        Clear othText;
        Clear saveText;
        Clear arrType;
        Clear arrAcro;
        index = 1;
        xxTBID = 'BUSI';

        //* Build information for WI only.
        If QQSt = 'WI';

           Setll nkey pna01;
           Reade(n) nkey pna01;
           Dow not %eof;
              //* Include legal nature if populated and not producer info.
              If NAENCD <> 999 and NALEGN <> ' ' and NAName <> ' ';
                 xxKey = NALEGN;
                 Chain(n) gtky pgt01;
                 If %found;
                    //* Build unique list of legal natures and their acronyms.
                    If %lookup(NALEGN:arrType) = 0;
                       arrType(index) = NALEGN;
                       If GTCF01 <> ' ';
                          arrAcro(index) = GTCF01;
                       Else;
                          arrAcro(index) = %subst(GTDesc:1:10);
                       Endif;
                       index = index + 1;
                       //* Flag for Other type if more than one legal nature.
                       If index > 2;
                          othType = 'Y';
                       Endif;
                       If index > 9;
                          Leave;
                       Endif;
                    Endif;
                 Endif;
              Endif;
              Reade(n) nkey pna01;
           Enddo;

           //* Build Other text up to 20 characters from acronym array if needed.
           If othType = 'Y';
              counter1 = 2;
              saveText = %trim(arrAcro(1));
              Dow counter1 < index;
                 saveText = %trim(saveText) + ',' + %trim(arrAcro(counter1));
                 If %len(%trim(saveText)) >= 20;
                    Leave;
                 Endif;
                 counter1 = counter1 + 1;
              Enddo;
              othText = %subst(saveText:1:20);
           Endif;

        Endif;

      /end-free
      /free
       // Initilize variables & arrays. Call subroutine SRSTATE.
      /end-free
     C*
     C* If the current status of a state is to be deleted, or previously
     C* reported as deleted, then total that premium to be removed.
     C*
     C                   Z-add     0             delStPrem        11 2
      /free
        Setll NKEY PNH01;
        Reade(n) NKEY PNH01;
        Dow not %eof(WXNHL);
           index = %LookUp(NHst:currentState);
           Clear xxStatus;
           If index <> 0;
              xxStatus = currentStatus(index);
           Endif;
           If xxStatus = 'P' or xxStatus = 'D';
              delStPrem = delStPrem + NHAnpr;
           Endif;
           Reade(n) NKEY PNH01;
        Enddo;
      /end-free
     C*
     C     NKEY          CHAIN(n)  PNH01                              60
     C     *IN60         IFEQ      '0'
     C* * MOVE FIELDS * * *
     C                   MOVE      NHCAR#        ##CARR
     C                   If        xxClt# = '045' and QQOldCar = 'Y'
     C                   Z-add     33057         ##CARR
     C                   Endif
     C     QQST          IFEQ      *BLANKS
     C     XXCLT#        ANDEQ     '132'
     C     NHFND         ANDEQ     5
     C                   Z-ADD     12985         ##CARR
     C                   ENDIF                                                  QQST IFEQ
     C                   MOVEL     #1POL#        ##POL#
     C                   EXSR      SRPOL#
     C                   MOVE      NHPFDT        ##PFDT
     C                   MOVE      XXDTJ         ##TRDT
     C*** TRANS CODE HERE!
      *
     C                   SETOFF                                       66
      *
     C                   SELECT
      *
     C     XXCODE        WHENEQ    'R'
     C                   MOVE      02            ##TRCD                          RENEWAL
      *
     C     XXCODE        WHENEQ    'I'
     C                   MOVE      14            ##TRCD                          REISSUE
     C     QQST          IFEQ      'PA'
     C     QQST          OREQ      'DE'
     C     QQST          OREQ      'MA'
     C     QQST          OREQ      'WI'
     C                   MOVE      08            ##TRCD                          REWRITE
     C                   ENDIF
      *
     C     XXCODE        WHENEQ    'W'
     C                   MOVE      06            ##TRCD                          REWRITE
      *
     C     XXCODE        WHENEQ    'K'
     C                   MOVE      06            ##TRCD                          REWRITE
     C                   SETON                                        66
     C     XXCODE        WHENEQ    'T'
     C                   MOVE      08            ##TRCD
     C     QQST          IFNE      'PA'
     C     QQST          ANDNE     'DE'
     C     QQST          ANDNE     'MA'
     C     QQST          ANDNE     'WI'
     C                   MOVE      14            ##TRCD
     C                   ENDIF                                                  QQST IFNE
     C     XXCODE        WHENEQ    'N'
     C                   MOVE      10            ##TRCD
     C     QQST          IFNE      'PA'
     C     QQST          ANDNE     'DE'
     C     QQST          ANDNE     'MA'
     C     QQST          ANDNE     'WI'
     C                   MOVE      14            ##TRCD
     C                   ENDIF                                                  QQST IFNE
      *
     C                   OTHER
     C                   MOVE      01            ##TRCD                          ORIGINAL
      *
     C                   ENDSL                                                  SHCODE WHEQ
     C                   Clear                   polSkip           1
      /free
       If xxTR15 = 'Y';
          If ##TRCD = 08 or ##TRCD = 14 or ##TRCD = 10 or
           xxCode = 'A';
          //*** begin chg ***  11/04/22  ******************************
          //*****  Index = %lookup(QQst:priorState);
            index = %lookup(QQst:currentState);
          //*** end chg *****  11/04/22  ******************************
            Select;
            When flgGenCX = 'Y' and index > 0 and CurrentStatus(index) = 'D';
               xxTR15 = ' ';
            Other;
             ##TRCD = 15;
            Endsl;
          Endif;

          //*** begin chg ***  11/04/22  ******************************
          //*****Index = %lookup(QQst:priorState);
          index = %lookup(QQst:currentState);
          //*** end chg *****  11/04/22  ******************************
          If Index > 0 and CurrentStatus(Index) = 'D' and flgGenCX = 'Y';
             polSkip = 'Y';
             WrtStateCxRcd();
          Endif;

       Endif;

       //*** begin add ***  03/10/23  ******************************************
       //* Only perform this processing for specific clients.
       Select;
       When xxClt# = '148';
       //*** end add *****  03/10/23  ******************************************
       //* Resubmissions for CA should be 06 re-write unless all transactions which
       //* triggered this resubmission are mid-term endorsements with WC890600* form.
          If ##TRCD = 14 and QQSt = 'CA' and SHReq# = 0;
             ##TRCD = 06;
          //*** begin add ***  02/04/22  ******************************************
             *in66 = *on;
             SHSbdt = SHPfdt;
          //*** end add *****  02/04/22  ******************************************
          Endif;
       //*** begin add ***  03/10/23  ******************************************
       Endsl;
       //*** end add *****  03/10/23  ******************************************
      /end-free
     C*
     C*  If generating a state CX record, prevent the policy transaction from
     C*  generating and just update sent status to '4' below at normal location.
     C*
     C                   If        polSkip <> 'Y'
     C                   MOVE      *BLANK        XSREIS            1
     C     ##TRCD        IFEQ      08
     C     ##TRCD        OREQ      10
     C     ##TRCD        OREQ      14
     C     ##TRCD        OREQ      15
     C                   MOVE      'Y'           XSREIS
     C                   ENDIF                                                  ##TRCD IFEQ
     C*
     C                   MOVE      ##LNK         P01LNK
     C                   MOVE      '01'          P01RTP
     C                   MOVE      3             P01EXI
     C***                IF        ELEXM1 = *ZERO
     C*** Beg Chg *** 03/04/24 *****************************************
     C*************      If        ELexm1 = 0 or ELnoexm1 = 'Y'
     C*************      MOVE      5             P01EXI
     C*************      ENDIF
       If ELexRt <> ' ';
          p01ExI = %Dec(ELexRt:1:0);
       Endif;
     C*** End Chg *** 03/04/24 *****************************************
     C*
     C* Set to bureau rating if it has been set.
     C*
     C                   If        xxBurRate <> 0
     C                   Z-add     xxBurRate     P01EXI
     C                   Endif
     C                   MOVE      *ZEROS        P01IRI
     C*** Begin Add *** 05/24/22 *******************************************
     C     EMKEY         CHAIN     PEM01                              61
     C     *IN61         IFEQ      '0'
     C                   MOVEL     EMRSK#        XXIR#             9
     C                   TESTN                   XXIR#                61
     C     *IN61         IFEQ      *ON
     C                   MOVE      XXIR#         P01IRI
     C                   ELSE
     C                   MOVE      *ZEROS        P01IRI
     C                   ENDIF
     C                   ELSE
     C                   MOVE      *ZEROS        P01IRI
     C                   ENDIF
     C*** End   Add *** 05/24/22 *******************************************
     C                   MOVE      NHPTDT        P01PED
     C                   MOVE      NHSIP         P01SIP
     C                   MOVE      NHPTI         P01PTI
     C                   MOVE      NHLPI         P01LPI
     C     XXXMD         IFEQ      XX2MD
     C                   MOVE      1             P01PTP
     C                   ELSE
     C                   MOVE      4             P01PTP
     C                   END
     C*
     C                   MOVEL     *BLANKS       P01PP#
     C     NHPP#         IFNE      *BLANKS
     C                   SELECT
     C     QQST          WHENEQ    'CA'
     C                   MOVE      *ON           STRIP
     C     XXCLT#        WHENEQ    '129'
     C     XXFND         ANDEQ     1
     C                   MOVE      *ON           STRIP
     C     XXCLT#        WHENNE    '129'
     C                   MOVE      *ON           STRIP
     C                   OTHER
     C                   MOVE      *OFF          STRIP             1
     C                   ENDSL
     C*
     C     STRIP         IFEQ      *ON
     C*
     C                   CLEAR                   CAP
     C                   CLEAR                   CAP2
     C                   MOVEA     NHPP#         CAP
     C                   Z-ADD     1             T                 2 0
     C                   Z-ADD     1             U                 2 0
     C*
     C     T             DOWLE     25
     C*
     C     CAP(T)        IFNE      '-'
     C     u             andle     18
     C                   MOVE      CAP(T)        CAP2(U)
     C                   ADD       1             U
     C                   ENDIF
     C*
     C                   ADD       1             T
     C                   ENDDO
     C*
     C                   MOVEA     CAP2          NHPP#
     C*
     C                   ENDIF
     C                   MOVEL(P)  NHPP#         P01PP#
     C                   END
     C                   MOVE      *BLANKS       P01PC#
     C*
     C                   MOVE      *BLANKS       P01TON
     C***** TRANSLATE TOB  TO LEGAL NATURE OF EMPLOYER
     C                   MOVEL(P)  'BUSI'        XXTBID
     C                   MOVEL(P)  NHTPOB        XXKEY
     C*
     C     GTKY          CHAIN     PGT01                              60
     C*
     C*  IF RECORD FOUND AND NCCI VALUE WITHIN VALID RANGE, MOVE THE
     C*  NCCI CODE TO THE NCCI LEGAL NATURE OF EMPLOYER CODE
     C*
     C     *IN60         IFEQ      *OFF
     C     GTN501        ANDGE     1
     C     GTN501        ANDLE     98
     C     othType       ANDNE     'Y'
     C                   Z-ADD     GTN501        P01LNI
     C*
     C                   ELSE
     C*
     C*  IF RECORD WAS NOT FOUND, OR THE NCCI CODE IS "99" (OTHER),
     C*  LOAD "99" TO THE NCCI LEGAL NATURE OF EMPLOYER CODE, AND
     C*  EMPLOYER'S NATURE OF BUSINESS DESCRIPTION
     C*
     C                   MOVE      99            P01LNI
     C     EMKEY         CHAIN     PEM01                              61
     C     *IN61         IFEQ      '0'
     C                   MOVEL     EMNAOB        P01TON
     C                   ENDIF
     C                   If        othType = 'Y'
     C                   Eval      P01TON = othText
     C                   Endif
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       SVNAOB           40
     C     QQST          IFEQ      'CA'
     C*
     C     EMKEY         CHAIN     PEM01                              61
     C     *IN61         IFEQ      '0'
     C                   MOVEL     EMNAOB        SVNAOB
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   MOVE      1             P01TOB
     C                   MOVE      2             P01WUP
      /free
         P01BSI# = %DEC(%Editc(NHBSI#:'X'):7:0);
      /end-free
     C                   Z-ADD     NHMNPR        P01PMP
     C                   MOVE      NHMPST        P01PMS
     C                   Z-ADD     NHANPR        P01ESP
     c                   Sub       delStPrem     P01ESP
     C                   Z-ADD     NHPDPR        P01PDP
     C                   MOVE      1             P01AUF
     C                   MOVE      *ZEROS        P01BLF
     C                   MOVE      3             P01RRI
     C*
     C* SET RETROSPECTIVE RATING FLAG PROPERLY.
     C*
     C                   MOVEL     ELBSCD        XXKEY
     C     TBKY          CHAIN     PBS01                              60
     C*
     C                   MOVE      *OFF          XXRTRO            1
     C     *IN60         IFEQ      *OFF
     C     BSLPGM        IFEQ      'P'
     C     BSLPGM        OREQ      'I'
     C                   MOVE      *ON           XXRTRO
     C                   ENDIF
     C                   ENDIF
     C*
     C     XXRTRO        IFEQ      *ON
     C*
     C                   MOVEL(P)  W00503        XXFORM
     C     MUKY          CHAIN     PMU01                              60
     C   60              MOVEL(P)  W29503        XXFORM
     C   60MUKY          CHAIN     PMU01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      '1'           P01RRI
     C                   ELSE
     C                   MOVE      '5'           P01RRI
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C                   Z-ADD     NHACC         ##ACC
     C                   Z-ADD     NHAGG         ##AGG
     C                   Z-ADD     NHDIS         ##DIS
     C                   MOVE      ##P2L         P01P2L
     C*
     C* **** PRODUCER NAME LAST, FIRST
     C*
     C                   MOVEL     NHPLSN        P01NOP
     C*
      *
     C                   SELECT
      *
      *
     C     ##TRCD        WHENEQ    6
     C     *IN66         ANDEQ     *OFF
     C                   EXSR      SRRWRT
      *
     C     ##TRCD        WHENEQ    6
     C     *IN66         ANDEQ     *ON
     C                   Z-ADD     ##CARR        P01OCC
     C                   MOVEL     ##POL#        P01OP#
     C                   Z-ADD     SHSBDT        P01OED
     C                   Z-ADD     6             ##TRCD
      *
     C                   OTHER
     C                   MOVE      *ZEROS        P01OCC
     C                   MOVEL     *BLANKS       P01OP#
     C                   MOVE      *ZEROS        P01OED
      *
     C                   ENDSL                                                  ##TRCD WHEQ
     C                   MOVE      NHPCDT        P01EPC
     C                   MOVE      NHXPC         P01XPC
     C     XSREIS        IFEQ      'Y'
     C     NHPCDT        ANDEQ     *ZERO
     C                   MOVE      NHPFDT        P01EPC
     C                   MOVE      NHPTDT        P01XPC
     C                   ENDIF                                                  XSREIS IFEQ
     C*** begin add ***  10/31/24  *********************************************
     C                   If        QQSt = 'CA' and (##TRCD = 1 or ##TRCD = 2)
     C                   Z-add     0             P01EPC
     C                   Z-add     0             P01XPC
     C                   Endif
     C*** end add *****  10/31/24  *********************************************
     C*
     C* Do not report change dates to CA if no changes have occurred on record.
     C*
     C                   If        QQST = 'CA' and XSREIS = 'Y'
     C*
     C                   If        svP01LNI = P01LNI and
     C                             svP01TON = P01TON and
     C                             svP01NOP = P01NOP
     C                   Z-add     0             P01EPC
     C                   Z-add     0             P01XPC
     C                   Endif
     C*
     C                   Endif
     C                   MOVE      *BLANK        XXREIS            1
     C     NHPCDT        IFNE      *ZERO
     C                   MOVE      'Y'           XXREIS
     C                   ENDIF                                                  NHPCDT IFNE
     C*
     C*** LOAD WCPOLS DATA AND WRITE RECORD ***
     C*
     C                   MOVEL     P01           RDATA
     C                   WRITE     RPOLS
     C                   ADD       1             ##TOTR           10 0
     C                   ADD       1             ##TOTH            8 0
     C     P01PP#        IFEQ      *BLANKS
     C                   ADD       1             ##TLNP
     C                   ELSE
     C                   ADD       1             ##TLRP
     C                   ENDIF                                                  P01PP# IFEQ
     C     SHTRDT        IFLT      XXTDE
     C                   Z-ADD     SHTRDT        XXTDE
     C                   ENDIF
     C     SHTRDT        IFGT      XXTDL
     C                   Z-ADD     SHTRDT        XXTDL
     C                   ENDIF
     C*
     C                   EXSR      SRSTRP
     C*
     C                   MOVEL     EMNAME        XXNAME           40
     C     EMFDNM        IFNE      *BLANKS
     C                   MOVEL     EMFDNM        XXNAME
     C                   ENDIF
     C*
     C                   MOVEL(P)  X1FORM        XXFORM
     C     MUKY          CHAIN     PMU01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   CLEAR                   PAI
     C*
     C                   MOVE      ##LNK         PAILNK
     C                   MOVE      '29'          PAIST
     C                   MOVE      'AI'          PAIRTP
     C                   MOVEL     'WC290409'    PAIEN#
     C                   MOVE      'A'           PAIBVI
     C                   CLEAR                   E00409
     C     PEKY1         CHAIN     PPE01                              60
     C     *IN60         IFEQ      *OFF
     C     PEDATA        ANDNE     *BLANKS
     C                   MOVEL     PEDATA        E00409
     C                   MOVEL     XXINOP        PAINOP
     C                   MOVE      XXIEFP        PAIEFP
     C                   MOVE      XXIPRP        PAIPRP
     C                   ELSE
     C                   MOVEL     X1DESC        PAINOP
     C                   MOVE      ELPFDT        PAIEFP
     C                   MOVE      ELASPS        PAIPRP
     C                   ENDIF                                                  *IN60 IFEQ
     C                   MOVEL     XXNAME        PAINAM
     C                   MOVE      ##PFDT        PAIEED
     C*
     C                   CLEAR                   RDATA
     C**** WRITE RECORD
     C*
     C                   MOVEL     PAI           RDATA
     C                   WRITE     RPOLS
     C                   ADD       1             ##TOTR           10 0
     C                   ENDIF
     C*
     C                   Endif                                                  polSkip <> 'Y'
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C*** begin del ***  12/18/24  *********************************************
     C******             If        polSkip <> 'Y'
     C*** end del *****  12/18/24  *********************************************
     C     NKEY          READE     PNH01                                  60
     C     *IN60         DOWEQ     '0'
     C     NHST          IFNE      '**'
     C*
     C*** FOR EACH HEADER READ, WE OUTPUT 1 STATE RECORD (RCD TYPE 4) *NOW*
     C*
     C*
     C* Set Employer Detail to state being processed.
     C*
     C     ELKY1         Chain     PEL02                              60
     C     *IN60         Doweq     *OFF
     C*
     C     RSKY          Chain     PRS01                              60
     C     *IN60         Ifeq      *OFF
     C     RSJRST        Andeq     NHST
     C                   Leave
     C                   Endif
     C*
     C     ELKY1         Reade     PEL02                                  60
     C                   Enddo                                                  *IN60 DOWEQ
      /free
       P04pecr = 0;
       P04pmpr = 0;
       index = %LookUp(NHst:currentState);
       P04adi = ' ';
       P04rasc = 0;
       Clear xxStatus;
       If index <> 0;
          xxStatus = currentStatus(index);
       Endif;

       If xxStatus <> 'P' or QQSt <> '  ';
       If xxStatus = 'P';
          P04adi = ' ';
       Else;
          P04adi = ' ';
          P04rasc = 0;
          If xxRENW = 'Y' or xxTR15 = 'Y';
             index = %LookUp(NHst:currentState);
             If index <> 0;
                If currentSTATUS(index) = 'A';
                   Select;
                   When xxRENW = 'Y';
                      P04RASC = 3;
                   Other;
                      P04RASC = 2;
                      P04adi = 'A';
                   Endsl;
                Endif;
                If currentSTATUS(index) = 'D'and xxTR15 = 'Y';
                   P04adi = 'D';
                Endif;
             Endif;
          Endif;
       Endif;
      /end-free
     C*
     C*** begin add ***  12/18/24  *********************************************
     C                   If        polSkip <> 'Y'
     C*** end add *****  12/18/24  *********************************************
     C                   MOVE      ##LNK         P04LNK
     C                   MOVE      '04'          P04RTP
     C                   MOVE      NHMPST        P04ST
     C                   If        NHSt = 'WI' and xxWIShort = 'Y'
     C                   Z-add     2             p04pecr
     C                   Z-add     2             p04pmpr
     C                   Endif
     C*
     C*** Begin Chg *** 05/24/22 *******************************************
     C*****EMKEY         CHAIN     PEM01                              61
     C*
     C******IN61         IFEQ      '0'
     C**********         MOVEL     EMRSK#        XXIR#             9
     C**********         TESTN                   XXIR#                61
     C******IN61         IFEQ      *ON
     C*
     C**********         MOVE      XXIR#         P04IR#
     C**********         ELSE
     C**********         MOVE      *ZEROS        P04IR#
     C**********         ENDIF
      *
     C**********         ELSE
     C**********         MOVE      *ZEROS        P04IR#
     C**********         ENDIF
     C**********
       Clear P04IR#;
       Chain (SHco#:SHfnd:NHst) pf300;
       If %Found(wdf3l);
          P04IR# = F3fein;
       Else;
          Chain (SHco#:SHfnd) pf300;
          If %Found(wdf3l);
             P04IR# = F3fein;
          EndIf;
       EndIf;
     C*** End   Chg *** 05/24/22 *******************************************
     C                   MOVE      *BLANKS       P04IRR
     C     NHMPST        IFEQ      '29'
     C     NHMPST        OREQ      '07'
     C                   MOVE      *ZEROS        P04IRR
     C                   MOVE      NHDRSK        P04IRR
     C                   ENDIF
     C     NHMPST        IFEQ      '37'
     C                   MOVE      NHDRSK        P04IRR
     C                   ENDIF
     C                   MOVE      *ZEROS        P04CCD
     C     NHEXM1        MULT      1000          P04EMF
      *** Beg Add ***** 08/19/25 ****************************
        If NHmpst = '20' and Nhst = 'MA';
           Chain (SHCO# : SHFND : SHFYR : SHEMP#) PEL01Q;
           If maARP1 <> 0;
              P04ARP = maARP1 * 1000;
           Endif;
        Endif;
      *** End Add ***** 08/19/25 ****************************
      /Free
       Select;
         //*** beg add ***** 05/29/24 *************************************
         When NHst = 'AL' and ELprom = 'M';
            P04EMS = 4;
         //*** end add ***** 05/29/24 *************************************
         When ELexm1 = 0 or ELnoexm1 = 'Y';
            P04EMS = 3;
         When NHexmd1nf = 'Y';
            P04EMS = 2;
         Other;
            P04EMS = 1;
        Endsl;
      /End-Free
     C                   MOVE      '1'           P04EMT
     C                   MOVE      NHOIRR        P04ORF
     C                   MOVE      NHCPRD        P04CPD
     C                   MOVE      NHCPTD        P04TPD
     C                   Z-ADD     NHANPR        P04ESP
     C*** begin add ***  01/16/24  ******************************************
     C                   If        NHAnpr < 0
     C                   Z-ADD     0             P04ESP
     C                   Endif
     C*** end add *****  01/16/24  ******************************************
     C                   Z-ADD     NHPRTD        P04EXC
     C                   Z-ADD     NHLOSS        P04LSC
     C                   Z-ADD     NHPRMD        P04PDS
     C                   MOVE      NHANDT        P04ARD
     C                   Z-add     0             P04EMD
     C                   Z-ADD     1             P04PAP
     C*
      /free
       If ELexmf = 'Y' and NHmpst <> '07' and NHmpst <> '20'
        and NHmpst <> '37' and NHmpst <> '48';
          P04TNS = 2;
       Else;
          If NHmpst <> '32' and NHmpst <> '48';
             xxForm = 'WC000308';
             Chain muky pmu01;
             If %found;
                P04TNS = 8;
             Else;
                If NHmpst = '07' or NHmpst = '37';
                   P04TNS = 9;
                Else;
                   P04TNS = 1;
                Endif;
             Endif;
          Else;
             P04TNS = 1;
          Endif;
       Endif;
      /end-free
     C                   MOVE      NHPCDT        P04EFD
     C                   MOVE      NHXPC         P04EXD
     C     XSREIS        IFEQ      'Y'
     C     NHPCDT        ANDEQ     *ZERO
     C                   MOVE      NHPFDT        P04EFD
     C                   MOVE      NHPTDT        P04EXD
     C                   ENDIF                                                  XSREIS IFEQ
     C*** begin add ***  10/31/24  *********************************************
     C                   If        QQSt = 'CA' and (##TRCD = 1 or ##TRCD = 2)
     C                   Z-add     0             P04EFD
     C                   Z-add     0             P04EXD
     C                   Endif
     C*** end add *****  10/31/24  *********************************************
     C*
     C* Do not report change dates to CA if no changes have occurred on record.
     C*
     C                   If        QQST = 'CA' and XSREIS = 'Y'
     C*
     C                   Eval      index = %LookUp(NHst:priorState)
     C*
     C                   If        index <> 0
     C*
     C                   If        svaNHEXM1(index) = NHEXM1 and
     C                             svaNHEXM2(index) = NHEXM2 and
     C                             svaNHEXM3(index) = NHEXM3 and
     C                             svaNHEMD1(index) = NHEMD1 and
     C                             svaNHEMD2(index) = NHEMD2 and
     C                             svaNHEMD3(index) = NHEMD3 and
     C                             svaNHANDT(index) = NHANDT and
     C                             svaP04EMS(index) = P04EMS
     C                   Z-add     0             P04EFD
     C                   Z-add     0             P04EXD
     C                   Endif
     C*
     C                   Endif
     C*
     C                   Endif
     C                   MOVE      *BLANK        XXSPLT            1
     C*
     C*
     C*
     C*
     C*
     C*
     C*
     C* REPORT ONE STATE PREMIUM RECORD FOR EACH MODIFIER FOR CA.
     C*
     C     QQST          IFNE      *blanks
     C     NHANDT        ANDNE     *ZERO
     C     NHANDT        ANDLT     NHPTDT
     C     NHANDT        ANDGT     NHPFDT
     C     QQST          ORNE      *blanks
     C     NHEXM2        ANDNE     *zero
     C*** Begin Add *** 07/30/23 **************************************
     C     QQST          ORNE      *blanks
     C     ELNOEXM2      ANDEQ     'Y'
     C*** End   Add *** 07/30/23 **************************************
     C*
     C                   Clear                   Dys
     C                   Clear                   Pro
     C                   Clear                   Prem
     C                   Eval      pDat = 9999999
     C                   Z-add     NHPFDT        pDat(1)
     C*
     C*
     C                   Eval      splitDt = 9999999
     C
     C                   Z-add     1             x
     C
     C     NHANDT        IFNE      *ZERO
     C     NHANDT        ANDLT     NHPTDT
     C     NHANDT        ANDGT     NHPFDT
     C                   Z-add     NHANDT        splitDt(x)
     C                   Add       1             x
     C                   Z-add     NHANDT        pDat(x)
     C                   Endif
     C
     C     NHEXM2        IFNE      *ZERO
     C     NHEMD2        ANDNE     NHANDT
     C                   Z-add     NHEMD2        splitDt(x)
     C                   Add       1             x
     C                   Z-add     NHEMD2        pDat(x)
     C                   Endif
     C
     C     NHEXM3        IFNE      *ZERO
     C     NHEMD3        ANDNE     NHANDT
     C                   Z-add     NHEMD3        splitDt(x)
     C                   Add       1             x
     C                   Z-add     NHEMD3        pDat(x)
     C                   Endif
     C
     C                   Sorta     splitDt
     C*
     C                   Add       1             x
     C                   Z-add     NHPTDT        pDat(x)
     C*
     C                   Sorta     pDat
     C*
     C                   Z-ADD     NHPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM            8 0
     C*
     C                   Z-ADD     NHPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQTO              8 0
     C*
     C                   MOVE      XQFROM        QQFROM            8 0
     C                   MOVE      XQTO          QQTO              8 0
     C                   Z-ADD     *ZERO         QQDAYS            5 0
     C                   MOVE      *BLANK        QQERR             1
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQFROM
     C                   PARM                    QQTO
     C                   PARM                    QQDAYS
     C                   PARM                    QQERR
     C                   PARM                    QQTERM
     C*
     C                   EXSR      SRLEAP
     C*
     C                   Z-ADD     QQDAYS        PDAYS             5 0
     C* Determine # of days, prorate factor, and premium for each period.
     C*
     C                   Z-ADD     1             X
     C     X             ADD       1             Z                 2 0
     C*
     C     pDat(X)       DOWNE     9999999
     C     pDat(Z)       ANDNE     9999999
     C*
     C                   Z-ADD     PDAT(X)       SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM            8 0
     C*
     C                   Z-ADD     PDAT(Z)       SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQTO              8 0
     C*
     C                   MOVE      XQFROM        QQFROM            8 0
     C                   MOVE      XQTO          QQTO              8 0
     C                   Z-ADD     *ZERO         QQDAYS            5 0
     C                   MOVE      *BLANK        QQERR             1
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQFROM
     C                   PARM                    QQTO
     C                   PARM                    QQDAYS
     C                   PARM                    QQERR
     C                   PARM                    QQTERM
     C*
     C                   EXSR      SRLEAP
     C*
     C                   Z-ADD     QQDAYS        DYS(X)
     C*
     C     PDAYS         IFNE      0
     C     DYS(X)        DIV(H)    PDAYS         PRO(X)
     C                   ENDIF
     C*
     C                   ADD       1             X
     C                   ADD       1             Z
     C                   ENDDO
     C*
     C                   XFOOT     PRO           XXWK54            5 4
     C     1             SUB       XXWK54        XXWK54
     C                   ADD       XXWK54        PRO(1)
     C*
     C                   Z-ADD     0             XXTOTL           11 2
     C     QQST          IFEQ      'NJ'
     C                   MOVE      'Y'           XXSPLT
     C                   ENDIF
     C*
     C* ADD BALANCE TO MINIMUM FIGURE INTO 1ST STATE PREMIUM RECORD.
     C*
     C*** begin chg ***  02/03/25  *****************************************
     C*****              MOVEL     NHST          XXST
     C*****              Z-ADD     NHPFDT        XXEFDT
     C*
     C*****RRKY          SETGT     PRR01
     C*****RRKY1         READPE    PRR01                                  60
     C*
     C*****NKEY          CHAIN     PNX01                              60
     C******IN60         DOWEQ     '0'
     C*
     C*****NXCLAS        IFEQ      RRMPCD
     C*****NXST          ANDEQ     NHMPST
     C*****              ADD       NXEMPR        XXTOTL
     C*****              ENDIF
     C*
     C*****NKEY          READE     PNX01                                  60
     C*****              ENDDO
     C*
     C* Total the premium for the state.
     C*
     C*****ELKY1         CHAIN     PPP01                              60
     C******IN60         DOWEQ     *OFF
     C*
     C*****PPST          IFEQ      NHST
     C*****              ADD       PPSTPR        XXTOTL
     C*****              ENDIF                                                  PPST IFEQ
     C*
     C*****ELKY1         READE     PPP01                                  60
     C*****              ENDDO
     C                   Z-ADD     NHANPR        xxTotl
     C*** end chg *****  02/03/25  *****************************************
     C*** begin add ***  01/16/24  ******************************************
     C                   If        xxTotl < 0
     C                   Z-ADD     0             xxTotl
     C                   Endif
     C*** end add *****  01/16/24  ******************************************
     C*
     C* Split the premium total into periods.
     C*
     C                   Z-ADD     1             X
     C     pro(x)        DOWNE     0
     C*
     C     XXTOTL        Mult(h)   pro(x)        prem(x)
     C                   Z-add     x             z
     C*
     C                   Add       1             X
     C                   Enddo
     C*
     C                   XFOOT     prem          XXWK11           11 0
     C     xxtotl        SUB       xxwk11        XXWK11
     C                   ADD       XXWK11        prem(1)
     C*
     C                   Z-ADD     prem(1)       P04ESP
     C                   Z-ADD     0             P04EXC
     C                   Z-ADD     0             P04LSC
     C                   Z-ADD     0             P04PDS
     C                   Z-add     0             P04ARD
     C                   Z-add     0             P04EMD
     C*
     C     NHEXM2        IFNE      *ZERO
     C*** Begin Add *** 07/30/23 **************************************
     C     ELNOEXM2      OREQ      'Y'
     C*** End   Add *** 07/30/23 **************************************
     C*
     C*** Beg Add *** 04/10/24 *****************************************
     C* First record is written here
     C*** begin chg ***  08/22/24  *************************************
     C******             If        NHexm2 <> 0 and P04exd <> 0
     C                   If        NHEmd2 <> 0 and P04exd <> 0
     C*** end chg *****  08/22/24  *************************************
     C                   Move      NHemd2        P04exd
     C                   Endif
     C*** End Add *** 04/10/24 *****************************************
     C*
     C                   MOVEL     P04           RDATA
     C                   WRITE     RPOLS
     C*
     C                   ADD       1             ##TOTR           10 0
     C                   Z-ADD     prem(2)       P04ESP
     C*
     C                   If        NHEMD2 > splitDt(1) and NHANDT <> 0
     C
     C                   Z-add     NHANDT        P04ARD
     C                   MOVEL     P04           RDATA
     C                   WRITE     RPOLS
     C                   ADD       1             ##TOTR           10 0
     C                   Z-ADD     prem(3)       P04ESP
     C
     C                   Endif
     C
     C                   If        NHEMD2 = NHANDT
     C                   Z-add     NHANDT        P04ARD
     C                   Endif
     C*
     C     NHEXM2        MULT      1000          P04EMF
     C                   MOVE      NHEMD2        P04EMD
      *** Beg Add ***** 08/19/25 ****************************
        If NHmpst = '20' and Nhst = 'MA';
           If maARP2 <> 0;
              P04ARP = maARP2 * 1000;
           Endif;
        Endif;
      *** End Add ***** 08/19/25 ****************************
     C*** Beg Add *** 04/10/24 *****************************************
     C                   If        P04efd <> 0 and P04exd <> 0
     C                   If        (xsreis = 'Y' and NHpcdt = 0)
     C* Don't change override values
     C                   Else
     C                   Move      NHemd2        P04efd
     C                   Move      NHxpc         P04exd
     C*** begin chg ***  08/22/24  *************************************
     C******             If        NHexm3 <> 0
     C                   If        NHEmd3 <> 0
     C*** end chg *****  08/22/24  *************************************
     C                   Move      NHemd3        P04exd
     C                   Endif
     C                   Endif
     C                   Endif
     C*** End Add *** 04/10/24 *****************************************
      /Free
       Select;
         //*** beg add ***** 05/29/24 *************************************
         When NHst = 'AL' and ELprom = 'M';
            P04EMS = 4;
         //*** end add ***** 05/29/24 *************************************
         //*** Begin Add *** 07/30/23 **************************************
         When ELexm2 = 0 Or ELnoexm2 = 'Y';
            P04EMS = 3;
         //*** End   Add *** 07/30/23 **************************************
         When NHexmd2nf = 'Y';
            P04EMS = 2;
         Other;
            P04EMS = 1;
       Endsl;
      /End-Free
     C*
     C     NHEXM3        IFNE      *ZERO
     C*** Begin Add *** 07/30/23 **************************************
     C     ELNOEXM3      OREQ      'Y'
     C*** End   Add *** 07/30/23 **************************************
     C*
     C                   MOVEL     P04           RDATA
     C                   WRITE     RPOLS
     C*
     C                   ADD       1             ##TOTR           10 0
     C                   If        NHEMD2 > splitDt(1) and NHANDT <> 0
     C                   Z-ADD     prem(4)       P04ESP
     C                   Else
     C                   Z-ADD     prem(3)       P04ESP
     C                   Endif
     C*
     C                   If        NHEMD3 > splitDt(2) and NHANDT <> 0
     C                             and P04ARD = 0
     C
     C                   Z-add     NHANDT        P04ARD
     C                   MOVEL     P04           RDATA
     C                   WRITE     RPOLS
     C                   ADD       1             ##TOTR           10 0
     C
     C                   Endif
     C
     C                   If        NHEMD3 = NHANDT
     C                   Z-add     NHANDT        P04ARD
     C                   Endif
     C*
     C     NHEXM3        MULT      1000          P04EMF
     C                   MOVE      NHEMD3        P04EMD
      *** Beg Add ***** 08/19/25 ****************************
        If NHmpst = '20' and Nhst = 'MA';
           If maARP3 <> 0;
              P04ARP = maARP3 * 1000;
           Endif;
        Endif;
      *** End Add ***** 08/19/25 ****************************
     C*** Beg Add *** 04/10/24 *****************************************
     C                   If        P04efd <> 0 and P04exd <> 0
     C                   If        (xsreis = 'Y' and NHpcdt = 0)
     C* Don't change override values
     C                   Else
     C                   Move      NHemd3        P04efd
     C                   Move      NHxpc         P04exd
     C                   Endif
     C                   Endif
     C*** End Add *** 04/10/24 *****************************************
      /Free
       Select;
         //*** beg add ***** 05/29/24 *************************************
         When NHst = 'AL' and ELprom = 'M';
            P04EMS = 4;
         //*** end add ***** 05/29/24 *************************************
         //*** Begin Add *** 07/30/23 **************************************
         When ELexm3 = 0 Or ELnoexm3 = 'Y';
            P04EMS = 3;
         //*** End   Add *** 07/30/23 **************************************
         When NHexmd3nf = 'Y';
            P04EMS = 2;
         Other;
            P04EMS = 1;
       Endsl;
      /End-Free
     C*
     C                   ENDIF                                                  NHEXM3 IFNE
     C*
     C                   ENDIF                                                  NHEXM2 IFNE
     C*
     C                   If        NHANDT <> 0 and P04ARD = 0
     C                             and NHANDT < NHPTDT and NHANDT > NHPFDT
     C
     C                   MOVEL     P04           RDATA
     C                   WRITE     RPOLS
     C                   ADD       1             ##TOTR           10 0
     C
     C                   Z-add     NHANDT        P04ARD
     C
     C                   Endif
     C                   Z-ADD     prem(z)       P04ESP
     C                   Z-ADD     NHPRTD        P04EXC
     C                   Z-ADD     NHLOSS        P04LSC
     C                   Z-ADD     NHPRMD        P04PDS
     C*
     C                   ENDIF                                                  QQST IFEQ
     C*
     C*** LOAD WCPOLS DATA AND WRITE RECORD ***
     C*
     C                   MOVEL     P04           RDATA
     C                   WRITE     RPOLS
     C*
     C                   ADD       1             ##TOTR           10 0
     C*
     C*** begin add ***  12/18/24  *********************************************
     C                   Endif                                                  polSkip <> 'Y'
     C*** end add *****  12/18/24  *********************************************
      /free
       // This is Endif of "If xxTR15 = 'Y'and currentSTATUS(index) = 'P'"
       Endif;
       If P04adi = ' ' and (xxStatus = 'P' or
        (xxStatus = 'D' and flgGenCX = 'Y'));
          index = %LookUp(NHst:currentState);
          If index <> 0;
             NHSADD =currentSTATUS(index);
             Update PNH01;
          Endif;
       Endif;

       // If the add/delete state field is equal to 'D' or 'A' then
       If P04adi = 'D' or P04adi = 'A';
          index = %LookUp(NHst:currentState);
          If index <> 0;
             NHSADD =currentSTATUS(index);
             Update PNH01;
          Endif;
       Endif;
      /end-free
     C                   ENDIF                                                  NHST IFNE
     C*
     C     NKEY          READE     PNH01                                  60
     C                   ENDDO                                                  DOWEQ *IN60
     C*** begin del ***  12/18/24  *********************************************
     C******             Endif                                                  polSkip <> 'Y'
     C*** end del *****  12/18/24  *********************************************
     C*
     C     NKEY          CHAIN(n)  PNH01                              60
     C     *IN60         IFEQ      *OFF
     C                   If        polSkip <> 'Y'
     C*
     C*** *** *** WRITE AND ADDRESS RECORD FOR THE FUND(CARRIER) *** ***
     C                   MOVE      ##LNK         P03LNK
     C                   MOVE      '03'          P03RTP
     C                   MOVE      3             P03TOA
     C                   MOVE      '1'           P03ASI
     C*** begin add ***  06/24/25  **************************************
     C                   Clear                   P03SIC
     C*** end add *****  06/24/25  **************************************
     C*** FORMAT ADDRESS LINE
     C                   CLEAR                   PFN01
     C     FNKEY         CHAIN     PFN01                              65
     C*
     C     OFKY          CHAIN     POF01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     QQST          ANDEQ     'CA'
     C     *IN60         OREQ      *OFF
     C     XXCLT#        ANDEQ     '139'
     C     ##CARR        ANDEQ     22055
     C*
     C                   MOVEL(P)  OFADR1        ##ADDR
     C     OFADR2        IFNE      *BLANKS
     C                   CAT(P)    OFADR2:1      ##ADDR
     C                   ENDIF
     C                   MOVEL     OFCTY         ##CITY
     C                   MOVEL     OFST          ##ST
     C                   MOVE      OFZIP         XXZIP
     C                   MOVE      XXZIP1        ##ZIP1
      *
     C     XXZIP2        IFEQ      0
     C                   MOVE      *BLANKS       ##ZIP2
     C                   ELSE
     C                   MOVE      XXZIP2        ##ZIP2
     C                   ENDIF                                                  XXZIP2 IFEQ
     C*
     C                   MOVEL     ##ADRS        P03ADR
     C*
     C                   Z-ADD     999           ##ENCD
     C                   Z-ADD     99            ##ST1
     C                   Z-ADD     99999         ##ELCD
     C*
     C                   MOVE      ##NEL         P03NEL
     C*
     C                   MOVEL     OFAARA        ##WK10           10 0
     C                   MOVE      OFAPHN        ##WK10
     C*
     C                   MOVE      ##WK10        P03PH#
     C                   MOVE      NHPCDT        P03EFD
     C                   MOVE      NHXPC         P03EXD
     C     XSREIS        IFEQ      'Y'
     C     NHPCDT        ANDEQ     *ZERO
     C                   MOVE      NHPFDT        P03EFD
     C                   MOVE      NHPTDT        P03EXD
     C                   ENDIF                                                  XSREIS IFEQ
     C*** begin add ***  10/31/24  *********************************************
     C                   If        QQSt = 'CA' and (##TRCD = 1 or ##TRCD = 2)
     C                   Z-add     0             P03EFD
     C                   Z-add     0             P03EXD
     C                   Endif
     C*** end add *****  10/31/24  *********************************************
     C*
     C                   ELSE                                                   *IN60 IFEQ
     C*
     C                   MOVEL(P)  FNMAD1        ##ADDR
     C                   CAT(P)    FNMAD2:1      ##ADDR
     C                   MOVEL     FNMCTY        ##CITY
     C                   MOVEL     FNMST         ##ST
     C                   MOVE      FNMZIP        XXZIP
     C                   MOVE      XXZIP1        ##ZIP1
      *
     C     XXZIP2        IFEQ      0
     C                   MOVE      *BLANKS       ##ZIP2
     C                   ELSE
     C                   MOVE      XXZIP2        ##ZIP2
     C                   ENDIF                                                  XXZIP2 IFEQ
     C*
     C                   MOVEL     ##ADRS        P03ADR
     C*
     C                   Z-ADD     999           ##ENCD
     C                   Z-ADD     99            ##ST1
     C                   Z-ADD     99999         ##ELCD
     C*
     C                   MOVE      ##NEL         P03NEL
     C*
     C                   MOVEL     FNAREA        ##WK10           10 0
     C                   MOVE      FNPH2         ##WK10
     C*
     C                   MOVE      ##WK10        P03PH#
     C                   MOVE      NHPCDT        P03EFD
     C                   MOVE      NHXPC         P03EXD
     C     XSREIS        IFEQ      'Y'
     C     NHPCDT        ANDEQ     *ZERO
     C                   MOVE      NHPFDT        P03EFD
     C                   MOVE      NHPTDT        P03EXD
     C                   ENDIF                                                  XSREIS IFEQ
     C*** begin add ***  10/31/24  *********************************************
     C                   If        QQSt = 'CA' and (##TRCD = 1 or ##TRCD = 2)
     C                   Z-add     0             P03EFD
     C                   Z-add     0             P03EXD
     C                   Endif
     C*** end add *****  10/31/24  *********************************************
     C*
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C*
     C* Do not report change dates to CA if no changes have occurred on record.
     C*
     C                   If        QQST = 'CA' and XSREIS = 'Y' and xxunid <> 0
     C*
     C                   Eval      xxAType = 3
     C     keyWXA6L      Chain(n)  pa600
     C*
     C                   If        %found and
     C                             A6Addr = P03ADR and
     C                             A6PH#  = P03PH#
     C                   Z-add     0             P03EFD
     C                   Z-add     0             P03EXD
     C                   Endif
     C*
     C                   Endif
     C*
     C                   CLEAR                   RDATA
     C**** WRITE ADDR RECORD
     C*
     C                   MOVEL     P03           RDATA
     C                   WRITE     RPOLS
     C                   ADD       1             ##TOTR           10 0
      /free
       Clear pa600;

       A6Co# = SHCo#;
       A6Fnd = SHFnd;
       A6Fyr = SHFyr;
       A6Emp# = SHEmp#;
       A6Unid = SHUnid;
       A6Type = P03TOA;
       A6Addr = P03ADR;
       A6NEL  = P03NEL;
       A6PH#  = P03PH#;
       A6EFD  = P03EFD;
       A6EXD  = P03EXD;

       Write pa600;

      /end-free
     C*
     C                   Z-ADD     0             XXENCD            3 0
     C*
     C     NKEY          SETGT     PNA01
     C     NKEY          READPE    PNA01                                  62
     C*
     C     *IN62         DOWEQ     *OFF
     C     NAENCD        ANDEQ     999
     C     NKEY          READPE    PNA01                                  62
     C                   ENDDO
     C*
     C     *IN62         IFEQ      *OFF
     C                   Z-ADD     NAENCD        XXENCD
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C*
     C                   MOVE      *BLANK        X1FRST            1
     C*
     C*
     C*
     C*
     C*
     C                   Z-ADD     0             SVXNML            3 0
     C*
     C                   MOVE      'P'           X2TYPE
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X2DATE
     C                   Z-ADD     DSTIME        X2TIME
     C*
     C     *IN59         IFEQ      *ON
     C*
     C     QQST          IFNE      *BLANKS
     C                   MOVE      QQST          X2ST
     C                   ELSE
     C                   MOVE      'XX'          X2ST
     C                   ENDIF
     C*
     C                   ELSE
     C                   MOVE      *BLANKS       X2ST
     C                   ENDIF
     C*
     C                   Z-ADD     0             XXUNID
     C*
     C     S1KY          SETGT     PS101
     C     S1KY1         READPE    PS101                                  60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     S1UNID        XXUNID
     C                   ENDIF
     C*
     C**** NAMES, ETC. HERE ***  USE THE KEY WITH UNIQUE ID TO GET ALL NAME
     C***  AND ADDRESS RECORDS TO INCLUDE.  (RCD TYPES 2 AND 3)
      /Free
       reportData = *On;
       If SHemp# = 1 and QQST = 'WI' and (FNPEOF = 'S' or FNPEOF = 'C');
          reportData = *Off;
       Endif;
       S3enCd = *Zeros;
       S3Namp = *blanks;
      /End-Free
     C*
     C*** begin add ***  08/29/23  *****************************************
     C*
     C* Clear the 1st address line if set to hardcoded value for non-specific addresses
     C*
     C     NKEY          Chain     PNA01                              62
     C     *IN62         Doweq     '0'
     C*
     C                   If        NAMad1 = '<No Specific Address>'
     C                   Z-add     DBRRB         SVRRN             9 0
     C     SVRRN         Chain     PNA02                              60
     C                   Clear                   NAMad1
     C                   Update    PNA02
     C                   Endif
     C*
     C     NKEY          Reade     PNA01                                  62
     C                   Enddo
     C*** end add *****  08/29/23  *****************************************
     C     NKEY          CHAIN     PNA01                              62
     C*
     C     *IN62         DOWEQ     '0'
     C*
     C                   EXSR      SRDEL
     C*** begin add ***  03/08/24  ******************************************
     C*Find UE# for bureau being reported to.
     C                   If        QQSt <> ' ' and NASUS# <= 1
     C                   Exsr      SRUE#
     C                   Endif
     C*
     C* Only allow 1st record to report for additional UE# for non-NCCI submissions.
     C*
     C                   If        QQSt = ' ' or (QQSt <> ' ' and NASUS# <= 1)
     C*** end add *****  03/08/24  ******************************************
     C*
     C* DO NOT REPORT SECOND RECORD FOR PRIMARY NAME FOR NJ, AND REPORT ANY
     C* ADDRESSES ATTACHED FOR IT UNDER PRIMARY NAME.
     C*
     C     NAMADT        IFEQ      'W'
     C                   MOVE      '1'           NAMADT
     C     QQST          IFNE      'NJ'
     C                   MOVE      'P'           NANAMP
     C                   ENDIF
     C                   ENDIF
     C*
     C     NAMADT        IFEQ      'X'
     C                   MOVE      '2'           NAMADT
     C     QQST          IFEQ      'NJ'
     C                   CLEAR                   NANAME
     C                   Z-ADD     NAENCD        SVXNML
     C                   Z-ADD     1             NAENCD
     C                   ENDIF
     C                   ENDIF
     C*
     C     NAENCD        IFEQ      SVXNML
     C     SVXNML        ANDNE     *ZERO
     C                   Z-ADD     1             NAENCD
     C                   ENDIF
     C     NAENCD        IFNE      999
     C     NANAME        ANDNE     *BLANKS
      /Free
        If reportData = *On or (S3namp <> 'C' and S3namp <> 'D');
      /End-Free
     C*
     C*
     C* REPORT NAME IF DELETED AND NOT SUBMITTED YET, OR IF NOT DELETED.
     C*
     C*
     C*
     C*
     C*
     C     XXNAMD        IFEQ      *BLANK
     C                   MOVE      ##LNK         P02LNK
     C                   MOVE      '02'          P02RTP
     C                   MOVE      2             P02TON
     C     NANAMP        IFEQ      'Y'
     C     NANAMP        OREQ      'Q'
     C     NANAMP        OREQ      'D'
     C                   MOVE      1             P02TON
     C                   ENDIF                                                  NANAMP IFEQ
     C                   SELECT
     C     NANAMP        WHENEQ    'P'
     C     NANAMP        OREQ      'Q'
     C                   MOVE      'P'           P02FU2
     C     NANAMP        WHENEQ    'C'
     C     NANAMP        OREQ      'D'
     C                   MOVE      'C'           P02FU2
     C                   OTHER
     C                   MOVE      *BLANK        P02FU2
     C                   ENDSL                                                  NANAMP SELEC
     C*
     C*
     C*
     C                   MOVE      NAENCD        P02ENC
     C                   MOVE      NANLKC        P02NLK
     C     NANAM2        IFGT      *BLANKS
     C                   CLEAR                   C
     C                   MOVE      NANAME        XX1A
     C     XX1A          IFEQ      *BLANK
     C                   Z-ADD     1             C
     C                   ENDIF
     C     NANAME        CAT       NANAM2:C      #WRK80           80
     C                   MOVEL(P)  #WRK80        P02NAM
     C                   ELSE
     C                   MOVEL(P)  NANAME        P02NAM
     C                   ENDIF
     C     NANAM3        IFNE      *BLANKS
     C                   CLEAR                   C
     C                   MOVE      NANAME        XX1A              1
     C     XX1A          IFEQ      *BLANK
     C                   Z-ADD     1             C                 1 0
     C                   ENDIF
     C     NANAME        CAT       NANAM3:C      #WRK10          100
     C                   MOVEL(P)  #WRK10        P02NAM
     C                   ENDIF
     C                   MOVE      NAFEIN        P02FID
     C                   MOVE      001           P02CSQ
      *
     C*
     C     NASUS#        IFLE      1
     C*
     C     NAENCD        IFEQ      S2ENCD
     C                   ADD       1             CSQCNT
     C                   ELSE
     C                   Z-ADD     1             CSQCNT
     C                   Z-ADD     NAENCD        S2ENCD
     C                   ENDIF
     C*
     C                   Z-ADD     CSQCNT        P02CSQ
     C                   ENDIF                                                  NASUS# IFLE
      *
     C*
     C                   MOVE      *ZEROS        P02NOE
     C*
     C*
     C*     TRANSLATE TOB  TO LEGAL NATURE OF EMPLOYER
     C                   MOVEL(P)  'BUSI'        XXTBID
     C                   MOVEL(P)  NALEGN        XXKEY
     C*
     C     GTKY          CHAIN     PGT01                              60
     C*
     C*  IF RECORD FOUND AND NCCI VALUE WITHIN VALID RANGE, MOVE THE
     C*  NCCI CODE TO THE NCCI LEGAL NATURE OF EMPLOYER CODE
     C*
     C     *IN60         IFEQ      *OFF
     C     GTN501        ANDGE     1
     C     GTN501        ANDLE     98
     C                   Z-ADD     GTN501        P02NOE
     C*
     C                   ELSE
     C*
     C*  IF RECORD WAS NOT FOUND, OR THE NCCI CODE IS "99" (OTHER),
     C*  LOAD "99" TO THE NCCI LEGAL NATURE OF EMPLOYER CODE.
     C*
     C                   MOVE      99            P02NOE
     C                   ENDIF
     C*
     C*
     C*
     C*
      *
     C                   MOVE      *BLANKS       P02ST1
     C                   MOVE      *BLANKS       P02IA1
     C                   MOVE      *BLANKS       P02ST2
     C                   MOVE      *BLANKS       P02IA2
     C                   MOVE      *BLANKS       P02ST3
     C                   MOVE      *BLANKS       P02IA3
     C*
     C     QQST          IFEQ      'NJ'
     C                   MOVE      NAST          P02ST1
     C                   ENDIF
      *
     C     NAST1         IFNE      *BLANKS
     C                   MOVE      NAST1         P02ST1
     C                   MOVE      NAUE#         P02IA1
      *
     C     NAUE#         IFEQ      *BLANKS
     C     NAST1         ANDEQ     '29'
     C                   Z-ADD     0             XX12             12 0
     C                   MOVEL     XX12          P02IA1
     C                   ENDIF                                                  NAUE# IFEQ
      *
     C                   ENDIF                                                  NAST1 IFNE
      *
     C     NAST2         IFNE      *BLANKS
     C                   MOVE      NAST2         P02ST2
     C                   MOVE      NAUE#2        P02IA2
      *
     C     NAUE#2        IFEQ      *BLANKS
     C     NAST2         ANDEQ     '29'
     C                   Z-ADD     0             XX12             12 0
     C                   MOVEL     XX12          P02IA2
     C                   ENDIF                                                  NAUE#2 IFEQ
      *
     C                   ENDIF                                                  NAST2 IFNE
      *
     C     NAST3         IFNE      *BLANKS
     C                   MOVE      NAST3         P02ST3
     C                   MOVE      NAUE#3        P02IA3
      *
     C     NAUE#3        IFEQ      *BLANKS
     C     NAST3         ANDEQ     '29'
     C                   Z-ADD     0             XX12             12 0
     C                   MOVEL     XX12          P02IA3
     C                   ENDIF                                                  NAUE#3 IFEQ
      *
     C                   ENDIF                                                  NAST3 IFNE
      *
     C                   MOVE      NASUS#        P02SUS
      *
     C                   MOVE      *BLANKS       P02EDS
      *
     C     QQST          IFEQ      'CA'
     C     P02NOE        ANDEQ     99
     C*
     C     NANAOB        IFNE      *BLANKS
     C                   MOVEL     NANAOB        P02EDS
     C                   ELSE
     C                   MOVEL     SVNAOB        P02EDS
     C                   ENDIF                                                  NANAOB IFNE
     C*
     C                   ENDIF                                                  QQST IFEQ
      *
      *
      *
     C                   MOVE      *ZEROS        P02EFD
     C                   MOVE      *ZEROS        P02EXD
      *
     C     XXREIS        IFEQ      'Y'
     C     XSREIS        OREQ      'Y'
     C                   MOVE      NAPFDT        P02EFD
     C                   If        QQSt <> ' '
     C                   Move      NANamEfd      P02Efd
     C                   Endif
     C                   MOVE      NAPTDT        P02EXD
     C     NAPFDT        IFEQ      NAPTDT
     C                   MOVE      'Y'           XXNAMD
     C                   ENDIF
     C*** begin add ***  10/31/24  *********************************************
     C                   If        QQSt = 'CA' and (##TRCD = 1 or ##TRCD = 2)
     C*** begin chg ***  12/17/24  *****************************************
     C******                       and XXNAMD <> 'Y'
     C                             and (XXNAMD <> 'Y' or
     C                             (xxNamD = 'Y' and savNamD = 'P'))
     C*** end chg *****  12/17/24  *****************************************
     C                   Z-add     0             P02EFD
     C                   Z-add     0             P02EXD
     C                   Endif
     C*** end add *****  10/31/24  *********************************************
     C                   ENDIF                                                  XXREIS IFEQ
     C*
     C* Do not report change dates to CA if no changes have occurred on record.
     C*
     C                   If        QQST = 'CA' and XSREIS = 'Y'
     C*** begin add ***  12/17/24  *****************************************
     C                             and (XXNAMD <> 'Y' or
     C                             (xxNamD = 'Y' and savNamD = 'P'))
     C*** end add *****  12/17/24  *****************************************
     C*
     C                   If        newName = ' ' and
     C                             svP02TON = P02TON
     C                   Z-add     0             P02EFD
     C                   Z-add     0             P02EXD
     C                   Endif
     C*
     C                   Endif
     C*
     C* PULL THE NAME FROM THE FULL NAME FIELD IF ITS POPULATED.
     C*
     C     NANAM4        IFNE      *BLANKS
     C                   MOVEL(P)  XXNM01        P02NAM
     C                   ENDIF                                                  NANAM4 IFNE
     C*** begin add ***  01/30/23  *********************************************
                         If xxClt# = '150' and QQSt = 'MI';
                            p02nam = %scanrpl(' DBA ':' ':p02nam);
                            p02nam = %scanrpl(' dba ':' ':p02nam);
                         Endif;
     C*** end add *****  01/30/23  *********************************************
     C*
     C                   CLEAR                   RDATA
     C**** WRITE NAME RECORD
     C*
     C                   MOVEL     P02           RDATA
     C                   WRITE     RPOLS
     C*
     C                   ADD       1             ##TOTR           10 0
     C*
     C* WRITE AN ADDITIONAL NAME RECORD FOR EACH ADDITIONAL 90 CHARACTERS
     C* OF THE FULL NAME.
     C*
     C     NANAM4        IFNE      *BLANKS
     C*
     C     2             DO        4             X
     C*
     C     NMA(X)        IFNE      *BLANKS
     C*
     C                   ADD       1             P02CSQ
     C                   Z-ADD     P02CSQ        CSQCNT
     C                   MOVEL(P)  NMA(X)        P02NAM
      *
     C*** begin add ***  01/30/23  *********************************************
                         If xxClt# = '150' and QQSt = 'MI';
                            p02nam = %scanrpl(' DBA ':' ':p02nam);
                            p02nam = %scanrpl(' dba ':' ':p02nam);
                         Endif;
     C*** end add *****  01/30/23  *********************************************
     C*
     C                   CLEAR                   RDATA
     C**** WRITE NAME RECORD
     C*
     C                   MOVEL     P02           RDATA
     C                   WRITE     RPOLS
     C*
     C                   ADD       1             ##TOTR           10 0
     C*
     C                   ENDIF                                                  NMA,X IFNE
     C*
     C                   ENDDO                                                  2 DO 4 X
     C*
     C                   ENDIF                                                  NANAM4 IFNE
     C*
     C                   ENDIF                                                  NAPFDT IFEQ
      /Free
        Endif;
      /End-Free
     C                   ENDIF                                                  NAENCD <> *ZER O
     C*
     C* IF TYPE OF ADDRESS IS NOT *BLANK, WRITE ADDRESS TOO!
     C*
     C***
     C     NAMADT        IFNE      *BLANK
      /Free
        If reportData = *On or (S3namP <> 'C' and S3namP <> 'D');
      /End-Free
     C*
     C*
     C*
     C* REPORT NAME IF DELETED AND NOT SUBMITTED YET, OR IF NOT DELETED.
     C*
     C*
     C*
     C*
     C*
     C     XXADRD        IFEQ      *BLANK
      *
     C*
     C                   MOVE      ##LNK         P03LNK
     C                   MOVE      '03'          P03RTP
     C                   MOVE      NAMADT        P03TOA
     C                   If        NANONA = 'Y' and P03TOA = 2
     C                   Eval      P03TOA = 6
     C                   ENDIF
     C*** begin add ***  12/11/23  *****************************************
     C                   Move      ' '           P03FAI
     C                   If        P03TOA = 1 and QQSt <> '  ' and
     C                             QQSt <> 'MI' and QQSt <> 'NJ'
     C                   Move      'N'           P03FAI
     C                   Endif
     C*** end add *****  12/11/23  *****************************************
     C                   MOVE      '1'           P03ASI
     C                   Z-ADD     NAEMPY        P03NOE
     C                   CLEAR                   P03SIC
     C     NASIC         IFNE      *BLANKS
     C                   MOVEA     NASIC         SIC1
     C     QQST          IFEQ      *BLANKS
     C     SIC1(5)       ANDEQ     *BLANK
     C     SIC1(6)       ANDEQ     *BLANK
     C                   MOVEA     SIC1          P03SIC
     C                   ELSE
     C                   Z-ADD     1             X
     C     *BLANK        LOOKUP    SIC1(X)                                42
     C     *IN42         IFEQ      *OFF
     C                   Z-ADD     6             X
     C                   ELSE
     C                   SUB       1             X
     C                   ENDIF                                                  *NI42 IFEQ *OFF
     C                   Z-ADD     6             Y
     C                   MOVE      *ZEROS        SIC2
     C     X             DOWGT     0
     C                   MOVE      SIC1(X)       SIC2(Y)
     C                   SUB       1             X
     C                   SUB       1             Y
     C                   ENDDO                                                  X DOWGT 0
     C                   MOVEA     SIC2          XXSIC
     C                   TESTN                   XXSIC                4242
     C     *IN42         IFEQ      *ON
     C                   MOVEA     SIC2          P03SIC
     C                   ELSE
     C                   MOVE      *ZEROS        P03SIC
     C                   ENDIF                                                  *IN42 IFEQ *ON
     C                   ENDIF                                                  QQST IFEQ
     C                   ENDIF                                                  NASIC IFNE
     C*
     C*** FORMAT ADDRESS LINE
     C*
     C                   MOVEL(P)  NAMAD1        ##ADDR
     C                   CAT(P)    NAMAD2:1      ##ADDR
     C                   MOVEL     NAMCTY        ##CITY
     C                   MOVEL     NAMST         ##ST
     C                   MOVE      NAMZIP        XXZIP
     C                   MOVE      XXZIP1        ##ZIP1
     C*
     C     XXZIP2        IFEQ      0
     C                   MOVE      *BLANKS       ##ZIP2
     C                   ELSE
     C                   MOVE      XXZIP2        ##ZIP2
     C                   ENDIF                                                  XXZIP2 IFEQ
     C*
     C                   MOVEL     ##ADRS        P03ADR
     C*** begin add ***  06/24/25  **************************************
     C                   If        P03TOA = 5
     C                   Clear                   P03SIC
     C                   Endif
     C*** end add *****  06/24/25  **************************************
     C*
     C                   MOVE      NAENCD        ##ENCD
     C                   MOVE      NANLKC        P03NLK
     C     NAMADT        IFEQ      '1'
     C     NAMADT        OREQ      '2'
     C                   MOVE      NAST          ##ST1
     C     QQST          IFEQ      'NC'
      *
     C                   MOVEL(P)  NAMST         XXKEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      STNST         ##ST1
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   ENDIF                                                  QQST IFEQ
     C                   ELSE
     C                   Z-ADD     99            ##ST1
     C                   ENDIF                                                  NAMADT IFEQ
     C                   Z-ADD     NAELCD        ##ELCD
     C*
     C                   MOVE      ##NEL         P03NEL
     C*
     C                   MOVEL     NAAREA        P03PH#
     C                   MOVE      NAPHN#        P03PH#
     C     XXREIS        IFEQ      'Y'
     C     XSREIS        OREQ      'Y'
     C                   MOVE      NAPFD2        P03EFD
     C                   MOVE      NAPTD2        P03EXD
     C                   If        QQSt <> ' '
     C                   Move      NAAdrEfd      P03Efd
     C                   Endif
     C     NAPFD2        IFEQ      NAPTD2
     C                   MOVE      'Y'           XXADRD
     C                   ENDIF
     C*** begin add ***  10/31/24  *********************************************
     C                   If        QQSt = 'CA' and (##TRCD = 1 or ##TRCD = 2)
     C*** begin chg ***  12/17/24  *****************************************
     C******                       and XXADRD <> 'Y'
     C                             and (XXAdrD <> 'Y' or
     C                             (xxAdrD = 'Y' and savAdrD = 'P'))
     C*** end chg *****  12/17/24  *****************************************
     C                   Z-add     0             P03EFD
     C                   Z-add     0             P03EXD
     C                   Endif
     C*** end add *****  10/31/24  *********************************************
     C                   ENDIF                                                  XXREIS IFEQ
     C*
     C*
     C* Do not report change dates to CA if no changes have occurred on record.
     C*
     C                   If        QQST = 'CA' and XSREIS = 'Y'
     C*** begin add ***  12/17/24  *****************************************
     C                             and (XXAdrD <> 'Y' or
     C                             (xxAdrD = 'Y' and savAdrD = 'P'))
     C*** end add *****  12/17/24  *****************************************
     C*
     C                   If        newAddr = ' ' and
     C                             svP03PH# = P03PH#
     C                   Z-add     0             P03EFD
     C                   Z-add     0             P03EXD
     C                   Endif
     C*
     C                   Endif
       //*** Beg Chg ***  01/15/24  *** WC2937 *************************
       //***P03Mail = NAmail;
       P03Mail = %subst(NAmail:1:42);
       //*** End Chg ***  01/15/24  *** WC2937 *************************
       //*** Beg Add ***  01/15/24  *** WC2937 *************************
       Clear P03Fu3;
       P03Fu3 = %subst(NAmail:43:18);
       //*** End Add ***  01/15/24  *** WC2937 *************************
     C*
     C*
     C                   CLEAR                   RDATA
     C**** WRITE ADDR RECORD
     C*
     C                   MOVEL     P03           RDATA
     C                   WRITE     RPOLS
     C                   ADD       1             ##TOTR           10 0
     C*
     C                   ENDIF                                                  NAPFD2 IFEQ
      /Free
        Endif;
      /End-Free
     C                   ENDIF
     C     SVRRN         CHAIN     PNA02                              60
     C*
     C                   MOVE      XXNAMD        NANAMD
     C                   MOVE      XXADRD        NAADRD
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         NADT
     C                   Z-ADD     DSTIME        NATIME
     C                   MOVEL(P)  Q1PGM         NAPGM
     C                   MOVEL(P)  Q1USER        NAUSER
     C*
     C                   UPDATE    PNA02
     C*** begin add ***  03/08/24  ******************************************
     C                   Endif                                                  If QQST and NASUS#
     C*** end add *****  03/08/24  ******************************************
     C*
     C     NKEY          READE     PNA01                                  62
     C*
     C*
     C                   ENDDO
     C*
     C*
     C*  *** EXPOSURE RECORDS TOO!
     C*
     C*
     C*
     C                   Clear                   ilstatcd          5 0
     C                   Clear                   ilstate           2
     C*
     C*
      /free
       Setll nkey pnx01;
       Reade nkey pnx01;
       Dow not %eof;

          //* Just copy records directly over for independent bureaus,
          //* or if there is no exposure link code set.
          If QQST <> *blanks or NXElcd = 0;
             Write pnx02;
          Else;
          //* Accumulate similiar exposure records together for NCCI submissions.
             xxEAPR = NXEAPR;
             xxEMPR = NXEMPR;
             Chain keyWXNX2L pnx02;
             NXElcd = 0;

             If not %found;
                Write pnx02;
             Else;
                NXEAPR = NXEAPR + xxEAPR;
                NXEMPR = NXEMPR + xxEMPR;
                Update pnx02;
             Endif;

          Endif;

          Reade nkey pnx01;
       Enddo;
      /end-free
     C*
     C*
     C     NKEY          CHAIN(N)  PNX02                              63
     C     *IN63         DOWEQ     '0'
     C*
     C                   Exsr      SRExpo
     C*
     C                   MOVE      NXST          XXNST
     C     STKY2         CHAIN     PST02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     STKEY         XXST
     C                   ENDIF
     C     NKEY          CHAIN(n)  PNH01                              60
     C     *IN60         DOWEQ     *OFF
     C     NHST          ANDNE     XXST
     c     NKEY          READE(n)  PNH01                                  60
     C                   ENDDO
     C*
     C* Report exposure records for state unless it is marked as
     C* previously deleted.
     C*
     C                   If        NHSadd <> 'P' or QQSt <> '  '
     C                   Z-ADD     NHPFDT        XXEFDT
     C     RRKY          SETGT     PRR01
     C     RRKY1         READPE    PRR01                                  60
     C                   Z-ADD     SHPFDT        XXEFDT
     C                   Z-ADD     0             XXSTCD            5 0
     C     RRKY          SETGT     PHC01
     C     RRKY1         READPE    PHC01                                  60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     HCSTCD        XXSTCD
     C                   ENDIF
      /free
       svStCd     = *Zeros;
       svPrtZrFlg = *Blank;
       svStatus   = *Blank;
    
       Select;
       When (QQst = *Blanks and xxNY = 'Y') Or
            (QQst = 'NY' and xxNY = 'Y');
          svState = 'NY';
       When QQst <> *Blanks;
          svState = QQst;
       Other;
          svState = *Blanks;
       EndSl;
    
       svStCd     = NXclas;
       svPrtZrFlg = 'N';
       svStatus   = 'I';
    
       Chain(n) keyWTSTL pst00;
       If %Found(wtstl);
          svPrtZrFlg = STflag;
          svStatus   = STstat;
       EndIf;
      /End-free
     C*
     C     svPrtZrFlg    IFEQ      'Y'
     C     svStatus      ANDEQ     'A'
     C     NXCLAS        ORNE      RRTERC
     C     NXCLAS        ANDNE     RRDTRC
     C     NXEMPR        ORNE      *ZERO
     C                   MOVE      ##LNK         P05LNK
     C                   MOVE      '05'          P05RTP
     C                   MOVE      NXST          P05ST
     C                   MOVE      NXCLAS        P05CLC
     C                   MOVE      *BLANKS       P05CWS
     C                   If        P05ST = 29 or P05ST = 04
     C                   MOVE      '00'          P05CWS
     C                   ENDIF
      /free
       If NXcsuf <> *blanks;
          P05cws = NXcsuf;
       Endif;
      /end-free
     C                   Z-ADD     NXEXPA        P05EXC                         ADD 07/03/00
     C                   Z-ADD     0             P05CHR
     C                   MOVE      NXRT          P05CHR
     C*
     C* Report a decimal rate for assessments to the state of PA.
     C*
     C                   If        QQSt = 'PA' or QQSt = 'DE'
     C
     C                   If        NXClas = RR1stC or NXClas = RR2ndC
     C                             or NXClas = RR3rdC or NXClas = RR4thC
     C                             or NXClas = RR5thC or NXClas = RR6thC
     C                             or NXClas = RR7thC
     C
     C*** begin chg ***  08/11/25  ******************************************
     C******             Eval      P05CHR = P05CHR / 100
     C                   Eval      P05CHR = P05CHR / 1000
     C*** end chg *****  08/11/25  ******************************************
     C
     C                   Endif
     C
     C*** begin add ***  08/11/25  ******************************************
     C                   Else
     C* Correct assessments for other states by dividing by 10 due to file chg.
     C                   If        NXClas = RR1stC or NXClas = RR2ndC
     C                             or NXClas = RR3rdC or NXClas = RR4thC
     C                             or NXClas = RR5thC or NXClas = RR6thC
     C                             or NXClas = RR7thC
     C
     C                   Eval      P05CHR = P05CHR / 10
     C
     C                   Endif
     C*** end add *****  08/11/25  ******************************************
     C                   Endif                                                  If QQSt = 'PA'/'DE'
     C     NXCLAS        IFNE      RRDBCD
     C     NXCLAS        ANDNE     RRCRCD
     C     NXCLAS        ANDNE     RRMRRD
     C     NXCLAS        ANDNE     RRMRRC
     C     NXCLAS        ANDNE     RRMRRN
     C     NXCLAS        ANDNE     RRTERC
     C     NXCLAS        ANDNE     RRDTRC
     C     NXCLAS        ANDNE     RR1STC
     C     NXCLAS        ANDNE     RR2NDC
     C     NXCLAS        ANDNE     RR3RDC
     C     NXCLAS        ANDNE     RR4THC
     C     NXCLAS        ANDNE     RR5THC
     C     NXCLAS        ANDNE     RR6THC
     C     NXCLAS        ANDNE     RR7THC
     C     NXCLAS        ANDNE     XXSTCD
     C*** begin chg ***  08/11/25  ******************************************
     C******             MULT      100           P05CHR
     C                   MULT      10            P05CHR
     C*** end chg *****  08/11/25  ******************************************
     C                   ENDIF                                                  NXCLAS IFNE
     C*** begin add ***  08/11/25  ******************************************
     C     NXCLAS        IFEQ      RRDBCD
     C     NXCLAS        OREQ      RRCRCD
     C     NXCLAS        OREQ      RRMRRD
     C     NXCLAS        OREQ      RRMRRC
     C     NXCLAS        OREQ      RRMRRN
     C     NXCLAS        OREQ      RRTERC
     C     NXCLAS        OREQ      RRDTRC
     C     NXCLAS        OREQ      XXSTCD
     C                   Eval      P05CHR = P05CHR / 10
     C                   ENDIF                                                  NXCLAS IFNE
     C*** end add *****  08/11/25  ******************************************
      /free

        //* Report a "factor" for increased limits/IL minimum for WI.

        If QQSt = 'WI';
           If NXClas = RRILMC;
              If P05CHR <> 0;
                 P05CHR = P05CHR / 100;
              Endif;
           Else;
              If xxSt <> ilState;
                 Setll elky1 pel02;
                 Reade(n) elky1 pel02;
                 Dow not %eof;
                    Chain rsky prs01;
                    If %found and RSjrst = xxst;
                       If ELilcd <> ' ';
                          xxilcd = ELilcd;
                          xxilef = ELpfdt;
                          Setgt lmky plm01;
                          Readpe(n) lmky1 plm01;
                          If not %eof;
                             ilStatCd = LMclas;
                             ilState = xxst;
                             Leave;
                          Endif;
                       Endif;
                    Endif;
                    Reade(n) elky1 pel02;
                 Enddo;
                 If %eof(wdell1);
                    ilState = xxSt;
                    Clear ilStatCd;
                 Endif;
              Endif;

              If NXClas = ilStatCd and ilStatCd <> 0;
                 If P05CHR <> 0;
                    P05CHR = P05CHR / 100;
                 Endif;
              Endif;

           Endif;

        Endif;

      /end-free
     C                   MOVE      NXEFDT        P05EED
     C                   MOVE      *ZEROS        P05BMR
     C                   Z-ADD     NXEAPR        P05EEX
     C                   Z-ADD     NXEMPR        P05EPR
     C                   MOVE      NXEXPP        P05EXP
     C                   MOVEL(P)  NXDESC        P05CLW
     C*
     C                   Z-ADD     0             ##ENCD
     C     NKEY          CHAIN     PNA01                              60
     C     *IN60         DOWEQ     *OFF
     C     NAELCD        IFEQ      NXELCD
     C                   MOVE      NAENCD        ##ENCD
     C                   ENDIF
     C     NKEY          READE     PNA01                                  60
     C                   ENDDO
     C*
     C                   MOVE      NXST          ##ST1
     C                   Z-ADD     NXELCD        ##ELCD
     C*
     C                   MOVE      ##NEL         P05NEL
     C                   If        P05ST = 04 and ##ENCD = 0
     C                   Z-add     1             ##ENCD
     C                   Z-add     1             ##ELCD
     C                   MOVE      ##NEL         P05NEL
     C                   Endif
     C                   MOVE      *ZEROS        P05SRT
     C                   MOVE      *ZEROS        P05#PA
     C                   MOVE      *ZEROS        P05#VL
     C                   MOVE      NXPSUR        P05PSR
     C                   MOVE      *ZEROS        P05PAF
     C                   MOVE      NHPCDT        P05EFD
     C                   MOVE      NHXPC         P05EXD
     C     XSREIS        IFEQ      'Y'
     C     NHPCDT        ANDEQ     *ZERO
     C                   MOVE      NHPFDT        P05EFD
     C                   MOVE      NHPTDT        P05EXD
     C                   ENDIF                                                  XSREIS IFEQ
     C*** beg Chg *** 05/28/24 *****************************************
     C*****              If        NXPayEfd <> 0
     C                   If        NXPayEfd <> 0 and nxPayEFD <> NHPFDT
     C                             and XSREIS = 'Y'
     C*** end Chg *** 05/28/24 *****************************************
     C                   Move      NXPayEfd      P05EFD
     C                   Endif
     C*** begin add ***  10/31/24  *********************************************
     C                   If        QQSt = 'CA' and (##TRCD = 1 or ##TRCD = 2)
     C                   Z-add     0             P05EFD
     C                   Z-add     0             P05EXD
     C                   Endif
     C*** end add *****  10/31/24  *********************************************
     C*
     C* Do not report change dates to CA if no changes have occurred on record.
     C*
     C                   If        QQST = 'CA' and XSREIS = 'Y'
     C*
     C                   If        newExpo = ' '
     C                   Z-add     0             P05EFD
     C                   Z-add     0             P05EXD
     C                   Endif
     C*
     C                   Endif
     C*
     C* SPLIT MANAGED CARE EXPOSURE INTO TWO RECORDS FOR NJ/DE/PA.
     C*
     C     NXCLAS        IFEQ      RR1ACD
     C     NHMPST        OREQ      '29'
     C     NXCLAS        ANDEQ     9874
     C*
     C     NHANDT        IFNE      *ZERO
     C     NHANDT        ANDLT     NHPTDT
     C     NHANDT        ANDGT     NHPFDT
     C     QQST          ANDNE     *BLANKS
     C*
     C     NHMPST        IFEQ      '29'
     C     NHMPST        OREQ      '07'
     C     NHMPST        OREQ      '37'
     C*
     C                   Z-ADD     0             XXTOTL           11 2
     C                   Z-ADD     0             XXTOT1           11 2
     C*
     C*
     C*  GET NUMBER OF EMPLOYER POLICY DAYS
     C*
     C*
     C     ELKY1         CHAIN     PPP01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     PPST          IFEQ      NHST
     C*
     C                   SELECT
     C*
     C     NXCLAS        WHENEQ    RR1ACD
     C     PPFRDT        IFLT      NHANDT
     C                   ADD       PPEDCM        XXTOTL
     C                   ELSE
     C                   ADD       PPEDCM        XXTOT1
     C                   ENDIF
     C*
     C     NXCLAS        WHENEQ    RRDV1C
     C     PPFRDT        IFLT      NHANDT
     C                   ADD       PPEDC2        XXTOTL
     C                   ELSE
     C                   ADD       PPEDC2        XXTOT1
     C                   ENDIF
     C*
     C     NXCLAS        WHENEQ    RRDV2C
     C     PPFRDT        IFLT      NHANDT
     C                   ADD       PPEDV2        XXTOTL
     C                   ELSE
     C                   ADD       PPEDV2        XXTOT1
     C                   ENDIF
     C*
     C                   ENDSL                                                  NXCLAS SELEC
     C*
     C                   ENDIF                                                  PPST IFEQ
     C*
     C     ELKY1         READE     PPP01                                  60
     C                   ENDDO
     C*
     C                   Z-ADD     XXTOTL        P05EPR
     C                   Z-ADD     NHPFDT        P05EED
     C*
     C                   CLEAR                   RDATA
     C*
     C                   MOVEL     P05           RDATA
     C                   WRITE     RPOLS
     C*
     C                   ADD       1             ##TOTR           10 0
     C*
     C                   Z-ADD     XXTOT1        P05EPR
     C                   MOVE      NHANDT        P05EED
     C*
     C                   ENDIF                                                  NHMPST IFEQ
     C*
     C                   ENDIF                                                  NHANDT IFNE
     C*
     C                   ENDIF                                                  RR1ACD IFEQ
     C********
     C* SPLIT ASSESSMENTS AND TERRORISM CHARGES INTO TWO RECORDS FOR PA.
     C********
     C*
     C                   CLEAR                   RDATA
     C**** WRITE ADDR RECORD
     C*
     C                   MOVEL     P05           RDATA
     C                   WRITE     RPOLS
     C                   ADD       1             ##TOTR           10 0
     C                   ENDIF                                                  NXCLAS IFEQ
     C                   Endif                                                  If NHSadd <> 'P'
     C*
     C     NKEY          READE(N)  PNX02                                  63
     C                   ENDDO
     C     NKEY          CHAIN(n)  PNH01                              60
     C*
     C                   Exsr      SREndorse
      *** Beg Add ***** 02/14/24 ************************************
       Clear xxExcl99;
       Setgt (xxco#:xxfnd:QQst:NHpfdt) pnx00;
       Readpe (xxco#:xxfnd:QQst) pnx00;
       If not %Eof(WMNXL);
          xxExcl99 = NXexcl99;
       Endif;
      *** End Add ***** 02/14/24 ************************************
     C                   CLEAR                   P07
     C                   CLEAR                   P07REC
     C                   MOVE      *BLANKS       SVSTID            2
     C                   MOVE      *BLANK        XXFRST            1
     C                   Z-ADD     0             I                 2 0
     C     MUKY1         CHAIN(N)  PMU02                              60
     C     *IN60         DOWEQ     *OFF
      /Free
       Setgt  (MUco#:MUfnd:MUform) pen00;
       Readpe (MUco#:MUfnd:MUform) pen00;
       If %eof;
          Clear enexfl;
          Clear enexcwc;
          Clear ennatfm;
          ENNatFm = MUNatFm;
       Endif;
       //*** begin add ***  12/12/22  ********************************************
       Select;
       When xxclt# = '153' and %subst(MUForm:1:8) = 'WC991011';
          MUNatFm = MUForm;
          ENNatFm = MUForm;
       Endsl;
       //*** end add *****  12/12/22  ********************************************
       //*** begin add ***  05/09/22  ********************************************
       Clear svForm;
       If not %eof and ENFormO <> ' ';
          svForm = MUForm;
          MUForm = ENFormO;
       Endif;
       //*** end add *****  05/09/22  ********************************************
       If ENExFL <> 'Y' And ENexcWC <> 'Y';
      *** Beg Add ***** 02/14/24 ************************************
          If (%Subst(MUform:1:4) <> 'WC99' OR (%Subst(MUform:1:4) = 'WC99'
             and xxExcl99 <> 'Y'));
      *** End Add ***** 02/14/24 ************************************
      /End-Free
     C                   MOVEL     MUFORM        XXF2              2
     C     XXF2          IFNE      'PN'
     C     QQST          ORNE      'CA'
     C     XXF2          ANDNE     'PN'
     C     QQST          ORNE      'CA'
     C     XXF2          ANDEQ     'PN'
     C     MUENST        ANDNE     'CA'
     C     QQST          OREQ      'CA'
     C     XXF2          ANDEQ     'PN'
     C     pnPrint       ANDEQ     'Y'
     C                   MOVEL     MUFORM        @#FORM                         Data Structure
     C*
     C                   MOVE      '00'          XXSTID
     C*
     C     MUENST        IFNE      *BLANKS
     C                   MOVEL(P)  MUENST        XXKEY
     C*
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      STNST         XXSTID
     C                   ENDIF
     C*
     C                   ENDIF                                                  MUENST IFNE
     C*
      /free
       Select;
          When MUform = 'WC990601' and qqSt = 'WI' and
           GetGroupAssoc (XXco#:XXfnd) = 'UWIC';
             isProcessEnd = *off;
          When MUform = 'WC990604' and qqSt = 'WI' and
           GetGroupAssoc (XXco#:XXfnd) = 'UWIC';
             isProcessEnd = *off;

          When MUform = 'WC990605' and qqSt = 'WI' and
           GetGroupAssoc (XXco#:XXfnd) = 'UWIC';
               isProcessEnd = *off;

          When MUform = 'WC990609A' and qqSt = 'WI' and
           GetGroupAssoc (XXco#:XXfnd) = 'UWIC';
             isProcessEnd = *off;

          When MUend# <> 0 and qqSt = 'PA' and MUform <> 'MID-YEAR';
             isProcessEnd = *on;

          When MUend# <> 0 and qqSt = 'DE' and MUform <> 'MID-YEAR';
             isProcessEnd = *on;

          When MUend# <> 0 and qqSt = 'MA' and MUform <> 'MID-YEAR';
             isProcessEnd = *on;

          When MUend# <> 0 and qqSt = 'CA' and MUform = 'WC890600B'
           and ##trcd <> 01 and ##trcd <> 02;
             isProcessEnd = *on;

          When MUend# = 0 and MUexFl = 'I';
               isProcessEnd = *on;

          Other;
             isProcessEnd = *off;
       EndSl;

       If isProcessEnd;

       index = %LookUp(muform:ppa);

       If (muend# <> 0 and index = 0) or muend# = 0;

      /end-free
     C     XXSTID        IFEQ      SVSTID
     C     XXFRST        OREQ      *BLANK
     C                   ADD       1             I
     C                   If        ENnatFm = ' '
     C                   MOVEL(P)  MUFORM        XXEND#
     C                   MOVEL(P)  MUFORM        XX9               9
     C                   MoveL(p)  ' '           xxEndE
     C                   Else
     C                   MoveL(P)  ENnatFm       XXEND#
     C                   MoveL(P)  ENnatFm       XX9               9
     C                   MoveL(p)  MUform        xxEndE
     C*** begin chg ***  06/07/24  ***********************************************
     C******             If        MUNatFm <> ' '
     C* Report as carrier version only if present from carrier dec page/schedule
     C* or endorsement has same value in both form and national/state form fields.
     C                   If        MUNatFm <> ' ' or ENForm = ENNatFm
     C*** end chg *****  06/07/24  ***********************************************
     C                   Clear                   xxend#
     C                   Clear                   xx9
     C                   Endif
     C                   EndIf
     C                   MOVE      XX9           XXENDB
     C                   MOVE      XXEND         PPA(I)
     C*
     C     XXFRST        IFEQ      *BLANK
     C                   MOVE      'N'           XXFRST
     C                   MOVE      XXSTID        SVSTID
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXSTID IFEQ
     C*
     C     I             IFEQ      11
     C     XXSTID        ORNE      SVSTID
     C     I             ANDNE     0
     C                   MOVE      ##LNK         P7LNK
     C                   MOVE      SVSTID        P7ST
     C                   MOVE      '07'          P7RTP
     C                   MOVEL     P07REC        P07
     C     SVSTID        IFNE      '00'
     C*
     C                   MOVE      SVSTID        XXNST
     C     STKY2         CHAIN     PST02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     STKEY         XXST
     C                   ENDIF
     C*
     C     NKEY          CHAIN(n)  PNH01                              60
     C     *IN60         DOWEQ     *OFF
     C     NHST          ANDNE     XXST
     C     NKEY          READE(n)  PNH01                                  60
     C                   ENDDO
     C*
     C                   ENDIF                                                  SVSTID IFNE
     C*
     C                   MOVE      NHXPC         P07EXD
     C                   MOVE      NHPCDT        P07EFD
     C     XSREIS        IFEQ      'Y'
     C     NHPCDT        ANDEQ     *ZERO
     C                   MOVE      NHPFDT        P07EFD
     C                   MOVE      NHPTDT        P07EXD
     C                   ENDIF                                                  XSREIS IFEQ
     C*** begin add ***  10/31/24  *********************************************
     C                   If        QQSt = 'CA' and (##TRCD = 1 or ##TRCD = 2)
     C                   Z-add     0             P07EFD
     C                   Z-add     0             P07EXD
     C                   Endif
     C*** end add *****  10/31/24  *********************************************
     C*
     C* Do not report change dates to CA if no changes have occurred on record.
     C*
     C                   If        QQST = 'CA' and XSREIS = 'Y'
     C*
     C                   If        newEndr = ' '
     C                   Z-add     0             P07EFD
     C                   Z-add     0             P07EXD
     C                   Endif
     C*
     C                   Endif
     C*
     C                   CLEAR                   RDATA
     C**** WRITE ADDR RECORD
     C*
     C                   If        NHSadd <> 'P' or QQSt <> '  '
     C                   MOVEL     P07           RDATA
     C                   WRITE     RPOLS
     C                   ADD       1             ##TOTR           10 0
     C                   Endif
     C*
     C                   CLEAR                   P07
     C                   CLEAR                   P07REC
     C                   Z-ADD     0             I
     C                   ENDIF
     C*
     C     XXSTID        IFNE      SVSTID
     C                   ADD       1             I
     C                   If        ENnatFm = ' '
     C                   MOVEL(P)  MUFORM        XXEND#
     C                   MOVEL(P)  MUFORM        XX9               9
     C                   MoveL(p)  ' '           xxEndE
     C                   Else
     C                   MoveL(P)  ENnatFm       XXEND#
     C                   MoveL(P)  ENnatFm       XX9               9
     C                   MoveL(p)  MUform        xxEndE
     C*** begin chg ***  06/07/24  ***********************************************
     C******             If        MUNatFm <> ' '
     C* Report as carrier version only if present from carrier dec page/schedule
     C* or endorsement has same value in both form and national/state form fields.
     C                   If        MUNatFm <> ' ' or ENForm = ENNatFm
     C*** end chg *****  06/07/24  ***********************************************
     C                   Clear                   xxend#
     C                   Clear                   xx9
     C                   Endif
     C                   EndIf
     C                   MOVE      XX9           XXENDB
     C                   MOVE      XXEND         PPA(I)
     C                   MOVE      XXSTID        SVSTID
     C                   ENDIF                                                  XXSTID IFNE
      /free

       Clear xmu00;

       XUCo#  = MUCo#;
       XUFnd  = MUfnd;
       XUEmp# = MUEmp#;
       XUDiv  = MUDiv;
       XUFyr  = MUFyr;
       XUUnid = NHUnid;
       XUForm = MUForm;
       //*** begin add ***  05/09/22  **************************************
       If svForm <> ' ';
          XUForm = svForm;
       Endif;
       //*** end add *****  05/09/22  **************************************
       XUEnst = MUEnst;

       XUPgm  = Q1Pgm;
       XUUser = Q1User;
       Exsr CnvDate;
       XUTime = X2Time;
       XUDt   = X2Date;

       Write xmu00;

      /end-free
     C*
     C                   ENDIF                                                  If muend# <> 0
     C                   ENDIF                                                  If isProcessEnd
     C                   ENDIF                                                  XXF2 IFNE   0

      /Free
      *** Beg Add ***** 02/14/24 ************************************
          Endif;
      *** End Add ***** 02/14/24 ************************************
       Endif; // ENExFL <> 'Y'
      /End-Free

     C     MUKY1         READE(N)  PMU02                                  60
     C                   ENDDO
     C*
     C     I             IFNE      0
     C                   MOVE      ##LNK         P7LNK
     C                   MOVE      SVSTID        P7ST
     C                   MOVE      '07'          P7RTP
     C                   MOVEL     P07REC        P07
     C     SVSTID        IFNE      '00'
     C*
     C                   MOVE      SVSTID        XXNST
     C     STKY2         CHAIN     PST02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     STKEY         XXST
     C                   ENDIF
     C*
     C     NKEY          CHAIN(n)  PNH01                              60
     C     *IN60         DOWEQ     *OFF
     C     NHST          ANDNE     XXST
     C     NKEY          READE(n)  PNH01                                  60
     C                   ENDDO
     C*
     C                   ENDIF                                                  SVSTID IFNE
     C*
     C                   MOVE      NHXPC         P07EXD
     C                   MOVE      NHPCDT        P07EFD
     C     XSREIS        IFEQ      'Y'
     C     NHPCDT        ANDEQ     *ZERO
     C                   MOVE      NHPFDT        P07EFD
     C                   MOVE      NHPTDT        P07EXD
     C                   ENDIF                                                  XSREIS IFEQ
     C*** begin add ***  10/31/24  *********************************************
     C                   If        QQSt = 'CA' and (##TRCD = 1 or ##TRCD = 2)
     C                   Z-add     0             P07EFD
     C                   Z-add     0             P07EXD
     C                   Endif
     C*** end add *****  10/31/24  *********************************************
     C*
     C* Do not report change dates to CA if no changes have occurred on record.
     C*
     C                   If        QQST = 'CA' and XSREIS = 'Y'
     C*
     C                   If        newEndr = ' '
     C                   Z-add     0             P07EFD
     C                   Z-add     0             P07EXD
     C                   Endif
     C*
     C                   Endif
     C*
     C                   CLEAR                   RDATA
     C**** WRITE ADDR RECORD
     C*
     C                   If        NHSadd <> 'P' or QQSt <> '  '
     C                   MOVEL     P07           RDATA
     C                   WRITE     RPOLS
     C                   ADD       1             ##TOTR           10 0
     C                   Endif
     C*
     C                   CLEAR                   P07
     C                   CLEAR                   P07REC
     C                   Z-ADD     0             I
     C                   ENDIF
     C*
      /free
       //*** begin del ***  01/31/22  ************************************
       //*****   //* Clean up the temporary file for next policy run.
       //*****   Close wmmul7;
       //*****
       //*****   Monitor;
       //*****      clear strCommand;
       //*****      strCommand = 'DLTOVR FILE(WMMUP)';
       //*****      Callp QCMDEXC(strCommand:%len(strCommand));
       //*****   On-error;
       //*****   Endmon;
       //*****
       //*****   Monitor;
       //*****      clear strCommand;
       //*****      strCommand = 'DLTOVR FILE(WMMUL7)';
       //*****      Callp QCMDEXC(strCommand:%len(strCommand));
       //*****   On-error;
       //*****   Endmon;
       //*****
       //*****   Monitor;
       //*****      clear strCommand;
       //*****      strCommand = 'DLTF FILE(QTEMP/WMMUL7)';
       //*****      Callp QCMDEXC(strCommand:%len(strCommand));
       //*****   On-error;
       //*****   Endmon;
       //*****
       //*****   Monitor;
       //*****      clear strCommand;
       //*****      strCommand = 'DLTF FILE(QTEMP/WMMUP)';
       //*****      Callp QCMDEXC(strCommand:%len(strCommand));
       //*****   On-error;
       //*****   Endmon;
       //*** end del *****  01/31/22  ************************************

      /end-free
     C     NKEY          CHAIN(n)  PNH01                              60
     C*
     C                   If        QQSt <> 'CA'
     C*
     C* REPORT ALL EXCLUDED STATES AS WELL.
     C*
     C                   Z-ADD     0             I                 2 0
     C                   CLEAR                   P06
     C                   CLEAR                   P06REC
     C                   CLEAR                   EST
     C     XSKY          CHAIN     PXS01                              60
     C     *IN60         IFEQ      *OFF
     C     *IN60         DOWEQ     *OFF
     C*
     C                   MOVEL(P)  XSST          XXKEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      *OFF
     C                   ADD       1             I
     C                   MOVE      STNST         EST(I)
     C                   If        I = 60
     C                   Leave
     C                   Endif
     C                   ENDIF
     C*
     C     XSKY          READE     PXS01                                  60
     C                   ENDDO
     C*
     C*
     C* ADDITIONALLY, INCLUDE ANY STATES ON THE POLICY WITHIN THE LIST.
     C*
     C     ELKY1         CHAIN     PEL02                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL(P)  RSJRST        XXKEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     1             Z                 2 0
     C     STNST         LOOKUP    EST(Z)                                 60
     C*
     C     *IN60         IFEQ      *OFF
     C                   ADD       1             I
     C                   MOVE      STNST         EST(I)
     C                   If        I = 60
     C                   Leave
     C                   Endif
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     ELKY1         READE     PEL02                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   MOVE      ##LNK         P06LNK
     C                   MOVE      '06'          P06RTP
     C                   MOVE      '2'           P06IEI
     C                   MOVE      NHPCDT        P06EFD
     C                   MOVE      NHXPC         P06EXD
     C     XSREIS        IFEQ      'Y'
     C     NHPCDT        ANDEQ     *ZERO
     C                   MOVE      NHPFDT        P06EFD
     C                   MOVE      NHPTDT        P06EXD
     C                   ENDIF                                                  XSREIS IFEQ
     C*** begin add ***  10/31/24  *********************************************
     C                   If        QQSt = 'CA' and (##TRCD = 1 or ##TRCD = 2)
     C                   Z-add     0             P06EFD
     C                   Z-add     0             P06EXD
     C                   Endif
     C*** end add *****  10/31/24  *********************************************
     C                   MOVEL     P06           XXDTA1
     C                   MOVEL     P06REC        P06
     C*
     C                   CLEAR                   RDATA
     C**** WRITE RECORD
     C*
     C                   MOVEL     P06           RDATA
     C                   WRITE     RPOLS
     C                   ADD       1             ##TOTR           10 0
     C                   ENDIF                                                  *IN60 IFEQ *OFF
     C*
     C                   Endif                                                  If QQSt <> 'CA'
     C*
     C*
     C* REPORT THE ENDORSEMENTS FROM WXPEP.
     C*
     C                   Z-ADD     0             X15               2 0
     C                   Z-ADD     0             X21               2 0
     C                   Z-ADD     0             X24               2 0
     C                   Z-ADD     0             X25               2 0
     C                   Z-ADD     0             X27               2 0
     C                   Z-ADD     0             X29               2 0
     C*** Begin Add ***  08/09/24  *** WC2978 *******************************
     C                   Z-ADD     0             XDK               2 0
     C*** End Add *****  08/09/24  *** WC2978 *******************************
     C*
     C***** Correct bad data - S37310 ********************************
     C                   Exsr      CorData
     C*
     C     PEKY          CHAIN     PPE01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C*
     C* Rename forms when needed.
     C*
     C                   Select
     C                   When      xxclt# = '148'
     C* Amwins forms
     C                   Select
     C                   When      PEEnd# = 'WC000406SS'
     C                   Eval      PEEnd# = 'WC000406'
     C                   When      PEEnd# = 'WC000406ASS'
     C                   Eval      PEEnd# = 'WC000406A'
     C                   Endsl
     C*
     C                   Endsl
     C                   Move      *on           xxEndorse         1
     C*
     C                   If        PESt <> 0 and PESt <> 99
     C*
     C                   Clear                   XXST
     C                   MOVE      PEST          XXNST
     C     STKY2         CHAIN     PST02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     STKEY         XXST
     C                   ENDIF
     C*
     C     NKEY          CHAIN(n)  PNH01                              60
     C     *IN60         DOWEQ     *OFF
     C     NHST          ANDNE     XXST
     C     NKEY          READE(n)  PNH01                                  60
     C                   ENDDO
     C*
     C                   If        XXST <> '  ' and XXST = NHST and
     C                             not (NHSadd <> 'P' or QQSt <> '  ')
     C*
     C                   Move      *off          xxEndorse
     C*
     C                   Endif
     C*
     C                   Endif
     C*
     C                   If        xxEndorse = *on
     C                   Clear                   xxForm8           8
     C                   Eval      xxForm8 = %subst(peend#:1:8)
     C                   SELECT
     C*
     C     PEEND#        WHENEQ    'WC070401'                                   DE Deductible
     C     xxClt#        OREQ      '150'
     C     PEEND#        ANDEQ     'WC990603'
     C     PEST          ANDEQ     07
     C     xxClt#        OREQ      '150'
     C     xxForm8       ANDEQ     'WC990603'
     C     PEST          ANDEQ     07
     C     xxClt#        OREQ      '150'
     C     PEEND#        ANDEQ     'WC990618'
     C     PEST          ANDEQ     07
     C                   EXSR      SR07401
     C     PEEND#        WHENEQ    'WC310315'                                   NY Deductible
     C     xxClt#        OREQ      '150'
     C     PEEND#        ANDEQ     'WC993102'
     C     xxClt#        OREQ      '150'
     C     PEEND#        ANDEQ     'WC993103'
     C                   EXSR      SR31315
     C     PEEND#        WHENEQ    'WC040603B'                                  CA Deductible
     C*** Begin Add *** 09/21/22 *******************************************
     C     xxClt#        OREQ      '150'
     C     PEEND#        ANDEQ     'WC990403'
     C*** End   Add *** 09/21/22 *******************************************
     C                   EXSR      SR04603
     C     PEEND#        WHENEQ    'WC000407'                                   RATE CHANGE
     C                   EXSR      SR0407
     C     PEEND#        WHENEQ    'WC000404'                                   PENDING RATE CHANGE
     C                   EXSR      SR0404
      *** Beg Add ***** 11/15/22 *********************************
     C     PEEND#        WHENEQ    'WC000405'                                   Poliy Period Endorse
     C                   EXSR      SR0405
      *** End Add ***** 11/15/22 *********************************
     C     PEEND#        WHENEQ    'WC000402'                                   ANNIV RATING DATE
     C                   EXSR      SR0402
     C     PEEND#        WHENEQ    W00503                                       RETROSPECTIVE PREMIU
     C     PEEND#        OREQ      W29503                                       NJ RETROSPECTIVE PRE
     C                   EXSR      SR0503
     C     PEEND#        WHENEQ    W00106                                       LONGSHORE AND HARBOR
     C                   EXSR      SR0106
     C     PEEND#        WHENEQ    W00201                                       MARITIME COVERAGE
     C                   EXSR      SR0201
     C     PEEND#        WHENEQ    W00301                                       ALTERNATE EMPLR COVG
     C                   EXSR      SR0301
     C     PEEND#        WHENEQ    'WC000302'                                   DESIGNATED WORKPLACE
     C                   EXSR      SR0302
     C     PEEND#        WHENEQ    'WC000102'                                   COAL MINE
     C                   EXSR      SR0102
     C     PEEND#        WHENEQ    W00311                                       VOL COMP AND EMPL LI
     C                   EXSR      SR0311
     C     PEEND#        WHENEQ    W00406                                       PREMIUM DISCOUNT
     C     PEEND#        OREQ      'WC000406'
     C                   EXSR      SR0406
     C     PEEND#        WHENEQ    'WC000313'                                   WAIVER OF RIGHT
     C     PEEND#        OREQ      LUBWV
     C                   EXSR      SR0313
     C     PEEND#        WHENEQ    'WC000310'                                   SOLE PROPRIETORS
     C                   EXSR      SR0310
     C     PEEND#        WHENEQ    'WC000308'                                   PARTNERS, OFF EXCLUS
     C                   EXSR      SR0308
     C     PEEND#        WHENEQ    W00401                                       AIRCRAFT PREMIUM
     C                   EXSR      SR0401
     C     PEEND#        WHENEQ    'WC000315'                                   DOMESTIC/AGRI EXCLUS
     C                   EXSR      SR0315
     C     PEEND#        WHENEQ    'WC370604'
     C                   EXSR      SR37604
     C     PEEND#        WHENEQ    'WC370403'                                   DEDUCTIBLE
     C     PEEND#        OREQ      'WC000603'
     C     PEEND#        OREQ      'WC090603'
      *** Beg Add ***** 02/27/23 ****************************
     C     PEEND#        OREQ      'WC150603'
      *** End Add ***** 02/27/23 ****************************
     C     PEEND#        OREQ      'WC090605'
     C     PEEND#        OREQ      'WC100602'
     C     PEEND#        OREQ      'WC120602A'
     C     PEEND#        OREQ      'WC130602'
     C     PEEND#        OREQ      'WC130604'
     C     PEEND#        OREQ      'WC240603'
     C     PEEND#        OREQ      'WC320302'
     C*** Begin Add *** 01/23/23 *******************************************
     C     PEEND#        OREQ      'WC350602D'
     C*** End   Add *** 01/23/23 *******************************************
     C     PEEND#        OREQ      'WC420602A'
     C     PEEND#        OREQ      'WC420605'
     C     PEEND#        OREQ      'WC420606'
     C     PEEND#        OREQ      'WC450603'
     C*** Begin Add *** 02/17/22 *******************************************
     C     PEEND#        OREQ      'WC470602'
     C*** End   Add *** 02/17/22 *******************************************
     C     xxClt#        OREQ      '150'
     C     PEEND#        ANDEQ     'WC990202'
     C     xxClt#        OREQ      '150'
     C     PEEND#        ANDEQ     'WC990318'
     C*** Begin Add *** 09/21/22 *******************************************
     C     xxClt#        OREQ      '150'
     C     PEEND#        ANDEQ     'WC990403'
     C*** End   Add *** 09/21/22 *******************************************
     C     xxClt#        OREQ      '150'
     C     PEEND#        ANDEQ     'WC990603'
     C     xxClt#        OREQ      '150'
     C     PEEND#        ANDEQ     'WC990203'
     C     xxClt#        OREQ      '150'
     C     PEEND#        ANDEQ     'WC990303'
     C     xxClt#        OREQ      '150'
     C     PEEND#        ANDEQ     'WC994103'
     C     xxClt#        OREQ      '150'
     C     PEEND#        ANDEQ     'WC994103A'
     C     xxClt#        OREQ      '150'
     C     PEEND#        ANDEQ     'WC995402'
     C     xxClt#        OREQ      '150'
     C     PEEND#        ANDEQ     'WC994203'
     C     xxClt#        OREQ      '150'
     C     PEEND#        ANDEQ     'WC990603A'
     C     xxClt#        OREQ      '150'
     C     xxForm8       ANDEQ     'WC990603'
     C     xxClt#        OREQ      '150'
     C     PEEND#        ANDEQ     'WC990610'
     C     xxClt#        OREQ      '150'
     C     PEEND#        ANDEQ     'WC990618'
     C     xxClt#        OREQ      '150'
     C     PEEND#        ANDEQ     'WC990910'
     C     xxClt#        OREQ      '150'
     C     PEEND#        ANDEQ     'WC990911'
     C     xxClt#        OREQ      '150'
     C     PEEND#        ANDEQ     'WC991118'
     C     xxClt#        OREQ      '150'
     C     PEEND#        ANDEQ     'WC991618'
     C     xxClt#        OREQ      '150'
     C     PEEND#        ANDEQ     'WC993618'
     C     xxClt#        OREQ      '150'
     C     PEEND#        ANDEQ     'WC993718'
     C     xxClt#        OREQ      '150'
     C     PEEND#        ANDEQ     'WC990608'
     C     xxClt#        OREQ      '150'
     C     PEEND#        ANDEQ     'WC990609'
     C     xxClt#        OREQ      '150'
     C     PEEND#        ANDEQ     'WC990611'
     C     xxClt#        OREQ      '150'
     C     PEEND#        ANDEQ     'WC990612'
     C*** Begin Add *** 02/09/22 *******************************************
     C     xxClt#        OREQ      '150'
     C     PEEND#        ANDEQ     'WC990678'
     C*** End   Add *** 02/09/22 *******************************************
     C     xxClt#        OREQ      '150'
     C     PEEND#        ANDEQ     'WC992002'
     C*** Begin Add *** 09/21/22 *******************************************
     C     xxClt#        OREQ      '150'
     C     PEEND#        ANDEQ     'WC992003'
     C*** End   Add *** 09/21/22 *******************************************
     C     xxClt#        OREQ      '150'
     C     PEEND#        ANDEQ     'WC992518'
     C     xxClt#        OREQ      '150'
     C     PEEND#        ANDEQ     'WC992818'
     C     xxClt#        OREQ      '150'
     C     PEEND#        ANDEQ     'WC992918'
     C     xxClt#        OREQ      '150'
     C     PEEND#        ANDEQ     'WC993501'
     C     xxClt#        OREQ      '150'
     C     PEEND#        ANDEQ     'WC994118'
     C     xxClt#        OREQ      '150'
     C     PEEND#        ANDEQ     'WC994218'
     C     xxClt#        OREQ      '150'
     C     PEEND#        ANDEQ     'WC994518'
     C*** Begin Add *** 02/09/22 *******************************************
     C     xxClt#        OREQ      '150'
     C     PEEND#        ANDEQ     'WC995203'
     C*** End   Add *** 02/09/22 *******************************************
    C*** Beg Add *** 08/28/24 *** 150WGA **********************************
     C     xxClt#        OREQ      '150'
     C     PEEND#        ANDEQ     'WC995410'
    C*** End Add *** 08/28/24 *** 150WGA **********************************
     C     xxClt#        OREQ      '133'
     C     PEEND#        ANDEQ     'WC990401'
     C     xxClt#        OREQ      '133'
     C     PEEND#        ANDEQ     'WC990401A'
     C     xxClt#        OREQ      '133'
     C     PEEND#        ANDEQ     'WC990401M'
     C     xxClt#        OREQ      '133'
     C     PEEND#        ANDEQ     'WC990501'
      *** Beg Add ***** 12/05/22 **************************************
     C     xxClt#        OREQ      '153'
     C     PEEND#        ANDEQ     'WC991011'
     C     xxClt#        OREQ      '153'
     C     PEEND#        ANDEQ     'WC991011FL'
     C     xxClt#        OREQ      '153'
     C     PEEND#        ANDEQ     'WC991011GA'
     C     xxClt#        OREQ      '153'
     C     PEEND#        ANDEQ     'WC991011MD'
     C     xxClt#        OREQ      '153'
     C     PEEND#        ANDEQ     'WC991011OK'
     C     xxClt#        OREQ      '153'
     C     PEEND#        ANDEQ     'WC991011TN'
     C     xxClt#        OREQ      '153'
     C     PEEND#        ANDEQ     'WC991011VA'
      *** End Add ***** 12/05/22 **************************************
    C*** Beg Add *** 07/16/24 *** 148WEW **********************************
     C     xxClt#        OREQ      '148'
     C     PEEND#        ANDEQ     'WC990660'
    C*** End Add *** 07/16/24 *** 148WEW **********************************
    C*** Beg Add *** 07/29/24 *** 148WEZ **********************************
     C     xxClt#        OREQ      '148'
     C     PEEND#        ANDEQ     'WC990003'
     C     xxClt#        OREQ      '148'
     C     PEEND#        ANDEQ     'WC990003NY'
    C*** End Add *** 07/29/24 *** 148WEZ **********************************
    C*** Beg Add *** 09/09/24 *** 148WFG **********************************
     C     xxClt#        OREQ      '148'
     C     PEEND#        ANDEQ     'WC990660VA'
    C*** End Add *** 09/09/24 *** 148WFG **********************************
    C*** Beg Add *** 07/23/25 *** *****************************************
     C     xxClt#        OREQ      '148'
     C     PEEND#        ANDEQ     'WC990660VAA'
    C*** End Add *** 07/23/25 *** *****************************************
    C*** Beg Add *** 07/30/24 *** 148WEX **********************************
     C     xxClt#        OREQ      '148'
     C     PEEND#        ANDEQ     'WC990635FL'
    C*** End Add *** 07/30/24 *** 148WEX **********************************
    C*** Beg Add *** 09/06/24 *** 148WFB **********************************
     C     xxClt#        OREQ      '148'
     C     PEEND#        ANDEQ     'WC990660TX'
     C     xxClt#        OREQ      '148'
     C     PEEND#        ANDEQ     'WC990660GA'
     C     xxClt#        OREQ      '148'
     C     PEEND#        ANDEQ     'WC990660AZ'
     C     xxClt#        OREQ      '148'
     C     PEEND#        ANDEQ     'WC990660MA'
     C     xxClt#        OREQ      '148'
     C     PEEND#        ANDEQ     'WC990660TN'
     C     xxClt#        OREQ      '148'
     C     PEEND#        ANDEQ     'WC990660KY'
     C     xxClt#        OREQ      '148'
     C     PEEND#        ANDEQ     'WC990660AR'
     C     xxClt#        OREQ      '148'
     C     PEEND#        ANDEQ     'WC990660ME'
     C     xxClt#        OREQ      '148'
     C     PEEND#        ANDEQ     'WC990660OR'
     C     xxClt#        OREQ      '148'
     C     PEEND#        ANDEQ     'WC990660RI'
     C     xxClt#        OREQ      '148'
     C     PEEND#        ANDEQ     'WC990660AK'
    C*** End Add *** 09/06/24 *** 148WFB **********************************
    C*** Beg Add ***** 11/26/24 *******************************************
     C     xxClt#        OREQ      '148'
     C     PEEND#        ANDEQ     'WC990660OK'
     C     xxClt#        OREQ      '148'
     C     PEEND#        ANDEQ     'WC990661OK'
    C*** End Add ***** 11/26/24 *******************************************
    C*** Beg Add *** 03/25/25 *** 148WFL **********************************
     C     xxClt#        OREQ      '148'
     C     PEEND#        ANDEQ     'WC990660IL'
    C*** End Add *** 03/25/25 *** 148WFL **********************************
     C                   EXSR      SR0403
     C     PEEND#        WHENEQ    W04302                                       CA PARTNER EXCLUSION
     C     PEEND#        OREQ      'WC040302B'                                  CA partner EXCLUSION
     C     PEEND#        OREQ      'WC040302C'                                  CA partner EXCLUSION
     C*** beg Chg *** 04/17/24 *****************************************
     C*****PEEND#        OREQ      'WC040302C'                                  CA partner EXCLUSION
     C     PEEND#        OREQ      'WC040302D'                                  CA partner EXCLUSION
     C*** End Chg *** 04/17/24 *****************************************
     C                   EXSR      S04302
     C     PEEND#        WHENEQ    'WC040303'                                   CA OFFICER EXCLUSION
     C     PEEND#        OREQ      'WC040303A'                                  CA OFFICER EXCLUSION
     C     PEEND#        OREQ      'WC040303B'                                  CA OFFICER EXCLUSION
     C     PEEND#        OREQ      'WC040303C'                                  CA OFFICER EXCLUSION
     C                   EXSR      S04303
     C     PEEND#        WHENEQ    W37310                                       PA EXCL OF OFFICERS
     C     PEEND#        OREQ      W37311
     C                   EXSR      S37310
     C     PEEND#        WHENEQ    W29601                                       NJ LARGE DEDUCTIBLE
     C*** Beg Del *** 07/23/24 *********************************************
     C*****   PEEND#        OREQ      'WC290604'                                NJ LARGE DEDUCTIBLE
     C*** End Del *** 07/23/24 *********************************************
     C*** Beg Add *** 07/06/22 *********************************************
     C     PEEND#        OREQ      'WC290605'
     C*** End Add *** 07/06/22 *********************************************
     C                   EXSR      S29601
     C     PEEND#        WHENEQ    'WC290303'                                   NJ EMPLR LEASING
     C                   EXSR      S29303
     C     PEEND#        WHENEQ    'WC000412'                                   CONTINGENT EXP MOD
     C                   EXSR      SR0412
     C     PEEND#        WHENEQ    'WC220304'                                   MN EMPLOYER LEASING
     C                   EXSR      S22304
     C     PEEND#        WHENEQ    'WC040305'                                   MN EMPLOYER LEASING
     C                   EXSR      SR04305
     C     PEEND#        WHENEQ    'WC040306'                                   MN EMPLOYER LEASING
     C                   EXSR      SR04306
     C     PEEND#        WHENEQ    'WC040318A'                                  MN EMPLOYER LEASING
     C     PEEND#        OREQ      'WC040318B'                                  CA OFFICER EXCLUSION
     C     PEEND#        OREQ      'WC040318C'                                  CA OFFICER EXCLUSION
     C                   EXSR      SR04318A
     C     PEEND#        WHENEQ    'WC200304B'                                  MA EMPLOYER LEASING
     C     PEEND#        OREQ      'WC200304C'                                  MA EMPLOYER LEASING
     C     PEEND#        OREQ      'WC200304D'                                  MA EMPLOYER LEASING
     C                   EXSR      SR20304
     C     PEEND#        WHENEQ    'WC480315'                                   WI EMPLOYEE LEASING
     C     PEEND#        OREQ      'WC480314'                                   WI EMPLOYER LEASING
     C*** begin add ***  10/14/22  ******************************************
     C     PEEND#        OREQ      'WC480314A'                                  WI EMPLOYER LEASING
     C     PEEND#        OREQ      'WC480315A'                                  WI EMPLOYER LEASING
     C*** end add *****  10/14/22  ******************************************
     C                   EXSR      SR48315
     C*** beg add *****  01/02/24  ******************************************
     C     PEEND#        WHENEQ    'WC480316'                                   WI EMPLOYER LEASING
     C                   EXSR      SR48316
     C*** end add *****  01/02/24  ******************************************
     C     PEEND#        WHENEQ    'WC040315B'                                  CA EMPLOYEE LEASING
     C                   EXSR      SR04315B
     C     PEEND#        WHENEQ    'WC040342B'                                  CA EMPLOYEE LEASING
     C                   EXSR      SR04342B
     C     PEEND#        WHENEQ    'WC040316B'                                  WI EMPLOYEE LEASING
     C                   EXSR      SR04316B
     C     PEEND#        WHENEQ    'WC000424'                                   WI EMPLOYEE LEASING
     C                   EXSR      SR00424
      *** Beg Del ***  01/15/25  *****************************************
     C*****PEEND#        WHENEQ    'WC370401'                                   WI EMPLOYEE LEASING
     C*****              EXSR      SR37401
      *** End Del ***  01/15/25  *****************************************
     C*** Begin Add ***  10/25/22  *** 103957 *******************************
     C     PEend#        WHENEQ    'WC480322'                                   WI EMPLOYEE LEASING
     C                   EXSR      SR48322
     C*** End   Add ***  10/25/22  *** 103957 *******************************
     C*** Begin Add ***  09/21/23  *** WC2923 *******************************
     C     PEend#        Wheneq    'WC480323'                                   WI EMPLOYEE LEASING
     C                   Exsr      SR48323
     C*** End   Add ***  09/21/23  *** WC2923 *******************************
     C*** Begin Add ***  07/25/24  *** WC2978 *******************************
     C     PEend#        Wheneq    'WC040602B'                                  CA Small Deductible
     C                   Exsr      SR04603
     C     PEend#        Wheneq    'WC040366'                                   CA Officer & Direct
     C                   Exsr      S04303
     C     PEend#        Wheneq    'WC040314B'                                  Agreement Limiting
     C                   Exsr      SR04315B
     C*** End   Add ***  07/25/24  *** WC2978 *******************************
     C                   OTHER
     C                   ENDSL
     C                   Endif                                                  If xxEndorse = *on
     C*
     C     PEKY          READE     PPE01                                  60
     C                   ENDDO
     C*
     C*
     C                   Endif                                                  If polSkip <> 'Y'
     C*
     C                   MOVE      '4'           SHSTAT                              PROCESSED
     C*
     C* UPDATE THE STATE LIST ON THE WMSHP RECORD
     C*
     C     *IN59         IFEQ      *ON
     C*
     C                   EXSR      SRUPDS
     C*
     C                   ENDIF                                                  *IN59 IFEQ
     C* Reset policy record back to '0' for future submissions.
     C                   Eval      SHReq# = 0
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SHSBDT
     C                   Z-ADD     DSTIME        SHSBTM
     C                   UPDATE    PSH01
     C*** begin chg ***  12/18/24  *********************************************
     C******             If        polSkip <> 'Y'
     C******             EXSR      SRSHD
     C******             Endif                                                  If polSkip <> 'Y'
     C                   EXSR      SRSHD
     C*** end chg *****  12/18/24  *********************************************
     C*
     C                   ELSE
     C                   MOVE      'E'           SHSTAT
     C*
     C* UPDATE THE STATE LIST ON THE WMSHP RECORD
     C*
     C     *IN59         IFEQ      *ON
     C*
     C                   EXSR      SRUPDS
     C*
     C                   ENDIF                                                  *IN59 IFEQ
     C                   UPDATE    PSH01
     C*
     C*** END FOR HEADER RECORD READ ***
     C*
     C                   ENDIF
     C*
          //* Clean up the temporary file for next policy run.
          Close wmmul7;

          Monitor;
             clear strCommand;
             strCommand = 'DLTOVR FILE(WMMUP)';
             Callp QCMDEXC(strCommand:%len(strCommand));
          On-error;
          Endmon;

          Monitor;
             clear strCommand;
             strCommand = 'DLTOVR FILE(WMMUL7)';
             Callp QCMDEXC(strCommand:%len(strCommand));
          On-error;
          Endmon;

          Monitor;
             clear strCommand;
             strCommand = 'DLTF FILE(QTEMP/WMMUL7)';
             Callp QCMDEXC(strCommand:%len(strCommand));
          On-error;
          Endmon;

          Monitor;
             clear strCommand;
             strCommand = 'DLTF FILE(QTEMP/WMMUP)';
             Callp QCMDEXC(strCommand:%len(strCommand));
          On-error;
          Endmon;
     C                   ENDIF                                                  *IN64 IFEQ
     C*
     C                   ENDIF                                                  *IN69 IFEQ *ON
     C                   ENDIF                                                  *IN60 IFEQ *ON
     C                   Endif                                                  xxProcess = *on
       // Print the submission detail
       PrintDetail(SHfnd:SHemp#:SHfyr:ELpfDt:SHsbDt:SHst01:SHtype);
     C     NEXTSH        TAG                                                                ********
     C*
     C     SHKEY         READE     PSH01                                  50
     C                   ENDDO
     C**********
      * FORMAT AND WRITE WCPOLS HERE
     C**********
      * FORMAT AND WRITE WCPOLS HERE
     C**********
      *
      * PROCESS CANCELLATION & REINSTATEMENT TRANSACTIONS
      *
     C     SHKY1         CHAIN     PSH04                              50
      *
     C     *IN50         DOWEQ     *OFF
     C                   Move      *on           xxProcess         1
     C
     C                   If        xxClt# = '045' and QQOldCar = 'Y'
     C                             and SHPFDT >= 1160101
     C                   Move      *off          xxProcess
     C                   Endif
     C
     C                   If        xxClt# = '045' and QQOldCar <> 'Y'
     C                             and SHPFDT < 1160101
     C                   Move      *off          xxProcess
     C                   Endif
     C*** begin add ***  04/21/25  *******************************************
     C* For NCCI Submission files only prevent CX if cancel/rewrited.
     C                   If        QQSt = '  '
     C*
     C                   Z-ADD     DSDate        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SHDt
     C                   Z-ADD     DSTime        SHTime
          If *in59 = *On;
             If QQst <> ' ';
                xzST = QQst;
             Else;
                xzST = 'XX';
             Endif;
          Else;
                xzST = '  ';
          Endif;
          //* If policy is cancelled and re-written and reported its new policy
          //* transaction today then prevent cancellation transaction from sending.
          Chain(n) (SHCo#:SHFnd:SHEmp#:SHDiv) pex02;
          If %found and EXCar = 'Y';
             Chain(n) (SHCo#:EXFnd:EXEmp#:SHFyr:'P':xzST:SHDt) ps101;
             If %found and S1Code = 'W';
                xxProcess = *off;
                SHStat = '4';
                If *in59 = *on;
                   Exsr SRUpds;
                Endif;
                SHPgm  = Q1PGM;
                SHUser = Q1USER;
                Update psh04;
             Endif;
          Endif;
     C                   Endif                                                  If QQSt = '  '
     C*** end add *****  04/21/25  *******************************************
     C
     C                   If        xxProcess = *on
     C     *IN59         IFEQ      *ON
     C     QQST          IFNE      *BLANKS
     C                   MOVE      QQST          XZST              2
     C                   ELSE
     C                   MOVE      'XX'          XZST
     C                   ENDIF
     C                   ENDIF                                                  *IN59 IFEQ *ON
      *
     C                   Z-ADD     1             I
     C     XZST          LOOKUP    SST(I)                                 60
     C     *IN60         IFEQ      *ON
     C     *IN59         ANDEQ     *OFF
     C     *IN60         OREQ      *ON
     C     *IN59         ANDEQ     *ON
     C     SSS(I)        ANDEQ     '3'
     C     QQER          IFNE      *BLANK
     C*
     C*** Beg Chg *** 03/04/24 *****************************************
     C***     EMKEY         CHAIN     PEM01                              60
     C***  N60QQER          CABNE     EMEXRT        ENDEND
     C     elky3         Chain     pel01                              60
     C  N60QQER          CabNe     ELexRt        nextSh
     C*** End Chg *** 03/04/24 *****************************************
     C                   ENDIF
      *
      * CHECK TO MAKE SURE A POLICY HAS BEEN PROCESSED FOR THIS RCD.
      *
      *
     C                   EXSR      CHKPOL
      *
     C     *IN69         IFEQ      *OFF
     C                   MOVE      *OFF          *IN64
     C*
     C     QQST          IFEQ      'NJ'
     C     XXCLT#        ANDEQ     '114'
     C*
     C  N64
     CAN 64              EXSR      ANNV
     C*
     C                   ENDIF                                                  QQST IFEQ
     C*
     C     *IN64         IFEQ      *OFF
      *********
      **IF*EMPLOYER HAS CHANGED, LOAD TRANSACTION DATE ARRAY
      *********
      *
      *
     C     EMKEY         CHAIN     PEM01                              60
     C  N60              MOVEL(P)  EMNAOB        P08NAE
     C   60              MOVEL(P)  *BLANKS       P08NAE
     C                   If        P08NAE = ' '
     C                   MOVEL(P)  'BUSI'        XXTBID
     C                   MOVEL(P)  EMTPOB        XXKEY
     C*
     C     GTKY          CHAIN     PGT01                              60
     C*
     C                   If        *in60 = *OFF
     C                   Movel(p)  GTDESC        P08NAE
     C                   Else
     C                   Movel(p)  'Business'    P08NAE
     C                   Endif
     C*
     C                   Endif
      *
      *
     C     SHTYPE        IFEQ      'C'
     C                   EXSR      SRCANC
     C                   ENDIF                                                  SHTYPE IFEQ 'C'
      *
     C     SHTYPE        IFEQ      'R'
     C                   EXSR      SRREIN
     C                   ENDIF                                                  SHTYPE IFEQ 'C'
      *
      *
     C                   ENDIF                                                  *IN64 IFEQ
      *
     C                   ENDIF                                                  *IN69 IFEQ *OFF
     C                   ENDIF                                                  *IN60 IFEQ *ON
     C                   Endif                                                  xxProcess = *on
      *
     C     SHKY1         READE     PSH04                                  50
      *
     C                   ENDDO                                                  *IN50 DOWEQ *OFF
     C*
     C*** GET ALL ENDORSMENT SUBMISSION REQUESTS
     C*
     C     FNPEOS        IFEQ      *BLANK
     C                   MOVE      'E'           XXTYPE            1
     C                   MOVE      '3'           XXSTAT            1
     C*
     C     SHKEY         CHAIN     PSH18                              50
     C*
     C     *IN50         DOWEQ     '0'
     C                   Move      *on           xxProcess         1
     C
     C                   If        xxClt# = '045' and QQOldCar = 'Y'
     C                             and SHPFDT >= 1160101
     C                   Move      *off          xxProcess
     C                   Endif
     C
     C                   If        xxClt# = '045' and QQOldCar <> 'Y'
     C                             and SHPFDT < 1160101
     C                   Move      *off          xxProcess
     C                   Endif
     C
     C                   If        xxProcess = *on
     C     *IN59         IFEQ      *ON
     C     QQST          IFNE      *BLANKS
     C                   MOVE      QQST          XZST              2
     C                   ELSE
     C                   MOVE      'XX'          XZST
     C                   ENDIF
     C                   ENDIF                                                  *IN59 IFEQ *ON
      *
     C                   Z-ADD     1             I
     C     XZST          LOOKUP    SST(I)                                 60
     C     *IN60         IFEQ      *ON
     C     *IN59         ANDEQ     *OFF
     C     *IN60         OREQ      *ON
     C     *IN59         ANDEQ     *ON
     C     SSS(I)        ANDEQ     '3'
     C******************************************************************
     C* * * CHECK TO MAKE SURE A POLICY HAS BEEN PROCESSED FOR THIS RCD.
     C*
      *
     C                   EXSR      CHKPOL
     C*
     C     *IN69         CABEQ     '1'           ENDEND
     C                   MOVE      *OFF          *IN64
     C*
     C     QQST          IFEQ      'NJ'
     C     XXCLT#        ANDEQ     '114'
     C*
     C  N64
     CAN 64              EXSR      ANNV
     C*
     C                   ENDIF                                                  QQST IFEQ
     C*
     C     *IN64         IFEQ      *OFF
     C******************************************************************
     C*
     C****** TRANSLATE TODAY TO A JULIAN DATE ******
     C****
     C                   Z-ADD     SHTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         #@FDAT            8 0
     C                   MOVE      #@FDAT        QQFDAT            8
     C                   MOVE      *BLANKS       QQJULD            5
     C                   CALL      'SRJULC'
     C                   PARM                    QQFDAT
     C                   PARM                    QQJULD
     C                   PARM      ' '           QQRTNC            1
     C*
     C     QQRTNC        IFNE      '1'
     C                   MOVE      QQJULD        XXDTJ             5 0
     C                   ELSE
     C                   MOVE      *ZEROS        XXDTJ
     C                   END
     C*
     C***
     C     NKEY          CHAIN     PNE01                              51
     C*
     C     *IN51         DOWEQ     '0'
     C***
     C**** DECIDE WHICH TYPE OF ENDORSMENT TO CREATE.....
     C**** THEN FILL FIELDS
     C*
     C     NETYPE        CASEQ     '10'          RCD10
     C     NETYPE        CASEQ     '88'          RCD88
     C     NETYPE        CASEQ     '89'          RCD89
     C     NETYPE        CASEQ     '87'          RCD87
     C     NETYPE        CASEQ     '42'          RCD42
     C                   ENDCS
     C*
     C     NKEY          READE     PNE01                                  51
     C                   ENDDO                                                       NE DOW 51
     C*
     C     SHTRDT        IFLT      XXTDE
     C                   Z-ADD     SHTRDT        XXTDE
     C                   ENDIF
     C     SHTRDT        IFGT      XXTDL
     C                   Z-ADD     SHTRDT        XXTDL
     C                   ENDIF
     C*
     C                   MOVE      '4'           SHSTAT                              PROCESSED
     C*
     C* UPDATE THE STATE LIST ON THE WMSHP RECORD
     C*
     C     *IN59         IFEQ      *ON
     C*
     C                   EXSR      SRUPDS
     C*
     C                   ENDIF                                                  *IN59 IFEQ
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SHSBDT
     C                   Z-ADD     DSTIME        SHSBTM
     C                   UPDATE    PSH18
     C                   EXSR      SRSHD
     C*
     C                   MOVE      #SHPOL        #18POL
     C*
     C                   ENDIF                                                  *IN64 IFEQ
     C*
     C     ENDEND        TAG
     C                   ENDIF                                                  *IN60 IFEQ *ON
     C                   Endif                                                  xxProcess = *on
     C*
     C     #SHPOL        DOUNE     #18POL
     C     *IN50         OREQ      *ON
     C     SHKEY         READE     PSH18                                  50
     C                   ENDDO
     C*
     C                   ENDDO
     C*
     C                   ENDIF                                                  FNPEOS = *BLANK
     C                   MOVE      *ON           *IN58
      *
     C     SVFND         IFEQ      *ZERO
      *
     C     PGKY          READE     XPG01                                  60
     C     *IN60         DOWEQ     *OFF
     C     PGFLAG        ANDEQ     'E'
     C     PGKY          READE     XPG01                                  60
     C                   ENDDO
      *
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     PGGRP         XXFND
     C                   MOVE      *OFF          *IN58
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   ENDIF                                                  SVFND IFEQ
      *
     C                   ENDDO                                                  *IN58 DOWEQ
     C*
     C*** *** *** *** WRITE A RCD TYPE 99 TO WCPOLS TOO! *******
     C*
     C                   CLEAR                   R99
     C*
     C                   MOVE      '99'          R99RTP
     C                   Z-ADD     ##TOTR        R99T#R
     C                   Z-ADD     ##TOTH        R99THR
     C*
     C     XXTDE         ADD       19000000      R99TDE
     C     XXTDL         ADD       19000000      R99TDL
     C*
     C                   MOVEL     R99           RDATA
     C                   WRITE     RPOLS
      *
      *
     c*  Print Totals
     c                   If        ##totR = 0
     c                   Except    norec
     c                   Else
     c                   Except    tot
     c                   Endif
      *
     C     SVFND         IFEQ      0
     C                   Z-ADD     0             XXFND
     C                   ENDIF
       //Print the routing page
       Chain (Q1pgm:'0') POR00;
       If %Found(smorl);
          Except Route;
       Endif;
     C                   MOVE      '1'           *INLR
     C*
      *
      * SUBROUTINE TO WRITE CANCELLATION RECORD TO WCPOLS FILE
      *
     C     SRCANC        BEGSR
      *************
      **USE*NEXT*TRANSACTION DATE FROM DATE ARRAY, TRANSLATED INTO JULIAN
      *************
     C                   Z-ADD     SHTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         #@FDAT            8 0
      *
     C                   MOVE      #@FDAT        QQFDAT            8
     C                   MOVE      *BLANKS       QQJULD            5
      *
     C                   CALL      'SRJULC'
     C                   PARM                    QQFDAT
     C                   PARM                    QQJULD
     C                   PARM      ' '           QQRTNC            1
      *
     C     QQRTNC        IFNE      '1'
     C                   MOVE      QQJULD        XXDTJ             5 0
     C                   ELSE
     C                   MOVE      *ZEROS        XXDTJ
     C                   ENDIF                                                  QQRTNC IFNE '1'
      *
     C     NKEY          CHAIN     PCR01                              60
     C                   If        xxClt# = '045' and QQOldCar = 'Y'
     C                   Z-add     33057         ##CARR
     C                   Endif
      *
      /Free
          If *in60 = *on and SHst = QQst and flgGenCX = 'Y';

             If qqSt = *blanks And xxClt# = '132' And CRfnd = 5;
                ##Carr = 12985;
             Endif;

             ##pol# = #shpol;
             Exsr SRPOL#;

             ##pfdt =  %Dec(%Subst(%Char(SHpfdt):2:6):6:0);
             ##trdt = xxDtj;
             ##trcd = 05;

             p08Lnk = ##lnk;
             If QQst <> *Blanks;
                Chain (QQst) pst01;
                If %Found;
                   P08st = STnst;
                Endif;
             Endif;
             P08rtp = '08';
             P08tid = 1;
             P08cnc = *zeros;
             If flatDel = 'Y';
                P08cnc = 1;
             Else;
                P08cnc = 2;
             Endif;
             P08rfc = 2;
             P08rec = *Zeros;
             P08noe = %TrimR(EMname) + ' ' + %TrimR(EMnam2);
             P08aoe = %TrimR(EMmad1) + ', ' + %TrimR(EMmcty) + ', ' +
                      %TrimR(EMmst) + ' ' + %subst(%editc(EMmzip:'X'):1:5);
             If %subst(%editc(EMmzip:'X'):6:4) <> '0000';
                P08aoe = %trimR(P08aoe) + '-' + %subst(%editc(EMmzip:'X'):6:4);
             Endif;
             P08dcm = %Dec(%Subst(%Char(SHdt):2:6):6:0);
             P08TS# = 1;
             P08recd = *Blanks;
             P08ccd = *Zeros;
             P08efd = P08dcm;

             Clear RDATA;
             Rdata = P08;

             Write RPOLS;
             Delete PSH04;
             ##totr = ##totr + 1;
             ##tlcx = ##tlcx + 1;

             leaveSr;

          Endif;
      /End-Free

     C     *IN60         IFEQ      *OFF
      *
      * MOVE FIELDS
      *
     C                   MOVE      CRCARR        ##CARR
     C                   If        xxClt# = '045' and QQOldCar = 'Y'
     C                   Z-add     33057         ##CARR
     C                   Endif
     C     QQST          IFEQ      *BLANKS
     C     XXCLT#        ANDEQ     '132'
     C     CRFND         ANDEQ     5
     C                   Z-ADD     12985         ##CARR
     C                   ENDIF                                                  QQST IFEQ
     C                   MOVEL     #SHPOL        ##POL#
      *
     C                   EXSR      SRPOL#
      *
     C                   MOVE      CRPFDT        ##PFDT
     C                   MOVE      XXDTJ         ##TRDT
      *
      * TRANS CODE HERE!
      *
     C                   MOVE      05            ##TRCD                          CANCELATION
      *
     C                   MOVE      ##LNK         P08LNK
     C                   Z-add     0             p08ccd
      /free
       If GetRatingState(SHco#:SHfnd:SHemp#:SHdiv:SHfYr) = 'CA' and not
        MultiState(SHco#:SHfnd:SHemp#:SHfYr);
          P08ST = '04';
       Else;
          If P0shst = 'WI';
             P08st = '48';
          Else;
             P08st = '99';
\          Endif;
       Endif;
      /end-free
     C     XXPEEP        IFEQ      'Y'
     C     XX#ST         ANDNE     *ZERO
     C                   MOVEL     XX#ST         P08ST
     C                   ENDIF
     C                   MOVE      '08'          P08RTP
      *
     c                   move      *blank        xxidov            1
     c     cxky          setgt     pcx01
     c     cxky1         readpe    pcx01                                  60
     c                   if        *in60 = *off
     c*
     c                   movel(p)  cxrsn         xxkey1           10
     C     elky3         chain     pel01                              60
     C  n60rsky          chain     prs01                              60
     C     *in60         ifeq      *off
     C                   move      rsjrst        xxst
     C                   endif                                                  *IN60 = *OFF

     c     crky          setgt     pcr02
     c     crky1         readpe    pcr02                                  60
     c*
     C     *in60         doweq     *off
     C     crstat        andeq     'I'
     C     CRKY1         readpe    pcr02                                  60
     C                   enddo
     C*
     c                   if        *in60 = *off
     c                   movel     ctidov        xxidov
     c                   endif                                                  *IN60 IFEQ
     c                   endif                                                  *IN60 IFEQ

     c                   if        xxidov <> ' '
     c                   move      xxidov        p08tid
     c                   else
     c                   if        crcrsn = '00'
     C                   move      3             p08tid
     c                   else
     C                   move      1             p08tid
     C                   endif                                                  crcrsn = '00'
     C                   endif                                                  cridov <> ' ''
      *
     C     CRFLAT        IFNE      ' '
     C                   MOVE      1             P08CNC
     C                   ENDIF                                                  CRFLAT IFNE ' '
      *
     C     CRPROR        IFNE      ' '
     C                   MOVE      2             P08CNC
     C                   ENDIF                                                  CRPROR IFNE ' '
      *
     C     CRSHRT        IFNE      ' '
     C                   MOVE      3             P08CNC
     C                   ENDIF                                                  CRSHRT IFNE ' '
      *
     C     CRNRNW        IFNE      ' '
     C                   MOVE      0             P08CNC
     C                   ENDIF                                                  CRNRNW IFNE ' '
      *
     C                   MOVE      CRCRSN        P08RFC
     C                   MOVE      *ZERO         P08REC
     C                   MOVEL     CRENAM        P08NOE
     C     CRNAM2        IFNE      *BLANKS
     C                   CLEAR                   #WRK10          100
     C*** Beg Chg *** 04/02/24 *** PHW115270 ***************************
     C***                EVAL      #WRK10 = CRNAM1 + CRNAM2
       #WRK10 = %trim(CRNAM1) + ' ' + %trim(CRNAM2);
     C*** End Chg *** 04/02/24 *** PHW115270 ***************************
     C                   MOVEL(P)  #WRK10        P08NOE
     C                   ENDIF
     C                   MOVEL     CREADR        P08AOE
     C                   MOVE      CRMLDT        P08DCM
     C                   MOVE      01            P08TS#
     C                   MOVE      CREFDT        P08EFD
     C                   Z-ADD     CRSEQ#        P08TS#
     C                   MOVE      '00'          P08RECD
      *
      * FORMAT AND WRITE WCPOLS HERE
      *
     C                   CLEAR                   RDATA
     C                   MOVEL     P08           RDATA
      *
     C                   WRITE     RPOLS
      *
     C                   ADD       1             ##TOTR           10 0
     C                   ADD       1             ##TLCX
     C*** begin del ***  03/20/23  ******************************************
     C*****              If        QQSt = 'NJ'
     C*****              Add       1             ##TOTH            8 0
     C*****              Endif
     C*** end del *****  03/20/23  ******************************************
     C     SHTRDT        IFLT      XXTDE
     C                   Z-ADD     SHTRDT        XXTDE
     C                   ENDIF
     C     SHTRDT        IFGT      XXTDL
     C                   Z-ADD     SHTRDT        XXTDL
     C                   ENDIF
      *
     C                   MOVE      '4'           SHSTAT                              PROCESSED
     C*
     C* UPDATE THE STATE LIST ON THE WMSHP RECORD
     C*
     C     *IN59         IFEQ      *ON
     C*
     C                   EXSR      SRUPDS
     C*
     C                   ENDIF                                                  *IN59 IFEQ
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SHSBDT
     C                   Z-ADD     DSTIME        SHSBTM
      *
     C                   UPDATE    PSH04
     C                   EXSR      SRSHD
      *
     C                   ELSE
      *
     C                   MOVE      'E'           SHSTAT
     C*
     C* UPDATE THE STATE LIST ON THE WMSHP RECORD
     C*
     C     *IN59         IFEQ      *ON
     C*
     C                   EXSR      SRUPDS
     C*
     C                   ENDIF                                                  *IN59 IFEQ
      *
     C                   UPDATE    PSH04
      *
     C                   ENDIF                                                  *IN60 IFEQ *OFF
      *
     C                   ENDSR                                                  SRCANC
     C     SRPOL#        BEGSR
      *
      * CHECK FOR A CARRIER/STATE REPORTING RECORD
      *
     C     RPKY          CHAIN     PRP01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     RPCAP#        IFNE      *BLANKS
      *
     C                   SELECT
     C     QQST          WHENEQ    'CA'
     C                   MOVE      *ON           STRIP
     C     XXCLT#        WHENEQ    '129'
     C     XXFND         ANDEQ     1
     C                   MOVE      *ON           STRIP
     C     XXCLT#        WHENNE    '129'
     C                   MOVE      *ON           STRIP
     C                   OTHER
     C                   MOVE      *OFF          STRIP             1
     C                   ENDSL
     C*
     C     STRIP         IFEQ      *ON
     C*
     C                   CLEAR                   CAP
     C                   CLEAR                   CAP2
     C                   MOVEA     RPCAP#        CAP
     C                   Z-ADD     1             T                 2 0
     C                   Z-ADD     1             U                 2 0
     C*
     C     T             DOWLE     25
     C*
     C     CAP(T)        IFNE      '-'
     C     u             andle     18
     C                   MOVE      CAP(T)        CAP2(U)
     C                   ADD       1             U
     C                   ENDIF
     C*
     C                   ADD       1             T
     C                   ENDDO
     C*
     C                   MOVEA     CAP2          ##POL#
     C*
     C                   ELSE
     C                   MOVEL(P)  RPCAP#        ##POL#
     C                   ENDIF
      *
     C                   ENDIF                                                  RPCAP# IFNE *BLANKS
      *
     C                   ENDIF                                                  *IN60 IFEQ *OFF
      *
     C                   ENDSR                                                  SRPOL#
      *
      * SUBROUTINE TO WRITE REINSTATEMENT RECORD TO WCPOLS FILE
      *
     C     SRREIN        BEGSR
      **USE*NEXT*TRANSACTION DATE FROM DATE ARRAY, TRANSLATED INTO JULIAN
      *************
      *
     C                   Z-ADD     SHTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         #@FDAT            8 0
      *
     C                   MOVE      #@FDAT        QQFDAT            8
     C                   MOVE      *BLANKS       QQJULD            5
      *
     C                   CALL      'SRJULC'
     C                   PARM                    QQFDAT
     C                   PARM                    QQJULD
     C                   PARM      ' '           QQRTNC            1
      *
     C     QQRTNC        IFNE      '1'
     C                   MOVE      QQJULD        XXDTJ             5 0
     C                   ELSE
     C                   MOVE      *ZEROS        XXDTJ
     C                   ENDIF                                                  QQRTNC IFNE '1'
      *
     C     NKEY          CHAIN     PCR01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
      * MOVE FIELDS
      *
     C                   MOVE      CREFDT        P08CCD
     C     XXPEEP        IFEQ      'Y'
     C                   Z-ADD     CRRCNL        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM3
     C                   MOVEL     SRDTO         P08CCD
     C                   ENDIF
      *
     C                   MOVE      CRCARR        ##CARR
     C                   If        xxClt# = '045' and QQOldCar = 'Y'
     C                   Z-add     33057         ##CARR
     C                   Endif
     C     QQST          IFEQ      *BLANKS
     C     XXCLT#        ANDEQ     '132'
     C     CRFND         ANDEQ     5
     C                   Z-ADD     12985         ##CARR
     C                   ENDIF                                                  QQST IFEQ
     C                   MOVEL     #2POL#        ##POL#
      *
     C                   EXSR      SRPOL#
      *
     C                   MOVE      CRPFDT        ##PFDT
     C                   MOVE      XXDTJ         ##TRDT
      * TRANS CODE HERE!
     C                   MOVE      05            ##TRCD                          CANCEL/REIN
      *
     C                   MOVE      ##LNK         P08LNK
      /free
       If GetRatingState(SHco#:SHfnd:SHemp#:SHdiv:SHfYr) = 'CA' and not
        MultiState(SHco#:SHfnd:SHemp#:SHfYr);
          P08ST = '04';
       Else;
          P08ST = '99';
       Endif;
      /end-free
     C     XXPEEP        IFEQ      'Y'
     C     XX#ST         ANDNE     *ZERO
     C                   MOVEL     XX#ST         P08ST
     C                   ENDIF
     C                   MOVE      '08'          P08RTP
      *
     C                   MOVE      2             P08TID
      *
     C                   MOVE      0             P08CNC
      *
     C                   MOVE      00            P08RFC
     C                   MOVE      CRTORE        P08REC
     C                   MOVEL     CRENAM        P08NOE
     C     CRNAM2        IFNE      *BLANKS
     C                   CLEAR                   #WRK10          100
     C*** Beg Chg *** 04/02/24 *** PHW115270 ***************************
     C***                EVAL      #WRK10 = CRNAM1 + CRNAM2
       #WRK10 = %trim(CRNAM1) + ' ' + %trim(CRNAM2);
     C*** End Chg *** 04/02/24 *** PHW115270 ***************************
     C                   MOVEL(P)  #WRK10        P08NOE
     C                   ENDIF
     C                   MOVEL     CREADR        P08AOE
     C                   MOVE      CRMLDT        P08DCM
     C                   MOVE      01            P08TS#
     C                   MOVE      CREFDT        P08EFD
     C                   Z-ADD     CRSEQ#        P08TS#
      /Free
       P08reCd = CRreCd ; //Load Reinstatement Reason Code
      /End-Free
      *
      * FORMAT AND WRITE WCPOLS HERE
      *
     C                   CLEAR                   RDATA
     C                   MOVEL     P08           RDATA
      *
     C                   WRITE     RPOLS
      *
     C                   ADD       1             ##TOTR           10 0
     C                   ADD       1             ##TLRE
     C*** begin del ***  03/20/23  ******************************************
     C*****              If        QQSt = 'NJ'
     C*****              Add       1             ##TOTH            8 0
     C*****              Endif
     C*** end del *****  03/20/23  ******************************************
     C     SHTRDT        IFLT      XXTDE
     C                   Z-ADD     SHTRDT        XXTDE
     C                   ENDIF
     C     SHTRDT        IFGT      XXTDL
     C                   Z-ADD     SHTRDT        XXTDL
     C                   ENDIF
     C                   MOVE      '4'           SHSTAT                              PROCESSED
     C*
     C* UPDATE THE STATE LIST ON THE WMSHP RECORD
     C*
     C     *IN59         IFEQ      *ON
     C*
     C                   EXSR      SRUPDS
     C*
     C                   ENDIF                                                  *IN59 IFEQ
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SHSBDT
     C                   Z-ADD     DSTIME        SHSBTM
      *
     C                   UPDATE    PSH04
     C                   EXSR      SRSHD
      *
     C                   ELSE
      *
     C                   MOVE      'E'           SHSTAT
     C*
     C* UPDATE THE STATE LIST ON THE WMSHP RECORD
     C*
     C     *IN59         IFEQ      *ON
     C*
     C                   EXSR      SRUPDS
     C*
     C                   ENDIF                                                  *IN59 IFEQ
      *
     C                   UPDATE    PSH04
      *
     C                   ENDIF                                                  *IN60 IFEQ *OFF
      *
     C                   ENDSR                                                  SRREIN
      *
      * SUBROUTINE TO LOAD ORIGINAL CARRIER #, ORIGINAL POLICY #
      * AND ORIGINAL EFFECTIVE DATE IN THE CASE OF A REWRITE
      *
     C     SRRWRT        BEGSR
      *
      * FIND STATE REPORTING DIVISION
      *
      *
      *
      * IF NO STATE REPORTING DIVISION FOUND, DEFAULT TO FIRST DIVISION
      *
     C                   EXSR      SRSTRP
      *
      * CHAIN TO EMPLOYER CROSS-REFERENCE FILE
      *
     C     *IN70         IFEQ      *OFF
     C     EXKY          CHAIN     PEX01                              71
      *
      * RETRIEVE ORIGINAL CARRIER NUMBER
      *
     C     *IN71         IFEQ      *OFF
      *
     C     F2KY          CHAIN     PF201                              72
      *
     C     *IN72         IFEQ      *OFF
     C                   Z-ADD     F2CAR#        P01OCC
     C                   ENDIF                                                  *IN72 IFEQ *OFF
     C                   If        xxClt# = '045' and QQOldCar = 'Y'
     C                   Z-add     33057         P01OCC
     C                   Endif
     C     QQST          IFEQ      *BLANKS
     C     XXCLT#        ANDEQ     '132'
     C     EXOFND        ANDEQ     5
     C                   Z-ADD     12985         P01OCC
     C                   ENDIF                                                  QQST IFEQ
      *
      * FIND LATEST POLICY YEAR FOR THE OLD EMPLOYER NUMBER
      *
     C     ELKY          SETGT     PEL01
     C     ELKY          READPE    PEL01                                  72
      *
     C     *IN72         IFEQ      *OFF
     C                   Z-ADD     ELPFDT        P01OED
     C                   MOVEL     #OPOL         P01OP#
      *
      * CHECK FOR A CARRIER POLICY NUMBER
      *
     C     RPKY0         CHAIN     PRP01                              73
      *
     C     *IN73         IFEQ      *OFF
      *
     C     RPCAP#        IFNE      *BLANKS
     C                   SELECT
     C     QQST          WHENEQ    'CA'
     C                   MOVE      *ON           STRIP
     C     XXCLT#        WHENEQ    '129'
     C     XXFND         ANDEQ     1
     C                   MOVE      *ON           STRIP
     C     XXCLT#        WHENNE    '129'
     C                   MOVE      *ON           STRIP
     C                   OTHER
     C                   MOVE      *OFF          STRIP             1
     C                   ENDSL
     C*
     C     STRIP         IFEQ      *ON
     C*
     C                   CLEAR                   CAP
     C                   CLEAR                   CAP2
     C                   MOVEA     RPCAP#        CAP
     C                   Z-ADD     1             T                 2 0
     C                   Z-ADD     1             U                 2 0
     C*
     C     T             DOWLE     25
     C*
     C     CAP(T)        IFNE      '-'
     C     u             andle     18
     C                   MOVE      CAP(T)        CAP2(U)
     C                   ADD       1             U
     C                   ENDIF
     C*
     C                   ADD       1             T
     C                   ENDDO
     C*
     C                   MOVEA(P)  CAP2          P01OP#
     C*
     C                   ELSE
     C                   MOVEL(P)  RPCAP#        P01OP#
     C                   ENDIF
     C                   ENDIF                                                  RPCAP# IFNE *BLANKS
      *
     C                   ENDIF                                                  *IN73 IFEQ *OFF
      *
     C                   ENDIF                                                  *IN72 IFEQ *OFF
      *
     C                   ENDIF                                                  *IN71 IFEQ *OFF
      *
     C                   ENDIF                                                  *IN70 IFEQ *OFF
      *
     C                   ENDSR                                                  SRRWRT
      **SUBROUTINE TO LOAD ARRAY OF TRANSACTION DATES FOR A GIVEN EMPLOYER
      **********
      **GET*LATEST PREVIOUSLY REPORTED TRANSACTION DATE
      **********
      **LOAD*PENDING TRANSACTION DATES TO ARRAY
      **********
      **START*AT LAST DATE AND VERIFY NO DUPLICATES BY SUBTRACTING 1 DAY
      **FROM*THE EARLIEST DATE ENCOUNTERED SO FAR IF THERE ARE DUPLICATES
      **********
      **IF*EARLIEST CALCULATED TRANSACTION DATE IS PRIOR TO THE LATEST
      **PREVIOUSLY REPORTED TRANSACTION DATE, ADJUST ALL DATES FORWARD
      **BY*THE*DIFFERENCE PLUS 1
      **********
      *
      * SUBROUTINE TO UPDATE STATE TABLE
      *
     C     SRUPDS        BEGSR
      *
     C                   Z-ADD     1             I
     C     QQST          IFNE      *BLANKS
     C                   MOVE      QQST          XZST              2
     C                   ELSE
     C                   MOVE      'XX'          XZST
     C                   ENDIF
      *
     C     XZST          LOOKUP    SST(I)                                 60
     C     *IN60         IFEQ      *ON
     C                   MOVE      SHSTAT        SSS(I)
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SSD(I)
     C                   Z-ADD     DSTIME        STI(I)
     C                   ENDIF
      *
     C     1             DO        15            I
      *
     C     SST(I)        IFNE      *BLANKS
     C     SSS(I)        ANDEQ     '3'
     C                   MOVE      '3'           SHSTAT
     C                   ENDIF
      *
     C                   ENDDO
      *
     C                   ENDSR                                                  SRUPDS
      *==================================================================*
     C     RCD10         BEGSR
      *==================================================================*
     C*
     C*
     C* * MOVE FIELDS * * *
     C                   MOVE      NECAR1        ##CARR
     C                   If        xxClt# = '045' and QQOldCar = 'Y'
     C                   Z-add     33057         ##CARR
     C                   Endif
     C     QQST          IFEQ      *BLANKS
     C     XXCLT#        ANDEQ     '132'
     C     NEFND         ANDEQ     5
     C                   Z-ADD     12985         ##CARR
     C                   ENDIF                                                  QQST IFEQ
     C                   MOVEL     #SHPOL        ##POL#                         ?????
     C                   EXSR      SRPOL#
     C                   MOVE      NEPFDT        ##PFDT
     C                   MOVE      XXDTJ         ##TRDT
     C*** TRANS CODE HERE!
     C                   MOVE      03            ##TRCD                          ENDORSMENT
     C*
     C                   MOVE      ##LNK         P10LNK
     C                   MOVE      '10'          P10RTP
     C*
     C***** DATA FILE FIELDS HERE
     C*
     C                   MOVE      NEST          P10ST
     C                   MOVE      NEEND#        P10EN#
     C                   MOVE      *BLANKS       P10BVI
     C                   MOVE      *BLANKS       P10CVI
     C                   MOVE      NEMEDT        P10EFD
     C                   EXSR      SRSTRP
     C     ELKY2         CHAIN     PEL01                              60
     C     *IN60         IFEQ      '0'
     C     ELEXM3        IFNE      *ZEROS
     C                   MOVEL     ELEXM3        @4A               4
     C                   ELSE
     C     ELEXM2        IFNE      *ZEROS
     C                   MOVEL     ELEXM2        @4A
     C                   ELSE
     C                   MOVEL     ELEXM1        @4A
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C     @4A           IFEQ      '0000'
     C*
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     RSJRST        IFNE      'CA'
     C     RSJRST        ANDNE     'DE'
     C     RSJRST        ANDNE     'NJ'
     C     RSJRST        ANDNE     'PA'
     C                   MOVE      '1000'        @4A
     C                   ENDIF                                                  RSJRST IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  @4A IFEQ
     C                   MOVE      @4A           P10EMF
     C                   MOVEL(P)  NERNAM        P10ENM
     C                   MOVE      NEEEDT        P10CGD
     C*
     C*
      * FORMAT AND WRITE WCPOLS HERE
     C*
      *
     C*
     C                   CLEAR                   RDATA
     C**** WRITE CANCL RECORD
     C*
     C                   MOVEL     P10           RDATA
     C                   WRITE     RPOLS
     C                   ADD       1             ##TOTR           10 0
     C                   ADD       1             ##TLEN
     C*
     C     END10         ENDSR                                                  TYPE='10'
     C*****************************************************************
     C     SR0402        BEGSR
     C*
     C     XXSPLT        IFEQ      *BLANK
     C                   CLEAR                   P09
     C                   MOVEL(P)  PEDATA        E00402
     C*
     C                   MOVE      ##LNK         P09LNK
     C                   MOVE      PEST          P09ST
     C                   MOVE      '09'          P09RTP
     C                   MOVEL     PEEND#        P09EN#
     C                   MOVEL(P)  PEEND#        XX9               9
     C                   MOVE      XX9           P09BVI
     C                   MOVE      *BLANKS       P09CVI
     C                   MOVEL     PENAME        P09NAM
     C                   MOVE      PEEFDT        P09CGD
     C*
     C     PEDATA        IFNE      *BLANKS
     C                   MOVE      X9ANDT        P09AND
     C                   ENDIF                                                  PEDATA IFNE
     C*
     C                   CLEAR                   RDATA
     C**** WRITE RECORD
     C*
     C                   MOVEL     P09           RDATA
     C                   WRITE     RPOLS
     C                   ADD       1             ##TOTR           10 0
     C                   ENDIF                                                  XXSPLT IFEQ
     C*
     C                   ENDSR
     C*****************************************************************
     C*
     C*==================================================================*
     C     RCD42         BEGSR
     C*==================================================================*
     C*
     C*
     C* * MOVE FIELDS * * *
     C                   MOVE      NECAR1        ##CARR
     C                   If        xxClt# = '045' and QQOldCar = 'Y'
     C                   Z-add     33057         ##CARR
     C                   Endif
     C     QQST          IFEQ      *BLANKS
     C     XXCLT#        ANDEQ     '132'
     C     NEFND         ANDEQ     5
     C                   Z-ADD     12985         ##CARR
     C                   ENDIF                                                  QQST IFEQ
     C                   MOVEL     #SHPOL        ##POL#                         ?????
     C                   EXSR      SRPOL#
     C                   MOVE      NEPFDT        ##PFDT
     C                   MOVE      XXDTJ         ##TRDT
     C*** TRANS CODE HERE!
     C                   MOVE      03            ##TRCD                          ENDORSMENT
     C*
     C                   MOVE      ##LNK         P42LNK
     C                   MOVE      '42'          P42RTP
     C*
     C***** DATA FILE FIELDS HERE
     C*
     C                   MOVE      NEST          P42ST
     C                   MOVE      NEEND#        P42EN#
     C                   MOVE      *BLANKS       P42BVI
     C                   MOVE      *BLANKS       P42CVI
     C                   MOVE      NEMEDT        P42EFD
     C                   EXSR      SRSTRP
     C     ELKY2         CHAIN     PEL01                              60
     C     *IN60         IFEQ      '0'
     C     ELEXM3        IFNE      *ZEROS
     C                   MOVEL     ELEXM3        @4A               4
     C                   ELSE
     C     ELEXM2        IFNE      *ZEROS
     C                   MOVEL     ELEXM2        @4A
     C                   ELSE
     C                   MOVEL     ELEXM1        @4A
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   MOVE      @4A           P42EMF
     C                   MOVE      *BLANKS       P42ENM
     C                   MOVE      NEEEDT        P42CGD
     C*
     C*
      * FORMAT AND WRITE WCPOLS HERE
     C*
      *
     C*
     C                   CLEAR                   RDATA
     C**** WRITE CANCL RECORD
     C*
     C                   MOVEL     P42           RDATA
     C                   WRITE     RPOLS
     C                   ADD       1             ##TOTR           10 0
     C                   ADD       1             ##TLEN
     C*
     C     END42         ENDSR                                                  TYPE='42'
     C*
     C*==================================================================*
     C     RCD87         BEGSR
     C*==================================================================*
     C*
     C*
     C* * MOVE FIELDS * * *
     C                   MOVE      NECAR1        ##CARR
     C                   If        xxClt# = '045' and QQOldCar = 'Y'
     C                   Z-add     33057         ##CARR
     C                   Endif
     C     QQST          IFEQ      *BLANKS
     C     XXCLT#        ANDEQ     '132'
     C     NEFND         ANDEQ     5
     C                   Z-ADD     12985         ##CARR
     C                   ENDIF                                                  QQST IFEQ
     C                   MOVEL     #SHPOL        ##POL#                         ?????
     C                   EXSR      SRPOL#
     C                   MOVE      NEPFDT        ##PFDT
     C                   MOVE      XXDTJ         ##TRDT
     C*** TRANS CODE HERE!
     C                   MOVE      03            ##TRCD                          ENDORSMENT
     C*
     C                   MOVE      ##LNK         P87LNK
     C                   MOVE      '87'          P87RTP
     C*
     C***** DATA FILE FIELDS HERE
     C*
     C                   MOVE      NEEND#        P87EN#
     C                   MOVE      *BLANKS       P87BVI
     C                   MOVE      *BLANKS       P87CVI
     C                   MOVE      NECAR2        P87CC#
     C                   MOVEL     #RVPOL        ##POL#
     C                   EXSR      SRPOL#
     C                   MOVEL     ##POL#        P87PL#
     C                   MOVE      NEPFD2        P87EFD
     C                   MOVE      *ZEROS        P87EXD
     C*
     C***** TRANSLATE TOB  TO LEGAL NATURE OF EMPLOYER
     C*
     C     NELGST        IFEQ      '1'
     C                   MOVE      01            P87LNI
     C                   ELSE
     C     NELGST        IFEQ      '2'
     C                   MOVE      02            P87LNI
     C                   ELSE
     C     NELGST        IFEQ      '3'
     C                   MOVE      03            P87LNI
     C                   ELSE
     C                   MOVE      99            P87LNI
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       P87RTO
     C                   MOVE      *BLANKS       P873AC
     C                   MOVE      *BLANKS       P873CI
     C                   MOVE      *BLANKS       P87S01
     C                   MOVE      *BLANKS       P87S02
     C                   MOVE      *BLANKS       P87S03
     C                   MOVE      *BLANKS       P87S04
     C                   MOVE      *BLANKS       P87S05
     C                   MOVE      *BLANKS       P87S06
     C                   MOVE      *BLANKS       P87S07
     C                   MOVE      *BLANKS       P87S08
     C                   MOVE      *BLANKS       P87S09
     C                   MOVE      *BLANKS       P87S10
     C                   MOVE      *BLANKS       P87S11
     C                   MOVE      *BLANKS       P87S12
     C                   MOVE      *BLANKS       P87S13
     C                   MOVE      *BLANKS       P87S14
     C                   MOVE      *BLANKS       P87S15
     C                   MOVE      *BLANKS       P87REL
     C                   MOVE      *BLANKS       P87IAP
     C                   MOVE      *BLANKS       P87REN
     C                   MOVE      *BLANKS       P87PRN
     C                   MOVE      NERSK#        P87RID
     C                   MOVE      NESEQ#        P87ES#
      *
      *
     C                   MOVE      *BLANKS       P87ENM
     C                   MOVE      NEEEDT        P87CGD
     C*
     C*
      * FORMAT AND WRITE WCPOLS HERE
     C*
      *
     C*
     C                   CLEAR                   RDATA
     C*
     C**** WRITE CANCL RECORD
     C*
     C                   MOVEL     P87           RDATA
     C                   WRITE     RPOLS
     C                   ADD       1             ##TOTR           10 0
     C                   ADD       1             ##TLEN
     C*
     C     END87         ENDSR                                                  TYPE='87'
     C*
     C*==================================================================*
     C     RCD88         BEGSR
     C*==================================================================*
     C*
     C*
     C* * MOVE FIELDS * * *
     C                   MOVE      NECAR1        ##CARR
     C                   If        xxClt# = '045' and QQOldCar = 'Y'
     C                   Z-add     33057         ##CARR
     C                   Endif
     C     QQST          IFEQ      *BLANKS
     C     XXCLT#        ANDEQ     '132'
     C     NEFND         ANDEQ     5
     C                   Z-ADD     12985         ##CARR
     C                   ENDIF                                                  QQST IFEQ
     C                   MOVEL     #SHPOL        ##POL#                         ?????
     C                   EXSR      SRPOL#
     C                   MOVE      NEPFDT        ##PFDT
     C                   MOVE      XXDTJ         ##TRDT
     C*** TRANS CODE HERE!
     C                   MOVE      03            ##TRCD                          ENDORSMENT
     C*
     C                   MOVE      ##LNK         P88LNK
     C                   MOVE      '88'          P88RTP
     C*
     C***** DATA FILE FIELDS HERE
     C*
     C                   MOVE      NEEND#        P88EN#
     C                   MOVE      *BLANKS       P88BVI
     C                   MOVE      *BLANKS       P88CVI
     C                   MOVE      '2'           P88NTP
     C                   MOVE      NEENCD        P88ECD
     C                   MOVE      NENLKC        P88NLK
     C                   MOVEL(P)  NERNAM        P88NAM
     C     NERNM2        IFNE      *BLANKS
     C     NERNAM        CAT(P)    NERNM2:0      #WRK10          100
     C                   MOVEL(P)  #WRK10        P88NAM
     C                   ENDIF                                                  NERNM2 IFNE
     C                   MOVE      NEFEID        P88FID
     C                   MOVE      001           P88CSQ
     C                   MOVE      *BLANKS       P88LNE
     C                   MOVE      *BLANKS       P88LND
     C                   MOVE      *BLANKS       P88RS1
     C                   MOVE      *BLANKS       P88EI1
     C                   MOVE      *BLANKS       P88RS2
     C                   MOVE      *BLANKS       P88EI2
     C                   MOVE      *BLANKS       P88RS3
     C                   MOVE      *BLANKS       P88EI3
      *
     C                   EXSR      SRSTRP
     C*
     C                   MOVEL(P)  EMNAME        P88ENM
     C     EMFDNM        IFNE      *BLANKS
     C                   MOVEL(P)  EMFDNM        P88ENM
     C                   ENDIF
     C                   MOVE      NEEEDT        P88CGD
     C     NEST          IFEQ      '29'
     C                   MOVE      NEUE#         P88EI1
     C                   MOVE      NEST          P88RS1
     C                   MOVE      '01'          P88UES
     C                   ENDIF
     C                   MOVE      *BLANK        P88NRC
     C*
     C                   SELECT
     C     NERNMP        WHENEQ    'P'
     C     NERNMP        OREQ      'Q'
     C                   MOVE      'P'           P88PEO
     C     NERNMP        WHENEQ    'C'
     C     NERNMP        OREQ      'D'
     C                   MOVE      'C'           P88PEO
     C                   OTHER
     C                   MOVE      *BLANK        P88PEO
     C                   ENDSL                                                  NANAMP SELEC
     C     NERNM4        IFNE      *BLANKS
     C                   MOVEL     XXRM01        P88NAM
     C                   ENDIF                                                  NERNM4 NE *BLAN
     C*
     C*
      * FORMAT AND WRITE WCPOLS HERE
     C*
      *
     C*
     C                   CLEAR                   RDATA
     C**** WRITE CANCL RECORD
     C*
     C                   MOVEL     P88           RDATA
     C                   WRITE     RPOLS
     C                   ADD       1             ##TOTR           10 0
     C*
     C* WRITE AN ADDITIONAL NAME RECORD FOR EACH ADDITIONAL 90 CHARACTERS
     C* OF THE FULL NAME.
     C*
     C     NERNM4        IFNE      *BLANKS
     C*
     C     2             DO        4             X
     C*
     C     RMA(X)        IFNE      *BLANKS
     C*
     C                   ADD       1             P88CSQ
     C                   ADD       1             CS8CNT
     C                   MOVEL(P)  RMA(X)        P88NAM
     C*
     C                   CLEAR                   RDATA
     C**** WRITE NAME RECORD
     C*
     C                   MOVEL     P88           RDATA
     C                   WRITE     RPOLS
     C*
     C                   ADD       1             ##TOTR           10 0
     C*
     C                   ENDIF                                                  RMA,X IFNE
     C*
     C                   ENDDO                                                  2 DO 4 X
     C*
     C                   ENDIF                                                  NERNM4 IFNE
     C                   ADD       1             ##TLEN
     C*
     C     END88         ENDSR                                                  TYPE='88'
     C*
     C*==================================================================*
     C     RCD89         BEGSR
     C*==================================================================*
     C*
     C*
     C* * MOVE FIELDS * * *
     C                   MOVE      NECAR1        ##CARR
     C                   If        xxClt# = '045' and QQOldCar = 'Y'
     C                   Z-add     33057         ##CARR
     C                   Endif
     C     QQST          IFEQ      *BLANKS
     C     XXCLT#        ANDEQ     '132'
     C     NEFND         ANDEQ     5
     C                   Z-ADD     12985         ##CARR
     C                   ENDIF                                                  QQST IFEQ
     C                   MOVEL     #SHPOL        ##POL#                         ?????
     C                   EXSR      SRPOL#
     C                   MOVE      NEPFDT        ##PFDT
     C                   MOVE      XXDTJ         ##TRDT
     C*** TRANS CODE HERE!
     C                   MOVE      03            ##TRCD                          ENDORSMENT
     C*
     C                   MOVE      ##LNK         P89LNK
     C                   MOVE      '89'          P89RTP
     C*
     C***** DATA FILE FIELDS HERE
     C*
     C                   MOVE      NEEND#        P89EN#
     C                   MOVE      *BLANKS       P89BVI
     C                   MOVE      *BLANKS       P89CVI
     C                   MOVE      NEADTP        P89ATP
     C                   MOVE      '1'           P89ASI
     C                   MOVEL     #RADR         P89ADR
     C                   MOVE      #NAEL         P89NAE
     C                   MOVE      NENLKC        P89NLK
     C                   MOVE      #PHN#         P89PH#
      *
     C                   EXSR      SRSTRP
     C*
     C                   MOVEL(P)  EMNAME        P89ENM
     C     EMFDNM        IFNE      *BLANKS
     C                   MOVEL(P)  EMFDNM        P89ENM
     C                   ENDIF
     C                   MOVE      NEEEDT        P89CGD
     C                   Z-ADD     NEEMPY        P89RNE
      *
     C                   MOVEA     NESIC         SIC1
     C                   Z-ADD     1             X
     C     *BLANK        LOOKUP    SIC1(X)                                42
     C     *IN42         IFEQ      *OFF
     C                   Z-ADD     6             X
     C                   ELSE
     C                   SUB       1             X
     C                   ENDIF                                                  *NI42 IFEQ *OFF
     C                   Z-ADD     6             Y
     C                   MOVE      *ZEROS        SIC2
     C     X             DOWGT     0
     C                   MOVE      SIC1(X)       SIC2(Y)
     C                   SUB       1             X
     C                   SUB       1             Y
     C                   ENDDO                                                  X DOWGT 0
     C                   MOVEA     SIC2          XXSIC
     C                   TESTN                   XXSIC                4242
     C     *IN42         IFEQ      *ON
     C                   MOVEA     SIC2          P89SIC
     C                   ELSE
     C                   MOVE      *ZEROS        P89SIC
     C                   ENDIF                                                  *IN42 IFEQ *ON
     C*
     C*
      * FORMAT AND WRITE WCPOLS HERE
     C*
      *
     C*
     C                   CLEAR                   RDATA
     C**** WRITE CANCL RECORD
     C*
     C                   MOVEL     P89           RDATA
     C                   WRITE     RPOLS
     C                   ADD       1             ##TOTR           10 0
     C                   ADD       1             ##TLEN
     C*
     C     END89         ENDSR                                                  TYPE='89'
     C*****************************************************************
     C     SR0407        BEGSR
     C*
      /free
       If qqSt = 'MA';
          LeaveSr;
       Endif;
      /end-free
     C                   CLEAR                   P11
     C                   MOVEL(P)  PEDATA        E00407
     C*
     C                   MOVE      ##LNK         P11LNK
     C                   MOVE      PEST          P11ST
     C                   MOVE      '11'          P11RTP
     C                   MOVEL     PEEND#        P11EN#
     C                   MOVEL(P)  PEEND#        XX9               9
     C                   MOVE      XX9           P11BVI
     C                   MOVE      *BLANKS       P11CVI
     C                   MOVEL     PENAME        P11NAM
     C                   MOVE      PEEFDT        P11CGD
     C*
     C     PEDATA        IFNE      *BLANKS
     C                   MOVE      XXEFD         P11EFD
     C                   MOVE      XXSCP         P11SCP
     C                   MOVE      XXLHP         P11LHP
     C                   MOVE      XXSID         P11SID
     C                   MOVE      XXLID         P11LID
     C                   MOVE      XXNLH         P11NLH
     C                   MOVE      XXCC1         P11CC1
     C                   MOVE      XXEC1         P11EC1
     C                   MOVE      XXRT1         P11RT1
     C                   MOVE      XXCC2         P11CC2
     C                   MOVE      XXEC2         P11EC2
     C                   MOVE      XXRT2         P11RT2
     C                   MOVE      XXCC1         P11CC3
     C                   MOVE      XXEC1         P11EC3
     C                   MOVE      XXRT1         P11RT3
     C                   MOVE      XXCC2         P11CC4
     C                   MOVE      XXEC2         P11EC4
     C                   MOVE      XXRT2         P11RT4
     C                   MOVE      XXCC1         P11CC5
     C                   MOVE      XXEC1         P11EC5
     C                   MOVE      XXRT1         P11RT5
     C                   MOVE      XXCC2         P11CC6
     C                   MOVE      XXEC2         P11EC6
     C                   MOVE      XXRT2         P11RT6
     C                   ENDIF                                                  PEDATA IFNE
     C*
     C                   CLEAR                   RDATA
     C**** WRITE RECORD
     C*
     C                   MOVEL     P11           RDATA
     C                   WRITE     RPOLS
     C                   ADD       1             ##TOTR           10 0
     C*
     C                   ENDSR
     C*****************************************************************
     C     SR0404        BEGSR
     C*
     C                   CLEAR                   P12
     C*
     C                   MOVE      ##LNK         P12LNK
     C                   MOVE      PEST          P12ST
     C                   MOVE      '12'          P12RTP
     C                   MOVEL     PEEND#        P12EN#
     C                   MOVEL(P)  PEEND#        XX9               9
     C                   MOVE      XX9           P12BVI
     C                   MOVE      *BLANKS       P12CVI
     C                   MOVEL     PENAME        P12NAM
     C                   MOVE      PEEFDT        P12CGD
     C*
     C*
     C                   CLEAR                   RDATA
     C**** WRITE RECORD
     C*
     C                   MOVEL     P12           RDATA
     C                   WRITE     RPOLS
     C                   ADD       1             ##TOTR           10 0
     C*
     C                   ENDSR
     C*****************************************************************
     C     SR0503        BEGSR
     C*
     C                   CLEAR                   P15
     C                   MOVEL(P)  PEDATA        E00503
     C*
     C                   ADD       1             X15
     C*
     C                   MOVE      ##LNK         P15LNK
     C                   MOVE      PEST          P15ST
     C                   MOVE      '15'          P15RTP
     C                   MOVE      '5'           P15RPO
     C                   MOVEL     PEEND#        P15EN#
     C                   MOVEL(P)  PEEND#        XX9               9
     C                   MOVE      XX9           P15BVI
     C                   MOVE      *BLANKS       P15CVI
     C                   MOVEL     PENAME        P15NAM
     C                   MOVE      PEEFDT        P15CGD
     C                   MOVE      X15           P15SEQ
     C*
     C     PEDATA        IFNE      *BLANKS
     C                   Z-ADD     XXLLIM        P15LL
     C                   MOVE      XXLCON        P15LCF
     C                   MOVE      XXTXST        P15STM
     C                   MOVE      XXTXFE        P15FTM
     C     PEST          IFEQ      29
     C                   MOVE      XXTXFE        P15WTM
     C                   MOVE      01000         P15FTM
     C                   ELSE
     C                   MOVE      01000         P15WTM
     C     XXTXFE        IFEQ      *ZERO
     C                   MOVE      01000         P15FTM
     C                   ENDIF
     C                   ENDIF
     C                   MOVE      XXRDF1        P15RD1
     C                   MOVE      XXRDF2        P15RD2
     C                   MOVE      XXRDF3        P15RD3
     C                   MOVE      XXM050        P15M05
     C                   MOVE      XXM100        P15M10
     C                   MOVE      XXM150        P15M15
     C                   MOVE      XXX050        P15X05
     C                   MOVE      XXX100        P15X10
     C                   MOVE      XXX150        P15X15
     C                   MOVE      XXB050        P15B05
     C                   MOVE      XXB100        P15B10
     C                   MOVE      XXB150        P15B15
     C                   MOVE      XXE050        P15P05
     C                   MOVE      XXE100        P15P10
     C                   MOVE      XXE150        P15P15
     C                   MOVE      XXLFST        P15SEL
     C                   MOVE      XXLFFE        P15FEL
     C     QQST          IFEQ      'NJ'
     C                   MOVE      XXHG          P15HG
     C                   ENDIF
     C                   MOVE      PEEFDT        P15EFD
     C                   ENDIF                                                  PEDATA IFNE
     C*
     C                   CLEAR                   RDATA
     C**** WRITE RECORD
     C*
     C                   MOVEL     P15           RDATA
     C                   WRITE     RPOLS
     C                   ADD       1             ##TOTR           10 0
     C*
     C                   ENDSR
     C*****************************************************************
     C     SR0106        BEGSR
     C*
     C                   CLEAR                   P19
     C                   MOVEL(P)  PEDATA        E00106
     C*
     C                   MOVE      ##LNK         P19LNK
     C                   MOVE      '19'          P19RTP
     C                   MOVEL     PEEND#        P19EN#
     C                   MOVEL(P)  PEEND#        XX9               9
     C                   MOVE      XX9           P19BVI
     C                   MOVE      *BLANKS       P19CVI
     C                   MOVEL     PENAME        P19NAM
     C                   MOVE      PEEFDT        P19CGD
     C*
     C     PEDATA        IFNE      *BLANKS
     C                   MOVE      XXS01         P19S01
     C                   MOVE      XXP01         P19P01
     C                   MOVE      XXS02         P19S02
     C                   MOVE      XXP02         P19P02
     C                   MOVE      XXS03         P19S03
     C                   MOVE      XXP03         P19P03
     C                   MOVE      XXS04         P19S04
     C                   MOVE      XXP04         P19P04
     C                   MOVE      XXS05         P19S05
     C                   MOVE      XXP05         P19P05
     C                   MOVE      XXS06         P19S06
     C                   MOVE      XXP06         P19P06
     C                   MOVE      XXS07         P19S07
     C                   MOVE      XXP07         P19P07
     C                   MOVE      XXS08         P19S08
     C                   MOVE      XXP08         P19P08
     C                   MOVE      XXS09         P19S09
     C                   MOVE      XXP09         P19P09
     C                   MOVE      XXS10         P19S10
     C                   MOVE      XXP10         P19P10
     C                   MOVE      XXS11         P19S11
     C                   MOVE      XXP11         P19P11
     C                   MOVE      XXS12         P19S12
     C                   MOVE      XXP12         P19P12
     C                   MOVE      XXS13         P19S13
     C                   MOVE      XXP13         P19P13
     C                   MOVE      XXS14         P19S14
     C                   MOVE      XXP14         P19P14
     C                   MOVE      XXS15         P19S15
     C                   MOVE      XXP15         P19P15
     C                   MOVE      XXS16         P19S16
     C                   MOVE      XXP16         P19P16
     C                   MOVE      XXS17         P19S17
     C                   MOVE      XXP17         P19P17
     C                   MOVE      XXS18         P19S18
     C                   MOVE      XXP18         P19P18
     C                   MOVE      XXS19         P19S19
     C                   MOVE      XXP19         P19P19
     C                   MOVE      XXS20         P19S20
     C                   MOVE      XXP20         P19P20
     C                   MOVE      XXS21         P19S21
     C                   MOVE      XXP21         P19P21
     C                   MOVE      XXS22         P19S22
     C                   MOVE      XXP22         P19P22
     C                   MOVE      XXS23         P19S23
     C                   MOVE      XXP23         P19P23
     C                   MOVE      XXS24         P19S24
     C                   MOVE      XXP24         P19P24
     C                   MOVE      XXS25         P19S25
     C                   MOVE      XXP25         P19P25
     C                   ENDIF                                                  PEDATA IFNE
     C*
     C                   CLEAR                   RDATA
     C**** WRITE RECORD
     C*
     C                   MOVEL     P19           RDATA
     C                   WRITE     RPOLS
     C                   ADD       1             ##TOTR           10 0
     C*
     C                   ENDSR
     C*****************************************************************
     C     SR0201        BEGSR
     C*
     C                   CLEAR                   P21
     C                   MOVEL(P)  PEDATA        E00201
     C*
     C                   ADD       1             X21
     C*
     C                   MOVE      ##LNK         P21LNK
     C                   MOVE      '21'          P21RTP
     C                   MOVEL     PEEND#        P21EN#
     C                   MOVEL(P)  PEEND#        XX9               9
     C                   MOVE      XX9           P21BVI
     C                   MOVE      *BLANKS       P21CVI
     C                   MOVEL     PENAME        P21NAM
     C                   MOVE      PEEFDT        P21CGD
     C                   MOVE      X21           P21SEQ
     C*
     C     PEDATA        IFNE      *BLANKS
     C                   MOVE      XPACC         P21ACC
     C                   MOVE      XPDIS         P21DIS
     C                   MOVE      XXTWM         P21TWM
     C                   MOVE      XXDOW         P21DOW
     C                   ENDIF                                                  PEDATA IFNE
     C*
     C                   CLEAR                   RDATA
     C**** WRITE RECORD
     C*
     C                   MOVEL     P21           RDATA
     C                   WRITE     RPOLS
     C                   ADD       1             ##TOTR           10 0
     C*
     C                   ENDSR
     C*****************************************************************
     C     SR0301        BEGSR
     C*
     C                   CLEAR                   P24
     C                   MOVEL(P)  PEDATA        E00301
     C*
     C                   ADD       1             X24
     C*
     C                   MOVE      ##LNK         P24LNK
     C                   MOVE      '24'          P24RTP
     C                   MOVEL     PEEND#        P24EN#
     C                   MOVEL(P)  PEEND#        XX9               9
     C                   MOVE      XX9           P24BVI
     C                   MOVE      *BLANKS       P24CVI
     C                   MOVEL     PENAME        P24NAM
     C                   MOVE      PEEFDT        P24CGD
     C                   MOVE      X24           P24SEQ
     C*
     C     PEDATA        IFNE      *BLANKS
     C                   MOVE      XXAEN         P24AEN
     C                   MOVE      XXAEA         P24AEA
     C                   MOVE      XXSSE         P24SSE
     C                   MOVE      XXCOP         P24COP
     C                   ENDIF                                                  PEDATA IFNE
     C*
     C                   CLEAR                   RDATA
     C**** WRITE RECORD
     C*
     C                   MOVEL     P24           RDATA
     C                   WRITE     RPOLS
     C                   ADD       1             ##TOTR           10 0
     C*
     C                   ENDSR
     C*****************************************************************
     C     SR0302        BEGSR
     C*
     C                   CLEAR                   P25
     C                   MOVEL(P)  PEDATA        E00302
     C*
     C                   ADD       1             X25
     C*
     C                   MOVE      ##LNK         P25LNK
     C                   MOVE      '25'          P25RTP
     C                   MOVEL     PEEND#        P25EN#
     C                   MOVEL(P)  PEEND#        XX9               9
     C                   MOVE      XX9           P25BVI
     C                   MOVE      *BLANKS       P25CVI
     C                   MOVEL     PENAME        P25NAM
     C                   MOVE      PEEFDT        P25CGD
     C                   MOVE      X25           P25SEQ
     C*
     C     PEDATA        IFNE      *BLANKS
     C                   MOVE      XXLNC         P25LNC
     C                   ENDIF                                                  PEDATA IFNE
     C*
     C                   CLEAR                   RDATA
     C**** WRITE RECORD
     C*
     C                   MOVEL     P25           RDATA
     C                   WRITE     RPOLS
     C                   ADD       1             ##TOTR           10 0
     C*
     C                   ENDSR
     C*****************************************************************
     C     SR0102        BEGSR
     C*
     C                   CLEAR                   P27
     C                   MOVEL(P)  PEDATA        E00102
     C*
     C                   ADD       1             X27
     C*
     C                   MOVE      ##LNK         P27LNK
     C                   MOVE      '27'          P27RTP
     C                   MOVEL     PEEND#        P27EN#
     C                   MOVEL(P)  PEEND#        XX9               9
     C                   MOVE      XX9           P27BVI
     C                   MOVE      *BLANKS       P27CVI
     C                   MOVEL     PENAME        P27NAM
     C                   MOVE      PEEFDT        P27CGD
     C                   MOVE      X27           P27SEQ
     C*
     C     PEDATA        IFNE      *BLANKS
     C                   MOVE      XXSCH         P27SCH
     C                   ENDIF                                                  PEDATA IFNE
     C*
     C                   CLEAR                   RDATA
     C**** WRITE RECORD
     C*
     C                   MOVEL     P27           RDATA
     C                   WRITE     RPOLS
     C                   ADD       1             ##TOTR           10 0
     C*
     C                   ENDSR
     C*****************************************************************
     C     SR0311        BEGSR
     C*
     C                   CLEAR                   P29
     C                   MOVEL(P)  PEDATA        E00311
     C*
     C                   ADD       1             X29
     C*
     C                   MOVE      ##LNK         P29LNK
     C                   MOVE      '29'          P29RTP
     C                   MOVEL     PEEND#        P29EN#
     C                   MOVEL(P)  PEEND#        XX9               9
     C                   MOVE      XX9           P29BVI
     C                   MOVE      *BLANKS       P29CVI
     C                   MOVEL     PENAME        P29NAM
     C                   MOVE      PEEFDT        P29CGD
     C                   MOVE      X29           P29SEQ
     C*
     C     PEDATA        IFNE      *BLANKS
     C                   MOVE      XXIE          P29IE
     C                   MOVE      XXIST         P29IST
     C                   MOVE      XXDWC         P29DWC
     C                   ENDIF                                                  PEDATA IFNE
     C*
     C                   CLEAR                   RDATA
     C**** WRITE RECORD
     C*
     C                   MOVEL     P29           RDATA
     C                   WRITE     RPOLS
     C                   ADD       1             ##TOTR           10 0
     C*
     C                   ENDSR
     C*****************************************************************
     C     SR0406        BEGSR
     C*
     C                   CLEAR                   P30
     C                   MOVEL(P)  PEDATA        E00406
     C*
     C*
     C                   MOVE      ##LNK         P30LNK
     C                   MOVE      '30'          P30RTP
     C                   MOVEL     PEEND#        P30EN#
     C                   MOVEL(P)  PEEND#        XX9               9
     C                   MOVE      XX9           P30BVI
     C                   MOVE      *BLANKS       P30CVI
     C                   MOVEL     PENAME        P30NAM
     C                   MOVE      PEEFDT        P30CGD
     C*
     C     PEDATA        IFNE      *BLANKS
     C                   MOVE      XXST1         P30ST1
     C                   MOVE      XXDP1         P30DP1
     C                   MOVE      XXAPD         P30APD
     C                   MOVE      XXST2         P30ST2
     C                   MOVE      XXDP2         P30DP2
     C                   MOVE      XXST3         P30ST3
     C                   MOVE      XXDP3         P30DP3
     C                   MOVE      XXST4         P30ST4
     C                   MOVE      XXDP4         P30DP4
     C                   MOVE      XXST5         P30ST5
     C                   MOVE      XXDP5         P30DP5
     C                   MOVE      XXST6         P30ST6
     C                   MOVE      XXDP6         P30DP6
     C                   ENDIF                                                  PEDATA IFNE
     C*
     C                   CLEAR                   RDATA
     C**** WRITE RECORD
     C*
     C                   MOVEL     P30           RDATA
     C                   WRITE     RPOLS
     C                   ADD       1             ##TOTR           10 0
     C*
     C                   ENDSR
     C*****************************************************************
     C     SR0313        BEGSR
     C*
     C                   CLEAR                   P36
     C                   MOVEL(P)  PEDATA        E00313
     C*
     C                   MOVE      ##LNK         P36LNK
     C                   MOVE      PEST          P36ST
     C                   MOVE      '36'          P36RTP
     C                   MOVEL     PEEND#        P36EN#
     C                   MOVEL(P)  PEEND#        XX9               9
     C                   MOVE      XX9           P36BVI
     C                   MOVE      *BLANKS       P36CVI
     C                   MOVEL     PENAME        P36NAM
     C                   MOVE      PEEFDT        P36CGD
     C*
     C     PEDATA        IFNE      *BLANKS
     c*** begin add ***  05/01/23  *******************************************
     C*****              MOVE      XXNOP         P36NOP
     C*****              MOVE      XXNOO         P36NOO
     C                   Clear                   xxTemp          100
      //* Remove unwanted characters and blank out the \n carrage returns.
           xxTemp = %trim(RmvExceptJSON(%trim(xxnop)));
           xxTemp = %scanrpl('\n':' ':xxTemp);
           p36NOP = %subst(xxTemp:1:60);
           Clear xxTemp;
           xxTemp = %trim(RmvExceptJSON(%trim(xxnoo)));
           xxTemp = %scanrpl('\n':' ':xxTemp);
           p36NOO = %subst(xxTemp:1:60);
     c*** end add *****  05/01/23  *******************************************
     C                   ENDIF                                                  PEDATA IFNE
     C*
     C                   CLEAR                   RDATA
     C**** WRITE RECORD
     C*
     C                   MOVEL     P36           RDATA
     C                   WRITE     RPOLS
     C                   ADD       1             ##TOTR           10 0
     C*
     C                   ENDSR
     C*****************************************************************
     C     SR0310        BEGSR
     C*
     C                   CLEAR                   P37
     C                   MOVEL(P)  PEDATA        E00310
     C*
     C                   MOVE      ##LNK         P37LNK
     C                   MOVE      '37'          P37RTP
     C                   MOVEL     PEEND#        P37EN#
     C                   MOVEL(P)  PEEND#        XX9               9
     C                   MOVE      XX9           P37BVI
     C                   MOVE      *BLANKS       P37CVI
     C                   MOVEL     PENAME        P37NAM
     C                   MOVE      PEEFDT        P37CGD
     C*
     C     PEDATA        IFNE      *BLANKS
     C                   MOVE      XXDS1         P37DS1
     C                   MOVE      XXNM1         P37NM1
     C                   MOVE      X1ST1         P37ST1
     C                   MOVE      XXDS2         P37DS2
     C                   MOVE      XXNM2         P37NM2
     C                   MOVE      X1ST2         P37ST2
     C                   MOVE      XXDS3         P37DS3
     C                   MOVE      XXNM3         P37NM3
     C                   MOVE      X1ST3         P37ST3
     C                   ENDIF                                                  PEDATA IFNE
     C*
     C                   CLEAR                   RDATA
     C**** WRITE RECORD
     C*
     C                   MOVEL     P37           RDATA
     C                   WRITE     RPOLS
     C                   ADD       1             ##TOTR           10 0
     C*
     C                   ENDSR
     C*****************************************************************
     C     SR0308        BEGSR
     C*
     C                   CLEAR                   P38
     C                   MOVEL(P)  PEDATA        E00308
     C*
     C                   MOVE      ##LNK         P38LNK
     C                   MOVE      '38'          P38RTP
     C                   MOVEL     PEEND#        P38EN#
     C                   MOVEL(P)  PEEND#        XX9               9
     C                   MOVE      XX9           P38BVI
     C                   MOVE      *BLANKS       P38CVI
     C                   MOVEL     PENAME        P38NAM
     C                   MOVE      PEEFDT        P38CGD
     C*
     C     PEDATA        IFNE      *BLANKS
     C                   MOVE      X1DS1         P38DS1
     C                   MOVE      X1NM1         P38NM1
     C                   MOVE      X1DS2         P38DS2
     C                   MOVEL     X1NM2         P38NM2
     C                   MOVE      X1DS3         P38DS3
     C                   MOVE      X1NM3         P38NM3
     C                   ENDIF                                                  PEDATA IFNE
     C*
     C                   CLEAR                   RDATA
     C**** WRITE RECORD
     C*
     C                   MOVEL     P38           RDATA
     C                   WRITE     RPOLS
     C                   ADD       1             ##TOTR           10 0
     C*
     C                   ENDSR
     C*****************************************************************
     C     SR0401        BEGSR
     C*
     C                   CLEAR                   P39
     C                   MOVEL(P)  PEDATA        E00401
     C*
     C                   MOVE      ##LNK         P39LNK
     C                   MOVE      '39'          P39RTP
     C                   MOVEL     PEEND#        P39EN#
     C                   MOVEL(P)  PEEND#        XX9               9
     C                   MOVE      XX9           P39BVI
     C                   MOVE      *BLANKS       P39CVI
     C                   MOVEL     PENAME        P39NAM
     C                   MOVE      PEEFDT        P39CGD
     C*
     C     PEDATA        IFNE      *BLANKS
     C                   MOVE      X2ST1         P39ST1
     C                   MOVE      XXAN1         P39AN1
     C                   MOVE      XXSC1         P39SC1
     C                   MOVE      XXMX1         P39MX1
     C                   MOVE      XXEP1         P39EP1
     C                   MOVE      X2ST2         P39ST2
     C                   MOVE      XXAN2         P39AN2
     C                   MOVE      XXSC2         P39SC2
     C                   MOVE      XXMX2         P39MX2
     C                   MOVE      XXEP2         P39EP2
     C                   MOVE      X2ST3         P39ST3
     C                   MOVE      XXAN3         P39AN3
     C                   MOVE      XXSC3         P39SC3
     C                   MOVE      XXMX3         P39MX3
     C                   MOVE      XXEP3         P39EP3
     C                   MOVE      X2ST4         P39ST4
     C                   MOVE      XXAN4         P39AN4
     C                   MOVE      XXSC4         P39SC4
     C                   MOVE      XXMX4         P39MX4
     C                   MOVE      XXEP4         P39EP4
     C                   MOVE      X2ST5         P39ST5
     C                   MOVE      XXAN5         P39AN5
     C                   MOVE      XXSC5         P39SC5
     C                   MOVE      XXMX5         P39MX5
     C                   MOVE      XXEP5         P39EP5
     C                   ENDIF                                                  PEDATA IFNE
     C*
     C                   CLEAR                   RDATA
     C**** WRITE RECORD
     C*
     C                   MOVEL     P39           RDATA
     C                   WRITE     RPOLS
     C                   ADD       1             ##TOTR           10 0
     C*
     C                   ENDSR
     C*****************************************************************
     C     SR0315        BEGSR
     C*
     C                   CLEAR                   P41
     C                   MOVEL(P)  PEDATA        E00315
     C*
     C                   MOVE      ##LNK         P41LNK
     C                   MOVE      '41'          P41RTP
     C                   MOVEL     PEEND#        P41EN#
     C                   MOVEL(P)  PEEND#        XX9               9
     C                   MOVE      XX9           P41BVI
     C                   MOVE      *BLANKS       P41CVI
     C                   MOVEL     PENAME        P41NAM
     C                   MOVE      PEEFDT        P41CGD
     C*
     C     PEDATA        IFNE      *BLANKS
     C                   MOVE      XXFAW         P41FAW
     C                   MOVE      XXDHW         P41DHW
     C                   ENDIF                                                  PEDATA IFNE
     C*
     C                   CLEAR                   RDATA
     C**** WRITE RECORD
     C*
     C                   MOVEL     P41           RDATA
     C                   WRITE     RPOLS
     C                   ADD       1             ##TOTR           10 0
     C*
     C                   ENDSR
     C*****************************************************************
     C     SR0403        BEGSR
     C*
      *** Beg Add ***** 02/14/24 ************************************
       If (%Subst(PEend#:1:4) = 'WC99' AND xxExcl99 = 'Y');
          LeaveSr;
       Endif;
      *** End Add ***** 02/14/24 ************************************
     C                   CLEAR                   P43
     C                   MOVEL(P)  PEDATA        E00403
     C*
     C                   MOVE      ##LNK         P43LNK
     C                   MOVE      PEST          P43ST
     C                   MOVE      '43'          P43RTP
     C                   MOVEL     PEEND#        P43EN#
     C                   MOVEL(P)  PEEND#        XX9               9
     C                   MOVE      XX9           P43BVI
     C                   MOVE      *BLANKS       P43CVI
     C*** begin add ***  12/12/22  ********************************************
       Select;
       When xxclt# = '153' and %subst(PEEnd#:1:8) = 'WC991011';
          P43EN# = ' ';
          P43BVI = ' ';
          P43CVI = PEEnd#;
       //*** begin add ***  08/13/24  *****************************************
       When xxclt# = '148' and %subst(PEEnd#:1:4) = 'WC99';
          P43EN# = ' ';
          P43BVI = ' ';
          P43CVI = PEEnd#;
       //*** end add *****  08/13/24  *****************************************
       Endsl;
     C*** end add *****  12/12/22  ********************************************
     C                   MOVEL     PENAME        P43NAM
     C                   MOVE      PEEFDT        P43CGD
     C*
     C     PEDATA        IFNE      *BLANKS
     C                   MOVE      XXDAC         P43DAC
     C                   MOVE      XXPRP         P43PRP
     C                   Move      xxdtype       P43DTP
     C                   Move      xxdpc         P43DP
     C                   Move      xxdag         P43DAA
     C                   ENDIF                                                  PEDATA IFNE
     C*
     C                   CLEAR                   RDATA
     C**** WRITE RECORD
     C*
     C                   MOVEL     P43           RDATA
     C                   WRITE     RPOLS
     C                   ADD       1             ##TOTR           10 0
     C*
     C                   ENDSR
     C*****************************************************************
      //*** Beg Add ***** 11/15/22 *************************************
      //========================================================
      // Report WC000405 Endorsement (Policy Period Endorsement)
      //========================================================
      /Free
       Begsr SR0405;
        Clear P13;
        E00405 = PEdata;
        P13Lnk = ##Lnk;
        P13Rtp = '13';
        P13En# = PEEnd#;
        P13BVI = %Subst(PEend#:9:1);
        P13CVI = *Blanks;
        P13Nam = PEname;
        P13Efd = %Dec(%char(%date(PEEFDT:*cymd):*ymd0):6:0);
        If PEdata <> *Blanks;
           P13EF1 = %Dec(x5EfDt1:6:0);
           P13EX1 = %Dec(x5ExDt1:6:0);
           P13EF2 = %Dec(x5EfDt2:6:0);
           P13EX2 = %Dec(x5ExDt2:6:0);
           P13EF3 = %Dec(x5EfDt3:6:0);
           P13EX3 = %Dec(x5ExDt3:6:0);
        Endif;
        Clear RData;
        RData = P13;
        Write RPols;
        ##totr = ##totr + 1;
       Endsr;
      /End-Free
      *** End Add ***** 11/15/22 *************************************
     C*****************************************************************
     C     SR07401       BEGSR
     C*
     C                   CLEAR                   PBD
     C                   MOVEL(P)  PEDATA        E00403
     C*
     C                   MOVE      ##LNK         PBDLNK
     C                   MOVE      PEST          PBDST
     C                   MOVE      'BD'          PBDRTP
     C                   MOVEL     PEEND#        PBDEN#
     C                   MOVEL(P)  PEEND#        XX9               9
     C                   MOVE      XX9           PBDBVI
     C                   MOVE      *BLANKS       PBDCVI
     C                   MOVEL     PENAME        PBDNAM
     C                   MOVE      PEEFDT        PBDCGD
     C*
     C     PEDATA        IFNE      *BLANKS
     C                   MOVE      XXDAC         PBDDAC
     C                   MOVE      XXPRP         PBDPRP
     C                   ENDIF                                                  PEDATA IFNE
     C*
     C                   CLEAR                   RDATA
     C**** WRITE RECORD
     C*
     C                   MOVEL     PBD           RDATA
     C                   WRITE     RPOLS
     C                   ADD       1             ##TOTR           10 0
     C*
     C                   ENDSR
     C*****************************************************************
     C     SR31315       BEGSR
     C*
     C                   CLEAR                   PCE
     C                   MOVEL(P)  PEDATA        E00403
     C*
     C                   MOVE      ##LNK         PCELNK
     C                   MOVE      PEST          PCEST
     C                   MOVE      'CE'          PCERTP
     C                   MOVEL     PEEND#        PCEEN#
     C                   MOVEL(P)  PEEND#        XX9               9
     C                   MOVE      XX9           PCEBVI
     C                   MOVE      *BLANKS       PCECVI
     C                   MOVEL     PENAME        PCENAM
     C                   MOVE      PEEFDT        PCECGD
     C*
     C     PEDATA        IFNE      *BLANKS
     C                   MOVE      XXDAC         PCEDAC
     C                   ENDIF                                                  PEDATA IFNE
     C*
     C                   CLEAR                   RDATA
     C**** WRITE RECORD
     C*
     C                   MOVEL     PCE           RDATA
     C                   WRITE     RPOLS
     C                   ADD       1             ##TOTR           10 0
     C*
     C                   ENDSR
     C*****************************************************************
     C     SR04603       BEGSR
     C*
     C                   CLEAR                   PDR
     C                   MOVEL(P)  PEDATA        E00403
     C*
     C                   MOVE      ##LNK         PDRLNK
     C                   MOVE      PEST          PDRST
     C                   MOVE      'DR'          PDRRTP
     C                   MOVEL     PEEND#        PDREN#
     C                   MOVEL(P)  PEEND#        XX9               9
     C                   MOVE      XX9           PDRBVI
     C                   MOVE      *BLANKS       PDRCVI
     C                   MOVEL     PENAME        PDRNAM
     C                   MOVE      PEEFDT        PDRCGD
     C*
     C     PEDATA        IFNE      *BLANKS
     C                   MOVE      XXDAC         PDRDAC
     C                   MOVE      XXDAG         PDRDAA
     C                   Z-add     0             PDRDNC
     C                   MOVE      XXALEI        PDRALEI
     C                   MOVE      XXFECI        PDRFECI
     C                   ENDIF                                                  PEDATA IFNE
     C                   If        PDRALEI = ' '
     C                   Move      'N'           PDRALEI
     C                   Endif
     C                   If        PDRFECI = ' '
     C                   Move      'N'           PDRFECI
     C                   Endif
     C*
     C                   CLEAR                   RDATA
     C**** WRITE RECORD
     C*
     C                   MOVEL     PDR           RDATA
     C                   WRITE     RPOLS
     C                   ADD       1             ##TOTR           10 0
     C*
     C                   ENDSR
     C*****************************************************************
     C*
     C*****************************************************************
     C     S37310        BEGSR
     C*
     C                   CLEAR                   PBI
     C                   MOVEL(P)  PEDATA        E37310
     C*
     C                   MOVE      ##LNK         PBILNK
     C                   MOVE      PEST          PBIST
     C                   MOVE      'BI'          PBIRTP
     C                   MOVEL     PEEND#        PBIEN#
     C                   MOVEL(P)  PEEND#        XX9               9
     C                   MOVE      XX9           PBIBVI
     C                   MOVE      *BLANKS       PBICVI
     C                   MOVEL     PENAME        PBINAM
     C                   MOVE      PEEFDT        PBIEED
     C                   Move      'C'           PBICOR
     C*
     C     PEDATA        IFNE      *BLANKS
     C                   MOVEL     X4NM1         PBINM1
     C                   MOVEL     XXOF1         PBIOH1
     C                   Z-ADD     XXPO1         PBIPO1
     C                   MOVEL     X4NM2         PBINM2
     C                   MOVEL     XXOF2         PBIOH2
     C                   Z-ADD     XXPO2         PBIPO2
     C                   MOVEL     x4Nm3         PBinm3
     C                   MOVEL     xxOf3         PBioh3
     C                   Z-ADD     xxPo3         PBipo3
     C                   ENDIF                                                  PEDATA IFNE
     C*
     C                   CLEAR                   RDATA
     C**** WRITE RECORD
     C*
     C                   MOVEL     PBI           RDATA
     C                   WRITE     RPOLS
     C                   ADD       1             ##TOTR           10 0
     C*
     C                   ENDSR                                                  S37310 SUBR
     C*****************************************************************
     C*
     C     S29601        BEGSR
     C*
     C                   CLEAR                   PAH
     C                   MOVEL(P)  PEDATA        E29601
     C*
     C                   MOVE      ##LNK         PAHLNK
     C                   MOVE      PEST          PAHST
     C                   MOVE      'AH'          PAHRTP
     C                   MOVEL     PEEND#        PAHEN#
     C                   MOVEL(P)  PEEND#        XX9               9
     C                   MOVE      XX9           PAHBVI
     C                   MOVE      *BLANKS       PAHCVI
     C                   MOVEL     PENAME        PAHNAM
     C                   MOVE      PEEFDT        PAHEED
     C*
     C     PEDATA        IFNE      *BLANKS
     C*** Beg Add ***** 07/23/24 *************************************
     C                   IF        PEEND# = 'WC290601A'
     C*** End Add ***** 07/23/24 *************************************
     C                   Z-ADD     XXBIA         PAHBIA
     C                   Z-ADD     XXBID         PAHBID
     C                   Z-ADD     XXABI         PAHABI
      //*** Beg Add ***** 07/23/24 ***********************************
     C                   ENDIF
     C                   IF        PEEND# = 'WC290605'
      //*** End Add ***** 07/23/24 ***********************************
     C                   Z-ADD     XXBIAP        PAHBIAP
     C                   Z-ADD     XXBIDP        PAHBIDP
      //*** Beg Chg *** 07/06/22 *******************************************
     C***************    Z-ADD     XXAGPO        PAHAGPO
       PAhAgPo = 0;
       //*** End Chg *** 07/06/22 ******************************************
     C                   Z-ADD     XXAGPP        PAHAGPP
      //*** Beg Add ***** 07/23/24 ***********************************
     C                   ENDIF
      //*** End Add ***** 07/23/24 ***********************************
     C                   ENDIF                                                  PEDATA IFNE
     C*
     C                   CLEAR                   RDATA
     C**** WRITE RECORD
     C*
     C                   MOVEL     PAH           RDATA
     C                   WRITE     RPOLS
     C                   ADD       1             ##TOTR           10 0
     C*
     C                   ENDSR                                                  S29601 SUBR
     C*****************************************************************
     C     S29303        BEGSR
     C*
     C                   CLEAR                   PAG
     C                   MOVEL(P)  PEDATA        E29303
     C*
     C                   MOVE      ##LNK         PAGLNK
     C                   MOVE      PEST          PAGST
     C                   MOVE      'AG'          PAGRTP
     C                   MOVEL     PEEND#        PAGEN#
     C                   MOVEL(P)  PEEND#        XX9               9
     C                   MOVE      XX9           PAGBVI
     C                   MOVE      *BLANKS       PAGCVI
     C                   MOVEL     PENAME        PAGNAM
     C                   MOVE      PEEFDT        PAGEED
     C*
     C     PEDATA        IFNE      *BLANKS
     C                   MOVEL     XGNM1         PAGCNM
     C                   MOVEL     XGAD1         PAGADR
     C                   ENDIF                                                  PEDATA IFNE
     C*
     C                   CLEAR                   RDATA
     C**** WRITE RECORD
     C*
     C                   MOVEL     PAG           RDATA
     C                   WRITE     RPOLS
     C                   ADD       1             ##TOTR           10 0
     C*
     C                   ENDSR                                                  S29303 SUBR
     C*****************************************************************
     C*
     C*****************************************************************
     C     S04302        BEGSR
     C*
     C                   CLEAR                   PDB
     C                   MOVEL(P)  PEDATA        E04302
     C*
     C                   MOVE      ##LNK         PDBLNK
     C                   MOVE      PEST          PDBST
     C                   MOVE      'DB'          PDBRTP
     C                   MOVEL     PEEND#        PDBEN#
     C                   MOVEL(P)  PEEND#        XX9               9
     C                   MOVE      XX9           PDBBVI
     C                   MOVE      *BLANKS       PDBCVI
     C                   MOVEL     PENAME        PDBNAM
     C                   MOVE      PEEFDT        PDBEED
     C*
     C     PEDATA        IFNE      *BLANKS
     C                   MOVEL     X2NM1         PDBNM1
     C                   MOVEL     X2NM2         PDBNM2
     C                   MOVEL     X2NM3         PDBNM3
     C                   ENDIF                                                  PEDATA IFNE
     C*
     C                   CLEAR                   RDATA
     C**** WRITE RECORD
     C*
     C                   MOVEL     PDB           RDATA
     C                   WRITE     RPOLS
     C                   ADD       1             ##TOTR           10 0
     C*
     C                   ENDSR
     C*****************************************************************
     C     S04303        BEGSR
     C*
     C                   CLEAR                   PDC
     C                   MOVEL(P)  PEDATA        E04303
     C*
     C                   MOVE      ##LNK         PDCLNK
     C                   MOVE      PEST          PDCST
     C                   MOVE      'DC'          PDCRTP
     C                   MOVEL     PEEND#        PDCEN#
     C                   MOVEL(P)  PEEND#        XX9               9
     C                   MOVE      XX9           PDCBVI
     C                   MOVE      *BLANKS       PDCCVI
     C                   MOVEL     PENAME        PDCNAM
     C                   MOVE      PEEFDT        PDCEED
     C*
     C     PEDATA        IFNE      *BLANKS
     C                   MOVEL     X3NM1         PDCNM1
     C                   MOVEL     X3NM2         PDCNM2
     C                   MOVEL     X3NM3         PDCNM3
     C                   ENDIF                                                  PEDATA IFNE
     C*
     C                   CLEAR                   RDATA
     C**** WRITE RECORD
     C*
     C                   MOVEL     PDC           RDATA
     C                   WRITE     RPOLS
     C                   ADD       1             ##TOTR           10 0
     C*
     C                   ENDSR
      *=====================================================
      * Report WC040305 Endorsement (CA Voluntary Labor)
      *=====================================================
     c     sr04305       begsr

     C                   Leavesr
     c                   clear                   pdd
     c                   movel(p)  PEDATA        e04305

     c                   move      ##lnk         PDDLNK
     c                   move      PEST          PDDST
     c                   move      'DD'          PDDRTP
     c                   movel     PEEND#        PDDEN#
     c                   movel(p)  PEEND#        xx9               9
     c                   move      xx9           PDDBVI
     c                   move      *blanks       PDDCVI
     c                   movel     PENAME        PDDNAM
     c                   move      PEEFDT        PDDEED

     c     PEDATA        ifne      *blanks
     c                   movel     x5nm1         PDDNM1
     c                   movel     x5nm2         PDDNM2
     c                   movel     x5nm3         PDDNM3
     c                   endif                                                  PEDATA IFNE
      *    Write record
     c                   clear                   rdata
     c                   movel     pdd           rdata
     c                   write     rpols
     c                   add       1             ##totr

     c                   endsr
      *=====================================================
      * Report WC040306 Endorsement (CA Waiver Charge)
      *=====================================================
     c     sr04306       begsr

     C                   Leavesr
     c                   clear                   pde
     c                   movel(p)  PEDATA        e04306

     c                   move      ##lnk         PDELNK
     c                   move      PEST          PDEST
     c                   move      'DE'          PDERTP
     c                   movel     PEEND#        PDEEN#
     c                   movel(p)  PEEND#        xx9               9
     c                   move      xx9           PDEBVI
     c                   move      *blanks       PDECVI
     c                   movel     PENAME        PDENAM
     c                   move      PEEFDT        PDEEED

     c     PEDATA        ifne      *blanks
     c                   movel     x6nm1         PDENM1
     c                   movel     x6nm2         PDENM2
     c                   movel     x6pct         PDEPCT
     c                   endif                                                  PEDATA IFNE

     c                   add       1             xxCaWvrSeq#
     c                   move      xxCaWvrSeq#   PDeseq

      *    Write record
     c                   clear                   rdata
     c                   movel     pde           rdata
     c                   write     rpols
     c                   add       1             ##totr

     c                   endsr
      *=====================================================
      * Report WC040318A Endorsement (CA LLC Exclusion)
      *=====================================================
     c     sr04318A      begsr

     c                   clear                   pdq
     c                   movel(p)  PEDATA        e04305

     c                   move      ##lnk         PDQLNK
     c                   move      PEST          PDQST
     c                   move      'DQ'          PDQRTP
     c                   movel     PEEND#        PDQEN#
     c                   movel(p)  PEEND#        xx9
     c                   move      xx9           PDQBVI
     c                   move      *blanks       PDQCVI
     c                   movel     PENAME        PDQNAM
     c                   move      PEEFDT        PDQEED

     c     PEDATA        ifne      *blanks
     c                   movel     x5nm1         PDQNM1
     c                   movel     x5nm2         PDQNM2
     c                   movel     x5nm3         PDQNM3
     c                   endif                                                  PEDATA IFNE
      *    Write record
     c                   clear                   rdata
     c                   movel     pdq           rdata
     c                   write     rpols
     c                   add       1             ##totr

     c                   endsr
     C*****************************************************************
     C     SR0412        BEGSR
     C*
     C     QQST          IFNE      'DE'
     C     QQST          ANDNE     'MI'
     C     QQST          ANDNE     'NJ'
     C     QQST          ANDNE     'NY'
     C     QQST          ANDNE     'PA'
     C*
     C                   CLEAR                   P42
     C                   MOVEL(P)  PEDATA        E00412
     C*
     C                   MOVE      ##LNK         P42LNK
     C                   MOVE      PEST          P42ST
     C     P42ST         IFEQ      '00'
     C     P42ST         OREQ      *BLANKS
     C                   MOVE      '99'          P42ST
     C                   ENDIF
     C*
     C                   MOVE      '42'          P42RTP
     C                   MOVEL     PEEND#        P42EN#
     C                   MOVEL(P)  PEEND#        XX9               9
     C                   MOVE      XX9           P42BVI
     C                   MOVE      *BLANKS       P42CVI
     C                   MOVEL     PENAME        P42ENM
     C                   MOVE      PEEFDT        P42CGD
     C*
     C     PEDATA        IFNE      *BLANKS
     C                   MOVE      X4EDT         P42EFD
     C                   MOVE      X4MOD         P42EMF
     C                   ENDIF                                                  PEDATA IFNE
     C*
     C                   CLEAR                   RDATA
     C**** WRITE RECORD
     C*
     C                   MOVEL     P42           RDATA
     C                   WRITE     RPOLS
     C                   ADD       1             ##TOTR           10 0
     C*
     C                   ENDIF                                                  QQST IFNE
     C*
     C                   ENDSR
      *=====================================================
      * Report WC200304B Endorsement (MA Employee Leasing)
      *=====================================================
     c     sr20304       begsr

     c                   clear                   pfa
     c                   movel(p)  PEDATA        e20304

     c                   move      ##lnk         PFALNK
     c                   move      PEST          PFAST
     c                   move      'FA'          PFARTP
     c                   movel     PEEND#        PFAEN#
     c                   movel(p)  PEEND#        xx9
     c                   move      xx9           PFABVI
     c                   move      *blanks       PFACVI
     c                   movel     PENAME        PFANAM
     c                   move      PEEFDT        PFAEED

     c     PEDATA        ifne      *blanks
     c                   movel     x7cname       PFACNAME
     c                   movel     x7cfein       PFACFEIN
     c                   movel     x7cadr        PFACADR
     c                   movel     x7ccity       PFACCITY
     c                   movel     x7cst         PFACST
     c                   movel     x7czip        PFACZIP
     c                   endif                                                  PEDATA IFNE
      *    Write record
     c                   clear                   rdata
     c                   movel     pfa           rdata
     c                   write     rpols
     c                   add       1             ##totr

     c                   endsr
      *=====================================================
      * Report WC480315 Endorsement (WI Employee Leasing)
      *=====================================================
     c     SR48315       begsr

     c                   clear                   PEC
     c                   movel(p)  PEDATA        e48315
                         If %subst(e48315:171:10) = *blank;
                            x8cprem = *Zeros;
                         Endif;

     c                   move      ##lnk         PECLNK
     c                   move      PEST          PECST
     c                   move      'EC'          PECRTP
     c                   movel     PEEND#        PECEN#
     c                   movel(p)  PEEND#        xx9
     c                   move      xx9           PECBVI
     c                   move      *blanks       PECCVI

     c     PEDATA        ifne      *blanks
     c                   movel     x8cname       PECCNAME
     c                   movel     x8cadr        PECCADR
     c                   movel     x8ccity       PECCCITY
     c                   movel     x8cst         PECCST
     c                   movel     x8czip        PECCZIP
     c                   movel     x8cfein       PECCFEIN
     c                   endif                                                  PEDATA IFNE

     c                   movel     PENAME        PECNAM
     c                   move      PEEFDT        PECEED
        If x8cprem <> *Zeros;
           PECpamt = x8cprem ;
        Endif;
      *    Write record
     c                   clear                   rdata
     c                   movel     PEC           rdata
     c                   write     rpols
     c                   add       1             ##totr

     c                   endsr
     c*** beg add *****  01/02/24  ******************************************
      *=====================================================
      * Report WC480316 Endorsement (WI Employee Leasing)
      *=====================================================
     c     SR48316       begsr

     c                   clear                   PED
     c                   movel(p)  PEDATA        e48316
                         If %subst(e48316:121:6) = *blank;
                            x10TEffDt = *Zeros;
                         Endif;
                         If %subst(e48316:179:6) = *blank;
                            x10DtSent = *Zeros;
                         Endif;

     c                   move      ##lnk         PEDLNK
     c                   move      PEST          PEDST
     c                   move      'ED'          PEDRTP
     c                   movel     PEEND#        PEDEN#
     c                   movel(p)  PEEND#        xx9
     c                   move      xx9           PEDBVI
     c                   move      *blanks       PEDCVI

     c     PEDATA        ifne      *blanks
     c                   movel     x10EmpLname   PEDELCMP
     c                   movel     x10Cltname    PEDCNAME
     c                   movel     x10EntRcvF    PEDERCVF
     c                   endif                                                  PEDATA IFNE

     c                   movel     PENAME        PEDNAM
     c                   move      PEEFDT        PEDEED
        If x10TEffDt <> *Zeros;
           PEDTED = x10TEffDt;
        Endif;
        If x10DtSent <> *Zeros;
           PEDDTSENT = x10DtSent;
        Endif;
      *    Write record
     c                   clear                   rdata
     c                   movel     PED           rdata
     c                   write     rpols
     c                   add       1             ##totr

     c                   endsr
     c*** end add *****  01/02/24  ******************************************
      *=====================================================
      * Report WC040316B Endorsement (CA Employee Leasing)
      *=====================================================
     c     SR04316B      begsr

     c                   clear                   PDL
     c                   movel(p)  PEDATA        e04316

     c                   move      ##lnk         PDLLNK
     c                   move      PEST          PDLST
     c                   move      'DL'          PDLRTP
     c                   movel     PEEND#        PDLEN#
     c                   movel(p)  PEEND#        xx9
     c                   move      xx9           PDLBVI
     c                   move      *blanks       PDLCVI
     c                   move      '01'          PDLSRNBR

     c     PEDATA        ifne      *blanks
     c                   movel     x9cname       PDLLCNAM
     c                   movel     x9cadr        PDLLCADR
     c                   movel     x9ccity       PDLLCCITY
     c                   movel     x9cst         PDLLCST
     c                   movel     x9czip        PDLLCZIP
     c                   Z-ADD     1             PDLSEQN
     c                   endif                                                  PEDATA IFNE

     c                   movel     PENAME        PDLNAM
     c                   move      PEEFDT        PDLEED
      *    Write record
     c                   clear                   rdata
     c                   movel     PDL           rdata
     c                   write     rpols
     c                   add       1             ##totr

     c                   endsr
      *=====================================================
      * Report WC040315B Endorsement (CA Employee Leasing)
      *=====================================================
     c     SR04315B      begsr

     c                   clear                   PDK
     c                   movel(p)  PEDATA        e04315

     c                   move      ##lnk         PDKLNK
     c                   move      PEST          PDKST
     c                   move      'DK'          PDKRTP
     c                   movel     PEEND#        PDKEN#
     c                   movel(p)  PEEND#        xx9
     c                   move      xx9           PDKBVI
     c                   move      *blanks       PDKCVI
     C*** begin chg ***  08/09/24  *** WC2978 *******************************
     c******             move      '01'          PDKSRNBR
     C                   Add       1             XDK
     c                   Move      XDK           PDKSRNBR
     C*** end chg *****  08/09/24  *** WC2978 *******************************

     c     PEDATA        ifne      *blanks
     c                   movel     xkNmCl        PDKCNAM
     c                   movel     xkadr1        PDKCADR
     c                   movel     xkadrcty      PDKCCITY
     c                   movel     xkadrstat     PDKCST
     c                   movel     xkadrzip      PDKCZIP
     c                   move      xkseq#        PDKSEQN
     c                   endif                                                  PEDATA IFNE

     c                   move      *blanks       PDKNAM
     c                   move      PEEFDT        PDKEED
      *    Write record
     c                   clear                   rdata
     c                   movel     PDK           rdata
     c                   write     rpols
     c                   add       1             ##totr

     c                   endsr
      *=====================================================
      * Report WC040342B Endorsement (CA Limiting Agreement)
      *=====================================================
      /Free
       Begsr SR04342B;

          Select;
          When PEstat = 'M';
             Clear PDM;
             e04342DM = PEData;
             PDMLnk = ##lnk;
             PDMSt  = PEST;
             PDMrtp = 'DM';
             PDMEn# = PEEnd#;
             PDMbvi = %subst(PEend#:9:1);
             PDMcvi = *Blank;
             PDMNAM = PENAME;
             PDMEED = %dec(%char(%date(PEEFDT:*cymd):*ymd0):6:0);
             PDMEXD = %dec(%char(%date(ELPTDT:*cymd):*ymd0):6:0);
             Clear intNamOpLen;
             If PEdata <> *Blank;
                PDMNamOp  = xmNamOp;
                PDMClsCd  = xmClsCd;
                PDMClsWSf = xmClsWSf;
                PDMClsWrd = xmClsWrd;
                PDMEndSeQ = xmEndSeq#;
                PDMSrNbr  = xmEndSer#;
                intNamOpLen = %len(%trim(xmNamOp));    //Check if xmNamOp exceeds 30 bytes
             Endif;
             Clear RData;
             RData = PDM;
             Write RPols;
             ##totr = ##totr + 1;
             //Write continuation record...
             If intNamOpLen > 30;
                Clear PDMNamOp;
                PDMNamOp  = %subst(xmNamOp:31:10);
                PDMEndSeQ = '02';
                Clear PDMClsCd;
                Clear PDMClsWSf;
                Clear PDMClsWrd;
                Clear PDMEed;
                Clear PDMExd;
                Clear PDMNam;
                Clear RData;
                RData = PDM;
                Write RPols;
                ##totr = ##totr + 1;
             Endif;
          When PEstat = 'O';
             Clear PDO;
             e04342DO = PEData;
             PDOLnk = ##lnk;
             PDOSt  = PEST;
             PDOrtp = 'DO';
             PDOEn# = PEEnd#;
             PDObvi = %subst(PEend#:9:1);
             PDOcvi = *Blank;
             PDONAM = PENAME;
             PDOEED = %dec(%char(%date(PEEFDT:*cymd):*ymd0):6:0);
             PDOEXD = %dec(%char(%date(ELPTDT:*cymd):*ymd0):6:0);
             If PEdata <> *Blank;
                PDOINam    = xoINam;
                PDOIrCod   = xoIRCod;
                PDOIrNam   = xoIRNam;
                PDOAltPol# = xoAltPol#;
                PDOAltInDt = xoAltInDt;
                PDOAltExDt = xoAltExdt;
                PDOLawInsI = xoLawInsI;
                PDOWrtAfoI = xoWrtAfoI;
                PDOSrNbr   = xoEndSer#;
                PDOEndSeq  = xoEndSeq#;
             Endif;
             Clear RData;
             RData = PDO;
             Write RPols;
             ##totr = ##totr + 1;
          Endsl;
       Endsr;
      *==========================================================
      * Report WC000424  Endorsement (Audit NonCompliance Charge)
      *==========================================================
     c     SR00424       begsr

     c                   clear                   P44
     c                   movel(p)  PEDATA        e00424

     c                   move      ##lnk         P44LNK
     c                   move      '44'          P44RTP
     c                   movel     PEEND#        P44EN#
     c                   movel(p)  PEEND#        xx9
     c                   move      xx9           P44BVI
     c                   move      *blanks       P44CVI

     c     PEDATA        ifne      *blanks
     c                   movel     x0State1      P44ST01
     c                   movel     x0ANCbasis1   P44ANCB01
     c                   movel     x0ANCmult1    P44ANCM01
     c                   movel     x0State2      P44ST02
     c                   movel     x0ANCbasis2   P44ANCB02
     c                   movel     x0ANCmult2    P44ANCM02
     c                   movel     x0State3      P44ST03
     c                   movel     x0ANCbasis3   P44ANCB03
     c                   movel     x0ANCmult3    P44ANCM03
     C                   If        P44ST01 <> ' '
     C                   Movel(p)  ancBasis      P44ANCB01
     C                   Endif
     C                   If        P44ST02 <> ' '
     C                   Movel(p)  ancBasis      P44ANCB02
     C                   Endif
     C                   If        P44ST03 <> ' '
     C                   Movel(p)  ancBasis      P44ANCB03
     C                   Endif
     c                   endif                                                  PEDATA IFNE

     c                   movel     PENAME        P44NAM
     c                   move      PEEFDT        P44EED
      *    Write record
     c                   clear                   rdata
     c                   movel     P44           rdata
     c                   write     rpols
     c                   add       1             ##totr

     c                   endsr
      *==========================================================
      * Report WC370401 Endorsement (Audit NonCompliance Charge)
      *==========================================================
      *** Beg Del ***  01/15/25  *****************************************
     c*****SR37401       begsr

     c*****              clear                   P44
     c*****              movel(p)  PEDATA        e37401

     c*****              move      ##lnk         P44LNK
     c*****              move      '44'          P44RTP
     c*****              movel     PEEND#        P44EN#
     c*****              movel(p)  PEEND#        xx9
     c*****              move      xx9           P44BVI
     c*****              move      *blanks       P44CVI

     c*****PEDATA        ifne      *blanks
     c*****              movel     x37State1     P44ST01
     c*****              movel     x37ANCbasis1  P44ANCB01
     c*****              movel     x37ANCmult1   P44ANCM01
     c*****              movel     x37State2     P44ST02
     c*****              movel     x37ANCbasis2  P44ANCB02
     c*****              movel     x37ANCmult2   P44ANCM02
     c*****              movel     x37State3     P44ST03
     c*****              movel     x37ANCbasis3  P44ANCB03
     c*****              movel     x37ANCmult3   P44ANCM03
     C*****              If        P44ST01 <> ' '
     C*****              Movel(p)  ancBasis      P44ANCB01
     C*****              Endif
     C*****              If        P44ST02 <> ' '
     C*****              Movel(p)  ancBasis      P44ANCB02
     C*****              Endif
     C*****              If        P44ST03 <> ' '
     C*****              Movel(p)  ancBasis      P44ANCB03
     C*****              Endif
     c*****              endif                                                  PEDATA IFNE

     c*****              movel     PENAME        P44NAM
     c*****              move      PEEFDT        P44EED
      *    Write record
     c*****              clear                   rdata
     c*****              movel     P44           rdata
     c*****              write     rpols
     c*****              add       1             ##totr

     c*****              endsr
      *** End Del ***  01/15/25  *****************************************

      *==========================================================
      * Report WC370604 Endorsement (PA Employer Assessment)
      *==========================================================
     c     SR37604       begsr

     c                   clear                   PBJ
     c                   movel(p)  PEDATA        e37604BJ

     c                   move      ##lnk         PBJLNK
     c                   move      '37'          PBJST
     c                   move      'BJ'          PBJRTP
     c                   movel     PEEND#        PBJEN#
     c                   movel(p)  PEEND#        xx9
     c                   move      xx9           PBJBVI
     c                   move      *blanks       PBJCVI

     c     PEDATA        ifne      *blanks
     c     X37604EAF     mult      10000         PBJEAF
     c     X37604EAA     mult      100           PBJEAA
     c                   endif                                                  PEDATA IFNE

     c                   movel     PENAME        PBJNAM
     c                   move      PEEFDT        PBJEED
      *    Write record
     c                   clear                   rdata
     c                   movel     PBJ           rdata
     c                   write     rpols
     c                   add       1             ##totr

     c                   endsr
      *** begin add ***  10/25/22  *** 103957 ******************************
      *====================================================================
      * Report WC480322 Endorsement (WI Employee Leasing Company Leased
      * Employee Inclusion Endorsement)
      *====================================================================
     c     SR48322       begsr

     c                   clear                   PEI
     c                   movel(p)  PEdata        e48322

     c                   move      ##lnk         PEilnk
     c                   move      PEst          PEist
     c                   move      'EI'          PEirtp
     c                   movel     PEend#        PEien#
     c                   movel(p)  PEend#        xx9
     c                   move      xx9           PEibvi
     c                   move      *blanks       PEicvi

     c     PEDATA        ifne      *blanks
     c                   movel     xiCName       PEicname
     c                   movel     xiCPol#       PEipol#
     c                   movel     xiCFein       PEicfein
     c                   movel     xiCAdr        PEicadr
     c                   movel     xiCCity       PEiccity
     c                   movel     xiCSt         PEicst
     c                   movel     xiCZip        PEiczip
     c                   endif                                                  PEDATA IFNE

     c                   movel     PEname        PEinam
     c                   move      PEefdt        PEieed
      *    Write record
     c                   clear                   rdata
     c                   movel     PEI           rdata
     c                   write     rpols
     c                   add       1             ##totr

     c                   endsr
      *** End   Add ***  10/25/22  *** 103957 ******************************
      *** Begin Add ***  09/21/23  *** WC2923 ******************************
      *====================================================================
      * Report WC480323 Endorsement (WI Named Insured Cancellation
      * Endorsement)
      *====================================================================
     c     SR48323       Begsr

     c                   Clear                   PEJ
     c                   Movel(p)  PEdata        e48323

     c                   Move      ##lnk         PEjlnk
     c                   Move      PEst          PEjst
     c                   Move      'EJ'          PEjrtp
     c                   Movel     PEend#        PEjen#
     c                   Movel(p)  PEend#        xx9
     c                   Move      xx9           PEjbvi
     c                   Move      *blanks       PEjcvi

     c     PEdata        Ifne      *blanks
     c                   Movel     xjCName       PEjcname
     c                   Movel     xjCAdr        PEjiadd
     c                   Movel     xjCCnlc       PEjrcnl

       // Convert Cancellation Effective Date MMDDYYYY to CCYYMMDD
       PEjcnld = %Dec(%Char(%Date(xjCCnld:*Usa):*Iso0):8:0);
     c                   Endif                                                  PEDATA IFNE
     c                   Movel     PEname        PEjnam

       // Convert Endorsement Effective Date CYYMMDD to CCYYMMDD
       PEjeed = %Dec(%Char(%Date(PEefdt:*CYMD):*Iso0):8:0);

      *    Write record
     c                   Clear                   rdata
     c                   Movel     PEJ           rdata
     c                   Write     rpols
     c                   Add       1             ##totr

     c                   Endsr
      *** End   Add ***  09/21/23  *** WC2923 ******************************
     C*
     C*****************************************************************
     C*
      *
      * CHECK IF THE POLICY HAS BEEN PROCESSED.
      *
     C     CHKPOL        BEGSR
      *
     C                   MOVE      'P'           X1TYPE            1
     C                   MOVE      '4'           X1STAT            1
      *
     C     S1KEY         CHAIN     PSH02                              69
      *
     C     *IN69         IFEQ      *ON
     C     *IN59         ANDEQ     *ON
      *
     C                   MOVE      'P'           X1TYPE            1
     C                   MOVE      '3'           X1STAT            1
      *
     C     S1KEY         CHAIN     PSH02                              69
      *
     C     *IN69         IFEQ      *OFF
     C                   Z-ADD     1             I
     C     QQST          IFNE      *BLANKS
     C                   MOVE      QQST          XZST              2
     C                   ELSE
     C                   MOVE      'XX'          XZST
     C                   ENDIF
      *
     C     XZST          LOOKUP    S2T(I)                                 60
     C     *IN60         IFEQ      *OFF
     C     S2S(I)        ORNE      '4'
     C                   MOVE      *ON           *IN69
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   ENDIF                                                  *IN69 IFEQ
      *
     C                   ENDIF                                                  *IN69 IFEQ
      *
     C                   ENDSR                                                  CHKPOL
     C*****************************************************************
     C     SRSTRP        BEGSR
     C*
      *
      * FIND STATE REPORTING DIVISION
      *
     C     EMKY1         CHAIN     PEM01                              70
      *
     C     *IN70         DOWEQ     *OFF
     C     EMSTRP        ANDNE     'Y'
     C     EMKY1         READE     PEM01                                  70
     C                   ENDDO                                                  *IN70 DOWEQ *OFF
      *
      * IF NO STATE REPORTING DIVISION FOUND, DEFAULT TO FIRST DIVISION
      *
     C     *IN70         IFEQ      *ON
     C     EMKY1         CHAIN     PEM01                              70
     C                   ENDIF                                                  *IN70 IFEQ *ON
      *
     C                   ENDSR                                                  STRP
     C*****************************************************************
     C*
     C* SUBROUTINE TO DETERMINE IF A DATE RANGE CONTAINS FEBRUARY 29TH.
     C*
     C*
     C*  IF FEB 29 IS A VALID DATE AND IS IN THE RANGE, SUBTRACT ONE DAY
     C*
      *================================================================
      *   SUBROUTINE THAT CHANGES PEO SUBMISSION HISTORY RECORDS
      *================================================================
     C     SRPEOS        BEGSR
      *

     c                   select
     c                   when      qqst <> *blanks
     c                   eval      xzst = qqst
     c                   when      qqst = *blanks
     c   59              eval      xzst = 'XX'                                  reports to states
     c  n59              eval      xzst = *blanks                               doesn't rpt to st
     c                   endsl
      *
      *  LOOP THROUGH ENDORSEMENT SUBMISSION HISTORY RECORDS
      *
     C                   MOVE      *OFF          XXPSFL
     C                   MOVE      'E'           XXTYPE
     C                   MOVE      '3'           XXSTAT
     C     SHKEY         CHAIN     PSH18                              60
     C                   Z-ADD     0             SVEMP#
     C                   Z-ADD     0             SVFYR
     C     *IN60         DOWEQ     *OFF
      *
      * IF THERE IS A STATE IN THE ENDORSEMENT ARRAY EQUAL TO THE STATE
      * THAT THE PROGRAM IS RUNNING FOR . . . PROCESS RECORD
      *
     C                   MOVE      *ON           XXLOCK            1
     c                   if        *in59 = *on                                  reporting state
     C                   Z-ADD     1             I
     C     XZST          LOOKUP    SST(I)                                 60
     C     *IN60         IFEQ      *ON
     C     SSS(I)        ANDEQ     '3'
     C                   MOVE      *BLANKS       XXPROC            1
     C     FNPEOS        IFNE      'Y'
     C     FNPEOF        ANDEQ     'M'
     C     FNPEOS        ORNE      'Y'
     C     FNPEOF        ANDEQ     'G'
     C     FNPEOS        ORNE      'Y'
     C     FNPEOF        ANDEQ     'C'
     C     ELKY3         CHAIN     PEL01                              60
     C  N60RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      RSJRST        XXST
     C                   MOVE      ELPFDT        XXEFDT
     C     RRKY          SETGT     PRR01
     C     RRKY1         READPE    PRR01                                  60
     C  N60RRPGFL        IFEQ      '1'
     C     NKEY          SETLL     PNE01
     C     NKEY          READE     PNE01                                  60
     C     *IN60         DOWEQ     *OFF
     C     NETYPE        IFEQ      '88'
     C                   MOVEL     'Y'           XXPROC
     C                   LEAVE
     C                   ENDIF                                                  NETYPE = '88'
     C     NKEY          READE     PNE01                                  60
     C                   ENDDO                                                  *IN60 = *OFF
     C                   ENDIF                                                  RRPGFL = '1'
     C                   ENDIF                                                  *IN60 = *OFF
     C                   ENDIF                                                  *FNEPOS <>'Y'
     C     FNPEOS        IFEQ      'Y'
     C     XXPROC        OREQ      'Y'
      *
      * GET THE POLICY RECORD ON YEAR OR EMPLOYER BREAK
      *
     C     SVEMP#        IFNE      SHEMP#
     C     SVFYR         ORNE      SHFYR
     C                   Z-ADD     SHEMP#        SVEMP#
     C                   Z-ADD     SHFYR         SVFYR
     C     XXPSFL        IFEQ      *ON
     C                   MOVE      PHCO#         QQCO#             3
     C                   MOVE      PHFND         QQFND             3
     C                   MOVE      PHEMP#        QQEMP#            9
     C                   MOVE      PHDIV         QQDIV             5
     C                   MOVE      PHFYR         QQFYR             3
     C                   CALL      'W7413R'      POLPL
     C                   Exsr      CleanWMSHP
     C                   ENDIF
     C                   MOVE      *OFF          XXPSFL            1            POLICY STATUS FLAG
     C                   ENDIF                                                  SVEMP# IFNE SHEMP#
      *
     C                   MOVE      'P'           XXTYPE
     C     SHKYL1        CHAIN     PSHL1                              60
     C     *IN60         IFEQ      *OFF
      *
     C                   Z-ADD     1             IX                2 0
     C     XZST          LOOKUP    PST(IX)                                60
     C     *IN60         IFEQ      *ON
      *
     C     PSS(IX)       IFEQ      '4'
     C     PSS(IX)       OREQ      '3'
     C     PSD(IX)       ANDNE     *ZERO
     C     PSC(IX)       ANDNE     'K'
     C     XXPSFL        OREQ      *ON
     C                   MOVE      '4'           SSS(I)
     C                   MOVE      '6'           PSS(IX)
     C                   MOVE      '6'           PHSTAT
     C                   MOVE      *ON           XXPSFL
     C     ELKY4         Chain     PEL01                              60
     C                   If        *in60 = *off and PHPfdt = ELPfdt
     C                             and PHCode = 'K'
     C                   Move      'I'           PHCode
     C                   Endif
     C*** begin add ***  07/01/25  *********************************************
     C                   Z-ADD     1             DX                2 0
     C     1             DO        15            DX
     C*
     C* Set all state status fields to submitted on endorsement record.
     C     SSS(DX)       IFEQ      '3'
     C     SST(DX)       ANDNE     *blanks
     C                   MOVE      '4'           SSS(DX)
     C                   ENDIF
     C*
     C* Set all state status fields to '6' for policy page to update if
     C* previously submitted and not a key change.
     C     PST(DX)       IFNE      *blanks
     C     PSS(DX)       IFEQ      '4'
     C     PSS(DX)       OREQ      '3'
     C     PSD(DX)       ANDNE     *ZERO
     C     PSC(DX)       ANDNE     'K'
     C                   MOVE      '6'           PSS(DX)
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDDO
     C*** end add *****  07/01/25  *********************************************
     C                   ENDIF                                                  PSS,IX = '4'
      *
      *  IF ALL ENDORSEMENT STATE STATUS RECORDS ARE NOW '4', UPDATE THE SHSTAT
      *  FIELD AS '4'
      *
     C                   MOVE      *ON           XX4FLG            1
     C                   Z-ADD     1             DX                2 0
     C     1             DO        15            DX
     C     SSS(DX)       IFEQ      '3'
     C                   MOVE      *OFF          XX4FLG
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
      *
     C     XX4FLG        IFEQ      *ON
     C                   MOVE      '4'           SHSTAT
     C                   ENDIF
      *
      *
     C                   MOVE      *OFF          XXLOCK
     C                   UPDATE    PSHL1
     C                   UPDATE    PSH18
      *
     C                   ENDIF
     C                   ENDIF                                                  *IN60=*ON
      *
     C                   ENDIF                                                  *IN60=*OFF
      *
     C     XXLOCK        IFEQ      *ON
     C                   UNLOCK    WMSHL1
     C                   ENDIF
      *
     C                   ENDIF                                                  *IN60=*ON
      *
     c                   else                                                   *in59 = *on
     C     SVEMP#        IFNE      SHEMP#
     C     SVFYR         ORNE      SHFYR
     C                   Z-ADD     SHEMP#        SVEMP#
     C                   Z-ADD     SHFYR         SVFYR
     C     XXPSFL        IFEQ      *ON
     C                   MOVE      PHCO#         QQCO#             3
     C                   MOVE      PHFND         QQFND             3
     C                   MOVE      PHEMP#        QQEMP#            9
     C                   MOVE      PHDIV         QQDIV             5
     C                   MOVE      PHFYR         QQFYR             3
     C                   CALL      'W7413R'      POLPL
     C                   ENDIF
     C                   MOVE      *OFF          XXPSFL            1
     C                   ENDIF
      *
      *  If previously sent, allow policy replacement.
      *
     c                   eval      xxtype = 'P'
     c     shkyl1        chain     pshl1
     c                   if        %found
     c                   if        PHSTAT = '4' or
     c                             XXPSFL = *ON or
     c                             PHSTAT = '3' and
     c                             PHCODE <> 'O' and
     c                             PHCODE <> 'R'
     c                   move      '6'           PHSTAT
     c                   move      '4'           SHSTAT
     c                   move      *on           xxpsfl
     c                   move      *off          xxlock
     c                   update    pshl1
     c                   update    psh18
     c                   endif                                                  SHSTAT = '4'
     c                   endif                                                  %found
     c                   endif                                                  *in59 = *on

     C     XXLOCK        IFEQ      *ON
     C                   UNLOCK    WMSHL18
     C                   ENDIF
     C                   UNLOCK    WMSHL1
     c                   move      *OFF          xxlock
      *
     C                   MOVE      'E'           XXTYPE
     C     SHKEY         READE     PSH18                                  60
     C                   ENDDO
      *
     C     XXPSFL        IFEQ      *ON
     C     XXLOCK        IFEQ      *ON
     C                   UNLOCK    WMSHL1
     C                   UNLOCK    WMSHL18
     c                   move      *OFF          xxlock
     C                   ENDIF
     C                   MOVE      PHCO#         QQCO#
     C                   MOVE      PHFND         QQFND
     C                   MOVE      PHEMP#        QQEMP#
     C                   MOVE      PHDIV         QQDIV
     C                   MOVE      PHFYR         QQFYR
     C                   CALL      'W7413R'      POLPL
     C                   MOVE      *OFF          XXPSFL
     C                   Exsr      CleanWMSHP
     C                   ENDIF
     C                   ENDSR                                                  SRPEOS
      *================================================================
      *   CleanWMSHP - Remove bureaus from 'E' records that are no
      *                longer on the policy record after data was
      *                updated from W7413R.
      *================================================================
     C     CleanWMSHP    BEGSR
     C                   Clear                   xxUpdate          1
     C*
      /free
         Chain(n) (PHCo#:PHFnd:PHEmp#:PHFyr:'P') pshl1;
         svt = pst;
         Setll (PHCo#:PHFnd:PHEmp#:PHFyr:'E') pshl1;
         Reade (PHCo#:PHFnd:PHEmp#:PHFyr:'E') pshl1;
         Dow not %eof;

            xxUpdate = ' ';
            For z = 1 to 15;
               If pst(z) <> '  ';
                  index = %lookup(pst(z):svt);
                  If index = 0;
                     Clear pst(z);
                     Clear pss(z);
                     Clear psd(z);
                     Clear pti(z);
                     Clear psc(z);
                     xxUpdate = 'Y';
                  Endif;
               Endif;
            Endfor;

            If xxUpdate = 'Y';
               PHStat = '4';
               For z = 1 to 15;
                  If pst(z) <> '  ' and pss(z) = '3';
                     PHStat = '3';
                     Leave;
                  Endif;
               Endfor;
               Update pshl1;
            Endif;

            Reade (PHCo#:PHFnd:PHEmp#:PHFyr:'E') pshl1;
         Enddo;
      /end-free
     C*
     C                   ENDSR                                                  CleanWMSHP
      *================================================================
      *   SUBROUTINE
      *================================================================
     C     ANNV          BEGSR
     C*
     C     EHKY          SETGT     PEH01
     C     EHKY1         READPE    PEH01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C     EHFAF         ANDEQ     'A'
     C     EHKY1         READPE    PEH01                                  60
     C                   ENDDO
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     EHANDT        ADD       10000         XXANDT            7 0
     C*
     C     EHANDT        IFNE      *ZERO
     C     XXANDT        ANDGT     EHPFDT
     C     XXANDT        ANDLT     EHPTDT
     C                   MOVE      *ON           *IN64
     C                   ENDIF
     C*
     C     EHPFDT        ADD       10000         XXANDT
     C*
     C     XXANDT        IFNE      EHPTDT
     C                   MOVE      *ON           *IN64
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDSR
      *================================================================
      *   SUBROUTINE TO WRITE SUBMISSION HISTORY DETAIL RECORDS.
      *================================================================
     C     SRSHD         BEGSR
     C*
     C                   MOVE      *BLANKS       X1ST              2
     C                   MOVE      SHCODE        X1CODE            1
     C                   Z-ADD     SHSBDT        X1SBDT            7 0
     C                   Z-ADD     SHSBTM        X1SBTM            6 0
     C*
     C* SET STATE SPECIFIC FIELDS.
     C*
     C     *IN59         IFEQ      *ON
     C*
     C                   Z-ADD     1             I
     C     QQST          IFNE      *BLANKS
     C                   MOVE      QQST          XZST              2
     C                   ELSE
     C                   MOVE      'XX'          XZST
     C                   ENDIF
      *
     C     XZST          LOOKUP    SST(I)                                 60
     C     *IN60         IFEQ      *ON
     C                   MOVE      SST(I)        X1ST
     C                   MOVE      SSC(I)        X1CODE
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN59 IFEQ
     C*
     C*  CLEAR AND WRITE RECORD TO WDSHP FILE.
     C*
     C                   CLEAR                   PS101
     C*
     C                   MOVE      SHCO#         S1CO#
     C                   MOVE      SHFND         S1FND
     C                   MOVE      SHEMP#        S1EMP#
     C                   MOVE      SHDIV         S1DIV
     C                   MOVE      SHFYR         S1FYR
     C                   MOVE      SHTYPE        S1TYPE
     C                   MOVE      X1ST          S1ST
     C                   MOVE      X1SBDT        S1SBDT
     C                   MOVE      X1SBTM        S1SBTM
     C                   MOVE      X1CODE        S1CODE
     C*** begin add ***  12/18/24  *********************************************
     C                   If        s1Type = 'P' and polSkip = 'Y'
     C                   MOVE      'C'           S1CODE
     C                   Endif
     C*** end add *****  12/18/24  *********************************************
     C                   MOVE      SHUNID        S1UNID
     C                   MOVE      SHPFDT        S1PFDT
     C                   MOVE      SHAGT#        S1AGT#
     C                   MOVE      SHDOC         S1DOC
     C                   MOVE      SHFOLD        S1FOLD
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         S1DT
     C                   Z-ADD     DSTIME        S1TIME
     C                   MOVEL(P)  Q1PGM         S1PGM
     C                   MOVEL(P)  Q1USER        S1USER
     C                   MOVEL     SRDTO         S1IDT
     C                   Z-ADD     DSTIME        S1ITIM
     C                   MOVEL(P)  Q1PGM         S1IPGM
     C                   MOVEL(P)  Q1USER        S1IUSR
     C*
     C                   WRITE     PS101
     C*
     C                   ENDSR                                                  SUBR SRSHD
      *================================================================
      *   SUBROUTINE TO CHECK PRIOR WXNAP RECORDS FOR DELETION REPORTED.
      *================================================================
     C     SRDEL         BEGSR
     C*
     C                   Z-ADD     DBRRB         SVRRN             9 0
     C                   MOVE      *BLANK        XXNAMD            1
     C                   MOVE      *BLANK        XXADRD            1
     C*** begin add ***  12/17/24  *****************************************
     C                   MOVE      *BLANK        savNAMD           1
     C                   MOVE      *BLANK        savADRD           1
     C*** end add *****  12/17/24  *****************************************
     C                   Clear                   svP02TON
     C                   Clear                   svP03PH#
      /Free
        If S3enCd <> NAenCd and ReportData = *Off;
           S3enCd = NAenCd;
           S3namP = NAnamp;

           // Check if this entity has a client instance
           If Nanamp <> 'C' and NAnamp <> 'D';
              kkCo#  = NAco#;
              kkFnd  = NAfnd;
              kkEmp# = NAemp#;
              kkFyr  = NAfyr;
              kkUnId = NAunid;
              kkNlKc = NAnlkc;
              Setll (kkCo#:kkFnd:kkEmp#:kkFyr:kkUnid:kkNlKc:S3EnCd) PNA03;
              If %equal(wxnal1);
                 Reade (kkCo#:kkFnd:kkEmp#:kkFyr:kkUnid:kkNlKc:S3EnCd) PNA03;
                 Dow not %eof(WXNAL1);
                   If NAnamp = 'C' or NAnamp = 'D';
                      S3namp = NAnamp;
                      Leave;
                   Endif;
                   Reade (kkCo#:kkFnd:kkEmp#:kkFyr:kkUnid:kkNlKc:S3EnCd) PNA03;
                 Enddo;
              Endif;
           Endif;
        Endif;

        //Reset WXNAP Record
          Chain(n) SVRRN PNA02;

      /End-Free

     C                   Eval      newName = 'Y'
     C                   Eval      newAddr = 'Y'
     C                   Move      NAENCD        X1ENCD            3 0
     C                   Move      NANLKC        X1NLKC            2 0
     C                   Move      NAELCD        X1ELCD            5 0
     C                   Move      NAST          X1STC             2
     C                   Z-add     0             svENCD            3 0
     C                   Z-add     0             svnlkc            2 0
     C*
     C* CHECK FOR PREVIOUSLY REPORTED INFORMATION IF PRIOR SUBMISSION EXISTS.
     C*
     C     XXUNID        IFNE      0
     C*
     C                   MOVE      NANAME        X1NAME
     C                   MOVE      NANAM2        X1NAM2
     C                   MOVE      NANAM4        X1NAM4
     C                   MOVE      NAFEIN        X1FEIN
     C                   MOVE      NAMAD1        X1MAD1
     C                   MOVE      NAMAD2        X1MAD2
     C                   MOVE      NAMCTY        X1MCTY
     C                   MOVE      NAMST         X1MST
     C                   MOVE      NAMZIP        X1MZIP
     C                   Z-ADD     NAPFDT        X1PFDT            7 0
     C                   Z-ADD     NAPTDT        X1PTDT            7 0
     C                   Z-ADD     NAPFD2        X1PFD2            7 0
     C                   Z-ADD     NAPTD2        X1PTD2            7 0
     C*
     C     X1PFDT        IFEQ      X1PTDT
     C     X1PFD2        OREQ      X1PTD2
     C*
     C*
     C* Find actual name if record is for address only.
     C*
     C                   If        x1name = ' '
     C*
     C* No need to check for name deletion status later.
     C                   Z-add     *hival        x1ptdt
     C*
     C                   Z-add     naencd        svencd
     C                   Z-add     nanlkc        svnlkc
     C*
     C     nkey          Chain     PNA03                              60
     C     *IN60         Doweq     *OFF
     C*
     C     NAENCD        Ifeq      svencd
     C     NANLKC        Andeq     svnlkc
     C     NANAME        Andne     ' '
     C                   MOVE      NANAME        X1NAME
     C                   MOVE      NANAM2        X1NAM2
     C                   MOVE      NANAM4        X1NAM4
     C                   Leave
     C                   Endif
     C*
     C     nkey          Reade     PNA03                                  60
     C                   Enddo
     C*
     C                   Z-add     0             svencd
     C                   Z-add     0             svnlkc
     C                   Endif
     C                   MOVEA     '00'          *IN(80)
     C*
     C     NAKY          CHAIN     PNA03                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     X1NAME        IFEQ      NANAME
     C     X1NAM2        ANDEQ     NANAM2
     C     X1NAM4        ANDEQ     NANAM4
     C     X1FEIN        ANDEQ     NAFEIN
     C                   Z-add     NAENCD        svENCD
     C                   Z-add     NANLKC        svNLKC
     C                   If        x1ptdt = *hival
     C                   Leave
     C                   Endif
     C                   ENDIF
     C     XXNAMD        IFEQ      *BLANK
     C     X1PFDT        ANDEQ     X1PTDT
     C*
     C     X1NAME        IFEQ      NANAME
     C     X1NAM2        ANDEQ     NANAM2
     C     X1NAM4        ANDEQ     NANAM4
     C     X1FEIN        ANDEQ     NAFEIN
     C*
     C     NANAMD        IFNE      *BLANK
     C                   MOVE      'P'           XXNAMD
     C                   ENDIF
     C                   Leave
     C*
     C                   ENDIF                                                  X1NAME IFEQ
     C*
     C                   ENDIF                                                  XXNAMD IFEQ
     C*
     C*
     C   80
     CAN 81              LEAVE
     C*
     C     NAKY          READE     PNA03                                  60
     C                   ENDDO
     C*
     C*
     C     NAKY          CHAIN     PNA03                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     XXADRD        IFEQ      *BLANK
     C     X1PFD2        ANDEQ     X1PTD2
     C*
     C     X1MAD1        IFEQ      NAMAD1
     C     X1MAD2        ANDEQ     NAMAD2
     C     X1MCTY        ANDEQ     NAMCTY
     C     X1MST         ANDEQ     NAMST
     C     X1MZIP        ANDEQ     NAMZIP
     C     svENCD        ANDEQ     NAENCD
     C     svNLKC        ANDEQ     NANLKC
     C*
     C     NAADRD        IFNE      *BLANK
     C                   MOVE      'P'           XXADRD
     C                   ENDIF
     C                   Leave
     C*
     C                   ENDIF                                                  X1MAD1 IFEQ
     C*
     C                   ENDIF                                                  XXADRD IFEQ
     C*
     C     NAKY          READE     PNA03                                  60
     C                   ENDDO
     C                   ENDIF                                                  XXUNID IFNE 0
     C*
     C*
     C* Check if name/address information has changed since last submission.
     C*
     C     NAKY          CHAIN     PNA03                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C* Name
     C*
     C     X1NAME        IFEQ      NANAME
     C     X1NAM2        ANDEQ     NANAM2
     C     X1NAM4        ANDEQ     NANAM4
     C     X1FEIN        ANDEQ     NAFEIN
     C     X1ENCD        ANDEQ     NAENCD
     C     X1NLKC        ANDEQ     NANLKC
     C*** begin add ***  12/17/24  *************************************
     C     X1PFDT        ANDEQ     NAPFDT
     C     X1PTDT        ANDEQ     NAPTDT
     C*** end add *****  12/17/24  *************************************
     C*
     C                   Clear                   newName
     C*
     C                   MOVE      2             svP02TON
     C     NANAMP        IFEQ      'Y'
     C     NANAMP        OREQ      'Q'
     C     NANAMP        OREQ      'D'
     C                   MOVE      1             svP02TON
     C                   ENDIF                                                  NANAMP IFEQ
     C*
     C                   ENDIF                                                  X1NAME IFEQ
     C*
     C* Address
     C*
     C     X1MAD1        IFEQ      NAMAD1
     C     X1MAD2        ANDEQ     NAMAD2
     C     X1MCTY        ANDEQ     NAMCTY
     C     X1MST         ANDEQ     NAMST
     C     X1MZIP        ANDEQ     NAMZIP
     C     X1ENCD        ANDEQ     NAENCD
     C     X1NLKC        ANDEQ     NANLKC
     C     X1ELCD        ANDEQ     NAELCD
     C     X1STC         ANDEQ     NAST
     C*** begin add ***  12/17/24  *************************************
     C     X1PFD2        ANDEQ     NAPFD2
     C     X1PTD2        ANDEQ     NAPTD2
     C*** end add *****  12/17/24  *************************************
     C*
     C                   Clear                   newAddr
     C*
     C                   MOVEL     NAAREA        svP03PH#
     C                   MOVE      NAPHN#        svP03PH#
     C*
     C                   ENDIF                                                  X1NAME IFEQ
     C*
     C     NAKY          READE     PNA03                                  60
     C                   ENDDO
     C                   ENDIF                                                  X1PFDT IFEQ
     C*
     C* RESET WXNAP RECORD.
     C*
     C     SVRRN         CHAIN(N)  PNA02                              60
     C                   If        QQSt <> *blanks
     C*** begin add ***  12/17/24  *****************************************
     C                   MOVE      xxNAMD        savNAMD
     C                   MOVE      xxADRD        savADRD
     C*** end add *****  12/17/24  *****************************************
     C                   MOVE      *BLANK        XXNAMD
     C                   MOVE      *BLANK        XXADRD
     C                   Endif
     C*
     C                   ENDSR                                                  SUBR SRDEL
     C*** begin add ***  03/08/24  ******************************************
      *================================================================
      *   SUBROUTINE to check for State's UE# to report for non-NCCI Submissions.
      *================================================================
     C     SRUE#         Begsr
     C*
     C                   Z-add     dbrrb         svRRN             9 0
     C                   Clear                   svUE#            15
     C                   Z-add     naencd        svencd
     C                   Z-add     nanlkc        svnlkc
     C*
     C     NAKY1         CHAIN     PNA03                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C* UE# for State being processed is found on record.
     C                   If        NASt1 = svUESt or NASt2 = svUESt or
     C                             NASt3 = svUESt
     C*
     C                   Select
     C                   When      NASt1 = svUESt
     C                   Eval      svUE# = NAUE#
     C                   When      NASt2 = svUESt
     C                   Eval      svUE# = NAUE#2
     C                   When      NASt3 = svUESt
     C                   Eval      svUE# = NAUE#3
     C                   Endsl
     C*
     C                   Leave
     C*
     C                   Endif
     C*
     C     NAKY1         READE     PNA03                                  60
     C                   ENDDO
     C*
     C* RESET WXNAP RECORD.
     C*
     C     svRRN         Chain(N)  PNA02                              60
     C*
     C* Only report the ue# for the state being processed.
     C*
     C                   If        svUE# <> ' '
     C                   Eval      NAUE# = svUE#
     C                   Eval      NASt1 = svUESt
     C                   Clear                   NAUE#2
     C                   Clear                   NAST2
     C                   Clear                   NAUE#3
     C                   Clear                   NAST3
     C                   Endif
     C*
     C                   Endsr                                                  Subr SRUE#
     C*** end add *****  03/08/24  ******************************************
     C     S22304        BEGSR
     C*
     C                   CLEAR                   PHC
     C                   MOVEL(P)  PEDATA        E22304
     C*
     C                   MOVE      ##LNK         PHCLNK                         LINK DATA
     C                   MOVE      PEST          PHCST                          STATE CODE
     C                   MOVE      'HC'          PHCRTP                         RECORD TYPE CODE
     C                   MOVEL     PEEND#        PHCEN#                         ENDORSEMENT NUMBER
     C                   MOVEL(P)  PEEND#        XX9               9
     C                   MOVE      XX9           PHCBVI                         BUREAU VERSION ID
     C                   MOVE      *BLANKS       PHCCVI                         CARRIER VERSION ID
     C                   MOVEL     PENAME        PHCNAM                         NAME OF INSURED
     C                   MOVE      PEEFDT        PHCEED                         ENDORSEMENT EFF DT
     C*
     C     PEDATA        IFNE      *BLANKS
     C                   MOVEL     XHNM1         PHCCNM                         NAME OF CLIENT
     C                   MOVEL     XHADT         PHCLAC                         LEAS ADDR TYPE CD
     C                   MOVEL     XHAD1         PHCADD                         ADDRESS - STREET
     C                   MOVEL     XHCTY         PHCCTY                         ADDRESS - CITY
     C                   MOVEL     XHST          PHCST1                         ADDRESS - STATE
     C                   MOVEL     XHZIP         PHCZIP                         ADDRESS - ZIP CODE
     C                   MOVEL     XHFEI         PHCFEI                         CLIENT FEIN
     C                   MOVEL     XHUIN         PHCUIN                         UNEMPLOYMENT INS#
     C                   ENDIF                                                  PEDATA <> ' '
     C*
     C                   CLEAR                   RDATA
     C**** WRITE RECORD
     C*
     C                   MOVEL     PHC           RDATA
     C                   WRITE     RPOLS
     C                   ADD       1             ##TOTR           10 0
     C*
     C                   ENDSR                                                  S22304 SUBR

      ****************************************************************
      ***  Convert Date
      ****************************************************************
     C     CnvDate       Begsr
     c                   Z-Add     DSdate        SRDTI
     c                   Call      'SRCVTDAT'    DTCYMD
     c                   MoveL     SRDTO         x2DATE
     c                   Z-Add     DStime        x2TIME
     c                   Endsr
      ****************************************************************

      ****************************************************************
      ***  Read through the state premium records for a prior submission
      ***  to determine if any states are going to be added or deleted on
      ***  the resubmission.
      ****************************************************************
      /free
       Begsr SRstate;
          Clear svP01LNI;
          Clear svP01TON;
          Clear svP01NOP;
          Clear svaP04EMS;
          Clear svaNHEXM1;
          Clear svaNHEXM2;
          Clear svaNHEXM3;
          Clear svaNHEMD1;
          Clear svaNHEMD2;
          Clear svaNHEMD3;
          Clear svaNHANDT;
          counter1 = 1;
          counter2 = 1;
          xxUNID   = 0;
          x2TYPE = 'P';
          If *in59 = *On;
             If QQst <> ' ';
                x2ST = QQst;
             Else;
                x2ST = 'XX';
             Endif;
          Else;
                x2ST = '  ';
          Endif;
          Exsr CnvDate;

       // xxCODE is equal to 'R' then subtract 1 from SHFYR to check the prior policy year
          If xxCode = 'R';
             SHfyr = SHfyr - 1;
          Endif;

          Setgt  S1KY  PS101;
          Readpe S1KY1 PS101;
          If not %eof(WDSHL1);
             xxUNID = S1unid;
          Endif;
          If xxUNID <> 0;
             Setll NAKY PNH01;
             ReadE NAKY PNH01;
             Dow not %eof(WXNHL);
                If  NHst = '**';
                   xxTBID = 'BUSI';
                   xxKey = NHTPOB;

                   Chain(n) gtky pgt01;
                   If %found and GTN501 >= 1 and GTN501 <= 98;
                      svP01LNI = GTN501;
                   Else;
                      svP01LNI = 99;
                      Chain(n) emkey pem01;
                      If %found;
                         svP01TON = %trim(EMNAOB);
                      Endif;
                   Endif;

                   svP01NOP = %trim(NHPLSN);
                Endif;
       // Store the states into the prior state array as long as the state field is not equal to
       // '**'.
                If  NHst <> '**';
                    priorState(counter1) = NHst;
                    svaNHEXM1(counter1) = NHEXM1;
                    svaNHEXM2(counter1) = NHEXM2;
                    svaNHEXM3(counter1) = NHEXM3;
                    svaNHEMD1(counter1) = NHEMD1;
                    svaNHEMD2(counter1) = NHEMD2;
                    svaNHEMD3(counter1) = NHEMD3;
                    svaNHANDT(counter1) = NHANDT;
                    Select;
                      When NHEXM1 = 0 or NHEXT1 = '3';
                         svaP04EMS(counter1) = 3;
                      When NHexmd1nf = 'Y';
                         svaP04EMS(counter1) = 2;
                      Other;
                         svaP04EMS(counter1) = 1;
                    Endsl;
                    If  NHsadd = 'D' or NHsadd = 'P';
                        deletionFLAG(counter1) = 'Y';
                    Else;
                        deletionFLAG(counter1) = ' ';
                    Endif;
                    counter1 = counter1 + 1;
                Endif;
                If counter1 > 51;
       //*** End change 04/16/15 *********************************************//
                   Leave;
                Endif;
                ReadE NAKY PNH01;
             Enddo;

       // This flag will be used to later check if states need
       // to be flagged with an Add if they are new on a renewal policy.
             If xxCODE = 'R';
                xxRENW = 'Y';
             Endif;

       // If XXCODE is equal to 'R' then add 1 back into SHFYR.
          IF xxCODE = 'R';
             Shfyr = SHfyr + 1;
          Endif;

       // Read through WXNHP, ignoring the '**' record.
             Setll NKEY PNH01;
             ReadE NKEY PNH01;
             Dow not %eof(WXNHL);
                 If  NHst <> '**';
                    If NHSt = QQSt and QQSt <> '  ' and NHPcdt = NHPfdt;
                       flatDel = 'Y';
                    Endif;
                     currentState(counter2) = NHst;
                     index = %LookUp(NHst:priorState);
                     If (%LookUp(NHst:priorState) = 0) Or
                        (%LookUp(NHst:priorState) <> 0) and
                        ((NHpcdt <> NHxpc and NHXPC <> 0) or NHXPC = 0) and
                        deletionFLAG(index) = 'Y';
                        currentSTATUS(counter2) = 'A';
                        If xxCode = 'I' or xxCode = 'T' or xxCode = 'N';
                           xxTR15 = 'Y';
                        Endif;
                     Else;
                        If NHpcdt = NHxpc and NHpcdt <> 0;
                           If deletionFLAG(index) <> 'Y';
                              currentSTATUS(counter2) = 'D';
                              If flgGenCX = 'Y';
                                 svDate = NHpcdt;
                              Endif;
                              If xxCode = 'I' or xxCode = 'T' or xxCode = 'N';
                                 xxTR15 = 'Y';
                              Endif;
                           Else;
                              currentSTATUS(counter2) = 'P';
                           Endif;
                        Endif;
                     Endif;
                     counter2 = counter2 + 1;
                 Endif;
                 If counter2 > 51;
                    Leave;
                 Endif;
                 ReadE NKEY PNH01;
             Enddo;

          Else;

             IF xxCODE = 'R';
                Shfyr = SHfyr + 1;
             Endif;

             Setll NKEY PNH01;
             ReadE NKEY PNH01;
             Dow not %eof(WXNHL);
                 xxTBID = 'NCCI';
                 xxKey = NHSt;
                 Chain(n) gtky pgt01;

                 If NHst = QQSt or (QQSt = '  ' and %found and NHSt <> '**');
                     currentState(counter2) = NHst;
                     If xxCode = 'A';
                        currentSTATUS(counter2) = 'A';
                        xxTR15 = 'Y';
                     Endif;
                     counter2 = counter2 + 1;
                 Endif;
                 If counter2 > 51;
                    Leave;
                 Endif;
                 ReadE NKEY PNH01;
             Enddo;

          Endif;

       Endsr;
       //**********************************************************************
       //*
       //* Read prior exposure record to determine if any changes have occurred.
       //*
       //**********************************************************************
       Begsr SRExpo;

          svRRN = dbrrb2;
          newExpo = 'Y';

          x5St   = NXSt;
          x5Clas = NXClas;
          x5Rt   = NXRt;
          x5Efdt = NXEfdt;
          x5Expp = NXExpp;
          x5Desc = NXDesc;
          x5Csuf = NXCsuf;
          //*** begin add ***  07/28/25  ********************************
          x5Eapr = NXEapr;
          //*** end add *****  07/28/25  ********************************

          Setll naky pnx01;
          Reade(n) naky pnx01;
          Dow not %eof;

             If NXSt = x5St and NXClas = x5Clas and NXRt = x5Rt and
              NXEfdt = x5Efdt and NXExpp = x5Expp and NXDesc = x5Desc
          //*** begin chg ***  07/28/25  ********************************
          //*****    and NXCsuf = x5Csuf;
              and NXCsuf = x5Csuf and NXEapr = x5Eapr;
          //*** end chg *****  07/28/25  ********************************
                newExpo = ' ';
                Leave;
             Endif;

             Reade(n) naky pnx01;
          Enddo;

          Chain(n) svRRN pnx02p;
       Endsr;
       //**********************************************************************
       //*
       //* Read prior exposure record to determine if any changes have occurred.
       //*
       //**********************************************************************
       Begsr SREndorse;
          newEndr = ' ';

          If xxUNID = 0;
             newEndr = 'Y';
             Leavesr;
          Endif;

          Setll muky1 pmu01;
          Reade(n) muky1 pmu01;
          Dow not %eof;

             xxForm = MUForm;
             Chain(n) xuky xmu00;
             If not %found;
                newEndr = 'Y';
                Leavesr;
             Endif;

             Reade(n) muky1 pmu01;
          Enddo;

          Setll xuky1 xmu00;
          Reade(n) xuky1 xmu00;
          Dow not %eof;

             xxForm = XUForm;
             Chain(n) muky pmu01;
             If not %found;
                newEndr = 'Y';
                Leavesr;
             Endif;

             Reade(n) xuky1 xmu00;
          Enddo;

       Endsr;
       //**********************************************************************
      /end-free
      ******************************************************************
      *** Correct Data for S37310 (As per latest WCIO BI Record Layout)
      ******************************************************************
     c     CorData       Begsr
      *
     c                   Movel(P)  W37310        XXform
     c     PEky1         chain     ppe01                              60
      *
     c     *in60         Ifeq      *On
     c                   Movel(P)  W37311        XXform
     c     PEky1         chain     ppe01                              60
     c                   Endif
      *
     c     *in60         Ifeq      *Off
     c                   Movel(P)  PEdata        E37310
      *
     c                   Monitor
     c                   Eval      XXpos1=%DEC(XXpo1:5:2)
     c                   On-error
     c                   Eval      ##co# = %EditC(SHco#:'X')
     c                   Eval      ##fnd = %EditC(SHfnd:'X')
     c                   Eval      ##emp#= %Editc(SHEmp#:'X')
     c                   Eval      ##div = %EditC(SHdiv:'X')
     c                   Eval      ##Fyr = %EditC(SHFyr:'X')
     c                   Eval      ##Mode='2'
     c                   Eval      ##Rrn = 0
     c                   Eval      ##Mod2= *blank
      *
     c                   Select
     c                   When      PEEnd# = 'WC370310B'
     c                   Eval      EndrPgm= 'SR37310'
     c                   When      PEEnd# = 'WC370310C'
     c                   Eval      EndrPgm= 'SR37310C'
     c                   Endsl
      *
      **** Call Endorsement Program to correct data in WXPEP *****
      *
     c                   Call      EndrPgm
     c                   Parm                    ##co#
     c                   Parm                    ##fnd
     c                   Parm                    ##emp#
     c                   Parm                    ##div
     c                   Parm                    ##Fyr
     c                   Parm                    ##Mode
     c                   Parm                    ##Rrn
     c                   Parm                    ##Mod2
     c
     c                   Endmon
      *
     c                   Endif
      *
     c                   Endsr
     C*****************************************************************
     C*
     C* SUBROUTINE TO DETERMINE IF A DATE RANGE CONTAINS FEBRUARY 29TH.
     C*
     C     SRLEAP        BEGSR
     C*
     C*  IF FEB 29 IS A VALID DATE AND IS IN THE RANGE, SUBTRACT ONE DAY
     C*
     C                   Z-ADD     0             XXLPDT            7 0
     C*
     C                   Z-ADD     XQFROM        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1FROM            7 0
     C*
     C                   Z-ADD     XQTO          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1TO              7 0
     C*
     C                   Z-ADD     X1FROM        XXLPDT
     C                   MOVE      '0229'        XXLPDT
     C*
     C     XXLPDT        IFLT      X1FROM
     C                   Z-ADD     X1TO          XXLPDT
     C                   MOVE      '0229'        XXLPDT
     C                   END
     C*
     C     XXLPDT        IFLT      X1FROM
     C     XXLPDT        ORGT      X1TO
     C                   Z-ADD     *ZERO         XXLPDT
     C                   END
     C*
     C     XXLPDT        IFNE      *ZERO
     C*
     C                   Z-ADD     XXLPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM
     C*
     C                   Z-ADD     XQFROM        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XQFROM
     C*
     C     SRERR         IFNE      '1'
     C     XXLPDT        ANDNE     X1FROM
     C                   SUB       1             QQDAYS
     C                   ELSE
     C                   Z-ADD     0             XXLPDT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR                                                  SRLEAP
     C*
     C*****************************************************************
     C*** begin add ***  11/19/21  *******************************************
     C*****************************************************************
     C*
     C* Determine if all endorsement records for a policy have the WC890600*
     C* form attached, and if policy has not been resubmitted then populate
     C* SHReq# field to indicate transaction 14 is valid (for CA policies).
     C*
     C     ChkEndorse    Begsr
      /Free
       //* Only process this coding if using state based submissions and
       //* submitting the data to CA.
         If *in59 = *off;
            Leavesr;
         Else;
            Select;
            When QQST = 'CA';
            Other;
               Leavesr;
            Endsl;
         Endif;
       //*** begin add ***  03/10/23  ******************************************
       //* Only perform this processing for specific clients.
         Select;
         When xxClt# = '148';
         Other;
            Leavesr;
         Endsl;
       //*** end add *****  03/10/23  ******************************************

       //* Check all pending endoresment records for group.
         xxStat = '3';
         xxType = 'E';

         Setll shkey psh18;
         Reade(n) shkey psh18;

         If not %Eof (wmshl18);
            svEmp# = SHEmp#;
            svFyr = SHFyr;
         Endif;
         xxProc = ' ';

         Dow not %eof;
         //* Check policy submission status if changing employers or years.
            If svEmp# <> SHEmp# or svFyr <> SHFyr;

            //* If all 'E' records found a WC890600* form in WMMUP.
               If xxProc = 'Y';
                  Chain(n) (xxCo#:xxFnd:SVEmp#:SVFyr:'P') pshl1;
                  If %found;
                     i = %Lookup(QQST:PST);

                  //* If policy has not been resubmitted/pending a policy submission
                  //* update to indicate all requests have WC890600* form.
                     If i <> 0 and PSS(i) = '4';
                        Chain (xxCo#:xxFnd:SVEmp#:SVFyr:'P') pshl1;
                        PHReq# = 1;
                        Update pshl1;
                     Endif;
                  Endif;

               Endif;

            //* Reset for next policy.
               svEmp# = SHEmp#;
               svFyr = SHFyr;
               xxProc = ' ';
            Endif;

         //* If there is an 'E' record pending for policy, check for attached forms.
            i = %Lookup(QQST:SST);

            If i <> 0 and SSS(i) = '3';

               Setll (SHCo#:SHFnd:SHFyr:SHEmp#:SHReq#) pmu06;
               Reade (SHCo#:SHFnd:SHFyr:SHEmp#:SHReq#) pmu06;

               Dow not %eof;

               //* WC890600* form found for request.
                  If %subst(MUForm:1:8) = 'WC890600' and xxProc <> 'N';
                     xxProc = 'Y';
                     Leave;
                  Endif;
                  Reade (SHCo#:SHFnd:SHFyr:SHEmp#:SHReq#) pmu06;
               Enddo;

            //* No WC890600* forms found for request.
               If xxProc <> 'Y' or %eof;
                  xxProc = 'N';
               Endif;

            Endif;

            Reade(n) shkey psh18;
         Enddo;

       //* Process last policy that had an endorsement record and has form.
         If xxProc = 'Y';

            Chain(n) (xxCo#:xxFnd:SVEmp#:SVFyr:'P') pshl1;
            If %found;
               i = %Lookup(QQST:PST);

            //* If policy has not been resubmitted/pending a policy submission
            //* update to indicate all requests have WC890600* form.
               If i <> 0 and PSS(i) = '4';
                  Chain (xxCo#:xxFnd:SVEmp#:SVFyr:'P') pshl1;
                  PHReq# = 1;
                  Update pshl1;
               Endif;
            Endif;

         Endif;

      /End-Free
     C                   Endsr                                                  ChkEndorse
     C*****************************************************************
     OQSYSPRT   E            ROUTE          1 06
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       ORDIS1              67
     O          E            ROUTE          1
     O                       ORDIS2              60
     O          E            ROUTE          1
     O                       ORDIS3              60
     O          E            ROUTE          2
     O                       ORDIS4              60
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
     O*
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
     O*
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       companyName         87
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                       Q1PGM               10
     O*
     O*
     O                       ORTITL              92
     O*
     O*
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O*
     O          E            HEAD           2
     O                                           45 '  Policy              '
     O                                           67 '                      '
     O                                           89 '                      '
     O          E            HEAD           1
     O                                           45 ' Effective Transaction'
     O                                           67 '         Transaction  '
     O                                           89 '                      '
     O          E            HEAD           2
     O                                           23 'Group  Employer#  Year'
     O                                           45 '   Date       Date    '
     O                                           67 '  State     Type      '
     O                                           89 '                      '
     O                                          111 '                      '
     O                                          132 '                     '
     O*
     O          E            DET            1
     O                       group#        Z     05
     O                       employer#     Z     17
     O                       policyYear    Z     23
     O                       polEffDate          33 '  /  /    '
     O                       subTranDate         45 '  /  /    '
     O                       state               51
     O                       subType             59
     O*
     O          E            TOT         1  1
     O                                           15 'Policy Totals:'            New Policy
     O*
     O          E            TOT         1  1
     O                                           13 '    New     '              New Policy
     O                                           27 '  Renewal   '              Renew Policy
     O                                           41 ' Cancelled  '              Cx Policy
     O                                           55 ' Reinstated '              Rein Policy
     O                                           69 'Endorsements'
     O                                           83 ' Total Hdrs '
     O                                           97 ' Total Rcds '
     O*
     O          E            TOT            1
     O                       ##TLNP        Z     11                             New Policy
     O                       ##TLRP        Z     25                             Renew Policy
     O                       ##TLCX        Z     39                             Cx Policy
     O                       ##TLRE        Z     53                             Rein Policy
     O                       ##TLEN        Z     67
     O                       ##TOTH        Z     81
     O                       ##TOTR        Z     95
     O*
     O*  SELECTION CRITERIA NOT FOUND
     O*
     O          E            NOREC
     O                                           19 '*** NO RECORDS MEET'
     O                                           42 'SELECTION CRITERIA ***'

     P*--------------------------------------------------
     P* Procedure name: PrintDetail
     P* Purpose:        Prints detail entry of the submission record   sion...
     P*--------------------------------------------------
     P PrintDetail     B
     D PrintDetail     PI
     D group#In                            Like(SHfnd)
     D employer#In                         Like(SHemp#)
     D policyYearIn                        Like(SHfyr)
     D polEffDate7In                       Like(SHdt)
     D subTranDate7In                      Like(SHdt)
     D stateIn                             Like(SHst01)
     D subTypeIn                           Like(SHtype)

      /Free
       group#     = group#IN;
       employer#  = employer#IN;
       policyYear = policyYearIN;

     C                   Z-Add     polEffDate7IN srdti
     C                   Call      'SRCVTDAT'    DTMDYY
     C                   MoveL     srdto         polEffDate

     C                   Z-Add     subTranDate7INSRDTI
     C                   Call      'SRCVTDAT'    DTMDYY
     C                   MoveL     srdto         subTranDate

       state      = stateIN;
       subType    = subTypeIN;

       // Write to detail of report
       If *inOA = *On;
          Except Head;
          *inOA = *Off;
       EndIf;
       Except det;
      /End-Free

     P PrintDetail     E
      //*** End Add 02/17/21 ***********************************************************
     P*--------------------------------------------------
     P* Procedure name: WrtStateCxRcd
     P* Purpose:        Write the WI cancelletion record when a WI division...
     P*                 is deleted.
     P*--------------------------------------------------
     P WrtStateCxRcd   B
     D WrtStateCxRcd   PI

      /Free
         //   Move fields
         Clear PSH0P;

         P0shfnd  =  SHfnd ;
         P0shemp# =  SHemp#;
         P0shco#  =  SHco# ;
         P0shdiv  =  SHdiv ;
         P0shfyr  =  SHfyr ;
         P0shunid =  SHunid;
         P0shtrdt =  SHtrdt;
         P0shsbdt =  SHsbdt;
         P0shtype = 'C';
         P0shcode = 'O';
         p0shstat = '3';
         P0shseq# =  SHseq#;
         P0shtime =  SHtime;
         P0shreq# =  SHreq#;
         P0shuser =  SHuser;
         P0shpgm  =  SHpgm ;
         P0shdt   =  svDate;
         P0shiusr =  SHiusr;
         P0shidt  =  SHidt ;
         P0shpfdt =  SHpfdt;
         P0shagt# =  SHagt#;
         P0shdoc  =  SHdoc ;
         P0shfold =  SHfold;
         P0shstep =  SHstep;
         p0shst01 = QQst;
         P0shss01 = '3';
         P0shst02 = '';
         P0shss02 = '';
         P0shst03 = '';
         P0shss03 = '';
         P0shst04 = '';
         P0shss04 = '';
         P0shst05 = '';
         P0shss05 = '';
         P0shst06 = '';
         P0shss06 = '';
         P0shst07 = '';
         P0shss07 = '';
         P0shst08 = '';
         P0shss08 = '';
         P0shst09 = '';
         P0shss09 = '';
         P0shst10 = '';
         P0shss10 = '';
         P0shst11 = '';
         P0shss11 = '';
         P0shst12 = '';
         P0shss12 = '';
         P0shst13 = '';
         P0shss13 = '';
         P0shst14 = '';
         P0shss14 = '';
         P0shst15 = '';
         P0shss15 = '';
         P0shsbtm =  SHsbtm;
         P0shcd01 =  SHcd01;
         P0shcd02 =  SHcd02;
         P0shcd03 =  SHcd03;
         P0shcd04 =  SHcd04;
         P0shcd05 =  SHcd05;
         P0shcd06 =  SHcd06;
         P0shcd07 =  SHcd07;
         P0shcd08 =  SHcd08;
         P0shcd10 =  SHcd10;
         P0shcd11 =  SHcd11;
         P0shcd12 =  SHcd12;
         P0shcd13 =  SHcd13;
         P0shcd14 =  SHcd14;
         P0shcd15 =  SHcd15;
         P0shst = QQst;

         Write PSH0P;

      /End-Free
     P WrtStateCxRcd   E
