     H*                                                               *
     H* 12/05/22 C:291 Project: 153WCA                                *
     H*                Add processing to electronically report Large  *
     H*                deductible endorsement forms for Vital services*
     H*                                                               *
     H* 11/15/22 C:291 Project: WC2891                                *
     H*                Electonically report WC000405.                 *
     H*                                                               *
     H* 11/14/22 C:079 Project: 153WAB                                *
     H*                Add printing of addresses for named insured    *
     H*                schedule.                                      *
     H*                                                               *
     H* 10/25/22 C:344 Project: 103957                                *
     H*                Add endorsement WC480322                       *
     H*                                                               *
     H* 10/17/22 C:291 Project: 140WBZ(UBIC)                          *
     H*                Allow deposit to be suppressed on Dec Page.    *
     H*                                                               *
     H* 10/14/22 C:65  Project: Phw#103927WC2814 (Sunz)               *
     H*                Add electronic reporting to WC480314A and      *
     H*                WC480315A endorsements.                        *
     H*                                                               *
     H* 10/04/22 C:65  Project: Phw#103827 (Amwins)                   *
     H*                Do not include flat cancelled states in item   *
     H*                3A for printing.                               *
     H*                                                               *
     H* 09/26/22 C:65  Project: Phw#103391 (Amwins)                   *
     H*                Consider additional locations on the PEO to be *
     H*                part of the PEO instead of clients.            *
     H*                                                               *
     H* 09/21/22 C:291 Project: 150WFT                                *
     H*                Add WC990403 and WC992003                      *
     H*                                                               *
     H* 07/13/22 C:79  Project: WC2872                                *
     H*                MT 25-2 endorsement processing                 *
     H*                                                               *
     H* 07/06/22 C:79  Project: WC2870                                *
     H*                Electonically report WC290605                  *
     H*                                                               *
     H* 06/28/22 C:65  Project: Phw#102526 (Sunz)                     *
     H*                If expense constant is set to be billed to     *
     H*                state it originates from, pull the state from  *
     H*                employer detail directly.                      *
     H*                                                               *
     H* 05/19/22 C:65  Project: Phw#102232 (Amwins)                   *
     H*                Change the reporting of client names for CA to *
     H*                include the PEO acronym on all names, not just *
     H*                those that match the primary client name.      *
     H*                                                               *
     H* 03/17/22 C:65  Project: 148WCR (Amwins) Phw#97504             *
     H*                Populate two new location fields in workfile   *
     H*                for additional locations and clear loc/ent     *
     H*                type for additional named insured.             *
     H*                                                               *
     H* 02/22/22 C:62  Project: 150WFN                                *
     H*                Use new GetNameAddress.                        *
     H*                                                               *
     H* 02/28/22 C:65  Project: Phw#99824 (Amwins)                    *
     H*                Remove the overriding of type of business for  *
     H*                CA - the base template will be updated to      *
     H*                check boxes based on table descriptions.       *
     H*                                                               *
     H* 02/17/22 C:62  Project: WC2846                                *
     H*                Electronic Report for WC470602                 *
     H*                                                               *
     H* 02/09/22 C:62  Project: 150WFQ                                *
     H*                Add WC995203 and WC990678                      *
     H*                                                               *
     H* 01/26/22 C:65  Project: WC2840                                *
     H*                Default the email from state reporting div     *
     H*                if one is not entered for division/location.   *
     H*                                                               *
     H* 01/12/22 C:65  Project: Phw#97504 (Amwins)                    *
     H*                Change name reporting to write all address     *
     H*                records to the federal name instead of dba.    *
     H*                                                               *
     H* 01/06/22 C:65  Project: Phw#94674  (Sunz)                     *
     H*                Restore original submission code for states    *
     H*                that have not submitted for non-rating         *
     H*                resubmissions.                                 *
     H*                                                               *
     H* 12/22/21 C:79  Project: WC2840                                *
     H*                Add email address logic                        *
     H*                                                               *
     H* 12/20/21 C:65  Project: Phw#100021 (Sunz)                     *
     H*                Allow all clients to print/report dec page for *
     H*                policies with CA coverage.                     *
     H*                                                               *
     H* 11/30/21 C:65  Project: Phw#96615 (Amwins)                    *
     H*                Ensure edition dates for dec page/schedules    *
     H*                are populated for templates.                   *
     H*                                                               *
     H* 11/29/21 C:65  Project: Phw#98391 (Amwins)                    *
     H*                Change Starstone specific coding from 35073    *
     H*                to 38073 carrier #.                            *
     H*                                                               *
     H* 11/22/21 C:65  Project: Phw#96615 (Amwins)                    *
     H*                Print/report the dec page and schedules as     *
     H*                long as CA is on the policy.                   *
     H*                                                               *
     H* 11/16/21 C:65  Project: 148WCY    (Amwins)                    *
     H*                Update to trigger electronic reporting records *
     H*                for Amwins specific versions of WC000406.      *
     H*                                                               *
     H* 11/15/21 C:65  Project: Phw#97400 (Amwins)                    *
     H*                Change the processing which pulls template     *
     H*                setup to use the policy's start date instead   *
     H*                of using system date as which form gets used   *
     H*                is generally a compliance issue for policy     *
     H*                pages.                                         *
     H*                                                               *
     H* 10/11/21 C:65  Project: Phw#98054 (Sunz)                      *
     H*                Check approved/expired dates on processing     *
     H*                large deductible endorsements for reporting.   *
     H*                                                               *
     H* 10/05/21 C:65  Project: Phw#97736 (Amwins)                    *
     H*                Turn on reissue flag if called from mid term   *
     H*                endorsements.                                  *
     H*                                                               *
     H* 09/28/21 C:65  Project: Phw#98032 (Sunz)  Update-95518        *
     H*                Change the CX date checks that were added to   *
     H*                ignore the CX date if its equal to the state   *
     H*                reporting division policy expiration date.     *
     H*                Non-renewal cancellations do not affect        *
     H*                reporting data.                                *
     H*                                                               *
     H* 09/16/21 C:65  Project: 148WDC    (Amwins)                    *
     H*                Leasing ID '8' and '9' are valid for reporting.*
     H*                Populate state/national form into wmmup for    *
     H*                dec page/schedules using a client form.  For   *
     H*                Amwins only, override CA leasing ID of '5' to  *
     H*                '4' if policy is effective prior to 7/1/21.    *
     H*                                                               *
     H* 08/09/21 C:65  Project: Phw#97169 (Amwins)                    *
     H*                Remove PEO output control type '2' hard coding *
     H*                override for Amwins.                           *
     H*                                                               *
     H* 07/14/21 C:65  Project: Phw#91687 (Sunz)                      *
     H*                Store premium in LDA for endorsements to pull  *
     H*                via the SRPAMT program.                        *
     H*                                                               *
     H* 07/09/21 C:65  Project: 148WCA                                *
     H*                Ensure aircraft surcharge stat code does not   *
     H*                print for the stop gap premium line item.      *
     H*                                                               *
     H* 06/28/21 C:65  Project: Phw#95518 (Luba)                      *
     H*                Add missing bureaus on a WCPOLS resubmission.  *
     H*                                                               *
     H* 06/17/21 C:180 Project: 148WCN (AMwins) Disabled in V21R1     *
     H*                Add signature to form 400 for Amwins and       *
     H*                ability for Amwins to re-generate the form.    *
     H*                                                               *
     H* 06/09/21 C:65  Project: 148WCR (Amwins)                       *
     H*                Populate two new location fields in workfile.  *
     H*                                                               *
     H* 06/03/21 C:65  Project: Phw#95518 (Luba)                      *
     H*                If a division has a CX date, then check cx     *
     H*                detail record for when it was written. Compare *
     H*                this date to the last submission done to the   *
     H*                state(bureau) the division is written under to *
     H*                determine if it was already reported and skip. *
     H*                                                               *
     H* 04/08/21 C:65  Project: Phw#95285 (Sunz)                      *
     H*                Trigger electronic records for any WC990603*   *
     H*                deductible endorsement for Sunz.               *
     H*                                                               *
     H* 03/16/21 C:65  Project: Phw#93467 (Sunz)                      *
     H*                Trigger electronic records for three TX deduc  *
     H*                endorsements.                                  *
     H*                                                               *
     H* 03/05/21 C:65  Project: Phw#94090 (Sunz)                      *
     H*                Only trigger name concatenation if NJ is the   *
     H*                only state on the policys across states.       *
     H*                                                               *
     H* 02/11/21 C:136 Project: WC2757                                *
     H*                Change logical to WMMUL7 to run over all       *
     H*                divisions that might have a policy page        *
     H*                endorsement (end# = 0).                        *
     H*                                                               *
     H* 02/05/21 C:65  Project: Phw#94364 (THE)                       *
     H*                Accumulate increased limits before minimum     *
     H*                amounts into total fields across states. Those *
     H*                fields instead will be used to indicate balance*
     H*                to increased limits minimum should be invoked. *
     H*                                                               *
     H* 01/29/21 C:65  Project: WC2734/Phw#87703 (CareWest)           *
     H*                Ensure Split Modifier shows if second modifier *
     H*                is populated, regardless if first is there.    *
     H*                                                               *
     H* 01/15/21 C:65  Support: 90900 (LUBA)                          *
     H*                If an address is deleted, ensure it stays      *
     H*                deleted when updating other records for the    *
     H*                same name/fein.                                *
     H*                                                               *
     H* 12/22/20 C:65  Project: Phw#93373(Sunz)/#93593(Amwins)        *
     H*                When adding the schedules and other forms to   *
     H*                Scriptura work file clear out the optional     *
     H*                form # field from endorsement table since its  *
     H*                value is carried over from last endorsement.   *
     H*                                                               *
     H* 12/25/20 C:65  Project: SY371                                 *
     H*                No need to set darts/remarkcode, those values  *
     H*                pull from prompt screen by STLETTER.           *
     H*                                                               *
     H* 11/23/20 C:65  Project: 148WCA                                *
     H*                Display Stop Gap Premium if client 148 and     *
     H*                the state is flagged as monopolistic and there *
     H*                is an additional load amount.                  *
     H*                                                               *
     H* 10/23/20 C:180 Project: WC2752                                *
     H*                Add endorsement WC480314                       *
     H*                                                               *
     H* 10/07/20 C:65  Project: Phw#90671 (THE)                       *
     H*                Add increased limits to minimum premium if     *
     H*                they have been billed to an addl. division.    *
     H*                                                               *
     H* 09/24/20 C:062 Project: 150WEW                                *
     H*                -Remove the following endorsements             *
     H*                 WC990616                                      *
     H*                 WC990617                                      *
     H*                 WC990704                                      *
     H*                 WC990704                                      *
     H*                -Add WC995402 to sunzLDForms                   *
     H*                -Add the following endorsements                *
     H*                 WC994103A                                     *
     H*                 WC995402                                      *
     H*                                                               *
     H* 09/24/20 C:65  Support: 91649 (Amwins)                        *
     H*                Only call W7413RDL for cover letters if doing  *
     H*                spool file output.                             *
     H*                                                               *
     H* 08/27/20 C:136 Project: WC2608 (not commented)                *
     H*                Add WC040302D and WC040303C to electronic      *
     H*                reporting logic                                *
     H*                                                               *
     H* 08/20/20 C:65  Support: 90449 (Sunz)                          *
     H*                Pull mods for WXNHP from earlier division -    *
     H*                instead of overwriting on each division.       *
     H*                                                               *
     H* 08/13/20 C:65  Project: 150WEV                                *
     H*                Add fields for all states assessment total and *
     H*                policy cost (total prem + total assessments)   *
     H*                to XML work file.                              *
     H*                                                               *
     H* 08/05/20 C:65  Project: WC2756                                *
     H*                Flag payroll classes with 00 exposure act if   *
     H*                it has been specified on the payroll table.    *
     H*                                                               *
     H* 07/30/20 C:65  Support: 90082 (Sunz)                          *
     H*                Allow merit rating to be split rated if        *
     H*                modifier changes during policy.                *
     H*                                                               *
     H* 07/15/20 C:65  Support: 90082 (Sunz) - WC2206                 *
     H*                Apply 1st period not rated coding to merit     *
     H*                rating exposure record - report 2nd modifier   *
     H*                and effective date.                            *
     H*                                                               *
     H* 07/08/20 C:65  Project: Phw# 90025 (Sunz) Ref - #83127        *
     H*                Fix coding to clear out unneeded bureaus to    *
     H*                reset index before each look up.               *
     H*                                                               *
     H* 06/16/20 C:65  Support: 150-88137                             *
     H*                Add logic for form WC200304D.                  *
     H*                                                               *
     H* 06/10/20 C:136 Project: WC2734                                *
     H*                Show first mod as blank if not rated.          *
     H*                                                               *
     H* 04/28/20 C:136 Support: 81232                                 *
     H*                Correct position of ldAddNamEd in svLDA        *
     H*                data structure.                                *
     H*                                                               *
     H* 03/30/20 C:65  Support: 85869 (Sunz)                          *
     H*                If policy has NJ and MI coverage then do not   *
     H*                concatenate the primary name record.           *
     H*                                                               *
     H* 03/23/20 C:136 Project: 150WEI                                *
     H*               -Add WC990603 to list of large deductible forms *
     H*                that report electronically.                    *
     H*               -Apply excluded states logic as well.           *
     H*               -Rewrote SRDEDUCT                               *
     H*               -Changed electronic reporting routine to        *
     H*                accomodate multiple states for large deductible*
     H*                forms.                                         *
     H*                                                               *
     H* 03/16/20 C:65  Support: 84996 (Amwins)                        *
     H*                Prevent flat cx'd UE#'s from being added to    *
     H*                the name/address records.                      *
     H*                                                               *
     H* 02/24/20 C:65  Project: 150WAD/Phw#85676 (SUNZ)               *
     H*                Pull pre-minimum premium figures for           *
     H*                calculating merit rating on minimum premium    *
     H*                policies.                                      *
     H*                                                               *
     H* 02/07/20 C:65  Project: 150WAD/Phw#84111 (SUNZ)               *
     H*                Ensure name/address printing reflects later    *
     H*                period when a division/location cancels, and   *
     H*                then becomes active again with a gap inbetween.*
     H*                Also, use variables to indicate when WXN1P     *
     H*                records should be written for new name as      *
     H*                key fields were getting reset by UPDATWXA5L2   *
     H*                procedure which was reading in WXN1P records.  *
     H*                                                               *
     H* 01/23/20 C:65  Support: 86879 (LUBA)                          *
     H*                If 3C verbiage is not checked to be overridden *
     H*                then clear the fields in employer detail.      *
     H*                                                               *
     H* 01/21/20 C:136 Project: 150WEI                                *
     H*                Add electronic reporting for new large         *
     H*                deductible forms.                              *
     H*                                                               *
     H* 12/18/19 C:136 Project: WC2720                                *
     H*                Add the FWCIGA Surcharge and Total field to    *
     H*                Scriptura workfile for the FL Dec Page         *
     H*                                                               *
     H* 12/10/19 C:65  Project: Phw#84671 (Sunz)                      *
     H*                Use XTPFDT for pulling assessments and terror- *
     H*                ism for NJ instead of ELPFDT - the state rpt   *
     H*                division's policy effective date.              *
     H*                                                               *
     H* 11/26/19 C:79  Support: 150-85231                             *
     H*                Add logic for form WC200304C                   *
     H*                                                               *
     H* 11/15/19 C:65  Project: Phw#84505/150WAD (Sunz)               *
     H*                Ensure PEO acronym is added to client DBA name *
     H*                when there is a federal name present.          *
     H*                                                               *
     H* 11/11/19 C:267 Project: WC2695                                *
     H*                Write endorsement data record for WC370604     *
     H*                PA Employer Assessment Endorsement form.       *
     H*                                                               *
     H* 11/05/19 C:65  Project: Phw#84505 (Sunz)                      *
     H*                Reduce the space between PEO acronym and the   *
     H*                client name to one space.                      *
     H*                                                               *
     H* 10/16/19 C:65  Project: Phw# 73865/150WAD (Sunz)              *
     H*                Do not generate electronic data records for    *
     H*                WC990608, WC990609, WC990611, and WC990612     *
     H*                for Sunz.  Data reporting happens from         *
     H*                WC990603 and WC990610 which will always be on  *
     H*                the policy at same time as prior four forms.   *
     H*                                                               *
     H* 10/10/19 C:65  Project: Phw# 83942 (Amwins)                   *
     H*                Clear out endorsement # and effective date if  *
     H*                not being called from mid-term endorsements.   *
     H*                Data was being populated but not meant for     *
     H*                regular policy page when auto-submitted from   *
     H*                mid-term endorsement of revised bill.          *
     H*                                                               *
     H* 09/26/19 C:65  Project: Phw# 83222 (SCS)                      *
     H*                Clear out temporary payroll records as WCPOLS  *
     H*                does not override to qtemp for resubmissions.  *
     H*                                                               *
     H* 09/24/19 C:65  Project: Phw# 83127/150WAD (Sunz)              *
     H*                When doing a policy data resubmission from     *
     H*                policy transfer - remove any bureaus which     *
     H*                are no longer on the policy.                   *
     H*                                                               *
     H* 09/13/19 C:65  Project: Phw# 81277/150WAD (Sunz)              *
     H*                Use state reporting division policy eff date   *
     H*                for stat rpt/rating for writing exposure.      *
     H*                                                               *
     H* 09/11/19 C:136 Support: 82446                                 *
     H*                Add WXPEP record for WC990603A to report       *
     H*                to WCPOLS                                      *
     H*                                                               *
     H* 09/06/19 C:65  Project: Phw# 82762/150WAD (Sunz)              *
     H*                If a name uncancels - update address dates on  *
     H*                same record to active as well.                 *
     H*                                                               *
     H* 08/21/19 C:65  Project: Phw# 77695/150WAD (Sunz)              *
     H*                Prorate payroll exposure for MA minimum prem   *
     H*                policies so payroll x rate = premium.          *
     H*                                                               *
     H* 08/19/19 C:65  Project: Phw#82227 (Amwins)                    *
     H*                Populate NHEXT1 field for no modifier prior to *
     H*                defaulting the exp mod to 1 for most states.   *
     H*                                                               *
     H* 08/07/19 C:65  Project: Phw# 73890/150WAD (Sunz)              *
     H*                When concatenating names for policies with NJ  *
     H*                coverage, do not concatenate any names written *
     H*                for MI.                                        *
     H*                                                               *
     H* 08/06/19 C:300 Support: 81001 (SUNZ)                          *
     H*                Job 417027/P150HHAW/W7051C1 abended            *
     H*                The target for a numeric operation is too small*
     H*                when moving elanpr or elenmp to fxmnpr         *
     H*                                                               *
     H* 07/29/19 C:65  Project: 148WBE                                *
     H*                Add processing for request wxpep for WC000406. *
     H*                                                               *
     H* 07/25/19 C:65  Project: Phw# 81294/150WAD (Sunz)              *
     H*                Separate out the coding to update wxnap for    *
     H*                effective dates vs. the coding to update for   *
     H*                ue#'s.  If the coding for ue# created a new    *
     H*                record then the lock for updating later for    *
     H*                dates was lost.                                *
     H*                                                               *
     H* 06/17/19 C:65  Project: Phw# 73671 (Sunz)                     *
     H*                Check on state for adding ue#'s to wxnap in    *
     H*                the case that the same value is used for       *
     H*                multiple states.                               *
     H*                                                               *
     H* 05/17/19 C:65  Project: Phw# 71498 (Sunz)                     *
     H*                Check on state submission date as well for     *
     H*                determining if a new bureau has been added     *
     H*                for transaction 15 add.                        *
     H*                                                               *
     H* 05/20/19 C:180 Project: WC2667                                *
     H*                Add ARAP to Policy Premium.                    *
     H*                                                               *
     H* 05/13/19 C:65  Project: Phw# 78538 (LUBA)                     *
     H*                Allow ARRST array to include flat cx'd states  *
     H*                to allow exposure records to be reported for   *
     H*                a state that is to be deleted from policy.     *
     H*                Add checks in STPAGE to prevent those flat     *
     H*                CX'd states from printing.                     *
     H*                                                               *
     H* 04/12/19 C:65  Project: 150WAD/#76092(SUNZ)/#75749(Amwins)    *
     H*                Consolidate attachment of schedules coding to  *
     H*                reduce redunency.  Further, access schedule    *
     H*                template subroutine to populate various client *
     H*                form fields early on - so policy data creation *
     H*                for resubmission transactions will attach      *
     H*                proper forms.                                  *
     H*                                                               *
     H* 04/09/19 C:180 Project: 150WDU Add type of business at the    *
     H*                         ANI level.                            *
     H*                                                               *
     H* 04/03/19 C:65  Project: 150WAD/Phw#73594 (SUNZ)               *
     H*                Populate NHDRSK with EMRSK# for each state     *
     H*                header record - so the value is not pulled     *
     H*                from state reporting division only in extract. *
     H*                                                               *
     H* 03/19/19 C:65  Project: 150WAD/Phw#71514 (SUNZ)               *
     H*                Ensure name/address records reflect later      *
     H*                period when a division/location cancels, and   *
     H*                then becomes active again with a gap inbetween.*
     H*                                                               *
     H* 03/15/19 C:65  Project: 150WAD/WC2423/Phw#73797 (SUNZ)        *
     H*                The WC890600B form should only be listing for  *
     H*                CA policies, not every policy.                 *
     H*                                                               *
     H* 03/05/19 C:65  Project: 150WAD/Phw#71514 (SUNZ)               *
     H*                Shift populate of expiration date for locs and *
     H*                check a name/address is not flat CX'd before   *
     H*                updating the earliest/latest eff/exp dates.    *
     H*                Correct population of CX date for additional   *
     H*                named insured.                                 *
     H*                                                               *
     H* 01/31/19 C:291 Project: WC2206                                *
     H*                Print and report a zero for the first modifier *
     H*                if not rated.                                  *
     H*                                                               *
     H* 01/29/19 C:65  Project: 150WAD/Phw#76664 (SUNZ)               *
     H*                Prevent divide by zero on flat cx division.    *
     H*                                                               *
     H* 01/15/19 C:65  Project: 148WBB/Phw#76193 (Amwins)             *
     H*                Prevent flat CX locations from printing on     *
     H*                schedule for Amwins/Starstone.                 *
     H*                                                               *
     H* 01/14/19 C:65  Project: 129WSB/Phw#76182                      *
     H*                Change call to Coverisk program for NY/MA      *
     H*                and cert holder spool file notices to only     *
     H*                print when spool file output is requested.     *
     H*                                                               *
     H* 01/03/19 C:65  Project: 150WAD/Phw#71514 (SUNZ)               *
     H*                Ensure address info gets updated even when     *
     H*                on a record with a name.  The address may      *
     H*                only exist on that record and would still      *
     H*                need updating.  Correct from/to dates in       *
     H*                update subroutines.                            *
     H*                                                               *
     H* 12/20/18 C:65  Project: 148WAN/Phw#74063                      *
     H*                Load terrorism breakout box with calculated    *
     H*                rates from employer detail.                    *
     H*                                                               *
     H* 12/04/18 C:65  Project: 150WCR/Phw#73852 (SUNZ)               *
     H*                Correct application of report as PEO flag to   *
     H*                work when personal name flag isnt set.         *
     H*                                                               *
     H* 11/08/18 C:180 Project:150WDC                                *
     H*                Override PEO name control/rules               *
     H*                                                              *
     H* 11/01/18 C:65  Project: WC2589                                *
     H*                Correct flat cx processing to use the policy   *
     H*                effective date when cx date is in prior year.  *
     H*                                                               *
     H* 10/29/18 C:65  Project: 150WAD/Phw#73029 (SUNZ)               *
     H*                Use PEO type of business for output control    *
     H*                mode of '3' - except for WI.                   *
     H*                                                               *
     H* 10/16/18 C:65  Project: WC2640                                *
     H*                Remove the defaulting of payroll printing as   *
     H*                Detailed for non-NCCI states and instead       *
     H*                check new generic table.                       *
     H*                                                               *
     H* 10/12/18 C:136 Support: 73053                                 *
     H*                Implement correction for Sunz large deductible *
     H*                percentage                                     *
     H*                                                               *
     H* 10/10/18 C:291 Project: WC2636                                *
     H*                Write endorsement data record for WC040342B    *
     H*                Limiting Agreement Endorsement for CA.         *
     H*                                                               *
     H* 10/09/18 C:65  Project: 150WCZ                                *
     H*                Populate prorated exp constant into workfile.  *
     H*                                                               *
     H* 10/03/18 C:65  Project: 133WQQ                                *
     H*                Write endorsement data record for Luba         *
     H*                deductible endorsements.                       *
     H*                                                               *
     H* 09/26/18 C:62  Ticket: 73514(Inhouse)                        *
     H*                When accessing Submission History don't use    *
     H*                division                                       *
     H*                                                               *
     H* 09/21/18 C:65  Project: 150WAR/Phw#50676                      *
     H*                Round large deductible percentage to 3 decimal *
     H*                places prior to sending to Scriptura workfile. *
     H*                                                               *
     H* 09/13/18 C:267 Project: WC2634 SUNZ                           *
     H*                populate PENAME with EMFDNM (Federal name)     *
     H*                if not blank; changes requested by client.     *
     H*                                                               *
     H* 08/30/18 C:65  Project: WC2607/150WAD/Phw#72882               *
     H*                Write endorsement data record for more         *
     H*                Sunz deductible endorsements - use internal    *
     H*                SRDEDUCT subroutine as all these endorsements  *
     H*                record the same information for type 43 rcds.  *
     H*                                                               *
     H* 08/27/18 C:267 Project: 72607 (SUNZ)                          *
     H*                Load XXBTYP4 into PWBUSTYPE with regards to    *
     H*                type of business 'other' description.          *
     H*                Dec pages are truncation description.          *
     H*                                                               *
     H* 08/20/18 C:267 Project: WC2634                                *
     H*                Write endorsement data record for WC040315B    *
     H*                CA Employee Leasing endorsement form.          *
     H*                                                               *
     H* 08/07/18 C:136 Project: WC2589                                *
     H*                Print and report flat cx'd divisions,          *
     H*                locations, and additional names.               *
     H*                                                               *
     H* 08/06/18 C:65  Project: 150WAD                                *
     H*                Start the 2nd thousand names with 002 for name *
     H*                link identifier.                               *
     H*                                                               *
     H* 07/27/18 C:65  Project: 150WAD                                *
     H*                Remove spaces from template form numbers       *
     H*                before writing to WMMUP for reporting.         *
     H*                                                               *
     H* 07/18/18 C:65  Project: 150WAD                                *
     H*                Add two fields to hold the name and address    *
     H*                effective date.  Independent bureaus will      *
     H*                report the full coverage date range so the     *
     H*                begin date will need to be retained.  NCCI     *
     H*                will continue reporting deletions with the     *
     H*                same date in policy change eff/exp date fields.*
     H*                                                               *
     H* 07/11/18 C:65  Project: 148WAY/Phw#71499                      *
     H*                Change the renewal check to use the WA069R     *
     H*                API, as it has more accurate logic.  It will   *
     H*                return the prior policy #.                     *
     H*                                                               *
     H* 07/10/18 C:65  Project: 150WCZ                                *
     H*                Populate premium discount into script workfile.*
     H*                                                               *
     H* 06/27/18 C:65  Project: SY371/Phw#71857                       *
     H*                Move the new/renew check for skipping policies *
     H*                to occur before requesting XML for scriptura.  *
     H*                                                               *
     H* 06/21/18 C:136 Support: 70925                                 *
     H*                Report a '0' or '4' to deviation type field    *
     H*                NHCPTD if MA and Dev 1 or Dev 2 stat code is   *
     H*                9037 (MA Rate Deviation)                       *
     H*                                                               *
     H* 06/07/18 C:65  Project: WC2607                                *
     H*                Move output specs to own source member for     *
     H*                space.                                         *
     H*                                                               *
     H* 05/31/18 C:65  Project: WC2607/150WAD                         *
     H*                Write endorsement data record for several      *
     H*                Sunz deductible endorsements.                  *
     H*                                                               *
     H* 05/30/18 C:65  Project: WC2607                                *
     H*                Write endorsement data record for several      *
     H*                national/state deductible endorsements.  If a  *
     H*                deductible endorsement uses SRTEXT, or is      *
     H*                otherwise generic, call subroutine to populate *
     H*                the WXPEP record instead of leaving it to the  *
     H*                endorsement print program.                     *
     H*                                                               *
     H* 05/11/18 C:291 Project: 150WCR                                *
     H*                Include Report as PEO flag to set personal name*
     H*                flag for PEO name status.                      *
     H*                                                               *
     H* 05/01/18 C:65  Project: 150WAD                                *
     H*                Change the output control flag to SRADDR for   *
     H*                CA policies which will use acronym to allow it *
     H*                to be appended to client name instead of PEO.  *
     H*                Ensure the acronym is appended to all instances*
     H*                of primary client name so it is not reported   *
     H*                twice.                                         *
     H*                                                               *
     H* 04/30/18 C:136 Support: 70391                                 *
     H*                Populate new Territory Rating fields in WXP3P  *
     H*                for CareWest.                                  *
     H*                                                               *
     H* 03/28/18 C:65  Project: 148WAF                                *
     H*                Ensure physical address from state reporting   *
     H*                division does not get overridden by mailing    *
     H*                address when going to print on schedule.       *
     H*                                                               *
     H* 03/20/18 C:65  Project: 148WAT                                *
     H*                Reverse the change to print the installment    *
     H*                schedule after endorsements.                   *
     H*                                                               *
     H* 03/19/18 C:65  Project: 150WAD                                *
     H*                Do not subtract out expense constant from bal  *
     H*                to minimum premium if it has already been      *
     H*                broken out.  Also, prorate increased limits    *
     H*                minimums if policy is short term for concur.   *
     H*                                                               *
     H* 03/18/18 C:65  Project: 150WAD                                *
     H*                Do not run re-attach endorsements program when *
     H*                called for mid-term endorsements.              *
     H*                                                               *
     H* 03/13/18 C:65  Project: 148WAN                                *
     H*                Restore coding which populated edition dates   *
     H*                for dec page and schedules.                    *
     H*                                                               *
     H* 03/12/18 C:65  Project: 150WAD                                *
     H*                Ensure dec page and schedules for CA policies  *
     H*                are written to wmmup for reporting when the    *
     H*                listing of schedules is enabled.               *
     H*                                                               *
     H* 03/07/18 C:65  Project: 150WAO                                *
     H*                Build dec page schedule strings with optional  *
     H*                form description when populated.               *
     H*                                                               *
     H* 03/05/18 C:136 Project: 150WCT                               *
     H*                Suppress Additional Workplace schedule if      *
     H*                running midterm endorsement for Additional     *
     H*                Named Insureds                                 *
     H*                                                               *
     H* 03/02/18 C:65  Project: 150WAD                                *
     H*                Use 1st position in SWCPF/WCSC04 to indicate   *
     H*                if the CA exclusion of privacy notices listing *
     H*                on schedule of forms should be overridden.     *
     H*                                                               *
     H* 02/22/18 C:291 Project: WC2521                                *
     H*                Popupate Non-Specific address field in WXNAP.  *
     H*                                                               *
     H* 02/08/18 C:65  Project: 148WAT                                *
     H*                Populate a rewrite field for Starstone groups. *
     H*                Populate a new page bundling field to combine  *
     H*                the page numbering for Starstone dec page and  *
     H*                schedule of premium.  Duplicate the main       *
     H*                insured on the policy to the workplace         *
     H*                schedule.  Ensure proper ordering of the       *
     H*                WC000000C form and schedules for Starstone.    *
     H*                Populate premium/total amounts into payroll    *
     H*                work file and flag assessment line items.      *
     H*                Endorsement seq# starts at 0 for Starstone.    *
     H*                Always print fein for starstone workplaces.    *
     H*                For mid-term endorsements, allow WXNAP record  *
     H*                processing to occur to allow proper name link  *
     H*                codes to populate for workplace schedule.      *
     H*                                                               *
     H* 02/06/18 C:65  Project: 150WAD                                *
     H*                Report a '0' for no premium deviation for MA.  *
     H*                                                               *
     H* 02/01/18 C:136 Project: 148WAH                                *
     H*                Add premium line item for policy fee.          *
     H*                                                               *
     H* 01/31/18 C:62  Project: 150WCO                                *
     H*                Add Override Form #                            *
     H*                                                               *
     H* 01/31/18 C:65  Project: 150WAD                                *
     H*                If a state is being added to a resubmitted     *
     H*                policy then flag it with an 'A' for status     *
     H*                so it reports as an add state transaction.     *
     H*                                                               *
     H* 01/31/18 C:62  Project: 150WCO                                *
     H*                Write to Optional Form # Override Field        *
     H*                                                               *
     H* 01/26/18 C:65  Project: Phw#63982 (Sunz)                      *
     H*                Increase size of resequence array.             *
     H*                                                               *
     H* 01/15/18 C:65  Project: 150WCJ                                *
     H*                If a policy has an overriding PEO specified    *
     H*                then pull the PEO name/fein from the indicated *
     H*                additional named insured record.               *
     H*                                                               *
     H* 01/10/18 C:33  Support 63341                                  *
     H*                Clear agt# before WA002R call                  *
     H*                                                               *
     H* 01/06/18 C:65  Project: 150WAD                                *
     H*                Remove the 2006 change which reported dba      *
     H*                names using a legal nature of '$' to represent *
     H*                the name as a "Trade Name".  Type 12 no longer *
     H*                means trade names; report the type of business *
     H*                instead.                                       *
     H*                                                               *
     H* 01/05/18 C:267 Project: 150WCI                                *
     H*                Populate Business Segment ID# field            *
     H*                                                               *
     H* 01/02/18 C:136 Project: 148WAN (63359)                        *
     H*                Always provide total line in premium detail    *
     H*                for AmWins.                                    *
     H*                                                               *
     H* 12/22/17 C:65  Project: 150WAO                                *
     H*                Ensure CA schedules use client template form#, *
     H*                description, and ed date if populated.         *
     H*                                                               *
     H* 12/21/17 C:65  Project: 150WBM                                *
     H*                Check the suppress policy notices flag before  *
     H*                including the VA and DE notices.  Prevent them *
     H*                from including on mid-term endorsements too.   *
     H*                                                               *
     H* 12/18/17 C:65  Project: 129WVA                                *
     H*                Ensure language codes are populaed on records. *
     H*                                                               *
     H* 12/11/17 C:65  PROJECT: 148WAN                                *
     H*                Add fields for originating transaction form -  *
     H*                this will be used in place of client or        *
     H*                default template for new business or renewal   *
     H*                business transactions (only).                  *
     H*                                                               *
     H* 12/05/17 C:65  PROJECT: 150WCD                                *
     H*                Update endorsement form array to have 15A      *
     H*                values to fully support endorsement table.     *
     H*                                                               *
     H* 11/22/17 C:65  PROJECT: 150WAO                                *
     H*                Populate employer link code for additional     *
     H*                locations/named insured scriptura work file.   *
     H*                                                               *
     H* 11/21/17 C:65  Project: 148WAN                                *
     H*                Populate installment schedule form #'s into    *
     H*                their own fields.                              *
     H*                                                               *
     H* 11/09/17 C:136 Project: 148WAN                                *
     H*                Add Schedule of Installments (AmWins Only)     *
     H*                                                               *
     H* 11/08/17 C:65  PROJECT: 148WAN                                *
     H*                Populate extra fields for schedules.           *
     H*                                                               *
     H* 11/07/17 C:65  PROJECT: 148WAN                                *
     H*                Populate full state name into premium workfile.*
     H*                                                               *
     H* 11/06/17 C:65  PROJECT: WC2565                                *
     H*                If a policy is merit rated then check the new  *
     H*                threshold values to report the exposure under  *
     H*                the correct stat code for high DB/CR values.   *
     H*                                                               *
     H* 11/03/17 C:136 Project: 150WCC                                *
     H*                Add processing for 'Exclude from schedule'     *
     H*                flag                                           *
     H*                                                               *
     H* 10/30/17 C:65  PROJECT: 148WAN                                *
     H*                Update the Scriptura work file to include the  *
     H*                schedules on the schedule of endorsements if   *
     H*                flagged in output control. Populate the client *
     H*                template form #, edition date, and endorsement *
     H*                effective date into the endorsement schedule   *
     H*                work file as well.                             *
     H*                                                               *
     H* 10/24/17 C:65  PROJECT: 150WBW                                *
     H*                After XMLTEMP records are created for endorse- *
     H*                ments read through them to determine if they   *
     H*                need form specific signatures added.           *
     H*                                                               *
     H* 10/05/17 C:65  PROJECT: 150WBL                                *
     H*                Change logical for printing/ordering           *
     H*                endorsement forms so they process in state     *
     H*                order first, then form order.                  *
     H*                                                               *
     H* 09/15/17 C:65  PROJECT: 150WAO                                *
     H*                Check flag on Policy Page Scriptura Setup      *
     H*                screen for always producing schedule of        *
     H*                endorsements.                                  *
     H*                                                               *
     H* 09/01/17 C:65  PROJECT: 150WBM                                *
     H*                Check new flag for suppressing loss notices    *
     H*                and other policy forms when writing templates  *
     H*                for scriptura.  Additionally, populate group   *
     H*                name and address into notice work file.        *
     H*                                                               *
     H* 08/16/17 C:65  PROJECT: 150WAO                                *
     H*                Populate flag into scriptura work file for     *
     H*                printing employer link codes.  Additionally    *
     H*                populate link codes and terrorism info into    *
     H*                premium schedule work file.  Lastly update the *
     H*                "to date" field for payroll records to reflect *
     H*                the payroll cancellation date.                 *
     H*                                                               *
     H* 08/11/17 C:65  PROJECT: SY371/PHW# 47957 (LCTA)               *
     H*                Do not request scriptura/xml if hardcopy is    *
     H*                not being requested.                           *
     H*                                                               *
     H* 07/27/17 C:62  PROJECT: 150WAL                                *
     H*                -Add Territorial Differential Factor to        *
     H*                 Schedule of Premium                           *
     H*                -No need to lock lda                           *
     H*                                                               *
     H* 07/19/17 C:65  PROJECT: 150WAP                                *
     H*                If a record is set up for POC notice on the    *
     H*                policy forms template grid for policy pages    *
     H*                then generate a POC notice record for the      *
     H*                policy if it has business in the corresponding *
     H*                state.  This will be triggered from new or     *
     H*                renewal policy transactions.                   *
     H*                                                               *
     H* 07/17/17 C:65  PROJECT: 150WAP                                *
     H*                Add coding to recognize the 'C' value for PEO. *
     H*                'C' groups have both a Master policy and many  *
     H*                MCP policies.  Any output control settings     *
     H*                that apply to MCP groups should apply to       *
     H*                client policies in a combined group as well.   *
     H*                In addition, treat any additional named insured*
     H*                or additional locations on employer 1 division *
     H*                0 as PEO business as well (name designation).  *
     H*                A new checkbox at employer division/addl named *
     H*                insured/addl location levels will indicate if  *
     H*                a name in a PEO should not be flagged with a   *
     H*                P/C for PEO or client.                         *
     H*                                                               *
     H* 06/27/17 C:65  Project: 136WHF                                *
     H*                Print Territory Rating factor to the right of  *
     H*                the experience modifier information on the     *
     H*                premium schedules.                             *
     H*                                                               *
     H* 06/16/17 C:65  Support: 44912 (CareWest)                      *
     H*                Do not include assessments in the CA deposit   *
     H*                premium figure.                                *
     H*                                                               *
     H* 06/13/17 C:65  Support: 47696 (Amwins)                        *
     H*                Call the NJEUE subroutine for employer master  *
     H*                records during the dba name section.  Correct  *
     H*                both NJEUE and NJAUE subroutines to properly   *
     H*                pull unemployment # or NJ state tax ID for     *
     H*                non-CSI clients.                               *
     H*                                                               *
     H* 05/16/17 C:136 Support: 46709                                 *
     H*                Don't allow state unemployment number to       *
     H*                carry forward to subsequent location           *
     H*                entries                                        *
     H*                                                               *
     H* 05/10/17 C:65  PROJECT: WC2490                                *
     H*                Print WC000001C form # for 5/1/17 or later     *
     H*                policies on base declaration page.             *
     H*                                                               *
     H* 05/08/17 C:180 Project: 93WHLH                                *
     H*                Add electronic reporting for WC000424,         *
     H*                WC370401 Endorsements (Audit NonCompliance     *
     H*                Charge).                                       *
     H*                                                               *
     H* 05/05/17 C:65  PROJECT: WC2480                                *
     H*                Take 03/20/17 coding which wrote the client    *
     H*                mailing address attached to client name in     *
     H*                case where PEO address was attached and        *
     H*                move it so client address gets attached to     *
     H*                federal name instead of attached to dba name   *
     H*                a second time.                                 *
     H*                                                               *
     H* 05/02/17 C:180 Project: WC2493                                *
     H*                Add electronic reporting for WC040316B (CA     *
     H*                Employee Leasing Endorsement)                  *
     H*                                                               *
     H* 04/27/17 C:65  Project: 148WAF - Amwins WCPOLS Testing        *
     H*                Condition output control flag processing to    *
     H*                only happen for MCP groups and not for the PEO *
     H*                policy itself either.                          *
     H*                                                               *
     H* 04/26/17 C:65  Project: 148WAF - Amwins WCPOLS Testing        *
     H*                Report primary name insured on WI MCP client   *
     H*                policies with PEO name concatenated with       *
     H*                client name.  Also trigger the mailing address *
     H*                to print when the PEO's address printed on the *
     H*                declaration page.                              *
     H*                                                               *
     H* 04/26/17 C:65  Project: Supp#42416 Revision                   *
     H*                Update coding to move the update out of the    *
     H*                NJeue# and NJaue# subroutines as well as the   *
     H*                UpdDivDates and UpdLocDates subroutines to     *
     H*                ensure indicators are properly set for         *
     H*                each name and update happens once.  Coding     *
     H*                changes are not commented.                     *
     H*                                                               *
     H* 04/21/17 C:180 Project: WC2488                                *
     H*                Add electronic reporting for WC480315 (WI      *
     H*                Employee Leasing Endorsement)                  *
     H*                                                               *
     H* 04/12/17 C:65  Project: WC2483                                *
     H*                Add electronic reporting for WC200304B (MA     *
     H*                Employee Leasing Endorsement)                  *
     H*                                                               *
     H* 04/10/17 C:136 Support: 46652                                 *
     H*                Move FEIN info to the right in the Schedule    *
     H*                of Locations because it is covering up the     *
     H*                name                                           *
     H*                                                               *
     H* 04/07/17 C:062 Project 136WHF                                 *
     H*                Add Territory Rating for CareWest.             *
     H*                                                               *
     H* 03/30/17 C:65  PROJECT: SY371                                 *
     H*                Pull discount percentage from largest division *
     H*                written in the state.                          *
     H*                                                               *
     H* 03/29/17 C:65  PROJECT: SY371                                 *
     H*                Increase size of xxline to prevent overflow.   *
     H*                                                               *
     H* 03/20/17 C:65  PROJECT: WC2480                                *
     H*                Ensure when the dba name has to be written on  *
     H*                an PEO client policy where the PEO address     *
     H*                has been reported on the main name, update the *
     H*                dba name so it has the client's address.       *
     H*                                                               *
     H* 03/20/17 C:65  PROJECT: SY371                                 *
     H*                Pull location FEINs from WXN1P file.           *
     H*                Ensure client address from employer master     *
     H*                prints as a work place when declaration page   *
     H*                is printing the PEO's information, and write   *
     H*                additional named insured to the same address.  *
     H*                                                               *
     H* 03/13/17 C:65  PROJECT: SY371                                 *
     H*                Set exp mods correctly in WXP3P.               *
     H*                                                               *
      * 03/10/17 C:180 PROJECT: WC2481                                *
      *                Resequence xml order.                          *
      *                                                               *
     H* 03/06/17 C:65  PROJECT: WC2481                                *
     H*                Add processing to generate MO and MT loss      *
     H*                control notices for Scriptura output.          *
     H*                                                               *
     H* 03/01/17 C:65  PROJECT: SY371                                 *
     H*                Disable spool file output when not requested.  *
     H*                                                               *
     H* 02/24/17 C:65  PROJECT: WC2480                                *
     H*                Rewrite some of the SVENCD coding to prevent   *
     H*                names from being written with same entity code.*
     H*                Add flexible processing for new acronym field. *
     H*                                                               *
     H* 02/15/17 C:65  PROJECT: SY371 - Scriptura Phase 2             *
     H*                Call ScriptFile for mid-term endorsements.     *
     H*                                                               *
     H* 02/14/17 C:65  PROJECT: *** Removed Comments to make room *** *
     H*                                                               *
     H* 02/03/17 C:180 Support: 45540                                 *
     H*                When processing Amtrust, do not print          *
     H*                premium information during WCPOLS extraction.  *
     H*                                                               *
     H* 01/30/17 C:65  PROJECT: SY371 - Scriptura Phase 2             *
     H*                Call the W7413RMPD program to check if VA      *
     H*                notice should be generated and pass flag       *
     H*                for producing spool file output to W7413C2.    *
     H*                                                               *
     H* 01/27/17 C:65  PROJECT: SY371 - Scriptura Phase 2             *
     H*                Clear out second description field for any     *
     H*                policy page attached endorsements to allow     *
     H*                the descriptions to properly copy to workfile. *
     H*                                                               *
     H* 01/18/17 C:136 Support: 45169                                 *
     H*                Correct overflow max value for Amtrust         *
     H*                additional locations page                      *
     H*                                                               *
     H* 01/06/17 C:65  Project: 114WCQ/Phw#44985                      *
     H*                Do not produce installment schedule during     *
     H*                WCPOLS extractions.                            *
     H*                                                               *
     H* 01/04/17 C:65  Project: 114WCQ                                *
     H*                Swap the item 3C fields for AMTRUST format.    *
     H*                                                               *
     H* 12/29/16 C:136 Project: SY371 - Scriptura Phase 2             *
     H*                Configure Scriptura to process midterm         *
     H*                endorsements                                   *
     H*                                                               *
     H* 12/16/16 C:65  PROJECT: SY371 - Scriptura Phase 2             *
     H*                Split the percentage/factor information out    *
     H*                of the heading into separate fields for        *
     H*                Scriptura premium line item workfile.          *
     H*                                                               *
     H* 12/15/16 C:65  PROJECT: SY371 - Scriptura Phase 2             *
     H*                Set new fields for the split city/state/zip    *
     H*                fields in scriptura workplace workfile.        *
     H*                Pull the declaration page template from the    *
     H*                policy page template setup file.               *
     H*                                                               *
     H* 12/13/16  C:65  PROJECT: WC1701                               *
     H*                 Update *in67 for printing detail payroll      *
     H*                 information if Group is set or NJ/DE is set.  *
     H*                                                               *
     H* 11/23/16 C:65  PROJECT: SY371 - Scriptura Phase 2             *
     H*                Update category on master record for mass      *
     H*                run to allow proper work file data program     *
     H*                to be called.                                  *
     H*                                                               *
     H* 11/01/16 C:65  Project: 114WCQ                                *
     H*                Print Amtrust policy declaration page and      *
     H*                schedules for AHC based on association ID -    *
     H*                SIC and WIC will represent Amtrust policies.   *
     H*                Different formats will be used for FL, NJ,     *
     H*                and PA(rest of states use this format).        *
     H*                Signature pages will print ahead of the        *
     H*                declaration page.                              *
     H*                                                               *
     H* 10/13/16 C:65  PROJECT: SY371 - Scriptura Phase 2             *
     H*                Only generate the summary page work file       *
     H*                record when the multi-state indicator is on.   *
     H*                                                               *
     H* 10/07/16 C:65  PROJECT: SY371 - Scriptura Phase 2             *
     H*                Update LDA so it does not overlay output       *
     H*                control window usage.                          *
     H*                                                               *
     H* 09/27/16 C:65  PROJECT: SY371 - Scriptura Phase 2             *
     H*                Add a monitor message to prevent job from      *
     H*                blowing up if Scriptura fails for PDSDEV.      *
     H*                                                               *
     H* 09/26/16 C:136 Support: 42416                                 *
     H*                Report multiple UE's from other states besides *
     H*                NJ                                             *
     H*                                                               *
     H* 09/16/16 C:65  PROJECT: SY371 - Scriptura Phase 2             *
     H*                Update the processing to generate a master     *
     H*                XMLPREP record when running mass policy pages  *
     H*                with linked records in XMLPREP.  Create        *
     H*                XMLTEMP additional template records for each   *
     H*                schedule and for each endorsement attached.    *
     H*                Finally, make one call to S4401C to submit     *
     H*                the information to Scriptura server.           *
     H*                                                               *
     H* 08/24/16 C:136 Project: WC2451                                *
     H*                Add electronic reporting for WC040318A (CA     *
     H*                LLC Exclusion)                                 *
     H*                                                               *
     H* 07/22/16 C:136 Project: WC2449                                *
     H*                Add electronic reporting for WC040306 (CA      *
     H*                Waiver Charge)                                 *
     H*                                                               *
     H* 07/15/16 C:136 Project: WC2449                                *
     H*                Add electronic reporting for WC040305 (CA      *
     H*                Voluntary Charge)                              *
     H*                                                               *
     H* 07/11/16 C:65  PROJECT: SY371 - Scriptura Phase 2             *
     H*                Populate fields for loss prevention notice.    *
     H*                                                               *
     H* 07/08/16 C:65  Support: Phw#42347 and 42353 (CareWest)        *
     H*                Add update form #'s to trigger additional      *
     H*                data processing for WC040302B and WC040303A.   *
     H*                                                               *
     H* 06/20/16 C:65  PROJECT: SY371 - Scriptura Phase 2             *
     H*                Change work file date fields to be 8.0s        *
     H*                Add separate city/state/zip fields.            *
     H*                                                               *
     H* 06/08/16 C:136 Support: 40756                                 *
     H*                Always report the NAIC code if available       *
     H*                without regard to the RRNAIC flag.             *
     H*                                                               *
     H* 06/06/16 C:65  PROJECT: SY371 - Scriptura Phase 2             *
     H*                Change policy name/address fields for dec page *
     H*                in work file.                                  *
     H*                                                               *
     H* 06/03/16 C:310 Support: 41658                                 *
     H*                Modified code to correct spacing on named      *
     H*                insureds.                                      *
     H*                                                               *
     H* 05/23/16 C:136 Support: 41552                                 *
     H*                Attach WC990605 endorsement if more than one   *
     H*                address exists on policy (even if no unique    *
     H*                addresses exist in Location Master).           *
     H*                                                               *
     H* 05/19/16 C:136 Support: 25724                                 *
     H*                Don't report other divisions as cx'd if        *
     H*                they have the same cx date as the state        *
     H*                reporting division.                            *
     H*                                                               *
     H* 05/06/16 C:298 Project: WC2423                                *
     H*                Add listing of form WC890600B to the schedule  *
     H*                of forms.                                      *
     H*                                                               *
     H* 04/27/16 C:306 Project: WC2424                                *
     H*                Experience Modifier Not final status flag      *
     H*                population.                                    *
     H*                                                               *
     H* 04/07/16 C:65  PROJECT: SY371 - Scriptura Phase 2             *
     H*                Fix AMN array by placing 'ds' above it.        *
     H*                                                               *
     H* 03/04/16 C:65  PROJECT: SY371 - Scriptura Phase 2             *
     H*                Populate work file for phase 2 scriptura.      *
     H*                Replace the work file title field with renew   *
     H*                and reissue fields.  Add CA printing flag and  *
     H*                a new work file for holding endorsement forms. *
     H*                Add incrementing keys to other work files and  *
     H*                remove endorsement form fields from primary.   *
     H*                                                               *
     H* 12/15/15 C:65  PROJECT: Phw#37015 Ref 136WBG SUPP#51219/51088 *
     H*                Remove temporary change for 136 and replace    *
     H*                with the group year flag for deposit by year.  *
     H*                                                               *
     H* 12/03/15 C:65  Project: 45WKP                                 *
     H*                If processing a policy effective prior to      *
     H*                1/1/2016 for LCTA then print/use old carrier#. *
     H*                                                               *
      * 10/20/15 C:302 PROJECT: WC2364                                *
      *                Add non-ratable class code flag                *
      *                                                               *
     H* 10/15/15 C:65  Project: WC2386                                *
     H*                Move heading to be right aligned if resolving  *
     H*                as PDF documents, to make room for logos to    *
     H*                print w/o image verbiage.  Adjusted exp const  *
     H*                and deposit items on dec page to be spaced     *
     H*                better and sure $ sign shows up.               *
     H*                                                               *
     H* 07/06/15 C:65  Support: Phase#35058 (CSI)                     *
     H*                Add anniv date to state data structure for     *
     H*                populating the header file for states.         *
     H*                                                               *
     H* 05/19/15 C:279 Project: WC2363                                *
     H*                Modified to condition policy page form# for NJ *
     H*                state based on date 01/01/2015.                *
     H*                                                               *
     H* 05/15/15 C:279 Project: 114WSX                                *
     H*                Modified to condition policy page form# for NJ *
     H*                state based on date 01/01/2015.                *
     H*                                                               *
     H* 04/08/15 C:242 Support: 33056                                 *
     H*                NJ TAX ID# & State Risk ID moved to WXNAP to   *
     H*                print correctly. Also replace MultiState var   *
     H*                with @@MultiSt due to new PR w that name.      *
     H*                                                               *
     H* 03/16/15 C:267 Support: 33186                                 *
     H*                Clear state reporting id field.  Issue with    *
     H*                NJ state reporting id printing always.         *
     H*                                                               *
     H* 02/26/15 C:180 Support: 31837                                 *
     H*                Fix risk id for PA                             *
     H*                                                               *
     H* 01/28/15 C:302 Support: 32918                                 *
     H*                Need to cut out a space in "Eff Date:" so     *
     H*                the whole thing will fit in 35 position field. *
     H* 01/14/15 C:282 Project: WC2334                                *
     H*                                                               *
     H*                Process to read the state code table to        *
     H*                determine if a state code would be reported    *
     H*                if there is no exposure amount. Added the      *
     H*                assessment for PA state. Removed the hardcoded *
     H*                assessment for 'NY' state.                     *
     H*                                                               *
     H* 02/18/15 C:65  Support: Phase#33385                           *
     H*                Set on the dba flag for SRADDR if called for a *
     H*                NJ policy.                                     *
     H*                                                               *
     H* 01/27/15 C:302 Support: 32968                                 *
     H*                Need to update field endEdt with the          *
     H*                editionDate for those hardcode forms.          *
     H*                                                               *
     H* 11/26/14 C:302 Project: 114WQH                                *
     H*                If a NJ policy and the group uses overlays     *
     H*                and there is an overlay setup for NJ version,  *
     H*                then use the NJ overlay.                       *
     H*                                                               *
     H* 11/12/14 C:282 Project: WC2318                                *
     H*          C:65  Policy Page Information Page Notes is          *
     H*                updated to WC000001B from WC000001A on         *
     H*                policies effective 1/1/2015 and later.         *
     H*                                                               *
     H* 11/11/14 C:65  Project: 114WQF                                *
     H*                Print Accident Fund declaration page and       *
     H*                schedules for CSI based on association ID.     *
     H*                This is the same design done for CompWest      *
     H*                under project 132WOC. Print United Wisconsin   *
     H*                declaration page and schedules for CSI based   *
     H*                on association ID as well - new formats.       *
     H*                                                               *
     H* 11/05/14 C:242 Support 84683 WA133                            *
     H*                Redesign Policy page other locations section.  *
     H*                Rewrite 10/06/14 changes and Othpdt* formats.  *
     H*                                                               *
     H* 11/04/14 C:65  Redmine #4704 - T.H.E.                         *
     H*                Confine the duplicate address printing to LUBA *
     H*                as other clients don't want to see that stuff. *
     H*                                                               *
     H* 10/23/14 C:136 Support: WA147 87085                           *
     H*                Move logic that sets xxct01 so that xxpdf      *
     H*                will be set correctly so that overlay          *
     H*                processing will function (see xxct01)          *
     H*                                                               *
     H* 10/09/14 C:282 Support 86454 WA114                            *
     H*                In the subroutine srFlatRateAdj the variables  *
     H*                xxst and xxpst were populated by 'IL' state    *
     H*                value so when the policy page runs for the     *
     H*                'NJ' state the footer 'WC 00 00 01 A' get      *
     H*                printed instead of 'WC 00 00 01 B'. Replaced   *
     H*                the variable xxst and xxpst with hardcoded     *
     H*                values of 'IL' within the subroutine           *
     H*                srFlatRateAdj.                                 *
     H*                                                               *
     H* 10/06/14 C:242 Support 84683  WA133                           *
     H*                Redesign Policy page. Repeat address specific  *
     H*                info after each address, instead of each loc.  *
     H*                                                               *
     H* 09/26/14 C:242 Support 84683  WA133                           *
     H*                Removed code changes made 04/17/14. Only       *
     H*                entities with UNIQUE Name/Fein/Address reported*
     H*                                                               *
     H* 09/25/14 C:242 Support 84212,84683  WA133                     *
     H*                84212: Commented original fix and added correct*
     H*                fix to treat FedName same as DBA. xFname/xSname*
     H*                                                               *
     H* 09/17/14 C:295 Support: WA102 86630                           *
     H*                Correct the condition to print the FEIN No.    *
     H*                                                               *
     H* 09/10/14 C:65  Support: 86701                                 *
     H*                Reset employer master after building state     *
     H*                array, a chain to employer master was added    *
     H*                under 85985 support fix.                       *
     H*                                                               *
     H* 07/23/14 C:242 Support: 85985                                 *
     H*                Do Not print Flat Cancelled Division info.     *
     H*                                                               *
     H* 07/03/14 C:242 Support: 85511                                 *
     H*                Wrong State code link in WCPOLS on mult-state  *
     H*                policies. Error in WXNAP.NAST                  *
     H*                                                               *
     H* 07/01/14 C:65  Project: WC2277                                *
     H*                Call W7413RDL to generate new/renewal business *
     H*                department letter prior to policy page.        *
     H*                                                               *
     H* 06/27/14 C:242 Project: WC2274                                *
     H*                Do NOT set CA Resubmission to Sent('4') Status *
     H*                                                               *
     H* 05/28/14 C:65  Project: WC2277                                *
     H*                Remove the SendNotice call (not commented).    *
     H*                                                               *
     H* 05/19/14 C:65  Project: 136WGJ                                *
     H*                Turn on the xxnjde flag to report payrolls by  *
     H*                location if the policy has any non-NCCI states,*
     H*                instead of just NJ/PA/DE.                      *
     H*                                                               *
     H* 05/14/14 C:300 Project: WC2251                                *
     H*                NY Assessment - Reporting when zero            *
     H*                Write an exposure record for New York assessment
     H*                0932 even when there is no exposure amount.    *
     H*                                                               *
     H* 05/08/14 C:279 Project: WC2287                                *
     H*                Modified to include the class suffix field     *
     H*                when writing exposure records for CA payroll   *
     H*                records that have an extension.  If CA and no  *
     H*                extension then populate suffix with '00' for   *
     H*                the primary wording value.                     *
     H*                                                               *
     H* 04/17/14 C:283 Support: WA133 84683                           *
     H*                When selecting the entities for printing on the*
     H*                Policy Page, if an entity is having the same   *
     H*                info as some other entity but the only         *
     H*                difference is the Physical address, it gets    *
     H*                skipped. Corrected the logic to compare the    *
     H*                Physical address also when selecting the       *
     H*                Entity/Location for Printing.                  *
     H*                                                               *
     H* 04/10/14 C:65  Project: WC2277                                *
     H*                Call the generate notice procedure for         *
     H*                creating records for a notification email.     *
     H*                                                               *
     H* 03/17/14 C:302 Support: WA099 80852                           *
     H*                Changed the overlay logic for the logo from    *
     H*                to use the documen definition in group aux     *
     H*                instead of the commercial flag in group        *
     H*                master if resolved as PDF.                     *
     H*                                                               *
     H* 03/05/14 C:242 Support: WA133 84212, 84565                    *
     H*                If a Deleted Division is added in back as a    *
     H*                location/entity, then use the Effective &      *
     H*                Deleted dates while comparing to existing      *
     H*                Divisions                                      *
     H*                                                               *
     H* 01/14/14 C:136 Project: WC2034 (10/2013 WCPOLS Updates)       *
     H*               -Reimplement WC2006 (New parms in SRADDR)       *
     H*               -Zero out Exp Mods in the WCPOLS header         *
     H*                record if the policy is merit rated            *
     H*                                                               *
     H* 03/05/14 C:242 Support: WA133 84212, 84565                    *
     H*                If a Deleted Division is added in back as a    *
     H*                location/entity, then use the Effective &      *
     H*                Deleted dates while comparing to existing      *
     H*                Divisions                                      *
     H*                                                               *
     H* 11/22/13 C:279 Project: 129WSB                                *
     H*                Implement project 129WAE changes for client    *
     H*                flagged '129'.                                 *
     H*                                                               *
     H* 11/14/13 C:282 Project: WC2233                                *
     H*                For the state PA/DE/MA if there are no         *
     H*                rating changes then report the transaction     *
     H*                with transaction code 10 instead of 08.        *
     H*                                                               *
     H* 10/07/13 C:190 Project: 25WSS                                 *
     H*                Change the program to determine if             *
     H*                Premium or Contribution should be printed      *
     H*                based on Group setup.                          *
     H*                                                               *
     H* 10/08/13 C:65  Project: WC1957                                *
     H*                Add coding to report the harbor exposure       *
     H*                balance to minimum record if a stat code has   *
     H*                been entered on the harbor exposure file.      *
     H*                                                               *
     H* 08/12/13 C:190 Project: WC2130                                *
     H*                Flag any state premium records in WXNHP with   *
     H*                the deletion date if that state has been       *
     H*                removed from the policy.                       *
     H*                                                               *
     H* 07/15/13 C:282 Project: WC2211                                *
     H*                To print the terrorism rates from the new      *
     H*                employer detail fields ( ELTERR and ELDTRR)    *
     H*                if populated.                                  *
     H*                                                               *
     H* 07/30/13 C:190 Project: 147WBW                                *
     H*                Print the schedule of forms for Optacomp       *
     H*                groups (client flagged for Optacomp).          *
     H*                                                               *
     H* 05/29/13 C:180 Support  WC093 82397                           *
     H*                Assign NAIC instead of SIC for locations       *
     H*
     H* 05/14/13 C:180 Support  WA093 82211                           *
     H*                Do not allow misc change for WI.               *
     H*                                                               *
     H* 03/21/13 C:283 Support: WA093 81118                           *
     H*                Correct the printing of form number on next    *
     H*                page with blank data when printing extension   *
     H*                of information.                                *
     H*                                                               *
     H* 02/08/13 C:190 Project: 140WAU                                *
     H*                Deposit on Installments.                       *
     H*                                                               *
     H* 01/08/13 C:283 Support: WA102 81220                           *
     H*                For Clt#102, changed the number of prints to   *
     H*                one from two that come out on the policy page. *
     H*                                                               *
     H*                                                               *
     H* 11/21/12 C:65  Project: WC2116                                *
     H*                Change concatenation of names to be a straight *
     H*                movel/move into the 100A fields to remove any  *
     H*                manipulation.  Transform will have one 100A    *
     H*                field for names so it will be reported as such.*
     H*                NANAM4 will remain as it is since it combines  *
     H*                multiple 100A names together.  Corrected WMANP *
     H*                coding to use NANAM3 instead of NANAM2 for 2nd *
     H*                part of the name (NANAM2 is only 40A in size). *
     H*                                                               *
     H* 11/20/12 C:65  Project: SY195                                 *
     H*                Change document ID passed to DA700R to have    *
     H*                '@' in order for new SY195 field to be correct.*
     H*                                                               *
     H* 10/19/12 C:283 SUPPORT WA089 80873                            *
     H*                Correct the Condition for Calculation of       *
     H*                the Flat Rate Adjustment.                      *
     H*                                                               *
     H* 10/17/12 C:136 Support: WA133 80908                           *
     H*                Do not update 'Policy Generated Date' in       *
     H*                New Business or Renewal Tracking               *
     H*                                                               *
     H* 07/26/12 C:136 Support: WA133 79330                           *
     H*                Allow future deletion dates to be printed      *
     H*                and reported                                   *
     H*                                                               *
     H* 03/26/12 C:180 Support WA139 79499
     H*                Fix add'tl named insured not printing.
     H*                                                               *
     H* 03/13/12 C:180 Support 79499
     H*                Fix infinite loop over wmanp
     H*                Changes not commented out because they
     H*                were work in progress by MD.
     H*
     H* 03/05/12 C:65  PROJECT: Supp# WC139/76362                     *
     H*                If a state has been added to the state list    *
     H*                w/o an associated status code, then consider   *
     H*                it a new state and set its status to O or R.   *
     H*                                                               *
     H* 12/13/11 C:191 SUPPORT 78728 WA089                            *
     H*                Correct error in IL flat rate calculations when*
     H*                multiple IL divisions exist                    *
     H*                                                               *
     H* 02/08/12 C:136 Support: WA132 77971                           *
     H*               -Correct reporting of deletion dates            *
     H*               --Non pending Cx dates need to be reported      *
     H*               --Account for midterm endorsement eff date      *
     H*                 for div, loc's, and addl names per JH         *
     H*               --Use separate variables to report and print    *
     H*                 effective and expiration dates                *
     H*               -Update eff date range for name/addresses on    *
     H*                policy page printout.                          *
     H*               -Correct name link increment logic (75444       *
     H*                 propagated from V10 incorrectly)              *
     H*               -Do not concatenate PEO name to dba name for    *
     H*                 PEO output flag '3' per 77700                 *
     H*               -Free up *in33 - it isn't used anymore to       *
     H*                indicate cx status                             *
     H*                                                               *
     H*
     H* 12/13/11 C:061 project 132WQH
     H*                clt132 Policy page signature (Bryan Bogardus).
     H*
     H* 12/06/11 C:279 Support: 78283                                 *
     H*                Modify to display Rate for split for Terrorism.*
     H*                                                               *
     H* 10/17/11 C:277 Project: SY195                                 *
     H*                QDLS migration to IFS                          *
     H*                Add two parameters with command                *
     H*                                                               *
     H* 11/10/11 C:180 SUPPORT 75300
     H*                Re-compiled only - Change prtf W7413OFL & W7413OCLT
     H*
     H* 11/01/11 C:180 SUPPORT 78074
     H*                Do not write ZERO class for ded class          *
     H*
     H* 10/13/11 C:190 Project: WC2079                                *
     H*          C:191                                                *
     H*                IL Premium Adjustment eff 9-1-11               *
     H*                                                               *
     H* 09/27/11 C:191 Project: 114WAZ                                *
     H*                Add drawer parameter final output              *
     H*                                                               *
     H* 09/13/11 C:136 Support: WA093 77912                           *
     H*                Don't process divisions that do not have a     *
     H*                current policy year                            *
     H*                                                               *
     H* 10/10/11 C:180 SUPPORT 77606
     H*                Change employee leasing ID for
     h*                state of MN when the user-entered value is
     H*                not between 1 and 7. See addt'l conditions.
     H*
     H* 08/23/11 C:65  PROJECT: Supp# 77525                           *
     H*                Reset employer detail before updating WMSHP.   *
     H*                                                               *
     H* 08/22/11 C:136 Project: WC1918                                *
     H*               -Inhibit WC1918 for clients with mod code       *
     H*                (065 and 093)                                  *
     H*               -Make WDF4L user open for backwards             *
     H*                compatibility                                  *
      *                                                               *
      * 08/21/11  C:33  Project: WC1918   (114WZC/Priss 22)           *
      *                -Populate xxf4ky correctly (left adjusted).    *
     H*
     H* 08/16/11 C:061 PROJECT 114WZW
     H*                Use expanded Minimum Prem field (if available).
     H*                                                               *
     H* 08/06/11 C:190 Project: WC2062                                *
     H*                Write electronic endorsement work  record      *
     H*                for HC record type.                            *
     H*                                                               *
     H* 07/27/11 C:062 Support: WC139 77229                           *
     H*                Check to see if Merit rated if so then use that*
     H*                code.                                          *
     H*                                                               *
     H* 07/06/11 C:191 Support: WA132 77313                           *
     H*                Fix bug in 76592 fix                           *
     H*                                                               *
     H* 06/18/11 C:189 Support: WC136 77319                           *
     H*                Print 4 digit class with no zero suppression   *
     H*                for CA                                         *
     H*
     H* 05/30/11 C:189 Support: WC114 72000                           *
     H*                Report the WC370310C ENDORSEMENT electronically.
     H*                                                               *
     H* 05/25/11 C:136 Support: WA132 76102                           *
     H*                Update endorsement file WXPEP when called      *
     H*                in update only mode                            *
     H*                                                               *
     H* 05/22/11 C:190 Project: 114WZD                                *
    H*                Controled printing of countersign line for PA  *
    H*                dec page from Group Auxillary setup            *
     H*                                                               *
     H* 05/17/11 C:191 Support: WA139 76592                           *
     H*                Report cancelled divisions data to NCCI        *
     H*                Do not include flat cancelled division/location*
     H*                /named insured in reporting or policy page     *
     H*                                                               *
     H* 05/17/11 C:191 Project: WC1964 Priss #15                      *
     H*                Print cancelled divisions on policy page, with *
     H*                effective and expiry dates                     *
     H*                                                               *
     H* 05/04/11 C:136 Support: WA089 76454                           *
     H*                Address policy dates in WXNAP need to be       *
     H*                updated so that div/loc prints hard copy      *
     H*                and reports WCPOLS correctly.                  *
     H*                                                               *
     H* 04/21/11 C:136 Support: WA133 76656                           *
     H*                Mass Policy Page is printing location          *
     H*                addresses from previous employers              *
     H*                                                               *
     H* 04/05/11 C:136 Support: WA132 76334                           *
     H*                Align payroll description on CA page in        *
     H*                QSYSPRT1                                       *
     H*                                                               *
     H* 03/25/11 C:191 Project: WC1964                                *
     H*                Change policy page to print deleted/cancelled  *
     H*                locations/divisions and named insured on       *
     H*                schedule of operations                         *
     H*                                                               *
     H*                BUG FIX: Policy date comparison was always with*
     H*                last division eff and end date. Added fix to   *
     H*                always retrieve current division from WDELP    *
     H*                                                               *
     H* 03/16/11 C:190 PROJECT: 132WPI                                *
     H*                Print the new optional heading field for the   *
     H*                contractor's credit line item if it is not     *
     H*                blank(client 132 only).                        *
     H*                                                               *
     H* 03/11/11 C:190 PROJECT: 132WPN                                *
     H*                Do not report the exposure for 1st deviation   *
     H*                if the stat code is not entered.               *
     H*                                                               *
     H* 03/04/11 C:65  Project: 133WIU/supp# 76082                    *
     H*                Only run the re-attach program in turn 1, as   *
     H*                no processing occurs in turn 2 to restore the  *
     H*                endorsements that the policy page itself       *
     H*                attaches.                                      *
     H*                                                               *
     H* 03/01/11 C:180  PROJECT WC1918
     H*                 Check for existance of project WC1918.
     H*
     H* 02/07/11 C:255 PROJECT: 133WJW Priss#38                       *
     H*                 Duplicate records were being printed on Policy*
     H*                 page, added code to populated Entity/Location *
     H*                 and Division fields to WXA5P file.            *
     H*                                                               *
      * 02/01/11 C:136  Support: WA093 75589                          *
      *                 Correct printing of locations                 *
      *                                                               *
      * 01/20/11  C:246 support 75444 - CLT - WA071                   *
      *                 Name Address link fields were not being reset *
      *                 after each policy which causing overflow error*
      *                                                               *
      * 01/19/11  C:180 SUPPORT 75180
      *                 Move 'EXTENSION OF INFORMATION PAGE FOR:'
      *                 heading 2 spaces to the right.
      *
     H* 01/11/11 C:190 Support: 75053                                 *
     H*                For Clt#139, changed the number of prints to   *
     H*                one from two that come out on the policy page. *
     H*                                                               *
     H* 01/10/11 C:190 PROJECT: 132WOY                                *
     H*                Modify so that form NC5000 does not print      *
     H*                in list of attached endorsements (Section 3D). *
     H*
     H* 01/10/11-B C:65  PROJECT: 139WPL Priss #95                      *
     H*                  Ensure stat code prints for HCN on state page. *
     H*                                                               *
     H* 01/04/11  C:62  support 74886
     H*                 Correct Month on CCS Reports.
     H*                                                               *
     H* 12/31/10 C:255 PROJECT: WC1918                                *
     H*                Use fields(F4TXT1, F4TXT2) from WDF4P file     *
     H*                instead of file WDF2P.                         *
     H*                                                               *
     H* 12/14/10 C:255 PROJECT: 133WJW                                *
     H*                Added code to print the schedule of operations *
     H*                in location order.                             *
     H*                                                               *
     H* 12/10/10 C:255 PROJECT: WC1939 (Priss#19)                     *
     H*                Value of array index is corrected for PDAT &   *
     H*                RDAT.                                          *
     H*                                                               *
     H* 11/18/10 C:136 Support: WA139 74791                           *
     H*                Standardize GIC name formats                   *
     H*                Print Fed and DBA names on seperate lines      *
     H*                                                               *
     H* 11/10/10 C:65  PROJECT: 139WPL                                *
     H*                Add new line item for HCN credit for GIC.      *
     H*                This credit will be applied just before subject*
     H*                premium on schedule of operations.  The credit *
     H*                will also be reported as an exposure record.   *
      *                                                               *
     H* 11/08/10 C:190 PROJECT: WC1899 (Priss#25)                     *
     H*                Report correct Terrorism codes.                *
     H*                                                               *
      * 10/28/10  C:61  support wa139 74670
      *                 Correct print of address on DEC page.
     H*                                                               *
     H* 10/18/10 C:251 Project: WC1939                                *
     H*                09/01/2010 OK Rate split                       *
     H*                If a policy with OK coverage is unexpired as   *
     H*                of 09/01/10 then the policy will need to be    *
     H*                split for the rate decrease                    *
     H*                                                               *
     H* 10/05/10  C:189 PROJECT WC1776                                *
     H*                 Modify the code to show decimal places for    *
     H*                 payroll amounts in the policy page            *
     H*                                                               *
     H* 10/04/10 C:190 Seq# 74343                                     *
     H*                DBA acronym needed at end of first line name.  *
     H*                                                               *
     H* 09/17/10 C:220 PROJECT: 132WOM                                *
     H*                For client 132 add DISCLOSR form in endorsement*
     H*                table.                                         *
     H*                                                               *
     H* 09/07/10 C:255 PROJECT: WC1929                                *
     H*                Add changes in program to check flag 'Disable  *
     H*                DE Rate flag' based on this, changes for proje-*
     H*                ct WC1768 are disabled.                        *
     H*                                                               *
     H* 08/17/10 C:190 PROJECT: WC1899 (Priss#25)                     *
     H*                Report correct Stat codes for separate records.*
     H*                                                               *
     H* 07/20/10 C:190 Seq# 73596                                     *
     H*                Corrected Seq#71449 changes.                   *
     H*                For Clt#136, changed the number of prints to   *
     H*                one from two that come out on the policy page. *
     H*                                                               *
     H* 07/13/10 C:65  Project: 139WLR/SUPP#71516                     *
     H*                Ensure address line does not print twice.      *
     H*                                                               *
     H* 07/06/10 C:191 PROJECT: WC1899                                *
     H*                Report separate records for terrorism and      *
     H*                assessment amounts if rate changes between     *
     H*                periods                                        *
     H*                                                               *
     H* 06/21/10 C:255 Project: 114WVY                                *
     H*                Write 2nd period deviations exposure records.
     H*                ADD CHANGES IN PROGRAM FOR 2ND PERIOD DEVIATION*
     H*                WHEN DEVIATIONS ARE SPLIT. 2ND PERIOD DEVIATIO-*
     H*                NS CHANGES WILL APPLY FOR (M,J,E,S,D) PREMIUM  *
     H*                LEVELS AS IT IS FOR 1ST PERIOD DEVIATIONS.     *
     H*                                                               *
     H* 06/03/10 C:220 Project: 133WIU                                *
     H*                Re-attach endorsements prior to processing     *
     H*                                                               *
     H* 05/07/10 C:136 Project: WC1861 Priss 5                        *
     H*                Requirements changed for project               *
     H*                                                               *
     H* 05/06/10 C:190 Seq# 72476                                     *
     H*                Corrected incorrect printing of Entity/Loc     *
     H*                under Schedule of Locations.                   *
     H*                                                               *
     H* 05/07/10 C:65  PROJECT: 132WOC                                *
     H*                Add agent number and phone number to agent     *
     H*                info box.  Additionally, hard code the item 3C *
     H*                information to print a specific message.       *
     H*                                                               *
     H* 04/23/10 C:136 Project 25WQK PRISS                            *
     H*                Correction to display net rate on CCS packet   *
     H*                                                               *
     H* 03/29/10 C:190 Seq# 72141                                     *
     H*                Federal Name and DBA Name should print         *
     H*                on different lines.                            *
     H*                                                               *
     H* 03/22/10 C:65  PROJECT: 132WOC                                *
     H*                Add processing to produce a new policy dec     *
     H*                page along with new schedules for groups       *
     H*                105, 106, and 107 for CompWest.  Each group    *
     H*                will have its own logo page segment.           *
     H*                                                               *
     H* 03/13/10 C:235 PROJECT: 102WFC                                *
     H*                ADDITION CLASSES FOR LOGGING CALC              *
     H*                                                               *
     H* 03/12/10 C:062 Project: 25WQK                                 *
     H*                CCS Packets                                    *
     H*                                                               *
     H* 03/04/10 C:220 SUPPORT: 71217                                 *
     H* 05/26/10        Clear FEIN id for other work places.          *
     H*
     H* 03/03/10 C:235 PROJECT: 114WWG                                *
     H*                Heart and Lung Group 101                       *
     H*                                                               *
     H* 02/24/10 C:190 Support: 71449                                 *
     H*                For Clt#136, changed the number of prints to   *
     H*                one from two that come out on the policy page. *
     H*                                                               *
     H* 02/04/10 C:235 Support: 71498                                 *
     H*                "Taxpayer Identification"for NJ state needs to *
     H*                be pulled from State Unemployment field for GIC*
     H*                                                               *
     H* 01/14/10 C:062 PROJECT: 136WEH                                *
     H*                Add Privacy Notice.                            *
     H*                                                               *
     H* 01/12/10 C:65  PROJECT: WC1838                                *
     H*                LARGE DEDUCTIBLES - USE NEW FILE FOR LARGE     *
     H*                DEDUCTIBLE POLICIES, BASED ON BILLING SCHED CD.*
     H*                                                               *
     H* 01/11/10 C:189 PROJECT: 139WNO                                *
     H*                Suppress Ullico Privacy Notice from Prt        *
     H*                                                               *
     H* 12/30/09 C:180 SUPPORT 71103 - Incorrect limits printing
     H*                                                               *
     H* 12/29/09 C:136 PROJECT: WC1861                                *
     H*                ADD AZ RATE SPLIT - 01/01/2010                 *
     H*                                                               *
     H* 12/21/09 C:65  PROJECT: 139WNK                                *
     H*                Do not include safety fees in Fees & Taxes     *
     H*                line item for GIC.                             *
     H*                                                               *
     H* 11/23/09 C:136 PROJECT: 139WNR                                *
     H*                FL RATE SPLIT FOR 07/01/09 - CLIENT SPECIFIC   *
     H*                                                               *
     H* 11/12/09 C:61  SUPPORT: 70686                                 *
     H*                 Do not print to QSYSPRT1 when NJ.             *
     H*                                                               *
     H* 10/27/09 C:220 SUPPORT: 70246                                 *
     H*                 File# on policy page need to replaced by      *
    H*                 Policy# for NJ.                               *
    H*                                                               *
     H* 10/09/09  C:65  PROJECT: SUPP#69991                           *
     H*                 Correct indicator to print manual premium on  *
     H*                 summary page.                                 *
     H*                                                               *
     H* 09/07/09  C:61  Project 139WLR                                *
     H*                 Write "Deposit premium" and "Total Estimated  *
     H*                 premium" amounts to data area for clt139      *
     H*                 mid-term endorsement WC990416 processing.     *
     H*                                                               *
     A* 08/25/09 C:136 PROJECT: 135WMR (CLIENT SPECIFIC)              *
     A*                ADD RECORD FORMATS FOR BANCINSURE'S            *
     A*                DEC PAGE, RATING SCHEDULE, ETC.                *
     A*                                                               *
     H* 08/24/09  C:190 PROJECT: 135WNF                               *
     H*                 Replace contents of routine ADJNAMES          *
     H*                 with call to WA722R.                          *
     H*                                                               *
     H* 08/20/09  C:33  Seq#: WA135/69570                             *
     H*                 Check for existence of overlay prior to       *
     H*                  overriding.                                  *
     H*                                                               *
     H* 08/20/09  C:65  PROJECT: SUPP#69554                           *
     H*                 Ensure W7413OLP is opened/closed when OR      *
     H*                 notice needs to be printed.                   *
     H*                 Additionally, alter the read statements in    *
     H*                 printAddress subroutine to make them more     *
     H*                 reliable.                                     *
     H*                                                               *
     H* 08/14/09  C:190 PROJECT: 135WNC                               *
     H*                 The endorsements have non-standard names &    *
     H*                 will list as "BI DED CA 05 09",               *
     H*                 "BI DED IL 05 09", and "BI DED IL 05 09.      *
     H*                                                               *
     H* 08/13/09  C:190 PROJECT: 135WNB                               *
     H*                 'BIDED0509' will list as 'BI DED 05 09' on    *
     H*                  on list of attached endorsement forms.       *
     H*                                                               *
     H* 08/06/09  C:65  PROJECT: 139WLR                               *
     H*                 Add Ullico format schedules for Ullico        *
     H*                 policies along with privacy notice.           *
     H*                                                               *
     H* 07/22/09  C:65  PROJECT: WC1701                               *
     H*                 Only print the leasing ID for PEO groups.     *
     H*                                                               *
     H* 07/15/09  C:65  PROJECT: WC1701 Priss 13                      *
     H*                 Change the summary/detail payroll flag to the *
     H*                 following settings:                           *
     H*   67/68 Off     Blank - prints like base w/o changes          *
     H*   *IN67 On      Detail - Summary page as base, state pages    *
     H*                          broken down by location with div #.  *
     H*   *IN68 On      Summary - Summary and State pages combine     *
     H*                           Payrolls with same efdt/from/to dt. *
     H*                                                               *
     H* 07/10/09  C:65  PROJECT: 139WLR                               *
     H*                 Ensure ullico format is used for all types of *
     H*                 ullico policies.  In addition, do not print   *
     H*                 the group address for ullico policies.        *
     H*                                                               *
     H* 06/30/09  C:190 Support : 68941                               *
     H*                 Corrected the issue where the bottom of       *
     H*                 form's output overrites the footer text under *
     H*                 Extension schedule.                           *
     H*                                                               *
     H* 06/29/09 C:136 SUPPORT: WA139 68951                           *
     H*                ENSURE DELAWARE FORM PRINTS TO NEW PAGE        *
     H*                                                               *
     H* 06/19/09  C:61 SUPPORT WA114 68427                            *
     H*                Clt114 with PA coverage and LOGO printing      *
     H*                FORMP corrected.                               *
     H*                                                               *
     H* 06/18/09  C:65  PROJECT: WC1701 Priss 13                      *
     H*                 Replace the coding used for printing summary  *
     H*                 vs detail on the payroll classes. For summary *
     H*                 payroll, it will print as it currently does.  *
     H*                 For detail payroll, it will print both the    *
     H*                 summary page and the state pages from the NJ  *
     H*                 data set (epelcd <> 0), which is broken down  *
     H*                 by location.                                  *
     H*                 To accomplish this the section which combined *
     H*                 and then reported payrolls for NJ/DE has been *
     H*                 moved to after the STPAGE subroutine call.    *
     H*                                                               *
     H* 06/17/09  C:65  PROJECT: SUPP#55872/WC1633                    *
     H*                 Update the coding for deleted names/addresses *
     H*                 to recognize when a name is set as deleted    *
     H*                 and another instance of that name is active.  *
     H*                 Additionally, if a name is deleted and        *
     H*                 another instance has a later deletion date,   *
     H*                 update with that date.                        *
     H*                                                               *
     H* 05/11/09  C:61 PROJECT 93WHW                                  *
     H*                For clt093:                                    *
     H*                 - Print Nature of Business in place of "Other"*
     H*                   when Type of Business = '4'.                *
     H*                 - Print "A stock company" for groups 790, 830 *
     H*                   835, 840 and 900.                           *
     H*                 - Enable NCCI Submissions for non-NCCI states *
     H*                   and control submission based upon Iter or   *
     H*                   Intra-state rated settings.                 *
     H*                                                               *
     H* 05/07/09  C:65  PROJECT: 139WLR                               *
     H*                 ADD NEW POLICY DEC PAGE FOR ULLICO POLICIES.  *
     H*                                                               *
     H* 05/05/09  C:65 PROJECT 139WLU                                 *
     H*                Various changes to text for W7413OFL dec page. *
     H*                                                               *
     H* 04/29/09 C:65  Project: 114WTU                                *
     H*                Print full name for the expanded additional    *
     H*                location names.                                *
     H*                                                               *
     H* 04/28/09 C:65  Project: Support                               *
     H*                Fix the reporting of expanded employer names   *
     H*                to prevent extra spaces in the middle.         *
     H*                                                               *
     H* 04/27/09 C:191 Support: WA136 65348                           *
     H*                Changes to additional named insured printing   *
     H*                                                               *
     H* 04/23/09 C:180 SUPPORT
     H*                Override minimum premium with Estimated Normal
     H*                Premium when minimun premiun is invoked for
     H*                client 102 - REF prev pgm version, Rel ARC102,
     H*                for client 102 before client moved to base.
     H*
     H* 04/22/09 C:65  Project: Support                               *
     H*                Report waiver surcharge as exposure.           *
     H*                                                               *
     H* 04/21/09 C:65  Project: Supp  WA114 66041                     *
     H*                Apply the LCM to the terrorism rates before    *
     H*                reporting or printing the rates.               *
     H*                                                               *
     H* 04/20/09 C:190 PROJECT WC1473                                 *
     H*                Use the new logical(s) to get the description  *
     H*                by extension.                                  *
     H*                                                               *
     H* 04/16/09 C:65  Project: Supp  WA139 68066                     *
     H*                Perform an except on the WXNAP file before     *
     H*                writing a new record, in case the name/link    *
     H*                update program WARSTNLKC is called, to release *
     H*                any file lock.                                 *
     H*                                                               *
     H* 04/15/09 C:65  WC1701 Support WA139 68066                     *
     H*                Only call the printAddress subroutine when     *
     H*                hardcopy is requested, to avoid writing to     *
     H*                unopened printer files.                        *
     H*                                                               *
     H* 04/13/09 C:190 Support 67961                                  *
     H*                Fix the issue where the complete NAICS code    *
     H*                was not printing on policies with Multiple     *
     H*                Locations.                                      *
     H*                                                               *
     H* 04/10/09 C:61  WC1701 Support wa139 68003                     *
     H*                Fix so does not attempt to write QSYSPRT1 when *
     H*                QSYSPRT1 is not open.                          *
     H*                                                               *
     H* 03/25/09 C:191 WC1701  Priss#10                               *
     H*                Data not printing correctly                    *
     H*                                                               *
     H* 03/13/09 C:191 WC1701  Priss#9                                *
     H*                Additional locations not printing              *
     H*
     H* 03/09/09 C:136 Project: 114WTJ                                *
     H*                Print Delaware Deductible Form                 *
     H*
     H* 03/02/09 C:65  PROJECT: 81WEI                                 *
     H*                Use optional headings for terrorism charges.   *
     H*                                                               *
     H* 02/18/09 C:197 WC1701  Priss#8                                *
     H*                                                               *
     H* 02/16/09 C:197 WC1701  Priss#7                                *
     H*                                                               *
     H* 02/12/09 C:220 SUPPORT: 64480                                 *
     H*                Policy# is replaced by File# on policy page    *
    H*                                                               *
     H* 01/29/09 C:65 PROJECT: SUPP#66756                             *
     H*               Ensure additional named insured reporting is    *
    H*               only done when electronic reporting is needed.  *
    H*                                                               *
     H* 01/22/09 C:197 WC1701  Priss#6                                *
    H*                                                               *
     H* 01/19/09 C:65  PROJECT: supp#  66041                          *
     H*                Print terrorism rates on schedule of operations*
    H*                                                               *
     H* 01/09/09 C:33  Seq#: WA139/66612                              *
    H*                Ensure pages are opened correctly.             *
     H*                                                               *
     H* 12/16/08 C:136 PROJECT: WC1768                                *
     H*                DE RATE SPLIT FOR 10/01/08                     *
    H*                                                               *
     H* 12/12/08 C:190 Supp#: 65696                                   *
    H*                Resolved policy page issue for GIC             *
    H*                                                               *
     H* 12/11/08 C:33  Project; WC1769                                *
     H*                Add 6th and 7th assessments.                   *
    H*                                                               *
     H* 12/04/08 C:61  Project: 136WDU                                *
     H*                Add print of "Loss Control Letter" for clt136. *
    H*                                                               *
    H* 11/21/08 C:136 Project: 45WHA                                 *
    H*                Change logo and remove verbiage per client     *
    H*                request                                        *
    H*                                                               *
    H* 11/14/08 C:65  PROJECT: Supp# 65838/WC1537                    *
    H*                Do not add expense constant into ANNPREM, as   *
    H*                it already includes it from TAM(10).           *
    H*                                                               *
     H* 11/06/08 C:61  Project: 135WJZ  PRISS 26                      *
     H*                Correct printing issues.                       *
    H*                                                               *
     H* 10/21/08 C:65 PROJECT: Supp#65580  (replaced by 4/13/09 chg)  *
     H*               Expand xonaid field to hold entire naics value. *
    H*                                                               *
     H* 10/10/08 C:189 Project: 135WLA  PRISS 8                       *
     H*                When large noe of divisions in place some      *
     H*                Loss prevention notices getting missing        *
    H*                                                               *
    H* 10/01/08 C:197 Project: WC1701                                *
    H*                Change policy page program to print state      *
    H*                premium pages in state abbreviation order      *
    H*                and locations alphabetically by state          *
    H*                abbreviation and city name within state.       *
    H*                                                               *
     H* 09/23/08 C:65 PROJECT: Supp#65231                             *
     H*               Call STPAGE subroutine even when no hardcopy is *
     H*               needed in order to report payroll exposure      *
     H*               electronically for non nj/de/pa policies.       *
    H*                                                               *
    H* 09/23/08 C:136 Project: WA102 65290                           *
    H*                Make WC1627 (Net Rate) backwards compatible    *
    H*                for clients that have modified code and        *
    H*                have not implemented it.                       *
    H*                                                               *
     H* 09/22/08 C:65 PROJECT: Supp#65231                             *
     H*               Ensure PEO flag is set properly on additional   *
     H*               named insured.                                  *
     H*                                                               *
     H* 09/15/08 C:65 PROJECT: 114WGH/Supp#64386                      *
     H*               Report the domestic terrorism charge even when  *
     H*               zero to allow it to report for certain states.  *
     H*                                                               *
     H* 08/28/08 C:65 PROJECT: 114WGH                                 *
     H*               REPORT THE WC290601A ENDORSEMENT ELECTRONICALLY.*
     H*                                                               *
     H*  08/12/08 C:65 PROJECT: 146WAP                                *
     H*                ADD REPORTING REQUIREMENTS FOR THE WC000412    *
     H*                ENDORSEMENT.                                   *
    H*                                                               *
    H* 08/08/08 C:65  PROJECT: WC1425(Priss# 10) (SUPP#64516)        *
    H*                Chain back to state reporting div WDELP to     *
    H*                keep emplr detail information valid.           *
    H*                                                               *
    H* 07/17/08 C:197 Project: 135WJZ   PRISS#25                     *
    H*                Not to Print the States IL, GA, PA TN, NJ and  *
    H*                FL if WC 99 07 10 PPC/ICI is printing          *
    H*                                                               *
    H* 06/25/08 C:65  PROJECT: 114WRH                                *
    H*                Print attached schedule for WC000503A if the   *
    H*                group is set to process retro across group.    *
    H*                                                               *
    H* 06/20/08 C:33  Project: WC1570   SY156/PRISS#8                *
    H*                Print risk id for state reporting division     *
    H*                                                               *
    H* 06/19/08 C:197 PROJECT: C135WJZ Priss#20                      *
    H*                Replace General Deductible Form                *
    H*                                                               *
    H* 06/03/08 C:197 PROJECT: C135WJZ                               *
    H*                Replace General Deductible Form                *
    H*                                                               *
     H* 05/19/08 C:189 Project: 135WLA                                *
     H*                Add Loss Prevention notices to Policies        *
     H*                for TX, AR, KS, OK, OR, PA. The Loss prevention*
     H*                will only print for the state divisions not    *
     H*                Cancelled                                      *
    H*                                                               *
    H* 05/22/08 C:65  PROJECT: WC1537/Support                        *
    H*                Change process to include exp cnst if there is *
    H*                any coverage in FL and remove service program  *
     H*                call as it was not working interactively.      *
     H*                                                               *
     H* 05/15/08 C:180 PROJECT CI055
     H*                Write PDF to TEST FOLDER when running in Test env.
     H*
     H* 05/05/08 C:65 PROJECT: SUPP#63309                             *
     H*               Save off the names for CA dec page to prevent   *
     H*               additional divisions from overwriting them      *
    H*               before the dec page is printed.                 *
    H*                                                               *
    H* 05/02/08 C:197 PROJECT: C135WLF                               *
    H*                Create FL Large Deductible form                *
    H*                                                               *
    H* 04/30/08 C:190 PROJECT: WC1425(Priss# 10)                     *
    H*                AR notice must be generated if ANY division    *
    H*                on the policy has coverage in AR.              *
    H*                                                               *
    H* 04/18/08 C:61  PROJECT: WC1537                                *
    H*                If state reporting division has coverage in FL *
    H*                then include "Expense Constant" in "Total      *
    H*                Estimated Annual Premium".                     *
     H*                                                               *
     H* 04/18/08 C:65 PROJECT: Supp#62860                             *
     H*               Prevent hardcopy printing of additional named   *
     H*               when no hardcopy has been requested.            *
     H*                                                               *
     H* 04/03/08 C:65 PROJECT: Supp#62259                             *
     H*               Treat all locations/entities attached to a      *
     H*               deleted division as being deleted themselves.   *
     H*                                                               *
     H* 04/01/08 C:183 PROJECT: WC1619                                *
     H*                Call SREFFD to set the effective date to be    *
     H*                used on SETGT.                                 *
     H*                                                               *
     H* 03/17/08 C:65 PROJECT: 114WAK                                 *
     H*               REPORT THE WC370310B ENDORSEMENT ELECTRONICALLY.*
    H*                                                               *
    H* 03/12/08 C:65  PROJECT: 114WGH                                *
    H*                Print full 100 character names for divisions   *
    H*                on the schedule of operations.  Redo name      *
    H*                output specs to avoid blank lines.             *
    H*                                                               *
    H* 03/11/08 C:65  PROJECT: 114WGH                                *
    H*                Remove the checking of address on additional   *
    H*                location against division address since name   *
    H*                on addl location can be different now.         *
    H*                                                               *
    H* 03/10/08 C:65  PROJECT: 114WGH/Supp#62452                     *
    H*                Move printing of X2ONM2 down to the second     *
    H*                address lines and remove *IN71 restriction,    *
    H*                which was unnecessary for 2nd address.         *
    H*                                                               *
    H* 03/09/08 C:65  PROJECT: 114WGH                                *
    H*                Correct duplicate name checking to disregard   *
    H*                the federal name check when determining if the *
    H*                dba name is a duplicate.                       *
    H*                                                               *
    H* 02/26/08 C:65  PROJECT: 114WGH                                *
    H*                Re-change the writing of additional named      *
    H*                insured back to using unique name link codes.  *
    H*                Add additional coding as well to prevent       *
    H*                duplicate name records from being written.     *
    H*                                                               *
    H* 02/15/08 C:190 PROJECT: WC1425                                *
    H*                Add code to extract AR form information from   *
    H*                the Endorsement Form/Policy Information file   *
    H*                based upon the Policy Rating State and the     *
    H*                Policy Effective Date.                         *
    H*                                                               *
    H* 02/06/08 C:65  PROJECT: WC1633/Supp#62127                     *
    H*                Also check for current WDELP record before     *
    H*                writing/printing names/addresses on divisions. *
    H*                                                               *
    H* 02/04/08 C:65  PROJECT: WC1633/Supp#62127                     *
    H*                Prevent WXNAP records from being written for   *
    H*                divisions or locations deleted prior to the    *
     H*                current year.                                  *
     H*                                                               *
     H* 01/22/08  C:65  PROJECT: WC1627                               *
     H*                 PULL THE NET RATE AND NET PREMIUM FOR EACH    *
     A*                 PAYROLL CLASS FROM THE NEW NET FIELDS.        *
     H*                                                               *
     H* 12/17/07 C:189 PROJECT 109WEN                                 *
     H*                Rating Deviation calculation Renewal credit    *
    H*                                                               *
     H*
    H* 12/14/07 C:180 PROJECT: CI055                                 *
     H*                Use PDF for clients 132, 133, and 136. Name
     H*                PDF file using CO#, Group, Employer.PDF
     H*                file is to be stored on IFS root.              *
     H*
    H* 12/07/07 C:65  PROJECT: 114WGH                                *
    H*                Clear the mailing address header array.        *
    H*                                                               *
    H* 12/05/07 C:65  PROJECT: 114WGH                                *
    H*                Correct NJAUE# subroutine to use the additional*
    H*                location fields instead of employer master.    *
    H*                                                               *
    H* 12/04/07 C:65  PROJECT: 114WGH                                *
    H*                Move the writing of additional named insured   *
    H*                to write last and set all with entity code of  *
    H*                001 to report as contiuations of main policy   *
    H*                name.                                          *
    H*                                                               *
    H* 11/28/07 C:65  PROJECT: 114WGH                                *
    H*                Only set the prorate factor for minimum prem   *
    H*                based on the state reporting division.         *
    H*                                                               *
    H* 11/28/07 C:65  PROJECT: 114WGH                                *
    H*                Cap the number of payroll classes that can     *
     H*                print on a page to prevent certain printers    *
    H*                from cutting off the last payroll class.       *
    H*                                                               *
    H* 11/27/07 C:65  PROJECT: 114WOQ                                *
    H*                Alter countersign line for PA dec page to swap *
     H*                the date and signed lines.                     *
     H*                                                               *
     H* 11/26/07 C:65 PROJECT: 133WCV/Supp# 61235                     *
     H*               Correct the line adding in harbor exposure fee  *
     H*               to the estimated annual premium total, the      *
    H*               current location has the value reset shortly    *
    H*               later.                                          *
    H*                                                               *
    H* 10/31/07 C:65  PROJECT: Supp/SY082                            *
    H*                Report additional named insured with unique    *
    H*                entity code to prevent identical names from    *
    H*                divisions/addl locations from being reported   *
    H*                with their proper addresses.                   *
    H*                                                               *
    H*                                                               *
    H* 10/22/07 C:65  PROJECT: WC1633                                *
    H*                Report all names and addresses for electronic  *
    H*                submission.  Flag any deleted names/addresses  *
     H*                with policy changes eff/exp date fields.  Add  *
    H*                processing to update the state submission code *
    H*                fields as well.                                *
    H*                                                               *
    H* 10/15/07 C:65  PROJECT: 114WOQ                                *
    H*                Do not print expense constant on the schedule  *
    H*                of operations for group 2 PA business for 114. *
     H*                Print GL-HL-1 at bottom of dec page and        *
     H*                schedule of operations pages.                  *
     H*          C:61 -Print endorsement "ACT 477 / LIA FORM1" on all *
     H*                Group 2 policies for clt114.                   *
     H*                                                               *
     H* 10/11/07 C:190 SUPPORT WA099 54504                            *
     H*                MAKE SURE TO DO NOT ACCESS THE DEDUCTIBLE      *
     H*                DISCOUNT CODE HAVING INACTIVE RECORDS.         *
     H*                                                               *
     H*  09/20/07 C:65 PROJECT: WC1249                                *
     H*                Set XXSIX off at start of program for mass run.*
    H*                                                               *
    H* 09/20/07 C:184 PROJECT: C135WJY                               *
    H*                Imperial TN/GA deductible form wording         *
    H*                                                               *
    H* 09/18/07 C:184 PROJECT: C135WKE                               *
    H*                Create IL Large Deductible form                *
    H*                                                               *
    H* 09/13/07 C:184 PROJECT: C135WJX                               *
    H*                Create PA Large Deductible form                *
    H*                                                               *
    H* 09/11/07 C:61  PROJECT: 139WIB                                *
    H*                Various client 139 requested changes.          *
    H*                                                               *
    H* 08/25/07 C:65  PROJECT: Supp# 59097                           *
    H*                Print locations that are active as of print    *
    H*                date, rather than if they had coverage during  *
    H*                the policy year.                               *
    H*                                                               *
    H* 08/24/07 C:65  PROJECT: Supp# 59649                           *
    H*                Ensure blank line prints between locations on  *
    H*                the schedule of locations.                     *
    H*                                                               *
    H* 08/17/07 C:65  PROJECT: WC1425                                *
    H*                The AR notice should only print if the client  *
    H*                id is equal to 135 for PPC.                    *
     H*                                                               *
    H* 07/26/07 C:61  PROJECT: WC1570                                *
     H*                Print "state risk ID#"                         *
     H*                in schedule if flag set in                     *
     H*                reporting rating record.                       *
    H*                                                               *
    H* 07/25/07 C:65  PROJECT: 114WOQ                                *
    H*                Create new Declaration page and Schedule of    *
    H*                Forms for group 2 PA business for client 114.  *
    H*                                                               *
    H* 07/24/07 C:65  PROJECT: SUPP# 59157                           *
    H*                Correct the lost aldon code that was dated     *
    H*                05/12/06 and re-added 6/26/07.  It was missing *
    H*                coding to print employer division fein #'s.    *
    H*                                                               *
    H* 07/21/07 C:65  PROJECT: 139WCI/139WBU                         *
    H*                Print the PEO name L/C/F Client name for       *
    H*                Multiple employer PEO's only and when the      *
    H*                output control flag is set to ' ' or '3'.      *
    H*                                                               *
    H* 07/20/07 C:65  PROJECT: 114WMF/114WDJ                         *
    H*                Remove the 114WDJ change for adding fees to    *
    H*                the premium total on dec page.                 *
     H*                                                               *
     H*  07/11/07 C:65 PROJECT: WC1249                                *
     H*           C:53 ADD PEO CONTROLS.                              *
    H*                                                               *
    H* 07/09/07 C:65  PROJECT: Supp#58842                            *
    H*                Include NC for electronic endorsement reporting*
    H*                                                               *
    H* 07/06/07 C:65  PROJECT: Support                               *
    H*                Update time stamp PETIME to allow endorsement  *
     H*                programs to pull latest created record.        *
     H*                                                               *
     H* 06/26/07 C:132 Project: WC1432                                *
     H*                Insert code from 05/12/06 lost with ALDON      *
     H*                conversion, Included code with 05/12/06 dates  *
    H*                                                               *
    H* 06/26/07 C:65  PROJECT: WC1250/Support                        *
    H*                Populate the rate field for domestic terrorism *
     H*                exposure record.                               *
     H*                                                               *
     H* 06/22/07 C:61  Project: 139WHP                                *
     H*              - Replace signature "Paul Halter" with           *
     H*                "Theodore G. Bryant".                          *
    H*                                                               *
     H*  06/20/07 C:65 PROJECT: 114WGH                                *
     H*                Fix a name field check for duplicate names.    *
     H*                Removed federal entity code check on duplicate *
     H*                names as well as name field check appeared to  *
     H*                make that coding invalid.                      *
     H*                                                               *
     H*  06/04/07 C:65 PROJECT: 132WAY                                *
     H*                Flag policy records with code of 'I' or 'T'    *
     H*                (resubmissions) as sent for CA, as they do     *
     H*                not accept those transactions.                 *
     H*                                                               *
     H*  05/15/07 C:189 PROJECT: WA135/55403                          *
     H*                                                               *
     H*                THE Prorated Premium calculation should        *
     H*                include Expense constant from all Divisions    *
    H*                                                               *
     H*  05/11/07 C:65 PROJECT: 114WGH                                *
     H*                REMOVE THE CHECK ON WCFL14 FOR POPULATING      *
     H*                THE NASIC FIELD.                               *
     H*                                                               *
     H* 05/11/07 C:189 Project: WA139/56081                           *
     H*                Unneccessery message appearing in QSYSOPR      *
     H*                                                               *
     H* 05/09/07 C:65  Project:  WC1524/139WBR                        *
     H*                Revert employer leasing id code to pull from   *
     H*                employer detail, and only check peo flag if    *
     H*                a valid value is not present.                  *
    H*                                                               *
     H* 05/08/07 C:132 Project: 102WEE - Back To Base
     H*                Below Coded for Client 102
     H*              - Suppress leading digit for logging class (47845)
     H*              - Apply Logging rate Adjustments-Elec Rptng (102CN)
     H*              - Add Producer number to print on Pol Page (102BS)
     H*                Below will apply to all clients (BASE)
     H*              - Add Net-rate enhancements (103CO)
     H*                                                               *
     H* 04/13/07 C:65  SEQ#: 114WMF/135WJD/WC1566                     *
     H*                Remove 114 check on override prorata to 1.00.  *
     H*                                                               *
    H* 04/11/07 C:33  Seq#: WA132/57910                              *
    H*                Change heading on Policy to read 'Locations '  *
    H*                 as opposed to 'Operations' for CA policies.   *
     H*                                                               *
     H* 03/30/07 C:180 Project: WA139/52320
     H*                Print policy jacket for all employers when
     H*                mass processing for client 139.
     H*                                                               *
     H* 03/21/07 C:65  SEQ#: 114WGH/114WFI                            *
     H*                WHEN PRINTING CLIENT NAME UNDER A PEO, PRINT   *
     H*                BOTH FEDERAL AND DBA NAMES FOR CLIENT.         *
     H*                ADDITIONALLY FOR NJ, REPORT ALL CLIENT NAMES   *
     H*                CONCATENATED WITH THE PEO NAME + L/C/F.        *
     H*                WXNAP WILL HAVE A NEW NAME FIELD ADDED TO      *
     H*                HANDLE THE FULL POSSIBLE NAME.  THIS FIELD     *
     H*                IS SET TO 360 CHARACTERS TO ALLOW UP TO 4      *
     H*                NAME RECORDS CREATED.                          *
     H*                WRITE PEO NJ TAXID ON ALL NAME RECORDS         *
     H*                CREATED FOR A NJ POLICY.  ADDITIONALLY PRINT   *
     H*                THE PEO NJ TAX ID ON THE DECLARATION PAGE.     *
     H*                                                               *
     H* 03/20/07 C:180 Project: 114WMF WA114/57612
     H*                - Fix sched of oper output. Do not truncate output.
     H*                  Use a new line.
    H*                                                               *
     H* 03/16/07 C:180 Project: 114WMF WA114/57614
     H*                - Print NJTIN# .
     H*                                                               *
     H*  03/14/07 C:65 PROJECT: Support                               *
     H*                Write an address record for dba names on addl  *
     H*                divisions and locations even if that address   *
     H*                record was already written for federal name.   *
    H*                                                               *
    H* 03/06/07 C:33  Project: 45WFB  WA045/57390                    *
    H*                Re-implment projects that were removed with    *
    H*                 this project.                                 *
     H*        45WDD - Add additional verbiage to the policy page     *
     H*                 just under the information page heading       *
     H*        45WES - Change "Fees and Taxes" line on schedule of    *
     H*                 operations to 'Minimum USL&H Charge" in       *
     H*                 table at bottom of code in program.           *
     H*                                                               *
     H*  03/01/07 C:65 PROJECT: 132WAC/Support#56162                  *
     H*                Change servicing address to use the office     *
     H*                mailing address instead of accounting addr.    *
    H*                                                               *
    H* 02/21/07 C:33  Seq#: WA139/57269                              *
    H*               -Correct calc for TAM(41)                       *
    H*               -Only print one listing for each attached       *
    H*                 endorsement                                   *
     H*
    H* 02/16/07 C:180 Seq#: WA139/56774                              *
     H*                - Retrieve missing FEIN's
    H*                                                               *
    H* 12/19/06 C:33  Seq#: WA132/53167                              *
    H*                Print the 'PN' endorsements on the schedule for*
    H*                 CLT132.                                       *
    H*                                                               *
    H* 12/18/06 C:33  Seq#: WA139/52401                              *
    H*                When listing deductible information            *
    H*                -Retrieve the appropriate deductible info      *
    H*                -For client 139, include the Limit amount      *
    H*                                                               *
    H* 12/04/06 C:33  Seq#: WA133/55512                              *
    H*                Be sure to print locations.                    *
      *
      * 11/27/06 C:15  Project: 139WFS                                *
      *                Added code to process the new GIC PEO type 'G' *
      *                the same as PEO type 'M'.                      *
      *                This change copied by C180 to solve
      *                ALDON CONFLICTS.
     H*                                                               *
     H* 11/27/06 C:65  PROJECT: 114WGH/SUPP#51114                     *
     H*                WRITE EXPOSURE RECORD FOR BALANCE TO INCREASED *
     H*                LIMITS MINIMUM AMOUNT.                         *
    H*                                                               *
    H* 11/22/06 C:65  Seq#: WA135/55959                              *
    H*                When printing premium subject to exp mod line, *
    H*                subtract deductible amounts rather than add.   *
    H*                Also print total line including fees/taxes if  *
    H*                there are any assessments.                     *
    H*                                                               *
    H* 11/20/06 C:65  Seq#: WA088/55932                              *
    H*                Do not try and write to WXNAL if not open -    *
    H*                code expanded to include new processing.       *
    H*                                                               *
    H* 11/16/06 C:65  PROJECT: 132WAY/Support                        *
    H*                Negate the rate on deviation surcharges to     *
     H*                prevent invalid data from being sent.          *
     H*                                                               *
     H*  11/02/06 C:65 PROJECT: 114WMF (Reimplment projects to base)  *
     H*                                                               *
     H* 05/18/05 C:65 PROJECT: 114WGH                                 *
     H*               PULL ASSESSMENTS AS OF THE POL EFF DATE FOR NJ. *
     H*                                                               *
     H*  X   05/17/05 C:65 PROJECT: 114WGH                            *
     H*               REPORT TERRORISM EXPOSURE RECORD EVEN IF NO     *
     H*               PREMIUM AMOUNT FOR NJ.                          *
     H*                                                               *
     H*      10/22/04 C:33 PROJECT: 114WGH/SUPP                       *
     H*               Do not clear the Entity/Location name if the    *
     H*               mailing address was not written.                *
     H*                                                               *
     H*      08/27/04 C:33 PROJECT: 114WGH                            *
     H*               Concatinate Fed Name + 'DBA' + DBA Name         *
     H*                                                               *
     H*  X   08/02/04 C:65 PROJECT: 114WGH/SUPP                       *
     H*               REPORT POLICY EFF DATE/ANNIV DATE FOR EXPOSURE  *
     H*               EFFECTIVE DATE ON SPLIT POLICIES. ASSESSMENTS,  *
     H*               TERRORISM CHARGE, MINIMUM PREMIUM, AND NJ       *
     H*               MANAGED CARE CREDIT ARE REPORTED WITH ARD WHILE *
     H*               OTHER EXPOSURES ARE REPORTED WITH POL EFF DATE. *
     H*                                                               *
     H*  X   06/09/04 C:65  PROJECT: 114WGH/SUPP                      *
     H*               WRITE A RECORD IN WXPEP FOR WC290409A           *
     H*               ENDORSEMENT FOR STORING POLICY DATA.            *
     H*                                                               *
     H*  X   02/13/04 C:65  PROJECT: 114WGH/SUPP                      *
     H*               REMOVE RECORDS WITH 0 EXPOSURE THAT HAVE        *
     H*               ALREADY BEEN REPORTED FOR THE STATE WHEN        *
     H*               REPORTING BY STATE/EXPOSURE LINK.               *
     H*                                                               *
     H*                                                               *
     H* 09/21/06 C:33  Seq#: WA046/54818                              *
     H*                Implement 46WFO into base so that the client   *
     H*                 does not have a modified version of this code.*
     H*               "Change text "policy period" to "fund year".    *
     H*                Add additional text."                          *
     H*                This change copied by C180 to solve ALDON CONFLICTS.
     H*
     H* 09/08/06 C:180 PROJECT: 114WMF -                              *
     H*                                                               *
     H*                REF 114WBX
     H*                User controlled pro-rata factor (Client 114)   *
     H*                Pull expense constant from employer detail.    *
     H*                                                               *
     H*                Remove 5th parameter when calling SRDDIF8      *
     H*
     H*                REF 114WDJ
     H*                Add fees and taxes into the estimated total    *
     H*                premium on the cover page.                     *
     H*
     H*                REF 114WFM
     H*                Print state risk ID if entered for policy.
     H*
     H*                REF 114WFN
     H*                IF PRINTING FOR A NJ POLICY THEN CHANGE THE
    H*                DEC PAGE FOOTER AND THE SCHEDULE WORDING.      *
     H*
    H* 09/07/06 C:65  PROJECT: 126WCD/Support                        *
    H*                Correct data structure field for 2nd deduc     *
     H*                to not overlay experience mod change amount.   *
     H*                                                               *
     H* 09/05/06 C:33  Seq#: WA132/52905                              *
     H*                CX date should be compared to the run date as  *
     H*                 opposed to the policy effective date.         *
     H*
     H* 08/25/06 C:65 PROJECT: 114WGH                                *
     H*                ADD PROCESSING TO CREATE RECORD INFORMATION    *
     H*                FOR THE WC290303 ENDORSEMENT.                  *
     H*                THIS CHANGE COPIED BY C180 to solve ALDON CONFLICTS
     H*
     H* 08/01/06 C:65 Seq#: 53145/132WAC                              *
     H*               Pull deposit premium for estimating employers   *
     H*               from new api program SRINST.  Additionally,     *
     H*               restrict adding of assessments to CA only.      *
     H*                                                               *
     H* 07/25/06 C:65 Seq#: WA133/52349                               *
     H*               When determining if there is a policy record    *
     H*               in WMSHP for a single employer run, check on    *
     H*               a status of '2' or '3'.  If the record is in    *
     H*               a status of '3' then store SHCODE value.        *
     H*                                                               *
     H* 07/07/06 C:33  Seq#: WA139/53166                               *
     H*                Process GIC's pages for AR also.                *
     H*
     H*  07/06/06 C:168  PROJECT: 132WEW
     H*                  Accumulate the aircraft reinsurance charge for
     H*                  policy pages.
     H*
     H* 06/19/06 C:61 PROJECT: 133WCV                                 *
     H*               Add premium line for "Harbor Exposure Fee"      *
     H*               after Normal Premium and before                 *
     H*               Expense Constant.                               *
     H*                                                               *
     H*  06/15/06 C:65 PROJECT: 139WBR                                *
     H*                SET PEO/COMPANY CLIENT NAME FIELD IN THE NAME  *
     H*                RECORD.  THIS WILL BE FLAGGED FROM THE PERSONAL*
     H*                NAME FIELD IN THE WXNAP FILE. ADDITIONALLY,    *
     H*                SET THE EMPLOYER LEASING POLICY TYPE FROM THE  *
     H*                GROUP LEVEL FLAG FOR PEO'S.                    *
     H*                                                               *
     H* 06/01/06 C:65  PROJECT WC945                                  *
     H*                CHAIN FOR PAYROLL EXTENSION DESCRIPTION USING  *
     H*                EFFECTIVE DATE.                                *
     H*                                                               *
     H* 05/12/06 C:132 PROJECT: WC1432                                *
     H*              - If SIC Code or NAICS Code in other locations   *
     H*                (WXNAP) is not blanks/zeros, then print under  *
     H*                the section "OTHER WORKPLACES".                *
     H*              - If NAICS exists load in WXNAP/NASIC, else load *
     H*                SIC code with an appended "S" at end.          *
     H*                                                               *
     H*  05/09/06 C:65 PROJECT: SUPP#52660                            *
     H*                EXPAND TYPE OF BUSINESS FIELD TO 20 ALPHA.     *
     H*                                                               *
     H*  05/08/06 C:65 PROJECT: 114WGH                                *
     H*                ADD REPORTING REQUIREMENTS FOR THE WC000402    *
     H*                ENDORSEMENT.                                   *
     H*                SET THE STATE CODE FIELD FOR THE ANNIVERSARY   *
     H*                RATING DATE ENDORSEMENT RECORD.                *
     H*                                                               *
     H*  04/25/06 C:65 PROJECT: 132WAY                                *
     H*                LOAD PERSONAL NAME FLAG FOR ALL WXNAP RECORDS  *
     H*                WHICH CONTAIN A NAME.  ADDITIONALLY, LOAD      *
     H*                '$' VALUE INTO LEGAL NATURE TO DESIGNATE A     *
     H*                NAME AS A TRADE NAME(DBA).                     *
     H*                                                               *
     H* 04/12/06 C:132 PROJECT: WC1417                                *
     H*                Change "Terrorism Risk Act" in table at end to *
     H*                "Foreign Terrorism", no other changes to code. *
     H*                                                               *
     H* 04/10/06 C:33  Seq#: WA139/50831                              *
     H*                Print 'WC 00 00 01 B' on the bottom of the     *
     H*                 Premium Ext page for NJ policies.             *
     H*                                                               *
     H*  04/10/06 C:65 PROJECT SUPP#51293                             *
     H*                CHANGE DEDUCTIBLE DISCOUNT TO PULL BASED ON    *
     H*                ANNIV RATING DATE INSTEAD OF POLICY EFF DATE.  *
    H*                                                               *
    H* 03/14/06 C:33  Implement 08/05/04, 07/25/05, 07/26/05         *
    H*                 & 07/27/05                                    *
    H*                 chgs from CLT114                              *
    H*          07/27/05 C:65 PROJECT: 114WGH                        *
    H*                ENSURE AN ADDRESS PRINTS FOR A DIVISION AS THE *
    H*                NAME LINK IDENTIFIER WAS NOT BEING SET AT THAT *
    H*                TIME TO PROPERLY CHECK FOR DUPLICATE ADDRESSES.*
    H*          07/26/05 C:65 PROJECT: 114WGH                        *
    H*                REPORT MULTIPLE TAX ID'S FOR NJ UNDER THE SAME *
    H*                NAME ON THE SAME NAME RECORD.  IF MORE THAN 3  *
    H*                TAX ID'S THEN WRITE ANOTHER NAME RECORD AND    *
    H*                INCREMENT THE TAX ID SEQUENCE #.               *
    H*          07/25/05 C:65 PROJECT: 114WGH                        *
    H*                Report federal names on all div's & entities.  *
    H*          08/05/04 C:65 PROJECT: 114WGH/SUPP                   *
    H*               REPORT A UNIQUE EXPOSURE LINK CODE FOR EACH     *
    H*               NAME LINK IDENTIFIER/ADDRESS.  FOR DIVISIONS'   *
    H*               PHYSICAL ADDRESS (OR MAILING IF NO PHYSICAL)    *
    H*               THIS WILL BE DIVISION (+1 IF DIV 0 EXISTS).     *
    H*               FOR ADDITIONAL ADDRESS RECORDS AND ENTITIES/    *
    H*               LOCATIONS THE FIRST NUMBER WILL BE THE HIGHEST  *
    H*               DIVISION + 1 (+1 MORE IF DIV 0 EXISTS).         *
    H*                                                               *
    H* 03/13/06 C:65  PROJECT: 136WBT                                *
    H*                REARRANGE THE BOTTOM SECTION OF THE DEC PAGE   *
    H*                FOR CA POLICIES TO ALLOW A SIGNATURE/GRAPHIC.  *
     H*                                                               *
     H* 03/07/06 C:33  Seq#: WA135/51779                              *
     H*                Increase size of xxPage.                       *
     H*                                                               *
     H* 03/06/06 C:33  Seq#: WA133/51411                              *
     H*                For 133, print the form name for the Premium & *
     H*                 Workplace Schedules.                          *
     H*                                                               *
     H* 03/06/06 C:65  Project:  135WGL                               *
     H*                Add coding for 135 to print specific changes   *
     H*                for NJ on the dec page and on schedules.       *
     H*                                                               *
     H* 02/23/06 C:33  Project: WC1425   WA135/48985                  *
     H*                Add Arkansas notice for for clients resolving  *
     H*                 to PDF.                                       *
     H*                                                               *
     H* 02/02/06 C:65  PROJECT: SUPP# 51219/51088                     *
     H*                FOR 109 AND 136, ONLY ACCUMULATE PRIOR DEPOSIT *
     H*                BILLED FROM CURRENT YEAR TEMPORARILY.          *
     H*                                                               *
     H* 02/01/06 C:65  PROJECT: SUPP#51254                            *
     H*                Clear item 3a1 and 3a2 for printer files so    *
     H*                states do not copy over during mass runs.      *
    H*                                                               *
    H* 01/26/06 C:33  Project: 139WAJ                                *
    H*               -WA139/51010                                    *
    H*                Print SIC & NAICS codes for CLT139.            *
    H*               -WA139/51133                                    *
    H*                Print leading zeros for FEINs.                 *
    H*                                                               *
    H* 01/25/06 C:33  Project: 139WBU    WA139/51083                 *
    H*                Do not use PEO printer control flag for non-PEO*
    H*                 groups.                                       *
    H*                                                               *
    H* 01/20/06 C:33  Seq#: WA136/51039                              *
    H*                If CA policy print the State Risk ID instead   *
    H*                 of the NCCI Risk ID.                          *
     H*                                                               *
     H* 01/07/06 C:65  PROJECT: SUPPORT                               *
     H*                THERE ARE THREE CONDITIONS WHEN TO INCREMENT   *
     H*                THE ENTITY CODE FOR A NAME.  THE THIRD         *
     H*                CONDITION IS WHEN WE ARE SET TO WRITE A FEDERAL*
     H*                NAME, BUT IT ALREADY EXISTS AND WE'RE SET TO   *
     H*                WRITE A DBA NAME.  WE NEED TO INCREMENT THE    *
     H*                DBA NAME COUNTER IN THAT INSTANCE.             *
    H*                                                               *
    H* 01/06/06 C:65  PROJECT: 139WBU                                *
    H*                POPULATE THE PRIMARY AND SECONDARY NAME RECORDS*
    H*                WITH THE INFORMATION PASSED BACK FROM THE      *
    H*                NAME/ADDRESS PROGRAM.  THIS INFORMATION WILL   *
    H*                MATCH THE PEO/CLIENT NAME/ADDRESS INFORMATION. *
    H*                FEIN IS ALSO SET IN XXFEIN FOR RECORDS AS WELL.*
    H*                                                               *
    H* 01/05/06 C:65  PROJECT: SUPPORT                               *
    H*                PASS IN HOLD AND RELEASE STATUS FOR W7413C2.   *
    H*                                                               *
    H* 01/05/06 C:65  PROJECT: SUPP# 50709                           *
    H*                ONLY PRINT FOOTER FOR OTHER LOCATION PAGE IF   *
    H*                THEY HAVE PRINTED.                             *
    H*                                                               *
    H* 01/04/06 C:65  PROJECT: SUPP# 50345/48285                     *
    H*                ONLY PRINT '-' PERCENTAGE SIGNS FOR 132.       *
    H*                                                               *
    H* 12/18/05 C:136 PROJECT: 136WBH                                *
    H*                ADD COMMISSION MOD MULTIPLER                   *
    H*                                                               *
    H* 12/29/05 C:62  Project: 139WBU                                *
    H*                Address changes.                               *
    H*                                                               *
    H* 12/12/05 C:33  Seq#: WA088/50124                              *
    H*                Do not try and write to WXNAL if not open.     *
    H*                                                               *
    H* 12/01/05 C:33  Project: 139WAJ                                *
    H*               -Convert to RPGIV and remove comments.          *
    H*               -Implement PRTF processing.                     *
    H*                                                               *
    H* 11/08/05 C:33  Project: 139WAJ                                *
    H*                Create WC000000.A within the process.          *
    H*                                                               *
    H* 10/18/05 C:65  PROJECT: SUPP# 49182                           *
    H*                CORRECTED BLANK PAGE AT TOP OF SCHEDULE OF     *
    H*                FORMS FOR DTM PRINTING.                        *
    H*                                                               *
    H* 10/12/05 C:65  PROJECT: 132WAY                                *
    H*                STORE NATURE OF BUSINESS DESCRIPTION IN THE    *
    H*                NAME/ADDRESS RECORD WXNAP FOR CALIFORNIA.      *
    H*                                                               *
    H* 10/10/05 C:65  PROJECT: 132WAY                                *
    H*                MARK OTHER TYPES OF BUSINESS UNDER 'OTHER'.    *
    H*                                                               *
    H* 10/07/05 C:65  PROJECT: 132WAY                                *
    H*                PRINT THE CA EXTENSION OF INFORMATION PAGE     *
    H*                FORMS ON THE SCHEDULE OF FORMS FOR CA.  ALSO,  *
    H*                DO NOT PRINT POLICY HOLDER NOTICES ON THE      *
    H*                SCHEDULE OF FORMS UNLESS ITS FORM PN049902B.   *
    H*                                                               *
    H* 10/05/05 C:61  PROJECT WC1287                                 *
    H*              - Change so NAIC code is written whether         *
    H*                SWCPF/WCFL14 is set or not.                    *
    H*                                                               *
    H* 09/09/05 C:33  Project: 135WCV  WA132/48852                   *
    H*                Implement project changes for CA dec page.     *
    H*                                                               *
    H* 08/19/05 C:136 SUPPORT: WA132 48285                           *
    H*                - PRINT NEGATIVE PERCENTAGES FOR POSITIVE      *
    H*                DEVIATIONS BECAUSE THEY REDUCE PREMIUM         *
    H*                - FIX F-CLASS PERCENTAGES ON ASSESSMENTS SO    *
    H*                THAT THEY PRINT CORRECTLY                      *
    H*                                                               *
    H* 08/15/05 C:33  Seq#: WA135/48575                              *
    H*                Do not print '.' on last line for DTM clients. *
    H*                                                               *
    H* 08/12/05 C:33  Seq#: WA135/48547                              *
    H*                Print 'NO ENDORSEMENTS ATTACHED TO POLICY', if *
    H*                 that is the case.                             *
    H*                                                               *
    H* 08/02/05 C:65  Seq#: WA135/48337                              *
    H*                PRINT ENDORSEMENT FORMS ON 2ND LINE IF THERE   *
    H*                IS ONLY ONE LINE TO PREVENT DTM OVERFLOW.      *
    H*                                                               *
    H* 07/18/05 C:33  Seq#: WA135/48117                              *
    H*                Make the FEIN print in variable positions so   *
    H*                 that DTM clients do not get multiple pages.   *
    H*                                                               *
    H* 07/12/05 C:65  PROJECT: SUPP#47613                            *
    H*                FIX THE CODE WHICH WAS LOADING THE AGENT CITY, *
    H*                STATE, AND ZIP TO THE REPORTING FILE WXNAP.    *
    H*                                                               *
    H* 06/30/05 C:65  PROJECT: 135WCV                                *
    H*                ADD CODE TO ALLOW LOGOS AND SIGNTURES TO FIT   *
    H*                PROPERLY IF FLAGGED ON THE GROUP AUX LEVEL.    *
    H*                                                               *
    H* 06/23/05 C:61  PROJECT 133WBS                                 *
    H*                Change to process Form LUB0020199.             *
    H*                                                               *
    H* 06/22/05 C:33  Seq#: WC135/47793                              *
    H*                If there is no mailing address, load the       *
    H*                 physical address.                             *
    H*                                                               *
    H* 06/01/05 C:65  PROJECT: SUPP#46959/WC1026                     *
    H*                PRINT THE MERIT RATING HEADING ON A PER-STATE  *
    H*                BASIS AS CERTAIN STATES ON A POLICY MAY BE     *
    H*                MERIT RATED WHILE THE REST OF THE POLICY ISN'T.*
    H*                                                               *
    H* 05/12/05 C:65  PROJECT: 132WAY                                *
    H*                STORE THE 2ND AND 3RD EXPERIENCE MODIFIERS IN  *
    H*                THE HEADER FILE WXNHP.  ADDITIONALLY, REPORT   *
    H*                ENDORSEMENT INFORMATION ELECTRONICALLY FOR     *
    H*                CA ALONG WITH TWO MORE ENDORSEMENTS. IF THERE  *
    H*                IS NO EXPERIENCE MODIFIER FOR CA,DE,PA,NJ THEN *
    H*                REPORT 0 INSTEAD OF 1.                         *
    H*                                                               *
    H* 04/07/05 C:33  PROJECT: 126WCD                                *
    H*                Add 2nd Deductible to Premium Schedule.        *
    H*                                                               *
    H* 04/28/05 C:61  PROJECT: 135WCG                                *
    H*                Allow for 11 char policy endorsement names.    *
    H*                                                               *
    H* 04/20/05 C:53  PROJECT: 135WCH/WC1292                         *
    H*                ADD PEO PROCESSING                             *
    H*                                                               *
    H* 04/07/05       PROJECT: 126WDK                                *
    H*          C:33  PROJECT: 126WCD                                *
    H*                Multiple Deductible discounts per Year.        *
    H*                                                               *
    H* 03/31/05 C:53  PROJECT: WC1287/135WCB                         *
    H*                Added processing for NAICS code requirements
    H*                                                               *
    H* 03/07/05 C:65  PROJECT: WC1202                                *
    H*                PRINT EXTENDED EMPLR NAME FIELDS ON DECLARATION*
    H*                PAGE OF THE POLICY PAGE.                       *
    H*                                                               *
    H* 02/15/05 C:65  PROJECT: WC1167                                *
    H*                PRINT NET RATE ON SCHEDULE OF OPERATIONS IF    *
    H*                APPROPRIATE.                                   *
    H*                                                               *
    H* 02/14/05 C:136 PROJECT: WC1250                                *
    H*                ADD DOMESTIC TERRORISM CHARGE.                 *
    H*                                                               *
    H* 02/11/05 C:33  Seq#: WA132/45648                              *
    H*                Be sure to list Enities with different name but*
    H*                 same address.                                 *
    H*                                                               *
    H* 01/28/05 C:65  PROJECT: SUPP#45446                            *
    H*                ADD DEDUCTIBLE DISCOUNT TO THE MINIMUM PREMIUM *
    H*                BALANCE AMOUNT INSTEAD OF SUBTRACTING, AS IT   *
    H*                IS A CREDIT, NOT A DEBIT.                      *
    H*                                                               *
    H* 01/18/05 C:65 PROJECT: 114WGH/SUPP                            *
    H*               DO NOT BLANK OUT THE NAME ON A PHYSICAL ADDRESS *
    H*               IF THE MAILING ADDRESS WAS NOT STORED.          *
    H*                                                               *
    H* 01/10/05 C:33  Seq#: WA132/45347                              *
    H*                Adjust 'Additional Name Insured' page for DTM. *
