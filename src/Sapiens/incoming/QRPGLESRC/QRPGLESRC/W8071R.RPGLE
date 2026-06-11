     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H*
     H/TITLE W8071R - EXTRACT MANAGER
     H*
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*
     H*****************************************************************
     H*  DESCRIPTION:  THIS PGM PROCESSES ALL RECORDS FROM THE LOG    *
     H*    HISTORY FILE WHICH ARE DUE FOR EXTRACTION.  FIRST, THE     *
     H*    EDI DATA ELEMENTS ARE EDITED, THEN THEY ARE PRINTED TO     *
     H*    1 OF 2 REPORTS, THE EDI 148 DETAIL REPORT OR THE EDI 148   *
     H*    ERROR REJECTION REPORT.  IF THE TRANSACTION PASSES ALL     *
     H*    EDITS, IT IS WRITTEN TO ONE OF THE EDI FLAT FILES (WHICH   *
     H*    IS DETERMINED BY THE TRANS SET ID), THEN THE LOG HISTORY   *
     H*    FILE IS UPDATED.  AFTER ALL RECORDS HAVE BEEN PROCESSED,   *
     H*    THE HARDCOPY PRINT PROGRAM IS CALLED AND THE SUBMISSION    *
     H*    MANAGEMENT RECORD IS WRITTEN.                              *
     H*                                                               *
     H*  CALLED FROM:  W8071C                                         *
     H*  CALLS TO:     W8070R6 - LOADS THE DATA ELEMENT ARRAY         *
     H*                W8070R7 - EDITS THE DATA ELEMENTS              *
     H*                                                               *
     H*                                                               *
     H*************************************************************************
     H*  REVISIONS:                                                           *
     H*                                                                       *
     H* 03/24/25 C:053 Support 146/118567                                     *
     H*                Use Primary flag (WMAWL6) on Wage grid for IP.         *
     H*                Client # 146
     H*                                                                       *
     H*                                                                       *
     H* 10/10/24 C:053 Support 152/117512                                     *
     H*                Correct GWA reporting.                                 *
     H*                                                                       *
     H* 03/19/24 C:053 Support 146/115213                                     *
     H*                remove 109011                                          *
     H*                                                                       *
     H* 01/22/24 C:053 Support 114717                                         *
     H*                Remove end of line characters from narrative           *
     H*                                                                       *
     H* 11/09/23 C:053 Support 146/114359                                     *
     H*                Correct MCO segment processing.                        *
     H*                                                                       *
     H* 09/08/23 C:053 Support 114/113569                                     *
     H*                Correct Gross Weekly Amount reporting                  *
     H*                                                                       *
     H* 05/25/23 C:053 Support 146/109011                                     *
     H*                Update for PEO processing.                             *
     H*                                                                       *
     H* 10/20/22 C:079 Support: 152 103556                                    *
     H*                For 152 Remove condition for writing BTC 240 info      *
     H*                They want to report when 0 paid and eve on MTC IP      *
     H*                                                                       *
     H* 03/02/22 C:053 Support: 98717                                         *
     H*                Prevent Ben Seg write if zero value                    *
     H*                                                                       *
     H* 02/14/22 C:079 Project: WC2797                                        *
     H*                IAIABC 3.1.4 Changes                                   *
     H*                                                                       *
     H* 12/20/21 C:079 Support: 102/93493                                     *
     H*                Use Extract date not log rec. create date              *
     H*                                                                       *
     H* 01/11/21 C:053 Support: 93877                                         *
     H*                Correct witness segment                                *
     H*                                                                       *
     H* 06/09/20 C:318 Project: SY420                                         *
     H*                Mask SSN                                               *
     H*                                                                       *
     H* 05/04/20 C:180 Support 87315                                          *
     H*                Check for IP comp rate                                 *
     H*
     H* 04/03/20 C:062 Project WC2730                                         *
     H*                Add new field about collective bargaining              *
     H*                                                                       *
     H* 01/09/20 C:053 Support 86298                                          *
     H*                Base Perm Impair segment load on Whole Body % field    *
     H*                                                                       *
     H* 01/06/20 C:180 Support 86064                                          *
     H*                Make MTC date same as extract date for client S1 Med   *
     H*                                                                       *
     H* 06/28/19 C:053 Support PHW79500                                       *
     H*                Code for no CSM record found                           *
     H*                                                                       *
     H* 06/17/19 C:053 Support PHW79587                                       *
     H*                Correct EP segment                                     *
     H*                                                                       *
     H* 06/10/19 C:180 Support PHW80124                                       *
     H*                Fix infinite loop                                      *
     H*                                                                       *
     H* 03/04/19 C:079 Support PHW77111                                       *
     H*                Do not fill with zeros                                 *
     H*                                                                       *
     H* 12/14/18 C:053 Project: WC2515
     H*                Changes due to issues with State submissions
     H*                                                                       *
     H* 11/21/18 C:053 Project: WC2515
     H*                Changes to data from Mitchell submissions
     H*                                                                       *
     H* 05/30/18 C:079 Project: WC2515
     H*                Claims EDI 3.1 - Added processing for R21 segments:
     H*                 - Part of Body
     H*                 - Change Data Elements
     H*                 - Cancel Elements
     H*
     H* 05/30/18 C:079 Project: WC2515
     H*                Claims EDI 3.1
     H*                                                                       *
     H* 05/30/18 C:TXO Project: WC2515
     H*                Claims EDI 3.1
     H*
     H* 07/19/17 C:053 SUPPORT 48222
     H*                Move load of FRDODD due to WC2410 code placement.
     H*                                                                       *
     H* 01/31/17 C:306 PROJECT: WC2410                                        *
     H*                Added fields FRINJS, FRSEMP, FRCLSB,FRDRES and FRACCD  *
     H*                from file WXFRP.                                       *
     H*                Added fields SUINJS file WXSUP.                        *
     H*                                                                       *
     F* 08/05/15 C:307 Project: SY354                                         *
     F*                -Adding 'Route To:'                                    *
     F*                                                                       *
     H* 08/26/14 C:307 Project: SY354                                         *
     H*                -Implement Routing Page Changes.                       *
     H*                                                                       *
     H* 04/30/14 C:180 SUPPORT WC147 85477
     H*                Fix reporting of Denial reason codes
     H*
     H* 04/21/14 C:180 SUPPORT 85390 Lef justify SSN
     H*                                                                       *
     H* 04/09/14 C:180 SUPPORT WC034 85307
     H*                Fix OBT for VA.
     H*
     H* 03/04/14 C:180 SUPPORT WC114 84947
     H*                Fix MCO code.
     H*
     H* 02/08/14 C:180 SUPPORT PRISS 027 - WC2234
     H*                Re-compiled only.
     H*                                                                       *
     H* 12/12/13 C:287 PROJECT: WC2234                                        *
     H*                Added fields FRWWTC, FRDSC, and FRESID from file WXFRP *
     H*                Added fields SUPSPA, SUWWTC, SUWDSC, and SUESID from   *
     H*                file WXSUP.                                            *
     H*                                                                       *
     H* 09/11/13 C:190   Project : WC2227
     H*                  Replace the qualification to load LGMTCD
     H*                  from LGMTC=IP to LGMTCD<>0.
     H*
     H* 07/02/13 C:190   Project : 147WBF((Additional changes)
     H*                  Add params on the call to W8070R7.
     H*                  Populate Fnd, Year & Case in WXFRP, WXSUP.
     H*
     H* 01/30/13 C:180 SUPPORT WA146 81539 Omit BTC 330 for GA.
     H*
     H* 01/30/12 C:180 SUPPORT 79110 Change Ben Adj Start date field
     H*                              definition from DEC 9/2 to DEC 7/0
     H*                              so date conversion pgm does not
     H*                              return zero value when Ben Adj
     H*                              start date is converted.
     H*
     H* 12/20/11 C:180 SUPPORT 78545 Populate GWEFD with DOI.
     H*
     H* 09/19/11 C:180 SUPPORT 74774 Create EDI-DARTS remark when
     H*                              log record status changes to 'EI'.
     H*
     H* 01/25/11 C:180 SUPPORT 75125 Do not send recov code 422 when
     H*                              state is CA.
     H*
     H* 12/01/10 C:180 SUPPORT 74979 Donot allow zeros for employer's
     H*                              zip+4 when state is CA.
     H*                              (Scope of supp changed).
     H*
     H* 11/24/10 C:180 SUPPORT 74979 Donot allow zeros for accident
     H*                              zip+4 when state is CA.
     H*
     H* 11/10/10 C:180 SUPPORT 74646
     H*                Fix number of weeks calc for Recoveries.
     H*
     H* 10/14/10 C:180   Project : TDB for SUPP 68101
     H*
     H*                  Undo bad code dated 07/21/09 (Hard deleted, too
     H*                  spread all over to comment out and put new
     H*                  changes) to restore original version for that
     H*                  section of pgm.
     H*
     H*                  Fix understated amount of recoveries associated
     H*                  with a payment code.
     H*
     H*                  Fix overstated benefit amounts.
     H*
     H*                  Fix Begin and end dates for reported payments.
     H*
     H*                  Change to report benefit amounts net of
     H*                  recoveries.
     H*
     H* 10/13/10 C:53    Project: 139WNN/PRISS 78
     H*                  Bypass edit for GIC.
     H*
     H* 09/28/10 C:220   Project: 139WNN/ PRISS#53
     H*                  Add EDIF key field in M1KY
     H*
     H* 08/11/10 C:220   Project: 139WNN/ PRISS#54
     H*                  Add EDIF parameter to process EDI format
     H*                  specific log records.
     H*
     H* 07/22/10 C:220   Project: 139WNN/ PRISS 39
     H*                  CORRECT WA076R CALL TO PROVIDE CORRECT IXHCED
     H*
     H* 07/01/10 C:220   Project: 139WNN/ PRISS 29
     H*                  CORRECT WA076R CALL TO PROVIDE CORRECT FUND
     H*
     H* 06/28/10     C:198      Project: WC1871
     H*                         OBT code of 422 is not to be reported
     H*                         to GA via EDI.
     H*
     H* 06/08/10     C:190      Project: 139WOM
     H*                         For CLTID = 139 change the program to
     H*                         correctly access the check schedule
     H*                         maintenance record by subtracting 1 day.
     H*
     H* 05/14/10     C:180      SUPPORT 68934
     H*                         NE does not accept BTC of 422
     H*
     H* 04/05/10 C:189   Project: 139WLM
     H*                  EDI-Create diary when EDI Log History
     H*
     H*
     H* 03/24/10 C:232  Project: 139WNN                               *
     H*                 Update programs to use the new EDI Control API*
     H*                 WA076R instead of using WMIXP.                *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 12/31/09     C:235      SUPPORT: 69771
     H*                         releax the edit on Dependent Relationship
     H*                         field also Numerical Birth order need to
     H*                         show as 0 if the field is blank.
     H*
     H* 12/10/09     C:53       SUPPORT 69551
     H*                         Addtl change to use 1st non-adjusted pmt.
     H*
     H* 10/30/09     C:53       SUPPORT 69551,69387,68527,69381,70594
     H*                         Ensure the benefit & payment segments match
     H*                         and have the correct payment.
     H*
     H* 10/27/09     C:180      SUPPORT 70443 - Do not populate telephone #
     H*                         with zeros.
     H*
     H* 10/26/09     C:180      SUPPORT 70443 - Wage period code for R1.
     H*
     H* 10/22/09     C:180      SUPPORT 70133 - Report interest payments.
     H*
     H*
     H* 09/16/09     C:180      SUPPORT 69900
     H*                         Fix WAGE PERIOD CODE
     H*
     H* 08/06/09     C:53       SUPPORT 68853
     H*                         Do not consider start and end dates
     H*                         adjustments, stops & voids
     H*
     H* 07/21/09     C:198      Support 68101 needs to be in sync with
     H*                         W8074R1.
     H*
     H* 07/15/09     C:180      WC1836   Add MCO name
     h*
     H*
     H* 05/15/09     C:180      SUPPORT 67933 + 68430
     H*                         Fix RTW date for FL - RTW date if not
     H*                         populated, do not populate with initial
     H*                         rtw date.
     H*
     H* 05/11/09     C:53       SUPPORT 68383
     H*                         Removed code that caused negative numbers.
     H*
     H* 04/29/09     C:180      SUPPORT 68059 - EP claims
     H*
     H* 04/22/09-B   C:180      SUPPORT 68021, 68041, 68147
     H*                         Report impairments using the following
     H*                         logic and order:
     H*
     H*                          1. If data is found in impairment
     H*                             screen, report it.
     H*
     H*                          2. If data is not found in impairment
     H*                             screen, but found in medical info
     H*                             then report impairment % from
     H*                             medical info and body part code
     H*                             from accident info.
     H*
     H*                          3. Step 2 above does not apply to
     H*                             FL  - only step 1 applies to FL.
     H*
     H* 04/22/09     C:53       SUPPORT 66238
     H*
     H*
     H* 04/14/09     C:53       SUPPORT 67840
     H*                         Allow update of GW eff dt if LT NW eff dt.
     H*
     H* 04/13/09     C:180      SUPPORT 67817
     H*                         Do not reduce net weekly amt when
     H*                         processing Benefit Redistribution codes.
     H*
     H* 04/01/09     C:180      SUPPORT 66757
     H*                         FOR CSI only - Use accident date instead
     H*                         of entry date to select claims.
     H*
     H*
     H* 04/01/09     C:180      SUPPORT 67903
     H*                         Change so body part code comes from
     H*                         impairments screen for state of FL.
     H*
     H* 03/24/09     C:180      SUPPORT 67172
     H*                         Update RTW qualifier.
     H*
     H* 03/21/09     C:189      SUPPORT 67378
     H*                         Dont pick the records with dependent
     H*                         relation as '9' and incative
     H*
     H* 03/18/09-B   C:180      SUPPORT 67674
     H*                         Send body part from accident screen
     H*                         when not found in Impairments function.
     h*
     H*
     H* 03/18/09     C:189      SUPPORT 67548
     H*                         Codes wre picked up & sent even there was no
     H*                         $ amount.
     H*
     H* 03/16/09     C:180      SUPPORT 67172
     H*                         Update RTW.
     H*
     H* 03/14/09     C:53       SUPPORT 67522
     H*                         Process new flag for MTC link for ben seg.
     H*
     H* 03/09/09     C:53       SUPPORT 67506
     H*                         Do not calculate the pmt issue date if
     H*                         coming from the linked payment file since
     H*                         we are now calculating it there.
     H*
     H* 02/27/09     C:53       SUPPORT 67299  (TO ACCOMODATE 139WGE)
     H*                         Changes for client 139 due to the way their
     H*                         check date is assigned.
     H*
     H* 02/20/09     C:53       SUPPORT 67079
     H*                         Additional changes.
     H*
     H* 02/10/09     C:53       SUPPORT 67079
     H*                         Allow the override of payment issue date.
     H*
     H* 02/03/09     C:53       SUPPORT 66963
     H*                         Changes for RTW in Release 1
     H*
     H* 01/07/09     C:53       SUPPORT 66678
     H*                         Correct date format from WDAJP.
     H*
     H* 01/02/09     C:180      SUPPORT 66366
     H*                         Populated field RDENDT with valid value.
     H*
     H* 12/19/08     C:53       SUPPORT 66503
     H*                         Set check the index for the proper value.
     H*
     H* 12/16/08     C:53       SUPPORT 66356
     H*                         Consider Ben ACR entries for sweep data.
     H*
     H* 12/02/08     C:53       SUPPORT 66162
     H*                         Changes for RTW fields
     H*
     H* 11/20/08     C:53       SUPPORT 65779
     H*                         Adjust Wage Period code for differances in
     H*                         Release 1 & 3.
     H*
     H* 11/17/08     C:53       SUPPORT 63706
     H*                         Changes for insured fein #
     H*
     H* 11/03/08     C:180      SUPPORT 65627
     H*                         Donot load MTC FN, SA for variable segment
     H*                         Re-done.
     H*
     H* 10/30/08     C:180      SUPPORT 65671, 65757 - Add EDI R3 field:
     H*                         Employee ID other than SSN
     H*                         Reduced Benef Amt code
     H*
     H*                                                               *
     H*
     H* 10/23/08 C:53  SUPPORT 65710
     H*                For R3 use the mapping for init Rtn to wk to pick
     H*                up the rls to work date.
     H*
     H* 10/22/08 C:53  SUPPORT 65670
     H*                For R3 allow data to be written from WDAJP even if
     H*                the amount = 0
     H*
     H* 10/21/08 C:180 SUPPORT 65627
     H*                Donot load MTC FN, SA for variable segment.
     H*
     H* 10/20/08 C:180 SUPPORT 65603
     H*                Populate date of knowledge of disability.
     H*
     H* 10/16/08 C:53  SUPPORT 65601
     H*                Correct payment selection for IP.
     H*
     H* 10/15/08-B C:180 SUPPORT 65530
     H*                Load Current Date disability began
     H*
     H* 10/15/08 C:180 SUPPORT: 65486, 65627
     H*                Fix error with payment issue date.
     H*                                                               *
     H* 10/10/08 C:180 SUPPORT: 64670
     H*                Fix error with body impairment pct.
     H*
     H* 10/06/08 C:53  Support: 65434                                 *
     H*                Load Claim Admin FEIN into FRCAFE.
     H*                Use blanks if zip +4 = 0000                    *
     H*                                                               *
     H* 10/19/07 C:191 Project: WC1584                                *
     H*                EDI Release 3 Processing, load R22 info        *
     H*                Per ROES: Make changes from 02/27/06 - 51600
     H*                for all clients.
     H*                                                               *
     H* 05/11/07 C:191 Support: 57704                                 *
     H*                - Add processing for RE transactions in SRLOAD *
     H*                                                               *
     H* 05/01/07 C:185 Project: WC1177                                *
     H*                EDI Release 3 Processing.                      *
     H*                                                               *
     H* 03/15/06 C:53 Support: WC129/51837                            *
     H*               Added changes from #51600 for client 129 also.  *
     H*                                                               *
     H* 02/27/06 C:53 Support: WC132/51600                            *
     H*               Per ROES, the body part code should be "99 "    *
     H*               and not "099".  Made changes for client 132     *
     H*               only.                                           *
     H*                                                               *
     H* 02/03/06 C:15 Support: WC135/51226                            *
     H*               Per ROES, DN,8 should be loaded with blanks     *
     H*               instead of zeros, if no value is entered.       *
     H*                                                               *
     H* 01/24/06 C:53 Support: WC132/50510                            *
     H*               Add distribution fields to routing output       *
     H*                                                               *
     H* 03/15/05 C:15  Project: 114WJE                                *
     H*                Added code to check the EDI flag at the group  *
     H*                level to determine if EDI should be processed. *
     H*                                                               *
     H* 10/08/04 C:53 Support: WC055/44145                            *
     H*               Do not load zeros if state is 'NE'              *
     H*                                                               *
     H* 12/09/03 C:15 Project: WC577                                  *
     H*               Reinstall code that was deleted, which loaded   *
     H*               PTD totals into WET1P.                          *
     H*                                                               *
     H* 07/01/03 C:53 Project: WC1134                                 *
     H*               Check for NAICS code and load to FRSIC else     *
     H*               load the SIC code as before.                    *
     H*                                                               *
     H* 03/12/03 C:53 Support: WC055/36107                            *
     H*               Do not load zeros if state is 'NE'              *
     H*                                                               *
     H* 10/30/02 C:53  Support: WC082/34551                           *
     H*                Added fields to rename to prevent data from    *
     H*                bleeding over                                  *
     H*                                                               *
     H* 05/20/02 C:15  Support: WC048/32558                           *
     H*                Added code to prevent QQEXDT from being chgd.  *
     H*                                                               *
     H* 05/08/02 C:53  Support: WC048 # 32064                         *
     H*                Update log rcd w/EI status when errors exist.  *
     H*                                                               *
     H* 03/22/02 C:15  Support: WC025                                 *
     H*                Per Rick Honeycutt, load the pmt transaction   *
     H*                date into the MTC date for 'IP' records.       *
     H*                                                               *
     H* 08/07/01 C:15  Support: WC025                                 *
     H*               -When sending an 'IP', verfiy that the 1st pmt  *
     H*                hasn't been voided or adjusted.  We should be  *
     H*                sending the 1st legitimate pmt.                *
     H*               -When sending an A49, only send Pmt/Adj segments*
     H*                with an amt > 0.                               *
     H*                                                               *
     H* 04/26/01 C:53  Project: WC065 # 26752                         *
     H*                Add adjustor to print files 0 & 1              *
     H*                                                               *
     H* 01/23/01 C:53  Project: WC072 # 25985                         *
     H*                Clear file fields after write.                 *
     H*                                                               *
     H* 01/02/01 C:15  Project: WC468                                 *
     H*                Fixed dates being loaded as 1900.              *
     H*                                                               *
     H* 12/15/00 C:15  Project: WC468                                 *
     H*                Added code to right justify the wage period.   *
     H*                (Per Celerity).                                *
     H*                                                               *
     H* 10/16/00 C:15  Project: WC468                                 *
     H*                Added code to load a Pmt/adj record for full   *
     H*                salary amount, weeks and days paid by the      *
     H*                employer.                                      *
     H*                                                               *
     H* 09/26/00 C:15  Project: WC468   Seq# 24396                    *
     H*                Added code to load the PTD/Reduced earnings/   *
     H*                Recovery segment using the new file.           *
     H*                                                               *
     H* 07/20/00 C:15  Support: WC065/23626                           *
     H*                Output indicates 'Due date' but the maint code *
     H*                date is printing.  Chgd pgm to print the due   *
     H*                date.                                          *
     H*                                                               *
     H* 06/10/00  C:58 PROJECT WC993
     H*                 Add goup master WMFNL for test group flag
     H*                 FNTEST; select only non-test groups.
     H*                                                               *
     H* 04/03/00 C:118 Project: WC854                                 *
     H*                Change code that compares CMIPCT <> 0 to       *
     H*                compare CMIMP=Y.                               *
     H*                                                               *
     H* 03/29/00 C:15 Project: WC468                                  *
     H*               Changed condition which determines if a previous*
     H*               148 has been submitted.                         *
     H*                                                               *
     H* 03/23/99 C:15 Project: WC468                                  *
     H*               Added code to load the PTD fields DN95 and      *
     H*               DN96, and the death dependent field DN97.       *
     H*                                                               *
     H* 05/13/96 C:15 PROJECT: 25WDE                                  *
     H*               ADDED CODE TO PROCESS THE A49 TRANSACTIONS.     *
     H*                                                               *
     H* 07/12/96 C:15 PROJECT: 25WHF                                  *
     H*               CHGD THE EXTRACT CRITERIA TO SELECT LOG RECORDS *
     H*               BASED ON THE INITIAL ENTRY DT (LGIDT) INSTEAD   *
     H*               OF THE DUE DT (LGDUE).                          *
     H*                                                               *
     H* 08/26/96 C:58 PROJECT: 81WBP                                  *
     H*               Modifications supporting the use of the         *
     H*               Edit Matrix in the Interactive Log API          *
     H*                                                               *
     H* 08/28/96 C:15 PROJECT: 25WDE                                  *
     H*               FIXED CODE WHICH LOADS THE BODY PART CODE IN    *
     H*               THE VARIABLE SEGMENT.                           *
     H*                                                               *
     H* 11/18/96 C:58 PROJECT: 81WBP                                  *
     H*               MODIFY FOR A49 PAYMENT CORRECTION PROCESS:      *
     H*               UTILIZE THE PAYMENT/ADJUSTMENT CORRECTION       *
     H*               RECORD, IF FOUND, FOR LOG RECORD PROCESSED.     *
     H*                                                               *
     H* 11/19/96 C:15 PROJECT: 68AMN                                  *
     H*               ADDED CODE TO SORT THE ERROR REPORTS BY ADJ/    *
     H*               DUE DT/CLM ALPHA AND PAGE BREAK FOR EACH ADJ.   *
     H*               ADDED "NO RECORDS" MESSAGE TO ALL REPORTS.      *
     H*                                                               *
     H* 03/05/97 C:15 PROJECT: 72WBG                                  *
     H*               CHANGED CODE WHICH LOADS THE NATURE, PART, AND  *
     H*               CAUSE DESCRIPTIONS.                             *
     H*                                                               *
     H* 04/15/97 C:15 PROJECT: 72WBG                                  *
     H*               For A49s, only load the first comp pmt info if  *
     H*               processing an 'IP'.                             *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  *IN21 - IF *ON, DO NOT PROCESS 148 TRANSACTIONS              *
     H*  *IN22 - IF *ON, DO NOT PROCESS A49 TRANSACTIONS              *
     H*                                                               *
     H*  *IN83 - *ON, Print 148 Totals
     H*  *IN84 - *ON, Print A49 Totals
     H*
     H*****************************************************************
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWMLGL2    UF   E           K DISK
     F                                     INFDS(FIDS01)
     FWXFRL     O    E           K DISK
     FWMM1L     UF   E           K DISK
     FWTERL     IF   E           K DISK
     F*
     F* Group Master
     FWMFNL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PAT01)
     F                                     IGNORE(PSP01)
     F                                     IGNORE(PNC01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PRT01)
     F                                     IGNORE(PPC01)
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
     FWXSUP     O  A E           K DISK
     FWXPIP     O  A E           K DISK
     FWXAJP     O  A E           K DISK
     FWXRDP     O  A E           K DISK
     FWMBYL     IF   E           K DISK
     FWDPAL     IF   E           K DISK
     F                                     RENAME(PPA01:PPA02)
     FWMBNL     IF   E           K DISK
     FWMLGL     IF   E           K DISK
     F*** Beg Chg *** 02/14/22 *******************************************
     F***********************************  RENAME(PLG01:PLG02)
     F                                     RENAME(PLG01:PLG02) Prefix(L2:2)
     F*** Beg Chg *** 02/14/22 *******************************************
     FWTGTL     IF   E           K DISK
     FWDAJL     IF   E           K DISK
     F                                     RENAME(PAJ01:XAJ01)
     FWDPTL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWTDCL     IF   E           K DISK
     FWXXTL     IF A E           K DISK
     FWMUPL1    IF   E           K DISK
     FSDCMLA    UF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     FWXT2P     O    E           K DISK
     FWXT3P     O    E           K DISK
     FWDACL     IF   E           K DISK
     FWEREL     IF   E           K DISK
     FWMWIL     IF   E           K DISK
     FWDRPL3    IF   E           K DISK
     FWACPL     IF   E           K DISK
     FWDA3L     IF   E           K DISK
     FWDE3L     IF   E           K DISK
     FWDR3L     IF   E           K DISK
     FWXPTP     O    E           K DISK
     FWXA3L     UF A E           K DISK    RENAME(PA301:PA301X)
     FWXE3L     UF A E           K DISK    RENAME(PE301:PE301X)
     FWXR3L     UF A E           K DISK    RENAME(PR301:PR301X)
     FSMUPL1    IF   E           K DISK         RENAME(PUP01:PUP03)
     F
     FSTCTL1    IF   E           K DISK
     F*
      *** Beg Add *** 05/04/20  *********************************
     FWMAWL5    IF   E           K DISK    rename(PAW01:PAW05)
      *** End Add *** 05/04/20 ***********************************
      *** Beg Add *** 03/24/25  *********************************
     FWMAWL6    IF   E           K DISK    rename(PAW01:PAW06) Prefix(L6:2)
      *** End Add *** 03/24/25 ***********************************
      *** Beg Add *** 05/30/18 **************************
     FWDH0L     IF   E           K DISK
     FWXH3L     UF A E           K DISK
     FWDN0L     IF   E           K DISK
     FWXN3L     UF A E           K DISK
     FWXV3L     UF A E           K DISK
     FWDV0L     UF A E           K DISK
      *** End Add *** 05/30/18 **************************
     F*  148 Reports - Audit detail
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOA)
     F                                     USROPN
     F*              - Error rejection
     FQSYSPRT2  O    F  198        PRINTER OFLIND(*INOC)
     F                                     USROPN
     F*
     F*  A49 Reports - Audit detail
     FQSYSPRT1  O    F  198        PRINTER OFLIND(*INOB)
     F                                     USROPN
     F*              - Error rejection
     FQSYSPRT3  O    F  198        PRINTER OFLIND(*INOD)
     F                                     USROPN
     F*
     F*     Summary totals report
     FQSYSPRT4  O    F  198        PRINTER OFLIND(*INOE)
     D*________________________________________________________________
     D*     TABLES and ARRAYS
     D*
     D*   Data Elements
     D DN              S            150    DIM(999)
     D*
     D*   Remark sequence numbers in EDI Log file
     D*
     D WAM             S             11  2 DIM(10)
     D*
     D PCD             S              3    DIM(10)
     D PC1             S              3    DIM(5)
     D AMT             S             11  2 DIM(10)
     D SDT             S              8  0 DIM(10)
     D EDT             S              8  0 DIM(10)
     D WKS             S              5  0 DIM(10)
     D DYS             S              3  0 DIM(10)
     D GWA             S             11  2 DIM(10)
     D GWE             S              8  0 DIM(10)
     D NWE             S              8  0 DIM(10)
     D PYD             S              8  0 DIM(10)
     D LNKM            S              1    DIM(10)
     D PDCD            S              3    DIM(10)
     D PDAM            S             11  2 DIM(10)
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D  Q2RRN#               397    400B 0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D*     Remark sequence numbers array
     D                 DS
     D  LGDSQA                 1      5  0
     D  LGDSQB                 6     10  0
     D  LGDSQC                11     15  0
     D  LGDSQD                16     20  0
     D  LGDSQE                21     25  0
     D  LGDSQF                26     30  0
     D  SQ                     1     30  0
     D                                     DIM(6)
     D*
     D                 DS
     D  LGCO#                  1      3  0
     D  LGFND                  4      6  0
     D  LGFYR                  7      9  0
     D  LGCASE                10     16  0
     D  XXACLC                 1     25
     D*
     D                 DS
     D  DJCD01                 1      3
     D  DJCD02                 4      6
     D  DJCD03                 7      9
     D  DJCD04                10     12
     D  DJCD05                13     15
     D  DJCD06                16     18
     D  DJCD07                19     21
     D  DJCD08                22     24
     D  DJCD09                25     27
     D  DJCD10                28     30
     D  DJCD                   1     30
     D                                     DIM(10)
      *
     D                 DS
     D  DJPD01                 1      9  2
     D  DJPD02                10     18  2
     D  DJPD03                19     27  2
     D  DJPD04                28     36  2
     D  DJPD05                37     45  2
     D  DJPD06                46     54  2
     D  DJPD07                55     63  2
     D  DJPD08                64     72  2
     D  DJPD09                73     81  2
     D  DJPD10                82     90  2
     D  DJPD                   1     90  2
     D                                     DIM(10)
     D*
     D                 DS
     D  DJWA01                 1      9  2
     D  DJWA02                10     18  2
     D  DJWA03                19     27  2
     D  DJWA04                28     36  2
     D  DJWA05                37     45  2
     D  DJWA06                46     54  2
     D  DJWA07                55     63  2
     D  DJWA08                64     72  2
     D  DJWA09                73     81  2
     D  DJWA10                82     90  2
     D  DJWA                   1     90  2
     D                                     DIM(10)
     D*
     D                 DS
     D  DJST01               001    007  0
     D  DJST02               008    014  0
     D  DJST03               015    021  0
     D  DJST04               022    028  0
     D  DJST05               029    035  0
     D  DJST06               036    042  0
     D  DJST07               043    049  0
     D  DJST08               050    056  0
     D  DJST09               057    063  0
     D  DJST10               064    070  0
     D  DJST                   1    070  0
     D                                     DIM(10)
      *
     D                 DS
     D  DJEN01               001    007  0
     D  DJEN02               008    014  0
     D  DJEN03               015    021  0
     D  DJEN04               022    028  0
     D  DJEN05               029    035  0
     D  DJEN06               036    042  0
     D  DJEN07               043    049  0
     D  DJEN08               050    056  0
     D  DJEN09               057    063  0
     D  DJEN10               064    070  0
     D  DJEN                   1    070  0
     D                                     DIM(10)
      *
     D                 DS
     D  DJWK01                 1      5  0
     D  DJWK02                 6     10  0
     D  DJWK03                11     15  0
     D  DJWK04                16     20  0
     D  DJWK05                21     25  0
     D  DJWK06                26     30  0
     D  DJWK07                31     35  0
     D  DJWK08                36     40  0
     D  DJWK09                41     45  0
     D  DJWK10                46     50  0
     D  DJWK                   1     50  0
     D                                     DIM(10)
      *
     D                 DS
     D  DJDY01                 1      1  0
     D  DJDY02                 2      2  0
     D  DJDY03                 3      3  0
     D  DJDY04                 4      4  0
     D  DJDY05                 5      5  0
     D  DJDY06                 6      6  0
     D  DJDY07                 7      7  0
     D  DJDY08                 8      8  0
     D  DJDY09                 9      9  0
     D  DJDY10                10     10  0
     D  DJDY                   1     10  0
     D                                     DIM(10)
     D                 DS
     D  DJGA01                 1      9  2
     D  DJGA02                10     18  2
     D  DJGA03                19     27  2
     D  DJGA04                28     36  2
     D  DJGA05                37     45  2
     D  DJGA06                46     54  2
     D  DJGA07                55     63  2
     D  DJGA08                64     72  2
     D  DJGA09                73     81  2
     D  DJGA10                82     90  2
     D  DJGA                   1     90  2
     D                                     DIM(10)
     D*
     D                 DS
     D  DJGE01               001    007  0
     D  DJGE02               008    014  0
     D  DJGE03               015    021  0
     D  DJGE04               022    028  0
     D  DJGE05               029    035  0
     D  DJGE06               036    042  0
     D  DJGE07               043    049  0
     D  DJGE08               050    056  0
     D  DJGE09               057    063  0
     D  DJGE10               064    070  0
     D  DJGE                   1    070  0
     D                                     DIM(10)
     D*
     D                 DS
     D  DJWD01               001    007  0
     D  DJWD02               008    014  0
     D  DJWD03               015    021  0
     D  DJWD04               022    028  0
     D  DJWD05               029    035  0
     D  DJWD06               036    042  0
     D  DJWD07               043    049  0
     D  DJWD08               050    056  0
     D  DJWD09               057    063  0
     D  DJWD10               064    070  0
     D  DJWD                   1    070  0
     D                                     DIM(10)
     D*
     D                 DS
     D  DJPI01               001    007  0
     D  DJPI02               008    014  0
     D  DJPI03               015    021  0
     D  DJPI04               022    028  0
     D  DJPI05               029    035  0
     D  DJPI06               036    042  0
     D  DJPI07               043    049  0
     D  DJPI08               050    056  0
     D  DJPI09               057    063  0
     D  DJPI10               064    070  0
     D  DJPI                   1    070  0
     D                                     DIM(10)
RKK  D*
RKK  D                 DS
RKK  D  XX150                  1    150
RKK  D  XX1501                 1     50
RKK  D  XX1502                51    100
RKK  D  XX1503               101    150
     D                 DS
     D  DJLM01                 1      1
     D  DJLM02                 2      2
     D  DJLM03                 3      3
     D  DJLM04                 4      4
     D  DJLM05                 5      5
     D  DJLM06                 6      6
     D  DJLM07                 7      7
     D  DJLM08                 8      8
     D  DJLM09                 9      9
     D  DJLM10                10     10
     D  DJLM                   1     10
     D                                     DIM(10)
     D*
     D                 DS
     D  XXDAT1                 1      8
     D  XXDAT2                 9     16
     D*
     D  XXDATY                 5      8  0
     D  XXDATJ                 5     12
     D*
     D                 DS
     D  a50a                   1     50
     D  a50b                  51    100
     D  a50c                 101    150
     D  a150                   1    150
     D                                     DIM(3)
     D*
     D                 DS
     D  d50a                   1     50
     D  d50b                  51    100
     D  d50c                 101    150
     D  d150                   1    150
     D                                     DIM(3)
     D*
     D                 DS
     D  covt                   1      2
     d  mcname                 3     42
     D  mcof                  43     51
     D  fill2                 52     71    inz(*blanks)
     D  coverage               1     71
     D*
     D                 DS
     D  name                   1     40
     D  phn#                  41     55  0
     D  fill3                 56     75    inz(*blanks)
     D  witness                1     75
     D*
     D                 ds
     D witness1                1     75
     D witness2               76    150
     D witness3              151    225
     D witness4              226    300
     D witness5              301    375
     D witnesses               1    375
     D*
     D                 DS
     D  CMDNY1                 1      2
     D  CMDNY2                 3      4
     D  CMDNY3                 5      6
     D  CMDNY4                 7      8
     D  CMDNY5                 9     10
     D  @DNY                   1     10    DIM(5)

     D* Stand alone
     D c               s              2  0
     D ad              s              3  0
     D dy              s              3  0
     D dr              s              3  0
     D cv              s              3  0
     D wn              s              3  0
     D deny            s             10
     D x1              s              5  0
     D x2              s              5  0
     D bi              s              3  0
     D cd              s              3  0
     D cda             s              3  0
     D chgdata#        s           3960
     d Q1exDt          s              8
     D*
     D                 DS
     D  frpart                 1      2
     d  frloc                  3      3
     D  frlocft                4      4
     D  fill4                  5     24    inz(*blanks)
     D  bodyinj                1     24
     D*
     D                 DS
     D  change#                1      4
     D  chgcode                5      5
     D  fill5                  6     40    inz(*blanks)
     D  chgdata                      40    dim(99)
     D*
     D                 DS
     D  cancode                1      1
     D  claim#                 2     26
     D  canres                27    176
     D  fill6                177    196    inz(*blanks)
     D  candata                1    196
     D*
     D*
     D                 DS
     D  xxzip                  1      9
t    D  xxzip5                 1      5
     D  xxzip4                 6      9
     D                 DS
     D  BNACOD                 1     10
     D  XXACOD                 2      4
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D*** Constants
     D*
     D XCE148          C                   CONST('No FROI has been ext-
     D                                     racted.')
     D#brnd1           s             80    inz
     D#brnd2           s             80    inz
     D*
     dintlen           s              5  0
     D*** Beg Add *** 03/24/25 ********************************************
     dXXprimy          s              1
     D*** End Add *** 03/24/25 ********************************************
     D/copy SPrRtvBrnd
     D*** begin add ***  01/22/24  *******************************************
     D/copy sprString
     D*** end add *****  01/22/24  *******************************************
     D                 ds
     D QCKEY                         40
     D  QCCO#                         3  0 OVERLAY(QCKEY)
     D  QCFND                         3  0 OVERLAY(QCKEY:4)
     D  QCFYR                         3  0 OVERLAY(QCKEY:7)
     D  QCCASE                        7  0 OVERLAY(QCKEY:10)
     D
      * Indicators
     D I_IMPAIR        S               N   INZ(*OFF)
     D I_REDIST        S               N   INZ(*OFF)
     d*=========================================================================
     d* Procedures
     d*** Beg Add *** 02/14/22 *************************************************
     d cvtDate         pr                  ExtPgm('SRCVTDAT')
     d  inDate                        8S 0
     d  inFormat                      8
     d  outFormat                     8
     d  outDate                      10
     d  outError                      1
     d  inDate         s              8S 0
     d  inFormat       s              8
     d  outFormat      s              8
     d  outDate        s             10
     d  outError       s              1
     d*** End Add *** 02/14/22 *************************************************
     d*
     I*
     I*
     IPCM02
     I              CMKEY                       C2KEY
     I              CMSEQ#                      C2SEQ#
     I              CMSTAT                      C2STAT
     I              CMPGM                       C2PGM
     I              CMTIME                      C2TIME
     I              CMDT                        C2DT
     I              CMDCDT                      C2DCDT
     I              CMDIFL                      C2DIFL
     I*** Beg Del *** 02/14/22 *******************************************
     IPLG02
     I************  LGCO#                       L2CO#
     I************  LGFND                       L2FND
     I************  LGFYR                       L2FYR
     I************  LGCASE                      L2CASE
     I************  LGTSET                      L2TSET
     I************  LGMTC                       L2MTC
     I************  LGJST                       L2JST
     I************  LGDUE                       L2DUE
     I************  LGSTS                       L2STS
     I************  LGSTAT                      L2STAT
     I************  LGEXDT                      L2EXDT
     I************  LGMTCD                      L2MTCD
     I************  LGSENT                      L2SENT
     I************  LGARCV                      L2ARCV
     I************  LGAENT                      L2AENT
     I************  LGDSQA                      L2DSQA
     I************  LGDSQB                      L2DSQB
     I************  LGDSQC                      L2DSQC
     I************  LGDSQD                      L2DSQD
     I************  LGDSQE                      L2DSQE
     I************  LGDSQF                      L2DSQF
     I************  LGDSQG                      L2DSQG
     I************  LGDSQH                      L2DSQH
     I************  LGDSQI                      L2DSQI
     I************  LGDSQJ                      L2DSQJ
     I************  LGDSQK                      L2DSQK
     I************  LGDSQL                      L2DSQL
     I************  LGDSQM                      L2DSQM
     I************  LGHCPY                      L2HCPY
     I************  LGSCHP                      L2SCHP
     I************  LGPRSQ                      L2PRSQ
     I************  LGIUSR                      L2IUSR
     I************  LGIDT                       L2IDT
     I************  LGITM                       L2ITM
     I************  LGIPGM                      L2IPGM
     I************  LGUSER                      L2USER
     I************  LGDT                        L2DT
     I************  LGTIME                      L2TIME
     I************  LGPGM                       L2PGM
     I************  LGRLS                       L2RLS
     I************  LGBLFL                      L2BLFL
     I*** End Del *** 02/14/22 *******************************************
     I*
     IXAJ01
     I              AJCO#                       DJCO#
     I              AJFND                       DJFND
     I              AJFYR                       DJFYR
     I              AJCASE                      DJCASE
     I              AJSEQ#                      DJSEQ#
     I              AJCD01                      DJCD01
     I              AJCD02                      DJCD02
     I              AJCD03                      DJCD03
     I              AJCD04                      DJCD04
     I              AJCD05                      DJCD05
     I              AJCD06                      DJCD06
     I              AJCD07                      DJCD07
     I              AJCD08                      DJCD08
     I              AJCD09                      DJCD09
     I              AJCD10                      DJCD10
     I              AJPD01                      DJPD01
     I              AJPD02                      DJPD02
     I              AJPD03                      DJPD03
     I              AJPD04                      DJPD04
     I              AJPD05                      DJPD05
     I              AJPD06                      DJPD06
     I              AJPD07                      DJPD07
     I              AJPD08                      DJPD08
     I              AJPD09                      DJPD09
     I              AJPD10                      DJPD10
     I              AJWA01                      DJWA01
     I              AJWA02                      DJWA02
     I              AJWA03                      DJWA03
     I              AJWA04                      DJWA04
     I              AJWA05                      DJWA05
     I              AJWA06                      DJWA06
     I              AJWA07                      DJWA07
     I              AJWA08                      DJWA08
     I              AJWA09                      DJWA09
     I              AJWA10                      DJWA10
     I              AJST01                      DJST01
     I              AJST02                      DJST02
     I              AJST03                      DJST03
     I              AJST04                      DJST04
     I              AJST05                      DJST05
     I              AJST06                      DJST06
     I              AJST07                      DJST07
     I              AJST08                      DJST08
     I              AJST09                      DJST09
     I              AJST10                      DJST10
     I              AJEN01                      DJEN01
     I              AJEN02                      DJEN02
     I              AJEN03                      DJEN03
     I              AJEN04                      DJEN04
     I              AJEN05                      DJEN05
     I              AJEN06                      DJEN06
     I              AJEN07                      DJEN07
     I              AJEN08                      DJEN08
     I              AJEN09                      DJEN09
     I              AJEN10                      DJEN10
     I              AJWK01                      DJWK01
     I              AJWK02                      DJWK02
     I              AJWK03                      DJWK03
     I              AJWK04                      DJWK04
     I              AJWK05                      DJWK05
     I              AJWK06                      DJWK06
     I              AJWK07                      DJWK07
     I              AJWK08                      DJWK08
     I              AJWK09                      DJWK09
     I              AJWK10                      DJWK10
     I              AJDY01                      DJDY01
     I              AJDY02                      DJDY02
     I              AJDY03                      DJDY03
     I              AJDY04                      DJDY04
     I              AJDY05                      DJDY05
     I              AJDY06                      DJDY06
     I              AJDY07                      DJDY07
     I              AJDY08                      DJDY08
     I              AJDY09                      DJDY09
     I              AJDY10                      DJDY10
     I              AJGA01                      DJGA01
     I              AJGA02                      DJGA02
     I              AJGA03                      DJGA03
     I              AJGA04                      DJGA04
     I              AJGA05                      DJGA05
     I              AJGA06                      DJGA06
     I              AJGA07                      DJGA07
     I              AJGA08                      DJGA08
     I              AJGA09                      DJGA09
     I              AJGA10                      DJGA10
     I              AJGE01                      DJGE01
     I              AJGE02                      DJGE02
     I              AJGE03                      DJGE03
     I              AJGE04                      DJGE04
     I              AJGE05                      DJGE05
     I              AJGE06                      DJGE06
     I              AJGE07                      DJGE07
     I              AJGE08                      DJGE08
     I              AJGE09                      DJGE09
     I              AJGE10                      DJGE10
     I              AJWD01                      DJWD01
     I              AJWD02                      DJWD02
     I              AJWD03                      DJWD03
     I              AJWD04                      DJWD04
     I              AJWD05                      DJWD05
     I              AJWD06                      DJWD06
     I              AJWD07                      DJWD07
     I              AJWD08                      DJWD08
     I              AJWD09                      DJWD09
     I              AJWD10                      DJWD10
     I              AJPI01                      DJPI01
     I              AJPI02                      DJPI02
     I              AJPI03                      DJPI03
     I              AJPI04                      DJPI04
     I              AJPI05                      DJPI05
     I              AJPI06                      DJPI06
     I              AJPI07                      DJPI07
     I              AJPI08                      DJPI08
     I              AJPI09                      DJPI09
     I              AJPI10                      DJPI10
     I              AJLTRN                      DJLTRN
     I              AJIUSR                      DJIUSR
     I              AJIDT                       DJIDT
     I              AJITM                       DJITM
     I              AJIPGM                      DJIPGM
     I              AJUSER                      DJUSER
     I              AJDT                        DJDT
     I              AJTIME                      DJTIME
     I              AJPGM                       DJPGM
     I              AJLM01                      DJLM01
     I              AJLM02                      DJLM02
     I              AJLM03                      DJLM03
     I              AJLM04                      DJLM04
     I              AJLM05                      DJLM05
     I              AJLM06                      DJLM06
     I              AJLM07                      DJLM07
     I              AJLM08                      DJLM08
     I              AJLM09                      DJLM09
     I              AJLM10                      DJLM10
     C*
     C*   ENTRY PARAMETERS (ALL ALPHANUMERIC)
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQJST             2
     C                   PARM                    QQEXDT            8
     C                   PARM                    QQTSET            1
     C                   PARM                    QQEDIF            3
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDYY'       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTCYM2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YYMD'       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     IXAPI         PLIST
     C                   PARM                    XXCO#
     C                   PARM                    XXJST             2
     C                   PARM                    XXTSET            3
     C                   PARM                    X1FND             3 0
     C                   PARM      *BLANKS       @RTNCD            1
     C                   PARM      *BLANKS       @EDIF             3
     C                   PARM      *BLANKS       IXELT             1
     C                   PARM      *ZEROS        IXELTD            3 0
     C                   PARM      *BLANKS       IXFROI            2
     C                   PARM      *ZEROS        IXSDUE            3 0
     C                   PARM      *BLANKS       IXRLS             1
     C                   PARM      *ZEROS        IXCSDT            7 0
     C                   PARM      *BLANKS       IXEADC            5
     C                   PARM      *BLANKS       IXHCPR            5
     C                   PARM      *BLANKS       IXFNRC            5
     C                   PARM      *BLANKS       IXHCED            1
     C                   PARM      *BLANKS       IXCRER            5
     C                   PARM      *BLANKS       IXCEIR            5
     C                   PARM      *BLANKS       IXCECR            5
     C                   PARM      *BLANKS       IXCTAR            5
     C                   PARM      *BLANKS       IXCTRR            5
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQJST         XXJST
     C                   MOVE      QQEXDT        X1EXDT            8 0
     C                   Z-ADD     X1EXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEXDT
     C*
     C*   INITIALIZATION SUBROUTINE
     C*
     C                   EXSR      SRINIT
     C*
     C*  CALL THE SUBROUTINE TO LOAD THE EXTRACT WORKFILE.
     C*
     C                   EXSR      SRLOAD
     C*
     C*   PROCESS ALL 148 TRANSACTIONS FIRST, THEN PROCESS A49'S.
     C*
     C                   MOVE      '1'           XXLOOP
     C                   MOVE      '148'         XXTSET
     C     QQTSET        IFEQ      'A'
     C                   MOVE      '2'           XXLOOP
     C                   MOVE      'A49'         XXTSET
     C                   END
     C     STRT          TAG
     C*
     C* MAINLINE OF PROGRAM.  LOOP THRU LOG FILE
     C*
     C     XTKY          CHAIN     PXT01                              60
B001 C     *IN60         DOWEQ     *OFF
     C*
     C*  GET THE EDI LOG HISTORY RECORD
     C*
     C     LGKY1         CHAIN     PLG01                              60
     C     *IN60         DOWEQ     *OFF
     C     LGSTS         IFEQ      'EI'
     C     LGSTS         OREQ      'EC'
     C     LGSTS         OREQ      'RE'
     C     LGTSET        IFEQ      XTTSET
     C     LGMTC         ANDEQ     XTMTC
     C                   LEAVE
     C                   END
     C                   END
     C     LGKY1         READE     PLG01                                  60
     C                   END
     C   60              GOTO      SKIP
     C*
     C                   Z-ADD     *ZEROS        SVISDT            7 0
     C*
     C*    Load the Data Element Array
     C*
     C                   Z-ADD     Q2RRN#        QQRRN#
     C                   CLEAR                   DN
     C*
      /Free
       //Make MTC date same as extract date for client 152
      /End-Free
       Q1exDt = %Editc(X1exDt:'X');
     C*
     C                   MOVE      LGSTS         SVSTS             2
     C                   MOVE      LGCO#         QQCO#             3
     C                   MOVE      LGFND         QQFND             3
     C                   MOVE      LGFYR         QQFYR             3
     C                   MOVE      LGCASE        QQCASE            7
     C                   MOVEL     LGTSET        QQTST1            3
     C                   MOVEL     LGMTC         QQMTC             2
     C                   MOVEL     LGJST         QQJST             2
     C*
     C                   CALL      'W8070R6'
     C                   PARM                    Q1EXDT
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQTST1
     C                   PARM                    QQMTC
     C                   PARM                    QQJST
     C                   PARM                    DN
     C                   MOVEL     DN(4)         XXJRST            2
     C*
     C*  Run the editor for each data element
     C*
     C                   MOVE      'N'           XXERR
     C     CMKY          CHAIN     PCM01                              60
     C   60              MOVE      *HIVAL        CMACDT
     C*
     C*  Insure that there has been an FROI extracted.
     C*
     C     LGTSET        IFEQ      'A49'
     C     CMIDT         ANDGT     XXCSDT
     C*
     C                   MOVE      *OFF          XXFROI
     C     L2KY          CHAIN     PLG02                              61
     C     *IN61         DOWEQ     *OFF
     C*
     C     L2TSET        IFEQ      '148'
     C     L2STS         IFNE      'EI'
     C     L2STS         ANDNE     'EC'
     C     L2STS         ANDNE     'TR'
     C                   MOVE      *ON           XXFROI
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*
     C     L2KY          READE     PLG02                                  61
     C                   ENDDO
     C*
     C     CLTID         IFNE      '139'
     C     XXFROI        IFEQ      *OFF
     C                   MOVE      '1'           *IN25
     C                   MOVE      'EI'          LGSTS
     C                   EXSR      SRDET3
     C                   MOVE      'Y'           XXERR
     C                   ENDIF
     C                   ENDIF
     C                   MOVE      '0'           *IN25
     C*
     C                   ENDIF
     C*
     C                   MOVEL     XXTSET        HDTSET
     C                   MOVEL     *BLANKS       XXTSET
     c                   MOVE      LGFND         X1FND
     c                   call      'WA076R'      ixapi                  99
     c                   if        @rtncd = 'Y'
     c                   move      '0'           *in91
     C                   MOVE      IXRLS         XXRLS
     c                   else
     c                   move      '1'           *in91
     c                   endif
     C     *IN91         IFEQ      *OFF
     C*
     C*
     C                   SELECT
     C     XXRLS         WHENEQ    '1'
     C                   MOVE      100           CNTS              3 0
     C     XXRLS         WHENEQ    '3'
     C                   MOVE      329           CNTS
     C     XXRLS         WHENEQ    'A'
     C                   MOVE      436           CNTS
     C                   MOVE      '3'           XXRLS
     C                   MOVE      *ON           XXRLSA            1
     C                   ENDSL
     C*
     C                   ENDIF
     C                   MOVEL     HDTSET        XXTSET
     C*
     C     1             DO        CNTS          P
     C                   CLEAR                   QQERRS
     C                   CALL      'W8070R7'
     C                   PARM                    XXCO#
     C                   PARM                    XXJST
     C                   PARM                    P
     C                   PARM                    LGTSET
     C                   PARM                    LGMTC
     C                   PARM                    DN
     C                   PARM                    QQERRS
     c                   PARM                    QQFND
     c                   PARM                    QQFYR
     c                   PARM                    QQCASE
     C*
     C*   CHECK EACH DATA ELEMENT FOR ERRORS
     C*
     C                   DO        10            M
     C     M             MULT      3             X
     C                   SUB       2             X
     C     3             SUBST     QQERRS:X      XXERRS
     C*
     C     XXERRS        IFNE      *BLANKS
     C                   MOVE      'EI'          LGSTS
     C*
     C     LGTSET        IFEQ      '148'
     C                   EXSR      SRDET2
     C                   ELSE
     C                   EXSR      SRDET3
     C                   END
     C                   MOVE      'Y'           XXERR
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   ENDDO
     C*
     C*
     C*  IF THERE WERE NO ERRORS FOR THIS RECORD;  CREATE A RECORD IN
     C*  THE EDI 148 SUBMISSION FILE, UPDATE THE LOG HISTORY RECORD,
     C*  AND PRINT THE RECORD ON THE DETAIL REPORT.
     C*
     C     LGTSET        IFEQ      '148'
     C*
     C*
     C                   MOVEL     LGTSET        XXTSET
     C                   MOVE      LGFND         X1FND
     C                   call      'WA076R'      ixapi                  99
     C                   if        @rtncd = 'Y'
     C                   MOVE      IXHCED        XXHCED            1
     C                   else
     C                   MOVE      *blank        XXHCED
     C                   endif
     C*
     C*
     C     XXERR         IFEQ      'N'
     C                   EXSR      SR148
     C                   EXSR      SRDIRC
     C*
     C     XXHCED        CASEQ     'E'           SRUPDT
     C                   ENDCS
     C*                                                                *
     C                   EXSR      SRDET
     C                   ADD       1             XXACPT
     C                   ELSE
     C     XXHCED        CASEQ     'E'           SRUPDE
     C                   ENDCS
     C                   ADD       1             XXREJ
     C                   ENDIF
     C                   END
     C*
     C*  IF THIS WAS AN A49 RECORD AND THERE WERE NO ERRORS, WRITE A
     C*  RECORD TO THE APPROPRIATE A49 FILE, UPDATE THE LOG HISTORY
     C*  RECORD, AND PRINT THE RECORD ON THE DETAIL REPORT.
     C*
     C     LGTSET        IFEQ      'A49'
     C     XXERR         IFEQ      'N'
     C                   EXSR      SRA49
     C                   EXSR      SRUPDT
     C                   ADD       1             X9ACPT
     C                   ELSE
     C                   EXSR      SRUPDE
     C                   ADD       1             X9REJ
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ADD       1             X4CNT
     C*
     C     SKIP          TAG
     C*
     C*   PROCESS NEXT LOG RECORD
     C*
     C     XTKY          READE     PXT01                                  60
E001 C                   ENDDO
     C     XXLOOP        IFEQ      '1'
     C     QQTSET        ANDEQ     'B'
     C                   MOVE      '2'           XXLOOP
     C                   MOVE      'A49'         XXTSET
     C                   GOTO      STRT
     C                   END
     C*
     C*   PRINT NO RECORDS SELECTED MESSAGES IF NO DETAILS WRITTEN
     C*
     C     QQTSET        IFEQ      '1'
     C     QQTSET        OREQ      'B'
     C*
     C     *IN63         IFEQ      '0'
     C                   EXCEPT    HEAD
     C                   EXCEPT    MSG
     C                   ENDIF
     C     *IN67         IFEQ      '0'
     C                   EXCEPT    HEAD2
     C                   EXCEPT    MSG2
     C                   ENDIF
     C                   ENDIF
     C*
     C     QQTSET        IFEQ      'A'
     C     QQTSET        OREQ      'B'
     C*
     C     *IN65         IFEQ      '0'
     C                   EXCEPT    HEAD1
     C                   EXCEPT    MSG1
     C                   ENDIF
     C     *IN69         IFEQ      '0'
     C                   EXCEPT    HEAD3
     C                   EXCEPT    MSG3
     C                   ENDIF
     C                   ENDIF
     C*
     C*   PRINT THE SUMMARY REPORT
     C*
     C     XXACPT        ADD       XXREJ         XX148T
     C     X9ACPT        ADD       X9REJ         XXA49T
     C                   EXCEPT    HEAD4
     C                   EXCEPT    DET4
     C*
     C*   UPDATE THE SUBMISSION MANAGEMENT RECORD
     C*
     C                   MOVE      '148'         XXTSET
     C     M1KY          CHAIN     PM101                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     XX148T        M1148T
     C                   Z-ADD     XXACPT        M1148S
     C                   Z-ADD     0             M1A49T
     C                   Z-ADD     0             M1A49S
     C                   MOVE      *BLANKS       M1STAT
     C                   MOVEL     Q1PGM         M1IPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        M1ITM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         M1IDT
     C                   UPDATE    PM101
     C                   END
     C*
     C                   MOVE      'A49'         XXTSET
     C     M1KY          CHAIN     PM101                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     0             M1148T
     C                   Z-ADD     0             M1148S
     C                   Z-ADD     XXA49T        M1A49T
     C                   Z-ADD     X9ACPT        M1A49S
     C                   MOVE      *BLANKS       M1STAT
     C                   MOVEL     Q1PGM         M1IPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        M1ITM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         M1IDT
     C                   UPDATE    PM101
     C                   END
     C*
     C*  END OF PROGRAM
     C*
     C     TERM          TAG
     C*
      /Free
       If %open(qsysprt);
          xxFile = '0';
          Chain orky POR01;
          If %found;
             Except Route;
          Endif;
       Endif;
       If %open(qsysprt2);
          xxFile = '2';
          Chain ORKY POR01;
          If %found;
             Except Route2;
          Endif;
       Endif;
       xxFile = '4';
          Chain ORKY POR01;
       If %found;
          Except Route4;
       Endif;
       If %open(qsysprt1);
          xxFile = '1';
          Chain ORKY POR01;
          If %found;
             Except Route1;
          Endif;
       Endif;
       if %open(qsysprt3);
          xxFile = '3';
          Chain ORKY POR01;
          If %found;
             Except Route3;
          Endif;
       Endif;
      /End-free
     C     QQTSET        IFEQ      '1'
     C     QQTSET        OREQ      'B'
     C                   CLOSE     QSYSPRT
     C                   CLOSE     QSYSPRT2
     C                   ENDIF
     C*
     C     QQTSET        IFEQ      'A'
     C     QQTSET        OREQ      'B'
     C                   CLOSE     QSYSPRT1
     C                   CLOSE     QSYSPRT3
     C                   ENDIF
     C*
     C                   MOVE      '1'           *INLR
     C*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     C*
     C* DEFINE ALL KLISTS, FIELD DEFINITIONS, ETC...
     C*
     C     SRINIT        BEGSR
     C*
     C*  SMCOL - KEY LIST FOR THE COMPANY MASTER FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  SMORL - KEY LIST FOR THE OUTPUT ROUTING FILE
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXFILE
     C*
     C*  WMIXL - KEY LIST FOR THE EDI CLAIMS CONTROL FILE
     C*
     C     IXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJST
     C                   KFLD                    XXTSET
     C*
     C*  WMLGL2 - KEY LIST FOR THE EDI LOG HISTORY FILE
     C*
     C     LGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJST
     C*
     C*  KEY LIST FOR THE DARTS REMARK FILE
     C*
     C     SDKEY         KLIST
     C                   KFLD                    XXCKEY
     C                   KFLD                    XXDSEQ
     C*
     C*  WMM1L - SYSTEM MANAGEMENT RECORDS
     C*
     C     M1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJST
     C                   KFLD                    XXEXDT
     C                   KFLD                    XXTSET
     C                   KFLD                    XXEDIF
     C*
     C*  WTERL - ERROR MESSAGE TABLE FILE
     C*
     C     ERKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXERR#
     C*
     C*  WTDCL - DATA ELEMENT TABLE FILE
     C*
     C     DCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJST
     C                   KFLD                    XXDN#
     C*
     C*  WTABL - Work Comp Table File
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C*
     C*  WMLGL - KEY LIST FOR THE EDI LOG HISTORY FILE
     C*
     C     L2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    LGFND
     C                   KFLD                    LGFYR
     C                   KFLD                    LGCASE
     C*
     C*  WMBYL - KEY LIST FOR THE BODY PARTS FILE
     C*
     C     BYKY          KLIST
     C                   KFLD                    LGCO#
     C                   KFLD                    LGFND
     C                   KFLD                    LGFYR
     C                   KFLD                    LGCASE
     C*
     C*  WMBNL - KEY LIST FOR THE BENEFIT ADJ FILE
     C*
     C     BNKY          KLIST
     C                   KFLD                    LGCO#
     C                   KFLD                    LGFND
     C                   KFLD                    LGFYR
     C                   KFLD                    LGCASE
     C*
     C*  WDPAL6 - KEY LIST FOR THE PAYMENTS/ADJUSTMENTS (DESCEND BY DT)
     C*
     C     PAKY          KLIST
     C                   KFLD                    LGCO#
     C                   KFLD                    LGFND
     C                   KFLD                    LGFYR
     C                   KFLD                    LGCASE
     C*
     C*  KEY LIST FOR THE GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
     C*
     C*  KEY LIST FOR THE A49 PAYMENT/ADJUSTMENT CORRECTION FILE
     C*
     C     AJKY          KLIST
     C                   KFLD                    LGCO#
     C                   KFLD                    LGFND
     C                   KFLD                    LGFYR
     C                   KFLD                    LGCASE
     C                   KFLD                    LGDSQM
     C*
     C*  KEY LIST FOR THE EDI LOG FILE
     C*
     C     LGKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJST
     C                   KFLD                    XTFND
     C                   KFLD                    XTFYR
     C                   KFLD                    XTCASE
     C*
     C*  KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    LGFND
     C                   KFLD                    LGFYR
     C                   KFLD                    LGCASE
     C*
     C*  KEY LIST FOR THE CLAIMANT DEMOGRAPHIC FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*
     C*  KEY LIST FOR THE EDI EXTRACT WORKFILE
     C*
     C     XTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTSET
     C*
     C*  Key list for the A49 PTD/Reduced Earnings/Recoveries file
     C*
     C     REKY          KLIST
     C                   KFLD                    LGCO#
     C                   KFLD                    LGFND
     C                   KFLD                    LGFYR
     C                   KFLD                    LGCASE
     C                   KFLD                    LGDSQH
     C*
     C*  KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    XXSTAF
     C*
     C*  Fund master
     C     FNKY          KLIST
     C                   KFLD                    LGCO#
     C                   KFLD                    LGFND
     C*
     C     P0KY          KLIST
     C                   KFLD                    LGCO#
     C                   KFLD                    LGFND
     C                   KFLD                    LGFYR
     C                   KFLD                    LGCASE
     C                   KFLD                    LGDSQH
     C*
     C     *LIKE         DEFINE    LGCO#         SVCO#
     C     *LIKE         DEFINE    LGFND         SVFND
     C     *LIKE         DEFINE    XXTSET        HDTSET
     C     *LIKE         DEFINE    PASEQ#        KYSEQ#
     C     *LIKE         DEFINE    PASEQ#        SVSEQ#
     C     *LIKE         DEFINE    PAPODT        XXPODT
     C*
     C*  KEY LIST FOR THE WITNESS FILE
     C*
     C     WIKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     RPKY          KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
     C                   KFLD                    XXSEQ#
     C*
     C     CPKY          KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PATRDT
     C                   KFLD                    PAFND
     C*
     C*
     C     CPKY1         KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    XXPODT
     C                   KFLD                    PAFND
     C*
     C     A3KY          KLIST
     C                   KFLD                    LGCO#
     C                   KFLD                    LGFND
     C                   KFLD                    LGFYR
     C                   KFLD                    LGCASE
     C*
     C     A3KY1         KLIST
     C                   KFLD                    LGCO#
     C                   KFLD                    LGFND
     C                   KFLD                    LGFYR
     C                   KFLD                    LGCASE
     C                   KFLD                    XXBTC             3
     C*
     C     E3KY          KLIST
     C                   KFLD                    LGCO#
     C                   KFLD                    LGFND
     C                   KFLD                    LGFYR
     C                   KFLD                    LGCASE
     C                   KFLD                    XXENAM           40
     C*
     C     R3KY          KLIST
     C                   KFLD                    LGCO#
     C                   KFLD                    LGFND
     C                   KFLD                    LGFYR
     C                   KFLD                    LGCASE
     C                   KFLD                    XXREWK            2 0
     C*
     C     PAKY1         KLIST
     C                   KFLD                    LGCO#
     C                   KFLD                    LGFND
     C                   KFLD                    LGFYR
     C                   KFLD                    LGCASE
     C                   KFLD                    KYSEQ#
     C*
     C     HOKY          KLIST
     C                   KFLD                    LGCO#
     C                   KFLD                    LGFND
     C                   KFLD                    LGFYR
     C                   KFLD                    LGCASE
     C                   KFLD                    LGTSET
     C                   KFLD                    LGMTC
     C*
     C     NOKY          KLIST
     C                   KFLD                    LGCO#
     C                   KFLD                    LGFND
     C                   KFLD                    LGFYR
     C                   KFLD                    LGCASE
     C                   KFLD                    LGMTC
     C                   KFLD                    LGTSET
     C*
     C     WXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    QQJST
     C                   KFLD                    X1EXDT
     C                   KFLD                    XXTSET
     C                   KFLD                    FRACLC
     C                   KFLD                    QQMTC
     C*
     C     *LIKE         DEFINE    LGCO#         XXCO#
     C     *LIKE         DEFINE    LGJST         XXJST
     C     *LIKE         DEFINE    LGEXDT        XXEXDT
     C     *LIKE         DEFINE    LGTSET        XXTSET
     C     *LIKE         DEFINE    ORFIL#        XXFILE
     C     *LIKE         DEFINE    ORTITL        XXTITL
     C     *LIKE         DEFINE    ORTITL        X2TITL
     C     *LIKE         DEFINE    ORTITL        X4TITL
     C     *LIKE         DEFINE    Q2RRN#        QQRRN#
     C     *LIKE         DEFINE    NAKEY         XXKEY
     C     *LIKE         DEFINE    NADESC        XXNATR
     C     *LIKE         DEFINE    PADESC        XXPART
     C     *LIKE         DEFINE    CADESC        XXCAUS
     C     *LIKE         DEFINE    ERMSG#        XXERR#
     C     *LIKE         DEFINE    ERDESC        XXEDSC
     C     *LIKE         DEFINE    DCDN#         XXDN#
     C     *LIKE         DEFINE    C2KEY         XXCKEY
     C     *LIKE         DEFINE    LGDSQA        XXDSEQ
     C     *LIKE         DEFINE    ORTITL        X1TITL
     C     *LIKE         DEFINE    ORTITL        X3TITL
     C     *LIKE         DEFINE    *IN60         XXFROI
     C     *LIKE         DEFINE    PADESC        XXBPDS
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTDESC        XXBADS
     C     *LIKE         DEFINE    IXCSDT        XXCSDT
     C     *LIKE         DEFINE    GTDESC        XXPDDS
     C     *LIKE         DEFINE    GTDESC        XXDPRL
     C     *LIKE         DEFINE    LGFND         XXFND
     C     *LIKE         DEFINE    LGFYR         XXFYR
     C     *LIKE         DEFINE    LGCASE        XXCASE
     C     *LIKE         DEFINE    BNAMT         XBNAMT
     C**   *LIKE         DEFINE    BNAMT         XBNSDT
     C     *LIKE         DEFINE    BNSTDT        XBNSDT
     C     *LIKE         DEFINE    BNENDT        XXCURD
     C     *LIKE         DEFINE    PASEQ#        XXSEQ#
     C                   IF        Q1PRMS > 4
     C                   MOVE      QQEDIF        XXEDIF            3
     C                   ENDIF
     C*
     C                   MOVE      *ZEROS        XXACLC
     C                   MOVE      *ZEROS        XXDATE            8 0
     C                   MOVE      *ZEROS        P                 5 0
     C                   MOVE      *ZEROS        M                 5 0
     C                   MOVE      *ZEROS        X                 5 0
     C                   MOVE      *ZEROS        XXCLEN           15 5
     C                   MOVE      *BLANKS       XXWK30           30
     C                   MOVE      *BLANKS       QQERRS           30
     C                   MOVE      *BLANKS       XXERRS            3
     C                   MOVE      *BLANK        XXERR             1
     C                   MOVE      *ZEROS        XXACPT            7 0
     C                   MOVE      *ZEROS        XXREJ             7 0
     C                   MOVE      *ZEROS        X4CNT             7 0
     C                   MOVE      *BLANKS       XXACD1           75
     C                   MOVE      *BLANKS       XXACD2           75
     C                   MOVE      *BLANKS       XXSEX            30
     C                   MOVE      *BLANKS       XXSIC            30
     C                   MOVE      *BLANKS       XXITRT           30
     C                   MOVE      *BLANKS       XXMSTS           30
     C                   MOVE      *BLANKS       XXESTS           30
     C                   MOVE      *ZEROS        X9ACPT            7 0
     C                   MOVE      *ZEROS        X9REJ             7 0
     C                   MOVE      *ZEROS        XX148T            7 0
     C                   MOVE      *ZEROS        XXA49T            7 0
     C                   MOVE      *BLANKS       XXLOOP            1
     C                   MOVE      *BLANKS       XXCLNM           40
     C                   MOVE      *BLANKS       XXSTAF            3
     C                   MOVE      *BLANKS       XXADJ            40
     C                   MOVE      *BLANKS       X2ADJ            40
     C                   MOVE      *BLANKS       X3ADJ            40
     C                   MOVE      'N'           XXPROC            1
     C                   TIME                    DSDTTM
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKY          CHAIN     PCO01                              60
     C   60              MOVE      *BLANKS       CONAME
     C                   callp     RtvBrnd('@':#Brnd1:#brnd2)
     C*
     C*  GET THE ROUTING RECORDS AND PRINT THE ROUTING PAGES
     C*
     C     QQTSET        IFEQ      '1'
     C     QQTSET        OREQ      'B'
     C*
     C                   MOVE      *ON           *IN83
     C*
     C                   OPEN      QSYSPRT
     C                   OPEN      QSYSPRT2
     C*
     C                   MOVE      '0'           XXFILE
     C     ORKY          CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     ORTITL        XXTITL
     C                   END
     C*
     C                   MOVE      '2'           XXFILE
     C     ORKY          CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     ORTITL        X2TITL
     C                   END
     C*
     C                   ENDIF
     C*
     C                   MOVE      '4'           XXFILE
     C     ORKY          CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     ORTITL        X4TITL
     C                   END
     C*
     C     QQTSET        IFEQ      'A'
     C     QQTSET        OREQ      'B'
     C*
     C                   MOVE      *ON           *IN84
     C*
     C                   OPEN      QSYSPRT1
     C                   OPEN      QSYSPRT3
     C*
     C                   MOVE      '1'           XXFILE
     C     ORKY          CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     ORTITL        X1TITL
     C                   END
     C*
     C                   MOVE      '3'           XXFILE
     C     ORKY          CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     ORTITL        X3TITL
     C                   END
     C*
     C                   ENDIF
     C*
     C                   MOVE      '1'           *INOA
     C                   MOVE      '1'           *INOB
     C                   MOVE      '1'           *INOC
     C                   MOVE      '1'           *INOD
     C                   MOVE      '1'           *INOE
     C*
     C                   ENDSR
     C*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     C*
     C* MOVE FIELDS TO RECORD 148 FIELDS AND THEN WRITE RECORD
     C*
-BSR CSR   SR148         BEGSR
     C*                                                                   SR148
     C*   CREATE A RECORD FOR THE EDI 148 FLAT FILE, IF THE               SR148
     C*    ELECTRONIC SUBMISSION FLAG INDICATES.                          SR148
     C*                                                                   SR148
     C     XXHCED        IFEQ      'E'
     C*                                                                   SR148
     C                   Z-ADD     XXCO#         FRCO#
     C                   Z-ADD     XXEXDT        FREXDT
     C                   MOVEL     DN(1)         FRTSET
     C                   MOVEL     DN(2)         FRMTC
     C     DN(3)         IFEQ      *BLANKS
     C                   MOVE      *ZEROS        DN(3)
     C                   END
     C                   MOVEL     DN(3)         XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         FRTDT
     C                   MOVEL     DN(4)         FRJRST
     C                   MOVEL     DN(5)         FRACL#
     C     DN(6)         IFEQ      *BLANKS
     C                   MOVE      *ZEROS        DN(6)
     C                   END
     C                   MOVE      DN(6)         FRCLCD
     C                   MOVEL     DN(7)         FRCLSN
     C                   MOVE      DN(8)         FRPADC
     C                   MOVEL     DN(9)         FRPLSN
     C                   MOVEL     DN(10)        FRAAD1
     C                   MOVEL     DN(11)        FRAAD2
     C                   MOVEL     DN(12)        FRACIT
     C                   MOVEL     DN(13)        FRAST
     C                   MOVE      DN(14)        FRAZP
     C     XXRLS         IFEQ      '3'
     C                   MOVE      FRAZP         XXZIP
     C     XXZIP4        IFEQ      '0000'
     C                   MOVE      '    '        FRAZP
     C                   ENDIF
     C                   ENDIF
     C                   MOVEL     DN(15)        FRACLC
     C     DN(16)        IFEQ      *BLANKS
     C                   MOVE      *ZEROS        DN(16)
     C                   END
     C                   MOVE      DN(16)        FREMP#
     C                   MOVEL     DN(17)        FRILSN
     C                   MOVEL     DN(18)        FRELSN
     C                   MOVEL     DN(19)        FREAD1
     C                   MOVEL     DN(20)        FREAD2
     C                   MOVEL     DN(21)        FRECIT
     C                   MOVEL     DN(22)        FREST
     C                   MOVE      DN(23)        FREZP
     C     XXRLS         IFEQ      '3'
     C                   MOVE      FREZP         XXZIP
     C     XXZIP4        IFEQ      '0000'
     C                   MOVE      '    '        FREZP
     C                   ENDIF
     C                   ENDIF
     C                   IF        FRJRST = 'CA' and
     C                             %SUBST(FREZP:6:4) = '0000'
     C                   EVAL      %SUBST(FREZP:6:4) = *BLANKS
     C                   ENDIF
     C                   MOVEL     DN(24)        FRSFIN
     C                   MOVEL     DN(25)        FRSIC
     C*
     C*** FOR TX LOAD THE ADDRESS INFO IF TPA FLAG IS NOT = 'Y'
     C*
     C     FRJRST        IFEQ      'TX'
     C                   MOVEL(P)  'SIC'         XXTBID
     C                   MOVEL(P)  FRSIC         XXKEY
     C*
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      '0'
     C     GTCF01        ANDNE     *BLANKS
     C                   MOVEL(P)  GTCF01        FRSIC
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVEL     DN(26)        FRRPT#
     C                   MOVEL     DN(27)        FRLOC#
     C                   MOVEL     DN(28)        FRPOL#
     C     DN(29)        IFEQ      *BLANKS
     C                   MOVE      *ZEROS        DN(29)
     C                   END
     C                   MOVEL     DN(29)        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         FRPEFD
     C     DN(30)        IFEQ      *BLANKS
     C                   MOVE      *ZEROS        DN(30)
     C                   END
     C                   MOVEL     DN(30)        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         FRPEXD
     C     DN(31)        IFEQ      *BLANKS
     C                   MOVE      *ZEROS        DN(31)
     C                   END
     C                   MOVEL     DN(31)        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         FRACDT
     C     DN(32)        IFEQ      *BLANKS
     C                   MOVE      *ZEROS        DN(32)
     C                   END
     C                   MOVE      DN(32)        FRACTM
     C                   MOVE      DN(33)        FRAZP1
     C     XXRLS         IFEQ      '3'
     C                   MOVE      FRAZP1        XXZIP
     C     XXZIP4        IFEQ      '0000'
     C                   MOVE      '    '        FRAZP1
     C                   ENDIF
     C                   ENDIF
     C                   IF        FRJRST = 'CA' and
     C                             %SUBST(FRAZP1:6:4) = '0000'
     C                   EVAL      %SUBST(FRAZP1:6:4) = *BLANKS
     C                   ENDIF
     C                   MOVEL     DN(34)        FREADR
     C                   MOVEL     DN(35)        FRNATR
     C                   MOVEL     DN(36)        FRPART
     C                   MOVEL     DN(37)        FRCAUS
     C                   MOVEL     DN(38)        FRACDS
     C                   MOVEL     DN(39)        FRITRT
     C     DN(40)        IFEQ      *BLANKS
     C                   MOVE      *ZEROS        DN(40)
     C                   END
     C                   MOVEL     DN(40)        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         FRDTRP
     C     DN(41)        IFEQ      *BLANKS
     C                   MOVE      *ZEROS        DN(41)
     C                   END
     C                   MOVEL     DN(41)        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         FRRCVD
     C     DN(42)        IFEQ      *BLANKS
     C                   MOVE      *ZEROS        DN(42)
     C                   END
     C                   MOVEL     DN(42)        FRSS#
     C                   MOVEL     DN(43)        FRLSN
     C                   MOVEL     DN(44)        FRFSN
     C                   MOVEL     DN(45)        FRMID
     C                   MOVEL     DN(46)        FRADR1
     C                   MOVEL     DN(47)        FRADR2
     C                   MOVEL     DN(48)        FRCTY
     C                   MOVEL     DN(49)        FRST
     C                   MOVE      DN(50)        FRZIP
     C     XXRLS         IFEQ      '3'
     C                   MOVE      FRZIP         XXZIP
     C     XXZIP4        IFEQ      '0000'
     C                   MOVE      '    '        FRZIP
     C                   ENDIF
     C                   ENDIF
     C                   MOVE      DN(51)        FRPHN#
     C     DN(52)        IFEQ      *BLANKS
     C                   MOVE      *ZEROS        DN(52)
     C                   END
     C                   MOVEL     DN(52)        XXDATE
     C                   MOVE      XXDATE        XXDAT1
     C     XXDATY        IFLT      1940
     C                   MOVE      XXDATE        XXDAT2
     C                   MOVE      XXDATJ        FRDTOB
     C                   ELSE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         FRDTOB
     C                   ENDIF
     C                   MOVEL     DN(53)        FRSEX
     C                   MOVEL     DN(54)        FRMSTS
     C     DN(55)        IFEQ      *BLANKS
     C                   MOVE      *ZEROS        DN(55)
     C                   END
     C                   MOVE      DN(55)        FRDEP
     C     DN(56)        IFEQ      *BLANKS
     C                   MOVE      *ZEROS        DN(56)
     C                   END
     C                   MOVEL     DN(56)        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         FRDBDT
     C     DN(57)        IFEQ      *BLANKS
     C                   MOVE      *ZEROS        DN(57)
     C                   END
     C                   MOVEL     DN(57)        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         FRDOD
     C                   MOVEL     DN(58)        FRESTS
     C                   MOVE      DN(59)        FRCLAS
     C                   MOVEL     DN(60)        FROCC
     C     DN(61)        IFEQ      *BLANKS
     C                   MOVE      *ZEROS        DN(61)
     C                   END
     C                   MOVEL     DN(61)        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         FRDOH
     C                   MOVE      DN(62)        FRAWW
     C                   MOVE      *BLANKS       FRWGPD
     C                   MOVEL     DN(63)        XXWK1A            1
     C* Process different formats for releases
     C                   IF        XXRLS = '3'
     C                   MOVE      XXWK1A        XXWK2N            2 0
     C                   MOVE      XXWK2N        FRWGPD
     C                   IF        FRWGPD = '00'
     C                   MOVE      *BLANKS       FRWGPD
     C                   ENDIF
     C                   ELSE
     C                   MOVE      XXWK1A        FRWGPD
     C                   ENDIF
     C     DN(64)        IFEQ      *BLANKS
     C                   MOVE      *ZEROS        DN(64)
     C                   END
     C                   MOVE      DN(64)        FRDAYS
     C     DN(65)        IFEQ      *BLANKS
     C                   MOVE      *ZEROS        DN(65)
     C                   END
     C                   MOVEL     DN(65)        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         FRDTLW
     C                   MOVEL     DN(66)        FRFSAL
     C                   MOVEL     DN(67)        FRCSAL
     C     DN(68)        IFEQ      *BLANKS
     C                   MOVE      *ZEROS        DN(68)
     C                   END
     C                   MOVEL     DN(68)        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         FRRTW
     C*
     C* Populate R21 Fields
     C*
     C     XXRLS         IFEQ      '3'
     C                   EXSR      SRPTP
     C*                  EXSR      SR148B
     C                   EXSR      SRR21
     C                   ENDIF
       //*** Beg Add *** 02/14/22 *********************************************
       If xxRlsa = *on;
          Monitor;
          FRarDt    = %Dec(%Subst(DN(441):1:7):7:0);
          On-error;
             FRarDt = 0;
          Endmon;
       Endif;
       //*** End Add *** 02/14/22 *********************************************
     C                   MOVE      'A'           FRSTAT
     C                   MOVEL     Q1USER        FRUSER
     C                   MOVEL     Q1PGM         FRPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        FRTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FRDT
     c                   eval      FRfnd  = LGfnd
     c                   eval      FRyear = LGfyr
     c                   eval      FRcase = LGcase
     c
     C*                                                                   SR148
     C                   WRITE     PFR01
     C*                                                                   SR148
     C                   END
     C*                                                                   SR148
-ESR CSR                 ENDSR
     C*****************************************************************
     C*  SRDIRC - COMPLETE DIARY RECORDS.
     C*****************************************************************
     C     SRDIRC        BEGSR
     C*
     C*  Claim is complete and extracted:
     C*
     C*    o complete diaries A-C (1-3) for Initial submission (MTC=00)
     C*    o complete diaries D-F (4-6) for Subsequent (MTC=02 or CO)
     C*
     C                   MOVEL     XXACLC        XXCKEY
     C*
     C                   Z-ADD     0             T                 3 0
     C     LGMTC         IFEQ      '00'
     C                   Z-ADD     1             T
     C                   ENDIF
     C     LGMTC         IFEQ      '02'
     C     LGMTC         OREQ      'CO'
     C                   Z-ADD     4             T
     C                   ENDIF
     C*
     C*  Check each log sequence number
     C     T             IFGT      0
     C     T             DO        3             S                 3 0
     C*
     C     SQ(S)         IFGT      0
     C                   Z-ADD     SQ(S)         XXDSEQ
     C     SDKEY         CHAIN     PCM02                              63
     C     *IN63         IFEQ      *OFF
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C2DCDT
     C                   UPDATE    PCM02
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     C*
     C* MOVE FIELDS TO RECORD A49 FIELDS AND THEN WRITE RECORD
     C*
-BSR CSR   SRA49         BEGSR
     C*                                                                   SRA49
     C*   CREATE A RECORD FOR THE EDI A49 FLAT FILES, IF THE              SRA49
     C*    ELECTRONIC SUBMISSION FLAG INDICATES.                          SRA49
     C*                                                                   SRA49
     C                   Z-ADD     XXCO#         SUCO#
     C                   Z-ADD     XXEXDT        SUEXDT
     C                   MOVEL     DN(1)         SUTSET
     C                   MOVEL     DN(2)         SUMTC
     C     DN(3)         IFEQ      *BLANKS
     C                   MOVE      *ZEROS        DN(3)
     C                   END
     C                   MOVEL     DN(3)         XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         SUTDT
     C                   MOVEL     DN(4)         SUJRST
     C     DN(6)         IFEQ      *BLANKS
     C                   MOVE      *ZEROS        DN(6)
     C                   END
     C                   MOVE      DN(6)         SUCLCD
     C                   MOVE      DN(8)         SUFEIN
     C                   MOVE      DN(14)        SUAZIP
     C                   MOVE      SUAZIP        XXZIP
     C     XXZIP4        IFEQ      '0000'
     C                   MOVE      '    '        SUAZIP
     C                   ENDIF
     C     DN(42)        IFEQ      *BLANKS
     C                   MOVE      *ZEROS        DN(42)
     C                   END
     C                   MOVEL     DN(42)        SUSS#
     C     DN(55)        IFEQ      *BLANKS
     C                   MOVE      *ZEROS        DN(55)
     C                   END
     C                   MOVE      DN(55)        SUDEP
     C                   MOVEL     DN(69)        SUPRED
     C     DN(56)        IFEQ      *BLANKS
     C                   MOVE      *ZEROS        DN(56)
     C                   END
     C                   MOVEL     DN(56)        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         SUDBDT
     C     DN(70)        IFEQ      *BLANKS
     C                   MOVE      *ZEROS        DN(70)
     C                   END
     C                   MOVEL     DN(70)        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         SUMAXI
     C     DN(72)        IFEQ      *BLANKS
     C                   MOVE      *ZEROS        DN(72)
     C                   END
     C                   MOVEL     DN(72)        XXDATE
     C                   IF        DN(72) = *BLANKS or DN(72)= *ZEROS
     C                   IF        DN(68) <> *BLANKS
     C                   MOVEL     DN(68)        XXDATE
     C                   ENDIF
     C                   ENDIF
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         SURTW
     C*
      *** RTW DATE FOR FL - REMOVE DEFAULT VALUE (INITIAL RTW DATE) IF
      ***                   NOT POPULATED.
      ***
     C                   IF        DN(72) = *BLANKS or DN(72)= *ZEROS
     C                   IF        SUJRST = 'FL'
     C                   MOVE      *ZEROS        SURTW
     C                   ENDIF
     C                   ENDIF
     C                   MOVEL     DN(71)        SURETW
     C                   IF        XXRLS = '1'
     C*
     C* FOR R1 ONLY REPORT DN71 IF DN72 WAS LOADED FROM WORK HIST RCD.
     C*
     C     SURTW         IFEQ      *ZEROS
     C                   MOVEL     *BLANKS       SURETW
     C                   ENDIF
     C                   ENDIF
     C*
     C     DN(57)        IFEQ      *BLANKS
     C                   MOVE      *ZEROS        DN(57)
     C                   END
     C                   MOVEL     DN(57)        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         SUEDTD
     C                   MOVE      DN(62)        SUWAGE
     C                   MOVE      *BLANKS       SUWGPD
     C                   MOVEL(P)  DN(63)        XXWK1A
     C* Process different formats for releases
     C                   IF        XXRLS = '3'
     C                   MOVE      XXWK1A        XXWK2N            2 0
     C                   MOVE      XXWK2N        SUWGPD
     C                   IF        SUWGPD = '00'
     C                   MOVE      *BLANKS       SUWGPD
     C                   ENDIF
     C                   ELSE
     C                   MOVE      XXWK1A        SUWGPD
     C                   ENDIF
     C     DN(64)        IFEQ      *BLANKS
     C                   MOVE      *ZEROS        DN(64)
     C                   END
     C                   MOVE      DN(64)        SUDAYS
     C                   MOVEL     DN(67)        SUCSAL
     C     DN(31)        IFEQ      *BLANKS
     C                   MOVE      *ZEROS        DN(31)
     C                   END
     C                   MOVEL     DN(31)        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         SUACDT
     C                   MOVEL     DN(26)        SURPT#
     C                   MOVEL     DN(15)        SUACLC
     C                   MOVEL     DN(5)         SUACL#
     C                   MOVEL     DN(73)        SUCSTS
     C                   MOVEL     DN(74)        SUCTYP
     C                   MOVEL     DN(75)        SUACCD
     C     DN(76)        IFEQ      *BLANKS
     C                   MOVE      *ZEROS        DN(76)
     C                   END
     C                   MOVEL     DN(76)        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         SURPDT
     C                   MOVEL     DN(77)        SULATC
     C                   MOVE      DN(78)        SU#PI
     C                   MOVE      DN(79)        SU#PA
     C                   MOVE      DN(80)        SU#BA
     C                   MOVE      DN(81)        SU#RER
     C                   MOVE      DN(82)        SU#DDR
     C                   Z-ADD     *ZEROS        SU#PMT
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'SUSS#'       QQNAME           25
     C     SUSS#         PARM      SUSS#         QQVAL           100
     C*
     C*  PRINT THE A49 DETAIL LINES
     C*
     C                   EXCEPT    HEAD1
     C                   EXCEPT    DET1
     C                   MOVE      '1'           *IN65
     C                   CALL      'SPIMASK'
     C                   PARM      '*UNMASK'     QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'SUSS#'       QQNAME           25
     C     SUSS#         PARM      SUSS#         QQVAL           100
     C*
     C*  IF PROCESSING A A49, DETERMINE WHICH SEGMENT TO PROCESS.
     C*
     C                   EXSR      SR#PI
     C                   EXSR      SR#BA
     C                   EXSR      SR#PA
     C                   EXSR      SR#RER
     C                   EXSR      SR#DDR
     C                   IF        XXRLS = '3'
     C                   EXSR      SRPTP
RKK  C                   EXSR      SR#E3
RKK  C                   EXSR      SR#R3
     C                   IF        XXRLSA = *ON
     C                   EXSR      SR#HD
     C                   EXSR      SR#CN
     C                   ENDIF
     C                   EXSR      SRR22
     C* blank out the data elements that were being used in release 1 and are no
     C                   EVAL      SUFEIN = *BLANKS
     C                   EVAL      SURETW = *BLANKS
     C                   EVAL      SUWAGE = *ZEROS
     C                   EVAL      SUCSAL = *BLANKS
     C                   EVAL      SU#PA = *ZEROS
     C                   EXSR      SR#A3
     C                   SELECT
     C                   WHEN      CMEITQ = 'E'
     C                   MOVEL     DN(152)       SUEID
     C                   WHEN      CMEITQ = 'G'
     C                   MOVEL     DN(153)       SUEID
     C                   WHEN      CMEITQ = 'A'
     C                   MOVEL     DN(154)       SUEID
     C                   WHEN      CMEITQ = 'P'
     C                   MOVEL     DN(156)       SUEID
     C                   OTHER
     C                   MOVEL     DN(42)        SUEID
     C                   ENDSL
     C                   MOVEL     DN(270)       SUEITQ
      *
     C                   MOVEL     DN(202)       SURBAC
     C                   ENDIF
     C                   IF        XXRLSA = *ON
     C                   EXSR      SRR22A
     C                   ENDIF
     C*
     C                   MOVE      'A'           SUSTAT
     C                   MOVEL     Q1USER        SUUSER
     C                   MOVEL     Q1PGM         SUPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        SUTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SUDT
     c                   eval      SUfnd  = LGfnd
     c                   eval      Suyear = LGfyr
     c                   eval      SUcase = LGcase
     C*
     C                   WRITE     PSU01
     C*
-ESR CSR   XXA49         ENDSR
     C**************************************************************************
     C*  THIS SUBROUTINE RETRIEVES THE PERMANENT IMPAIRMENT
     C*  RECORDS AND WRITES UP TO 6 SEGMENTS.
     C*
     C     SR#PI         BEGSR
     C*
     C                   MOVE      *ZEROS        SU#PI
     C***
     C***GET THE 1ST BODY PART FROM CLAIM MASTER.
     C***
     C*  If % impairment whole body > 0 load whole body, else load from grid
     C*    Load from grid
     C     CMIPCT        IFLE      0
     C                   MOVE      *OFF          I_IMPAIR
     C*
     C*  GET THE REST OF THE PERMANENT IMPAIRMENT BODY PARTS FROM THE
     C*  CORRESPONDING FILE.
     C*
     C     BYKY          CHAIN     PBY01                              13
     C     *IN13         DOWEQ     *OFF
     C     BYSTAT        IFEQ      'A'
     C*
     C                   ADD       1             SU#PI
     C                   MOVEL     BYKEY         PIIBPC
     C                   MOVE      BYIMLC        PIIMLC
     C                   MOVEL     BYPCT         PIIMPP
     C                   Z-ADD     XXCO#         PICO#
     C                   Z-ADD     XXEXDT        PIEXDT
     C                   MOVEL     DN(1)         PITSET
     C                   MOVEL     DN(2)         PIMTC
     C                   MOVEL     DN(3)         XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         PITDT
     C                   MOVEL     DN(4)         PIJRST
     C                   MOVEL     DN(15)        PIACLC
     C                   MOVE      'A'           PISTAT
     C                   MOVEL     Q1USER        PIUSER
     C                   MOVEL     Q1PGM         PIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        PITIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PIDT
     C                   WRITE     PPI01
     C*
     C                   MOVE      *ON           I_IMPAIR
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     PIIBPC        XXKEY
     C                   MOVEL(P)  'PART'        XXTBID
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL     GTDESC        XXBPDS
     C*
     C*  PRINT THE A49 PERMANENT IMPAIRMENT DETAIL RECORDS.
     C*
     C     *INOB         IFEQ      '1'
     C                   EXCEPT    HEAD1
     C                   EXCEPT    SUBHPI
     C                   ELSE
     C     SU#PI         IFEQ      1
     C                   EXCEPT    SUBHPI
     C                   END
     C                   END
     C                   EXCEPT    SUBDPI
     C*
     C                   IF        XXRLSA = *ON
     C                   MOVEL     BYIMLC        PIIMLC
     C     SU#PI         IFGE      10
     C                   LEAVE
     C                   ENDIF
     C                   ELSE
     C     SU#PI         IFGE      6
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*
     C                   END
     C     BYKY          READE     PBY01                                  13
     C                   ENDDO
     C*
     C*  Load whole body
     C                   ELSE
     C                   ADD       1             SU#PI
     C                   MOVEL     '99'          XXIBPC            2
     C                   MOVEL     XXIBPC        PIIBPC
     C                   MOVE      ' '           PIIBPC
     C                   MOVEL     CMIPCT        PIIMPP
     C                   IF        XXRLSA = *ON
     C                   ENDIF
     C                   Z-ADD     XXCO#         PICO#
     C                   Z-ADD     XXEXDT        PIEXDT
     C                   MOVEL     DN(1)         PITSET
     C                   MOVEL     DN(2)         PIMTC
     C                   MOVEL     DN(3)         XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         PITDT
     C                   MOVEL     DN(4)         PIJRST
     C                   MOVEL     DN(15)        PIACLC
     C                   MOVE      'A'           PISTAT
     C                   MOVEL     Q1USER        PIUSER
     C                   MOVEL     Q1PGM         PIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        PITIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PIDT
     C                   WRITE     PPI01
     C*
     C*  PRINT THE A49 PERMANENT IMPAIRMENT DETAIL RECORDS.
     C*
     C                   IF        *INOB
     C                   EXCEPT    HEAD1
     C                   ENDIF
      *
     C                   IF        SU#PI = 1
     C                   EXCEPT    SUBHPI
     C                   ENDIF

     C                   EXCEPT    SUBDPI
     C                   ENDIF
     C                   ENDSR
     C**************************************************************************
     C*  THIS SUBROUTINE RETRIEVES THE PAYMENT/ADJUSTMENT
     C*  RECORDS AND WRITES UP TO 10 SEGMENTS.
     C*
     C     SR#PA         BEGSR
     C*
     C                   CLEAR                   PCD
     C                   CLEAR                   AMT
     C                   CLEAR                   SDT
     C                   CLEAR                   EDT
     C                   CLEAR                   WKS
     C                   CLEAR                   DYS
     C                   CLEAR                   WAM
     C                   CLEAR                   GWA
     C                   CLEAR                   GWE
     C                   CLEAR                   NWE
     C                   CLEAR                   PYD
     C                   Z-ADD     *ZEROS        SU#BFT
RKK  C                   Z-ADD     *ZEROS        SU#OBF
RKK  C                   Z-ADD     *ZEROS        SU#REC
     C                   CLEAR                   LNKM
     C*
     C***       IF THERE IS A PAYMENT/ADJUSTMENT CORRECTION RECORD FOR
     C***       THIS LOG RECORD, LOAD THE ARRAYS FROM THAT RECORD.
     C*
     C     LGDSQM        IFGT      *ZEROS
     C     AJKY          CHAIN     XAJ01                              13
     C                   ELSE
     C                   MOVE      *ON           *IN13
     C                   ENDIF
     C*
     C     *IN13         IFEQ      *OFF
     C                   MOVE      DJCD          PCD
     C                   MOVE      DJPD          AMT
     C                   MOVE      DJWA          WAM
     C                   MOVE      DJST          SDT
     C                   MOVE      DJEN          EDT
     C                   MOVE      DJWK          WKS
     C                   MOVE      DJDY          DYS
     C                   IF        XXRLS = '3'
     C                   MOVE      DJGA          GWA
     C                   MOVE      DJGE          GWE
     C                   MOVE      DJWD          NWE
     C                   MOVE      DJPI          PYD
     C                   MOVE      DJLM          LNKM
     C                   DO        10            X
     C*
     C     GWE(X)        IFNE      *ZEROS
     C                   Z-ADD     GWE(X)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         GWE(X)
     C                   ENDIF
     C*
     C     PYD(X)        IFNE      *ZEROS
     C                   Z-ADD     PYD(X)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         PYD(X)
     C                   ENDIF
     C*
     C                   ENDDO
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C*
     C*  FIRST, PROCESS ALL PAYMENTS
     C                   MOVE      'Y'           XXIPMT            1
     C                   MOVE      'N'           XXPROC
     C     PAKY          CHAIN     PPA02                              13
     C     *IN13         DOWEQ     *OFF
     C     PARETP        IFEQ      'C'
     C     LGMTC         ORNE      'IP'
     C     XXPROC        IFEQ      'Y'
     C     LGMTC         ANDEQ     'IP'
     C                   LEAVE
     C                   END
     C     LGMTC         IFEQ      'IP'
     C     PATYPE        IFNE      'P'
     C     PATYPE        ANDNE     'M'
     C     PATYPE        ANDNE     'A'
     C     PAADST        OREQ      'S'
     C     PAADST        OREQ      'V'
     C     PAADST        OREQ      'Z'
     C                   GOTO      NEXT
     C                   ENDIF
     C                   ENDIF
     C*  SUMMARIZE THE PMTS BY PMT CODE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     PABR2         XXKEY
     C     TBKY          CHAIN     PB201                              60
      *
      * Capture recoveries associated with a payment code
      *
     C     *IN60         IFEQ      *OFF
     C     PATYPE        ANDEQ     'R'
     C     B2ERCV        ANDNE     *BLANKS
     C*
     C                   MOVEL(P)  'PADJ'        XXTBID
     C                   MOVEL(P)  B2ERCV        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      '0'
     C     GTFG01        ANDEQ     '2'
     C*** Check for the value in first 2 chars of 5 fields GTCF01 - GTCF05
     C
     C                   if        %subst(GTCF01:1:2) <> XXJST
     C                             and %subst(GTCF02:1:2) <> XXJST
     C                             and %subst(GTCF03:1:2) <> XXJST
     C                             and %subst(GTCF04:1:2) <> XXJST
     C                             and %subst(GTCF05:1:2) <> XXJST
     C                             and %subst(GTCF01:1:2) <> XXJST
     C
     C                   Z-ADD     1             X
     C     B2ERCV        LOOKUP    PCD(X)                                 61
     C  N61              Z-ADD     1             X
     C  N61*BLANKS       LOOKUP    PCD(X)                                 61
     C*
     C     *IN61         IFEQ      '1'
     C                   MOVEL     B2ERCV        PCD(X)
     C                   ADD       PAAMT         AMT(X)
     C                   ENDIF
     C                   endif
     C                   ENDIF
     C                   ENDIF
      *
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     PABR2         XXKEY
     C     TBKY          CHAIN     PB201                              60
     C     *IN60         IFEQ      '0'
     C     B2PYCD        ANDNE     *BLANKS
     C*
     C                   MOVEL(P)  'PADJ'        XXTBID
     C                   MOVEL(P)  B2PYCD        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      '0'
     C     GTFG01        ANDEQ     ' '
     C*** Check for the value in first 2 chars of 5 fields GTCF01 - GTCF05
     C
     C                   if        %subst(GTCF01:1:2) <> XXJST
     C                             and %subst(GTCF02:1:2) <> XXJST
     C                             and %subst(GTCF03:1:2) <> XXJST
     C                             and %subst(GTCF04:1:2) <> XXJST
     C                             and %subst(GTCF05:1:2) <> XXJST
     C                             and %subst(GTCF01:1:2) <> XXJST
     C
     C*
     C                   Z-ADD     1             X
     C     B2PYCD        LOOKUP    PCD(X)                                 61
     C  N61              Z-ADD     1             X
     C  N61*BLANKS       LOOKUP    PCD(X)                                 61
     C*
     C     *IN61         IFEQ      '1'
     C     XXIPMT        IFEQ      'Y'
     C                   MOVE      'N'           XXIPMT
     C                   MOVE      PASEQ#        SVSEQ#
     C                   ENDIF
     C
     C                   MOVEL     B2PYCD        PCD(X)
     C                   IF        PATYPE = 'R' or PATYPE = 'S'or
     C                             PATYPE = 'V'
     C                   SUB       PAAMT         AMT(X)
     C                   ELSE
     C                   ADD       PAAMT         AMT(X)
     C                   ENDIF
     C*
     C**Do not consider start and end dates
     C**adjustments, stops & voids
     C*
     C                   IF        PATYPE = 'S' or PATYPE = 'V' or
     C                             PATYPE = 'R' or PAADST > *BLANK
      * omit date calc
      *
      * Check for Recoveries that reduce weeks e.g. Overpayment Recoveries
      *
     C                   MOVEL     B2ERCV        XXERCV            1
     C                   IF        PATYPE = 'R' and XXERCV = '8'
     C                   SUB       PAWKS         WKS(X)
     C                   SUB       PADAYS        DYS(X)
     C     DYS(X)        IFLT      0
     C                   SUB       1             WKS(X)
     C                   ADD       7             DYS(X)
     C                   END
     C                   END
     C
     C                   ELSE
     C     SDT(X)        IFEQ      0
     C     PAFRDT        ORLT      SDT(X)
     C                   MOVE      PAFRDT        SDT(X)
     C                   MOVE      PAFRDT        NWE(X)
     C                   END
     C*
     C                   IF        B2PYCD = '050'   and
     C                             CMACDT > *ZEROS  and CMACDT < PATODT
     C                   MOVE      CMACDT        NWE(X)
     C                   ENDIF
     C*
     C     PATODT        IFGT      EDT(X)
     C                   MOVE      PATODT        EDT(X)
     C                   END
     C*
     C     PAAMT         IFGE      0
     C*
     C                   ADD       PAWKS         WKS(X)
     C                   ADD       PADAYS        DYS(X)
     C     DYS(X)        IFGE      7
     C                   ADD       1             WKS(X)
     C                   SUB       7             DYS(X)
     C                   END
     C*
     C                   ELSE
     C*
     C                   SUB       PAWKS         WKS(X)
     C                   SUB       PADAYS        DYS(X)
     C     DYS(X)        IFLT      0
     C                   SUB       1             WKS(X)
     C                   ADD       7             DYS(X)
     C                   END
     C*
     C                   END
     C*
     C                   ENDIF
     C*
     C     DYS(X)        DIV       7             XXWK52            5 2
     C                   ADD       WKS(X)        XXWK52
     C     XXWK52        IFNE      0
     C     AMT(X)        DIV       XXWK52        WAM(X)
     C                   ENDIF
     C                   EVAL      GWA(X) = *ZEROS
     C                   EVAL      GWE(X) = *ZEROS
     C                   EVAL      PYD(X) = *ZEROS
     C*** Load WDA3P fields to arrays
     C                   IF        XXRLS = '3'
     C                   EVAL      XXBTC = PCD(X)
     C     CLTID         IFEQ      '139'
     C                   exsr      Srpast
     C     CPKY1         CHAIN     PCP01                              60
     C                   ELSE
     C     CPKY          CHAIN     PCP01                              60
     C                   ENDIF
     C                   IF        *IN60 = *OFF and CPCMDT > *ZERO
     C                   MOVE      CPCMDT        PYD(X)
     C                   ADD       19000000      PYD(X)
     C                   ELSE
     C                   MOVE      PATRDT        CPCMDT
     C                   MOVE      CPCMDT        PYD(X)
     C                   ADD       19000000      PYD(X)
     C                   ENDIF
     C*
     C** FOR IP'S SAVE THE ISSUE DATE FOR THE PAYMENT SEGMENT.
     C*
     C     LGMTC         IFEQ      'IP'
     C                   EVAL      SVISDT = CPCMDT
     C                   ENDIF
     C                   ENDIF
     C*
     C     LGMTC         IFEQ      'IP'
     C                   EXSR      SRPTPI
     C                   ENDIF
     C*
     C*
     C                   MOVE      'Y'           XXPROC
     C*
     C                   END
     C                   endif
     C                   ENDIF
     C                   END
     C                   END
     C*
     C     NEXT          TAG
     C*
     C     PAKY          READE     PPA02                                  13
     C                   ENDDO
     C*
     C*
     C***  IXRLS         IFEQ      '3'
     C     XXRLS         IFEQ      '3'
     C*
     C                   Z-ADD     *ZEROS        X
     C                   DO        10            X
     C*
     C     PCD(X)        IFEQ      *BLANKS
     C                   LEAVE
     C                   ENDIF
     C*
     C                   Z-ADD     *ZEROS        XBNAMT
     C                   Z-ADD     NWE(X)        XBNSDT
     C*
     C     GWE(X)        IFEQ      *ZEROS
     C     NWE(X)        ORLT      GWE(X)
     C                   Z-ADD     NWE(X)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         GWE(X)
     C                   ENDIF
      *
     C                   MOVE      *OFF          I_REDIST
     C*
     C     BNKY          CHAIN     PBN01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     BNSTAT        IFEQ      'A'
     C     PCD(X)        ANDEQ     XXACOD
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCURD
     C* WITHIN RANGE
     C*
     C     BNSTDT        IFLE      XXCURD
     C     BNENDT        ANDGE     XXCURD
     C     BNSTDT        ORLE      XXCURD
     C     BNENDT        ANDEQ     *ZEROS
     C*
     C                   IF        %SUBST(BNACOD:1:1) = 'H' or
     C                             %SUBST(BNACOD:1:1) = 'K'
     C                   MOVE      *ON           I_REDIST
      *
     C                   ELSE
     C                   ADD       BNAMT         XBNAMT
     C                   ENDIF
     C*
     C     BNSTDT        IFGT      XBNSDT
     C                   MOVE      BNSTDT        XBNSDT
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C                   CLEAR                   PRD01
     C     BNKY          READE     PBN01                                  60
     C                   ENDDO
     C*
     C     CMCRT         IFNE      *ZEROS
     C*** Beg Chg *** 09/08/23 ********************************************
     C****************** Z-ADD     CMCRT         GWA(X)
     C*** Beg Del *** 10/10/24 ********************************************
     C****************** Z-ADD     CMAWW         GWA(X)
     C*** Eng Del *** 10/10/24 ********************************************
     C*** End Chg *** 09/08/23 ********************************************
     C*** Beg Add *** 10/10/24 ********************************************
     C                   SELECT
     C                   WHEN      CLTID = '114'
     C                   Z-ADD     CMAWW         GWA(X)
     C                   OTHER
     C                   Z-ADD     CMCRT         GWA(X)
     C                   ENDSL
     C*** Eng Add *** 10/10/24 ********************************************
     C                   Z-ADD     CMCRT         WAM(X)
     C                   ENDIF
      /Free
       If LGmtc = 'IP';
     C*** Beg Add *** 03/24/25 ********************************************
          XXPrimy = ' ';
       // Check for record flagged as Primary.  If found bypass PAW05.
          If CLTID = '146';
          Chain (LGco#:LGfnd:LGfyr:LGcase) PAW06;
             If not %eof(WMAWL6);
                If L6crt <> *Zeros;
                   GWA(X) = L6crt;
                   WAM(X) = L6crt;
                   XXPrimy = 'Y';
                Endif;
             Endif;
          Endif;
          If XXprimy <> 'Y';
     C*** End Add *** 03/24/25 ********************************************
          Setgt (LGco#:LGfnd:LGfyr:LGcase:LGmtc:XXexdt) PAW05;
          Readpe (LGco#:LGfnd:LGfyr:LGcase:LGmtc) PAW05;
           If not %eof(WMAWL5);
                 If AWcrt <> *Zeros;
     c*** Beg Chg *** 09/08/23 ********************************************
     c************* GWA(X) = AWcrt;
     c*** Beg Del *** 10/10/24 ********************************************
     c************* GWA(X) = AWaww;
     c*** End Del *** 10/10/24 ********************************************
     C*** End Chg *** 09/08/23 ********************************************
     c*** Beg Add *** 10/10/24 ********************************************
                         Select;
                         When CltId = '114';
                            gwa(X) = AWaww;
                         Other;
                            gwa(X) = AWcrt;
                         Endsl;
     C*** Eng Add *** 10/10/24 ********************************************
                    WAM(X) = AWcrt;
                 Endif;
           Endif;
     C*** Beg Add *** 03/24/25 ********************************************
        Endif;
     C*** End Add *** 03/24/25 ********************************************
       Endif;
      /End-Free
     C*
     C     XBNAMT        IFNE      *ZEROS
     C*
     C     WAM(X)        SUB       XBNAMT        WAM(X)
     C*
     C                   ENDIF
      ***
     C                   IF        XBNAMT <> *ZEROS or I_REDIST
     C     XBNSDT        IFNE      *ZEROS
     C                   MOVE      XBNSDT        NWE(X)
     C                   ENDIF
     C                   ENDIF
     C                   ENDDO
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  Load full salary amount, weeks, and days paid by employer
     C*    using code 240 (per TX).
     C*
     C     XXRLS         IFEQ      '1'
     C*
     C*
     C                   Z-ADD     1             X
     C     '240'         LOOKUP    PCD(X)                                 61
     C*
     C     *IN61         IFEQ      '0'
     C                   Z-ADD     1             X
     C     *BLANKS       LOOKUP    PCD(X)                                 61
     C*
     C     *IN61         IFEQ      '1'
     C                   MOVEL     '240'         PCD(X)
     C                   Z-ADD     CMFSAM        AMT(X)
     C                   Z-ADD     CMDBDT        SDT(X)
     C                   Z-ADD     CMFSDT        EDT(X)
     C                   Z-ADD     CMFSWK        WKS(X)
     C                   Z-ADD     CMFSDY        DYS(X)
     C                   EVAL      GWA(X) = *ZEROS
     C                   EVAL      GWE(X) = *ZEROS
     C                   EVAL      PYD(X) = *ZEROS
     C*
     C     DYS(X)        DIV       7             XXWK52
     C                   ADD       WKS(X)        XXWK52
     C     XXWK52        IFNE      0
     C     AMT(X)        DIV       XXWK52        WAM(X)
     C                   ENDIF
     C*
     C                   MOVE      'Y'           XXPROC
     C                   END
     C*
     C                   END
     C*
     C                   ENDIF
     C*
     C     XXRLS         IFEQ      '3'
     C     A3KY          SETLL     WDA3L
      *
     C                   IF        %EQUAL(WDA3L)
     C     A3KY          READE     WDA3L
      *
     C                   DOW       NOT %EOF(WDA3L)
     C     A3BTC         LOOKUP    PCD                                    61
     C*
     C     *IN61         IFEQ      '0'
     C                   Z-ADD     1             X
     C     *BLANKS       LOOKUP    PCD(X)                                 61
     C*
     C     *IN61         IFEQ      '1'
     C                   MOVEL     A3BTC         PCD(X)
     C                   Z-ADD     A3PAID        AMT(X)
     C                   Z-ADD     A3STDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         XDT7              7
     C                   MOVE      XDT7          SDT(X)
     C                   Z-ADD     A3ENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         XDT7              7
     C                   MOVE      XDT7          EDT(X)
     C                   ENDIF
     C                   ENDIF
      *
     C     A3KY          READE     WDA3L
     C                   ENDDO

     C                   ENDIF
     C                   ENDIF
     C*  LOAD THE ARRAYS INTO THE PMT/ADJ SEGMENT FILE
     C*
     C                   MOVE      LGCO#         AJCO#
     C                   Z-ADD     XXEXDT        AJEXDT
     C                   MOVEL     DN(1)         AJTSET
     C                   MOVEL     DN(2)         AJMTC
     C                   MOVEL     DN(3)         XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         AJTDT
     C                   MOVEL     DN(4)         AJJRST
     C                   MOVEL     DN(15)        AJACLC
     C                   MOVE      'A'           AJSTAT
     C                   MOVEL     Q1USER        AJUSER
     C                   MOVEL     Q1PGM         AJPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        AJTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AJDT
     C*
     C                   MOVE      *ZEROS        SU#PA
     C                   Z-ADD     1             X
     C     PCD(X)        DOWNE     *BLANKS
     C*** Beg Add *** 03/02/22 ********************************************
     C     AMT(X)        IFNE      0
     C     AJMTC         OREQ      'EP'
     C*** Beg Add *** 10/20/22 ********************************************
     C     cltId         oreq      '152'
     C*** End Add *** 10/20/22 ********************************************
     C*** End Add *** 03/02/22 ********************************************
     C*
     C                   MOVE      PCD(X)        AJPYAJ
     C                   Z-ADD     AMT(X)        AJPADT
     C*
     C                   Z-ADD     WAM(X)        AJAWAM
     C*
     C                   Z-ADD     SDT(X)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         AJPSDT
     C*
     C                   Z-ADD     EDT(X)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         AJPEDT
     C*
     C                   Z-ADD     WKS(X)        AJAWPD
     C                   Z-ADD     DYS(X)        AJADPD
     C                   MOVE      LNKM(X)       AJLNKM
     C                   Z-ADD     *ZEROS        AJGWAM
     C                   Z-ADD     *ZEROS        AJGWEF
     C                   Z-ADD     *ZEROS        AJNWEF
     C                   Z-ADD     *ZEROS        AJPYDT
     C*
     C***                IF        IXRLS='3'
     C                   IF        XXRLS='3'
     C                   Z-ADD     GWA(X)        AJGWAM
     C*
     C                   Z-ADD     GWE(X)        AJGWEF
     C*
     C                   Z-ADD     NWE(X)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         AJNWEF
     C*
     C                   Z-ADD     PYD(X)        AJPYDT
     C                   ENDIF                                                 I
     C*
     C                   if        ajpyaj >= '800' and ajpyaj <= '899'
     C                   add       1             su#rec
     C                   else
     C                   ADD       1             SU#PA
     C                   Z-ADD     SU#PA         SU#BFT
     C                   endif
     C*
     C                   WRITE     PAJ01
     C*
     C*  PRINT THE A49 PAYMENT/ADJUSTMENT DETAIL RECORDS.
     C*
     C     *INOB         IFEQ      '1'
     C                   EXCEPT    HEAD1
     C                   EXCEPT    SUBHPA
     C                   ELSE
     C     X             IFEQ      1
     C                   EXCEPT    SUBHPA
     C                   END
     C                   END
     C                   EXCEPT    SUBDPA
     C*
     C     X             IFGE      10
     C                   LEAVE
     C                   ENDIF
     C*
     C*** Beg Add *** 03/02/22 ********************************************
     C                   ENDIF
     C*** End Add *** 03/02/22 ********************************************
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   ENDSR
     C**************************************************************************
     C*  THIS SUBROUTINE RETRIEVES THE BENEFIT ADJUSTMENT
     C*  RECORDS AND WRITES UP TO 10 SEGMENTS.
     C*
     C     SR#BA         BEGSR
     C*
     C                   MOVE      *ZEROS        SU#BA
RKK  C                   Z-ADD     0             SU#ACR
     C*
     C     BNKY          CHAIN     PBN01                              13
     C     *IN13         DOWEQ     *OFF
     C     BNSTAT        IFEQ      'A'
     C*
     C                   ADD       1             SU#BA
     C*
     C                   MOVE      LGCO#         RDCO#
     C                   MOVE      XXEXDT        RDEXDT
     C                   MOVEL     DN(1)         RDTSET
     C                   MOVEL     DN(2)         RDMTC
     C                   MOVE      DN(3)         RDTDT
     C                   MOVEL     DN(4)         RDJRST
     C                   MOVEL     DN(15)        RDACLC
     C                   MOVEL     BNACOD        RDBACD
     C                   Z-ADD     BNAMT         RDWAAM
     C                   MOVE      BNSTDT        RDASDT
     C                   ADD       19000000      RDASDT
     C                   MOVE      'A'           RDSTAT
     C                   MOVEL     Q1USER        RDUSER
     C                   MOVEL     Q1PGM         RDPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        RDTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RDDT
     C                   IF        XXRLS = '3'
     C* Load BNENDT to RDENDT
     C                   IF        BNENDT > *ZERO
     C                   MOVE      BNENDT        RDENDT
     C                   ADD       19000000      RDENDT
     C                   ELSE
     C                   MOVE      *ZEROS        RDENDT
     C                   ENDIF
RKK  C                   ADD       1             SU#ACR
     C                   ENDIF
     C                   IF        XXRLSA = *ON
     C                   Z-ADD     BNWIAO        RDWIAO
     C                   ENDIF
     C*
     C                   WRITE     PRD01
     C*
     C                   MOVEL     'BADJ'        XXTBID
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     BNACOD        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL     GTDESC        XXBADS
     C*
     C*  PRINT THE A49 PAYMENT/ADJUSTMENT DETAIL RECORDS.
     C*
     C     *INOB         IFEQ      '1'
     C                   EXCEPT    HEAD1
     C                   EXCEPT    SUBHBA
     C                   ELSE
     C     SU#BA         IFEQ      1
     C                   EXCEPT    SUBHBA
     C                   END
     C                   END
     C                   EXCEPT    SUBDBA
     C*
     C     SU#BA         IFGE      10
     C                   LEAVE
     C                   ENDIF
     C*
     C                   END
     C                   CLEAR                   PRD01
     C     BNKY          READE     PBN01                                  13
     C                   ENDDO
     C*
     C                   ENDSR
     C**************************************************************************
     C*  THIS SUBROUTINE RETRIEVES THE PAID TO DATE/REDUCED EARNINGS/RECOVERIES
     C*  RECORDS AND WRITES UP TO 25 SEGMENTS.
     C*
     C     SR#RER        BEGSR
     C*
     C                   MOVE      *ZEROS        SU#RER
     C                   CLEAR                   PDCD
     C                   CLEAR                   PDAM
     C*
     C*
     C*  LOAD THE ARRAYS INTO THE PTD SEGMENT FILE
     C*
     C                   MOVE      LGCO#         T2CO#
     C                   MOVE      XXEXDT        T2EXDT
     C                   MOVEL     DN(1)         T2TSET
     C                   MOVEL     DN(2)         T2MTC
     C                   MOVE      DN(3)         T2TDT
     C                   MOVEL     DN(4)         T2JRST
     C                   MOVEL     DN(15)        T2ACLC
     C                   MOVE      'A'           T2STAT
     C                   MOVEL     Q1USER        T2USER
     C                   MOVEL     Q1PGM         T2PGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        T2TIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         T2DT
     C*
     C                   Z-ADD     1             X
     C     REKY          CHAIN     PRE01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C     RESTAT        IFEQ      'A'
     C*
     C                   MOVE      RECODE        T2CODE
     C                   Z-ADD     REAAMT        T2AMT
RKK  C                   MOVE      RECODE        XXPYN             3 0
RKK  C                   IF        SU#OBF < 25  AND
RKK  C                             XXPYN >= 300 AND XXPYN <= 499
RKK  C                             AND XXPYN <> 410
RKK  C                   ADD       1             SU#OBF
RKK  C                   ENDIF
     C                   WRITE     PT201
     C*
     C                   MOVEL(P)  'PTD '        XXTBID
     C                   MOVEL(P)  T2CODE        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL     GTDESC        XXPDDS
     C*
     C*  PRINT THE A49 PTD/REDUCED EARNINGS DETAIL RECORDS.
     C*
     C     *INOB         IFEQ      '1'
     C                   EXCEPT    HEAD1
     C                   EXCEPT    SUBHPD
     C                   MOVE      '0'           *INOB
     C                   ELSE
     C     X             IFEQ      1
     C                   EXCEPT    SUBHPD
     C                   END
     C                   END
     C                   EXCEPT    SUBDPD
     C                   ADD       1             SU#RER
     C*
     C     X             IFGE      25
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ADD       1             X
     C*
     C                   ENDIF
     C*
     C     REKY          READE     PRE01                                  99
     C*
     C                   ENDDO
     C*
     C*  If no WEREP records were processed, load this data from the
     C*  Pmt Detail File.
     C*
     C     X             IFEQ      1
     C*
     C*  Process and summarize the pmt transactions.
     C*
     C     PAKY          CHAIN     PPA02                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     PATYPE        IFEQ      'R'
     C     PATYPE        OREQ      'S'
     C     PATYPE        OREQ      'V'
     C                   MULT      -1            PAAMT
     C                   END
     C*
     C*  Summarize the pmts by pmt code
     C*
     C                   MOVEL(P)  PABR2         XXKEY
     C     TBKY          CHAIN     PB201                              60
     C     *IN60         IFEQ      '0'
     C     B2PYCD        ANDNE     *BLANKS
     C*
     C                   SELECT
     C                   WHEN      (SUJRST = 'NE' and B2PYCD = '422')
      * OMIT
     C                   WHEN      (SUJRST = 'CA' and B2PYCD = '422')
      * OMIT
     C                   WHEN      (SUJRST = 'GA' and B2PYCD = '330')
      * OMIT
      * OMIT
     C                   WHEN      (SUJRST = 'VA') and
     C                             ((B2PYCD >= '300' and B2PYCD < '340') or
     C                              (B2PYCD >= '380' and B2PYCD < '430') or
     C                              (B2PYCD > '465'))
     C                   OTHER
     C                   MOVEL(P)  'PADJ'        XXTBID
     C                   MOVEL(P)  B2PYCD        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      '0'
     C     GTFG01        ANDEQ     '2'
     C*** Check for the value in first 2 chars of 5 fields GTCF01 - GTCF05
     C
     C                   if        %subst(GTCF01:1:2) <> XXJST
     C                             and %subst(GTCF02:1:2) <> XXJST
     C                             and %subst(GTCF03:1:2) <> XXJST
     C                             and %subst(GTCF04:1:2) <> XXJST
     C                             and %subst(GTCF05:1:2) <> XXJST
     C                             and %subst(GTCF01:1:2) <> XXJST
     C
     C*
     C                   Z-ADD     1             X
     C     B2PYCD        LOOKUP    PDCD(X)                                61
     C  N61              Z-ADD     1             X
     C  N61*BLANKS       LOOKUP    PDCD(X)                                61
     C*
     C     *IN61         IFEQ      '1'
     C                   MOVEL     B2PYCD        PDCD(X)
     C                   ADD       PAAMT         PDAM(X)
     C                   END
     C*
     C                   ENDIF
     C*
     C                   END
     C                   ENDSL
     C                   END
     C*
     C     PAKY          READE     PPA02                                  60
     C                   ENDDO
     C*
     C*  Load the summarized records into the extract file.
     C*
     C                   Z-ADD     1             X
     C     PDCD(X)       DOWNE     *BLANKS
     C*
     C                   MOVE      PDCD(X)       T2CODE
     C     PDAM(X)       IFLT      *ZERO
     C     PDAM(X)       MULT      -1            T2AMT
     C                   ELSE
     C                   Z-ADD     PDAM(X)       T2AMT
     C                   ENDIF
     C                   MOVE      PDCD(X)       XXPYN
     C                   IF        SU#OBF < 25  AND
     C                             XXPYN >= 300 AND XXPYN <= 499
     C                             AND XXPYN <> 410
     C                   ADD       1             SU#OBF
     C                   ENDIF
     C                   WRITE     PT201
     C*
     C                   MOVEL(P)  'PTD '        XXTBID
     C                   MOVEL(P)  T2CODE        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL     GTDESC        XXPDDS
     C*
     C*  PRINT THE A49 PTD/REDUCED EARNINGS DETAIL RECORDS.
     C*
     C     *INOB         IFEQ      '1'
     C                   EXCEPT    HEAD1
     C                   EXCEPT    SUBHPD
     C                   MOVE      '0'           *INOB
     C                   ELSE
     C     X             IFEQ      1
     C                   EXCEPT    SUBHPD
     C                   END
     C                   END
     C                   EXCEPT    SUBDPD
     C                   ADD       1             SU#RER
     C*
     C     X             IFGE      10
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ADD       1             X
     C*
     C                   ENDDO
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C**************************************************************************
     C*  THIS SUBROUTINE RETRIEVES THE DEATH DEPENDENT/PAYEE RELATIONSHIP
     C*  RECORDS AND WRITES UP TO 12 SEGMENTS.
     C*
     C     SR#DDR        BEGSR
     C*
     C                   MOVE      *ZEROS        SU#DDR
     C     DN(57)        IFNE      *ZEROS
     C*
     C*  Load the key fields.
     C*
     C                   MOVE      LGCO#         T3CO#
     C                   MOVE      XXEXDT        T3EXDT
     C                   MOVEL     DN(1)         T3TSET
     C                   MOVEL     DN(2)         T3MTC
     C                   MOVE      DN(3)         T3TDT
     C                   MOVEL     DN(4)         T3JRST
     C                   MOVEL     DN(15)        T3ACLC
     C                   MOVE      'A'           T3STAT
     C                   MOVEL     Q1USER        T3USER
     C                   MOVEL     Q1PGM         T3PGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        T3TIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         T3DT
     C*
     C                   Z-ADD     1             X
     C     PAKY          CHAIN     PAC01                              60
     C     *IN60         DOWEQ     *OFF
     C     ACREL         IFNE      *BLANK
     C     ACREL         ANDNE     '9'
     C     ACSTAT        ANDNE     'I'
     C*
     C                   MOVEL     ACREL         T3REL
     C     ACORD         IFEQ      *BLANK
     C                   MOVE      *ZEROS        ACORD
     C                   MOVEL     ACORD         T3BODR
     C                   ELSE
     C                   MOVEL     ACORD         T3BODR
     C                   ENDIF
     C                   IF        XXRLSA = *ON
     C                   MOVEL(P)  ACFNAM        T3DFSN
     C                   MOVEL(P)  ACPNAM        T3DLSN
     C*
     C                   Z-ADD     ACDDOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         T3DDOB
     C*
     C                   MOVE      ACDSEX        T3DSEX
     C                   MOVE      ACDPCY        T3EXTD
     C                   ENDIF
     C                   WRITE     PT301
     C*
     C                   MOVEL(P)  'DPRL'        XXTBID
     C                   MOVEL(P)  T3REL         XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL     GTDESC        XXDPRL
     C*
     C*  Print the A49 Death Dependent detail record.
     C*
     C     *INOB         IFEQ      '1'
     C                   EXCEPT    HEAD1
     C                   EXCEPT    SUBHDP
     C                   MOVE      '0'           *INOB
     C                   ELSE
     C     X             IFEQ      1
     C                   EXCEPT    SUBHDP
     C                   END
     C                   END
     C                   EXCEPT    SUBDDP
     C                   ADD       1             SU#DDR
     C*
otxx C     XXRLSA        IFEQ      *ON
     C     X             ANDGE     20
     C                   LEAVE
otxx C                   ELSE
     C     X             IFGE      12
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ADD       1             X
     C                   ENDIF
     C     PAKY          READE     PAC01                                  60
     C                   ENDDO
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C* UPDATE THE LOG RECORD STATUS TO 'EI'
     C*
     C     SRUPDE        BEGSR
     C                   MOVE      'EI'          LGSTS
     C                   MOVEL     Q1USER        LGUSER
     C                   MOVEL     Q1PGM         LGPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        LGTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LGDT
     C                   IF        SVSTS <> 'EI'
     C                   EXSR      DARTS
     C                   ENDIF
     C                   UPDATE    PLG01
     C*
     C                   ENDSR
     C*
     C*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     C*
     C* UPDATE THE LOG RECORD STATUS TO 'EX'-DOWNLOADED TO EXTRACT FILE
     C*
     C     SRUPDT        BEGSR
     C*
     C                   MOVE      'EX'          LGSTS
     C                   MOVEL     Q1USER        LGUSER
     C                   MOVEL     Q1PGM         LGPGM
     C                   MOVEL     XXEXDT        LGEXDT
     C                   MOVEL     XXEXDT        LGSCHP
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        LGTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LGDT
     C                   UPDATE    PLG01
     C*
     C     XXUPDT        ENDSR
     C**************************************************************************
     C*  THIS SUBROUTINE LOADS ADDITIONAL INFORMATION FOR THE DETAIL
     C*  REPORT AND PRINTS THE DETAIL LINES.
     C*
     C     SRDET         BEGSR
     C*
     C*  LOAD THE SIC DESCRIPTION
     C*
     C                   MOVEL(P)  FRSIC         XXKEY
     C                   MOVE      *BLANKS       XXSIC
     C                   MOVEL(P)  'SIC '        XXTBID
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL     GTDESC        XXSIC
     C*
     C*  LOAD MARITAL STATUS DESCRIPTION
     C*
     C                   MOVEL(P)  FRMSTS        XXKEY
     C                   MOVE      *BLANKS       XXMSTS
     C                   MOVEL     'MSTS'        XXTBID
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL     GTDESC        XXMSTS
     C*
     C*  LOAD THE NATURE DESCRIPTION
     C*
     C                   MOVEL(P)  FRNATR        XXKEY
     C                   MOVE      *BLANKS       XXNATR
     C                   MOVEL(P)  'NATR'        XXTBID
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL     GTDESC        XXNATR
     C*
     C*  LOAD THE PART DESCRIPTION
     C*
     C                   MOVEL(P)  FRPART        XXKEY
     C                   MOVE      *BLANKS       XXPART
     C                   MOVEL(P)  'PART'        XXTBID
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL     GTDESC        XXPART
     C*
     C*  LOAD EMPLOYMENT STATUS DESCRIPTION
     C*
     C                   MOVEL     'EMST'        XXTBID
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     FRESTS        XXKEY
     C                   MOVE      *BLANKS       XXESTS
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     GTDESC        XXESTS
     C                   ENDIF
     C*
     C*  LOAD THE CAUSE DESCRIPTION
     C*
     C                   MOVEL(P)  FRCAUS        XXKEY
     C                   MOVE      *BLANKS       XXCAUS
     C                   MOVEL(P)  'CAUS'        XXTBID
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL     GTDESC        XXCAUS
     C*
     C*  LOAD THE INITIAL TREATMENT DESCRIPTION
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL(P)  FRITRT        XXKEY
     C                   MOVE      *BLANKS       XXITRT
     C                   MOVEL(P)  'ITRT'        XXTBID
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL     GTDESC        XXITRT
     C*
     C*  LOAD GENDER DESCRIPTION
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL(P)  FRSEX         XXKEY
     C                   MOVE      *BLANKS       XXSEX
     C                   MOVEL(P)  'GNDR'        XXTBID
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL     GTDESC        XXSEX
     C*
     C*  LOAD THE ACCIDENT DESCRIPTION
     C*
     C                   MOVEL     FRACDS        XXACD1
     C                   MOVE      FRACDS        XXACD2
     C*
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'FRSS#'       QQNAME           25
     C     FRSS#         PARM      FRSS#         QQVAL           100
     C*  PRINT THE DETAIL LINES
     C*
     C                   EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   MOVE      '1'           *IN63
     C*
     C                   CALL      'SPIMASK'
     C                   PARM      '*UNMASK'     QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'FRSS#'       QQNAME           25
     C     FRSS#         PARM      FRSS#         QQVAL           100
     C                   ENDSR
     C**************************************************************************
     C*  THIS SUBROUTINE LOADS ADDITIONAL INFORMATION FOR THE
     C*  ERROR REJECTION REPORT AND HANDLES THE PRINTING OF THE
     C*  DETAIL LINES.
     C*
     C     SRDET2        BEGSR
     C*
     C*  LOAD THE DETAIL INFORMATION
     C*
     C                   MOVEL     DN(15)        XXCLA#           19
     C                   MOVEL     DN(5)         XXACL#           25
     C                   MOVEL     DN(43)        XXCNAM           30
     C     XXCNAM        CAT       ',':0         XXCNAM
     C     XXCNAM        CAT       DN(44):1      XXCNAM
     C                   MOVEL     DN(31)        XXACDT            8 0
     C                   MOVEL     DN(2)         XXMTC             2
     C                   Z-ADD     XTDUE         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXMTCD
     C*
     C*  GET THE DATA ELEMENT # AND DESCRIPTION
     C*
     C                   Z-ADD     P             XXDN#
     C*
     C*  GET THE DATA ELEMENT DESCRIPTION
     C                   MOVE      *BLANKS       XXWK30
     C     DCKY          CHAIN     PDC01                              60
     C  N60              MOVEL     DCDESC        XXWK30
     C*
     C*  GET THE ERROR DESCRIPTION.
     C*
     C                   MOVE      XXERRS        XXERR#
     C                   MOVE      *BLANKS       XXEDSC
     C     ERKY          CHAIN     PER01                              60
     C  N60              MOVEL     ERDESC        XXEDSC
     C*
     C*   PRINT THE SUB HEADING IF NECESSARY, THEN PRINT THE DETAIL.
     C*
     C     XTADJ         IFNE      X2ADJ
     C                   EXCEPT    HEAD2
     C                   MOVEL     XTADJ         X2ADJ
     C                   MOVE      '0'           *INOC
     C                   END
     C     *INOC         IFEQ      '1'
     C                   EXCEPT    HEAD2
     C                   MOVE      'N'           XXERR
     C                   END
     C                   MOVE      '1'           *IN67
     C     XXERR         IFEQ      'N'
     C                   EXCEPT    SUBH2
     C                   END
     C                   EXCEPT    DET2
     C*
     C                   ENDSR
     C**************************************************************************
     C*  THIS SUBROUTINE LOADS ADDITIONAL INFORMATION FOR THE
     C*  ERROR REJECTION REPORT AND HANDLES THE PRINTING OF THE
     C*  DETAIL LINES.
     C*
     C     SRDET3        BEGSR
     C*
     C*  LOAD THE DETAIL INFORMATION
     C*
     C                   MOVEL     DN(15)        XXCLA#           19
     C                   MOVEL     DN(5)         XXACL#           25
     C                   MOVEL     DN(43)        XXCNAM           30
     C     XXCNAM        CAT       ',':0         XXCNAM
     C     XXCNAM        CAT       DN(44):1      XXCNAM
     C                   MOVEL     DN(31)        XXACDT            8 0
     C                   MOVEL     DN(2)         XXMTC             2
     C                   Z-ADD     XTDUE         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXMTCD            8 0
     C*
     C*  GET THE DATA ELEMENT # AND DESCRIPTION
     C*
     C                   Z-ADD     P             XXDN#
     C*
     C*  GET THE DATA ELEMENT DESCRIPTION
     C                   MOVE      *BLANKS       XXWK30
     C     DCKY          CHAIN     PDC01                              60
     C  N60              MOVEL     DCDESC        XXWK30
     C*
     C*  GET THE ERROR DESCRIPTION.
     C*
     C                   MOVE      XXERRS        XXERR#
     C                   MOVE      *BLANKS       XXEDSC
     C     ERKY          CHAIN     PER01                              60
     C  N60              MOVEL     ERDESC        XXEDSC
     C*
     C*   PRINT THE SUB HEADING IF NECESSARY, THEN PRINT THE DETAIL.
     C*
     C     XTADJ         IFNE      X3ADJ
     C                   EXCEPT    HEAD3
     C                   MOVEL     XTADJ         X3ADJ
     C                   MOVE      '0'           *INOD
     C                   END
     C     *INOD         IFEQ      '1'
     C                   EXCEPT    HEAD3
     C                   MOVE      'N'           XXERR
     C                   END
     C                   MOVE      '1'           *IN69
     C     XXERR         IFEQ      'N'
     C                   EXCEPT    SUBH3
     C                   END
     C                   EXCEPT    DET3
     C*
     C                   ENDSR
     C*
     C**************************************************************************
     C*  GETFND  Get Fund Master -- for Test Group flag FNTEST.
     C*
     C**************************************************************************
     C     GETFND        BEGSR
     C     FNKY          CHAIN     PFN01                              63
     C     *IN63         IFEQ      *ON
     C                   MOVEL     'Y'           FNTEST
     C                   ENDIF
     C                   Z-ADD     LGCO#         SVCO#
     C                   Z-ADD     LGFND         SVFND
     C                   ENDSR
     C*
     C**************************************************************************
     C*  THIS SUBROUTINE LOADS THE EXTRACT WORKFILE SO THE REPORT WILL
     C*  BE IN EDI REVIEWER ORDER.
     C**************************************************************************
     C     SRLOAD        BEGSR
     C*
     C*  READ ALL EDI LOG RECORDS FOR THE ENTERED JURISDICTION ST
     C*
     C     LGKY          CHAIN(N)  PLG01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C*  GET THE EDI CLAIMS CONTROL RECORD.  IF THE ELECTRONIC
     C*  SUBMISSION FLAG DOESN'T EQUAL 'E' FOR THE TRANSACTION
     C*  TYPE, DON'T PROCESS ANY OF THOSE TYPE RECORDS.
     C*
     C                   MOVE      '   '         XXTSET
     C                   MOVE      LGFND         X1FND
     c                   call      'WA076R'      ixapi                  60
     c                   if        @rtncd = 'Y'
     c                   move      '0'           *in21
     c                   else
     c                   move      '1'           *in21
     c                   endif
     C   21              MOVE      *ZEROS        IXCSDT
     C                   MOVE      IXCSDT        XXCSDT
     C                   MOVE      IXRLS         XXRLS             1
     C*
     C                   MOVE      '148'         XXTSET
     c                   call      'WA076R'      ixapi                  99
     c                   if        @rtncd = 'Y'
     c                   move      '0'           *in21
     c                   else
     c                   move      '1'           *in21
     c                   endif
     C*
     C                   MOVE      'A49'         XXTSET
     c                   call      'WA076R'      ixapi                  60
     c                   if        @rtncd = 'Y'
     c                   move      '0'           *in22
     c                   else
     c                   move      '1'           *in22
     c                   endif
     C*
     C*  CHECK SELECTION CRITERIA
     C*
     C*
     C*  Bypass test groups.
     C     LGCO#         IFNE      SVCO#
     C     LGFND         ORNE      SVFND
     C                   EXSR      GETFND
     C                   ENDIF
     C     FNTEST        CABEQ     'Y'           SKIP1
     C     FNEDI         CABEQ     'Y'           SKIP1
     C     FNEDIF        CABNE     XXEDIF        SKIP1
     C   21LGTSET        CABEQ     '148'         SKIP1
     C   22LGTSET        CABEQ     'A49'         SKIP1
     C     LGSTAT        CABEQ     'H'           SKIP1
     C     LGIDT         CABGT     XXEXDT        SKIP1
     C     LGSTS         IFNE      'EI'
     C     LGSTS         ANDNE     'EC'
     C     LGSTS         ANDNE     'RE'
     C                   GOTO      SKIP1
     C                   END
     C*
     C*  GET THE ADJUSTER'S NAME
     C*
     C                   MOVE      *ZEROS        CMSS#
     C                   MOVE      *BLANKS       CMCLEX
     C     CMKY          CHAIN     PCM01                              60
     C*
     C                   MOVE      *BLANKS       UPUSNM
     C                   MOVEL     CMCLEX        XXSTAF
     C     UPKY          CHAIN     PUP01                              60
     C                   MOVEL     UPUSNM        XXADJ
     C*
     C*  GET THE CLAIMANT'S NAME
     C*
     C                   MOVE      *BLANKS       CDLSN
     C                   MOVE      *BLANKS       CDFSN
     C     CDKY          CHAIN     PCD01                              60
     C                   MOVEL     CDLSN         XXCLNM
     C                   CAT       ',':0         XXCLNM
     C                   CAT       CDFSN:1       XXCLNM
     C*
     C*  LOAD THE RECORD INTO THE EXTRACT WORKFILE
     C*
     C                   Z-ADD     XXCO#         XTCO#
     C                   Z-ADD     LGFND         XTFND
     C                   Z-ADD     LGFYR         XTFYR
     C                   Z-ADD     LGCASE        XTCASE
     C                   MOVEL     LGTSET        XTTSET
     C                   MOVEL     XXADJ         XTSORT
     C                   Z-ADD     LGDUE         XTDUE
     C                   MOVEL     XXADJ         XTADJ
     C                   MOVEL     XXCLNM        XTCLNM
     C                   MOVEL     Q1USER        XTUSER
     C                   MOVEL     Q1PGM         XTPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        XTTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XTDT
     C                   MOVEL     LGMTC         XTMTC
     C                   WRITE     PXT01
     C*
     C*  PROCESS NEXT LOG RECORD
     C*
     C     SKIP1         TAG
     C     LGKY          READE(N)  PLG01                                  60
     C                   ENDDO
     C*
     C                   ENDSR
     C*
     C**************************************************************************
     C*  THIS SUBROUTINE BLANK OUT THE DATA ELEMENTS THAT WERE BEING
     C*  USED IN RELEASE 1 AND ARE NOT IN RELEASE 3 OF EDI 148 FORMAT.
     C**************************************************************************
     C     SR148B        BEGSR
     C*
     C                   MOVE      *BLANKS       FRCLSN
     C                   MOVE      *BLANKS       FRILSN
     C                   MOVE      *BLANKS       FRRPT#
     C                   MOVE      *BLANKS       FREADR
     C                   MOVE      *BLANKS       FRACDS
     C                   MOVE      *BLANKS       FRSS#
     C                   MOVE      *BLANKS       FRMID
     C                   MOVE      *BLANKS       FRPHN#
     C                   MOVE      *BLANKS       FROCC
     C                   MOVE      *BLANKS       FRCSAL
     C*
     C                   ENDSR
     C*
     C**************************************************************************
     C*  THIS SUBROUTINE BLANK OUT THE DATA ELEMENTS THAT WERE BEING
     C*  USED IN RELEASE 1 AND ARE NOT IN RELEASE 3 OF EDI A49 FORMAT.
     C**************************************************************************
     C     SRA49B        BEGSR
     C*
     C                   MOVE      *BLANKS       SUFEIN
     C                   MOVE      *BLANKS       SUSS#
     C                   MOVE      *BLANKS       SURETW
     C                   MOVE      *ZEROS        SUWAGE
     C                   MOVE      *BLANKS       SUCSAL
     C                   MOVE      *ZEROS        SU#PA
     C*
     C                   ENDSR
     C*
     C**************************************************************************
     C* MOVE FIELDS TO RECORD R21 FIELDS AND THEN WRITE RECORD
     C**************************************************************************
     C     SRR21         BEGSR
     C*
     C                   MOVEL     DN(295)       FRMTCC
     C                   MOVEL     DN(296)       XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         FRMTC2
     C                   MOVEL     DN(186)       FRJRBC
     C                   MOVEL     DN(15)        FRACLC
     C                   MOVEL     DN(187)       FRCAFE
     C                   MOVEL     DN(188)       FRCLNM
     C                   MOVEL     DN(135)       FRCLAL
     C                   MOVEL     DN(10)        FRAAD1
     C                   MOVEL     DN(11)        FRAAD2
     C                   MOVEL     DN(136)       FRCLCC
     C                   MOVEL     DN(270)       FREITQ
     C                   eval      freid = '000000000000000'
     C                   MOVEL     DN(255)       FRLSNS
     C                   MOVEL     DN(150)       FRAURL
     C                   MOVEL     DN(157)       FRSSRL
     C                   MOVEL     DN(43)        FRLSN
     C                   MOVEL     DN(45)        FRMID
     C                   MOVEL     DN(46)        FRADR1
     C                   MOVEL     DN(47)        FRADR2
     C                   MOVEL     DN(155)       FRACC
     C                   MOVE      DN(51)        FRPHN#
     C                   MOVEL     DN(146)       FRDRIC
     C                   MOVEL     DN(290)       FRTLC
     C                   MOVEL     DN(228)       FRRWEI
     C                   MOVEL     DN(189)       FRRTWT
     C                   MOVEL     DN(224)       FRPRI
     C                   MOVE      DN(314)       FRIDFE
     C*** Beg Del *** 05/25/23 *************************************
     C***                MOVEL     DN(17)        FRELSN
     C*** End Del *** 05/25/23 *************************************
     C*** Beg Add *** 03/19/24 *************************************
     C                   MOVEL     DN(17)        FRELSN
     C*** End Add *** 03/19/24 *************************************
     C                   MOVEL     DN(184)       FRETC
     C                   MOVEL     DN(185)       FRITCD
     C                   MOVEL     DN(292)       FRICSN
     C                   MOVEL     DN(200)       FRAAZP
     C                   MOVE      FRAAZP        XXZIP
     C     XXZIP4        IFEQ      '0000'
     C                   MOVE      '    '        FRAAZP
     C                   ENDIF
     C                   MOVEL     DN(249)       FRACPC
     C                   MOVEL     DN(118)       FRACSC
     C                   MOVEL     DN(119)       FRACLN
     C                   MOVEL     DN(120)       FRACNM
     C                   MOVEL     DN(121)       FRACTY
     C                   MOVEL     DN(122)       FRASTR
     C                   MOVEL     DN(123)       FRACST
     C                   MOVEL     DN(280)       FRACCC
     C                   MOVEL     DN(18)        FRELSN
     C                   MOVEL     DN(329)       FREUI#
     C                   MOVEL     DN(19)        FREAD1
     C                   MOVEL     DN(20)        FREAD2
     C                   MOVEL     DN(164)       FREMPC
     C                   MOVEL     DN(159)       FREPH#
     C                   MOVEL     DN(160)       FRECSN
     C                   MOVEL     DN(163)       FREAL
     C                   MOVEL     DN(165)       FREMCT
     C                   MOVEL     DN(166)       FREMCC
     C                   MOVEL     DN(167)       FREMZP
     C                   MOVE      FREMZP        XXZIP
     C     XXZIP4        IFEQ      '0000'
     C                   MOVE      '    '        FREMZP
     C                   ENDIF
     C                   MOVEL     DN(168)       FREMA1
     C                   MOVEL     DN(169)       FREMA2
     C                   MOVEL     DN(170)       FREMST
     C                   MOVEL     DN(60)        FROCCD
     C                   MOVEL     DN(199)       XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         FRFDDT
     C                   MOVEL     DN(73)        FRCLSC
     C                   MOVEL     DN(74)        FRCLTC
     C                   MOVEL     DN(77)        FRCLRC
     C                   MOVEL     DN(273)       FRSILC
     C                   MOVEL     DN(281)       XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         FRDODD
     C                   MOVEL     DN(204)       FRWWTC
     C                   MOVEL     DN(205)       FRWDSC
     C                   MOVEL     DN(206)       FRESID
     C                   MOVEL     DN(229)       FRINJS
     C                   MOVEL     DN(230)       FRSEMP
     C                   MOVEL     DN(75)        FRACCD
     C
     C                   MOVEL     DN(196)       XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         FRDRES
     C                   MOVEL(P)  DN(438)       FRCOLB
      /Free
       If dn(231) <> *blanks;
          intlen = %len(dn(231));
          If intLen > 1;
             FRClSb  = %subst(dn(231):%len(dn(231))-1:2);
           Else;
             Evalr FRClSb  = %trim(dn(231));
          Endif;
       Endif;
      /End-Free
     C*
     C* Load Variable Segment fields and calc counters
     C*
     C                   eval      FR#ADN = 0
     C                   eval      FR#DRC = 0
     C                   eval      FR#DRN = 0
     C                   eval      FR#MCO = 0
     C                   eval      FR#WN  = 0
     C                   eval      frvs21 = *blanks
     C*
 NEW C     CMKY          CHAIN     PCM01                              99
     C     *IN99         IFEQ      '0'
     C*
     C                   MOVE      CMDNLT        FRDNLT
     C                   MOVEL     CMJURO        FRJURO
     C                   MOVEL     CMCOVT        FRCOVT
     C                   MOVEL     CMMCOF        FRMCOF
     C                   IF        CLTID = '139'
     C                   MOVEL     CMMCTP        FRCOVT
     C                   ENDIF
     C                   MOVEL     CMMCNM        FRMCNM
     C                   MOVEL     CMIDTH        FRIDTH
     C                   MOVEL     CMDNYN        FRDNYN
     C                   MOVEL     CMDNY1        FRDNY1
     C                   MOVEL     CMDNY2        FRDNY2
     C                   MOVEL     CMDNY3        FRDNY3
     C                   MOVEL     CMDNY4        FRDNY4
     C                   MOVEL     CMDNY5        FRDNY5
     C                   SELECT
     C                   WHEN      CMEITQ = 'E'
     C                   MOVEL     DN(152)       FREID
     C                   WHEN      CMEITQ = 'G'
     C                   MOVEL     DN(153)       FREID
     C                   WHEN      CMEITQ = 'A'
     C                   MOVEL     DN(154)       FREID
     C                   WHEN      CMEITQ = 'P'
     C                   MOVEL     DN(156)       FREID
     C                   OTHER
     C                   MOVEL     DN(42)        FREID
     C                   ENDSL
otxx C                   If        XXRLSA = *ON
otxx C                   EXSR      SRR21A
otxx C                   Endif
     C*
     C                   EXSR      SRVS21
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C**************************************************************************
     C* MOVE FIELDS TO RECORD R22(RLS 3.1)FIELDS AND THEN WRITE RECORD
     C**************************************************************************
     C     SRR21A        BEGSR
     C*
     C                   MOVEL     DN(421)       FRLOC
     C                   MOVEL     DN(422)       FRLOCFT
     C*
     C                   MOVEL     DN(297)       XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         FRIDLT
     C*
     C                   MOVEL     DN(403)       FRIRTC
     C                   MOVEL     DN(404)       FRIRPR
     C                   MOVEL     DN(405)       FRIRSE
     C                   MOVEL     DN(406)       FRLRTC
     C                   MOVEL     DN(407)       FRLRPR
     C                   MOVEL     DN(408)       FRLRSE
     C*
     C                   MOVEL     DN(144)       XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         FRCDDB
     C*
     C                   MOVEL     DN(145)       XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         FRCDLW
     C*
     C                   MOVEL     DN(416)       FRCDEK
     C                   MOVEL     DN(417)       FRCDAK
     C                   EVAL      FRCAPH = DN(137)
     C                   EVAL      FRCAEM = DN(138)
     C                   EVAL      FRCARP = DN(140)
     C                   EXSR      SR#PB
     C                   EXSR      SR#CE
     C*
     C                   ENDSR
     C**************************************************************************
     C* MOVE FIELDS TO RECORD R22 FIELDS AND THEN WRITE RECORD
     C**************************************************************************
     C     SRR22         BEGSR
     C*
     C                   EVAL      SUTSET = DN(1)
     C                   EVAL      SUMTCC = DN(295)
     C*
     C                   MOVEL     DN(296)       XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         SUMTC2
     C*
     C                   MOVEL     DN(298)       XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         SUCNLT
     C*
     C                   EVAL      SUJURO = DN(186)
     C                   EVAL      SUACLC = DN(15)
     C                   EVAL      SUCAFE = DN(187)
     C                   EVAL      SUCNAM = DN(188)
     C                   EVAL      SUCARP = DN(140)
     C                   EVAL      SUCAPH = DN(137)
     C                   EVAL      SUCAEM = DN(138)
     C                   EVAL      SUCAFX = DN(139)
     C                   MOVEL     DN(42)        SUSS#
     C                   EVAL      SULSN  = DN(43)
     C                   EVAL      SUFSN  = DN(44)
     C                   EVAL      SUMID  = DN(45)
     C*
     C                   MOVEL     DN(52)        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         SUDTOB
     C*
     C                   EVAL      SUMSTS = DN(54)
     C     DN(213)       IFEQ      *BLANKS
     C                   MOVE      *ZEROS        DN(213)
     C                   ENDIF
     C                   MOVEL     DN(213)       SUENEX
     C                   EVAL      SUETFS = DN(158)
     C                   EVAL      SUIDTH = DN(146)
     C                   MOVE      DN(314)       SUIDFE
     C                   MOVE      DN(16)        SUEMP#
     C                   MOVE      DN(292)       SUIIF
     C                   MOVE      DN(23)        SUEZP
     C     XXRLS         IFEQ      '3'
     C                   MOVE      SUEZP         XXZIP
     C     XXZIP4        IFEQ      '0000'
     C                   MOVE      '    '        SUEZP
     C                   ENDIF
     C                   ENDIF
     C                   IF        FRJRST = 'CA' and
     C                             %SUBST(SUEZP:6:4) = '0000'
     C                   EVAL      %SUBST(SUEZP:6:4) = *BLANKS
     C                   ENDIF
     C                   EVAL      SUSAME = DN(228)
     C*
     C                   MOVEL     DN(281)       XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         SUDODD
     C*
     C                   EVAL      SUNCPC = DN(212)
     C                   EVAL      SUWMTH = DN(172)
     C*
     C                   MOVEL     DN(145)       XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         SUCDLW
     C*
     C                   MOVEL     DN(144)       XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         SUCDOB
     C*
     C                   MOVEL     DN(65)        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         SUIDLD
     C*
     C                   EVAL      SURTWT = DN(189)
     C                   EVAL      SUPRIN = DN(224)
     C*
     C                   MOVEL     DN(193)       XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         SUSUEF
     C*
     C                   MOVEL     DN(199)       XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         SUDYEF
     C*
     C                   MOVEL     DN(196)       XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         SUDRES
     C*
     C                   EVAL      SUPDNY = DN(294)
     C                   MOVE      DN(134)       SUCAWW
     C*
     C                   MOVEL     DN(256)       XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         SUWGEF
     C*
     C     DN(149)       IFEQ      *BLANKS
     C                   MOVE      *ZEROS        DN(149)
     C                   ENDIF
     C                   MOVE      DN(149)       SUDFBE
     C*
     C                   EVAL      SUTOLC = DN(290)
     C                   EVAL      SUESTS = DN(58)
     C                   EVAL      SUPIMP = DN(223)
     C*
     C     DN(68)        IFEQ      *BLANKS
     C                   MOVE      *ZEROS        DN(68)
     C                   END
     C                   MOVEL     DN(68)        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         SURTWD
     C*
     C                   EVAL      SUFWPI = DN(66)
     C                   EVAL      SULUMC = DN(293)
     C                   EVAL      SUFSAL = DN(273)
     C*
     C                   MOVE      DN(286)       SUAWW
     C*
     C                   MOVEL     DN(297)       XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         SUIDLT
     C*
     C                   MOVEL     DN(299)       XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         SUSTDT
     C*
     C                   EVAL      SUCAZP = DN(200)
     C                   MOVE      SUCAZP        XXZIP
     C     XXZIP4        IFEQ      '0000'
     C                   MOVE      '    '        SUCAZP
     C                   ENDIF
     C* Variable Segment counters
     C*** DONE IN SR#PA
     C*** DONE IN SRPTP
RKK  C                   Z-ADD     0             SU#DRC
RKK  C                   Z-ADD     0             SU#DRN
     C                   Z-ADD     0             SU#SNN
     C                   EVAL      SUPSPA = DN(203)
     C                   EVAL      SUWWTC = DN(204)
     C                   EVAL      SUWDSC = DN(205)
     C                   EVAL      SUESID = DN(206)
     C                   EVAL      SUINJS = DN(229)
     C     CMKY          CHAIN     PCM01                              99
     C                   IF        *IN99 = *OFF
     C                   IF        CMDNY1 <> ' '
     C                   ADD       1             SU#DRC
     C                   ENDIF
     C*
     C                   IF        CMDNY2 <> ' '
     C                   ADD       1             SU#DRC
     C                   ENDIF
     C*
     C                   IF        CMDNY3 <> ' '
     C                   ADD       1             SU#DRC
     C                   ENDIF
     C*
     C                   IF        CMDNY4 <> ' '
     C                   ADD       1             SU#DRC
     C                   ENDIF
     C*
     C                   IF        CMDNY5 <> ' '
     C                   ADD       1             SU#DRC
     C                   ENDIF
      *
      *** R3.1 DENIAL REASONS START *** (OCCUR CHANGE FROM 3 to 10)
     C                   MOVEL(P)  CMDNYN        XX150
     C                   IF        XX1501 <> ' '
     C                   ADD       1             SU#DRN
     C                   ENDIF
     C*
     C                   IF        XX1502 <> ' '
     C                   ADD       1             SU#DRN
     C                   ENDIF
     C*
     C                   IF        XX1503 <> ' '
     C                   ADD       1             SU#DRN
     C                   ENDIF
      *** R3.1 DENIAL REASONS END ***
     C
      *** R3.1 SUSPENSION NARRATIVE START *** (OCCUR CHANGE FROM 3 to 10)
     c*** begin add ***  01/22/24  *******************************************
     C                   Clear                   xxTemp          150
      /free
      //* Remove unwanted characters and blank out the \n carrage returns.
           xxTemp = %trim(RmvExceptJSON(%trim(CMsusn)));
           xxTemp = %scanrpl('\n':' ':xxTemp);
           XX150  = %subst(xxTemp:1:150);
      /end-free
     c*** end add *****  01/22/24  *******************************************
     c*** beg del *****  01/22/24  *******************************************
     C****               MOVEL(P)  CMSUSN        XX150
     c*** end del *****  01/22/24  *******************************************
     C                   IF        XX1501 <> ' '
     C                   ADD       1             SU#SNN
     C                   ENDIF
     C*
     C                   IF        XX1502 <> ' '
     C                   ADD       1             SU#SNN
     C                   ENDIF
     C*
     C                   IF        XX1503 <> ' '
     C                   ADD       1             SU#SNN
     C                   ENDIF
     C*** R3.1 SUSPENSION NARRARIVE END ***
     C*
     C                   ENDIF
       //*** Beg Add *** 02/14/22 *********************************************
       Monitor;
       SUeduL = %Dec(DN(151):2:0);
       On-error;
          SUedul = 0;
       Endmon;
       //*** End Add *** 02/14/22 *********************************************
     C*
     C                   ENDSR
     C**************************************************************************
     C* MOVE FIELDS TO RECORD R22(RLS 3.1)FIELDS AND THEN WRITE RECORD
     C**************************************************************************
     C     SRR22A        BEGSR
     C*
     C                   MOVEL     DN(201)       SUAWLI
     C                   MOVEL     DN(416)       XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         SUCDEK
     C                   MOVEL     DN(417)       XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         SUCDAK
     C                   MOVEL     DN(401)       SUJRC#
     C                   MOVEL     DN(403)       SUIRTC
     C                   MOVEL     DN(404)       SUIRPR
     C                   MOVEL     DN(405)       SUIRSE
     C                   MOVEL     DN(406)       SULRTC
     C                   MOVEL     DN(407)       SULRPR
otxx C                   If        SULRTC = *BLANKS OR SULRPR = *BLANKS
otxx C                   MOVE      *BLANKS       SURTW
otxx C                   Endif
     C                   MOVEL     DN(408)       SULRSE
     C                   MOVEL     DN(418)       SUSRCF
     C                   MOVEL     DN(419)       SUSRCP
     C*
     C                   MOVEL     DN(423)       XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         SUALKD
     C*
     C                   MOVEL     DN(436)       XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         SUPDED
       //*** Beg Add *** 02/14/22 *********************************************
       SUbcRc = DN(439);

       Monitor;
       SUwhAf = %Dec(DN(440):9:2)/100;
       On-error;
          SUwhAf = 0;
       Endmon;

       Monitor;
       SUeduL = %Dec(DN(151):2:0);
       On-error;
          SUeduL = 0;
       Endmon;

       Monitor;
       SUoPyA = %Dec(DN(433):9:2)/100;
       On-error;
          SuoPya = 0;
       Endmon;

       SUnTz  = DN(442);
       //*** End Add *** 02/14/22 *********************************************
     C*
     C                   ENDSR
     C**************************************************************************
     C* SRPTPI - Load WXPTP with payment information for IP ONLY
     C**************************************************************************
     C     SRPTPI        BEGSR
     C*
     C*** This subroutine will only be called for an IP from SR#PA
     C*** We will already have the WDPAP record from the SR#PA subroutine
     C*
     C                   Z-ADD     XXEXDT        PTEXDT
     C                   MOVEL     DN(1)         PTTSET
     C                   MOVEL     DN(2)         PTMTC
     C                   MOVEL     DN(4)         PTJRST
     C                   MOVEL     DN(15)        PTACLC
     C                   MOVEL     Q1USER        PTUSER
     C                   MOVEL     Q1PGM         PTPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        PTTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PTDT
     C                   EVAL      PTCO# = PACO#
     C                   EVAL      PTFND = PAFND
     C                   EVAL      PTFYR = PAFYR
     C                   EVAL      PTCASE = PACASE
     C                   MOVE      LGCO#         PTCO#
     C                   Z-ADD     XXEXDT        PTEXDT
     C                   MOVEL     DN(1)         PTTSET
     C                   MOVEL     DN(2)         PTMTC
     C**                 MOVEL     DN(3)         XXDATE
     C**                 Z-ADD     XXDATE        SRDTI
     C**                 CALL      'SRCVTDAT'    DTYYMD
     C**                 MOVEL     SRDTO         PTTDT
     C                   MOVEL     DN(4)         PTJRST
     C                   MOVEL     DN(15)        PTACLC
     C*  Retrieve payment reason code
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     PABR2         XXKEY
     C     TBKY          CHAIN     PB201                              60
     C     *IN60         IFEQ      '0'
     C     B2PYCD        ANDNE     *BLANKS
     C                   EVAL      PTPRCD = B2PYCD
     C                   ELSE
     C                   EVAL      PTPRCD = *BLANKS
     C                   ENDIF
     C* Retrieve Payee name
     C                   EVAL      PTPAYE = *BLANKS
     C                   EVAL      PTISDT = *ZEROS
     C                   Z-ADD     PASEQ#        XXSEQ#
     C     RPKY          CHAIN     PRP01                              60
     C                   IF        *IN60 = *OFF and RPCHK# = PACHK#
     C                   EVAL      PTPAYE = RPPNAM
     C                   ELSE
     C                   Z-ADD     PABASQ        XXSEQ#
     C     RPKY          CHAIN     PRP01                              60
     C                   IF        *IN60 = *OFF and RPCHK# = PACHK#
     C                   EVAL      PTPAYE = RPPNAM
     C                   ENDIF
     C                   ENDIF
     C* Retrieve Payment Issue Date
     C     CLTID         IFEQ      '139'
     C                   exsr      Srpast
     C     CPKY1         CHAIN     PCP01                              60
     C                   ELSE
     C     CPKY          CHAIN     PCP01                              60
     C                   ENDIF
     C                   IF        *IN60 = *OFF
     C                   EVAL      PTISDT = CPCMDT
     C                   ENDIF
     C*
     C                   IF        SVISDT <> *ZEROS
     C                   EVAL      PTISDT = SVISDT
     C                   ENDIF
     C*
     C                   EVAL      PTAMT  = PAAMT
     C                   EVAL      PTSTDT = PAFRDT
     C                   EVAL      PTENDT = PATODT
     C                   EVAL      PTFILL = *ZEROS
     C                   EVAL      PTUSER = Q1USER
     C                   EVAL      PTPGM = Q1PGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        PTTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PTDT
     C*
     C                   WRITE     PPT01
     C                   ADD       1             SU#PMT
     C*
     C                   ENDSR
     C*
     C**************************************************************************
     C* SRPTP - Load WXPTP with payment information
     C**************************************************************************
     C     SRPTP         BEGSR
     C*** NOT IP, READ FROM NEW FILE
     C
     C                   IF        LGMTC = 'PY' OR LGMTC = '02' OR
     C                             LGMTC = 'AP' OR LGMTC = 'RB'
     C*
     C*** IF THERE IS A PAYMENT RECORD FOR THIS LOG RECORD,
     C*** LOAD THE ARRAYS FROM THAT RECORD IN WDPTP.
     C*
     C     LGDSQH        IFGT      *ZEROS
     C     P0KY          CHAIN     PP001                              60
     C                   DOW       NOT *IN60
     C                   MOVE      LGCO#         PTCO#
     C                   Z-ADD     XXEXDT        PTEXDT
     C                   MOVEL     DN(1)         PTTSET
     C                   MOVEL     DN(2)         PTMTC
     C                   MOVEL     DN(4)         PTJRST
     C                   MOVEL     DN(15)        PTACLC
     C                   MOVEL     Q1USER        PTUSER
     C                   MOVEL     Q1PGM         PTPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        PTTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PTDT
     C**********
     C                   EVAL      PTCO# = P0CO#
     C                   EVAL      PTFND = P0FND
     C                   EVAL      PTFYR = P0FYR
     C                   EVAL      PTCASE = P0CASE
     C                   EVAL      PTPRCD = P0PRCD
     C                   EVAL      PTPAYE = P0PAYE
     C                   EVAL      PTISDT = P0ISDT
     C                   EVAL      PTAMT  = P0AMT
     C                   EVAL      PTSTDT = P0STDT
     C                   EVAL      PTENDT = P0ENDT
     C                   EVAL      PTFILL = *ZEROS
     C                   EVAL      PTUSER = Q1USER
     C                   EVAL      PTPGM = Q1PGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        PTTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PTDT
     C*
     C                   WRITE     PPT01
     C                   ADD       1             SU#PMT
     C*
     C     P0KY          READE     PP001                                  60
     C                   ENDDO
     C                   ENDIF
     C                   ENDIF
     C**************************************************************
     C*
     C*****
     C**************************************************************
     C****FOR IP, READ FROM THE PAYMENT FILE
     C*****
     C*
     C                   ENDSR
     C**************************************************************************
     C* SR#A3 - Load WXA3P
     C**************************************************************************
     C     SR#A3         BEGSR
     C* Read records from WDA3P
     C     A3KY          CHAIN     PA301                              60
     C                   DOW       NOT *IN60
     C                   EVAL      XXBTC = A3BTC
     C     A3KY1         CHAIN     PA301X
     C                   EVAL      A0CO# = A3CO#
     C                   EVAL      A0FND = A3FND
     C                   EVAL      A0FYR = A3FYR
     C                   EVAL      A0CASE= A3CASE
     C                   EVAL      A0BTC = A3BTC
     C                   EVAL      A0PAID= A3PAID
     C                   EVAL      A0STDT= A3STDT
     C                   EVAL      A0ENDT= A3ENDT
     C                   EVAL      A0PID = A3PID
     C*
     C                   IF        %FOUND
     C                   UPDATE    PA301X
     C                   ELSE
     C                   WRITE     PA301X
     C                   ENDIF
     C*
     C     A3KY          READE     PA301                                  60
     C                   ENDDO
     C                   ENDSR
     C**************************************************************************
     C* SR#CN - Load WXN3P Claim Narrative
     C**************************************************************************
      /Free
       Begsr sr#Cn;
       //
       // Read records from wdv0p and write to wxv3p
       //


           Setll (LGco#:LGfnd:LGfyr:LGcase:LGmtc:LGmtCd) pv001;

           If %equal(WDV0L);
              Reade (LGco#:LGfnd:LGfyr:LGcase:LGmtc:LGmtCd) pv001;

              Dow not %eof(WDV0L);

                 Chain (LGco#:DN(4):xxExDt:DN(1):DN(15):DN(2)) pv301;

                 V3Co# = LGco#;
                 V3JrSt = DN(4);
                 V3ExDt = xxExDt;
                 V3tSet = DN(1);
                 V3aClc = DN(15);
                 V3mtC  = DN(2);
                 V3cRnA = V0cRnA;
                 V3seq# = V0seq#;

                 If %Found(wxv3l);
                    Update pv301;
                 Else;
                   Write pv301;
                 Endif;

                 Reade (LGco#:LGfnd:LGfyr:LGcase:LGmtc:LGmtCd) pv001;
              Enddo;
           Endif;
       Endsr;
      /End-Free
     C**************************************************************************
     C* SR#E3 - Load WXE3P
     C**************************************************************************
     C     SR#E3         BEGSR
RKK  C                   Z-ADD     0             SU#CEM
     C* Read records from WDE3P
     C     A3KY          CHAIN     PE301                              60
     C                   DOW       NOT *IN60
     C                   EVAL      XXENAM = E3ENAM
RKK  C                   ADD       1             SU#CEM
     C     E3KY          CHAIN     PE301X
     C                   EVAL      E0CO# = E3CO#
     C                   EVAL      E0FND = E3FND
     C                   EVAL      E0FYR = E3FYR
     C                   EVAL      E0CASE= E3CASE
     C                   EVAL      E0ENAM= E3ENAM
     C                   EVAL      E0PHON= E3PHON
     C                   EVAL      E0WAGE= E3WAGE
     C                   EVAL      E0FIL = E3FIL
     C                   IF        %FOUND
     C                   UPDATE    PE301X
     C                   ELSE
     C                   WRITE     PE301X
     C                   ENDIF
     C     A3KY          READE     PE301                                  60
     C                   ENDDO
     C                   ENDSR
     C**************************************************************************
     C* SR#R3 - Load WXR3P
     C**************************************************************************
     C     SR#R3         BEGSR
RKK  C                   Z-ADD     0             SU#RDE
     C* Read records from WDR3P
     C     A3KY          CHAIN     PR301                              60
     C                   DOW       NOT *IN60
     C                   EVAL      XXREWK = R3REWK
RKK  C                   ADD       1             SU#RDE
     C     R3KY          CHAIN     PR301X
     C                   EVAL      R0CO# = R3CO#
     C                   EVAL      R0FND = R3FND
     C                   EVAL      R0FYR = R3FYR
     C                   EVAL      R0CASE= R3CASE
     C                   EVAL      R0REWK= R3REWK
     C                   EVAL      R0ARE = R3ARE
     C                   EVAL      R0DRE = R3DRE
     C                   IF        XXRLSA = *ON
     C                   IF        R3RESD > *ZERO
     C                   MOVE      R3RESD        R0RESD
     C                   ELSE
     C                   MOVE      *ZEROS        R0RESD
     C                   ENDIF
     C                   IF        R3REED > *ZERO
     C                   MOVE      R3REED        R0REED
     C                   ELSE
     C                   MOVE      *ZEROS        R0REED
     C                   ENDIF
     C                   IF        R3REWA > *ZERO
     C                   Eval      R0REWA = R3REWA
     C                   ELSE
     C                   Eval      R0REWA  = 0
     C                   ENDIF
     C                   ENDIF
     C                   IF        %FOUND
     C                   UPDATE    PR301X
     C                   ELSE
     C                   WRITE     PR301X
     C                   ENDIF
     C     A3KY          READE     PR301                                  60
     C                   ENDDO
     C                   ENDSR
     C**************************************************************************
     C**************************************************************************
     C* SR#PB - Used to determine if a segment is needed
     C**************************************************************************
     C     SR#PB         BEGSR
     C* Used to determine if a segment is needed
     C                   EVAL      FR#PBI = 0
     C                   If        FRPART <> *blanks
     C                   EVAL      FR#PBI = 1
     C                   ENDIF
     C                   ENDSR
     C**************************************************************************
     C**************************************************************************
     C* SR#HD - Load WXH3P
     C**************************************************************************
     C     SR#HD         BEGSR
     C                   EVAL      FR#DE = 0
     C* Read records from WDH0P
     C     HOKY          CHAIN     PH001                              60
     C                   DOW       NOT *IN60
     C                   EVAL      H3CO# = LGCO#
     C                   MOVEL     DN(4)         H3JRST
     C                   EVAL      H3EXDT= XXEXDT
     C                   MOVEL     DN(1)         H3TSET
     C                   MOVEL     DN(15)        H3ACLC
     C                   MOVEL     DN(2)         H3MTC
     C                   EVAL      H3CDN#= H0CDN#
     C                   EVAL      H3CRCD= H0CRCD
     C*
     C                   WRITE     PH301
     C                   EVAL      FR#DE = FR#DE + 1
     C*
     C     HOKY          READE     PH001                                  60
     C                   ENDDO
     C                   ENDSR
     C**************************************************************************
     C**************************************************************************
     C     SR#CE         BEGSR
     C                   EVAL      FR#CE = 0
     C* Read records from WDN0P
     C     NOKY          CHAIN     PN001                              60
     C                   DOW       NOT *IN60
     C                   EVAL      N3CO# = LGCO#
     C                   MOVEL     DN(4)         N3JRST
     C                   EVAL      N3EXDT= XXEXDT
     C                   MOVEL     DN(1)         N3TSET
     C                   MOVEL     DN(15)        N3ACLC
     C                   MOVEL     DN(2)         N3MTC
     C                   EVAL      N3CXRC= N0CXRC
     C                   EVAL      N3JRC#= N0JRC#
     C                   EVAL      N3CRNA= N0CRNA
     C*
     C                   WRITE     PN301
     C                   EVAL      FR#CE = FR#CE + 1
     C*
     C     NOKY          READE     PN001                                  60
     C                   ENDDO
     C                   ENDSR
      *** End Add *** 05/30/18 **************************
     C**************************************************************************
     C* MOVE FIELDS TO RECORD R22 FIELDS AND THEN WRITE RECORD
     C**THIS PROCESSING IS BEING COMMENTED FOR PHASE 1.......
     C**************************************************************************
     C***************************************************************
     C*  VARIABLE SEGMENT LOADING
     C***************************************************************
     C     SRVS21        BEGSR
     C*
     C                   movea     dn(38)        a150
     C     1             do        3             c                 2 0
     C                   if        A150(c) <> *blanks
     C                   eval      fr#adn = fr#adn + 1
     c                   endif
     C                   enddo
     c*
     C*
      /Free
           x2 = *zeros;
       For x1 = 1 to 5;
           if @dny(x1) <> *blanks;
              fr#drc = fr#drc + 1;
              if fr#drc = 1;
                 deny = @dny(x1);
              else;
              deny = %subst(deny:1:x2) + @dny(x1);
              Endif;
              x2 = x2 + 2;
           Endif;
       Endfor;
      /End-free
     C                   movea     cmdnyn        d150
     C     1             do        3             c
     C                   if        d150(c) <> *blanks
     C                   eval      fr#drn = fr#drn + 1
     c                   endif
     C                   enddo
     C*
     C*
      /Free
       //*** Beg Chg *** 11/09/23 *********************************/
       //***if (cmmcof <> *blanks and cmmcof <> '000000000') or
       //***   (cmmcnm >  *blanks);
         if (cmmcof <> *blanks and cmmcof <> '000000000') or
            (cmmcnm >  *blanks) or (cmmctp <> *blanks);
       //*** End Chg *** 11/09/23 *********************************/
            fr#mco = 1;
            covt   = cmmctp;
            mcname = cmmcnm;
            mcof   = cmmcof;
         endif;
      /End-Free
     C                   clear                   witness1
     C                   clear                   witness2
     C                   clear                   witness3
     C                   clear                   witness4
     C                   clear                   witness5
     C                   eval      c = 0
     C*
     C     cmky          chain     pwi01
     c                   if        %found
     c                   dou       %eof or c = 5
     C                   clear                   witness
     C                   eval      fr#wn = fr#wn + 1
     c                   eval      name  =  winame
     c                   eval      phn# =  wibph#
     c                   eval      c = c + 1
     c*
     c                   select
     c                   when      c = 1
     c                   eval      witness1 = witness
     c                   when      c = 2
     c                   eval      witness2 = witness
     c                   when      c = 3
     c                   eval      witness3 = witness
     c                   when      c = 4
     c                   eval      witness4 = witness
     c                   when      c = 5
     c                   eval      witness5 = witness
     c                   endsl
     C     cmky          reade     pwi01
     c                   enddo
     c                   endif
     C*
     C                   select
     C                   when      fr#adn = 0
     c                   eval      ad = 0
     C                   when      fr#adn = 1
     c                   eval      ad = 50
     C                   when      fr#adn = 2
     c                   eval      ad = 100
     C                   when      fr#adn = 3
     c                   eval      ad = 150
     C                   endsl
     C*
     C                   select
     C                   when      fr#drc = 0
     C                   eval      dy = 0
     C                   when      fr#drc = 1
     C                   eval      dy = 2
     C                   when      fr#drc = 2
     C                   eval      dy = 4
     C                   when      fr#drc = 3
     C                   eval      dy = 6
     C                   when      fr#drc = 4
     C                   eval      dy = 8
     C                   when      fr#drc = 5
     C                   eval      dy = 10
     C                   endsl
     C*
     C                   select
     C                   when      fr#drn = 0
     c                   eval      dr = 0
     C                   when      fr#drn = 1
     c                   eval      dr = 50
     C                   when      fr#drn = 2
     c                   eval      dr = 100
     C                   when      fr#drn = 3
     c                   eval      dr = 150
     C                   endsl
     C*
     C                   select
     C                   when      fr#mco = 0
     c                   eval      cv = 0
     C                   when      fr#mco = 1
     c                   eval      cv = 71
     C                   endsl
     C*
     C                   select
     C                   when      fr#wn  = 0
     c                   eval      wn = 0
     C                   when      fr#wn  = 1
     c                   eval      wn = 75
     C                   when      fr#wn  = 2
     c                   eval      wn = 150
     C                   when      fr#wn  = 3
     c                   eval      wn = 225
     C                   when      fr#wn  = 4
     c                   eval      wn = 300
     C                   when      fr#wn  = 5
     c                   eval      wn = 375
     C                   endsl
     C*
     C*
      /Free
       // Part of Body Segment

       If FR#pbi <> *zeros;
          BI = 24;
       Else;
          BI = *zeros;
       Endif;


       // Change Data Elements Segment

       clear chgdata;
       clear chgdata#;

       If FR#de > *zeros;
          Setll wxky WXH3L;
          If %equal(WXH3L);
             Reade wxky PH301;
             Dow not %eof(WXH3L);
                cda = cda + 1;
                change# = h3cdn#;
                chgcode = h3crcd;
                If cda = 1;
                   chgdata# = chgdata(cda);
                Else;
                   Chgdata# = %subst(chgdata#:1:cd) + chgdata(cda);
                Endif;
                cd  = cd + 40;
                Reade wxky PH301;
             Enddo;
          Endif;
       Endif;

       // Cancel Elements Segment
       clear candata;

       If FR#ce > *zeros;
          Chain wxky WXN3L;
          If %found(WXN3L);
             cancode = N3cxrc;
             claim#  = N3jrc#;
             canres  = N3crna;
          Endif;
       Endif;

      /End-Free
     C*
     C                   If        XXRLSA = *ON
     C                   eval      frvs21 = %subst(fracds:1:ad) +
     C                             %subst(deny:1:dy) + %subst(cmdnyn:1:dr) +
     C                             %subst(coverage:1:cv) +
     C                             %subst(witnesses:1:wn)+
     C                             %subst(bodyinj:1:bi)+
     C                             %trim(chgdata#)+ candata
     C                   Else
     C                   eval      frvs21 = %subst(fracds:1:ad) +
     C                             %subst(deny:1:dy) + %subst(cmdnyn:1:dr) +
     C                             %subst(coverage:1:cv) +
     C                             %subst(witnesses:1:wn)
     C                   Endif
     C*
     C                   ENDSR
     C     DARTS         BEGSR
     C*
     C     LGTSET        IFEQ      '148'
     C     LGTSET        OREQ      'A49'
     C*
     C*                  MOVE      LGJST         XXJST
     C*                  MOVE      LGTSET        XXTSET
     C*
     C                   MOVE      LGFND         X1FND
     c                   CALL      'WA076R'      IXAPI                  99
     c                   if        @RTNCD = 'Y'
     C*
     C                   SELECT
     C     LGSTS         WHENEQ    'RE'
     C     IXCRER        ANDNE     *BLANKS
     C                   MOVE      IXCRER        Q#RMCD            5
     C*
     C     LGSTS         WHENEQ    'EI'
     C     IXCEIR        ANDNE     *BLANKS
     C                   MOVE      IXCEIR        Q#RMCD            5
     C*
     C     LGSTS         WHENEQ    'EC'
     C     IXCECR        ANDNE     *BLANKS
     C                   MOVE      IXCECR        Q#RMCD            5
     C*
     C     LGSTS         WHENEQ    'TA'
     C     IXCTAR        ANDNE     *BLANKS
     C                   MOVE      IXCTAR        Q#RMCD
     C*
     C     LGSTS         WHENEQ    'TR'
     C     IXCTRR        ANDNE     *BLANKS
     C                   MOVE      IXCTRR        Q#RMCD
     C*
     C*
     C                   OTHER
     C                   GOTO      END
     C                   ENDSL
     C*
     C                   MOVE      LGCO#         QCCO#
     C                   MOVE      LGFND         QCFND
     C                   MOVE      LGFYR         QCFYR
     C                   MOVE      LGCASE        QCCASE
     C                   MOVEL(P)  QCKEY         Q#KEY
     C*
     C     Q#RMCD        CHAIN     PCT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL(P)  CTDEPG        Q#DCID           12
     C                   MOVEL(P)  CTDESC        Q#COMM
     C                   MOVEL(P)  CTFL01        Q#DIFL            1
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       Q#COM2           19
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         Q#CDAT            8
     C*
     C                   MOVEL(P)  'W0070R'      Q#PNAM           10
     C                   MOVE      *ZEROS        Q#SECL            3
     C*
     C     CMCLEX        CHAIN     PUP03                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE(P)   UPUSR         Q#USER           10
     C                   ENDIF
     C     Q#USER        IFEQ      *BLANKS
     C                   GOTO      END
     C                   ENDIF
     C     LGSTS         IFEQ      'EC'
     C     IXCECR        ANDNE     *BLANKS
     C                   MOVE(P)   Q1USER        Q#USER           10
     C                   ENDIF
     C     LGSTS         IFEQ      'TA'
     C     IXCTAR        ANDNE     *BLANKS
     C                   MOVE(P)   Q1USER        Q#USER           10
     C                   ENDIF
     C*
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C*
     C     Q#DCID        IFNE      *BLANKS
     C                   MOVEL(P)  Q#KEY         Q#FOLD           80
     C                   ELSE
     C                   MOVEL(P)  *BLANKS       Q#FOLD           80
     C                   ENDIF
     C*
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C*
     C     Q#DCID        IFNE      *BLANKS
     C                   MOVEL(P)  'Y'           Q#FL01            1
     C                   ELSE
     C                   MOVEL(P)  *BLANK        Q#FL01            1
     C                   ENDIF
     C*
     C     CTDPRG        IFGT      0
     C                   MOVEL(P)  'Y'           Q#FL02            1
     C                   ELSE
     C                   MOVEL(P)  *BLANK        Q#FL02            1
     C                   ENDIF
     C*
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
     C                   MOVEL     *BLANKS       Q#COMM
     C     LGMTC         CAT       '-':0         Q#COMM
     C                   CAT       CTDESC:0      Q#COMM
     C                   CALL      'DA700R'
     C                   PARM                    Q#KEY            40
     C                   PARM                    Q#COMM          256
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
     C
     C                   ENDIF
     C                   ENDIF
     C
     C     END           ENDSR
      *==============================================================
      *   SRPAST - SUBROUTINE TO CALCULATE A DATE IN THE PAST    *
      *==============================================================
     c     Srpast        begsr
     **
     c                   eval      srdays = 1
     c                   eval      srdti = patrdt
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         wkfdat            8 0
     c                   move      wkfdat        srdte
      *
      * Calculate a date in the past
     C                   call      'SRPAST8'
     C                   Parm                    srdte             8 0
     C                   Parm                    srdays            5 0
     C                   Parm      *blank        srerr             1
     C                   Parm      *blank        srterm            1
      *
     c                   z-add     srdte         srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         xxpodt

     c                   endsr
     O**************************************************************************
     O*  EDI 148 AUDIT DETAIL REPORT
     OQSYSPRT   E            ROUTE          1 06
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       ORDIS1              60
     O          E            ROUTE          1
     O                       ORDIS2              60
     O          E            ROUTE          1
     O                       ORDIS3              60
     O          E            ROUTE          2
     O                       ORDIS4              60
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       XXTITL              80
     O          e            route          1 59
     O                       #brnd1
     O          e            route          1
     O                       #brnd2
     O*
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             118
     O                       DSDATE        Y    198
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       XXTITL             129
     O                                          192 'PAGE:'
     O                       PAGE          Z    198
     O          E            HEAD           2
     O                                           19 'JURISDICTION STATE:'
     O                       QQJST               22
     O                                           53 'EXTRACT SUBMISSION DATE:'
     O                       X1EXDT              64 '  /  /    '
     O                                           79 'ADJUSTER:'
     O                       XTADJ              122
     O*
     O          E            DET            1
     O                                           26 'TRANSACTION             '
     O                                          123 'JURISDICTION            '
     O          E            DET            1
     O                                           26 ' 1. TRANSACTION SET ID  '
     O                       FRTSET              54
     O                                          123 ' 4. JURISDICTION        '
     O                       FRJRST             150
     O          E            DET            1
     O                                           26 ' 2. TRANSACTION SET PURP'
     O                                           47 'OSE CODE             '
     O                       FRMTC               53
     O*
     O                                          123 ' 5. AGENCY CLAIM NUMBER '
     O                       FRACL#             173
     O          E            DET            1
     O                                           26 ' 3. TRANSACTION SET DATE'
     O                       FRTDT               61 '    /  /  '
      *
     O          E            DET         1  1
     O                                           26 'CLAIM ADMINISTRATION    '
     O                                          123 'INSURED                 '
     O          E            DET            1
     O                                           26 ' 6. CLAIM ADMINISTRATOR '
     O                                           47 'CODE                 '
     O                       FRCLCD              60
     O                                          123 '16. EMPLOYER CODE       '
     O                       FREMP#             158
     O          E            DET            1
     O                                           26 ' 7. CLAIM ADMINISTRATOR '
     O                                           47 'NAME                 '
     O                       FRCLSN              81
     O                                          123 '17. INSURED NAME        '
     O                       FRILSN             178
     O          E            DET            1
     O                                           26 ' 8. INDEPENDENT ADJUSTOR'
     O                                           47 ' CODE                '
     O                       FRPADC              60
     O                                          123 '18. EMPLOYER NAME       '
     O                       FRELSN             178
     O          E            DET            1
     O                                           26 ' 9. INDEPENDENT ADJUSTOR'
     O                                           47 ' NAME                '
     O                       FRPLSN              81
     O                                          123 '19. EMPLOYER ADDRESS, LI'
     O                                          144 'NE 1                 '
     O                       FREAD1             178
     O          E            DET            1
     O                                           26 '10. CLAIM ADMINISTRATOR '
     O                                           47 'ADDRESS, LINE 1      '
     O                       FRAAD1              81
     O                                          123 '20. EMPLOYER ADDRESS, LI'
     O                                          144 'NE 2                 '
     O                       FREAD2             178
     O          E            DET            1
     O                                           26 '11. CLAIM ADMINISTRATOR '
     O                                           47 'ADDRESS, LINE 2      '
     O                       FRAAD2              81
     O                                          123 '21. EMPLOYER CITY       '
     O                       FRECIT             163
     O          E            DET            1
     O                                           26 '12. CLAIM ADMINISTRATOR '
     O                                           47 'CITY                 '
     O                       FRACIT              66
     O                                          123 '22. EMPLOYER STATE      '
     O                       FREST              150
     O          E            DET            1
     O                                           26 '13. CLAIM ADMINISTRATOR '
     O                                           47 'STATE                '
     O                       FRAST               53
     O                                          123 '23. EMPLOYER POSTAL CODE'
     O                       FREZP              158
     O          E            DET            1
     O                                           26 '14. CLAIM ADMINISTRATOR '
     O                                           47 'POSTAL CODE          '
     O                       FRAZP               61
     O                                          123 '24. SELF-INSURED INDICAT'
     O                                          144 'OR                   '
     O                       FRSFIN             149
     O          E            DET            1
     O                                           24 '15. CLAIM ADMINISTRATO'
     O                                           44 'R''S CLAIM NUMBER    '
     O                       FRACLC              76
     O                                          123 '25. SIC CODE            '
     O                       FRSIC              154
     O                       XXSIC              185
     O          E            DET            1
     O                                          123 '26. INSURED REPORT NUMBE'
     O                                          144 'R                    '
     O                       FRRPT#             158
     O          E            DET            1
     O                                          123 '27. INSURED LOCATION NUM'
     O                                          144 'BER                  '
     O                       FRLOC#             163
      *
     O          E            DET         1  1
     O                                           26 'POLICY                  '
     O          E            DET            1
     O                                           26 '28. POLICY NUMBER       '
     O                       FRPOL#              81
     O          E            DET            1
     O                                           26 '29. POLICY EFFECTIVE    '
     O                       FRPEFD              61 '    /  /  '
     O          E            DET            1
     O                                           26 '29. POLICY EXPIRATION   '
     O                       FRPEXD              61 '    /  /  '
      *
     O          E            DET         1  1
     O                                           26 'ACCIDENT                '
     O          E            DET            1
     O                                           26 '31. DATE OF INJURY      '
     O                       FRACDT              61 '    /  /  '
     O          E            DET            1
     O                                           26 '32. TIME OF INJURY      '
     O                       FRACTM              56 '  :  '
     O          E            DET            1
     O                                           26 '33. POSTAL CODE OF INJUR'
     O                                           47 'Y                    '
     O                       FRAZP1              61
     O          E            DET            1
     O                                           25 '34. EMPLOYER''S PREMISES'
     O                                           46 ' INDICATOR           '
     O                       FREADR              52
     O          E            DET            1
     O                                           26 '35. NATURE OF INJURY COD'
     O                                           47 'E                    '
     O                       FRNATR              53
     O                       XXNATR              86
     O          E            DET            1
     O                                           26 '36. PART OF BODY INJURED'
     O                                           47 ' CODE                '
     O                       FRPART              53
     O                       XXPART              86
     O          E            DET            1
     O                                           26 '37. CAUSE OF INJURY CODE'
     O                       FRCAUS              53
     O                       XXCAUS              86
     O          E            DET            1
     O                                           26 '38. ACCIDENT DESCRIPTION'
     O                                           47 '/CAUSE               '
     O                       XXACD1             126
     O          E            DET            1
     O                       XXACD2             126
      *
     O          E            DET            1
     O                                           26 '39. INITIAL TREATMENT   '
     O                       FRITRT              53
     O                       XXITRT              85
     O          E            DET            1
     O                                           26 '40. DATE REPORTED TO EMP'
     O                                           47 'LOYER                '
     O                       FRDTRP              61 '    /  /  '
     O          E            DET            1
     O                                           26 '41. DATE REPORTED TO CLA'
     O                                           47 'IM ADMINISTRATOR     '
     O                       FRRCVD              61 '    /  /  '
      *
     O          E            DET         1  1
     O                                           26 'CLAIMANT                '
     O                                          123 'EMPLOYMENT              '
     O          E            DET            1
     O                                           26 '42. SOCIAL SECURITY NUMB'
     O                                           47 'ER                   '
     O                       FRSS#               60
     O                                          123 '58. EMPLOYMENT STATUS CO'
     O                                          144 'DE                   '
     O                       FRESTS             150
     O                       XXESTS             181
     O          E            DET            1
     O                                           26 '43. CLAIMANT LAST NAME  '
     O                       FRLSN               81
     O                                          123 '59. CLASS CODE          '
     O                       FRCLAS             152
     O          E            DET            1
     O                                           26 '44. CLAIMANT FIRST NAME '
     O                       FRFSN               66
     O                                          123 '60. OCCUPATION DESCRIPTI'
     O                                          144 'ON                   '
     O                       FROCC              178
     O          E            DET            1
     O                                           26 '45. CLAIMANT MIDDLE INIT'
     O                                           47 'IAL                  '
     O                       FRMID               52
     O                                          123 '61. DATE OF HIRE        '
     O                       FRDOH              158 '    /  /  '
     O          E            DET            1
     O                                           26 '46. CLAIMANT ADDRESS, LI'
     O                                           47 'NE 1                 '
     O                       FRADR1              81
     O                                          123 '62. WAGE                '
     O                       FRAWW         4    160
     O          E            DET            1
     O                                           26 '47. CLAIMANT ADDRESS, LI'
     O                                           47 'NE 2                 '
     O                       FRADR2              81
     O                                          123 '63. WAGE PERIOD         '
     O                       FRWGPD             150
     O          E            DET            1
     O                                           26 '48. CLAIMANT CITY       '
     O                       FRCTY               66
     O                                          123 '64. NUMBER DAYS WORKED  '
     O                       FRDAYS             149
     O          E            DET            1
     O                                           26 '49. CLAIMANT STATE      '
     O                       FRST                53
     O                                          123 '65. DATE LAST DAY WORKED'
     O                       FRDTLW             158 '    /  /  '
     O          E            DET            1
     O                                           26 '50. CLAIMANT POSTAL CODE'
     O                       FRZIP               60
     O                                          123 '66. FULL WAGES PD FOR DO'
     O                                          144 'I INDICATOR          '
     O                       FRFSAL             149
     O          E            DET            1
     O                                           26 '51. CLAIMANT PHONE      '
     O                       FRPHN#              61
     O                                          123 '67. SALARY CONTINUED IND'
     O                                          144 'ICATOR               '
     O                       FRCSAL             149
     O          E            DET            1
     O                                           26 '52. DATE OF BIRTH       '
     O                       FRDTOB              61 '    /  /  '
     O                                          123 '68. DATE OF RETURN TO WO'
     O                                          144 'RK                   '
     O                       FRRTW              158 '    /  /  '
     O          E            DET            1
     O                                           26 '53. GENDER CODE         '
     O                       FRSEX               52
     O                       XXSEX               85
     O          E            DET            1
     O                                           26 '54. MARITAL STATUS CODE '
     O                       FRMSTS              52
     O                       XXMSTS              85
     O          E            DET            1
     O                                           26 '55. NUMBER OF DEPENDENTS'
     O                       FRDEP               53
     O          E            DET            1
     O                                           26 '56. DATE DISABILITY BEGA'
     O                                           47 'N                    '
     O                       FRDBDT              61 '    /  /  '
     O          E            DET            1
     O                                           26 '57. DATE OF DEATH       '
     O                       FRDOD               61 '    /  /  '
     O          E            MSG         1
     O                                           98 '*** NO RECORDS MEET '
     O                                          119 'SELECTED CRITERIA ***'
     O**************************************************************************
     O*  EDI A49 AUDIT DETAIL REPORT
     O**************************************************************************
     OQSYSPRT1  E            ROUTE1         1 06
     O          E            ROUTE1         1
     O                                           15 'ROUTE TO:'
     O                       ORDIS1              60
     O          E            ROUTE1         1
     O                       ORDIS2              60
     O          E            ROUTE1         1
     O                       ORDIS3              60
     O          E            ROUTE1         2
     O                       ORDIS4              60
     O          E            ROUTE1         2
     O                                           19 'REPORT TITLE:'
     O                       X1TITL              80
     O          e            route1         1 59
     O                       #brnd1
     O          e            route1         1
     O                       #brnd2
     O*
     O          E            HEAD1          1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             118
     O                       DSDATE        Y    198
     O          E            HEAD1          2
     O                       Q1PGM               10
     O                       X1TITL             129
     O                                          192 'PAGE:'
     O                       PAGE1         Z    198
     O          E            HEAD1          2
     O                                           19 'JURISDICTION STATE:'
     O                       QQJST               22
     O                                           53 'EXTRACT SUBMISSION DATE:'
     O                       X1EXDT              64 '  /  /    '
     O                                           79 'ADJUSTER:'
     O                       XTADJ              122
     O*
     O          E            DET1           1
     O                                           26 'TRANSACTION             '
     O                                          123 'JURISDICTION            '
     O          E            DET1           1
     O                                           26 ' 1. TRANSACTION SET ID  '
     O                       SUTSET              54
     O                                          123 ' 4. JURISDICTION        '
     O                       SUJRST             150
     O          E            DET1           1
     O                                           26 ' 2. MAINTENANCE TYPE COD'
     O                                           47 'E                    '
     O                       SUMTC               53
     O          E            DET1           1
     O                                           26 ' 3. MAINTENANCE TYPE COD'
     O                                           47 'E DATE               '
     O                       SUTDT               61 '    /  /  '
      *
     O          E            DET1        1  1
     O                                           26 'CLAIM ADMINISTRATION    '
     O          E            DET1           1
     O                                           26 ' 6. INSURER FEIN        '
     O                                           47 '                     '
     O                       SUCLCD              60
     O*                                                                *********
     O*  Completely replaced section to redo field positions           *********
     O*                                                                *********
     O                                          123 '14. CLAIM ADMINISTRATOR '
     O                                          144 'POSTAL CODE          '
     O                       SUAZIP             157
     O          E            DET1           1
     O                                           26 ' 8. THIRD PARTY ADMINIST'
     O                                           47 'RATOR FEIN           '
     O                       SUFEIN              60
      *
     O          E            DET1        1  1
     O                                           26 'EMPLOYEE                '
     O          E            DET1           1
     O                                           26 '42. SOCIAL SECURITY NUMB'
     O                                           47 'ER                   '
     O                       SUSS#               60
     O                                          123 '70. DATE OF MAXIMUM MEDI'
     O                                          144 'CAL IMPROVEMENT      '
     O                       SUMAXI             158 '    /  /  '
     O          E            DET1           1
     O                                           26 '55. NUMBER OF DEPENDENTS'
     O                       SUDEP               53
     O                                          123 '71. RETURN TO WORK QUALI'
     O                                          144 'FIER                 '
     O                       SURETW             149
     O          E            DET1           1
     O                                           26 '69. PRE-EXISTING DISABIL'
     O                                           47 'ITY                  '
     O                       SUPRED              52
     O                                          123 '72. DATE OF RETURN/RELEA'
     O                                          144 'SE TO WORK           '
     O                       SURTW              158 '    /  /  '
     O          E            DET1           1
     O                                           26 '56. DATE DISABILITY BEGA'
     O                                           47 'N                    '
     O                       SUDBDT              61 '    /  /  '
     O                                          123 '57. EMPLOYEE DATE OF DEA'
     O                                          144 'TH                   '
     O                       SUEDTD             158 '    /  /  '
      *
     O          E            DET1        1  1
     O                                           26 'WAGE                    '
     O          E            DET1           1
     O                                           26 '62. WAGE                '
     O                       SUWAGE              62
     O                                          123 '64. NUMBER OF DAYS WORKE'
     O                                          144 'D                    '
     O                       SUDAYS             149
     O          E            DET1           1
     O                                           26 '63. WAGE PERIOD         '
     O                       SUWGPD              53
     O                                          123 '67. SALARY CONTINUED IND'
     O                                          144 'ICATOR               '
     O                       SUCSAL             149
      *
     O          E            DET1        1  1
     O                                           26 'ACCIDENT                '
     O          E            DET1           1
     O                                           26 '31. DATE OF INJURY      '
     O                       SUACDT              61 '    /  /  '
     O                                          123 '15. CLAIM ADMINISTRATOR '
     O                                          144 'CLAIM NUMBER         '
     O                       SUACLC             173
     O          E            DET1           1
     O                                           26 '26. INSURED REPORT NUMBE'
     O                                           47 'R                    '
     O                       SURPT#              76
     O                                          123 ' 5. AGENCY CLAIM NUMBER '
     O                       SUACL#             173
      *
     O          E            DET1        1  1
     O                                           26 'CLAIM STATUS            '
     O          E            DET1           1
     O                                           26 '73. CLAIM STATUS        '
     O                       SUCSTS              52
     O                                          123 '75. AGREEMENT TO COMPENS'
     O                                          144 'ATE CODE             '
     O                       SUACCD             149
     O          E            DET1           1
     O                                           26 '74. CLAIM TYPE          '
     O                       SUCTYP              52
     O                                          123 '76. DATE OF REPRESENTATI'
     O                                          144 'ON                   '
     O                       SURPDT             158 '    /  /  '
     O*                                                                *********
     O          E            DET1        1  1
     O                                           26 'PAYMENTS                '
     O          E            DET1           1
     O                                           26 '77. LATE REASON CODE    '
     O                       SULATC              53
     O*                                                                *********
     O          E            DET1        1  1
     O                                           26 'VARIABLE SEGMENT COUNTS '
     O          E            DET1           1
     O                                           26 '78. NUMBER OF PERMANENT '
     O                                           47 'IMPAIRMENTS          '
     O                       SU#PI               60
     O                                          123 '81. NUMBER OF PAID TO DA'
     O                                          144 'TES/REDUCED EARNINGS/'
     O                                          154 'RECOVERIES'
     O                       SU#RER             173
     O          E            DET1           1
     O                                           26 '79. NUMBER OF PAYMENTS/A'
     O                                           47 'DJUSTMENTS           '
     O                       SU#PA               60
     O                                          123 '82. NUMBER OF DEATH DEPE'
     O                                          144 'NDENT/PAYEE RELATIONS'
     O                                          148 'HIPS'
     O                       SU#DDR             173
     O          E            DET1           1
     O                                           26 '80. NUMBER OF BENEFIT AD'
     O                                           47 'JUSTMENTS            '
     O                       SU#BA               60
      *
     O          E            SUBHPI      2  1
     O                                           26 'PERMANENT IMPAIRMENTS   '
     O          E            SUBHPI         0
     O                                           28 '83. BODY PART CODE AND D'
     O                                           38 'ESCRIPTION'
     O                                           68 '84. IMPAIRMENT PERCENT'
     O          E            SUBHPI         2
     O                                           28 '________________________'
     O                                           41 '_____________'
     O                                           68 '______________________'
     O          E            SUBDPI         1
     O                       PIIBPC               8
     O                                           10 '-'
     O                       XXBPDS              41
     O                       PIIMPP        4     60
     O                                           62 '%'
      *
     O          E            SUBHPA      2  1
     O                                           26 'PAYMENT/ADJUSTMENTS     '
     O          E            SUBHPA         0
     O                                           12 '85. CODE'
     O                                           33 '86. PAID TO DATE'
     O                                           55 '87. WEEKLY AMOUNT'
     O                                           74 '88. START DATE'
     O                                           91 '89. END DATE'
     O                                          110 '90. WEEKS PAID'
     O                                          128 '91. DAYS PAID'
     O          E            SUBHPA         2
     O                                           12 '________'
     O                                           33 '________________'
     O                                           55 '_________________'
     O                                           74 '______________'
     O                                           91 '____________'
     O                                          110 '______________'
     O                                          128 '_____________'
     O          E            SUBDPA         1
     O                       AJPYAJ               9
     O                       AJPADT        J     31
     O                       AJAWAM        J     53
     O                       AJPSDT              72 '    /  /  '
     O                       AJPEDT              90 '    /  /  '
     O                       AJAWPD        4    106
     O                       AJADPD        4    122
      *
     O          E            SUBHBA      2  1
     O                                           26 'BENEFIT ADJUSTMENTS     '
     O          E            SUBHBA         0
     O                                           28 '92. BENEFIT ADJ CODE AND'
     O                                           40 ' DESCRIPTION'
     O                                           79 '93. BENEFIT ADJ WEEKLY A'
     O                                           84 'MOUNT'
     O                                          113 '94. BENEFIT ADJ START DA'
     O                                          115 'TE'
     O          E            SUBHBA         2
     O                                           28 '________________________'
     O                                           51 '_______________________'
     O                                           79 '________________________'
     O                                           84 '_____'
     O                                          113 '________________________'
     O                                          115 '__'
     O          E            SUBDBA         1
     O                       RDBACD               8
     O                                           10 '-'
     O                       XXBADS              51
     O                       RDWAAM        J     75
     O                       RDASDT             107 '    /  /  '
     O*
     O          E            SUBHPD      2  1
     O                                           26 'PTD/REDUCED EARNINGS/REC'
     O                                           33 'OVERIES'
     O          E            SUBHPD         0
     O                                           28 '95. PTD CODE AND DESCRIP'
     O                                           40 'TION        '
     O                                           69 '96. PTD AMOUNT'
     O          E            SUBHPD         2
     O                                           28 '________________________'
     O                                           51 '_______________________'
     O                                           69 '______________'
     O          E            SUBDPD         1
     O                       T2CODE               8
     O                                           10 '-'
     O                       XXPDDS              51
     O                       T2AMT         J     70
     O*
     O          E            SUBHDP      2  1
     O                                           26 'DEATH DEPENDENTS        '
     O          E            SUBHDP         0
     O                                           28 '97. DEPENDENT/PAYEE RELA'
     O                                           40 'TIONSHIP    '
     O                                           73 'NUMERICAL BIRTH ORDER'
     O          E            SUBHDP         2
     O                                           28 '________________________'
     O                                           50 '______________________'
     O                                           73 '_____________________'
     O          E            SUBDDP         1
     O                       T3REL                7
     O                                            9 '-'
     O                       XXDPRL              50
     O                       T3BODR              55
      *
     O          E            MSG1        1
     O                                           98 '*** NO RECORDS MEET '
     O                                          119 'SELECTED CRITERIA ***'
     O**************************************************************************
     O*  EDI 148 ERROR REJECTION REPORT
     OQSYSPRT2  E            ROUTE2         1 06
     O          E            ROUTE2         1
     O                                           15 'ROUTE TO:'
     O                       ORDIS1              60
     O          E            ROUTE2         1
     O                       ORDIS2              60
     O          E            ROUTE2         1
     O                       ORDIS3              60
     O          E            ROUTE2         2
     O                       ORDIS4              60
     O          E            ROUTE2         2
     O                                           19 'REPORT TITLE:'
     O                       X2TITL              80
     O          e            route2         1 59
     O                       #brnd1
     O          e            route2         1
     O                       #brnd2
     O*
     O          E            HEAD2          1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             118
     O                       DSDATE        Y    198
     O          E            HEAD2          2
     O                       Q1PGM               10
     O                       X2TITL             129
     O                                          192 'PAGE:'
     O                       PAGE2         Z    198
     O          E            HEAD2          2
     O                                           19 'JURISDICTION STATE:'
     O                       QQJST               22
     O                                           53 'EXTRACT SUBMISSION DATE:'
     O                       X1EXDT              64 '  /  /    '
     O                                           79 'ADJUSTER:'
     O                       XTADJ              122
     O          E            HEAD2          1
     O                                          115 'SUBMISSION'
     O                                          124 ' LOG  '
     O          E            HEAD2          1
     O                                           18 'CLAIM ADM CLAIM#'
     O                                           37 'AGENCY CLAIM#'
     O                                           65 'CLAIMANT NAME'
     O                                           96 'ACCIDENT DT'
     O                                          102 'MTC'
     O                                          115 ' DUE DATE '
     O                                          124 'STATUS'
     O                                          129 'DN#'
     O                                          142 'DESCRIPTION'
     O                                          166 'ERR#'
     O                                          185 'ERROR DESCRIPTION'
     O          E            SUBH2       1  0
     O                       XXCLA#              21
     O                       XXACL#              49
     O                       XXCNAM              82
     O                       XXACDT              95 '  /  /    '
     O                       XXMTC              101
     O                       XXMTCD             115 '  /  /    '
     O                       LGSTS              122
     O*
     O          E            DET2           1
     O                       XXDN#         4    129
     O                       XXWK30             161
     O                       XXERR#        4    165
     O                       XXEDSC             198
     O          E            MSG2        1
     O                                           98 '*** NO RECORDS MEET '
     O                                          119 'SELECTED CRITERIA ***'
     O*
     O**************************************************************************
     O*  EDI A49 ERROR REJECTION REPORT
     OQSYSPRT3  E            ROUTE3         1 06
     O          E            ROUTE3         1
     O                                           15 'ROUTE TO:'
     O                       ORDIS1              60
     O          E            ROUTE3         1
     O                       ORDIS2              60
     O          E            ROUTE3         1
     O                       ORDIS3              60
     O          E            ROUTE3         2
     O                       ORDIS4              60
     O          E            ROUTE3         2
     O                                           19 'REPORT TITLE:'
     O                       X3TITL              80
     O          e            route3         1 59
     O                       #brnd1
     O          e            route3         1
     O                       #brnd2
     O*
     O          E            HEAD3          1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             118
     O                       DSDATE        Y    198
     O          E            HEAD3          2
     O                       Q1PGM               10
     O                       X3TITL             129
     O                                          192 'PAGE:'
     O                       PAGE3         Z    198
     O          E            HEAD3          2
     O                                           19 'JURISDICTION STATE:'
     O                       QQJST               22
     O                                           53 'EXTRACT SUBMISSION DATE:'
     O                       X1EXDT              64 '  /  /    '
     O                                           82 'ADJUSTER:'
     O                       XTADJ              123
     O          E            HEAD3          1
     O                                          115 'SUBMISSION'
     O                                          124 ' LOG  '
     O          E            HEAD3          1
     O                                           18 'CLAIM ADM CLAIM#'
     O                                           37 'AGENCY CLAIM#'
     O                                           65 'CLAIMANT NAME'
     O                                           96 'ACCIDENT DT'
     O                                          102 'MTC'
     O                                          115 ' DUE DATE '
     O                                          124 'STATUS'
     O                                          129 'DN#'
     O                                          142 'DESCRIPTION'
     O                                          166 'ERR#'
     O                                          185 'ERROR DESCRIPTION'
     O          E            SUBH3       1  0
     O                       XXCLA#              21
     O                       XXACL#              49
     O                       XXCNAM              82
     O                       XXACDT              95 '  /  /    '
     O                       XXMTC              101
     O                       XXMTCD             115 '  /  /    '
     O                       LGSTS              122
     O*
     O          E   N25      DET3           1
     O                       XXDN#         4    129
     O                       XXWK30             161
     O                       XXERR#        4    165
     O                       XXEDSC             198
     O          E    25      DET3           1
     O                       XCE148             195
     O          E            MSG3        1
     O                                           98 '*** NO RECORDS MEET '
     O                                          119 'SELECTED CRITERIA ***'
     O*
     O**************************************************************************
     O*  EDI 148/A49 SUMMARY REPORT
     OQSYSPRT4  E            ROUTE4         1 06
     O          E            ROUTE4         1
     O                                           15 'ROUTE TO:'
     O                       ORDIS1              60
     O          E            ROUTE4         1
     O                       ORDIS2              60
     O          E            ROUTE4         1
     O                       ORDIS3              60
     O          E            ROUTE4         2
     O                       ORDIS4              60
     O          E            ROUTE4         2
     O                                           19 'REPORT TITLE:'
     O                       X4TITL              80
     O          e            route4         1 59
     O                       #brnd1
     O          e            route4         1
     O                       #brnd2
     O*
     O          E            HEAD4          1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             118
     O                       DSDATE        Y    198
     O          E            HEAD4          2
     O                       Q1PGM               10
     O                       X4TITL             129
     O                                          192 'PAGE:'
     O                       PAGE4         Z    198
     O          E            HEAD4          2
     O                                           19 'JURISDICTION STATE:'
     O                       QQJST               22
     O                                           53 'EXTRACT SUBMISSION DATE:'
     O                       X1EXDT              64 '  /  /    '
     O*
     O          E    83      DET4        3  2
     O                                           20 'TOTAL 148 RECORDS'
     O                                           30 'ACCEPTED:'
     O                       XXACPT        3     41
     O          E    83      DET4           2
     O                                           20 'TOTAL 148 RECORDS'
     O                                           30 'REJECTED:'
     O                       XXREJ         3     41
     O          E    83      DET4           2
     O                                           21 'TOTAL 148 RECORDS:'
     O                       XX148T        3     41
     O          E    84      DET4        2  2
     O                                           20 'TOTAL A49 RECORDS'
     O                                           30 'ACCEPTED:'
     O                       X9ACPT        3     41
     O          E    84      DET4           2
     O                                           20 'TOTAL A49 RECORDS'
     O                                           30 'REJECTED:'
     O                       X9REJ         3     41
     O          E    84      DET4           2
     O                                           21 'TOTAL A49 RECORDS:'
     O                       XXA49T        3     41
     O          E            DET4        2  2
     O                                           20 'GRAND TOTAL:   '
     O                       X4CNT         3     41
