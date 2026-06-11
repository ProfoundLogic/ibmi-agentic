     H Copyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H bnddir('WTBNDDIR')
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE W7265R - CHANGE PROTECTED CLAIM VALUES
     H*****************************************************************
     H* PROGRAMMER NOTES:                                             *
     H*    THIS PROGRAM HAS CODE THAT WILL INTERFACE WITH THE FEE     *
     H*    SCHEDULE SYSTEM TO TRANSFER A CLAIM THROUGHOUT VARIOUS     *
     H*    FEE SCHEDULE FILES IN ADDITION TO THE WCMS FILES ALREADY   *
     H*    HANDLED BY THIS PROGRAM.  IF THIS PROGRAM IS BEING USED    *
     H*    TO CREATE A MOD VERSION FOR A NEW CLIENT WITH FEE SCHEDULE *
     H*    INTERFACE, JOB DESCRIPTIONS AND DATA AREAS MAY NEED TO BE  *
     H*    UPDATED/CREATED TO WORK WITH BOTH FS AND WCMS FILES.       *
     H*    FS TRANSFER PROCESS IS ONLY CALLED IF USER'S CHGS RESULT   *
     H*    IN A CHANGE IN FYR OR CASE. IT IS NOT CALLED IF CLAIM      *
     H*    TRANSFER IS BEING USED TO CHANGE EMP#, CLASS CODE ETC.     *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  01/14/25    C:340  Project WC2995                            *
     H*                     Recompile Only;                           *
     H*                                                               *
     H*  05/23/24 C:300 Support: 115634 (SUNZ)                        *
     H*                 Update WMFNP Maintenance fields while updating*
     H*                 Next Available Claim Number                   *
     h*                                                               *
     H*  12/18/20 C:180 Support 133/93297                             *
     H*                 Change IDC policy id to use pol year          *
     H*                 instead of division.                          *
     H*                                                               *
     H*  10/14/20 C:079 Support: 146/91816                            *
     H*                 Change IDC processing for changes to Emp#/Div *
     H*                                                               *
     H*  06/09/20 C:298 Project: SY420                                *
     H*                 Mask SSN                                      *
     H*                                                               *
     H*  08/23/19 C:180 Project: WC2604                               *
     H*                 Add IDC processing.                           *
     H*                                                               *
     h*  07/25/16 C:136 Project: 114WYQ                               *
     h*                 Add Relay Messenger processing                *
     h*                                                               *
     h*  05/09/16 C:300 Project: 114WXV                               *
     h*                 Add Cell Number to Claimant Demographics.     *
     H*                                                               *
     H*                                                               *
     H*  09/10/15 C:180 Support: PHW36930                             *
     H*                 Update SADAP file on transfer to existing     *
     H*                 claim.                                        *
     H*                                                               *
     H*  09/08/15 C:062 Support: 37101(99)                            *
     H*                 Trigger transfer if zipcode changes           *
     H*  09/24/14 C:062 Support: WC129 86840                          *
     H*                 Remove WDELL lock                             *
     H*                                                               *
     H*  12/27/11 C:282 Project: WC2005                               *
     H*                Add API WA085R to change EFT payee name/FEIN   *
     H*                                                               *
     H*  10/20/11 C:278  PROJECT: 133WLW PRISS#28                     *
     H*                  The Dart Remark sometimes retain the         *
     H*                  previous claimant address if more than one   *
     H*                  person is changed in a row.                  *
     H*                                                               *
     H*  10/04/11 C:279  Project: 133WLX                              *
     H*                  Changed to update all claims for same SS# if *
     H*                  name is changed with DARTS remarks.          *
     H*
     H*  10/03/11 C:278  Project: 133WLW                              *
     H*                  The Dart Remark will be updated to           *
     H*                  include the claimant's old address, along    *
     H*                  with the reason for the change.              *
     H*                                                               *
     H*  12/13/10 C:198   Project: WC1963                             *
     H*                   Update HICN#.                               *
     H*                                                               *
     H*  10/20/10 C:220   PROJECT: WC1933                             *
     H*                   Implement change of 67952 as per QAer It    *
     H*                   should be consistant.                       *
     H*                   Do not allow periods in First and Last name *
     H*                                                               *
     H* 03/10/10 C:242 Project: 25WRK                                 *
     H*                  Retrofit 25WJW                               *
     H*               Added code to update the Claim Index file when  *
     H*               appropriate.                                    *
     H*                                                               *
     H*  01/04/10 C:190   PROJECT: 25WRE                              *
     H*                   Implement change to calculate total         *
     H*                   number of claims.                           *
     H*                                                               *
     H* 10/05/09 C:33  Project: WC1770                                *
     H*                Add 'Medical Beneficiary' flag.                *
     H*                                                               *
     H* 07/14/09 C:189 Project: 114WOJ                                *
     H*                  Change of Address Tracking/Force new Darts   *
     H*                  Remark                                       *
     H*                                                               *
     H* 04/20/09 C:190 PROJECT WC1473                                 *
     H*                Use the new logical(s) for editing class       *
     H*                code information.                              *
     H*                                                               *
     H* 01/03/08 C:15  Support: WC135/60772                           *
     H*                Chgd code to not update WDDDP records if a     *
     H*                claim is transferred to another emp/div within *
     H*                the same PEO, for clt135.                      *
     H*                                                               *
     H* 08/22/07 C:33  Project: 114WMF                                *
     H*                Back to Base.                                  *
     H*       10/14/02 Project: 114WBY  seq# 34365                    *
     H*                Added code to update WMCRP.                    *
     H*                                                               *
     H* 06/07/07 C:15  Support: WC132/58275                           *
     H*                Added code to clear CMDEPT if a claim is       *
     H*                transferred from one emp/div to another.       *
     H*                                                               *
     H* 05/25/07 C:191 Project: WC1515                                *
     H*                Update cliamant name in claims security file   *
     H*                                                               *
     H* 05/15/07 C:180 Support: WC139/57297
     H*                Display specific error message on Window.
     H*
     H*
     H* 08/23/06 C:15  Support: WC114/53679                           *
     H*                Chgd code to update the seq# in WDRPP when a   *
     H*                claim is transferred into another claim.       *
     H*                                                               *
     H* 01/05/05 C:15  Support: WC129/45312                           *
     H*                Chgd code to use the correct format for the    *
     H*                claimant's name when selecting request pmts    *
     H*                to change when the claimant's name or address  *
     H*                changes.  Also, added code to change request   *
     H*                pmts for other claims with the same SS#, if    *
     H*                the claimant's name or address changes.        *
     H*                                                               *
     H* 03/09/04 C:15  Support: WC065/41604                           *
     H*                Added code to pass the 'from' claim year to    *
     H*                W8024R so the A/R billing adjustments are      *
     H*                written with the correct year.                 *
     H*                                                               *
     H* 01/09/04 C:15  Support: WC126/40664                           *
     H*                Added code for employer# search on SCR02.      *
     H*                                                               *
     H* 12/10/03 C:15  Project: WC1172                                *
     H*                Added code to load the Tot Inc, Rsv chg amt,   *
     H*                and Paid amt into WACTP on transferred claims. *
     H*                                                               *
     H* 11/14/03 C:62  Project: WC909                                 *
     H*                Change parms for F7095R                        *
     H*                                                               *
     H* 10/20/03 C:53  Project: WC909                                 *
     H*                Added country info.                            *
     H*                                                               *
     H* 12/30/02 C:62 Support: WC095/35194                            *
     H*               Pass the "To" case rrn to F7090R.               *
     H*                                                               *
     H* 09/04/02 C:15 Support: WC065/340XX                            *
     H*               Fixed the parms on the call to the EDI pgm that *
     H*               updates the log records when the FYR changes.   *
     H*                                                               *
     H* 08/09/02 C:15 Support: WC045/33707                            *
     H*               Fixed the policy coverage override edit.  It    *
     H*               was allowing users to select an accident date   *
     H*               that was not within a policy date range.        *
     H*                                                               *
     H* 02/22/01 C:15 Support: WC094/26470                            *
     H*               Fixed the call to the Dup SS# search.  The pgm  *
     H*               was not displaying any claims.                  *
     H*                                                               *
     H* 11/20/00 C:15 Support: WC065/25133                            *
     H*               Added code to update WDRPP and WDACP if the FYR *
     H*               is changed on a claim.                          *
     H*                                                               *
     H* 11/19/00 C:15 Support: WC094/25014                            *
     H*               Pgm was locking up if user did not override the *
     H*               policy edit on the accident date.  Fixed code.  *
     H*                                                               *
     H* 09/21/00 C:15 Support: WC025/24427                            *
     H*               Pgm was not updating SS# when the SS# already   *
     H*               existed on another claim.  Fixed code.          *
     H*                                                               *
     H* 08/18/00 C:15 Support: WC065/23588                            *
     H*               Chgd pgm to not update the maint fields in      *
     H*               various files.                                  *
     H*                                                               *
     H*  04/27/00 C:53  SUPPORT: WC065/19768
     H*                 SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
     H*                                                               *
     H*  04/26/00 C:31  Project: DR097
     H*                 Chg ALL PDS pgms
     H*                 1. Replace write to SDCMP with API DA700R
     H*                                                               *
     H* 01/20/00 C:15 Support: WC065/21088                            *
     H*               Changed the claim FYR calculation and added     *
     H*               code to allow authorized users to override the  *
     H*               policy coverage edits.                          *
     H*                                                               *
     H* 10/21/99 C:53 Project: WCMNT                                  *
     H*               REMOVE PCR01 DUE TO REMOVAL OF TABLE FILE
     H*                                                               *
     H* 08/04/99 C:58 Project: WC855                                  *
     H*               Change name and/or address in Payment Request
     H*               file (WARPLC) also.
     H*                                                               *
     H* 07/01/99 C:118 SUPPORT: DR095/13079,DR048/13846,DR087/16339   *
     H*                              DR048/17294                      *
     H*                Added FEOD SDCM* to WRITEPCM0* so DARTS updates*
     H*                immediately.                                   *
     H*                                                               *
     H*  06/30/99 C:15 Support: WC025/18666                           *
     H*                Added code to check the SS# for alphanumeric   *
     H*                characters before loading into a numeric field *
     H*                to prevent data decimal errors.                *
     H*
     H*  04/26/99 C:118 PROJECT: WC577
     H*                 Modified display file to match V3 standards
     H*
     H*  03/08/99 C:15 Support: WC060/16867
     H*                Added code to change the claimant name fields
     H*                in the claim file if the SS# is changed and
     H*                another claim exists with that SS#.
     H*
     H*  02/01/99 C:15 Support: WC048/16346
     H*                Added edit to SCR02 to require the SS# (this
     H*                field should not be blank).
     H*
     H*  01/04/99 C:15 Support: WC091/15655
     H*                The parms to the DARTS transfer pgm were only
     H*                being loaded if WCMC was installed.  Changed
     H*                pgm to load the parms in all situations.
     H*
     H*  12/03/98 C:94 PROJECT: SUP15351
     H*                CHANGE PARM Q1ACDT FROM 7 POS TO 8 POS DATE
     H*
     H*
     H*  11/20/98 C:94 PROJECT: SUP15110
     H*                CHANGE WHAT XYRRN2 IS LOADED WITH IN A SINGLE
     H*                CASE TRANSFER
     H*
     H*  11/11/98 C:94 PROJECT: MAINTENANCE
     H*                CHANGE UPDATE SO THAT IT WILL UPDATE IF SS#
     H*                IS NOT CHANGED
     H*
     H*  11/04/98 C:94 PROJECT: MAINTENANCE
     H*                CHANGE WHAT IS LOADED INTO FYR
     H*
     H*  10/30/98 C:94 PROJECT: SUPPORT 14737
     H*                REMOVE ##FLAG = X CONDITION ON SINGLE CASE
     H*                TRANSFER
     H*
     H*  10/12/98 C:94 PROJECT: SUPPORT
     H*                CONDITION TRANSFER OF WCMC RECORDS BASED ON IF
     H*                WCMC IS INSTALLED
     H*
     H*  09/16/98 C:94 Project: MC151
     H*                Add call to F7090R (claim transfer/deletion)
     H*                on Managed Care side
     H*
     H*  09/04/98 C:78 Project: WC832
     H*                add the following fields from WMCDP to SCR02:
     H*                area code (CDAREA), phone# (CDPHN#), marital
     H*                status (CDMSTS), county code (CDCNTY), work
     H*                area code (CDWARA), work phone# (CDWPH#).
     H*
     H*  09/02/98 C:58 Conversion issue RMSC--82WBV
     H*                Change arrays OLS and NES to 9999 to allow
     H*                the consolidation of claims with more than 99
     H*                payment history records.
     H*
     H*  08/11/98 C:15 Support: WC021/13107                           *
     H*                Changed pgm to use new API to determine if     *
     H*                WCMS is interfaced with WCMC on the client's   *
     H*                machine.  Also, removed call to W7265C since   *
     H*                all files are in the same libraries.           *
     H*                                                               *
     H*  06/25/98 C:33 SEQ#: WC033/12363                              *
     H*                AFTER USING THE ACCIDENT DATE TO GET THE POLICY*
     H*                YEAR FROM EMP DETAIL, LOAD SCYR FROM ELFYR, NOT*
     H*                ELPFDT                                         *
     H*                                                               *
     H*  05/28/98 C:33 SEQ#: WC095/11435                              *
     H*               -USE C1IDT INSTEAD OF CMIDT                     *
     H*               -CLEAR THE GROUP NAME BEFORE CHAINING           *
     H*                                                               *
     H*  05/09/98 C:78 PROJECT: WC577                                 *
     H*                ADD F10=SEARCH ON CLASS CODE IN SCR02.         *
     H*                                                               *
     H*  05/08/98 C:33 SUPPORT                                        *
     H*                USE THE SAME EDITS AS IN CLAIM MASTER.         *
     H*                                                               *
     H*  05/07/98 C:33 SEQ#: WC095/11435                              *
     H*                UPDATE THE NAME IN WMCMP.                      *
     H*                                                               *
     H*  05/06/98 C:33 SEQ#: WC091/11423                              *
     H*                USE CMIDT FOR THE INITIAL ENTRY DATE.          *
     H*                                                               *
     H*  04/30/98 C:33 SEQ#: WC999/11237                              *
     H*               -If a claim is transfered to another employer   *
     H*                and the accident occured on the employer       *
     H*                premisis, upate the place of accident.         *
     H*                SEQ#: WC060/11348                              *
     H*               -Use relax claim coverage flag to determine if  *
     H*                the class code is required.                    *
     H*                                                               *
     H*  04/16/98 C:15 Project: WC577                                 *
     H*                If a claim is transfered to another employer,  *
     H*                W8024R will be called to adjust deductible     *
     H*                totals if necessary.                           *
     H*                                                               *
     H*  03/25/98 C:15 Project: WC577                                 *
     H*               -Changed pgm to update all claims with the same *
     H*                SS# if the claimant name is changed.           *
     H*               -Removed code added on 11/11/97 (SS# change)    *
     H*                                                               *
     H*  03/16/98 C:15 Project: WC577                                 *
     H*                Chgd pgm to call W0563R instead of W0563R1     *
     H*                since the latter pgm does not exist.           *
     H*                                                               *
     H*  02/23/98 C:85 Project: WC577                                 *
     H*                CHANGED THE PROGRAM MOVED TO CMPNAM            *
     H*                FROM W7265R TO W0070R.                         *
     H*                CHANGED THE FIELD MOVED TO CMSMCD              *
     H*                FROM *BLANKS TO CTSMCD.                        *
     H*                                                               *
     H* 11/11/97 C:47 Project: WC577                                  *
     H*               IF SS# CHANGES, UPDATE THE CLAIMANT NAME FOR ALL*
     H*               CLAIMS (IN WMCMP) WITH THE SAME SS#.
     H*                                                               *
     H* 11/05/97 C:15 Project: WC729                                  *
     H*               Changed the format of date fields used in the   *
     H*               parm list for pgm FA002R from MDCY to CYMD.     *
     H*                                                               *
     H* 11/04/97 C:15 Project: WC729                                  *
     H*               Removed reference to FXCTP and added an API to  *
     H*               load data into FXCTP.                           *
     H*                                                               *
     H* 08/27/97 C:58   EDI: REMOVE WMEIL1                            *
     H*                                                               *
     H* 07/24/97 C:47 VERSION 3 UPDATES                               *
     H*          V301 BY-PASS PAYROLL COVERAGE EDITS IF CLAIMS ONLY   *
     H*               IS INSTALLED (WDF1P:F1CLMS=Y).                  *
     H*                                                               *
     H*          V302 GET YEAR FROM EMPLOYER DETAIL (WDELP),          *
     H*               NOT GROUP DETAIL (WDF1P).                       *
     H*               (UNDO 5/30/95 25WAR CHANGES)                    *
     H*                                                               *
     H*          V303 WC366: IF CASE# CHANGES AND CHECK RECONCILIATION*
     H*               IS INSTALLED, UPDATE THE CHECK REC FILE (WDCRP).*
     H*                                                               *
     H*          V304 USE STATE FILE (STSTP), NOT ARRAY ST.           *
     H*                                                               *
     H*          V305 USE CLAIM/DARTS CONTROL FILE, CLAIM NAME CHG    *
     H*               FIELD (WMCXP:CXRCNC) TO UPDATE DARTS RECORD     *
     H*               INSTEAD OF HARD CODING 'CLNM'.                  *
     H*                                                               *
     H*          V306 IF EMPLOYER IS CHANGED AND THE OFFICE# ASSIGNED *
     H*               TO THE NEW EMP IS NOT THE SAME AS THE OFFICE#
     H*               CURRENTLY ASSIGNED TO THE CLAIM, DSP WINDOW MSG
     H*               ASKING USER TO VERIFY THAT THE NEW OFFICE# SHOULD
     H*               BE ASSIGNED TO THE CLAIM.                       *
     H*                                                               *
     H*  01/29/93 C:09 PROJECT 99AAG                                  *
     H*                ADDED CALL TO DARTS RECORDS TRANSFER SUBMITTER *
     H*                PROGRAM (SC901C) TO TRANSFER DARTS REMARKS     *
     H*                ASSOCIATED WITH THE CLAIM TO BE TRANSFERRED.   *
     H*                                                               *
     H*  02/22/96 C:47 PROJECT: 68APG                                 *
     H*                FOR VALID SS# IN CLAIMANT DEMOGRAPHIC FILE:    *
     H*                IF THE SS# ON A CLAIM IS CHANGED, UPDATE ALL   *
     H*                RECORDS IN THE PAYMENT DETAIL FILE (WDPAP)     *
     H*                THAT HAVE THE SAME PAYEE FED ID# AS THE        *
     H*                OLD SS# IN THE CLAIMS MASTER.                  *
     H*                THESE RECORDS IN WDPAP WILL BE UPDATED         *
     H*                REGARDLESS OF WHETHER WE'RE DOING A TRANSFER   *
     H*                OR NOT BECAUSE IT'S NOT IN THE *IN70 CONDITION.*
     H*                                                               *
     H*  02/23/96 C:47 PROJECT: 68APE                                 *
     H*                IF YOU ARE CHANGING THE SS# TO ONE THAT ALREADY*
     H*                EXISTS, DISPLAY THAT CLAIMANT INFO IN A WINDOW.*
     H*                PRESS ENTER TO CONTINUE PROCESSING OR F12.     *
     H*                                                               *
     H*  05/21/96 C:33 PROJECT: 78WAI                                 *
     H*                ADDED DELETION CODE & DESC WITH A SEARCH TO    *
     H*                THE DELETION WINDOW.                           *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H* ***************************************************************
     H* 05/23/90 JJJ  CORRECTION TO ACCIDENT DATE EDIT, NEED TO READ  *
     H*               UNTIL ACTIVE RECORD IS FOUND, AND ALSO CHECK    *
     H*               FOR ANNIVERSARY RATING DATE.                    *
     H*                                                               *
     H* 11/06/90 KWK  ADDED TWO FIELDS TO CASE TRANSFER RECORD,       *
     H*               TOTAL CLAIM PAYMENT AMOUNT FOR CLAIM BEING      *
     H*               TRANSFERRED (CTAMT) AND CLAIM TRANSFER DATE     *
     H*               (CTTRDT)                                        *
     H*                                                               *
     H* 11/09/90 KWK  MODIFIED CODE EXECUTED WHEN CASE IS CHANGED     *
     H*               BUT NOT TRANSFERRED TO ALWAYS PERFORM EDITS.    *
     H*                                                               *
     H* 11/13/90 KWK  ADDED CODE TO PROPERLY PERFORM CLAIM SEARCH     *
     H*                                                               *
     H* 02/21/91 JJJ  PROJECT: WC045                                  *
     H*               - CLAIM TRANSFER MUST ALSO UPDATE MULT CLM FORM *
     H*                 RECORDS AND RECOVERY REQUEST RECORDS          *
     H*                                                               *
     H* 08/07/91 KWK  MOVED CODE THAT ACCUMULATES TOTAL CLAIM PAYMENT *
     H*               AMOUNT FOR CLAIM BEING TRANSFERRED (CTAMT) TO   *
     H*               ALSO HANDLE ACCIDENT DATE CHANGE RESULTING      *
     H*               IN A FUND YEAR CHANGE.                          *
     H*                                                               *
     H* 08/27/91 JJJ  PROJECT: WC062                                  *
     H*               - REVISED TO UPDATE WDELP WITH CORRECT CLM      *
     H*                 TOTALS IF EMPLOYER CHANGES                    *
     H*                                                               *
     H* 12/04/91 JJJ  PROJECT: WC099                                  *
     H*               - UPDATE RR FILE, NOT LT                        *
     H*                                                               *
     H* 04/17/92 LPK  PROJECT: AMG                                    *
     H*               ADDED CODE TO UPDATE WDRPP WHEN THE FYR OR CASE *
     H*               IS CHANGED.                                     *
     H*                                                               *
     H* 05/22/92 KRR  PROJECT: FS001                                  *
     H*               - ADDED CODE TO TRANSFER FEE SCHEDULE RECORDS   *
     H*               - ADDED SYSTEM CONTROL FILE CHECK TO SEE IF     *
     H*                 FSMS INSTALLED BEFORE DOING FSMS TRANFERS     *
     H*                                                               *
     H* 08/27/92 DVM  PROJECT: AQE                                    *
     H*               ADDED CODE TO VALIDATE CLAIMANT'S ACCIDENT DATE *
     H*               AGAINST THE PAYROLL CANCELLATION DATE (IF ONE   *
     H*               EXISTS).  CLAIMS CANNOT EXIST FOR A CLASS CODE  *
     H*               AFTER THE CANCELLATION OF THAT CLASS CODE.      *
     H*                                                               *
     H* 11/13/92 C:09 PROJECT 99AAG                                   *
     H*               HARD DELETED OLD CODE FOR FS INTERFACE          *
     H*               05/22/92 ADDED FOR PROJECT FS001                *
     H*               BASED ON CONDITION OF FS INTERFACE FLAG         *
     H*               (SSYLP/SYFBIF,SYFIIF) A RECORD WILL BE NOW      *
     H*               WRITTEN TO FXCTP AND A CALL WILL BE MADE TO     *
     H*               F7063C TO SUBMIT FS CASE TRANSFER.  ALSO,       *
     H*               REMOVED EDIT THAT CHECKS TO SEE IF OWNER IS     *
     H*               INCL/EXCL FROM COVERAGE BASED ON THE ACCIDENT   *
     H*               DATE PER REQUEST FROM JACKIE J.                 *
     H*                                                               *
     H* 02/02/93 C:09 PROJECT 99AAG                                   *
     H*               CLAIM TRANSFER FOR CLIENTS WITH WCMS/FSMS       *
     H*               INTERFACE WILL NOW BE ACCOMPLISHED BY ADDING    *
     H*               NECESSARY FEE SCHEDULE LIBRARIES BEFORE         *
     H*               TRANSFER PROCESS IS PERFORMED AT THE BEGINNING  *
     H*               OF THE PROGRAM AND RESTORING INITIAL USER       *
     H*               LIBRARY LIST ONCE TRANSFER IS COMPLETE.  A NEW  *
     H*               PROGRAM (W7265C) WILL BE CALLED FROM THIS       *
     H*               PROGRAM TO SET UP INTERACTIVE LIBRARY LIST AS   *
     H*               REQUIRED TO TRANSFER FOR WC AND FS FILES.       *
     H*               CHANGED CODE TO PROCESS FEE SCHEDULE CASE       *
     H*               TRANSFER INTERACTIVELY BY CALLING W7064C        *
     H*               INSTEAD OF W7063C.                              *
     H*               ADDED CALL TO DARTS RECORDS TRANSFER SUBMITTER  *
     H*               PROGRAM (SC901C) TO TRANSFER DARTS REMARKS      *
     H*               ASSOCIATED WITH THE CLAIM TO BE TRANSFERRED.    *
     H*                                                               *
     H* 03/17/93 C:09 PROJECT 99AAG                                   *
     H*               FIXED PROGRAM TO PREVENT RECORD LOCK BY SAME    *
     H*               JOB THAT WAS OCCURRING WHEN FSCSTR WAS CALLED   *
     H*               BEFORE UPDATE TO WMCMP.  ALSO, ADDED CALL TO    *
     H*               FSCSTR TO HANDLE CHANGE IN SOCIAL SECURITY #.   *
     H*                                                               *
     H* 09/02/94 C:28 PROJECT: 03BAW                                  *
     H*               - REVISED TO UPDATE WDNTP, TRACKING CLAIMANT    *
     H*                 NAME OR ADDRESS CHANGES. ALSO THE DARTS       *
     H*                 REMARK IS ADDED TO COMPLETE THE TRACKING      *
     H*                 PROCEDURE.                                    *
     H*                                                               *
     H*  10/19/94 C:28 PROJECT: 03BAW - REVISION                      *
     H*                WHEN WRITE TO DARTS OR TO CHANGE NAME TRACKING *
     H*                VERIFY IF SAVE FIELDS ARE NOT BLANKS.          *
     H*                                                               *
     H*  10/20/94 C:28 PROJECT: 03BAW - REVISION                      *
     H*                WHEN WRITE TO DARTS THE 'CLNM' RECORD :        *
     H*                 CHANGE 'MOVE' TO 'MOVEL'                      *
     H*                                                               *
     H* 11/16/94 C:09 PROJECT WC279                                   *
     H*               ON NON-COMMON EXPIRATION DATE POLICIES,         *
     H*               WHEN THE ACCIDENT DATE IS CHANGED WE NEED       *
     H*               TO BE USING EMPLOYER POLICY START AND END       *
     H*               DATES TO DETERMINE IF THERE WAS A FUND YEAR     *
     H*               CHANGE FOR A CLAIM.                             *
     H*                                                               *
     H* 05/30/95 C:28 PROJECT 25WAR                                   *
     H*               REMOVED MOD:   AQE  8/27/92                     *
     H*                              WC279  11/16/94                  *
     H*               ALSO REMOVED CONDITION TO LOADS ELT#CL          *
     H*                                                               *
     H* 07/25/95 C:24 PROJECT 25WAQ                                   *
     H*               UPDATE DEDUCTIBLE BILLING RECORDS WITH CHANGED  *
     H*               CASE NUMBER OR FUND YEAR.  DELETE DEDUCTIBLE    *
     H*               BILLING RECORDS IF THE EMPLOYER CHANGES, AND    *
     H*               RECORD THE BILLED AMOUNT IN THE HISTORY         *
     H*               RECORD.                                         *
     H*                                                               *
     H* 12/22/95 C:15 PROJECT: 25WDE                                  *
     H*              -ADDED CODE TO CHANGE OR DELETE RECORDS FROM     *
     H*               NEW FILES WHICH WERE CREATED FOR THE EDI        *
     H*               PROCESS.                                        *
     H*              -ADDED A WINDOW TO PROMPT THE USER FOR AN        *
     H*               EXPLANATION, IF A CLAIM IS TRANSFERRED.         *
     H*              -ADDED CODE TO WRITE A CLAIM HISTORY RECORD, IF  *
     H*               A CLAIM IS TRANSFERRED.                         *
     H*              -ADDED CODE TO CALL THE EDI STATE TRIGGER PGM.   *
     H*                                                               *
     H* 05/08/96 C:46 PROJECT: 25WDE                                  *
     H*              -CORRECTED PARMS THAT ARE PASSED TO THE EDI      *
     H*               CLAIM DELETION PROGRAM W0070R2.                 *
     H*              -ADDED THE EDI STATE FILE WMNIP AND UPDATED      *
     H*               THE KEY INFORMATION WHEN A FUND YEAR IS CHANGED *
     H*                                                               *
     H* 06/17/96 C:15  PROJECT: 25WAN                                 *
     H*                ADDED CODE TO RENAME THE CLAIM MASTER FIELDS   *
     H*                WHICH ARE ALSO USED IN THE DARTS FILE, BECAUSE *
     H*                THE CLAIM FIELDS WERE GETTING CHANGED BEFORE   *
     H*                THE CLAIM FILE WAS UPDATED.                    *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H* ***************************************************************
     H* INDICATORS:                                                   *
     H*                                                               *
     H*  70    ONE IF TRANSFERRING FROM ONE CLAIM NUMBER TO ANOTHER   *
     H*        CLAIM NUMBER                                           *
     H*                                                               *
     H* ***************************************************************
     FW7265F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMFNL     UF   E           K DISK
     F                                     INFDS(DBFDS1)
     FWMUPL     IF   E           K DISK
     FWMCMP     IF   E             DISK
     F                                     RENAME(PCM01:PCM0P)
     F                                     INFDS(DBFDS)
     FWMCML1    UF   E           K DISK
     F                                     infds(dsClm1)
     FWMCDL     UF A E           K DISK
     FWMRHL     UF   E           K DISK
     FWMDDL     UF   E           K DISK
     FWMCHL     UF   E           K DISK
     FWARPL     UF   E           K DISK
     FWMTHL     UF   E           K DISK
     FWMWHL     UF   E           K DISK
     FWMLPL     UF   E           K DISK
     FWDPAL     UF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PEM0P)
     FWMEPL1E   IF   E           K DISK
     FWMPXP     IF   E             DISK
     F                                     RENAME(PPX01:PPX02)
      *  RATE SET CODE FILE
     FWMRSL     IF   E           K DISK
      *
      *  EMPLOYER PAYROLL FOR CLASS CODE SEARCH
     FWMEPP     IF   E             DISK
     F                                     RENAME(PEP01:PEP0P)
     FWDELL     UF   E           K DISK
     F                                     INFDS(DBFDS2)
     FWACTP     O  A E           K DISK
     FWALTL2    UF   E           K DISK
     FWMRRL     UF   E           K DISK
     FWDRPL     UF   E           K DISK
     F                                     RENAME(PRP01:PRP03)
     FWDDDL     UF   E           K DISK
     F                                     RENAME(PDD01:PDD02)
     FWDNTP     O    E           K DISK
     FWDELL2    IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
      *
     FWDF1L     IF   E           K DISK
     FWDF1L1    IF   E           K DISK
     F                                     RENAME(PF101:PF102)
      *
     FWACML     O    E           K DISK
     F                                     RENAME(PCM01:PCM05)
     FWMAWL     UF   E           K DISK
     FWMBNL     UF   E           K DISK
     FWMBYL     UF   E           K DISK
     FWMNIL     UF   E           K DISK
     FWTGTL     IF   E           K DISK
      *
     FWDCRL8    UF   E           K DISK
     F*
     F*  OFFICE MASTER
     FWMOFL     IF   E           K DISK
     F*
     FWMCXL     IF   E           K DISK
     FSTSTL3    IF   E           K DISK
     F*                                                                *********
     FWMCMLB    UF   E           K DISK
     F                                     RENAME(PCM01:PCM0B)
      *
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PPA01)
     F                                     IGNORE(PID01)
     F                                     IGNORE(PRS01)
      *  COUNTY CODE FILE
     FWTCNP     IF   E             DISK
     F                                     INFDS(DBFDS3)
     F                                     RENAME(PCN01:PCN0P)
     FWDF2L     IF   E           K DISK
     FWMRPL4    IF   E           K DISK    RENAME(PRP01:PRP04) PREFIX(R4:2)
     FWMIDCTRL4 IF   E           K DISK    RENAME(PTR01:PTR04)
     FWMIDCQTL7 IF   E           K DISK    RENAME(PQT01:PQT07)
     FWAIDL1    UF   E           K DISK
     FWARPLC    UF   E           K DISK
     F                                     RENAME(PRP01:PRP02)
     FWDACL     UF   E           K DISK
     F*** Claims security file
     FWMCUL     UF   E           K DISK
     FWMCRL     UF   E           K DISK
     F                                     RENAME(PCR01:PCR02)
     F*     Diagnosis code info file.
     FSADAL3    UF   E           K DISK
      *

     d/copy sPrModule
     d/copy sPrRelay
      /copy sPrMask
      /copy sPrSecure
     d/copy WprIdc
     d isRlyEnbld      s               n
     d isSmsDsbld      s               n
     D isNewIdc        s               n   inz(*Off)
     D isIdcTrnOk      s               n   inz(*Off)
     D qqCo#1          s              3  0
     D qqCar1          s              5
     D qqPolId1        s             18
     D qqPolEfDt1      s              8
     D qqClmId1        s             12
     D qqAcDt1         s              8
     D qqCo#2          s              3  0
     D qqCar2          s              5
     D qqPolId2        s             18
     D qqPolEfDt2      s              8
     D qqClmId2        s             12
     D qqAcDt2         s              8
     D qqFyr2          S                   LIKE(ELfyr) INZ
     D CTXjrst         s                   LIKE(CMjrst)
     D strCaP#         S             25A   inz
     D intPos          S              5  0 INZ
     D OLS             S              5  0 DIM(9999)
     D NES             S              5  0 DIM(9999)
     D CS1             S              1    DIM(7) ASCEND
     D CS2             S              1    DIM(7) ASCEND
     D REQ             S              1    DIM(30)
     D AMT             S              1    DIM(9) ASCEND
     D DRT             S             21    DIM(1) CTDATA PERRCD(1)
     D EMG             S             55    DIM(6) CTDATA PERRCD(1)
     D*  first name last name array
     D OCD1            S              1    DIM(30)
     D                 DS
     D  QCLNEW                 1     81
     D  QCDRT4                 1     21
     D  QCLNAM                22     81
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS                  INZ
     D  RR                     1     90  2
     D                                     DIM(10)
     D  CMRR01                 1      9  2
     D  CMRR02                10     18  2
     D  CMRR03                19     27  2
     D  CMRR04                28     36  2
     D  CMRR05                37     45  2
     D  CMRR06                46     54  2
     D  CMRR07                55     63  2
     D  CMRR08                64     72  2
     D  CMRR09                73     81  2
     D  CMRR10                82     90  2
     D                 DS                  INZ
     D  AR                     1     90  2
     D                                     DIM(10)
     D  CMAR01                 1      9  2
     D  CMAR02                10     18  2
     D  CMAR03                19     27  2
     D  CMAR04                28     36  2
     D  CMAR05                37     45  2
     D  CMAR06                46     54  2
     D  CMAR07                55     63  2
     D  CMAR08                64     72  2
     D  CMAR09                73     81  2
     D  CMAR10                82     90  2
     D                 DS
     D  RT                     1     20
     D                                     DIM(10)
     D  CMRT01                 1      2
     D  CMRT02                 3      4
     D  CMRT03                 5      6
     D  CMRT04                 7      8
     D  CMRT05                 9     10
     D  CMRT06                11     12
     D  CMRT07                13     14
     D  CMRT08                15     16
     D  CMRT09                17     18
     D  CMRT10                19     20
     D                 DS
     D  QFKEY                  1     40
     D  QFCO#                  1      3  0
     D  QFFND                  4      6  0
     D  QFFYR                  7      9  0
     D  QFCAS                 10     16  0
     D                 DS
     D  QTKEY                  1     40
     D  QTCO#                  1      3  0
     D  QTFND                  4      6  0
     D  QTFYR                  7      9  0
     D  QTCAS                 10     16  0
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCFYR                  7      9  0
     D  QCCASE                10     16  0
     D                 DS
     D  XXSS#                  1      9
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9
     D                 DS                  INZ
     D  XXYCS                  1     15
     D  XXFYR                  1      3  0
     D  XXCASE                 4     10  0
     D  SVSEQ#                11     15  0
     D                 DS                  INZ
     D  XXYCS2                 1     15
     D  X1FYR                  1      3  0
     D  X1CASE                 4     10  0
     D  PASEQ#                11     15  0
     D                 DS                  INZ
     D  XXAMTS                 1      9  2
     D  XXAMTC                 1      9
     D                 DS
     D  RRT                    1     20
     D                                     DIM(10)
     D  RHRT01                 1      2
     D  RHRT02                 3      4
     D  RHRT03                 5      6
     D  RHRT04                 7      8
     D  RHRT05                 9     10
     D  RHRT06                11     12
     D  RHRT07                13     14
     D  RHRT08                15     16
     D  RHRT09                17     18
     D  RHRT10                19     20
     D                 DS                  INZ
     D  RSU                    1     90  2
     D                                     DIM(10)
     D  RHSU01                 1      9  2
     D  RHSU02                10     18  2
     D  RHSU03                19     27  2
     D  RHSU04                28     36  2
     D  RHSU05                37     45  2
     D  RHSU06                46     54  2
     D  RHSU07                55     63  2
     D  RHSU08                64     72  2
     D  RHSU09                73     81  2
     D  RHSU10                82     90  2
     D*
     D OPTNM           C                   CONST('Claim Transfer')
     D XXDDCN          C                   CONST('DEDUCTIBLES DELETED')
     D MODEC           C                   CONST('- Change -')
     D MODED           C                   CONST('-Display- ')
     D REASON          C                   CONST('Transfer initiated f-
     D                                     rom Workers Comp')
     D*  first name last name
     D CONOC1          C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ''- abcdefgh-
     D                                     ijklmnopqrstuvwxyz')
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS1          DS
     D  DBRRN1               397    400B 0
     D DBFDS3          DS
     D  DBRRN3               397    400B 0
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     d dsClm1          ds
     d  rrnClm1              397    400b 0

     IPCM01
     I              CMSTAT                      C1STAT
     I              CMIDT                       C1IDT
     I              CMITM                       C1ITM
      * =================================================================

     C*
     C*  CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    XXSS#
     C*
     C*  CLAIM HISTORY MASTER FILE
     C*
     C     CHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     CMKYB         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSS#
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
      *
      *   KEY LIST FOR THE CHECK RECONCILIATION FILE
      *
     C     CRKLST        KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAACCT
     C                   KFLD                    XXYCS
     C*
     C*  CLAIM/DARTS CONTROL FILE
     C*
     C     CXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  DIARY DATE MASTER FILE
     C*
     C     DDKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  DEDUCTIBLE DETAIL BILLS FILE
     C*
     C     DDKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CTXFYR
     C                   KFLD                    CTXCAS
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*   KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXPFDT
     C*
     C     ELKY2B        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  EMPLOYER PAYROLL CLASS FILE
     C*
     C     EPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    SCCLAS
     C                   KFLD                    SCSPEC
     C                   KFLD                    SCEXT
     C                   KFLD                    XXEFDT
     C*
     C     EPKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    SCCLAS
     C                   KFLD                    SCSPEC
     C                   KFLD                    SCEXT
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      *   KEY LIST FOR THE FUND DETAIL FILE
      *
     C     F1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C     F1KY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXPFDT
     C*
     C*  KEY LIST FOR THE GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
     C*
     C*  LATE PAYMENT FILE
     C*
     C     LPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  MULTIPLE CLAIM FORM FILE
     C*
     C     LTKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*   KEY LIST FOR THE OFFICE MASTER FILE
     C*
     C     OFKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXOFF#
     C*
     C*  PAYMENT DETAIL FILE
     C*
     C     PAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  PAYMENT DETAIL FILE - TO FILE
     C*
     C     PAKY1         KLIST
     C                   KFLD                    X1CO#
     C                   KFLD                    X1FND
     C                   KFLD                    X1FYR
     C                   KFLD                    X1CASE
     C*
     C*  RESERVE HISTORY MASTER FILE
     C*
     C     RHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  REQUEST PAYMENT FILE
     C*
     C     RPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     RPKYB         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C*  REQUEST PAYMENT DETAIL FILE
     C*
     C     RPKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    PATRDT
     C                   KFLD                    PACHK#
     C                   KFLD                    SVSEQ#
     C*
     C*  RECOVERY REQUEST FILE
     C*
     C     RRKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     RRKY1         KLIST
     C                   KFLD                    X1CO#
     C                   KFLD                    X1FND
     C                   KFLD                    X1FYR
     C                   KFLD                    X1CASE
     C*
     C*  FS BASE CONTROL FILE
     C*
     C*   KEY LIST FOR THE STATE FILE
     C*
     C     STKY          KLIST
     C                   KFLD                    XXKEY
      *
      *  TABLE FILE
      *
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C*  TRANSFER LOST TIME - MED ONLY FILE
     C*
     C     THKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  WORK HISTORY MASTER FILE
     C*
     C     WHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C     CUKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
      *
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELRTCD
      *
     C*
     C     *LIKE         DEFINE    CTAMT         XXPAY
     C     *LIKE         DEFINE    CDMDBN        SVMDBN
     C     *LIKE         DEFINE    CDHICN        SVHICN
     C     *LIKE         DEFINE    CDLSN         SVLSN
     C     *LIKE         DEFINE    CDMID         SVMID
     C     *LIKE         DEFINE    CDFSN         SVFSN
     C     *LIKE         DEFINE    CDADR1        SVADR1
     C     *LIKE         DEFINE    CDADR2        SVADR2
     C     *LIKE         DEFINE    CDST          SVST
     C     *LIKE         DEFINE    CDCTY         SVCTY
     C     *LIKE         DEFINE    CDZIP         SVZIP
     C     *LIKE         DEFINE    CDZIP         SCZIP
     C     *LIKE         DEFINE    CDCTRY        SVCTRY
     C     *LIKE         DEFINE    CDADR3        SVADR3
     C     *LIKE         DEFINE    NTTYPE        XTYPE1
     C     *LIKE         DEFINE    NTTYPE        XTYPE2
     C     *LIKE         DEFINE    CMACDT        XXACDT
     C     *LIKE         DEFINE    CMACDT        XXDT
     C     *LIKE         DEFINE    CMACDT        XXEFDT
     C     *LIKE         DEFINE    SCACDT        BFACDT
     C     *LIKE         DEFINE    CMACDT        XXPFDT
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMEMP#        XXEMP#
     C     *LIKE         DEFINE    CMEMP#        BFEMP#
     C     *LIKE         DEFINE    CMDIV         XXDIV
     C     *LIKE         DEFINE    CMDIV         BFDIV
     C     *LIKE         DEFINE    CMFYR         BFYR
     C     *LIKE         DEFINE    CMFYR         XUFYR
     C     *LIKE         DEFINE    CMCASE        BFCASE
     C     *LIKE         DEFINE    CMCASE        XXWK70
     C     *LIKE         DEFINE    CMCO#         X1CO#
     C     *LIKE         DEFINE    CMFND         X1FND
     C     *LIKE         DEFINE    CMOFF#        XXOFF#
     C     *LIKE         DEFINE    CMSS#         BFSS#
     C     *LIKE         DEFINE    CMCLAS        BFCLAS
     C     *LIKE         DEFINE    CMSPEC        BFSPEC
     C     *LIKE         DEFINE    CMEXT         BFEXT
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         XXKEY
     C     *LIKE         DEFINE    PASEQ#        XXSEQ#
     C     *LIKE         DEFINE    DDAMTB        XXAMTB
     C     *LIKE         DEFINE    SCDTOB        SVDTOB
     C     *LIKE         DEFINE    SCSEX         SVSEX
     C     *LIKE         DEFINE    GTKEY         QQKEY
     C     *LIKE         DEFINE    GTTBID        QQTBID
     C     *LIKE         DEFINE    SCAREA        SVAREA
     C     *LIKE         DEFINE    SCHPN1        SVHPN1
     C     *LIKE         DEFINE    SCHPN2        SVHPN2
     C     *LIKE         DEFINE    SCWARA        SVWARA
     C     *LIKE         DEFINE    SCWPN1        SVWPN1
     C     *LIKE         DEFINE    SCWPN2        SVWPN2
     C     *LIKE         DEFINE    SCCARA        SVCARA
     C     *LIKE         DEFINE    SCCPN1        SVCPN1
     C     *LIKE         DEFINE    SCCPN2        SVCPN2
     C     *LIKE         DEFINE    SCCCCD        SVCCCD
     C     *LIKE         DEFINE    SCMSTS        SVMSTS
     C     *LIKE         DEFINE    CTTINC        XXTINC
     C     *LIKE         DEFINE    CTRSV         XXRSVC
     C     *LIKE         DEFINE    CTRSV         XXAMT
     C     *LIKE         DEFINE    CTPAID        XXPAID
     C     *LIKE         DEFINE    PXST          XXST
     C     *LIKE         DEFINE    CMCO#         XXCO1#
     C     *LIKE         DEFINE    CMFND         XXFND1
     C     *LIKE         DEFINE    CMFYR         XXFYR1
     C     *LIKE         DEFINE    CMCASE        XXCAS1
     C*** Beg Add *** 10/14/20 *************************************************
     C     *Like         Define    CMfYr         xxBFyr
     C     *Like         Define    CMemp#        xxBFemp#
     C     *Like         Define    CMdiv         xxBFdiv
     C     *Like         Define    CMfYr         xxCTyr
     C     *Like         Define    CMemp#        xxCTemp#
     C     *Like         Define    CMdiv         xxCTdiv
     C*** End Add *** 10/14/20 *************************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
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
     C     ACDPL         PLIST
     C                   PARM                    ##KC              1
     C                   PARM                    ##KL              1
     C                   PARM      '2'           ##MODE            1
     C                   PARM      *ZERO         ##RRN             9 0
     C                   PARM      DBRRN2        ##RRN2            9 0
     C     ACDPL2        PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      *ZERO         ##RRN             9 0
     C                   PARM                    XXKEY
     C                   PARM                    SCST
     C                   PARM      2             ##VIEW            1 0
     C     ACDPL3        PLIST
     C     *INKC         PARM      *OFF          ##KC              1
     C     *INKL         PARM      *OFF          ##KL              1
     C                   PARM      '2'           ##MODE            1
     C                   PARM      *ZERO         ##RRN             9 0
     C                   PARM                    ##RRN2            9 0
     C                   PARM      'W'           ##QTE             1
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C                   MOVEL     XXDT          XXCYM             5 0
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*
     C*   CHECK TO SEE IF FSMS IS INSTALLED
     C*
     C                   CALL      'S6000R'
     C                   PARM                    QQWCMC            1
     C                   PARM                    QQCHK             1
     C                   PARM                    QQBAR             1
     C                   PARM                    QQDED             1
     C                   PARM                    QQADMN            1
     C                   PARM                    QQCLM             1
     C                   PARM                    QQEDI             1
     C                   PARM                    QQMICR            1
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     TOP           TAG
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPFND
     C                   END
     C                   MOVE      UPCO#         XXCO#
      /Free
       qqCo#1 = UPCO#;                          //Get non-changeable IDC keys
       qqCo#2 = UPCO#;
       Chain (UPco#:UPfnd) PF201;
       If %found(WDF2L);
          qqCar1 = %Editc(F2car#:'X');
          qqCar2 = %Editc(F2car#:'X');
       Endif;
      /End-Free
     C                   MOVE      UPFND         XXFND
     C                   Z-ADD     UPPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPRDT            8 0
     C                   Z-ADD     UPRAL         XXRAL            11 2
     C                   Z-ADD     UPFYR         XUFYR
     C                   Z-ADD     UPFYR         SCYR
     C*
     C*  CHECK AUTHORITY TO UPDATE CLAIMANT NAME
     C*
     C     UPFL2         COMP      'Y'                                8484
     C*
     C*  GET THE COMPANY AND FUND RECORDS
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   CLEAR                   FNNAME
     C     FNKEY         CHAIN(N)  PFN01                              60
     C                   MOVE      UPFND         SCFND
     C                   MOVE      FNNAME        SCNAME
     C                   MOVEL     FNNAME        SCFNAM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL(P)  ##DATA        SCPRDS
     C*
     C     QQCALL        COMP      '1'                                    09
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
      /Free
       SCSS# = MaskData('SSN':'CMSS#':CMSS#);
      /End-free
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
      /Free
       SCSS# = UnMaskData('SSN':'CMSS#':SCSS#);
      /End-free
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C                   EXFMT     SCR03
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR04'
     C                   EXFMT     SCR04
     C                   END
     C*_______________________________________________________________
     C*
     C*  PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*===============================================================
     C*  F1 = RESTART
     C*===============================================================
     C*
     C     *INKA         IFEQ      '1'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   Z-ADD     XUFYR         SCYR
     C                   Z-ADD     0             SCCASE
     C                   GOTO      STRT
     C                   END
     C*
     C*===============================================================
     C*  F3 = EXIT
     C*===============================================================
     C*
     C     *INKC         IFEQ      '1'
     C                   GOTO      TERM
     C                   END
     C*
     C*===============================================================
     C*  F10 = SEARCH
     C*===============================================================
     C*
     C     *INKJ         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C                   MOVEA     '00'          *IN(70)
     C     HFLD          IFEQ      'SCYR'
     C     HFLD          OREQ      'SCCASE'
     C                   MOVE      SCYR          ##YEAR            3 0
     C                   MOVE      *ON           *IN70
     C                   END
     C     HFLD          IFEQ      'SCYR2'
     C     HFLD          OREQ      'SCCAS2'
     C                   MOVE      SCYR2         ##YEAR
     C                   MOVE      *ON           *IN71
     C                   END
     C*
     C*  Call the 'Work with claims' search.
     C*
     C     *IN70         IFEQ      '1'
     C     *IN71         OREQ      '1'
     C                   CALL      'W0070R'
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@KC              1
     C                   PARM                    @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM      *ZERO         @@RRN             9 0
     C                   PARM      'CN'          @@SORT            2
     C                   PARM      DBRRN1        @@RRN2            9 0
     C                   PARM      ##YEAR        @@YEAR            3 0
     C*
     C     @@KC          CABEQ     *ON           TERM
     C*
     C     @@KL          IFEQ      *ON
     C     @@RRN         OREQ      *ZERO
     C   70              MOVE      '1'           *IN40
     C   71              MOVE      '1'           *IN41
     C                   GOTO      STRT
     C                   END
     C*
     C     @@RRN         CHAIN     WMCMP                              60
     C   70              MOVE      CMFYR         SCYR
     C   70              MOVE      CMCASE        SCCASE
     C   71              MOVE      CMFYR         SCYR2
     C   71              MOVE      CMCASE        SCCAS2
     C   70              MOVE      '1'           *IN41
     C*
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
      *
      *  Search Employer and Division
      *
     C     Q2RECF        IFEQ      'SCR02'
     C     HFLD          ANDEQ     'SCEMP#'
     C     Q2RECF        OREQ      'SCR02'
     C     HFLD          ANDEQ     'SCDIV'
     C                   MOVE      SCFND         XXFND
     C     FNKEY         CHAIN(N)  PFN01                              60
     C  N60              Z-ADD     DBRRN1        ##RRN2
     C   60              Z-ADD     *ZERO         ##RRN2
      *
     C                   CALL      'W0050R'      ACDPL3
      *
     C     @@KC          CABEQ     *ON           TERM
     C     @@KL          CABEQ     *ON           STRT                     75
     C     ##RRN         CABEQ     *ZERO         STRT                     75
      *
     C     ##RRN         CHAIN     PEM0P                              60
     C                   SETON                                        76
     C                   Z-ADD     EMEMP#        SCEMP#
     C                   Z-ADD     EMDIV         SCDIV
     C                   Z-ADD     EMEMP#        XXEMP#
     C                   Z-ADD     EMDIV         XXDIV
     C                   MOVEL     EMNAME        SCENAM
     C                   MOVEL     EMOFF#        SCEOFF
      *
      *  GET THE OFFICE DESCRIPTION FROM THE OFFICE MASTER FILE
      *
     C                   MOVEL     EMOFF#        XXOFF#
     C                   MOVE      *BLANK        SCEOF2
     C     OFKY          CHAIN     POF01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  OFNAME        SCEOF2
     C                   MOVEL(P)  OFNAME        SCEOFD
     C                   END
      *
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C     HFLD          ANDEQ     'SCCLAS'
     C*
     C     ELKEY         CHAIN(n)  PEL01                              99
     C                   CALL      'W0060R'      ACDPL                  99
     C*
      *
      *  GET THE PAYROLL CLASS CODE
      *
     C     ##RRN         CHAIN     PEP0P                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     EPCLAS        SCCLAS
     C                   MOVEL     EPSPEC        SCSPEC
     C                   MOVEL     EPEXT         SCEXT
     C                   MOVE      *ON           *IN86
     C                   ELSE
     C                   MOVE      *ON           *IN85
     C                   END
     C*
     C                   ENDIF
     C     RSKY          CHAIN     PRS01                              61
     C  N61              MOVE      RSJRST        XXST
      *
     C     Q2RECF        IFEQ      'SCR02'
     C     HFLD          ANDEQ     'SCEXT'
     C                   CALL      'W0650R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    ##RRN             9 0
     C                   PARM                    ELCO#
     C                   PARM                    SCCLAS
     C                   PARM                    XXST
     C*
     C     *INKC         IFEQ      *ON
     C                   MOVE      *ON           *INKC
     C                   ENDIF
     C*
     C     ##RRN         IFEQ      *ZEROS
      *
     C                   MOVE      *ON           *IN51
      *
     C                   ELSE
      *
     C     ##RRN         CHAIN     PPX02                              99
     C     *IN99         IFEQ      *OFF
     C     PXSTAT        ANDEQ     'I'
     C                   SETON                                        52  90
     C                   SETON                                        73
     C                   ENDIF
     C                   MOVEL     PXEXT         SCEXT
     C                   ENDIF
      *
     C                   ENDIF
      *
      *  MARITAL STATUS
      *
     C     Q2RECF        IFEQ      'SCR02'
     C     HFLD          ANDEQ     'SCMSTS'
     C                   MOVEL(P)  'MSTS'        QQTBID
     C                   MOVE(P)   *BLANK        QQKEY
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQTBID
     C                   PARM                    QQKEY
     C                   PARM                    QQRTCD
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   END
     C     QQRTCD        IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
     C                   MOVE      *ON           *IN47
     C                   END
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  QQKEY         SCMSTS
     C                   MOVEL(P)  'MSTS'        XXTBID
     C                   MOVEL(P)  QQKEY         XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCMSTD
     C                   MOVE      *ON           *IN49
     C                   END
     C                   END
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C     HFLD          ANDEQ     'SCCTRY'
     C     *IN70         ANDEQ     *OFF
     C                   MOVEL(P)  'CTRY'        QQTBID
     C                   MOVE(P)   *BLANK        QQKEY
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQTBID
     C                   PARM                    QQKEY
     C                   PARM                    QQRTCD
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   END
     C     QQRTCD        IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
     C                   MOVE      *ON           *IN38
     C                   END
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  QQKEY         SCCTRY
     C                   MOVEL(P)  'CTRY'        XXTBID
     C                   MOVEL(P)  QQKEY         XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCCTRD
     C                   MOVE      *ON           *IN37
     C                   END
     C                   END
     C                   END
     C*
      *
      *  CLAIMANT COUNTY
      *
     C     Q2RECF        IFEQ      'SCR02'
     C     HFLD          ANDEQ     'SCCCCD'
     C                   MOVE      '2'           ##MODE
     C                   MOVE      *BLANK        XXKEY
     C                   CALL      'W2215R'      ACDPL2                 99
     C     ##RRN         CHAIN     PCN0P                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN48
     C                   ELSE
     C                   MOVE(P)   CNKEY         XX8A              8
     C                   MOVEL(P)  XX8A          SCCCCD
     C                   MOVEL(P)  CNKEY         XXKEY
     C                   MOVEL(P)  CNDESC        SCCCDE
     C                   MOVE      *ON           *IN65
     C                   END
     C                   END
     C*
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*===============================================================
     C*  F12 = CANCEL
     C*===============================================================
     C*
     C     *INKL         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*===============================================================
     C*  F13 = CHANGE USER PARAMETERS
     C*===============================================================
     C*
     C     *INKM         IFEQ      '1'
     C                   CALL      'W0006R'
     C                   GOTO      TOP
     C                   END
     C*
     C*===============================================================
     C*  HELP REQUEST
     C*===============================================================
     C*
     C     *IN10         IFEQ      '1'
     C*
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*_______________________________________________________________
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C     Q2RECF        CABEQ     'SCR03'       SCR03T
     C     Q2RECF        CABEQ     'SCR04'       SCR04T
     C*
     C* =================================================================
     C*   PROCESS REQUEST SCREEN 1
     C* =================================================================
     C*
     C     SCR01T        TAG
     C                   MOVE      '0'           *IN90
     C                   MOVE      *BLANK        ##FLAG
      /Free
       isNewIdc = *Off;
      /End-Free
     C*
     C*  THE FROM AND TO CASES CANNOT BE THE SAME
     C*
     C     SCYR          IFEQ      SCYR2
     C     SCCASE        ANDEQ     SCCAS2
     C                   MOVE      '1'           *IN36
     C                   GOTO      STRT
     C                   END
     C*
     C*  MAKE SURE CLAIM IS ON FILE
     C*
     C                   Z-ADD     SCYR          XXFYR
     C                   Z-ADD     SCCASE        XXCASE
     C     CMKY          CHAIN(N)  PCM01                              22
     C     *IN22         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
      /Free
       CTXjrst = CMjrst;
       setll (xxCo#:xxFnd:xxFyr:xxCase) PTR04;
       If %equal(WMIDCTRL4);
          *in43 = *On;
       Else;
          *in43 = *Off;
          setll (xxCo#:xxFnd:xxFyr:xxCase) PQT07;
          If %equal(WMIDCQTL7);
             *in43 = *On;
          Endif;
       Endif;
      /End-Free
     C*
     C   43              GOTO      STRT
      /Free
       Setll (xxCo#:xxFnd:scyr2:scCas2) PTR04;
       If %equal(WMIDCTRL4);
          *in46 = *On;
       Else;
          *in46 = *Off;
          Setll (xxCo#:xxFnd:scyr2:scCas2) PQT07;
          If %equal(WMIDCQTL7);
             *in46 = *On;
          Endif;
       Endif;
      /End-Free
     C*
     C   46              GOTO      STRT
     C* ---------------------------------------------------------------
     C*  IF TRANSFER TO AN EXISTING CLAIM, MAKE SURE NEW CLAIM EXISTS
     C*
     C                   MOVE      '0'           *IN70
     C     SCYR2         IFNE      *ZERO
     C     SCCAS2        ORNE      *ZERO
     C                   Z-ADD     SCYR2         XXFYR
     C                   Z-ADD     SCCAS2        XXCASE
     C     CMKY          CHAIN(N)  PCM01                              23
     C     *IN23         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C*                                                    JJJ 12-01-88
     C*  REDUCE THE REMAINING RESERVES BY ANY REQUESTS THAT EXIST
     C*  FOR THE "TRANSFER TO" CLAIM  (FOR EDITTING)       |
     C*                                                    |
     C     RPKEY         CHAIN     PRP01                              60
     C     *IN60         DOWEQ     '0'
     C     RPSTAT        IFEQ      ' '
     C                   Z-ADD     1             X                 2 0
     C     RPRECD        LOOKUP    RT(X)                                  61
     C                   SUB       RPAMT         RR(X)
     C                   END
     C     RPKEY         READE     PRP01                                  60
     C                   END
     C*
     C* ---------------------------------------------------------------
     C*  NOW GET ALL REQUESTS FOR THE "FROM CLAIM" & EDIT RESERVES
     C*
     C                   Z-ADD     SCYR          XXFYR
     C                   Z-ADD     SCCASE        XXCASE
     C*
     C*  REDUCE RESERVES BY REQUESTS TO BE TRANSFERRED
     C*  (COMPARE FROM-CLAIM REQUESTS AGAINST TO-CLAIM RESERVES)
     C*
     C     RPKEY         CHAIN     PRP01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     RPSTAT        IFEQ      ' '
     C                   Z-ADD     1             X
     C     RPRECD        LOOKUP    RT(X)                                  61
     C  N61              Z-ADD     10            X
     C                   SUB       RPAMT         RR(X)
     C     RR(X)         CABLT     *ZERO         STRT                   24
     C                   END
     C     RPKEY         READE     PRP01                                  60
     C                   END
     C*
     C*  CHECK PAYMENT DETAIL
     C*  (COMPARE FROM-CLAIM PAYMENTS AGAINST TO-CLAIM RESERVES)
     C*
     C     PAKEY         CHAIN(N)  PPA01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   Z-ADD     1             X
     C     PARECD        LOOKUP    RT(X)                                  61
     C  N61              Z-ADD     10            X
     C     PATYPE        IFEQ      'R'
     C     PATYPE        OREQ      'V'
     C     PATYPE        OREQ      'S'
     C                   ADD       PAAMT         RR(X)
     C                   ELSE
     C                   SUB       PAAMT         RR(X)
     C                   END
     C     PAKEY         READE     PPA01                                  60
     C                   ENDDO
     C*
     C*  CHECK ARRAY FOR NEGATIVE AMOUNTS
     C*  (IF FROM-CLAIM PAYMENTS GREATER THAN TO-CLAIM RESERVES,
     C*  INCREASE TO-CLAIM RESERVES)
     C*
     C                   Z-ADD     1             X
     C     X             DOWLT     11
     C     RR(X)         CABLT     0             STRT                   24
     C                   ADD       1             X
     C                   END
     C*
     C*  RESTORE ORIGINAL CLAIM (FROM-CLAIM) TO MEMORY
     C*
     C                   MOVE      '1'           *IN70
     C     CMKY          CHAIN(N)  PCM01                              22
     C                   MOVE      'Y'           ##FLAG
     C                   END
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1
     C                   PARM      *BLANK        ##MCIF            1
     C                   PARM      *BLANK        ##WCIN            1
     C                   PARM      *BLANK        ##AR              1
     C                   PARM      *BLANK        ##NTWK            1
     C                   PARM      *BLANK        ##UR              1
     C                   PARM      *BLANK        ##MED             1
     C                   PARM      *BLANK        ##EDI             1
     C                   PARM      *BLANK        ##WCDK            1
     C*
     C     ##MCIF        IFEQ      'Y'
     C     ##WCIN        ANDEQ     'Y'
     C*
     C*   FIRST YOU NEED TO GET THE RRN OF THE FIRST CASE
     C*
     C     ##FLAG        IFEQ      'Y'
     C                   MOVE      XXCO#         XYCO#             3
     C                   MOVE      XXFND         XYFND             3
     C                   MOVE      SCYR          XYFYR             3
     C                   MOVE      SCCASE        XYCASE            7
     C*
     C                   CALL      'FA201R'
     C                   PARM                    XYCO#
     C                   PARM                    XYFND
     C                   PARM                    XYFYR
     C                   PARM                    XYCASE
     C                   PARM      *ZEROS        XYRRN1
     C                   PARM      *BLANK        XYRTCD
     C*
     C*   NEXT YOU NEED TO GET THE RRN OF THE 'TO' CASE
     C*
     C                   MOVE      XXFND         XYFND             3
     C                   MOVE      SCYR2         XYFYR             3
     C                   MOVE      SCCAS2        XYCASE            7
     C*
     C                   CALL      'FA201R'
     C                   PARM                    XYCO#
     C                   PARM                    XYFND
     C                   PARM                    XYFYR
     C                   PARM                    XYCASE
     C                   PARM      *ZEROS        XYRRN2            9 0
     C                   PARM      *BLANK        XYRTCD
     C*
     C*  NOW YOU HAVE TO CLEAR OUT AND CREATE TEMP FILES
     C*
     C                   MOVEL(P)  'FAT3PQ'      #FNAME
     C                   MOVEL(P)  'FAT3PQ'      #TNAME
     C*
     C                   CALL      'FA710C'
     C                   PARM      ' '           ##RTN             1
     C                   PARM                    #FNAME           10
     C                   PARM                    #TNAME           10
     C*
     C                   MOVEL(P)  'FAT3LQ'      #FNAME
     C                   MOVEL(P)  'FAT3LQ'      #TNAME
     C*
     C                   CALL      'FA710C'
     C                   PARM      ' '           ##RTN             1
     C                   PARM                    #FNAME           10
     C                   PARM                    #TNAME           10
     C*
     C*   NOW YOU CALL F7090R TO SET UP THE TRANSFER
     C*
     C                   CALL      'F7090R'
     C                   PARM      ' '           ##CALL            1
     C                   PARM                    ##KL              1
     C                   PARM      XYRRN1        ##RRN             9 0
     C                   PARM      *ZEROS        ##RRN1            9 0
     C                   PARM      *ZEROS        ##RRN2            9 0
     C                   PARM      XYRRN2        ##RRN3            9 0
     C                   PARM      'T'           ##TD              1
     C                   PARM      'Y'           ##FLAG            1
     C                   PARM      'Y'           ##WCMS            1
     C*
     C     ##KL          IFEQ      *ON
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDIF
      *
      * ---------------------------------------------------------------
      *
     C                   MOVEL(P)  SCYR          XXWK10           10
     C     XXWK10        CAT(P)    '-':0         SCYRCS
     C                   MOVEL(P)  SCCASE        XXWK10
     C                   CAT       XXWK10:0      SCYRCS
     C                   MOVEL     CMACL#        SCSCL#
      *
     C                   MOVE      CMEMP#        SCEMP#
     C                   MOVE      CMDIV         SCDIV
     C                   MOVE      SCEMP#        XXEMP#
     C                   MOVE      SCDIV         XXDIV
      *
      *  GET THE OFFICE DESCRIPTION FROM THE OFFICE MASTER FILE
      *
     C                   MOVEL     CMOFF#        SCCOFF
     C                   MOVEL     CMOFF#        XXOFF#
     C                   MOVE      *BLANK        SCCOFD
     C     OFKY          CHAIN     POF01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  OFNAME        SCCOFD
     C                   END
     C*
     C     EMKEY         CHAIN     PEM01                              20
     C                   MOVEL     EMNAME        SCENAM
     C                   MOVEL     EMOFF#        SCEOFF
      *
      *  GET THE OFFICE DESCRIPTION FROM THE OFFICE MASTER FILE
      *
     C                   MOVEL     EMOFF#        XXOFF#
     C                   MOVE      *BLANK        SCEOF2
     C     OFKY          CHAIN     POF01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  OFNAME        SCEOF2
     C                   MOVEL(P)  OFNAME        SCEOFD
     C                   END
     C*
     C                   MOVE      *BLANKS       SCLSN
     C                   MOVE      *BLANKS       SCFSN
     C                   MOVE      *BLANKS       SCMID
     C                   MOVE      *BLANKS       SCADR1
     C                   MOVE      *BLANKS       SCADR2
     C                   MOVEL     *BLANKS       SCCTY
     C                   MOVE      *BLANKS       SCST
     C                   MOVE      *ZERO         SCZIP1
     C                   MOVE      *ZERO         SCZIP2
     C                   MOVE      *BLANKS       SCDTOB
     C                   MOVEL     *BLANKS       SCSEX
     C                   MOVE      *BLANKS       SCMSTS
     C                   MOVE      *BLANKS       SCMSTD
     C                   Z-ADD     *ZEROS        SCAREA
     C                   MOVE      *BLANKS       SCHPN1
     C                   MOVE      *BLANKS       SCHPN2
     C                   Z-ADD     *ZEROS        SCWARA
     C                   MOVE      *BLANKS       SCWPN1
     C                   MOVE      *BLANKS       SCWPN2
     C                   Z-ADD     *ZEROS        SCCARA
     C                   MOVE      *BLANKS       SCCPN1
     C                   MOVE      *BLANKS       SCCPN2
     C                   MOVE      *BLANKS       SCCCCD
     C                   MOVE      *BLANKS       SCCCDE
     C                   MOVE      *BLANK        SCCTRY
     C                   MOVE      *BLANK        SCADR3
     C                   MOVE      *BLANKS       SCHICN
     C*
     C                   MOVE      CMSS#         SCSS#
     C                   MOVE      SCSS#         XXSS#
     C     CDKEY         CHAIN(N)  PCD01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVEL     CDLSN         QQLAST           56
     C                   MOVEL     CDFSN         QQFRST           15
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        SCCDNM
     C                   MOVE      CMCLAS        SCCLAS
     C                   MOVEL     CMSPEC        SCSPEC
     C                   MOVEL     CMEXT         SCEXT
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C                   MOVEL     SRDTO         SCDOI
     C                   MOVE      *BLANK        SCUOF#
     C*
      /Free
       SCHSS# = DisplaySS#(MaskData('SSN':'XXSS#':XXSS#));
      /End-free
      *
      * LOAD CLAIMANT INFO TO SCREEN
      *
     C                   MOVEL     CDLSN         SCLSN
     C                   MOVEL     CDFSN         SCFSN
     C                   MOVEL     CDMID         SCMID
     C                   MOVEL     CDADR1        SCADR1
     C                   MOVEL     CDADR2        SCADR2
     C                   MOVEL     CDCTY         SCCTY
     C                   MOVEL     CDST          SCST
     C                   MOVEL     CDZIP         SCZIP1
     C                   MOVE      CDZIP         SCZIP2
     C                   MOVE      CDZIP         SCZIP
     C                   MOVE      CDMDBN        SCMDBN
     C                   MOVE      CDHICN        SCHICN
     C*
     C                   MOVEL(P)  CDCTRY        SCCTRY
     C                   MOVEL(P)  CDADR3        SCADR3
     C                   MOVEL(P)  CDCTRY        XXKEY
     C                   MOVEL(P)  'CTRY'        XXTBID
     C     GTKY          CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCCTRD
     C                   ELSE
     C                   MOVE      *BLANKS       SCCTRD
     C                   ENDIF
     C*
     C                   MOVE      CDMSTS        SCMSTS
     C                   MOVE      *BLANK        SCMSTD
     C     CDMSTS        IFNE      *BLANK
     C                   MOVEL(P)  'MSTS'        XXTBID
     C                   MOVEL(P)  CDMSTS        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCMSTD
     C                   END
     C                   END
      *  HOME PHONE
     C                   Z-ADD     CDAREA        SCAREA
     C                   MOVEL(P)  CDPHN#        SCHPN1
     C                   MOVE(P)   CDPHN#        SCHPN2
      *  WORK PHONE
     C                   Z-ADD     CDWARA        SCWARA
     C                   MOVEL(P)  CDWPH#        SCWPN1
     C                   MOVE(P)   CDWPH#        SCWPN2
      *  MOBILE PHONE
     C                   Z-ADD     CDCARA        SCCARA
     C                   MOVEL(P)  CDCPHN        SCCPN1
     C                   MOVE(P)   CDCPHN        SCCPN2
      *  GET COUNTY DESCRIPTION
     C                   MOVE(P)   *BLANK        SCCCCD
     C                   MOVE(P)   *BLANK        SCCCDE
     C                   MOVEL(P)  CDCNTY        SCCCCD
     C     CDST          CAT(P)    CDCNTY:0      XXKEY
     C     TBKY          CHAIN     PCN01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  CNDESC        SCCCDE
     C                   END
     C                   Z-ADD     CDDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDTOB
     C                   MOVEL     CDSEX         SCSEX
     C                   END
     C     Q1USER        CAT(P)    '-':1         SCREQ
     C                   CAT       UPUSNM:1      SCREQ
     C*
     C*  Load save fields.
     C*
     C                   Z-ADD     SCYR          BFYR
     C                   MOVE      SCEMP#        BFEMP#
     C                   MOVE      SCDIV         BFDIV
     C                   Z-ADD     SCACDT        BFACDT
     C                   MOVE      SCCLAS        BFCLAS
     C                   MOVEL     SCSPEC        BFSPEC
     C                   MOVEL     SCEXT         BFEXT
     C                   MOVE      SCSS#         BFSS#
     C                   MOVE      SCMDBN        SVMDBN
     C                   MOVE      SCHICN        SVHICN
     C                   MOVE      SCLSN         SVLSN
     C                   MOVE      SCFSN         SVFSN
     C                   MOVE      SCMID         SVMID
     C                   MOVE      SCADR1        SVADR1
     C                   MOVE      SCADR2        SVADR2
     C                   MOVE      SCCTY         SVCTY
     C                   MOVE      SCST          SVST
     C                   MOVE      SCZIP         SVZIP
     C                   MOVEL(P)  SCCTRY        SVCTRY
     C                   MOVEL(P)  SCADR3        SVADR3
     C                   MOVE      SCDTOB        SVDTOB
     C                   MOVE      SCSEX         SVSEX
     C                   MOVEL     SCZIP1        SVZIP1            5 0
     C                   MOVE      SCZIP2        SVZIP2            4 0
     C                   MOVE      SCMSTS        SVMSTS
     C                   Z-ADD     SCAREA        SVAREA
     C                   MOVE      SCHPN1        SVHPN1
     C                   MOVE      SCHPN2        SVHPN2
     C                   Z-ADD     SCWARA        SVWARA
     C                   MOVE      SCWPN1        SVWPN1
     C                   MOVE      SCWPN2        SVWPN2
     C                   Z-ADD     SCCARA        SVCARA
     C                   MOVE      SCCPN1        SVCPN1
     C                   MOVE      SCCPN2        SVCPN2
     C                   MOVE      SCCCCD        SVCCCD
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C* ================================================================
     C* PROCESS SCREEN 2 - CLAIM INFORMATION
     C* ================================================================
     C*
     C     SCR02T        TAG
     C*
     C*  FIRST UPDATE TRANSFER TO A NEW EXISTING CLAIM
     C*
     C     *IN70         IFEQ      '1'
     C*
     C*  DISPLAY A WINDOW WHICH REQUIRES AN EXPLANATION OF THE TRANSFER.
     C*
     C                   MOVE      *BLANKS       SCDLTR
     C                   MOVE      *BLANKS       SCDLTD
     C                   MOVE      *BLANKS       SCDEX1
     C                   MOVE      *BLANKS       SCDEX2
     C     WDW1          TAG
     C                   EXFMT     WDW01
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C     *INKC         CABEQ     '1'           TERM
     C     *INKL         CABEQ     '1'           STRT
     C*
     C* F10 = SEARCH FOR DELETE REASON CODE
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C                   MOVE      *ON           *IN39
     C                   MOVE      *BLANKS       XXTBID
     C                   MOVEL     'DLTR'        XXTBID
     C                   MOVE      *BLANKS       XXKEY
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXTBID
     C                   PARM                    XXKEY
     C                   PARM                    QQRTC             1
     C*
     C                   MOVEL     XXKEY         SCDLTR
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     XXKEY         OREQ      *BLANKS
     C                   MOVE      *ON           *IN30
     C                   MOVE      *ON           *IN42
     C                   GOTO      WDW1
     C                   ENDIF
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVEL     'DLTR'        XXTBID
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCDLTR        XXKEY
     C     GTKY          CHAIN     PGT01                              42
     C     *IN42         IFEQ      *OFF
     C     GTSTAT        CABEQ     'I'           WDW1                     42
     C                   MOVEL     GTDESC        SCDLTD
     C                   GOTO      WDW1
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  EDIT THE EXPLANATION FIELDS.
     C*
     C     SCDEX1        IFEQ      *BLANKS
     C     SCDEX2        ANDEQ     *BLANKS
     C     SCDLTR        ANDEQ     *BLANKS
     C                   MOVE      '1'           *IN21
     C                   GOTO      WDW1
     C                   END
     C*
     C*  WRITE A RECORD TO THE CLAIM HISTORY FILE.
     C*
     C                   Z-ADD     SCYR          XXFYR
     C                   Z-ADD     SCCASE        XXCASE
     C     CMKY          CHAIN(N)  PCM01                              60
     C*
     C                   MOVEL     SCDLTR        CMDLTR
     C                   MOVEL     SCDEX1        CMDEX1
     C                   MOVEL     SCDEX2        CMDEX2
     C                   Z-ADD     XXDT          CMDLDT
     C                   MOVEL     Q1PGM         CMPGM
     C                   MOVEL     Q1USER        CMUSER
     C                   Z-ADD     XXDT          CMDT
     C                   Z-ADD     DSTIME        CMTIME
     C                   MOVEL     CMUSER        CMIUSR
     C                   MOVE      CMDT          CMIDT
     C                   MOVE      CMTIME        CMITM
     C                   WRITE     PCM05
     C*
     C*  CALL THE CLAIM DELETION PGM WHICH WILL DELETE THE CLAIM
     C*  RECORDS FROM THE FILES ADDED BY THE EDI PROCESS.
     C*
     C                   MOVE      XXCO#         QQCO#
     C                   MOVE      XXFND         QQFND
     C                   MOVE      SCYR          QQFYR
     C                   MOVE      SCCASE        QQCASE
     C                   CALL      'W0070R2'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQRTCD
     C*
     C                   Z-ADD     SCYR2         XXFYR
     C                   MOVE      SCCAS2        XXCASE
     C     CMKY          CHAIN     PCM01                              40
     C*
      /Free
       isNewIdc = *on;                                //Claim number change
      /End-Free
     C                   MOVE      CMCO#         CTCO#
     C                   MOVE      SCFND         CTFND
     C                   Z-ADD     SCYR2         CTFYR
     C                   MOVE      SCCAS2        CTCASE
     C                   MOVE      CMEMP#        CTEMP#
     C                   MOVE      CMDIV         CTDIV
     C                   Z-ADD     CMACDT        CTACDT
     C                   MOVE      CMSS#         CTSS#
     C                   MOVE      CMCLAS        CTCLAS
     C                   Z-ADD     SCYR          CTXFYR
     C                   Z-ADD     SCCASE        CTXCAS
     C                   MOVE      BFEMP#        CTXEMP
     C                   MOVE      BFDIV         CTXDIV
     C                   Z-ADD     BFACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CTXACD
     C                   MOVE      BFSS#         CTXSS#
     C                   MOVE      BFCLAS        CTXCLS
     C                   MOVE      SCREQ         CTREQ
     C                   Z-ADD     *ZERO         CTDET#
     C                   MOVE      Q1USER        CTUSER
     C                   MOVE      Q1PGM         CTPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     XXDT          CTDT
     C                   Z-ADD     DSTIME        CTTIME
     C                   Z-ADD     XXDT          CTTRDT
     C                   MOVEL     SCDLTR        CTDLTR
     C*
     C                   Z-ADD     *ZERO         XXPAY
     C                   Z-ADD     *ZERO         XXPAID
     C                   Z-ADD     *ZERO         XXTINC
     C                   Z-ADD     *ZERO         XXRSVC
     C*
     C*  MOVE DETAIL TO NEW CLAIM NUMBER
     C*
     C                   Z-ADD     SCYR          XXFYR
     C                   Z-ADD     SCCASE        XXCASE
     C*
     C*  UPDATE DIARY DATE FILE
     C*
     C     DDKEY         CHAIN     PDD01                              60
     C     *IN60         IFEQ      '0'
     C                   DELETE    PDD01
     C                   END
     C*
     C*  UPDATE RESERVE HISTORY
     C*
     C     RHKEY         CHAIN     PRH01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*  Calculate the total rsv amt deleted (tot inc) and the rsv
     C*  change amt within the current month.
     C*
     C                   XFOOT     RSU           XXAMT
     C                   ADD       XXAMT         XXTINC
     C*
     C                   MOVEL     RHEFDT        XXCYM1            5 0
     C     XXCYM1        IFEQ      XXCYM
     C                   ADD       XXAMT         XXRSVC
     C                   ENDIF
     C*
     C                   DELETE    PRH01
     C     RHKEY         READE     PRH01                                  60
     C                   END
     C*
     C*  FIX STATUS CLAIM HISTORY
     C*
     C     CHKEY         CHAIN     PCH01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PCH01
     C     CHKEY         READE     PCH01                                  60
     C                   END
     C*
     C*  FIX TRANSFER HISTORY - LOST TIME - MED ONLY FILE
     C*
     C     *IN60         DOUEQ     '1'
     C     THKEY         DELETE    PTH01                              60
     C                   END
     C*
     C*  FIX WORK HISTORY - LOST TIME - MED ONLY FILE
     C*
     C     *IN60         DOUEQ     '1'
     C     WHKEY         DELETE    PWH01                              60
     C                   END
     C*
     C*  FIX LATE PAYMENT FILE
     C*
     C     *IN60         DOUEQ     '1'
     C     LPKEY         DELETE    PLP01                              60
     C                   END
     C*
     C*  Delete the Claim Index record
     C*
     C     *IN60         DOUEQ     '1'
     C     LPKEY         DELETE    PID01                              60
     C                   END
     C*
     C*  FIX REQUEST PAYMENT
     C*
     C     RPKEY         CHAIN     PRP01                              60
     C     *IN60         DOWEQ     '0'
     C                   MOVE      SCYR2         RPFYR
     C                   MOVE      SCCAS2        RPCASE
     C                   ADD       1             CTDET#
     C                   UPDATE    PRP01
     C*
     C     RPKEY         READE     PRP01                                  60
     C                   END
     C*
     C*  UPDATE WDRPL - CHECK VIEW FILE
     C*
     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C*
     C*  FIRST CHAIN TO THE PAYMENT DETAIL FILE & GET THE
     C*  LAST SEQUENCE NUMBER.
     C*
     C                   Z-ADD     XXCO#         X1CO#
     C                   Z-ADD     XXFND         X1FND
     C                   Z-ADD     SCYR2         X1FYR
     C                   MOVE      SCCAS2        X1CASE
     C*
     C     PAKY1         SETGT     PPA01
     C     PAKY1         READPE    PPA01                                  60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     0             XXSEQ#
     C                   ELSE
     C                   Z-ADD     PASEQ#        XXSEQ#
     C                   END
     C*
     C*  FIX PAYMENT DETAIL
     C*
     C                   Z-ADD     0             OLS
     C                   Z-ADD     0             NES
     C                   Z-ADD     0             Y                 5 0
     C*
     C     PAKEY         CHAIN     PPA01                              60
     C     *IN60         DOWEQ     '0'
     C                   ADD       1             Y
     C                   Z-ADD     PASEQ#        OLS(Y)
     C                   Z-ADD     PASEQ#        SVSEQ#
     C                   MOVE      SCYR2         PAFYR
     C                   MOVE      SCCAS2        PACASE
     C     XXSEQ#        ADD       1             XXSEQ#
     C                   Z-ADD     XXSEQ#        PASEQ#
     C                   Z-ADD     XXSEQ#        NES(Y)
     C*
     C     PABASQ        IFNE      *ZERO
     C                   Z-ADD     1             Z                 5 0
     C     PABASQ        LOOKUP    OLS(Z)                                 60
     C   60              Z-ADD     NES(Z)        PABASQ
     C  N60              Z-ADD     0             PABASQ
     C                   END
     C*
     C                   ADD       1             CTDET#
     C*
     C*  UPDATE WDRPL - CHECK VIEW FILE
     C*
     C     RPKY2         CHAIN     PRP03                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     SCYR2         RPFYR
     C                   MOVE      SCCAS2        RPCASE
     C                   Z-ADD     PASEQ#        RPSEQ#
     C                   UPDATE    PRP03
     C                   END
     C*
     C*   IF CHECK RECONCILIATION IS INSTALLED, UPDATE FILE
     C*
     C     QQCHK         IFEQ      'Y'
     C*
     C     PATYPE        IFNE      'A'
     C     CRKLST        CHAIN     PCR01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  XXYCS2        CRKY
     C                   UPDATE    PCR01
     C                   END
     C                   END
     C*
     C                   END
     C*
     C*  Calculate the total paid amt.
     C*
     C     PATYPE        IFEQ      'R'
     C     PATYPE        OREQ      'V'
     C     PATYPE        OREQ      'S'
     C                   SUB       PAAMT         XXPAY
     C                   ELSE
     C                   ADD       PAAMT         XXPAY
     C                   END
     C*
     C*  Calculate the total paid amt within the current month.
     C*
     C                   MOVEL     PATRDT        XXCYM1
     C     XXCYM1        IFEQ      XXCYM
     C*
     C     PATYPE        IFEQ      'R'
     C     PATYPE        OREQ      'V'
     C     PATYPE        OREQ      'S'
     C                   SUB       PAAMT         XXPAID
     C                   ELSE
     C                   ADD       PAAMT         XXPAID
     C                   END
     C*
     C                   ENDIF
     C*
     C                   UPDATE    PPA01
     C*
     C                   Z-ADD     1             X
     C     PARECD        LOOKUP    RT(X)                                  61
     C  N61              Z-ADD     10            X
     C     PATYPE        IFEQ      'R'
     C     PATYPE        OREQ      'V'
     C     PATYPE        OREQ      'S'
     C                   ADD       PAAMT         RR(X)
     C                   SUB       PAAMT         AR(X)
     C                   ELSE
     C                   SUB       PAAMT         RR(X)
     C                   ADD       PAAMT         AR(X)
     C                   END
     C*
     C     PAKEY         READE     PPA01                                  60
     C                   ENDDO
     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C*
     C*  FIX MULTIPLE CLAIM FORM FILE
     C*
     C     LTKEY         CHAIN     PLT01                              60
     C     *IN60         DOWEQ     '0'
     C                   MOVE      SCYR2         LTFYR
     C                   MOVE      SCCAS2        LTCASE
     C                   UPDATE    PLT01
     C*
     C     LTKEY         READE     PLT01                                  60
     C                   END
     C*
     C*  FIRST CHAIN TO THE RECOVERY REQST FILE & GET THE
     C*  LAST REQUEST NUMBER.
     C*
     C                   Z-ADD     XXCO#         X1CO#
     C                   Z-ADD     XXFND         X1FND
     C                   Z-ADD     SCYR2         X1FYR
     C                   MOVE      SCCAS2        X1CASE
     C*
     C     RRKY1         SETGT     PRR01
     C     RRKY1         READPE(N) PRR01                                  60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     0             XXREQ#            5 0
     C                   ELSE
     C                   Z-ADD     RRREQ#        XXREQ#
     C                   END
     C*
     C*  FIX RECOVERY REQUEST FILE
     C*
     C     RRKEY         CHAIN     PRR01                              60
     C     *IN60         DOWEQ     '0'
     C                   MOVE      SCYR2         RRFYR
     C                   MOVE      SCCAS2        RRCASE
     C                   ADD       1             XXREQ#
     C                   Z-ADD     XXREQ#        RRREQ#
     C                   UPDATE    PRR01
     C*
     C     RRKEY         READE     PRR01                                  60
     C                   END
     C*
     C*  UPDATE WDACL - Additional address file
     C*
     C     RPKEY         CHAIN     PAC01                              60
     C     *IN60         DOWEQ     '0'
     C                   Z-ADD     SCYR2         ACFYR
     C                   MOVE      SCCAS2        ACCASE
     C                   UPDATE    PAC01
     C     RPKEY         READE     PAC01                                  60
     C                   END
      *
      /Free
       //  Update SADAP - Diagnosis code Info File

       Setll RPKEY SADAL3;
       Reade RPKEY SADAL3;
       Dow not %eof(SADAL3);
          DAFYR = SCYR2;
          DACASE = SCCAS2;
          Update PDA01;
          Reade RPKEY SADAL3;
       Enddo;

      /End-Free
      *
     C*  Update WMCRL - Recurring Payable file
     C*
     C     RPKEY         CHAIN     PCR02                              60
     C     *IN60         DOWEQ     '0'
     C                   Z-ADD     SCYR2         CRFYR
     C                   MOVE      SCCAS2        CRCASE
     C                   UPDATE    PCR02
     C     RPKEY         READE     PCR02                                  60
     C                   END
     C*
     C*  UPDATE DEDUCTIBLE DETAIL
     C*
     C                   EXSR      CHGDD
     C*
     C                   Z-ADD     XXPAY         CTAMT
     C                   Z-ADD     XXTINC        CTTINC
     C                   Z-ADD     XXRSVC        CTRSV
     C                   Z-ADD     XXPAID        CTPAID
     C*
     C*  WRITE WORK FILE RECORD
     C*
     C                   WRITE     PCT01
     C*
     C*  CALL THE EDI PGM TO INDICATE THAT A DELETION OCCURRED,
     C*  IF EDI IS INSTALLED.
     C*
     C     QQEDI         IFEQ      'Y'
     C                   MOVE      XXCO#         QQCO#
     C                   MOVE      XXFND         QQFND
     C                   MOVE      XXFYR         QQFYR
     C                   MOVE      XXCASE        QQCASE
     C                   MOVE      'D'           QQACTF
     C                   CALL      'W0070R6'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQACTF            1
     C                   PARM                    QQMGID            8
     C                   PARM                    QQRTCD
     C                   END
     C*
     C*  WRITE FS TRANSFER WORK RECORD AND CALL JOB (F7064C) THAT WILL
     C*  PERFORM CASE TRANSFER FOR FILES IN THE FEE SCHEDULE ENVIRONMENT
     C*
     C                   EXSR      FSCSTR
     C*
     C*  UPDATE THE CLAIM MASTER RESERVE & PMT TOTALS HELD IN RR & AR
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PCM01
     C                   END
     C*
     C                   Z-ADD     SCYR          XXFYR
     C                   Z-ADD     SCCASE        XXCASE
     C     CMKY          CHAIN     PCM01                              40
     C*
     C*  UPDATE EMPL DETAIL CLM TOTALS IF EMPLR CHANGES
     C*
     C     CTEMP#        IFNE      CTXEMP
     C     CTDIV         ORNE      CTXDIV
     C                   EXSR      CHGEL
      /Free
       //*** Begin Chg *** 10/14/20 ****************************************
       //***isNewIdc = *on;
       // Determine if Policy Number or Policy Eff. Date changed.
       If isModEnabled('IDC');
          xxBFyr   = BFyr;
          xxBFemp# = BFemp#;
          xxBFdiv  = BFdiv;
          xxCTyr   = scYr2;
          xxCTemp# = CTemp#;
          xxCTdiv  = CTdiv;
          Exsr IdcQualify;
       Endif;
       //*** End   Chg *** 10/14/20 ****************************************
      /End-Free
     C*
     C                   END
      /Free
       //Pending state check  (1st Occur)
       If isModEnabled('IDC') and isNewIdc = *on and
          isIdcState(CTco#:CTXjrst) = *on;

          //Old Policy Number
          Chain (xxCo#:xxFnd:BFyr:BFemp#:BFdiv) PRP04;
          If %found(WMRPL4) and R4cap# <> *blanks;

             //*** Begin Chg *** 10/14/20 **************************************
             //Remove hyphens
             //***strCaP# = R4cap#;
             //***IntPos = %scan('-':strCaP#);
             //***If IntPos > *zero;
             //***   Dou IntPos = *zero;
             //***      strCaP# = %subst(strCaP#:1:IntPos-1) +
             //***                %subst(strCaP#:IntPos+1:25-IntPos);
             //***      IntPos = %scan('-':strCaP#);
             //***   Enddo;

             //***   qqPolId1 = strCaP#;
             //***Endif;
             qqPolId1 = FormatPolId(R4cap#);
             //*** End   Chg *** 10/14/20 **************************************
          Else;
             //** Begin Change *** 12/18/20 ************************************
             //qqPolId1 = %Editc(xxFnd:'X') +
             //           %Editc(BFemp#:'X') + %Editc(BFdiv:'X');
             qqPolId1 = %Editc(xxFnd:'X') +
                        %Editc(BFemp#:'X') + %Editc(BFyr:'X');
             //** End   Change *** 12/18/20 ************************************
          Endif;

          chain(n) (xxCo#:xxFnd:BFemp#:BFdiv:BFyr) PEL01;
          If %found(WDELL) and Elpfdt > *zero;
             qqPolEfdt1 = %Char(%date(ELpfdt:*cymd):*iso0);
          Else;
             qqPolEfdt1 = *Zeros;
          Endif;
          qqClmId1 = %Editc(SCCase:'X');
          qqAcdt1 = %char(%date(%Editc(BFacdt:'X'):*usa0):*iso0);

          //New Policy Number
          Chain (xxCo#:xxFnd:SCyr2:CTemp#:CTdiv) PRP04;
          If %found(WMRPL4) and R4cap# <> *blanks;

             //*** Begin Chg *** 10/14/20 **************************************
             //Remove hyphens
             //***strCaP# = R4cap#;
             //***IntPos = %scan('-':strCaP#);
             //***If IntPos > *zero;
             //***   Dou IntPos = *zero;
             //***      strCaP# = %subst(strCaP#:1:IntPos-1) +
             //***                %subst(strCaP#:IntPos+1:25-IntPos);
             //***      IntPos = %scan('-':strCaP#);
             //***   Enddo;

             //***   qqPolId2 = strCaP#;
             //***Endif;
             qqPolId2 = FormatPolId(R4cap#);
             //*** End   Chg *** 10/14/20 **************************************
          Else;
             //*** Begin Chg *** 12/18/20 **************************************
             //qqPolId2 = %Editc(xxFnd:'X') +
             //           %Editc(CTemp#:'X') + %Editc(CTdiv:'X');
             qqPolId2 = %Editc(xxFnd:'X') +
                        %Editc(CTemp#:'X') + %Editc(SCyr2:'X');
             //*** End   Chg *** 12/18/20 **************************************
          Endif;

          chain(n) (xxCo#:xxFnd:CTemp#:CTdiv:SCyr2) PEL01;
          If %found(WDELL) and Elpfdt > *Zeros;
             qqPolEfdt2 = %Char(%date(ELpfdt:*cymd):*iso0);
             qqFyr2 = ELfyr;
          Else;
             qqPolEfdt2 = *Zeros;
             qqFyr2 = *Zeros;
          Endif;
          qqClmId2 = %Editc(CTcase:'X');
          qqAcdt2 = %char(%date(CTacdt:*cymd):*iso0);

          //*** Begin Chg *** 10/14/20 ****************************************
          //***If qqPolEfdt1 > *Zeros and qqPolEfdt2 > *Zeros;
          If qqPolId1 <> qqPolID2 or qqPolEfDt1 <> qqPolEfDt2;
          //*** End   Chg *** 10/14/20 ****************************************
             isIdcTrnOk =
             Transfer(qqCo#1:qqCar1:qqPolId1:qqPolEfdt1:qqClmId1:qqAcdt1:
                      qqCo#2:qqCar2:qqPolId2:qqPolEfdt2:qqClmId2:qqAcdt2:
                      qqFyr2);
          Endif;
       Endif;
      /End-Free
     C*
     C  N40              DELETE    PCM01
     C*
     C                   GOTO      CLEAR
     C                   END
     C*
     C* ---------------------------------------------------------------
     C*  IF CLAIM IS NOT BEING TRANSFERRED TO ANOTHER CASE & NO DETAIL
     C*  CHANGES WERE MADE, DO NOT UPDATE.
     C*
     C* Update SCZIP
     C                   Movel     scZip1        scZip
     C                   Move      scZip2        scZip
     C*
     C     BFEMP#        IFEQ      SCEMP#
     C     BFDIV         ANDEQ     SCDIV
     C     BFSS#         ANDEQ     SCSS#
     C     BFACDT        ANDEQ     SCACDT
     C     BFCLAS        ANDEQ     SCCLAS
     C     BFSPEC        ANDEQ     SCSPEC
     C     BFEXT         ANDEQ     SCEXT
     C     SVMDBN        ANDEQ     SCMDBN
     C     SVHICN        ANDEQ     SCHICN
     C     SVLSN         ANDEQ     SCLSN
     C     SVFSN         ANDEQ     SCFSN
     C     SVMID         ANDEQ     SCMID
     C     SVADR1        ANDEQ     SCADR1
     C     SVADR2        ANDEQ     SCADR2
     C     SVCTY         ANDEQ     SCCTY
     C     SVST          ANDEQ     SCST
     C     SVZIP         ANDEQ     SCZIP
     C     SVDTOB        ANDEQ     SCDTOB
     C     SVSEX         ANDEQ     SCSEX
     C     SVCCCD        ANDEQ     SCCCCD
     C     SVMSTS        ANDEQ     SCMSTS
     C     SVAREA        ANDEQ     SCAREA
     C     SVHPN1        ANDEQ     SCHPN1
     C     SVHPN2        ANDEQ     SCHPN2
     C     SVWARA        ANDEQ     SCWARA
     C     SVWPN1        ANDEQ     SCWPN1
     C     SVWPN2        ANDEQ     SCWPN2
     C     SVCARA        ANDEQ     SCCARA
     C     SVCPN1        ANDEQ     SCCPN1
     C     SVCPN2        ANDEQ     SCCPN2
     C     SVCTRY        ANDEQ     SCCTRY
     C     SVADR3        ANDEQ     SCADR3
     C                   GOTO      CLEAR
     C                   END
     C*
     C*  VALIDATE EMPLOYER NUMBER
     C*
     C                   MOVE      SCEMP#        XXEMP#
     C                   MOVE      SCDIV         XXDIV
     C     EMKEY         CHAIN     PEM01                              29
     C     *IN29         CABEQ     '1'           STRT
     C                   MOVEL     EMNAME        SCENAM
     C*
      /Free
       If SCemp# <> BFemp# or SCdiv <> BFdiv ;
       //*** Beg Chg *** 10/14/20 *********************************************
       //***   isNewIdc = *on;               //Policy number change
       If isModEnabled('IDC');
          xxBFyr   = BFyr;
          xxBFemp# = BFemp#;
          xxBFdiv  = BFdiv;
          xxCTyr   = scYr2;
          xxCTemp# = scEmp#;
          xxCTdiv  = scDiv;
          Exsr IdcQualify;
       Endif;
       //*** End Chg *** 10/14/20 *********************************************
       Endif;
      /End-Free
      *  IF THE EMPLOYER OR DIVISION WAS CHANGED,
      *  DISPLAY A WINDOW WHICH DISPLAYS CURRENT AND NEW TPA OFFICE#S.
      *
     C     SCUOF#        IFEQ      *BLANK
     C     SCEMP#        IFNE      BFEMP#
     C     SCDIV         ORNE      BFDIV
      *
      *  GET THE NEW EMPLOYER
      *
     C                   MOVE      *BLANKS       SCNEOF
     C                   MOVE      *BLANKS       SCNEOD
     C                   MOVEL(P)  EMOFF#        SCNEOF
      *
      *  GET THE OFFICE DESCRIPTION FROM THE OFFICE MASTER FILE
      *
     C                   MOVEL     EMOFF#        XXOFF#
     C                   MOVE      *BLANK        SCNEOD
     C     OFKY          CHAIN     POF01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     OFNAME        SCNEOD
     C                   END
     C                   MOVE      *BLANK        SCUOF#
      *
      * IF THE OFFICE# FOR THE CLAIM IS DIFFERENT FROM THE NEW
      * NEW EMPLOYER'S OFFICE#, DSP WINDOW MSG ASKING USER TO VERIFY
      * THAT ITS OK TO USE THE NEW OFFICE#.
      *
     C     SCCOFF        IFNE      SCNEOF
     C     *IN63         DOUEQ     *OFF
     C                   EXFMT     WDW02
     C                   MOVE      *OFF          *IN63
     C     SCUOF#        IFNE      'Y'
     C     SCUOF#        ANDNE     'N'
     C                   MOVE      *ON           *IN63
     C                   END
     C                   ENDDO
     C                   END
     C*
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C     *INKC         CABEQ     '1'           TERM
     C     *INKL         CABEQ     '1'           STRT
     C                   END
     C                   END
     C*
     C*
     C*  CHANGE SOCIAL SECURITY NUMBER, VALIDATE NEW CLAIMANT INFO
     C*
     C     SCSS#         IFNE      BFSS#
     C*
     C     SCSS#         IFEQ      *BLANKS
     C                   MOVE      *ON           *IN36
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      SCSS#         XXSS#
     C     CDKEY         CHAIN(N)  PCD01                              60
     C     *IN60         IFEQ      '0'
      *
      *  A CLAIMANT ALREADY EXISTS FOR THIS SS#, SO DISPLAY IT.
      *  PRESS ENTER TO CONTINUE PROCESSING  OR F12.
      *
     C                   MOVEL     SCSS#         QQSS#             9
     C                   MOVE      *BLANK        QQRTCD            1
     C                   CALL      'W0563R'
     C                   PARM                    QQPRDS
     C                   PARM      'T'           QQCALL
     C                   PARM                    QQSS#
     C                   PARM      *HIVAL        QXACDT            8
     C                   PARM      *ZEROS        QXFND             3 0
     C                   PARM      *ZEROS        QXFYR             3 0
     C                   PARM      *ZEROS        QXCASE            7 0
     C                   PARM                    QQRTCD
      *
     C                   MOVEL(P)  'SCR02'       Q2RECF
      *
     C     QQRTCD        IFEQ      'B'
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        CABEQ     'A'           TERM                 LR
     C*
     C                   EXFMT     WDW03
     C*
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C*
     C                   SELECT
     C     *INKL         WHENEQ    *ON
     C                   GOTO      STRT
     C*
     C     *INKC         WHENEQ    *ON
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
     C*
     C     *INKF         WHENEQ    *ON
     C*
     C                   MOVE      SCSS#         XXSS#
     C*
     C     CDKEY         CHAIN     PCD01                              60
     C*
     C                   MOVEL(P)  CDLSN         SCLSN
     C                   MOVEL(P)  CDFSN         SCFSN
     C                   MOVEL(P)  CDMID         SCMID
     C                   MOVEL(P)  CDADR1        SCADR1
     C                   MOVEL(P)  CDADR2        SCADR2
     C                   MOVEL(P)  CDCTY         SCCTY
     C                   MOVEL(P)  CDST          SCST
     C                   MOVEL     CDZIP         SCZIP1
     C                   MOVE      CDZIP         SCZIP2
     C                   MOVE      CDZIP         SCZIP
     C                   MOVEL(P)  CDCTRY        SCCTRY
     C                   MOVEL(P)  CDADR3        SCADR3
     C                   MOVE      CDMSTS        SCMSTS
     C                   MOVE      CDCNTY        SCCCCD
     C                   Z-ADD     CDAREA        SCAREA
     C                   MOVEL     CDPHN#        SCHPN1
     C                   MOVE      CDPHN#        SCHPN2
     C                   Z-ADD     CDWARA        SCWARA
     C                   MOVEL     CDWPH#        SCWPN1
     C                   MOVE      CDWPH#        SCWPN2
     C                   Z-ADD     CDCARA        SCCARA
     C                   MOVEL     CDCPHN        SCCPN1
     C                   MOVE      CDCPHN        SCCPN2
     C*
     C                   Z-ADD     CDDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDTOB
     C*
     C                   MOVEL     CDSEX         SCSEX
     C*
     C                   MOVEL(P)  CDLSN         QQLAST
     C                   MOVEL(P)  CDFSN         QQFRST
     C*
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM      '1'           QQTERM
     C*
     C                   MOVEL(P)  QQLAST        SCCDNM
     C*
     C                   MOVE      CDMDBN        SCMDBN
     C                   MOVE      CDHICN        SCHICN
     C*
     C                   ENDSL
      *
      *  MAKE SURE WE'RE USING THE OLD SS#
      *
     C                   MOVE      *ZERO         BFSS#N            9 0
     C                   TESTN                   BFSS#                6061
     C     *IN60         IFEQ      *ON
     C     *IN61         OREQ      *ON
     C                   MOVE      BFSS#         BFSS#N
     C                   ENDIF
     C*
     C                   MOVEL     CDLSN         QQLAST
     C                   MOVEL     CDFSN         QQFRST
     C                   MOVE      '1'           QQTERM
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        SCCDNM
     C*
     C                   ENDIF
     C*
     C                   ENDIF

     C*
     C* Medical Beneficiary flag edits
     C     SCMDBN        IFNE      *BLANKS
     C     SCMDBN        ANDNE     'Y'
     C                   MOVE      *ON           *IN77
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  THE CLAIMANT NAME CANNOT BE BLANK
     C*
     C     SCLSN         IFEQ      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C     SCLSN         IFNE      *BLANK
     C                   MOVEL(P)  SCLSN         XXLSN            30
     C                   MOVEA     XXLSN         OCD1
     C                   Z-ADD     1             I                 2 0
     C                   Z-ADD     0             SNPOS             2 0
     C                   DO        30
     C*
     C     OCD1(I)       SCAN      CONOC1        SNPOS             2 0
     C*
     C                   ADD       1             I
     C*
     C     SNPOS         IFEQ      0
     C                   SETON                                        9078
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDDO
     C                   ENDIF
     C     SCFSN         IFEQ      *BLANK
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  ADDRESS CANNOT BE BLANK
     C*
     C     SCADR1        IFEQ      *BLANK
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
      *
      *  BOTH THE C,S,Z IS AND OUT OF COUNTRY ADDRESS CANNOT BE ENTERED
      *
     C     SCCTY         IFNE      *BLANKS
     C     SCST          ANDNE     *BLANKS
     C     SCZIP1        ANDNE     *ZERO
     C     SCCTRY        ANDNE     *BLANK
     C     SCADR3        ANDNE     *BLANK
      *
     C     SCCTY         ORNE      *BLANKS
     C     SCST          ANDNE     *BLANKS
     C     SCZIP1        ANDNE     *ZERO
     C     SCCTRY        ANDNE     'USA'
     C     SCADR3        ANDNE     *BLANK
      *
     C     SCCTY         ORNE      *BLANKS
     C     SCST          ANDNE     *BLANKS
     C     SCZIP1        ANDNE     *ZERO
     C     SCCTRY        ANDNE     'USA'
     C     SCCTRY        ANDNE     *BLANK
     C     SCADR3        ANDNE     *BLANK
      *
     C     SCCTY         ORNE      *BLANKS
     C     SCCTRY        ANDNE     'USA'
     C     SCCTRY        ANDNE     *BLANK
     C     SCADR3        ANDNE     *BLANK
      *
     C     SCST          ORNE      *BLANKS
     C     SCCTRY        ANDNE     'USA'
     C     SCCTRY        ANDNE     *BLANK
     C     SCADR3        ANDNE     *BLANK
      *
     C     SCZIP1        ORNE      *ZERO
     C     SCCTRY        ANDNE     'USA'
     C     SCCTRY        ANDNE     *BLANK
     C     SCADR3        ANDNE     *BLANK
     C                   SETON                                        9030
     C                   GOTO      STRT
     C                   ENDIF
      *
      *  EITHER THE C,S,Z IS ENTERED OR THE OUT OF COUNTRY ADDRESS
      *
     C     SCCTY         IFEQ      *BLANKS
     C     SCST          ANDEQ     *BLANKS
     C     SCZIP1        ANDEQ     *ZERO
     C     SCCTRY        ANDEQ     *BLANK
     C     SCADR3        ANDEQ     *BLANK
     C                   SETON                                        9073
     C                   GOTO      STRT
     C                   ENDIF
      *
     C     SCCTRY        IFNE      *BLANK
     C     SCCTRY        ANDNE     'USA'
      *
     C     SCADR3        IFEQ      *BLANK
     C                   SETON                                        9073
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
      *
     C     SCCTRY        IFEQ      *BLANKS
     C     SCCTRY        OREQ      'USA'
     C*
     C*  THE CITY CANNOT BE BLANK
     C*
     C     SCCTY         IFEQ      *BLANK
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*  THE STATE MUST BE VALID
     C*
     C                   MOVEL(P)  SCST          XXKEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C*  THE ZIP CODE CANNOT BE ZERO
     C*
     C     SCZIP1        IFLE      *ZERO
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C                   ENDIF
     C*
     C     SCCTRY        IFNE      *BLANK
     C                   MOVEL(P)  'CTRY'        XXTBID
     C                   MOVEL(P)  SCCTRY        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   SETON                                        9072
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCADR3        IFEQ      *BLANK
     C     SCCTRY        IFNE      *BLANK
     C     SCCTRY        ANDNE     'USA'
     C                   SETON                                        9074
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
      *
      *  CHECK MARITAL STATUS
      *
     C     SCMSTS        IFNE      *BLANKS
     C                   MOVEL(P)  'MSTS'        XXTBID
     C                   MOVEL(P)  SCMSTS        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   SETON                                        54
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
      *
      *
      *  CHECK COUNTY CODE
      *
     C     SCCCCD        IFNE      *BLANK
     C     SCST          CAT(P)    SCCCCD:0      XXKEY
     C     TBKY          CHAIN     PCN01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        55
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
     C*  EDIT BIRTH DATE
     C*
     C     SCDTOB        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDTOB
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C*
     C                   ENDIF
     C*
     C*  EDIT SEX
     C*
     C     SCSEX         IFNE      'F'
     C     SCSEX         ANDNE     'M'
     C     SCSEX         ANDNE     *BLANKS
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE THE ACCIDENT DATE
     C*
     C                   Z-ADD     SCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCACDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
      *
     C                   Z-ADD     SCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXACDT
      *
      *  ACC DT MUST BE LESS THAN SYS DT
      *
     C     XXACDT        IFGT      XXDT
     C                   MOVE      *ON           *IN44
     C                   GOTO      STRT
     C                   END
      *
      *  ACC DT MUST BE LESS THAN CLAIMS INITIAL ENTRY DT
      *
     C     XXACDT        IFGT      C1IDT
     C     C1IDT         ANDNE     0
     C     XXACDT        ORGT      CMENTD
     C     CMENTD        ANDNE     0
     C                   MOVE      *ON           *IN45
     C                   GOTO      STRT
     C                   END
     C*
     C* WE HAVE PASSED ALL EDITS
     C*
     C     CXKY          CHAIN     PCX01                              99
      *
     C     CXAADP        IFNE      *BLANKS
     C                   EXSR      AUDIT
      *
     C     @@AFL         IFEQ      'Y'
     C     ##RTN         ANDNE     'Y'
     C                   SETON                                        90
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
      *
     C*  If the accident date changed, calculate the FYR using the
     C*  Employer Detail file.
     C*
     C                   Z-ADD     XXACDT        XXPFDT
     C     ELKY2         SETGT     PEL02
     C     ELKY2B        READPE    PEL02                                  60
     C  N60              Z-ADD     ELFYR         SCYR
      *
     C     *IN60         IFEQ      '1'
     C     ELPTDT        ORLE      XXPFDT
     C                   MOVE      '1'           *IN33
     C*
     C     F1KY1         SETGT     PF102
     C     FNKEY         READPE    PF102                                  60
     C     *IN60         IFEQ      '0'
     C     F1ENPO        ANDGT     XXACDT
     C                   Z-ADD     F1FYR         SCYR
     C                   ELSE
     C     XXACDT        DIV       10000         SCYR
     C                   END
     C*
     C                   END
     C*
     C*  Retrieve the Group Detail record to determine if emp coverage
     C*  edits should be invoked.
     C*
     C                   MOVE      SCYR          XXFYR
     C     F1KY          CHAIN     PF101                              60
     C     *IN60         IFEQ      '1'
     C     F1CLMS        ORNE      'Y'
     C*
     C                   EXSR      SRCOV
     C*
     C     *IN33         IFEQ      *ON
     C     *IN57         OREQ      *ON
     C     *IN59         OREQ      *ON
     C     *IN83         OREQ      *ON
     C                   EXSR      SROVR
     C                   END
     C*
     C                   END
     C*
     C     ##MCIF        IFEQ      'Y'
     C     ##WCIN        ANDEQ     'Y'
     C*
     C*   FIRST YOU NEED TO GET THE RRN ON THE MANAGED CARE SIDE
     C*   OF THE CASE YOU ARE CHANGING
     C*
     C                   MOVE      XXCO#         XYCO#             3
     C                   MOVE      XXFND         XYFND             3
     C                   MOVE      BFYR          XYFYR             3
     C                   MOVE      SCCASE        XYCASE            7
     C*
     C                   CALL      'FA201R'
     C                   PARM                    XYCO#
     C                   PARM                    XYFND
     C                   PARM                    XYFYR
     C                   PARM                    XYCASE
     C                   PARM      *ZEROS        XYRRN1            9 0
     C                   PARM      *BLANK        XYRTCD            1
     C                   Z-ADD     XYRRN1        XYRRN2
     C*
     C*  NOW YOU HAVE TO CLEAR OUT AND CREATE TEMP FILES
     C*
     C                   MOVEL(P)  'FAT3PQ'      #FNAME
     C                   MOVEL(P)  'FAT3PQ'      #TNAME
     C*
     C                   CALL      'FA710C'
     C                   PARM      ' '           ##RTN             1
     C                   PARM                    #FNAME           10
     C                   PARM                    #TNAME           10
     C*
     C                   MOVEL(P)  'FAT3LQ'      #FNAME
     C                   MOVEL(P)  'FAT3LQ'      #TNAME
     C*
     C                   CALL      'FA710C'
     C                   PARM      ' '           ##RTN             1
     C                   PARM                    #FNAME           10
     C                   PARM                    #TNAME           10
     C*
     C*
     C*   NOW YOU CALL F7090R TO SET UP THE TRANSFER
     C*
     C                   CALL      'F7090R'
     C                   PARM      ' '           ##CALL            1
     C                   PARM                    ##KL              1
     C                   PARM      XYRRN1        ##RRN             9 0
     C                   PARM      *ZEROS        ##RRN1            9 0
     C                   PARM      *ZEROS        ##RRN2            9 0
     C                   PARM      *ZEROS        ##RRN3            9 0
     C                   PARM      'T'           ##TD              1
     C                   PARM      'X'           ##FLAG            1
     C                   PARM      'Y'           ##WCMS            1
     C*
     C     ##KL          IFEQ      *ON
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDIF
     C* ---------------------------------------------------------------
     C*  CHECK CLAIM # ON FUND YEAR CHANGE
     C*
     C                   MOVE      SCCASE        BFCASE
     C     SCYR          IFNE      BFYR
     C*
     C                   Z-ADD     SCYR          XXFYR
     C                   Z-ADD     SCCASE        XXCASE
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*  ON AN ADD, ASSIGN CASE NUMBER
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     FNNACS        IFEQ      *ZERO
     C                   Z-ADD     1             FNNACS
     C                   END
     C                   MOVE      FNNACS        SCCASE
     C                   ADD       1             FNNACS
      *** Beg Add ***** 05/23/24 ***********************
     C                   MOVE      Q1USER        FNUSER
     C                   MOVE      Q1PGM         FNPGM
     C                   Z-ADD     XXDT          FNDT
     C                   MOVE      DSTIME        FNTIME
      *** End Add ***** 05/23/24 ***********************
     C                   UPDATE    PFN01
     C*
     C                   Z-ADD     SCCASE        XXCASE
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* ---------------------------------------------------------------
     C*  WRITE/UPDATE CLAIM MASTER
     C*
     C                   Z-ADD     BFYR          XXFYR
     C                   MOVE      BFCASE        XXCASE
     C     CMKY          CHAIN     PCM01                              40
     C                   Z-ADD     SCYR          CMFYR
     C                   MOVE      SCCASE        CMCASE
     C                   MOVE      SCEMP#        CMEMP#
     C                   MOVE      SCDIV         CMDIV
     C*
     C                   MOVEL(P)  SCLSN         CMLSN
     C                   MOVEL(P)  SCFSN         CMFSN
     C                   MOVEL(P)  SCMID         CMMID
     C*
     C     CMEADR        IFEQ      'Y'
     C*
     C                   MOVEL(P)  EMNAME        CMACNM
     C*
     C     EMPAD1        IFNE      *BLANK
     C                   MOVEL(P)  EMPAD1        CMACDR
     C                   MOVEL(P)  EMPCTY        CMACTY
     C                   MOVEL(P)  EMPST         CMAST
     C                   MOVEL     EMPZIP        CMACZ1
     C                   MOVE      EMPZIP        CMACZ2
     C                   ELSE
     C                   MOVEL(P)  EMMAD1        CMACDR
     C                   MOVEL(P)  EMMCTY        CMACTY
     C                   MOVEL(P)  EMMST         CMAST
     C                   MOVEL     EMMZIP        CMACZ1
     C                   MOVE      EMMZIP        CMACZ2
     C                   ENDIF
     C*
     C                   ENDIF
     C***  END   ADD  ***  04/30/98  *********************************
     C*
     C     SCUOF#        IFEQ      'Y'
     C                   MOVEL     SCNEOF        CMOFF#
     C                   END
     C     SCEMP#        IFNE      BFEMP#
     C     SCDIV         ORNE      BFDIV
     C                   MOVE      *BLANKS       CMDEPT
     C                   END
     C                   Z-ADD     XXACDT        CMACDT
     C                   MOVE      SCSS#         CMSS#
     C                   MOVE      SCCLAS        CMCLAS
     C                   MOVEL     SCSPEC        CMSPEC
     C                   MOVEL     SCEXT         CMEXT
     C                   MOVE      Q1USER        CMUSER
     C                   MOVE      Q1PGM         CMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     XXDT          CMDT
     C                   Z-ADD     DSTIME        CMTIME
      *
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PCM01
     C                   END
      *
      *  UPDATE WMCDP
      *
     C     CDKEY         CHAIN     PCD01                              40
     C                   MOVE      SCMSTS        CDMSTS
     C                   MOVEL     SCCCCD        CDCNTY
     C                   Z-ADD     SCAREA        CDAREA
     C                   MOVEL     SCHPN1        CDPHN#
     C                   MOVE      SCHPN2        CDPHN#
     C                   Z-ADD     SCWARA        CDWARA
     C                   MOVEL     SCWPN1        CDWPH#
     C                   MOVE      SCWPN2        CDWPH#
     C                   Z-ADD     SCCARA        CDCARA
     C                   MOVEL     SCCPN1        CDCPHN
     C                   MOVE      SCCPN2        CDCPHN
     C                   MOVE      SCMDBN        CDMDBN
     C                   MOVE      SCHICN        CDHICN
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PCD01
     C                   END
     C*
     C*  MOVE FIELDS FOR CLAIMANT DEMOGRAPHICS
     C*
     C     SCSS#         IFNE      BFSS#
     C     SCMDBN        ORNE      SVMDBN
     C     SCHICN        ORNE      SVHICN
     C     SCLSN         ORNE      SVLSN
     C     SCFSN         ORNE      SVFSN
     C     SCMID         ORNE      SVMID
     C     SCADR1        ORNE      SVADR1
     C     SCADR2        ORNE      SVADR2
     C     SCCTY         ORNE      SVCTY
     C     SCST          ORNE      SVST
     C     SCZIP         ORNE      SVZIP
     C     SCDTOB        ORNE      SVDTOB
     C     SCSEX         ORNE      SVSEX
     C     SCMSTS        ORNE      SVMSTS
     C     SCCCCD        ORNE      SVCCCD
     C     SCAREA        ORNE      SVAREA
     C     SCHPN1        ORNE      SVHPN1
     C     SCHPN2        ORNE      SVHPN2
     C     SCWARA        ORNE      SVWARA
     C     SCWPN1        ORNE      SVWPN1
     C     SCWPN2        ORNE      SVWPN2
     C     SCCARA        ORNE      SVCARA
     C     SCCPN1        ORNE      SVCPN1
     C     SCCPN2        ORNE      SVCPN2
     C     SCCTRY        ORNE      SVCTRY
     C     SCADR3        ORNE      SVADR3
     C                   MOVE      SCSS#         XXSS#
     C     CDKEY         CHAIN     PCD01                              41
      *
     C*
     C*   First make change to Payment Requests on this claim.
     C                   EXSR      SRPRCH
     C*
      *
     C                   MOVE      SCSS#         CDSS#
     C                   MOVE      SCLSN         CDLSN
     C                   MOVE      SCFSN         CDFSN
     C                   MOVE      SCMID         CDMID
     C                   MOVE      SCADR1        CDADR1
     C                   MOVE      SCADR2        CDADR2
     C                   MOVEL     SCCTY         CDCTY
     C                   MOVE      SCST          CDST
     C                   MOVEL     SCZIP1        CDZIP
     C                   MOVE      SCZIP2        CDZIP
     C                   MOVE      SCMDBN        CDMDBN
     C                   MOVE      SCHICN        CDHICN
     C                   MOVEL(P)  SCCTRY        CDCTRY
     C                   MOVEL(P)  SCADR3        CDADR3
     C                   Z-ADD     SCDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CDDTOB
     C                   MOVE      SCSEX         CDSEX
     C                   MOVE      SCMSTS        CDMSTS
     C                   Z-ADD     SCAREA        CDAREA
     C                   MOVEL     SCHPN1        CDPHN#
     C                   MOVE      SCHPN2        CDPHN#
     C                   Z-ADD     SCWARA        CDWARA
     C                   MOVEL     SCWPN1        CDWPH#
     C                   MOVE      SCWPN2        CDWPH#
     C                   Z-ADD     SCCARA        CDCARA
     C                   MOVEL     SCCPN1        CDCPHN
     C                   MOVE      SCCPN2        CDCPHN
     C                   MOVEL     SCCCCD        CDCNTY
     C                   MOVE      *BLANK        CDSTAT
     C                   MOVE      Q1USER        CDUSER
     C                   MOVE      Q1PGM         CDPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     XXDT          CDDT
     C                   Z-ADD     DSTIME        CDTIME
     C*
     C     *IN41         IFEQ      *ON
     C                   WRITE     PCD01
     C                   ELSE
     C                   UPDATE    PCD01
     C                   ENDIF
      /Free
       // Send welcome text message if cell phone number changed
       //  and Relay module is enabled and SMS not disabled
       isRlyEnbld = isModEnabled('RelaySMS');
       isSmsDsbld = isSmsDisabled('CS':rrnClm1:'OW':'W7265R');
       Select;
       When not(isRlyEnbld);
       When isSmsDsbld;
       When svCAra <> scCAra or svCPn1 <> scCPn1 or svCPn2 <> scCPn2
        and scCPn1 <> *zeros;
          ProcRelayReq('CS':rrnClm1:'OW');
       Endsl;
      /End-free
      *
     C*  UPDATE CLAIMANT NAME FIELDS IN CLAIM MASTER.    *
     C*
     C*  Call API to update Managed Care - FMCDP
     C*
     C                   MOVE      XXCO#         QQCO#
     C                   MOVE      XXFND         QQFND
     C                   MOVE      XXFYR         QQFYR
     C                   MOVE      XXCASE        QQCASE
     C*
     C                   MOVE      CMACDT        QQACDT
     C*
     C                   MOVE      CDZIP         QQZIP
     C                   MOVE      CDDTOB        QQDTOB
     C                   MOVE      CDAREA        QQAREA
     C                   MOVE      CDPHN#        QQPHN#
     C*
     C                   MOVE      CDWARA        QQWARA
     C                   MOVE      CDWPH#        QQWPH#
     C*
     C                   MOVE      CDCARA        QQCARA
     C                   MOVE      CDCPHN        QQCPHN
     C*
     C                   EXSR      MCAPI
     C*
      *
      *  UPDATE ALL THE PAYEE FEDERAL ID#S, FOR THE SAME KEY VALUES,
      *  IN THE PAYMENT DETAIL FILE   WITH THE NEW SS#.
      *
     C     BFSS#N        IFNE      *ZERO
     C*
     C     PAKEY         CHAIN     PPA01                              60
     C     *IN60         DOWEQ     *OFF
     C     BFSS#N        IFEQ      PAFEIN
     C                   MOVE      *BLANKS       PAFEIN
     C                   MOVE      SCSS#         PAFEIN
     C                   MOVE      Q1USER        PAUSER
     C                   MOVE      Q1PGM         PAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     XXDT          PADT
     C                   Z-ADD     DSTIME        PATIME
     C                   UPDATE    PPA01
     C                   END
     C     PAKEY         READE     PPA01                                  60
     C                   ENDDO
     C*
     C                   ENDIF
     C*
     C                   END
     C*
     C*   IF NAME OR ADDRESS CHANGED,
     C*   WRITE RECORD TO CLAIMANT NAME CHANGE TRACKING FILE
     C*
     C                   MOVE      *BLANK        XTYPE1
     C                   MOVE      *BLANK        XTYPE2
     C     SCLSN         IFNE      SVLSN
     C     SCLSN         ANDNE     *BLANK
     C     SCFSN         ORNE      SVFSN
     C     SCFSN         ANDNE     *BLANK
     C     SCMID         ORNE      SVMID
     C     SCMID         ANDNE     *BLANK
     C                   MOVE      'N'           XTYPE1
     C                   MOVE      'N'           NTTYPE
     C                   END
     C*
     C     SCADR1        IFNE      SVADR1
     C     SCADR1        ANDNE     *BLANK
     C     SCADR2        ORNE      SVADR2
     C     SCADR2        ANDNE     *BLANK
     C     SCCTY         ORNE      SVCTY
     C     SCCTY         ANDNE     *BLANK
     C     SCST          ORNE      SVST
     C     SCST          ANDNE     *BLANK
     C     SCZIP         ORNE      SVZIP
     C     SCZIP         ANDNE     *ZERO
     C                   MOVE      'A'           XTYPE2
     C                   MOVE      'A'           NTTYPE
     C                   END
     C*
     C     XTYPE1        IFNE      *BLANK
     C     XTYPE2        ANDNE     *BLANK
     C                   MOVE      'B'           NTTYPE
     C                   END
     C                   MOVE      '0'           *IN82
     C     SCLSN         IFEQ      *BLANK
     C     SCFSN         ANDEQ     *BLANK
     C     SCMID         ANDEQ     *BLANK
     C     SCADR1        ANDEQ     *BLANK
     C     SCADR2        ANDEQ     *BLANK
     C     SCCTY         ANDEQ     *BLANK
     C     SCST          ANDEQ     *BLANK
     C     SCZIP         ANDEQ     *ZERO
     C     SCCTRY        ANDEQ     *BLANK
     C     SCADR3        ANDEQ     *BLANK
     C                   MOVE      '1'           *IN82
     C                   END
     C     NTTYPE        IFNE      *BLANK
     C                   MOVE      *BLANKS       QQFMN            15
     C                   MOVE      *BLANKS       QQFSN
     C                   MOVE      *BLANKS       QQMID            56
     C                   MOVE      '1'           QQTERM
     C                   MOVEL     SVFSN         QQFSN
     C                   MOVEL     SVMID         QQMID
     C                   CALL      'SRFRLS'
     C                   PARM                    QQMID
     C                   PARM                    QQFSN
     C                   PARM                    QQTERM
     C                   MOVEL     QQMID         QQFMN
     C*
     C                   MOVEL     '1'           QQTERM
     C                   MOVE      *BLANK        QQLSN
     C                   MOVEL     SVLSN         QQLSN
     C                   CALL      'SRNAME'
     C                   PARM                    QQLSN
     C                   PARM                    QQFMN
     C                   PARM                    QQTERM
     C                   MOVEL     QQLSN         NTONAM
     C*
     C                   MOVE      *BLANKS       QQFMN            15
     C                   MOVE      *BLANKS       QQFSN            15
     C                   MOVE      *BLANKS       QQMID            56
     C                   MOVE      '1'           QQTERM            1
     C                   MOVEL     SCFSN         QQFSN
     C                   MOVEL     SCMID         QQMID
     C   82              MOVEL     SVFSN         QQFSN
     C   82              MOVEL     SVMID         QQMID
     C                   CALL      'SRFRLS'
     C                   PARM                    QQMID
     C                   PARM                    QQFSN
     C                   PARM                    QQTERM
     C                   MOVEL     QQMID         QQFMN
     C*
     C                   MOVEL     '1'           QQTERM            1
     C                   MOVE      *BLANK        QQLSN            56
     C                   MOVEL     SCLSN         QQLSN
     C   82              MOVEL     SVLSN         QQLSN
     C                   CALL      'SRNAME'
     C                   PARM                    QQLSN
     C                   PARM                    QQFMN
     C                   PARM                    QQTERM
     C                   MOVEL     QQLSN         NTNNAM
     C                   MOVEL     SVADR1        NTOAD1
     C                   MOVEL     SVADR2        NTOAD2
     C                   MOVEL     SVST          NTOST
     C                   MOVEL     SVCTY         NTOCTY
     C                   MOVEL     SVZIP         NTOZIP
     C                   MOVEL     SCADR1        NTNAD1
     C                   MOVEL     SCADR2        NTNAD2
     C                   MOVEL     SCST          NTNST
     C                   MOVEL     SCCTY         NTNCTY
     C                   MOVEL     SCZIP         NTNZIP
     C   82              MOVEL     SVADR1        NTNAD1
     C   82              MOVEL     SVADR2        NTNAD2
     C   82              MOVEL     SVST          NTNST
     C   82              MOVEL     SVCTY         NTNCTY
     C   82              MOVEL     SVZIP         NTNZIP
     C   82              MOVEL     SCCTRY        NTCTRY
     C   82              MOVEL     SCADR3        NTADR3
     C                   Z-ADD     XXCO#         NTCO#
     C                   Z-ADD     XXFND         NTFND
     C                   Z-ADD     SCYR          NTFYR
     C                   MOVE      SCCASE        NTCASE
     C                   MOVE      'A'           NTSTAT
     C                   TIME                    DSDTTM
     C                   Z-ADD     XXDT          NTDT
     C                   MOVE      NTDT          NTTRDT
     C                   MOVE      Q1USER        NTUSER
     C                   MOVE      Q1PGM         NTPGM
     C                   Z-ADD     DSTIME        NTTIME
     C                   WRITE     PNT01
     C                   END
     C*
     C*  If the claimant name was changed, update all claims with the
     C*    same SS#.
     C*
     C     NTTYPE        IFEQ      'N'
     C     NTTYPE        OREQ      'B'
     C     SCSS#         ORNE      BFSS#
     C*
     C                   MOVEL     SCSS#         XXSS#
     C     CMKYB         CHAIN     PCM0B                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C                   MOVEL     SCFSN         CMFSN
     C                   MOVEL     SCMID         CMMID
     C                   MOVEL     SCLSN         CMLSN
     C                   MOVEL     Q1USER        CMUSER
     C                   MOVEL     Q1PGM         CMPGM
     C                   Z-ADD     XXDT          CMDT
     C                   Z-ADD     DSTIME        CMTIME
     C                   UPDATE    PCM0B
     C                   Z-ADD     CMCO#         XXCO1#
     C                   Z-ADD     CMFND         XXFND1
     C                   Z-ADD     CMFYR         XXFYR1
     C                   Z-ADD     CMCASE        XXCAS1
     C                   EXSR      DA700R
     C*
     C     CMKYB         READE     PCM0B                                  60
     C                   ENDDO
     C*** Set update claims security file for Change name
     C     CUKY          CHAIN     PCU01                              60
     C     *IN60         IFEQ      *OFF
     C*** Build new claimant name
     C     SCLSN         CAT(P)    ',':0         XXCNM1           45
     C                   CAT       SCFSN:1       XXCNM1
     C                   CAT       SCMID:1       XXCNM1
     C                   ENDIF
     C     *IN60         DOWEQ     *OFF
     C*** Update all records for this claim
     C                   MOVEL(P)  XXCNM1        CUCNAM
     C                   UPDATE    PCU01
     C     CUKY          READE     PCU01                                  60
     C                   ENDDO
     C                   CALL      'WA085R'
     C                   PARM      'C'           XXTYPE            1
     C                   PARM      DBRRN         XXRRN             9 0
     C                   PARM      *BLANK        XXRTN             1
     C*
     C*  Reset the claim fields back to the original claim.
     C*
     C     CMKY          CHAIN(N)  PCM01                              40
     C*
     C                   ENDIF
     C*
     C*   IF NAME CHANGED,
     C*   WRITE RECORD TO DARTS REMARK FILE
     C*
     C*
     C*  WRITE WORK FILE RECORD
     C*
     C                   MOVE      CMCO#         CTCO#
     C                   MOVE      SCFND         CTFND
     C                   Z-ADD     SCYR          CTFYR
     C                   MOVE      SCCASE        CTCASE
     C                   MOVE      SCEMP#        CTEMP#
     C                   MOVE      SCDIV         CTDIV
     C                   Z-ADD     XXACDT        CTACDT
     C                   MOVE      SCSS#         CTSS#
     C                   MOVE      SCCLAS        CTCLAS
     C                   Z-ADD     BFYR          CTXFYR
     C                   MOVE      BFCASE        CTXCAS
     C                   MOVE      BFEMP#        CTXEMP
     C                   MOVE      BFDIV         CTXDIV
     C                   Z-ADD     BFACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CTXACD
     C                   MOVE      BFSS#         CTXSS#
     C                   MOVE      BFCLAS        CTXCLS
     C                   MOVE      SCREQ         CTREQ
     C                   Z-ADD     *ZERO         CTDET#
     C                   MOVE      Q1USER        CTUSER
     C                   MOVE      Q1PGM         CTPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     XXDT          CTDT
     C                   Z-ADD     DSTIME        CTTIME
     C                   Z-ADD     XXDT          CTTRDT
     C*
     C*  UPDATE WDELP CLM TOTALS IF EMPLOYER CHANGES
     C*
     C     SCEMP#        IFNE      BFEMP#
     C     SCDIV         ORNE      BFDIV
      /Free
       isNewIdc = *on;                          //Policy number hange
      /End-Free
     C                   EXSR      CHGEL
     C*
     C                   END
      /Free
       If SCacdt <> BFacdt;
          isNewIdc = *on;                       //Accident date change
       Endif;

      /End-Free
     C*
     C* ---------------------------------------------------------------
     C*  FUND YEAR OR CASE CHANGE, UPDATE CLAIM'S FILES
     C*
     C                   MOVE      SCCASE        XXWK70
     C     XXWK70        IFNE      BFCASE
     C     SCYR          ORNE      BFYR
     C                   Z-ADD     BFYR          XXFYR
     C                   Z-ADD     BFCASE        XXCASE
      /Free
       isNewIdc = *on;                                   //Claim number change
      /End-Free
     C*
     C*  FIX THE AWW HISTORY FILE
     C*
     C     CMKY          CHAIN     PAW01                              60
     C     *IN60         DOWEQ     '0'
     C                   MOVE      SCYR          AWFYR
     C                   MOVE      SCCASE        AWCASE
     C                   UPDATE    PAW01
     C     CMKY          READE     PAW01                                  60
     C                   END
     C*
     C*  FIX THE BODY PARTS FILE
     C*
     C     CMKY          CHAIN     PBY01                              60
     C     *IN60         DOWEQ     '0'
     C                   MOVE      SCYR          BYFYR
     C                   MOVE      SCCASE        BYCASE
     C                   UPDATE    PBY01
     C     CMKY          READE     PBY01                                  60
     C                   END
     C*
     C*  FIX THE BENEFIT ADJUSTMENT FILE
     C*
     C     CMKY          CHAIN     PBN01                              60
     C     *IN60         DOWEQ     '0'
     C                   MOVE      SCYR          BNFYR
     C                   MOVE      SCCASE        BNCASE
     C                   UPDATE    PBN01
     C     CMKY          READE     PBN01                                  60
     C                   END
     C*
     C*  FIX THE EDI SUBMISSION FILE, IF EDI IS INSTALLED.
     C*
     C     QQEDI         IFEQ      'Y'
     C*
     C     CMKY          CHAIN     PNI01                              60
     C     *IN60         DOWEQ     '0'
     C                   MOVE      SCYR          NIFYR
     C                   MOVE      SCCASE        NICASE
     C                   UPDATE    PNI01
     C     CMKY          READE     PNI01                                  60
     C                   END
     C*
     C                   END
     C*
     C*  UPDATE DIARY DATE FILE
     C*
     C     DDKEY         CHAIN     PDD01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     SCYR          DDFYR
     C                   MOVE      SCCASE        DDCASE
     C                   UPDATE    PDD01
     C                   END
     C*
     C*  UPDATE THE RESERVE HISTORY FILE
     C*
     C     RHKEY         CHAIN     PRH01                              60
     C     *IN60         DOWEQ     '0'
     C                   MOVE      SCYR          RHFYR
     C                   MOVE      SCCASE        RHCASE
     C                   ADD       1             CTDET#
     C                   UPDATE    PRH01
     C     RHKEY         READE     PRH01                                  60
     C                   END
     C*
     C*  FIX STATUS CLAIM HISTORY
     C*
     C     CHKEY         CHAIN     PCH01                              60
     C     *IN60         DOWEQ     '0'
     C                   MOVE      SCYR          CHFYR
     C                   MOVE      SCCASE        CHCASE
     C                   ADD       1             CTDET#
     C                   UPDATE    PCH01
     C     CHKEY         READE     PCH01                                  60
     C                   END
     C*
     C*  FIX TRANSFER HISTORY
     C*
     C     THKEY         CHAIN     PTH01                              60
     C     *IN60         DOWEQ     '0'
     C                   MOVE      SCYR          THFYR
     C                   MOVE      SCCASE        THCASE
     C                   UPDATE    PTH01
     C     THKEY         READE     PTH01                                  60
     C                   END
     C*
     C*  FIX WORK HISTORY
     C*
     C     WHKEY         CHAIN     PWH01                              60
     C     *IN60         DOWEQ     '0'
     C                   MOVE      SCYR          WHFYR
     C                   MOVE      SCCASE        WHCASE
     C                   UPDATE    PWH01
     C     WHKEY         READE     PWH01                                  60
     C                   END
     C*
     C*  FIX LATE PAYMENT FILE
     C*
     C     LPKEY         CHAIN     PLP01                              60
     C     *IN60         DOWEQ     '0'
     C                   MOVE      SCYR          LPFYR
     C                   MOVE      SCCASE        LPCASE
     C                   UPDATE    PLP01
     C     LPKEY         READE     PLP01                                  60
     C                   END
     C*
     C*  Fix the Claim Index records
     C*
     C     LPKEY         CHAIN     PID01                              60
     C     *IN60         DOWEQ     '0'
     C                   MOVE      SCYR          IDFYR
     C                   MOVE      SCCASE        IDCASE
     C                   UPDATE    PID01
     C     LPKEY         READE     PID01                                  60
     C                   END
     C*
     C                   Z-ADD     *ZERO         XXPAY
     C*
     C*  FIX REQUEST PAYMENT
     C*
     C     RPKEY         CHAIN     PRP01                              60
     C     *IN60         DOWEQ     '0'
     C                   MOVE      SCYR          RPFYR
     C                   MOVE      SCCASE        RPCASE
     C                   ADD       1             CTDET#
     C                   UPDATE    PRP01
     C     RPKEY         READE     PRP01                                  60
     C                   END
     C*                                                    JJJ 12-01-88
     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C*  FIX PAYMENT DETAIL
     C*
     C     PAKEY         CHAIN     PPA01                              60
     C     *IN60         DOWEQ     '0'
     C     PATYPE        IFEQ      'R'
     C     PATYPE        OREQ      'V'
     C     PATYPE        OREQ      'S'
     C                   SUB       PAAMT         XXPAY
     C                   ELSE
     C                   ADD       PAAMT         XXPAY
     C                   END
     C                   MOVE      SCYR          PAFYR
     C                   MOVE      SCCASE        PACASE
     C                   Z-ADD     PASEQ#        SVSEQ#
     C                   ADD       1             CTDET#
     C*
     C*   IF CHECK RECONCILIATION IS INSTALLED, UPDATE FILE
     C*
     C     QQCHK         IFEQ      'Y'
     C*
     C     PATYPE        IFNE      'A'
     C     CRKLST        CHAIN     PCR01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  XXYCS         CRKY
     C                   UPDATE    PCR01
     C                   END
     C                   END
     C*
     C                   END
     C*
     C                   UPDATE    PPA01
     C     PAKEY         READE     PPA01                                  60
     C                   ENDDO
     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C*
     C*  FIX MULTIPLE CLAIM FORM FILE
     C*
     C     LTKEY         CHAIN     PLT01                              60
     C     *IN60         DOWEQ     '0'
     C                   MOVE      SCYR          LTFYR
     C                   MOVE      SCCASE        LTCASE
     C                   UPDATE    PLT01
     C     LTKEY         READE     PLT01                                  60
     C                   END
     C*
     C*  FIX RECOVERY REQUEST FILE
     C*
     C     RRKEY         CHAIN     PRR01                              60
     C     *IN60         DOWEQ     '0'
     C                   MOVE      SCYR          RRFYR
     C                   MOVE      SCCASE        RRCASE
     C                   UPDATE    PRR01
     C     RRKEY         READE     PRR01                                  60
     C                   END
     C*
     C*  Update WDRPL - View check file
     C*
     C     RPKEY         CHAIN     PRP03                              60
     C     *IN60         DOWEQ     '0'
     C                   Z-ADD     SCYR          RPFYR
     C                   MOVE      SCCASE        RPCASE
     C                   UPDATE    PRP03
     C     RPKEY         READE     PRP03                                  60
     C                   END
     C*
     C*  Update WDACL - Additional address file
     C*
     C     RPKEY         CHAIN     PAC01                              60
     C     *IN60         DOWEQ     '0'
     C                   Z-ADD     SCYR          ACFYR
     C                   MOVE      SCCASE        ACCASE
     C                   UPDATE    PAC01
     C     RPKEY         READE     PAC01                                  60
     C                   END
      *
      /Free
       //  Update SADAP - Diagnosis code Info File

       Setll RPKEY SADAL3;
       Reade RPKEY SADAL3;
       Dow not %eof(SADAL3);
          DAFYR = SCYR;
          DACASE = SCCASE;
          Update PDA01;
          Reade RPKEY SADAL3;
       Enddo;

      /End-Free
     C*  Update WMCRL - Recurring Payable file
     C*
     C     RPKEY         CHAIN     PCR02                              60
     C     *IN60         DOWEQ     '0'
     C                   Z-ADD     SCYR          CRFYR
     C                   MOVE      SCCASE        CRCASE
     C                   UPDATE    PCR02
     C     RPKEY         READE     PCR02                                  60
     C                   END
     C*
     C                   Z-ADD     XXPAY         CTAMT
     C*
     C*  UPDATE DEDUCTIBLE DETAIL
     C*
     C                   EXSR      CHGDD
     C*
     C                   WRITE     PCT01
     C     CUKY          CHAIN     PCU01                              60
     C     *IN60         DOWEQ     *OFF
     C                   Z-ADD     SCYR          CUFYR
     C                   UPDATE    PCU01
     C     CUKY          READE     PCU01                                  60
     C                   ENDDO
     C*
     C*  CALL THE STATE PGM TO INDICATE THAT A FYR CHANGE OCCURRED,
     C*  IF EDI IS INSTALLED.
     C*
     C     QQEDI         IFEQ      'Y'
     C*
     C                   MOVE      XXCO#         QQCO#
     C                   MOVE      XXFND         QQFND
     C                   MOVE      SCYR          QQFYR
     C                   MOVE      SCCASE        QQCASE
     C                   MOVE      'C'           QQACTF
     C                   CALL      'W0070R6'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQACTF
     C                   PARM                    QQMGID
     C                   PARM                    QQRTCD
     C*
     C                   END
     C*
     C*  WRITE FS TRANSFER WORK RECORD AND CALL JOB (F7064C) THAT WILL
     C*  PERFORM CASE TRANSFER FOR FILES IN THE FEE SCHEDULE ENVIRONMENT
     C*
     C                   EXSR      FSCSTR
      /Free
      //Pending state check (2nd call)
       If isModEnabled('IDC') and isNewIdc = *on and
          isIdcState(CTco#:CTXjrst) = *on;

          //Old Policy Number
          Chain (xxCo#:xxFnd:BFyr:BFemp#:BFdiv) PRP04;
          If %found(WMRPL4) and R4cap# <> *blanks;

             //*** Begin Chg *** 10/14/20 **************************************
             //Remove hyphens
             //***strCaP# = R4cap#;
             //***IntPos = %scan('-':strCaP#);
             //***If IntPos > *zero;
             //***   Dou IntPos = *zero;
             //***      strCaP# = %subst(strCaP#:1:IntPos-1) +
             //***                %subst(strCaP#:IntPos+1:25-IntPos);
             //***      IntPos = %scan('-':strCaP#);
             //***   Enddo;

             //***   qqPolId1 = strCaP#;
             //***Endif;
             qqPolId1 = FormatPolId(R4cap#);
             //*** End   Chg *** 10/14/20 **************************************
          Else;
             //*** Begin Chg *** 12/18/20 **************************************
             //qqPolId1 = %Editc(xxFnd:'X') +
             //           %Editc(BFemp#:'X') + %Editc(BFdiv:'X');
             qqPolId1 = %Editc(xxFnd:'X') +
                        %Editc(BFemp#:'X') + %Editc(BFyr:'X');
             //*** End   Chg *** 12/18/20 **************************************
          Endif;

          chain(n) (xxCo#:xxFnd:BFemp#:BFdiv:BFyr) PEL01;
          If %found(WDELL) and ELpfdt > *Zeros;
             qqPolEfdt1 = %Char(%date(ELpfdt:*cymd):*iso0);
          Else;
             qqPolEfdt1 = *Zeros;
          Endif;
          qqClmId1 =  %Editc(BFCase:'X');
          qqAcdt1 = %Char(%date(%Editc(BFacdt:'X'):*usa0):*iso0);

          //New Policy Number
          Chain (xxCo#:xxFnd:SCyr:SCemp#:SCdiv) PRP04;
          If %found(WMRPL4) and R4cap# <> *blanks;

             //*** Begin Chg *** 10/14/20 **************************************
             //Remove hyphens
             //***strCaP# = R4cap#;
             //***IntPos = %scan('-':strCaP#);
             //***If IntPos > *zero;
             //***   Dou IntPos = *zero;
             //***      strCaP# = %subst(strCaP#:1:IntPos-1) +
             //***           %subst(strCaP#:IntPos+1:25-IntPos);
             //*** IntPos = %scan('-':strCaP#);
             //***   Enddo;

             //***   qqPolId2 = strCaP#;
             //***Endif;
             qqPolId2 = FormatPolId(R4cap#);
             //*** End   Chg *** 10/14/20 **************************************
          Else;
             //*** Begin Chg *** 12/18/20 **************************************
             //qqPolId2 = %Editc(xxFnd:'X') +
             //           %Editc(SCemp#:'X') + %Editc(SCdiv:'X');
             qqPolId2 = %Editc(xxFnd:'X') +
                        %Editc(SCemp#:'X') + %Editc(SCyr:'X');
             //*** End   Chg *** 12/18/20 **************************************
          Endif;

          chain(n) (xxCo#:xxFnd:SCemp#:SCdiv:SCyr) PEL01;
          If %found(WDELL) and Elpfdt > *Zeros;
             qqPolEfdt2 = %Char(%date(ELpfdt:*cymd):*iso0);
             qqFyr2 = ELfyr;
          Else;
             qqPolEfdt2 = *Zeros;
             qqFyr2 = *Zeros;
          Endif;
          qqClmId2 =  %Editc(SCcase:'X');
          qqAcdt2 = %Char(%date(CTacdt:*cymd):*iso0);

          //*** Beg   Chg *** 10/14/20 ****************************************
          //***If qqPolEfdt1 > *zeros and qqPolEfdt2 > *Zeros;
          If qqPolId1 <> qqPolID2 or qqPolEfDt1 <> qqPolEfDt2;
          //*** End   Chg *** 10/14/20 ****************************************
             isIdcTrnOk =
             Transfer(qqCo#1:qqCar1:qqPolId1:qqPolEfdt1:qqClmId1:qqAcdt1:
                      qqCo#2:qqCar2:qqPolId2:qqPolEfdt2:qqClmId2:qqAcdt2:
                      qqFyr2);
          Endif;
       Endif;
      /End-Free
     C*
     C*  DISPLAY CASE/FUND YEAR IF CHANGED
     C*
      /Free
       SCHSS# = DisplaySS#(MaskData('SSN':'XXSS#':XXSS#));
      /End-Free
     C*
     C     SCLSN         CAT(P)    ',':0         SCCDNM
     C                   CAT       SCFSN:1       SCCDNM
     C                   CAT       SCMID:1       SCCDNM
     C*
     C                   MOVEL(P)  'SCR03'       Q2RECF
     C                   GOTO      STRT
     C                   ELSE
     C*
     C*  UPDATE DEDUCTIBLE DETAIL
     C*
     C                   EXSR      CHGDD
     C*
     C                   WRITE     PCT01
     C*
     C*  WRITE FS TRANSFER WORK RECORD AND CALL JOB (F7064C) THAT WILL
     C*  PERFORM CASE TRANSFER FOR FILES IN THE FEE SCHEDULE ENVIRONMENT
     C*
     C                   EXSR      FSCSTR
      /Free
      //Pending state check (3rd call)
       If isModEnabled('IDC') and isNewIdc = *on and
          isIdcState(CTco#:CTXjrst) = *on;

          //Old Policy Number
          Chain (xxCo#:xxFnd:BFyr:BFemp#:BFdiv) PRP04;
          If %found(WMRPL4) and R4cap# <> *blanks;

             //*** Begin Chg *** 10/14/20 **************************************
             //Remove hyphens
             //***strCaP# = R4cap#;
             //***IntPos = %scan('-':strCaP#);
             //***If IntPos > *zero;
             //***   Dou IntPos = *zero;
             //***      strCaP# = %subst(strCaP#:1:IntPos-1) +
             //***                %subst(strCaP#:IntPos+1:25-IntPos);
             //***      IntPos = %scan('-':strCaP#);
             //***   Enddo;

             //***   qqPolId1 = strCaP#;
             //***Endif;
             qqPolId1 = FormatPolId(R4cap#);
             //*** End   Chg *** 10/14/20 **************************************
          Else;
             //*** Begin Chg *** 12/18/20 **************************************
             //qqPolId1 = %Editc(xxFnd:'X') +
             //           %Editc(BFemp#:'X') + %Editc(BFdiv:'X');
             qqPolId1 = %Editc(xxFnd:'X') +
                        %Editc(BFemp#:'X') + %Editc(BFyr:'X');
             //*** End   Chg *** 12/18/20 **************************************
          Endif;

          chain(n) (xxCo#:xxFnd:BFemp#:BFdiv:BFyr) PEL01;
          If %found(WDELL) and Elpfdt > *Zeros;
             qqPolEfdt1 = %Char(%date(ELpfdt:*cymd):*iso0);
          Else;
             qqPolEfdt1 = *Zeros;
          Endif;
          qqClmId1 =  %Editc(BFCase:'X');
          qqAcdt1 = %Char(%date(%Editc(BFacdt:'X'):*usa0):*iso0);

          //New Policy Number
          Chain (xxCo#:xxFnd:SCyr:SCemp#:SCdiv) PRP04;
          If %found(WMRPL4) and R4cap# <> *blanks;

             //*** Beg   Chg *** 10/14/20 **************************************
             //Remove hyphens
             //***strCaP# = R4cap#;
             //***IntPos = %scan('-':strCaP#);
             //***If IntPos > *zero;
             //***   Dou IntPos = *zero;
             //***      strCaP# = %subst(strCaP#:1:IntPos-1) +
             //***           %subst(strCaP#:IntPos+1:25-IntPos);
             //*** IntPos = %scan('-':strCaP#);
             //***   Enddo;

             //***   qqPolId2 = strCaP#;
             //***Endif;
             qqPolId2 = FormatPolId(R4cap#);
             //*** End   Chg *** 10/14/20 **************************************
          Else;
             //*** Beg   Chg *** 12/18/20 **************************************
             //qqPolId2 = %Editc(xxFnd:'X') +
             //           %Editc(SCemp#:'X') + %Editc(SCdiv:'X');
             qqPolId2 = %Editc(xxFnd:'X') +
                        %Editc(SCemp#:'X') + %Editc(SCyr:'X');
             //*** End   Chg *** 12/18/20 **************************************
          Endif;

          chain(n) (xxCo#:xxFnd:SCemp#:SCdiv:SCyr) PEL01;
          If %found(WDELL) and Elpfdt > *Zeros;
             qqPolEfdt2 = %Char(%date(ELpfdt:*cymd):*iso0);
             qqFyr2 = ELfyr;
          Else;
             qqPolEfdt2 = *Zeros;
             qqFyr2 = *Zeros;
          Endif;
          qqClmId2 = %Editc(SCcase:'X');
          qqAcdt2 = %Char(%date(CTacdt:*cymd):*iso0);

          //*** Beg   Chg *** 10/14/20 ****************************************
          //***If qqPolEfdt1 > *zeros and qqPolEfdt2 > *Zeros;
          If qqPolId1 <> qqPolID2 or qqPolEfDt1 <> qqPolEfDt2;
          //*** End   Chg *** 10/14/20 ****************************************
             isIdcTrnOk =
             Transfer(qqCo#1:qqCar1:qqPolId1:qqPolEfdt1:qqClmId1:qqAcdt1:
                      qqCo#2:qqCar2:qqPolId2:qqPolEfdt2:qqClmId2:qqAcdt2:
                      qqFyr2);
          Endif;
       Endif;
      /End-Free
     C*
     C     CLEAR         TAG
     C                   Z-ADD     XUFYR         SCYR
     C                   Z-ADD     0             SCCASE
     C                   Z-ADD     0             SCYR2
     C                   Z-ADD     0             SCCAS2
      /Free
       isNewIdc = *Off;
       qqPolId1 = *blanks;
       qqPolEfdt1 = *blanks;
       qqClmId1 = *blanks;
       qqAcdt1 = *blanks;
       qqPolId2 = *blanks;
       qqPolEfdt2 = *blanks;
       qqClmId2 = *blanks;
       qqAcdt2 = *blanks;
      /End-Free
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C* =================================================================
     C* PROCESS SCREEN 3 - DISPLAY CASE NUMBER
     C* =================================================================
     C*
     C     SCR03T        TAG
     C*
     C                   Z-ADD     XUFYR         SCYR
     C                   Z-ADD     0             SCCASE
     C                   Z-ADD     0             SCYR2
     C                   Z-ADD     0             SCCAS2
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C* =================================================================
     C* PROCESS SCREEN 4
     C* =================================================================
     C     SCR04T        TAG
     C*
     C                   GOTO      TERM
     C* =================================================================
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C*
     C*  If WCMC is installed, re-set the library list.
     C*
     C                   MOVE      '1'           *INLR
      *_______________________________________________________________
     C*
     C*===============================================================
     C*  SUBROUTINE TO UPDATE EMP DETAIL CLM TOTALS              *
     C*===============================================================
     C*
     C     CHGEL         BEGSR
     C*
     C                   XFOOT     AR            XXPMT            11 2
     C                   XFOOT     RR            XXRSV            11 2
     C*
     C*  ADD TOTALS TO EMPL BEING TRANSFERRED TO
     C*
     C                   Z-ADD     CTEMP#        XXEMP#
     C                   Z-ADD     CTDIV         XXDIV
     C                   Z-ADD     CTFYR         XXFYR
     C     ELKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      '0'
     C                   ADD       XXPMT         ELTCLP
     C                   ADD       XXRSV         ELOUTS
     C     XXPMT         IFNE      0
     C                   ADD       1             ELT#CL
     C                   END
     C     C1STAT        IFEQ      'C'
     C                   ADD       1             ELTCLC
     C                   END
     C                   ADD       1             ELT#CC
     C                   UPDATE    PEL01
      /Free
       qqPolEfdt2 = %Char(%date(ELpfdt:*cymd):*iso0);
      /End-Free
     C                   END
     C*
     C*  SUB TOTALS FROM EMPL BEING TRANSFERRED FROM
     C*
     C                   Z-ADD     CTXEMP        XXEMP#
     C                   Z-ADD     CTXDIV        XXDIV
     C                   Z-ADD     CTXFYR        XXFYR
     C     ELKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      '0'
     C                   SUB       XXPMT         ELTCLP
     C                   SUB       XXRSV         ELOUTS
     C     XXPMT         IFNE      0
     C                   SUB       1             ELT#CL
     C                   END
     C     C1STAT        IFEQ      'C'
     C                   SUB       1             ELTCLC
     C                   END
     C                   ADD       1             ELT#CC
     C                   UPDATE    PEL01
      /Free
       qqPolEfdt1 = %Char(%date(ELpfdt:*cymd):*iso0);
      /End-Free
     C                   END
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C* FSCSTR SUBROUTINE:
     C*   THIS SUBROUTINE WILL WRITE RECORDS TO THE FILE FXCTP TO BE
     C*   USED BY F7063C TO UPDATE ALL APPROPRIATE FEE SCHEDULE FILES
     C*   WITH THE NEW CLAIM TRANSFER INFORMATION
     C*===============================================================
     C*
     C     FSCSTR        BEGSR
     C*
     C*   CHECK TO SEE IF FSMS IS INSTALLED
     C*
     C***Call FA022R to load the WCMC Claim Transfer workfile (FXCTP)
     C***which is used by F7064C to update the WCMC files.
     C*
     C                   MOVE      XXCO#         ##FCO#            3
     C                   MOVE      XXFND         ##FFND            3
     C     *IN70         IFEQ      '1'
     C                   MOVE      SCYR          ##FFYR            3
     C                   MOVE      SCCASE        ##FCAS            7
     C                   MOVE      BFSS#         ##FSS#
     C                   Z-ADD     BFACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ##FACD            7
     C                   ELSE
     C                   MOVE      BFYR          ##FFYR
     C                   MOVE      BFCASE        ##FCAS
     C                   MOVE      BFSS#         ##FSS#            9
     C                   Z-ADD     BFACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ##FACD
     C                   END
     C*
     C                   MOVE      XXCO#         ##TCO#            3
     C                   MOVE      XXFND         ##TFND            3
     C     *IN70         IFEQ      '1'
     C                   MOVE      SCYR2         ##TFYR
     C                   MOVE      SCCAS2        ##TCAS
     C                   MOVE      CMSS#         ##TSS#
     C                   MOVE      CMACDT        ##TACD            7
     C                   MOVE      SCYR2         Q1FYR
     C                   MOVE      SCCAS2        Q1CASE
     C                   MOVE      CMSS#         Q1SS#
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         Q1ACDT
     C                   ELSE
     C                   MOVE      SCYR          ##TFYR            3
     C                   MOVE      SCCASE        ##TCAS            7
     C                   MOVE      SCSS#         ##TSS#            9
     C                   MOVE      XXACDT        ##TACD
     C                   MOVE      SCYR          Q1FYR
     C                   MOVE      SCCASE        Q1CASE
     C                   MOVE      SCSS#         Q1SS#
     C                   Z-ADD     XXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         Q1ACDT
     C                   END
     C*
     C*
     C*** IF MANAGED CARE IS INSTALLED CALL THE TRANSFER PROCESS
     C*** PROGRAM.
     C*
     C     QQWCMC        IFEQ      'Y'
     C                   MOVE      ##TCO#        Q1CO#
     C                   MOVE      ##TFYR        Q1FYR
     C                   MOVE      ##TCAS        Q1CASE
     C                   MOVE      SCEMP#        Q1EMP#
     C                   MOVE      SCDIV         Q1DIV
     C                   MOVEL(P)  SCLSN         Q1LSN
     C                   MOVEL(P)  SCFSN         Q1FSN
     C                   MOVE      SCMID         Q1MID
     C                   MOVEL(P)  SCADR1        Q1ADR1
     C                   MOVEL(P)  SCADR2        Q1ADR2
     C                   MOVEL(P)  SCCTY         Q1CTY
     C                   MOVEL(P)  SCST          Q1ST
     C                   MOVEL     SCZIP1        Q1ZIP
     C                   MOVE      SCZIP2        Q1ZIP
     C                   MOVE      SCDTOB        Q1DOB
     C                   MOVE      SCSEX         Q1SEX
     C                   MOVE      Q1USER        Q1USER
     C                   MOVE      '07'          Q1CODE           10 0
     C                   MOVEL     REASON        Q1COM1           70
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       #2RTCD            1
     C                   PARM      ##TCO#        #2TCO#            3
     C                   PARM      ##TFND        #2TFND            3
     C                   PARM      *BLANKS       #2SUB#            5
     C                   PARM      *BLANKS       #2FND#            3
     C*
     C                   MOVE      #2SUB#        ##SUB#            5 0
     C                   MOVE      #2FND#        ##FND#            3 0
     C*
     C                   CALL      'F7095R'
     C                   PARM      XYRRN1        Q1RRN             9 0
     C                   PARM                    Q1CO#             3 0
     C                   PARM      ##SUB#        Q1SUB#            5 0
     C                   PARM      ##FND#        Q1FND             3 0
     C                   PARM                    Q1FYR             3 0
     C                   PARM                    Q1CASE            7 0
     C                   PARM                    Q1EMP#            9 0
     C                   PARM                    Q1DIV             5 0
     C                   PARM                    Q1SS#             9
     C                   PARM                    Q1ACDT            8 0
     C                   PARM                    Q1LSN            30
     C                   PARM                    Q1FSN            15
     C                   PARM                    Q1MID             1
     C                   PARM                    Q1ADR1           40
     C                   PARM                    Q1ADR2           40
     C                   PARM                    Q1CTY            25
     C                   PARM                    Q1ST              2
     C                   PARM                    Q1ZIP             9 0
     C                   PARM                    Q1DOB             7 0
     C                   PARM                    Q1SEX             1
     C                   PARM                    Q1USER           10
     C                   PARM                    Q1CODE           10 0
     C                   PARM                    Q1COM1           70
     C                   PARM                    Q1COM2           70
     C                   PARM                    Q1COM3           70
     C                   PARM      ##FLAG        Q1FLAG            1
     C                   PARM      'T'           Q1TD              1
     C                   PARM      XYRRN2        Q1RRN3            9 0
     C                   PARM      'Y'           Q1WCMS            1
     C*
     C                   ENDIF
     C*
     C*
     C*  CALL DARTS CLAIM TRANSFER FACILITY
     C*
     C                   MOVE      ##FCO#        QFCO#
     C                   MOVE      ##FFND        QFFND
     C                   MOVE      ##FFYR        QFFYR
     C                   MOVE      ##FCAS        QFCAS
     C                   MOVE      ##TCO#        QTCO#
     C                   MOVE      ##TFND        QTFND
     C                   MOVE      ##TFYR        QTFYR
     C                   MOVE      ##TCAS        QTCAS
     C                   MOVE      *BLANK        QQRTCD
     C*
     C                   CALL      'SC900C'
     C                   PARM                    QFKEY
     C                   PARM                    QTKEY
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
     C                   PARM                    QQRTCD
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C*  SUBROUTINE TO UPDATE OR DELETE DEDUCTIBLE DETAIL
     C*===============================================================
     C*
     C     CHGDD         BEGSR
     C*
     C*  IF THE EMPLOYER CHANGED, DELETE THE RECORDS AND NOTE THE
     C*  AMOUNT IN THE CHANGE HISTORY "REQUESTED BY" FIELD.
     C*
     C     CTEMP#        IFNE      CTXEMP
     C     CTDIV         ORNE      CTXDIV
     C*
     C     CLTID         IFEQ      '135'
     C     FNPEOF        ANDNE     *BLANKS
     C                   GOTO      TAG1
     C                   ENDIF
     C*
     C                   CLEAR                   XXAMTB
     C                   CLEAR                   REQ
     C                   CLEAR                   AMT
     C*
     C     DDKY          CHAIN     PDD02                              60
     C     *IN60         DOWEQ     *OFF
     C                   ADD       DDAMTB        XXAMTB
     C                   DELETE    PDD02
     C     DDKY          READE     PDD02                                  60
     C                   ENDDO
     C*
     C     XXAMTB        IFNE      *ZERO
     C     XXAMTB        IFGT      *ZERO
     C                   Z-ADD     XXAMTB        XXAMTS
     C                   ELSE
     C                   Z-SUB     XXAMTB        XXAMTS
     C                   ENDIF
     C                   MOVEA     XXAMTC        AMT
     C                   Z-ADD     1             X
     C                   Z-ADD     1             Y
     C                   MOVE      '$'           REQ(X)
     C                   ADD       1             X
     C     XXAMTS        IFGT      9.99
     C     '0'           LOOKUP    AMT(Y)                             60
     C                   ELSE
     C                   Z-ADD     7             Y
     C                   END
     C*
     C     Y             DO        9             Y
     C                   MOVE      AMT(Y)        REQ(X)
     C                   ADD       1             X
     C*
     C     Y             IFEQ      1
     C     Y             OREQ      4
     C                   MOVE      ','           REQ(X)
     C                   ADD       1             X
     C                   ENDIF
     C*
     C     Y             IFEQ      7
     C                   MOVE      '.'           REQ(X)
     C                   ADD       1             X
     C                   ENDIF
     C*
     C     Y             IFEQ      9
     C     XXAMTB        ANDLT     *ZERO
     C                   MOVE      '-'           REQ(X)
     C                   ADD       1             X
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   ADD       1             X
     C                   MOVEA     XXDDCN        REQ(X)
     C                   MOVEA     REQ           CTREQ
     C*
     C*  Call W8024R to reverse all associated deduct entries.
     C*
     C                   EXSR      RVDED
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C*  IF THE CASE OR FYR CHANGED, UPDATE THE DEDUCTIBLE DETAIL
     C*
     C     CTCASE        IFNE      CTXCAS
     C     CTFYR         ORNE      CTXFYR
     C*
     C     TAG1          TAG
     C*
     C     DDKY          CHAIN     PDD02                              60
     C     *IN60         DOWEQ     *OFF
     C                   MOVE      CTCASE        DDCASE
     C                   MOVE      CTFYR         DDFYR
     C                   MOVE      CTEMP#        DDEMP#
     C                   MOVE      CTDIV         DDDIV
     C                   UPDATE    PDD02
     C     DDKY          READE     PDD02                                  60
     C                   ENDDO
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*
       //*** Beg Add *** 10/14/20 ************************************
       //=============================================================
       // Subroutine to determine if IDC Transfer is needed
       //=============================================================
       Begsr IdcQualify;

       // Old Policy Number
       Chain (xxCo#:xxFnd:xxBFyr:xxBFemp#:xxBFdiv) PRP04;
       If %found(WMRPL4) and R4cap# <> *blanks;
          qqPolId1 = FormatPolId(R4cap#);
       Else;
       //*** Beg Chg *** 12/18/20 ************************************
       //   qqPolId1 = %Editc(xxFnd:'X') + %Editc(xxBFemp#:'X')
       //    + %Editc(xxBFdiv:'X');
          qqPolId1 = %Editc(xxFnd:'X') + %Editc(xxBFemp#:'X') +
                     %Editc(xxBFyr:'X');
       //*** End Chg *** 12/18/20 ************************************
       Endif;

       // Old Policy Effective Date
       chain(n) (xxCo#:xxFnd:xxBFemp#:xxBFdiv:xxBFyr) PEL01;
       If %found(WDELL) and Elpfdt > *zero;
          qqPolEfdt1 = %Char(%date(ELpfdt:*cymd):*iso0);
       Else;
          qqPolEfdt1 = *Zeros;
       Endif;

       //New Policy Number
       Chain (xxCo#:xxFnd:xxCTyr:xxCTemp#:xxCTdiv) PRP04;
       If %found(WMRPL4) and R4cap# <> *blanks;
          qqPolId2 = FormatPolId(strCaP#);
       Else;
       //*** Beg Chg *** 12/18/20 ************************************
       //   qqPolId2 = %Editc(xxFnd:'X') +  %Editc(xxCTemp#:'X')
       //    + %Editc(xxCTdiv:'X');
          qqPolId2 = %Editc(xxFnd:'X') +  %Editc(xxCTemp#:'X')
           + %Editc(xxCTyr:'X');
       //*** End Chg *** 12/18/20 ************************************
       Endif;

       // New Policy Effective Date
       chain(n) (xxCo#:xxFnd:xxCTemp#:xxCTdiv:xxCTyr) PEL01;
       If %found(WDELL) and Elpfdt > *Zeros;
          qqPolEfdt2 = %Char(%date(ELpfdt:*cymd):*iso0);
       Else;
          qqPolEfdt2 = *Zeros;
       Endif;

       // Compare old policy to new policy
       If qqPolId1 <> qqPolId2 or qqPolEfDt1 <> qqPolEfDt2;
          isNewIdc = *on;
       Endif;

       Endsr;
       //*** End Add *** 10/14/20 ************************************
     C*===============================================================
     C* MCAPI - DATA INTERFACE TO MANAGED CARE - FMCDP
     C*===============================================================
     C*
     C     MCAPI         BEGSR
     C*
     C*  Only process if WCMS is interfaced with WCMC.
     C*
     C     QQWCMC        IFEQ      'Y'
     C*
     C                   CALL      'FA002R'
     C*
     C                   PARM      *BLANK        QQRTCD            1
     C*
     C                   PARM      CDSS#         QQSS#             9
     C                   PARM                    QQACDT            7
     C*
     C                   PARM                    QQCO#             3
     C                   PARM      *BLANK        QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C*
     C                   PARM      CDLSN         QQLST            30
     C                   PARM      CDFSN         QQNAME           15
     C                   PARM      CDMID         QQMAD1            1
     C                   PARM      CDADR1        QQADR1           40
     C                   PARM      CDADR2        QQADR2           40
     C                   PARM      CDCTY         QQCTY            25
     C                   PARM      CDST          QQST              2
     C                   PARM                    QQZIP             9
     C                   PARM                    QQDTOB            7
     C                   PARM      CDSEX         QQSEX             1
     C                   PARM                    QQAREA            3
     C                   PARM                    QQPHN#            7
     C                   PARM      CDMSTS        QQMSTS            1
     C                   PARM                    QQWARA            3
     C                   PARM                    QQWPH#            7
     C                   PARM      CDCTRY        QQCTRY            3
     C                   PARM      CDADR3        QQADR3           40
     C                   PARM                    QQCARA            3
     C                   PARM                    QQCPHN            7
     C*
     C                   ENDIF
     C*
     C                   ENDSR
      *
     C*===============================================================
     C* SRPRCH - This subroutine changes the claimant's name in
     C*          payment request records for this case.
     C*===============================================================
     C     SRPRCH        BEGSR
     C*
     C     CMKYB         CHAIN(N)  PCM0B                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C     RPKYB         CHAIN     PRP02                              61
     C     *IN61         DOWEQ     *OFF
     C*
     C*    Insure that payment is for the address being changed
     C     CDADR1        IFEQ      RPADR1
     C     CDADR2        ANDEQ     RPADR2
     C     CDCTY         ANDEQ     RPCTY
     C     CDST          ANDEQ     RPST
     C     CDZIP         ANDEQ     RPZIP
     C     CDADR1        OREQ      RPADR1
     C     CDADR2        ANDEQ     RPADR2
     C     CDCTRY        ANDEQ     RPCTRY
     C     CDADR3        ANDEQ     RPADR3
     C*
     C*    Now arrange previous name same as payment record
     C                   MOVE      *BLANKS       XXCNAM           40
     C     CDFSN         CAT       CDMID:1       XXCNAM
     C                   CAT       CDLSN:1       XXCNAM
     C*
     C     RPPNAM        IFEQ      XXCNAM
     C*
     C*    OK - update this payment
     C                   MOVEL     SCADR1        RPADR1
     C                   MOVEL     SCADR2        RPADR2
     C                   MOVEL     SCCTY         RPCTY
     C                   MOVEL     SCST          RPST
     C                   MOVEL     SCZIP1        RPZIP
     C                   MOVE      SCZIP2        RPZIP
     C                   MOVEL(P)  SCCTRY        RPCTRY
     C                   MOVEL(P)  SCADR3        RPADR3
     C*
     C*    Arrange new name for payment record
     C                   MOVE      *BLANKS       RPPNAM
     C     SCFSN         CAT       SCMID:1       RPPNAM
     C                   CAT       SCLSN:1       RPPNAM
     C*
     C                   MOVEL     Q1USER        RPUSER
     C                   MOVEL     Q1PGM         RPPGM
     C                   Z-ADD     XXDT          RPDT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        RPTIME
     C                   UPDATE    PRP02
     C                   ENDIF
     C                   ENDIF
     C*
     C     RPKYB         READE     PRP02                                  61
     C                   ENDDO
     C*
     C     CMKYB         READE(N)  PCM0B                                  99
     C                   ENDDO
     C*
     C     CMKY          CHAIN(N)  PCM01                              40
     C*
     C                   ENDSR
     C*
     C*
     C*===============================================================
     C*  SRCOV - This subroutine determines whether the claim falls
     C*          within an employer policy period.
     C*===============================================================
     C*
     C     SRCOV         BEGSR
     C*
     C*  ACCIDENT DATE MUST BE DURING AN EFFECTIVE PERIOD
     C*
     C     EMEFD1        IFNE      *ZERO
     C     XXACDT        IFGE      EMEFD1
     C*
     C*  VALIDATE ACCIDENT DATE AGAINST EMPLOYER CANCELLATION
     C*
     C     EMCAD1        IFEQ      *ZERO
     C     XXACDT        ORLT      EMCAD1
     C                   GOTO      PASSED
     C                   END
     C                   END
     C                   END
     C*
     C     EMEFD2        IFNE      *ZERO
     C     XXACDT        IFGE      EMEFD2
     C*
     C*  SUBTRACT ONE DAY FROM CANCELLATION DATE TO VALIDATE CORRECTLY
     C*
     C     EMCAD2        IFEQ      *ZERO
     C     XXACDT        ORLT      EMCAD2
     C                   GOTO      PASSED
     C                   END
     C                   END
     C                   END
     C*
     C     EMEFD3        IFNE      *ZERO
     C     XXACDT        IFGE      EMEFD3
     C*
     C*  SUBTRACT ONE DAY FROM CANCELLATION DATE TO VALIDATE CORRECTLY
     C*
     C     EMCAD3        IFEQ      *ZERO
     C     XXACDT        ORLT      EMCAD3
     C                   GOTO      PASSED
     C                   END
     C                   END
     C                   END
     C*
     C     EMEFD4        IFNE      *ZERO
     C     XXACDT        IFGE      EMEFD4
     C*
     C*  SUBTRACT ONE DAY FROM CANCELLATION DATE TO VALIDATE CORRECTLY
     C*
     C     EMCAD4        IFEQ      *ZERO
     C     XXACDT        ORLT      EMCAD4
     C                   GOTO      PASSED
     C                   END
     C                   END
     C                   END
     C*
     C     EMEFD5        IFNE      *ZERO
     C     XXACDT        IFGE      EMEFD5
     C*
     C*  SUBTRACT ONE DAY FROM CANCELLATION DATE TO VALIDATE CORRECTLY
     C*
     C     EMCAD5        IFEQ      *ZERO
     C     XXACDT        ORLT      EMCAD5
     C                   GOTO      PASSED
     C                   END
     C                   END
     C                   END
     C*
     C                   MOVE      '1'           *IN57
     C     PASSED        TAG
     C*
     C*  Verify the claim is within a payroll effective period.
     C*
     C     F1RLXC        IFNE      'Y'
     C     SCCLAS        ORNE      *ZEROS
     C*
     C                   Z-ADD     XXACDT        XXEFDT
     C*
     C     EPKEY         SETGT     PEP01
     C     EPKY2         READPE    PEP01                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     EPSTAT        ANDEQ     'I'
     C     *IN60         OREQ      '0'
     C     EPEFDT        ANDGT     XXEFDT
     C     EPKY2         READPE    PEP01                                  60
     C                   END
      *
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN83
     C                   END
     C*
     C     EPCADT        IFNE      0
     C     XXEFDT        ANDGE     EPCADT
     C                   MOVE      '1'           *IN59
     C                   END
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C*  SROVR - This subroutine determines whether the user can
     C*          override the coverage edits.
     C*===============================================================
     C     SROVR         BEGSR
     C*
     C     CXKY          CHAIN     PCX01                              60
     C     *IN60         IFEQ      *OFF
      *
      *  COMPARE SECURITY LEVEL & TYPE OF THE USER TO CLAIM/DARTS CONTROL
      *
     C                   MOVE      UPLVL         @@ULVL
     C                   MOVE      CXSECC        @@CLVL
      *
     C                   CALL      'SRCCSC'
     C                   PARM      UPTYPE        @@UTYP            1
     C                   PARM                    @@ULVL            3
     C                   PARM      CXOCAT        @@CTYP            1
     C                   PARM                    @@CLVL            3
     C                   PARM      '1'           @@EXEC            1
     C                   PARM                    @@RES             1
     C                   PARM      CXOCA2        @@CTP2            1
     C                   PARM      CXOCA3        @@CTP3            1
     C                   PARM      CXOCA4        @@CTP4            1
     C                   PARM      CXOCA5        @@CTP5            1
     C                   PARM      ' '           @@CTP6            1
     C*
     C     @@RES         IFEQ      '0'
     C                   MOVE      'N'           SCEOVR
      *
     C                   MOVE      *BLANKS       MSGINA
     C                   MOVE      *BLANKS       MSGINB
      *
     C                   SELECT
     C     *IN33         WHENEQ    *ON
     C                   MOVEL     EMG(1)        MSGINA
     C     *IN57         WHENEQ    *ON
     C                   MOVEL     EMG(2)        MSGINA
     C                   MOVEL     EMG(3)        MSGINB
     C     *IN59         WHENEQ    *ON
     C                   MOVEL     EMG(4)        MSGINA
     C                   MOVEL     EMG(5)        MSGINB
     C     *IN83         WHENEQ    *ON
     C                   MOVEL     EMG(6)        MSGINA
     C                   ENDSL
      *
     C                   EXFMT     WDW04
     C*
     C     SCEOVR        IFEQ      'Y'
     C                   SETOFF                                       335759
     C                   SETOFF                                       83
     C                   ELSE
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C     *IN33         IFEQ      *ON
     C     *IN57         OREQ      *ON
     C     *IN59         OREQ      *ON
     C     *IN83         OREQ      *ON
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C* RVDED - This subroutine calls W8024R which determines if the old
     C*   employer was on a deductible plan and, if so, reverses the deduct
     C*   transactions.
     C*===============================================================
     C*
     C     RVDED         BEGSR
     C*
     C                   MOVE      XXCO#         QQCO#
     C                   MOVE      XXFND         QQFND
     C                   MOVE      SCYR          QQFYR
     C                   MOVE      SCCASE        QQCASE
     C                   MOVE      BFEMP#        QQFEMP
     C                   MOVE      BFDIV         QQFDIV
     C                   MOVE      SCEMP#        QQTEMP
     C                   MOVE      SCDIV         QQTDIV
     C                   MOVE      XXAMTB        QQAMTB
     C                   MOVE      CTXFYR        QQFFYR
     C                   CALL      'W8024R'
     C                   PARM      *BLANK        QQCALL            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQFEMP            9
     C                   PARM                    QQFDIV            5
     C                   PARM                    QQTEMP            9
     C                   PARM                    QQTDIV            5
     C                   PARM                    QQAMTB            9
     C                   PARM      *BLANK        QQRTCD            1
     C                   PARM                    QQFFYR            3
     C*
     C                   ENDSR
     C*===============================================================
     C*   AUDIT  WRITE DART IF CLAIMANT INFO CHANGES
     C*===============================================================
     C*
     C     AUDIT         BEGSR
      *
     C                   MOVE      'N'           @@CHG             1
     C                   MOVE      ' '           @@AFL             1
     C     SCFSN         IFNE      SVFSN
     C     SVFSN         ANDNE     *BLANK
     C     SCLSN         ORNE      SVLSN
     C     SVLSN         ANDNE     *BLANK
     C     SCADR1        ORNE      SVADR1
     C     SVADR1        ANDNE     *BLANK
     C     SCADR2        ORNE      SVADR2
     C     SVADR2        ANDNE     *BLANK
     C     SCCTY         ORNE      SVCTY
     C     SVCTY         ANDNE     *BLANK
     C     SCST          ORNE      SVST
     C     SVST          ANDNE     *BLANK
     C     SCZIP1        ORNE      SVZIP1
     C     SVZIP1        ANDNE     *ZERO
     C     SCZIP2        ORNE      SVZIP2
     C     SVZIP2        ANDNE     *ZERO
     C     SCCTRY        ORNE      SVCTRY
     C     SVCTRY        ANDNE     *ZERO
     C     SCADR3        ORNE      SVADR3
     C     SVADR3        ANDNE     *ZERO
     C*
     C                   MOVE      'Y'           @@AFL
     C     SCADR1        IFEQ      SVADR1
     C     SCADR2        ANDEQ     SVADR2
     C     SCCTY         ANDEQ     SVCTY
     C     SCST          ANDEQ     SVST
     C     SCZIP1        ANDEQ     SVZIP1
     C     SCZIP2        ANDEQ     SVZIP2
     C*
     C                   MOVE      *BLANKS       XXCHAD          116
     C                   ELSE
     C                   MOVE      *BLANKS       XXCHAD          116
     C                   MOVE      SVZIP1        XXZIP1            5
     C                   MOVE      SVZIP2        XXZIP2            5
     C                   CAT(P)    SVADR1:1      XXCHAD
     C                   CAT(P)    SVADR2:1      XXCHAD
     C                   CAT(P)    SVCTY:1       XXCHAD
     C                   CAT(P)    SVST:1        XXCHAD
     C                   CAT(P)    XXZIP1:1      XXCHAD
     C                   CAT(P)    XXZIP2:1      XXCHAD
     C*
     C*
     C* If both address and name are changed.
     C     SCFSN         IFNE      SVFSN
     C     SVFSN         ANDNE     *BLANK
     C     SCLSN         ORNE      SVLSN
     C     SVLSN         ANDNE     *BLANK
     C*
     C                   MOVE      'Y'           @@CHG             1
     C                   ENDIF
     C                   ENDIF
     C*
     C                   CALL      'WA074R'
     C                   PARM      CMCO#         ##CO#             3 0
     C                   PARM      CMFND         ##FND             3 0
     C                   PARM      CMFYR         ##FYR             3 0
     C                   PARM      CMCASE        ##CASE            7 0
     C                   PARM      0             ##FEIN            9 0
     C                   PARM      0             ##MOD             3 0
     C                   PARM      CXNMAD        ##RCOD            5
     C                   PARM      *BLANKS       ##RTN             1
     C                   PARM      XXCHAD        ##XHAD          116
     C                   PARM      @@CHG         ##CHG             1
     C                   MOVE      'N'           @@CHG
     C                   ENDIF
     C*
     C                   ENDSR
     C*===============================================================
     C*   DA700R  => WRITE DART REMARKS IF CLAIMANT NAME CHANGES
     C*===============================================================
     C*
     C     DA700R        BEGSR
      *
     C     SCLSN         IFNE      SVLSN
     C     SCLSN         ANDNE     *BLANK
     C     SCFSN         ORNE      SVFSN
     C     SCFSN         ANDNE     *BLANK
     C     SCMID         ORNE      SVMID
     C     SCMID         ANDNE     *BLANK
     C                   MOVE      *BLANKS       QCKEY
     C                   Z-ADD     XXCO1#        QCCO#
     C                   Z-ADD     XXFND1        QCFND
     C                   Z-ADD     XXFYR1        QCFYR
     C                   MOVE      XXCAS1        QCCASE
     C*
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C                   MOVEL(P)  QCKEY         Q#KEY            40
     C                   MOVE      *BLANKS       QCLNEW
     C                   MOVEL     DRT(1)        QCDRT4
     C                   MOVE      *BLANKS       QQFMN
     C                   MOVE      *BLANKS       QQFSN
     C                   MOVE      *BLANKS       QQMID
     C                   MOVE      '1'           QQTERM
     C                   MOVEL     SVFSN         QQFSN
     C                   MOVEL     SVMID         QQMID
     C                   CALL      'SRFRLS'
     C                   PARM                    QQMID
     C                   PARM                    QQFSN
     C                   PARM                    QQTERM
     C                   MOVEL     QQMID         QQFMN
     C                   MOVEL     '1'           QQTERM
     C                   MOVE      *BLANK        QQLSN
     C                   MOVEL     SVLSN         QQLSN
     C                   CALL      'SRNAME'
     C                   PARM                    QQLSN
     C                   PARM                    QQFMN
     C                   PARM                    QQTERM
     C                   MOVEL     QQLSN         QCLNAM
     C                   MOVEL(P)  QCLNEW        Q#COMM          256
     C                   MOVE      *BLANKS       Q#COM2           19
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         Q#CDAT            8
     C                   MOVEL(P)  *BLANKS       Q#DCID           12
     C                   MOVEL(P)  'W0070R'      Q#PNAM           10
     C                   MOVE      *ZEROS        Q#SECL            3
     C                   MOVEL(P)  Q1USER        Q#USER           10
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C                   MOVEL(P)  *BLANKS       Q#FOLD           80
     C                   MOVEL(P)  *BLANKS       Q#DIFL            1
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C                   MOVEL(P)  *BLANKS       Q#FL01            1
     C                   MOVEL(P)  *BLANKS       Q#FL02            1
     C     CXKY          CHAIN     PCX01                              60
     C  N60              MOVEL(P)  CXRCNC        Q#RMCD            5
     C   60              MOVE      *BLANKS       Q#RMCD
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
     C                   END
     C*
     C                   ENDSR
     C*===============================================================
      * **  TABLE ST
      * ALAKAZARCACOCTDEDCFLGAHIIDILINIAKSKYLAMEMDMAMIMNMSWY
      * MOMTNENVNHNJNMNYNCNDOHOKORPARISCSDTNTXUTVTVAWAWVWIPR
**  TABLE DRT
OLD CLAIMANT NAME:
** ERROR MESSAGES
Employer record not found for new policy year.
The Employer Master file does not reflect active
coverage on this date.
Accident date is not within Employer Payroll
effective period for class code entered.
Payroll class is invalid.
