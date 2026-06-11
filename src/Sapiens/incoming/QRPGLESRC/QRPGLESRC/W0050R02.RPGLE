     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H DFTACTGRP(*no) ACTGRP(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0050R02 - EMPLOYER MASTER INFORMATION
     H*****************************************************************
     H*                                                               *
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
     H*                OR ADD RECORDS IN THE EMPLOYER MASTER FILE.    *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0050R - WORK WITH EMPLOYERS                               *
     H*    W0050R17 - TRANSFER/DELETE QUOTE EMPLOYERS                 *
     H*    W0051R19 - EMPLOYER MID TERM ENDORSEMENTS (ADD DIVISION)   *
     H*    W0051R20 - EMPLOYER MID TERM ENDORSEMENTS (DLT DIVISION)   *
     H*                                                               *
     H*  CALLS TO:  FA010R - API to update MC claim master FMCMP      *
     H*             SA001R - API to determine if MC installed         *
     H*                                                               *
     H*  CREATED: 03/03/97  PROGRAMMER: C:33  PROJECT: 60AND          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 11/19/18 C:65  PROJECT:  SY406 - Portal                       *
     H*                Do not open screen file if being called to     *
     H*                submit a quote from portal.                    *
     H*                                                               *
     H* 10/22/18 C:267 Support: 50562                                 *
     H*                Added new workfile WXDA1P/WXDA1L to store      *
     H*                newly added division# for use by STLETTER      *
     H*                to print darts remarks accordingly.            *
     H*                                                               *
     H*  09/27/17 C:33  Support: 49827                                *
     H*                 Remove DEFINE(WEB)                            *
     H*                                                               *
     H*  08/20/13 C:287 Project: CI126                                *
     H*                 Add division to CIEMPCHG program call         *
     H*                                                               *
     H*  04/26/12 C:279 Support: WA099  79773                         *
     H*                 Modified to add an edit to avoid duplicate    *
     H*                 employers.                                   *
     H*                                                               *
     H*  01/20/12 C:189 Support: 78910                                *
     H*                  Populate the EMDBA only for ECM as some other*
     H*                  client is using this old field              *
     H*                                                               *
     H*  12/27/11 C:282 Project: WC2005                               *
     H*                Add API WA085R to change EFT payee name/FEIN   *
     H*                                                               *
     H*  12/26/11 C:255 Support: 78796 WA099                          *
     H*                   In GETDIV subroutine File WMEML is being    *
     H*                   locked. Added code to read WMEML without    *
     H*                   lock.                                       *
     H*                                                               *
     H*  11/18/11 C:191 Support: 78491 WA093                          *
     H*                 - Edit in WC1932 was added at wrong place. Fix*
     H*                   edit so that it doesn't mess up other edits *
     H*                 - # of divisions calculation is wrong too     *
     H*                                                               *
     H*  02/28/11 C:220 Project: WC1932                               *
     H*                 - Added message window WDW02                  *
     H*                 - Update EXCLUDE FROM AUTO CASH DISSOLVE Flag *
     H*                 for all divisions if selected.                *
     H*                                                               *
     H*  09/08/10 C:062 PROJECT: SY190                                *
     H*                 Show flex heading despite client.             *
     H*                                                               *
     H*  05/04/10 C:246 SUPPORT: 72276                                *
     H*                 Fix the Emp# not displaying while adding a    *
     H*                 new dvision on SCR01,SCR02 and SCR03.         *
     H*                                                               *
     H*  03/23/10 C:136 PROJECT: 25WRE PRISS 25                       *
     H*                 FIX REGION # EDIT SO THAT IT INVOKES FOR ADD  *
     H*                 MODE                                          *
     H*                                                               *
     H*  03/12/10 C:246 PROJECT: 133WIM                               *
     H*                Fix PRISS 9. Issue with prompt on Chapter Field*
     H*                                                               *
     H*  02/23/10 C:246 PROJECT: 133WIM                               *
     H*                 CHAPTER fiels required for Quote/Live/Test    *
     H*                 for Client '133'.                             *
     H*                                                               *
     H*  01/04/10 C:190 PROJECT: 25WRE                                *
     H*                 "State Risk ID #" heading on SCR03 will be    *
     H*                 soft-coded to use field WDRRP/RRSEMH.         *
     H*                 If file field WDRRP/RRSEMH is blank then      *
     H*                 display "State Risk ID #" as heading.         *
     H*                                                               *
     H*  10/05/09 C:062 PROJECT: 139WMJ                               *
     H*                 ADD FLEX HEADING PROCESSING.                  *
     H*                                                               *
     H*  08/26/09 C:232 PROJECT: 71WKF                                *
     H*                 AUTO LOCATION CREATION, CHANGE TO ENTITY.     *
     H*                                                               *
     H*  05/21/09 C:220 102WEP/PRISS#4                                *
     H*                 MAKE OPTION F10 ENABLE FOR SCREEN 1           *
     H*
     F*  04/28/09 C:191 PROJECT WC1794                                *
     F*                 CHANGE EDITS ON EMPLOYER MASTER INFORMATION   *
     F*                 TO CHECK THE "STAT RPT/RATING FILE" WDRRP FOR *
     F*                 RISK ID REQUIREMENTS INFORMATION IN           *
     F*                 ADDITION TO THE ADMIN DARTS CONTROL FILE (WMEXP).
     H*                                                               *
     H*  04/21/09 C:189 SUPPORT: 67599                                *
     H*                 DIARY NOT GOING TO CORRECT USER IN ADMIN FILE *
     H*                                                               *
     F*  04/13/09 C:191 PROJECT 114WTU                                *
     F*                 load additional Location name field (ALNAM2)  *
     F*                 additional employer name field (EMNAM2) and   *
     F*                 load (ALFDN2) with (EMFDN2).                  *
     F*                                                               *
     H* 11/05/08 C:190 Project: WC1700                                *
     H*                WC000301A Resolve to Darts                     *
     H*                                                               *
     H* 10/30/08 C:189 Project: 136WDV                                *
     H*                Make 4 digit Ext mandatory                     *
     H*                                                               *
     H* 09/03/08 C:136 SUPPORT: WA132 64919                           *
     H*                CHANGE UPDATE OF LOCATIONS ONLY IF FEIN        *
     H*                OR TYPE OF BUSINESS CHANGES.                   *
     H*                                                               *
     H* 08/21/08  C:102 Project 102WEP
     H*                 BACK TO BASE: BASE OPTION                     *
     H*                  - EMPLR ENTRY: DEFAULT FIELD REP, RGN, CHAPTER
     H*                                                               *
     H* 11/23/07  C:189 Project 132WJP
     H*                Produce an Auto Diary on Employer name change  *
     H*                                                               *
     H* 05/23/08 C:33  Project: 132DKH                                *
     H*                For name and/or FEIN changes report them to    *
     H*                 CiCenter.                                     *
      *
      * 03/28/08 C:47  114MQN
      *                Client 114 only:  added DCRx field to SCR02.
     H*                                                               *
     H* 12/26/07 C:183 PROJECT: 81WZB                                 *
     H*                ADDED CUSTOMER ID FIELD.                       *
     H*                                                               *
     H* 08/26/07 C:136 SUPPORT: WA045 59814                           *
     H*                EXTEND LENGTH OF COMMAND STRING FOR QMCDEXC    *
     H*                TO PREVENT ABEND                               *
     H*                                                               *
     H* 07/06/07 C:189 SUPPORT WA099 - 58994                          *
     H*                ISSUE WITH THE CURSOR MOVEMENT ON COUNTY  &    *
     H*                REGION FIELD ON F10 CORRECTED                  *
     H*                                                               *
     H* 07/02/07 C:189 SUPPORT WA099 - 56371                          *
     H*          C:61  CHANGE DOES NOT APPLY V8R1                     *
     H*                                                               *
     H* 06/29/07 C:61  PROJECT WC1416 PRISS 9                         *
     H*                New "ADD a division" number needs to be        *
     H*                unused by employer in both QUOTE and LIVE.     *
     H*                                                               *
     H* 06/08/07 C:65  PROJECT:  WC1519/SUPPORT                       *
     H*                IF THERE IS NO COUNTY CODE LOADED THEN FLAG    *
     H*                AN ERROR (REINSTATING OLD ERROR CONDITION).    *
     F*                                                               *
     F* 03/20/07 C:185 Project: 71WHU                                 *
     F*                Auto Create Location 1 for Quote Entry         *
     H*                                                               *
     H* 02/06/07 C:33  Seq#: WA045/54881                              *
     H*               -All employer related file deletions should     *
     H*                 happen in W0050R01.                           *
     H*                                                               *
     H* 12/01/06 C:180 PROJECT: 114WMF
     H*                Fix "Copy Employer" option so pgm returns to
     H*                "Work with employers" screen after Copy option
     H*                is taken.
     H*
     H* 11/06/06 C:136 PROJECT: 71WHE (ADDITIONAL CHANGES)            *
     H*               -CLIENT SPECIFIC - ADD 4TH CONTACT. MOVED       *
     H*                INTERNET ADDRESS TO 2ND SCREEN TO MAKE ROOM.   *
     H*                                                               *
     H* 09/20/06 C:136 PROJECT: 71WHE                                 *
     H*                ADD CODE FOR ALTERNATE HEADINGS FOR:           *
     H*               - LOSS CONTROL REP                              *
     H*               - WATCH LIST                                    *
     H*               - VIP MEMBER                                    *
     H*               - SPECIAL ACCEPTANCE                            *
     H*               - ADD GROUP SELF INSURED #                      *
     H*                                                               *
     H* 08/18/06 C:180 PROJECT: 114WMF
     H*                Simplify "Copy employer" option.               *
     H*
     H* 04/25/06 C:65  PROJECT: 132WAY                                *
     H*                ADD NEW FIELDS TO DESIGNATE EMPLOYER NAMES AS  *
     H*                PERSONAL NAMES.  FLAGGED FOR CLIENTS 132 AND   *
     H*                136 AS THIS IS A CA REQUIREMENT.               *
     H*                                                               *
     H* 03/24/06 C:168 PROJECT: 132WET                                *
     H*                Added new field (# of employees per shift)     *
     H*                For earthquake insurance.                      *
     H*                                                               *
     H* 03/06/06 C:168 PROJECT WC1416                                 *
     H*                Added validation, when user adds a new division*
     H*                the effective date could not be less than the  *
     H*                effective date for the state reporting division*
     H*                EMSTRP=Y,                                      *
     H*                                                               *
     H* 02/15/06 C:168 PROJECT 139WCA                                 *
     H*                For client 139, in ADD mode when returning,    *
     H*                do not defualt field SCLSID to '1'             *
     H*                                                               *
     H* 01/05/06 C:132 SUPPORT WA139 - 50698                          *
     H*                If WMFNP/FNPEOF = "M", then override the edit  *
     H*                Restricting the FEIN to one EMPLOYER.          *
     H*                                                               *
     H* 11/29/05 C:53  SUPPORT # INTERNAL                             *
     H*                Only lock record prior to UPDATE.              *
     H*                                                               *
     H* 08/22/05 C:61  SUPPORT # 48669                                *
     H*                Only lock record prior to UPDATE.              *
     H*                                                               *
     H* 08/02/05 C:53  SUPPORT # 48340                                *
     H*                Force F3 in w/w when PEO = S and Emp 1 div 0   *
     H*                has just been added.                           *
     H*                                                               *
     H* 07/07/05 C: 58 PROJECT: WC1205
     H*              - Remove Activity Status code/Description.
     H*                                                               *
     H* 06/07/05 C:132 PROJECT: WA045 - 47508                         *
     H*              - Correct Activity Status Description in MVDBSC. *
     H*                                                               *
     H* 06/02/05 C:132 PROJECT: WA071 - 47296                         *
     H*              - Correct edits on County Code - If remove the   *
     H*                County Code on screen 2, go back and get the   *
     H*                correct County Code, if not in file leave blnk *
     H*                                                               *
     H* 04/18/05 C:65  PROJECT WC1279                                 *
     H*                Add PEO processing                             *
     H*                                                               *
     H* 04/15/05 C:132 PROJECT 135WBZ                                 *
     H*              - Add PEO Link Code WMEMP/EMLINK on Screen 3     *
     H*                There are no edits on this field.              *
     H*                                                               *
     H* 04/01/05 C:132 PROJECT WA099  46328                           *
     H*              - Correct edits on NCCI Risk ID # on Screen 3    *
     H*                Changes to W0050F02 only, no Code changes.     *
     H*                                                               *
     H* 03/31/05 C:53  PROJECT: WC1287/135WCB                         *
     H*                Added edit for NAICS code requirements         *
     H*                                                               *
     H* 03/16/05 C:132 PROJECT WC1118                                 *
     H*              - ADD new edits on County Code - The Zip Code    *
     H*                if resides in County Code file must be active  *
     H*              - Will default in the County Code field if valid *
     H*                                                               *
     H* 12/02/04 C:61  Project: WC577                                 *
     H*                Add pgm name and screen to error window text.  *
     H*                                                               *
     H* 10/07/04 C:132 Project: 132WBE   ( ALSO IS WC1262 )           *
     H*                Add Edits for State Risk ID# and NCCI ID#  if  *
     H*                new flags in WMEXP are set to "Y"              *
     H*                                                               *
     H* 09/22/04 C:33  Project: WC1202                                *
     H*                Load SCEDN with correct name.                  *
     H*                                                               *
     H* 08/27/04 C:132 PROJECT WC1118                                 *
     H*                Auto Assign County Code in Change Mode if      *
     H*                County Code in Employer is Blank.              *
     H*                                                               *
     H* 05/14/04 C:65  PROJECT WC1202                                 *
     H*                ADD PROCESSING TO EXPAND EMPLOYER NAME FIELDS  *
     H*                TO TWO 50 CHARACTER FIELDS EACH.               *
     H*                                                               *
     H* 03/25/04 C:61  PROJECT 114WHL                                 *
     H*                Create "New Business Tracking" record.         *
     H*                                                               *
     H* 01/27/04 C:132 SUPPORT WA099  39682                           *
     H*                If no Group number passed into program on an   *
     H*                ADD mode, return to W0050R after msg displayed *
     H*                                                               *
     H* 09/30/03 C:61  SUPPORT WA129 39275 WC1119                     *
     H*                Correct UE# edit.                              *
     H*                                                               *
     H* 04/29/03 C:61  SUPPORT WA126 36982                            *
     H*                Correct processing of NAIC code / Description. *
     H*                                                               *
     H* 04/18/03 C:136 PROJECT: WC1111                                *
     H*                Make edit on FEIN an informational edit.       *
     H*                Do not edit in change mode unless FEIN has     *
     H*                been changed.                                  *
     H*                Allow employer divisions to have same FEIN     *
     H*                without invoking edit message.                 *
     H*                                                               *
     H* 03/31/03 C:142 PROJECT WC1111                                 *
     H*                Do not allow duplicate employers in either     *
     H*                live or quote environments.                    *
     H*                                                               *
     H* 03/04/03 C:132 PROJECT WC1119                                 *
     H*                In Add or Change Mode, Check for the state     *
     H*                unemployment # flag in WDRPP/RRURFL to see if  *
     H*                state uemployment # is required. If so, issue  *
     H*                error message if missing.                      *
     H*                                                               *
     H* 02/28/03 C:132 PROJECT WC1118                                 *
     H*                In Add Mode, check ZIPCODE BY COUNTY file for  *
     H*                valid county code, and load into SCCNTY on the *
     H*                second screen. THIS IS A NEW FILE - WMCZP/L    *
     H*                                                               *
     H* 07/18/02 C:61  SUPPORT WA109 33442                            *
     H*                Delete DARTS remarks when deleting employer.   *
     H*                                                               *
     H* 06/14/02 C:61  SUPPORT WA099 32903                            *
     H*                Correct "Watch List" edit.                     *
     H*                                                               *
     H* 04/29/02 C:136 SUPPORT: WA065/32173                           *
     H*                Ensure that correct office # is displayed when *
     H*                user changes fund with F-13 in Add Mode.       *
     H*                                                               *
     H* 04/02/02 C:90  PROJECT: WA091/31889  WC1070                   *
     H*                In ADD mode, call Employer Contacts before     *
     H*                calling Owner/Officers.                        *
     H*                                                               *
     H* 03/04/02 C:90  PROJECT: CM027/31461                           *
     H*                Skip Check Security Level Subroutine if called *
     H*                from Midterm Endorsement process.              *
     H*                                                               *
     H* 01/31/02 C:61  PROJECT WC1070                                 *
     H*                Get contact names and phone from               *
     H*                Employer Contact File WMECP.                   *
     H*                                                               *
     H* 09/05/01 C:90  SUPPORT: WA112/#27178                          *
     H*                When the pgm is called from the midterm end.   *
     H*                "ADD OF DIVISION" process, do not lock the     *
     H*                record in WMEMP.                               *
     H*                                                               *
     H* 06/29/01 C:61  SUPPORT WA081 28039                            *
     H*                Chg SCR03/SCFEIN to display leading zeros.     *
     H*                                                               *
     H* 06/08/01 C:61  PROJECT WC1034                                 *
     H*                Daily Batch Lockout in ADD mode.               *
     H*                                                               *
     H* 05/02/01 C:09  SUPPORT: WA112/27351                           *
     H*                On midterm "ADD OF DIVISION", the endorsement  *
     H*                was not being submitted.  This was due to the  *
     H*                fact that in subroutine PGMSTR (SCFND/SCEMP#)  *
     H*                did not have values therefore the routine to   *
     H*                load the state rptg div (SVDIV) did not work.  *
     H*                Cannot move this code since that was causing   *
     H*                other issues (see Seq # 27174).  Instead, I    *
     H*                will simply access main employer record info   *
     H*                separately (twice) in this subroutine.         *
     H*                                                               *
     H* 08/18/00 C:09  SUPPORT: WA096/23928                           *
     H*                On midterm "ADD OF DIVISION", the policy eff   *
     H*                date was not being loaded (was equal to        *
     H*                zero) WDELP won't exist at this point on a     *
     H*                new div that is added, should use st rpt div   *
     H*                Also, we need to load the new division #       *
     H*                into ENDIV in order to be sure that W7418R     *
     H*                WCPOLS Electronic Rptg assigns a new entity    *
     H*                code                                           *
     H*                                                               *
     H* 08/15/00 C:61  SUPPORT 23970                                  *
     H*              - Disallow F5=Refresh in ADD mode.               *
     H*              - Use display file to condition appearance of    *
     H*                Function keys for SCR02.                       *
     H*                                                               *
     H* 04/27/00  C:53   SUPPORT: WC065/19768                         *
     H*                  SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
     H*                                                               *
     H* 04/14/00  C:35   SUPPORT: WA071 #22564                        *
     H*                  INCORRECT INDICATOR WAS BEING USED FOR ERROR *
     H*                  ON CHAPTER.                                  *
     H*                                                               *
     H* 04/12/00  C:53   PROJECT: MC189                               *
     H*                  CALL FA029R TO DELETE SECONDARY FILES        *
     H*                                                               *
     H* 03/10/00  C:118  SUPPORT: WA048/21972                         *
     H*                  UPDATE ALL LOCATIONS WHEN EMPLOYER TYPE OF   *
     H*                  BUSINESS CHGS.                               *
     H*                                                               *
     H* 03/07/00  C:09   PROJECT: CM027 (WA095/SEQ# 21849)            *
     H*                  GROUP CARRIER NUMBER WAS NOT BEING LOADED/   *
     H*                  PRINTED ON ENDORSEMENT/REPORTED TO NCCI.     *
     H*                                                               *
     H* 02/09/00  C:33   PROJECT: WC919                               *
     H*                  DEFAULT REGION, IN ADD MODE, FROM ADMIN/DARTS*
     H*                                                               *
     H* 01/25/00  C:35   PROJECT: SUPPORT WA999                       *
     H*                  INCORRECT MODE WAS BEING PASSED TO W0168R.   *
     H*                                                               *
     H* 01/04/00  C:33  SEQ#: WA093/21223                             *
     H*           C:118 DO NOT EDIT FOR ANOTHER USER UPDATE IN ADD MO *
     H*                                                               *
     H* 12/22/99  C:35  PROJECT: SUPPORT WA101 #21097                 *
     H*           C:33  ADD QUOTE PARM TO CALL TO DELETEION PROGRAM.  *
     H*                                                               *
     H* 09/22/99 C:33  SEQ#: WA096/19720                              *
     H*                UPDATE LOCATIONS WITH CURRENT NAME, FED NAME & *
     H*                FED ID #.                                      *
     H*                                                               *
     H* 09/20/99 C:61  PROJECT WC946                                  *
     H*                Add 'State unemployment number' SCR03.         *
     H*                                                               *
     H* 09/16/99 C:65  PROJECT: WC851                                 *
     H*                REMOVE THE SHORT RATE/PRORATE FLAG FROM SCR02  *
     H*                                                               *
     H* 09/07/99 C:118 PROJECT: WC934                                 *
     H*                ADD ENTRY FOR 'NAIC CODE' IN DISPLAY FILE ON   *
     H*                SCR03 AFTER 'EXPERIENCE RATING'. ADD ERROR     *
     H*                CHECK, VALIDATION AND F10 SEARCH ON NAIC CODE. *
     H*                                                               *
     H* 08/20/99 C:33  SEQ#: WA071/19351                              *
     H*                USE THE SEC LVL IN THE ADMIN/DARTS FILE FOR    *
     H*                CHECK THE OVERRIDE CX DATE < ACCIDENT DATE     *
     H*                                                               *
     H* 08/11/99 C:33  PROJECT: WC897                                 *
     H*                ADD SECURITY ON AN ADD                         *
     H*                                                               *
     H* 08/05/99 C:33  SEQ#: WA033/19101                              *
     H*                RELEASE EMP DETAIL RECORDS AFTER CHAIN         *
     H*                                                               *
     H* 08/04/99 C:33  PROJECT: 33WEE  WC033/19158                    *
     H*                RELEASE GROUP MASTER RECORDS AFTER CHAIN       *
     H*                                                               *
     H* 07/23/99  C:33   SEQ#: WA048/18972                            *
     H*                  UPDATE THE MAINTENANCE FIELDS FOR GRP MSTR.  *
     H*                                                               *
     H*  07/19/99 C:33  PROJECT: WC899                                *
     H*                 IF THE CHAPTER CHANGES, UPDATE RECORDS IN     *
     H*                 WDABP.                                        *
     H*                                                               *
     H* 06/23/99 C:113 PROJECT: WC895                                 *
     H*                Added copy option to Quote environment.        *
     H*                                                               *
     H*  06/22/99 C:61 PROJECT 48WQJ                                  *
     H*                Add previous fund number to header info.       *
     H*                                                               *
     H* 06/07/99 C:113 PROJECT: SUPPORT #18338                        *
     H*                Remove references to EMDBA.  This field is no  *
     H*                longer being used in V3.                       *
     H*                                                               *
     H* 06/04/99  C:33   SEQ#: WA093/18332                            *
     H*           C:118  TURN OF *IN93 IN DDS FOR SCR01 - SCR03, SO   *
     H*                  THAT THE MESSAGE WILL NOT DISPLAY INCORRECTLY*
     H*                                                               *
     H* 03/23/99  C:33   SEQ#: WA063/17247                            *
     H*                  DO NOT EDIT FIELDS NOT ON SCREEN.            *
     H*                                                               *
     H* 11/23/98  C:35   PROJECT: SUPPORT WA090 15125                 *
     H*                  INITIALIZE POLICY EFFECTIVE DATE IN THE      *
     H*                  ENDORSEMENT RECORDS FROM THE APPROPRIATE FYR *
     H*                  (BASED ON ENDORSEMENT EFFECTIVE DATE) IN THE *
     H*                  EMPLOYER DETAIL FILE.                        *
     H*                                                               *
     H* 10/14/98  C:35   PROJECT: SUPPORT WA091 13352                 *
     H*                  INITIALIZE CARRIER NUMBER IN THE ENDORSEMENT *
     H*                  RECORDS FROM THE VALUE IN FUND AUXILIARY.    *
     H*                                                               *
     H* 09/25/98 C:33 SEQ#: WA091/13385                               *
     H*               DO NOT WRITE TO DIARY TRANSFER FILE IN QUOTE ENV*
     H*                                                               *
     H* 09/18/98 C:61 Project: 48WLC                                  *
     H*               Change KLIST used for Employer Cross Reference  *
     H*               (WZEXP).                                        *
     H*                                                               *
     H* 09/03/98 C:90 Project: WC577                                  *
     H*               EMPLOYEE LEASING TO DEFAULT TO '1' IN ADD MODE. *
     H*                                                               *
     H*  08/28/98 C:09 SUPPORT  WA095  SEQ# 13446                     *
     H*                MIDTERM ENDORSEMENT NOT WRITING RECORD FOR     *
     H*                "ADD OF DIVISION".                             *
     H*                                                               *
     H* 08/10/98 C:61 Project: 48WLC                                  *
     H*               Display old Emp#/Div from Employer Cross        *
     H*               Reference (WZEXP).                              *
     H*                                                               *
     H*  08/07/98 C:78 SEQ#: DR095/12876                              *
     H*                ADD PARAMETER ##QTE WHEN CALLING W1057R FOR    *
     H*                QUOTE MODE. REMOVE F9 FROM SCR02.              *
     H*                                                               *
     H* 07/16/98  C:65  PROJECT: SUPPORT                              *
     H*                 ONLY DEFAULT OFFICE NUMBER IN ADD MODE.       *
     H*                                                               *
     H* 07/15/98  C:35  PROJECT: SUPPORT WA033 12672                  *
     H*                 IF EMPLOYER NAME IS CHANGED, CHANGE ALL       *
     H*                 LOCATION RECORDS TO MATCH.  ALSO, ONLY        *
     H*                 PROCESS QUOTE NAME CHANGE CODE IF EMPLOYER    *
     H*                 NAME OR CHAPTER HAS CHANGED.                  *
     H*                                                               *
     H* 06/26/98  C:33  PROJECT: WC849                                *
     H*                 DO NOT REQUIRE FEIN OR SIC CODE IN QUOTE.     *
     H*                                                               *
     H* 06/23/98  C:57  PROJECT: SUPPORT                              *
     H*                 FIX INCORRECT LOAD OF PHYSICAL STATE DURING   *
     H*                 ENDORSEMENT PROCESSING.                       *
     H*                                                               *
     H* 06/15/98  C:35  PROJECT: SUPPORT                              *
     H*                 NCCI RISK ID IS REPORTED TO NCCI AS A 9-DIGIT *
     H*                 NUMERIC FIELD.  THEREFORE, THE SCREEN FIELD   *
     H*                 HAS BEEN CHANGED TO 9 NUMERIC.  SINCE THE     *
     H*                 FIELD WAS PREVIOUSLY 15 ALPHA (WHICH IT IS IN *
     H*                 THE FILE), NEED TO VERIFY THAT DATA IS VALID  *
     H*                 NUMERIC DATA BEFORE MOVING TO SCREEN FIELD.   *
     H*                 IF NOT VALID, MOVE 0 TO SCREEN FIELD.  LEFT-  *
     H*                 JUSTIFY VALUE IN FILE.                        *
     H*                                                               *
     H*  04/27/98 C:53 PROJECT: MC145                                 *
     H*                USE EMFDNM FOR DBA NAME IN API PARM            *
     H*                                                               *
     H*  03/23/98 C:85 PROJECT: WC773                                 *
     H*                DETERMINE IF CLIENT IS ADMIN-ONLY (IND. 94),   *
     H*                OR CLAIMS-ONLY (IND. 95).                      *
     H*                                                               *
     H* 03/05/98  C:35  PROJECT: SUPPORT                              *
     H*                 CHANGE CODE SO THAT THE CANCELLATION DATE     *
     H*                 DISPLAYED ON THE SCREEN IS THE CX DATE        *
     H*                 ASSOCIATED WITH THE MOST RECENT EFFECTIVE     *
     H*                 DATE.  ALSO, THIS FIELD IS NOW BEING CLEARED  *
     H*                 SO THAT IT WON'T CARRY DATA FROM A PREVIOUSLY *
     H*                 DISPLAYED EMPLOYER IF THE CURRENT EMPLOYER    *
     H*                 IS NOT CANCELLED.  ALSO, CONTROL DISPLAY OF   *
     H*                 THIS FIELD WITH AN INDICATOR SO THAT IF THE   *
     H*                 EMPLOYER IS NOT CANCELLED, THE FIELD AND      *
     H*                 HEADING WILL NOT SHOW ON THE SCREEN.          *
     H*                                                               *
     H*  02/12/98  C:33  PROJECT: WC577                               *
     H*                  DELETE MORE EMPLOYER RELATED FILES.          *
     H*                                                               *
     H*  02/03/98  C:57  PROJECT: CM027                               *
     H*                 -SETUP FOR MID TERM ENDORSEMENT TRACKING IF   *
     H*                  CALLED FROM THAT PROCESS.                    *
     H*                                                               *
     H*  01/21/98  C:33  PROJECT: WC577                               *
     H*                  Do not default Federal Name to Name if not   *
     H*                  entered.                                     *
     H*                                                               *
     H*  12/09/97  C:80  PROJECT: WC778                               *
     H*                  Require entry of Federal tax id if EXFIFG    *
     H*                  was entered as 'Y' in admin/darts.           *
     H*                                                               *
     H*  11/07/97  C:33  PROJECT: WC577                               *
     H*                 -If Name or Chapter change update the Quote   *
     H*                  Status record                                *
     H*                 -Hard delete the Quote Status record          *
     H*                 -Use generic table file for SIC code          *
     H*                                                               *
     H*  11/04/97  C:33  PROJECT: WC577                               *
     H*                  Do not call Managed Care API in quote env    *
     H*                                                               *
     H*  10/31/97 C:79 PROJECT WC707                                  *
     H*                ADD CODE TO WRITE DIARY TRANSFER RECORDS TO    *
     H*                WADTP.                                         *
     H*                                                               *
     H*  10/30/97  C:33  PROJECT: WC577                               *
     H*                  Add call to W0050R17 (Transfer/Delete quoted *
     H*                  employers) to delete the quote transfer      *
     H*                  records when quoted employer is deleted      *
     H*                                                               *
     H*  10/29/97  C:33  PROJECT: WC577                               *
     H*                  Recompiled due to fields being added to WMEPP*
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H*  40 - ADD MODE                                                *
     H* N40 - UPDATE MODE                                             *
     H*  41 - ADD DIVISION                                            *
     H*  42 - Last Invoice Date Exists                                *
     H*  43 - Initial Deposit Exists                                  *
     H*  46 - SIC CODE REQUIRED                                       *
     H*  62 - QUOTE MODE                                              *
     H*  63 - Display old Emp#/Div in header info                     *
     H*  65 - SECURITY LEVEL VALID                                    *
     H* N65 - SECURITY LEVEL INVALID                                  *
     H*  66 - COMMERCIAL APPLICATION INSTALLED (WCFL07=Y)             *
     H*  90 - GENERAL ERROR INDICATOR                                 *
     H*  91 - GENERAL POSITION CURSOR                                 *
     H*  92 - GENERAL F KEY PRESSED                                   *
     H*                                                               *
     H*  SCR01:                                                       *
     H*  ERROR:                                                       *
     H*  20 - (WCR0503) DBA must be entered                           *
     H*  21 - (WCR0503) Mailing City must be entered                  *
     H*  22 - (WCR0504) Mailing State not valid                       *
     H*  23 - (WCR0503) Mailing Zip 1 cannot be zero                  *
     H*  24 - (WCR0503) Physical City cannot be blank                 *
     H*  25 - (WCR0504) Physical State invalid                        *
     H*  26 - (WCR0503) Physical Zip 1 cannot be zero                 *
     H*  29 - (WCR0524) Mailing/Physical Flag not 'M' or 'P'          *
     H*  30 - (WCR0525) Mailing Address must be entered if flag is 'M'*
     H*  31 - (WCR0525) Physical Address must be entered if flag is P *
     H*  POSITION CURSOR:                                             *
     H*                                                               *
     H*  SCR02:                                                       *
     H*  21 - Effective Date 1                                        *
     H*  22 - Effective Date 2                                        *
     H*  23 - Effective Date 3                                        *
     H*  24 - Effective Date 4                                        *
     H*  25 - Effective Date 5                                        *
     H*  26 - Cancel Date 1                                           *
     H*  27 - Cancel Date 2                                           *
     H*  28 - Cancel Date 3                                           *
     H*  29 - Cancel Date 4                                           *
     H*  30 - Cancel Date 5                                           *
     H*  70 - Policy Services Staff Code                              *
     H*  71 - Underwriter Staff Code                                  *
     H*  72 - Premium Account Staff Code                              *
     H*  73 - Audit Reviewer Staff Code                               *
     H*  74 - Audit Manager Staff Code                                *
     H*  75 - Loss Control Rep Staff Code                             *
     H*  76 - Activity Status                                         *
     H*  77 - Watch List                                              *
     H*  78 - County                                                  *
     H*  79 - VIP Member                                              *
     H*  80 - Region                                                  *
     H*  81 - Special Acceptance                                      *
     H*  82 - Chapter                                                 *
     H*  83 - Claim Bank Code                                         *
     H*  ERROR:                                                       *
     H*  20 - (WCR0609) Initial effective date > policy from date     *
     H*  31 - (WCR0500) Date Invalid                                  *
     H*  32 - (WCR0526) Cancel Date < Effective date                  *
     H*  33 - (WCR1061) Cancel Date < Accident date                   *
     H*  34 - (WCR0510) Audit Cancellation not 'S' or 'P'             *
     H*  35 - (WCR0513) Invalid Table (ASTS/CNTY/RGN#/CHAP/BKCD)      *
     H*  36 - (WCR0501) Not 'Y' or 'N' (WALS/VIP/SPAC)                *
     H*  37 - (WCR0505) Staff Codes not found                         *
     H*  38 - (WCR0690) Inactive Table (CNTY/RGN#)                    *
     H*  POSITION CURSOR:                                             *
     H*  39                                                           *
     H*                                                               *
     H*  SCR03:                                                       *
     H*  ERROR:                                                       *
     H*  20 - (WCR0501) State Reporting Flag not 'Y' or 'N'           *
     H*  21 - (WCR0513) Invalid Table (Type of Business)              *
     H*  22 - (WCR0513) Invalid Table (Employee Leasing Id)           *
     H*  23 - (WCR0513) Invalid Table (SIC Code)                      *
     H*  24 - (WCR0513) Invalid Table (TPA Office #)                  *
     H*  27 - (WCR0513) Invalid Table (Experience Rating)             *
     H*  80 - (WCR1277) State Reporting flag needs to be 'Y'          *
     H*  81 - (WCR1278) State Reporting flag needs to be 'N'          *
     H*  82 - (WCR1279) No State Reporting Division identified        *
     H*  84 - (WCR0513) Invalid Table (NAIC Codes)
     H*  POSITION CURSOR:                                             *
     H*  31 - Type of Business (SCTPOB)                               *
     H*  32 - Employee Leasing Id (SCLSID)                            *
     H*  33 - SIC Code (SCSIC)                                        *
     H*  34 - Nature of Business (SCNAOB)                             *
     H*  35 - TPA Office # (SCOFF#)                                   *
     H*  36 - Comments 1 (SCCOM1)                                     *
     H*  37 - Experience Rating (SCEXRT)                              *
     H*  48 - NAIC Code (SCNAIC)
     H*                                                               *
     H*  WDWEMP#M:                                                    *
     H*  ERROR:                                                       *
     H*  20 - (WCR0502) Employer# must be > zero                      *
     H*  21 - (WCR0502) Division must be > zero                       *
     H*  22 - (WC30005) Employer# previously used                     *
     H*  POSITION CURSOR:                                             *
     H*****************************************************************
      *
     FW0050F02  CF   E             WORKSTN
     F*** begin add ***  11/19/18  **************************************
     F                                     USROPN
     F*** end add *****  11/19/18  **************************************
      *IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      *ENDIF
     F                                     INFDS(FIDS01)
     FWMEMP     UF A E             DISK
     F                                     RENAME(PEM01:DBRCDF)
     F                                     INFDS(DBFDS)
      *
     F****  BEGIN CHG **** 02/28/11 *********************************
     F****WMEML   IF  E           K        DISK
     FWMEML     UF   E           K DISK
     F****  END   CHG **** 02/28/11 *********************************
      *
     F***  BEGIN ADD  ***  03/31/03  *********************************
     FWMEML12   IF   E           K DISK
     F                                     RENAME(PEM01:PEM12)
     F***  END   ADD  ***  03/31/03  *********************************
      ***** BEG ADD ******* 06/29/07 **********************************
      *
      * Other environment (LIVE or QUOTE): Employer Master
     FWMEMLQ    IF   E           K DISK    USROPN
     F                                     RENAME(PEM01:PEMOTH)
      ***** END ADD ******* 06/29/07 **********************************
     F***  Begin Chg  ***  02/06/07  **************************************
     F**********WDELL   UF  E           K        DISK
     FWDELL     IF   E           K DISK
     F***  End   Chg  ***  02/06/07  **************************************
      ***** BEG ADD ******* 08/10/98 **********************************
      *  Employer Cross Reference
     FWZEXL     IF   E           K DISK
     F                                     RENAME(PEX01:PEX02)
      ***** END ADD ******* 08/10/98 **********************************
      *
     FWMCMLC    IF   E           K DISK
      *
     FWMFNL     UF   E           K DISK
     F                                     INFDS(DBFDS1)
      *
     FWMUPL     IF   E           K DISK
     FWMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
      *
     FSMUPP     IF   E             DISK
     F                                     RENAME(PUP01:PUP03)
      *
     FWMREL     IF   E           K DISK
      *
     FWMREP     IF   E             DISK
     F                                     RENAME(PRE01:PRE02)
      *
     FWMCPL     IF   E           K DISK
     F***  BEGIN DEL  ***  11/07/97  *********************************
     F**********WTSCL   IF  E           K        DISK
     F***  END   DEL  ***  11/07/97  *********************************
      *
     F***  BEGIN ADD  ***  07/19/99  *********************************
     FWDABL2    UF   E           K DISK
     F***  END   ADD  ***  07/19/99  *********************************
     F****** BEG DEL ***** 07/07/05 **************************************
     F*****WTAVL***IF**E*****      K        DISK
     F*****WTAVP***IF**E*****               DISK
     F************PAV01******                       KRENAMEPAV02
     F****** END DEL ***** 07/07/05 **************************************
      *
     FWMOFL     IF   E           K DISK
     FWAOFL     UF A E           K DISK
     F                                     RENAME(POF01:PAF01)
      *
     FWMEXL     IF   E           K DISK
      *
     F***  Begin Del  ***  02/06/07  **************************************
     F**********WMEPL   UF  E           K        DISK
      **********
     F**********WMEAL   UF  E           K        DISK
      **********
     F**********WMAHL   UF  E           K        DISK
     F***  End   Del  ***  02/06/07  **************************************
      *_____________________________________________________________________
     F***  BEGIN CHG  ***  11/07/97  *********************************
     F*********WAQTL7  UF  E           K        DISK
      *
     FWAQTL7    IF   E           K DISK
     F                                     INFDS(DBFDSQ)
     FWAQTP     UF   E             DISK
     F                                     RENAME(PQT01:PQT02)
     F***  END   CHG  ***  11/07/97  *********************************
      ***** BEG ADD ******* 03/25/04 **********************************
      *
      *  New Business Tracking
     FWMNBL     UF A E           K DISK
      ***** END ADD ******* 03/25/04 **********************************
      *
      ***** BEG DEL ******* 07/18/02 **********************************
     F******SDCMLA  UF  E           K        DISK
     F******            PCM01                             KRENAMEPCM03
      ***** END DEL ******* 07/18/02 **********************************
      ***** BEG ADD ******* 07/18/02 **********************************
     F***  Begin Del  ***  02/06/07  **************************************
     F**********SDCMLB  UF  E           K        DISK
     F**********  PCM01                             KRENAMEPCM03
     F***  End   Del  ***  02/06/07  **************************************
      ***** END ADD ******* 07/18/02 **********************************
     F** BEG ADD *** 10/31/97 *****************************************
      *
      * DIARY TRANSFER FILE (CO#/FND/EMP#/DIV/CTGY)
     FWADTL1    O  A E           K DISK
     F** END ADD *** 10/31/97 *****************************************
      *
      ***** BEG DEL ******* 07/18/02 **********************************
     F******SMARL   UF  E           K        DISK
     F******            PAR01                             KRENAMEPAR02
      ***** END DEL ******* 07/18/02 **********************************
     F***  Begin Del  ***  02/06/07  **************************************
      ******BEG*ADD ******* 07/18/02 **********************************
     F**********SMARL1  UF  E           K        DISK
     F**********  PAR01                             KRENAMEPAR02
      **********
     F**********SMHDL   UF  E           K        DISK
     F**********SMH1L   UF  E           K        DISK
      ***** END ADD ******* 07/18/02 **********************************
     F***  End   Del  ***  02/06/07  **************************************
      *
     FWTGTL     IF   E           K DISK
      *
     F*** BEG CHG *** 04/27/00 **********************************
     F**STABL***IF**E********** K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END CHG *** 04/27/00 **********************************
      *
     F** BEG ADD *** 02/28/03 *****************************************
      * Zip Codes by County (CO#/STATE/ZIPCODE)
     FWMCZL     IF   E           K DISK
     F** END ADD *** 02/28/03 *****************************************
      *
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PSY01)
     F                                     IGNORE(PPC01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PBC01)
     F                                     IGNORE(PCS01)
      *
      *
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
      *
     FWTCNP     IF   E             DISK
     F                                     RENAME(PCN01:PCN02)
     F*** BEG ADD ***** 02/03/98 ***********************************************
      *
     FWTMYP     IF   E             DISK
      *
     FWMDXL     IF   E           K DISK
      *
     FWMENL3    IF A E           K DISK
     F*** END ADD ***** 02/03/98 ***********************************************
      **** BEGIN ADD **** 07/15/98 *********************************************
      *
     FWMALL4    UF   E           K DISK
      **** END   ADD **** 07/15/98 *********************************************
      **** BEGIN ADD **** 10/14/98 *********************************************
     FWDF2L     IF   E           K DISK
      **** END   ADD **** 10/14/98 *********************************************
      ***** BEG ADD ******* 01/31/02 **********************************
      *
      * Employer Contact
     FWMECL1    IF   E           K DISK
      ***** END ADD ******* 01/31/02 **********************************
      *
      ***** BEG ADD ***** 03/04/03 ************************************
      * Statistical Reporting/Rating file
     FWDRRL1    IF   E           K DISK
      ***** END ADD ***** 03/04/03 ************************************
      *****BEGIN ADD***** 03/20/07 ************************************
     FWMALP     O    E             DISK
     F                                     RENAME(PAL01:PAL02)
      *****END   ADD***** 03/20/07 ************************************
      **** BEG ADD ****** 11/23/07 *********************************************
     FSTCTL1    IF   E           K DISK
     FSMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP05)
      **** END ADD ****** 11/23/07 *********************************************
     F*** BEG ADD ****** 08/21/08 ************************************
     F*  City/demographic cross reference file
     FWMCYL1    IF   E           K DISK
     F*
     F*  City/demographic cross reference file (physical)
     FWMCYP     IF   E             DISK
     F                                     RENAME(PCY01:PCY02)
     F*** END ADD ****** 08/21/08 ************************************
     F*
     F*** Beg Add ****** 10/22/18 ************************************
      * New Division# - workfile
     FWXDA1L    IF   E           K DISK    RENAME(PDA01:PDA0L)
     FWXDA1P    UF A E             DISK    RENAME(PDA01:PDA0P)
     F*** End Add ****** 10/22/18 ************************************
      *
      /COPY SCOPYMT01D
     D*
     D FK1             S             77    DIM(5) CTDATA PERRCD(1)              F KEYS SCR01
     D FK12            S             77    DIM(5) CTDATA PERRCD(1)              F KEYS SCR01
      ******* BEGIN CHG ****** 08/07/98 *******************************
     D**********          FK2     1   3 77               F KEY2 SCR02
      ***** BEG DEL ******* 08/15/00 **********************************
     D*************       FK2     1   4 77               F KEY2 SCR02
      ***** END DEL ******* 08/15/00 **********************************
      ******* END   CHG ****** 08/07/98 *******************************
     D STAF            S             10    DIM(6) CTDATA PERRCD(1)              POSSIBLE STAFF SEARC
     D** BEG ADD *** 10/31/97 *****************************************
     D CTGY            S              1    DIM(6) CTDATA PERRCD(6)              CATEGORYS
     D** END ADD *** 10/31/97 *****************************************
     D*
     D***  Begin Del  ***  02/06/07  **************************************
     D**********PCM03
     D**********    CMKEY                           C3KEY
     D**********    CMSEQ#                          C3SEQ#
     D**********    CMSTAT                          C3STAT
     D**********    CMPGM                           C3PGM
     D**********    CMTIME                          C3TIME
     D**********    CMDT                            C3DT
     D**********    CMDCDT                          C3DCDT
     D**********    CMDIFL                          C3DIFL
     D**********PAR02
     D**********    ARKEY                           A2KEY
     D**********    ARSEQ#                          A2SEQ#
     D**********    ARSTAT                          A2STAT
     D**********    ARPGM                           A2PGM
     D**********    ARTIME                          A2TIME
     D**********    ARDT                            A2DT
     D***  End   Del  ***  02/06/07  **************************************
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS1          DS
     D  DBRRN1               397    400B 0
     D***  BEGIN ADD  ***  11/07/97  *********************************
     D DBFDSQ          DS
     D  DBRRNQ               397    400B 0
     D***  END   ADD  ***  11/07/97  *********************************
     D*
     D OPTNM           C                   CONST('Employer Master Inf-
     D                                     ormation')
     D COMAPI          C                   CONST('WA001R')
     D AGTAPI          C                   CONST('WA002R')
     D DOTS            C                   CONST(' . . . . . . . . . -
     D                                     . . . . . . . . .')
     D CNT1            C                   CONST('Contact')
     D CNT2            C                   CONST('Attention')
     D CNT3            C                   CONST('Contact 3')
      *** BEG ADD ***** 11/06/06 ******************************************
     D CNT4            C                   CONST('Contact 4')
      *** END ADD ***** 11/06/06 ******************************************
      **** BEGIN ADD **** 06/15/98 *********************************************
     D DIGITS          C                   CONST('0123456789')
      **** END   ADD **** 06/15/98 *********************************************
     D*** BEG ADD ****** 08/21/08 ************************************
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D*
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D*** END ADD ****** 08/21/08 ************************************
      *
      ***** BEG ADD ***** 10/07/04 ****************************************
     D TZEROS          C                   CONST('000000000')
      ***** END ADD ***** 10/07/04 ****************************************
      *** BEG ADD ***** 09/20/06 ******************************************
     D XXAHLC          C                   CONST('Loss control rep  :')
     D XXAHWL          C                   CONST('Watch list  . . . :')
     D XXAHVP          C                   CONST('VIP member  . . . :')
     D XXAHSA          C                   CONST('Special acceptance:')
      *** END ADD ***** 09/20/06 ******************************************
      ***** BEG ADD ******* 06/29/07 **********************************
      *
     D OVRDB1          C                   CONST('OVRDBF FILE(WMEMLQ)-
     D                                      TOFILE(')
     D OVRDB2          C                   CONST('/WMEMLQ) SHARE(*YES-
     D                                     )')
     D DLTOV1          C                   CONST('DLTOVR FILE(WMEMLQ)')
     D RTVLIB          C                   CONST('RTVENVLIB')
      ***** END ADD ******* 06/29/07 **********************************
      *****BEGIN ADD***** 03/20/07 ************************************
     D TONE            C                   CONST('0000000001')
      *****END   ADD***** 03/20/07 ************************************
     D*
     D                 DS
     D  SEF                    1     40  0
     D                                     DIM(5)                               EFF DATES/SCREEN
     D  SCEFD1                 1      8  0
     D  SCEFD2                 9     16  0
     D  SCEFD3                17     24  0
     D  SCEFD4                25     32  0
     D  SCEFD5                33     40  0
     D                 DS
     D  EEF                    1     35  0
     D                                     DIM(5)                               EFF DATES/FILE
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0
     D                 DS
     D  SCA                    1     40  0
     D                                     DIM(5)                               CX DATES/SCREEN
     D  SCCAD1                 1      8  0
     D  SCCAD2                 9     16  0
     D  SCCAD3                17     24  0
     D  SCCAD4                25     32  0
     D  SCCAD5                33     40  0
     D                 DS
     D  ECA                    1     35  0
     D                                     DIM(5)                               CX DATES/FILE
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
     D                 DS
     D  AR                     1     90  2
     D                                     DIM(10)                              CLAIM PMTS
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
     D  XXTEST                 1     18
     D                 DS
     D  STF                    1     18
     D                                     DIM(6)                               STAFF CODES
     D  SCPSRV                 1      3
     D  SCUWTR                 4      6
     D  SCPACC                 7      9
     D  SCARVW                10     12
     D  SCAMGR                13     15
     D  SCLREP                16     18
     D** BEG ADD *** 10/31/97 *****************************************
     D                 DS
     D  OSTF                   1     18
     D                                     DIM(6)                               STAFF CODES
     D  EMPSRV                 1      3
     D  EMUWTR                 4      6
     D  EMPACC                 7      9
     D  EMARVW                10     12
     D  EMAMGR                13     15
     D  EMLREP                16     18
     D** END ADD *** 10/31/97 *****************************************
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D  QCQTE                 40     40
     D*** BEG ADD ***** 02/03/98 ***********************************************
      ***** BEG ADD ******* 11/23/07 **********************************
     D                 DS
     D  QCKEY1                 1     40
     D  @#CO#                  1      3  0
     D  @#FND                  4      6  0
     D  @#EMP#                 7     15  0
     D  @#DIV                 16     20  0
     D  QCQTE1                40     40
      ***** END ADD ******* 11/23/07 **********************************
     D SVLDA           DS          1024
     D  LDEND#                 1      5  0
     D  LDENDA                 1      5
     D  LDMYRR                 6     14  0
     D  LDEEDT                15     22  0
     D  LDFYR                 23     25  0
     D  LDPYRL                26     26
     D                 DS
     D  XXZIP                  1      9
     D  XXZIP5                 1      5
     D  XXZIP4                 6      9
     D*** END ADD ***** 02/03/98 ***********************************************

     D useDots         s              1n   inz('0')
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
      *

     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C***  BEGIN ADD  ***  08/11/99  *********************************
     C                   PARM                    ##LVL             3 0
     C                   PARM                    ##TYPE            1
     C***  END   ADD  ***  08/11/99  *********************************
      *
      *  Agent/Producer retrieve program parameter list
      *
     C     AGTPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM      EMFND         ##FND
     C                   PARM      EMEMP#        ##EMP#            9 0
     C                   PARM      EMDIV         ##DIV             5 0
     C                   PARM                    ##FYR             3 0
     C     SCAGT#        PARM                    ##AGT#            5 0
     C                   PARM                    ##ANAM           60
     C     SCPROD        PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
      ***** BEG ADD ******* 01/31/02 **********************************
      *
      *
      *===============================================================
      * KLIST
      *===============================================================
      ***** BEG ADD ******* 03/25/04 **********************************
      *
      *  New Business Tracking
      *
     C     NBKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
      *
      ***** END ADD ******* 03/25/04 **********************************
      *
      *  Employer Contact
      *
     C     ECKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    KKPCNT            1
      *
      ***** END ADD ******* 01/31/02 **********************************
      *
      *  GROUP MASTER FILE
      *
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
      *
      *** BEGIN ADD *** 06/23/99 ***********************************************
      *+*+ BEGIN DEL *** 08/18/06 **********************************************
     C*+*+*+*+   FNKY1     KLIST
     C*+*+*+*+             KFLD           ##CO#
     C*+*+*+*+             KFLD           SCFFND
      *+*+ END   DEL *** 08/18/06 **********************************************
      *** END   ADD *** 06/23/99 ***********************************************
      *
      **** BEGIN ADD **** 10/14/98 *********************************************
      *
      * KEY LIST FOR THE GROUP AUXILIARY FILE
      *
     C     F2KY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
      **** END   ADD **** 10/14/98 *********************************************
      *
      *  EMPLOYER MASTER FILE
      *
     C     EMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
      *
     C     EMKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
      *
     C***  BEGIN ADD  ***  03/31/03  *********************************
     C     EMKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXFEIN
     C***  END   ADD  ***  03/31/03  *********************************
      *
      *  EMPLOYER DETAIL FILE
      *
     C     ELKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
      *
      **** BEGIN ADD **** 08/18/00 *********************************************
     C     ELKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SVDIV
      **** END ADD ****** 08/18/00 *********************************************
      *
      **** BEGIN ADD **** 03/06/06 *********************************************
     C     ELKY3         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
      **** END ADD ****** 03/06/06 *********************************************
      ***** BEG ADD ******* 08/10/98 **********************************
      *
      *  Employer Cross Reference
      *
      ***** BEG DEL ******* 09/18/98 **********************************
     C*********  ZEXKY     KLIST
     C*********            KFLD           ##CO#
     C*********            KFLD           SCFND
     C*********            KFLD           SCEMP#
     C*********            KFLD           SCDIV
      ***** END DEL ******* 09/18/98 **********************************
      ***** BEG ADD ******* 09/18/98 **********************************
     C     ZEXKY         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
      ***** END ADD ******* 09/18/98 **********************************
      ***** END ADD ******* 08/10/98 **********************************
      *
      *  CLAIM MASTER FILE
      *
     C     CMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    XXACDT
      *
     C     CMKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
      *
      *  KEY LIST FOR THE REGION
      *
     C     REKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCRGN#
      *
      *  KEY LIST FOR CHAPTER
      *
     C     CPKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCCHAP
     C***  BEGIN ADD  ***  07/19/99  *********************************
     C*
     C*  AGENCY BILLING DETAIL FILE
     C*
     C     ABKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C***  END   ADD  ***  07/19/99  *********************************
      *
      *  KEY LIST FOR THE OFFICE MASTER FILE
      *
     C     OFKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCOFF#
      *
      *  KEY LIST FOR THE OFFICE CHANGE AUXILIARY FILE
      *
     C     AFKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
      *
      *  KEY LIST FOR ADMIN/DARTS FILE
      *
     C     EXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
      *
      *  KEY LIST FOR THE GENERIC TABLE FILE
      *
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XZTBID
     C                   KFLD                    XXKEY
      *
      *  SYSTEM TABLE FILE
      *
     C     STKY          KLIST
     C                   KFLD                    XXKEY
      *
      *  TABLE FILE KEY
      *
     C     TBKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXKEY
      *
      *   KEY LIST FOR THE SYSTEM CONTROL FILE
      *
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
      *
      *  AGENT QUOTE FILE
      *
     C     QTKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    @@QTE
      *
      *  DELETION OF EMPLOYER RELATED FILES
      *
     C     DLTKY         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C***  Begin Del  ***  02/06/07  **************************************
      **********
      ***DARTS*REMARK DETAIL FILE
      **********
     C********** C3KY      KLIST
     C**********           KFLD           QCKEY
      **********
      ***DARTS*ADDITIONAL REMARK FILE
      **********
     C********** A2KY      KLIST
     C**********           KFLD           C3KEY
     C**********           KFLD           C3SEQ#
     C***  End   Del  ***  02/06/07  **************************************
      *** BEG ADD ***** 02/03/98 ***********************************************
      *
      *  ENDORS/DARTS CONTROL DEFINITION
      *
     C     DXKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
      *
     C***** beg add ***** 10/22/18 ***********************
      * WXDA1L workfile
     C     DAKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    SVDIV
     C                   KFLD                    LDFYR
     C                   KFLD                    Q1JOBN
     C                   KFLD                    LDEND#
     C                   KFLD                    ENAGT2
     C***** end add ***** 10/22/18 ***********************
      *
      *  ENDORSEMENT REQUEST WORKFILE
      *
     C     ENKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    LDFYR
     C                   KFLD                    Q1JOBN
     C                   KFLD                    LDEND#
      *** END ADD ***** 02/03/98 ***********************************************
      *
      **** BEGIN ADD **** 07/15/98 *********************************************
      *
      * ENTITY/LOCATION FILE
      *
     C     ALKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
      **** END   ADD **** 07/15/98 *********************************************
     C*** BEG ADD ****** 08/21/08 ************************************
     C*
     C*  City/Demographic Cross Reference file
     C*
     C     CYKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXST
     C                   KFLD                    XXCTY
     C*
     C     CYKY1         KLIST
     C                   KFLD                    ##CO#
     C*** END ADD ****** 08/21/08 ************************************
      *
     *** BEG ADD *** 02/28/03 *****************************************
      *
      * Zip Codes by County File
      *
     C     CZKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##CZST            2
     C                   KFLD                    ##ZIP5            5 0
     *** END ADD *** 02/28/03 *****************************************
      *
      ***** BEG ADD ***** 03/04/03 ************************************
     C     RRKY          KLIST
     C                   KFLD                    ##CO#
      ***** BEG CHG ******* 09/30/03 **********************************
     C*********            KFLD           ##FND
     C                   KFLD                    SCFND
      ***** END CHG ******* 09/30/03 **********************************
     C                   KFLD                    ##RRST            2
      ***** END ADD ***** 03/04/03 ************************************
      *
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##RRN             9 0
      *
     C     ACDPL1        PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##RRN             9 0
     C                   PARM                    XXKEY
      *
     C     ACDPL2        PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##RRN             9 0
     C                   PARM      DBRRN1        ##RRN2            9 0
      *
     C     ACDPL3        PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##RRN             9 0
     C                   PARM                    XXKEY
     C                   PARM      2             ##VIEW            1 0
      *
     C     ACDPL4        PLIST
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      '2'           ##CALL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM                    XXKEY
     C                   PARM      ZZST          ##ST              2
     C                   PARM      2             ##VIEW            1 0
     C*
     C***  BEGIN ADD  ***  08/20/99  *********************************
     C     PLCCSC        PLIST
     C                   PARM                    ##UTYP            1
     C                   PARM                    ##ULVL            3
     C                   PARM                    ##CTYP            1
     C                   PARM                    ##CLVL            3
     C                   PARM      '1'           ##EXEC            1
     C                   PARM      ' '           ##RES             1
     C***  END   ADD  ***  08/20/99  *********************************
      *
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@QTE             1
      *
     C     UPRMPL        PLIST
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      3             ##VIEW            1 0
     C*** BEG ADD ****** 08/21/08 ************************************
     C*
     C*  City/Demographic cross reference search parameter list
     C*
     C     CYSPL         PLIST
     C                   PARM      '2'           ##CALL
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      *BLANKS       ##ST
     C*** END ADD ****** 08/21/08 ************************************
      *
     C     *LIKE         DEFINE    SCFND         XXFND
     C     *LIKE         DEFINE    Q1PGM         XXPGM
     C     *LIKE         DEFINE    EMEFD1        XXEFDT
     C     *LIKE         DEFINE    EMEFD1        XXDAT1
     C     *LIKE         DEFINE    EMEFD1        XXDAT2
     C     *LIKE         DEFINE    EMEFD1        XXACDT
     C     *LIKE         DEFINE    CNKEY         XXKEY
     C     *LIKE         DEFINE    SCPSRV        ##STAF
     C     *LIKE         DEFINE    EMOFF#        XXOFF#
     C     *LIKE         DEFINE    FNEXDT        SYSDT
     C     *LIKE         DEFINE    EMCKTP        XXCKTP
     C     *LIKE         DEFINE    EMCKTP        @@CKTP
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    GTTBID        XZTBID
     C     *LIKE         DEFINE    CPCHAP        ##CHAP
     C     *LIKE         DEFINE    *IN60         XXMANC                         MC INSTALLED
      ***  BEGIN ADD  ***  11/07/97  *********************************
      **** BEGIN CHG **** 07/15/98 *********************************************
      ********** *LIKE     DEFN QTNAME    SVNAME
     C     *LIKE         DEFINE    EMNAME        SVNAME
     C     *LIKE         DEFINE    QTNAME        SVNAM2
      **** END   CHG **** 07/15/98 *********************************************
     C***  BEGIN ADD  ***  09/22/99  *********************************
     C     *LIKE         DEFINE    EMFDNM        SVFDNM
     C     *LIKE         DEFINE    EMFEIN        SVFEIN
      *** BEG ADD ***** 04/18/03 ***********************************************
     C     *LIKE         DEFINE    EMFEIN        S2FEIN
      *** END ADD ***** 04/18/03 ***********************************************
     C***  END   ADD  ***  09/22/99  *********************************
      ***  END   ADD  ***  11/07/97  *********************************
      *** BEG ADD ***** 02/03/98 ***********************************************
     C     *LIKE         DEFINE    EMPAD1        XXADR1
     C     *LIKE         DEFINE    EMPAD1        XXADR2
     C     *LIKE         DEFINE    EMPAD1        XXADR3
      *** END ADD ***** 02/03/98 ***********************************************
      **** BEGIN ADD **** 07/15/98 *********************************************
     C     *LIKE         DEFINE    EMCHAP        SVCHAP
      **** END   ADD **** 07/15/98 *********************************************
     C*** BEG ADD ****** 08/21/08 ************************************
     C     *LIKE         DEFINE    EMPST         XXST
     C     *LIKE         DEFINE    EMPCTY        XXCTY
     C*** END ADD ****** 08/21/08 ************************************
      *
      *** BEGIN ADD *** 06/23/99 ***********************************************
     C     *LIKE         DEFINE    EMFND         ##FFND
     C     *LIKE         DEFINE    EMEMP#        ##FEMP
     C     *LIKE         DEFINE    EMDIV         ##FDIV
     C     *LIKE         DEFINE    EMFND         ##TFND
     C     *LIKE         DEFINE    EMEMP#        ##TEMP
     C     *LIKE         DEFINE    EMDIV         ##TDIV
      *** END   ADD *** 06/23/99 ***********************************************
     C***** BEGIN ADD **** 03/10/00 ***********************************
     C     *LIKE         DEFINE    EMTPOB        SVTPOB
     C***** END   ADD **** 03/10/00 ***********************************
     C*** BEGIN ADD ***  04/25/06  ************************************
     C     *LIKE         DEFINE    EMNAMP        SVNAMP
     C     *LIKE         DEFINE    EMFDNP        SVFDNP
     C*** END ADD *****  04/25/06  ************************************
      **** BEGIN ADD **** 08/18/00 *********************************************
     C     *LIKE         DEFINE    EMDIV         SVDIV
      **** END ADD ****** 08/18/00 *********************************************
     C***  BEGIN ADD  ***  03/31/03  *********************************
     C     *LIKE         DEFINE    EMFEIN        XXFEIN
     C***  END   ADD  ***  03/31/03  *********************************
      *
     C***** BEG ADD *****  10/07/04  *********************************
     C     *LIKE         DEFINE    EXSRRB        XXSRRB
     C     *LIKE         DEFINE    EXSRRQ        XXSRRQ
     C     *LIKE         DEFINE    EXNRRB        XXNRRB
     C     *LIKE         DEFINE    EXNRRQ        XXNRRQ
     C***** END ADD *****  10/07/04  *********************************
      *
     C***** BEG ADD *****  01/05/06  *********************************
     C     *LIKE         DEFINE    FNPEOF        XXPEOF
     C***** END ADD *****  01/05/06  *********************************
     C*** BEG ADD *****  11/23/07  ************************************
     C     *LIKE         DEFINE    EXCNDR        XXCNDR
     C     *LIKE         DEFINE    EXCNST        XXCNST
     C     *LIKE         DEFINE    EMNAME        HNAME
     C     *LIKE         DEFINE    EMNAM2        HNAM2
     C     *LIKE         DEFINE    EMFDNM        HFDNM
     C     *LIKE         DEFINE    EMFDN2        HFDN2
     C*** END ADD *****  11/23/07  ************************************
      ****  BEGIN ADD **** 02/28/11 *********************************  *********
     C     *LIKE         DEFINE    EMDIV         X1DIV
      ****  END   ADD **** 02/28/11 ********************************** *********
      *
      **** BEGIN ADD **** 03/05/98 *********************************************
     C                   Z-ADD     0             I1                3 0
      **** END   ADD **** 03/05/98 *********************************************
      **** BEGIN ADD **** 06/15/98 *********************************************
     C                   MOVE      *BLANKS       XXRSK#            9
      **** END   ADD **** 06/15/98 *********************************************
      *
     C/COPY SCOPYMT01C
      *
     C***  BEGIN ADD  ***  08/11/99  *********************************
     C*===============================================================
     C*  CHECK SECURITY LEVEL FOR ADD
     C*===============================================================
     C*
     C     CHKSEC        BEGSR
     C*
     C                   CLEAR                   EXADLL
     C                   CLEAR                   EXADLT
     C                   CLEAR                   EXADQL
     C                   CLEAR                   EXADQT
     C*
     C     EXKY          CHAIN     PEX01                              60
      ***** BEG ADD ******* 11/23/07 **********************************
     C                   MOVE      EXCNDR        XXCNDR
     C                   MOVE      EXCNST        XXCNST
      ***** END ADD ******* 11/23/07 **********************************
     C*
     C     @@MODE        IFEQ      'A'
     C*
     C     @@QTE         IFEQ      'Q'
     C*
     C                   MOVE      EXADQL        ##CLVL
     C                   MOVE      EXADQT        ##CTYP
     C                   ELSE
     C                   MOVE      EXADLL        ##CLVL
     C                   MOVE      EXADLT        ##CTYP
     C                   ENDIF
     C*
     C                   MOVE      ##LVL         ##ULVL
     C                   MOVE      ##TYPE        ##UTYP
     C*
     C                   CALL      'SRCCSC'
     C                   PARM                    ##UTYP            1
     C                   PARM                    ##ULVL            3
     C                   PARM                    ##CTYP            1
     C                   PARM                    ##CLVL            3
     C                   PARM      '1'           ##EXEC            1
     C                   PARM      ' '           ##RES             1
     C*
     C     ##RES         IFEQ      *ON
     C                   EXFMT     WDW01
     C  N65              MOVEL     FK1(5)        SCFKL1
     C   65              MOVEL     FK12(5)       SCFKL1
     C                   Z-ADD     1             #OFSCR
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN09
     C                   ELSE
     C                   Z-ADD     3             #OFSCR
     C  N65              MOVEL     FK1(2)        SCFKL1
     C   65              MOVEL     FK12(2)       SCFKL1
     C                   ENDIF                                                  ##RES=*ON
     C*
     C                   ENDIF                                                  @@MODE=A
     C*
     C                   ENDSR                                                  CHKSEC
     C***  END   ADD  ***  08/11/99  *********************************
     C*
     C***  Begin Add  ***  05/23/08  **************************************
     C*====================================================================
     C* Update CiCenter with changes
     C*====================================================================
     C     CICNTR        BEGSR
     C*
     C     *LIKE         DEFINE    EMFEIN        SVFID
     C     *LIKE         DEFINE    SVFID         ##OFID
     C     *LIKE         DEFINE    SVDBA         ##ODBA
     C     *LIKE         DEFINE    XX100         SVDBA
     C*** Begin Add ** 08/20/13 ****************************
     C     *LIKE         DEFINE    SVDIV         ##ODIV
     C*** End   Add ** 08/20/13 ****************************
     C*
     C                   CLEAR                   #2TYPE
     C     EMNAME        CAT(P)    EMNAM2        XX100           100
     C*
     C                   SELECT
     C     SVDBA         WHENNE    XX100
     C                   MOVEL     'DBA'         #2TYPE
     C     SVFID         WHENNE    EMFEIN
     C                   MOVEL     'FEIN'        #2TYPE
     C                   ENDSL
     C*
     C     #2TYPE        IFNE      *BLANK
     C                   CALL      'CIEMPCHG'                           99
     C                   PARM      DBRRN         ##RRN
     C                   PARM                    #2TYPE           10
     C                   PARM      SVFID         ##OFID
     C                   PARM      SVDBA         ##ODBA
     C*** Begin Add ** 08/20/13 ****************************
     C                   PARM      SVDIV         ##ODIV
     C*** End   Add ** 08/20/13 ****************************
     C                   ENDIF
     C*
     C     EMNAME        CAT(P)    EMNAM2        SVDBA
     C                   MOVE      EMFEIN        SVFID
     C*
     C                   ENDSR
     C***  End   Add  ***  05/23/08  **************************************
     C*
      *** BEG ADD ***** 02/03/98 ***********************************************
      *===============================================================
      *  CHECK FOR REPORTABLE CHANGE AS MID TERM ENDORSEMENT
      *===============================================================
     C     CKCHGS        BEGSR
      *
     C     *DTAARA       DEFINE    *LDA          SVLDA
     C     *LOCK         IN        *DTAARA
     C                   OUT       *DTAARA
      *
     C     LDENDA        IFNE      *ZEROS
     C     LDENDA        ANDNE     *BLANKS
      *** BEGIN ADD *** 08/28/98 ***********************************************
     C***********@@QTE*****ANDEQ*BLANKS
     C     @@QTE         ANDNE     'Q'
      *** END ADD ***** 08/28/98 ***********************************************
      *
     C     *IN41         IFEQ      *ON                                          ADD DIV MODE
     C     @@MODE        OREQ      'E'                                          DELETE MODE
      **** BEGIN ADD **** 03/07/00 *********************************************
     C     F2KY          CHAIN     PF201                              60
      **** END ADD ****** 03/07/00 *********************************************
      *
      *  GET ENDORS/DARTS INFORMATION
      *
     C     DXKY          CHAIN     PDX01                              60
      *
     C                   CALL      AGTAPI        AGTPL
      *
      *  SETUP FOR NEXT REQUEST NUMBER
      *
     C     ENKY          SETGT     PEN01
     C     ENKY          READPE    PEN01                                  60
     C   60              Z-ADD     *ZERO         ENREQ#
     C                   Z-ADD     *ZERO         ENSEQ#
      *
     C                   MOVE      EMCO#         ENCO#
     C                   MOVE      EMFND         ENFND
     C                   MOVE      EMEMP#        ENEMP#
      **** BEGIN CHG **** 08/18/00 *********************************************
     C************IN41*****IFEQ *ON
     C*********************Z-ADD*ZERO     ENDIV
     C*********************ELSE
     C                   MOVE      SVDIV         ENDIV
     C*
     C*  SAVE VALUE OF DIVISION THAT WAS ADDED FOR W7418R PROCESSING
     C*
     C                   MOVE      EMDIV         ENAGT2
     C*********************ENDIF
      **** END CHG ****** 08/18/00 *********************************************
     C                   MOVE      LDFYR         ENFYR
     C                   MOVE      Q1JOBN        ENJOB
     C                   MOVE      LDEND#        ENEND#
     C                   ADD       1             ENREQ#
      *
     C                   MOVE      LDEEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENEEDT
      *
     C                   MOVEL     EMNAME        ENNAM1
     C                   MOVEL     EMFDNM        ENFDN1
     C*** BEGIN ADD ***  05/14/04  ************************************
     C                   MOVEL     EMNAME        ENENM1
     C                   MOVEL(P)  EMNAM2        XXNM10           10
     C                   MOVE      XXNM10        ENENM1
     C                   MOVEL     EMFDNM        ENEFN1
     C                   MOVEL(P)  EMFDN2        XXNM10           10
     C                   MOVE      XXNM10        ENEFN1
     C                   MOVE      EMNAM2        ENENM2
     C                   MOVE      EMFDN2        ENEFN2
     C*** END ADD *****  05/14/04  ************************************
      ***** BEG DEL ******* 01/31/02 **********************************
     C*************        MOVELEMCONT    ENCON1
     C*************        MOVELEMATTN    ENATT1
     C*************        MOVELEMCNT3    ENCN31
      ***** END DEL ******* 01/31/02 **********************************
      ***** BEG ADD ******* 01/31/02 **********************************
      *
      * Get Employer Contact Names / phone
      *
     C                   MOVE      '1'           KKPCNT
     C                   EXSR      SRCONT
     C                   MOVEL(P)  XXCNT         ENCON1
      *
     C                   MOVE      '2'           KKPCNT
     C                   EXSR      SRCONT
     C                   MOVEL(P)  XXCNT         ENATT1
      *
     C                   MOVE      '3'           KKPCNT
     C                   EXSR      SRCONT
     C                   MOVEL(P)  XXCNT         ENCN31
      *
      ***** END ADD ******* 01/31/02 **********************************
     C                   MOVE      EMFEIN        ENFEN1
     C                   MOVEL     EMTPOB        ENTOB1
     C                   MOVEL     EMMAD1        ENMA11
     C                   MOVEL     EMMAD2        ENMA21
     C                   MOVEL     EMMCTY        ENMCT1
     C                   MOVEL     EMMST         ENMST1
     C                   MOVEL     EMMZIP        ENMZP1
     C                   MOVEL     EMPAD1        ENPA11
     C                   MOVEL     EMPAD2        ENPA21
     C                   MOVEL     EMPCTY        ENPCT1
     C                   MOVEL     EMPST         ENPST1
     C                   MOVEL     EMPZIP        ENPZP1
     C                   MOVEL     EMCNTY        ENCNT1
      **** BEGIN ADD **** 10/14/98 *********************************************
     C                   MOVE      F2CAR#        ENCAR1
      **** END   ADD **** 10/14/98 *********************************************
      **** BEGIN ADD **** 03/07/00 *********************************************
     C                   MOVE      F2CAR#        ENCAR2
      **** END ADD ****** 03/07/00 *********************************************
      **** BEGIN ADD **** 11/23/98 *********************************************
      **** BEGIN CHG **** 08/18/00 *********************************************
     C***  BEGIN CHG  ***  08/05/99  *********************************
     C********** ELKY      CHAINPEL01                60
     C***********ELKY******CHAINPEL01               N60
     C***  END   CHG  ***  08/05/99  *********************************
     C     ELKY2         CHAIN(N)  PEL01                              60
      **** END CHG ****** 08/18/00 *********************************************
      *
     C     *IN60         DOWEQ     *OFF
     C     ELPTDT        ANDLE     ENEEDT
      *
     C***  BEGIN CHG  ***  08/18/00  *********************************
     C***  BEGIN CHG  ***  08/05/99  *********************************
     C********** ELKY      READEPEL01                    60
     C***********ELKY******READEPEL01               N    60
     C***  END   CHG  ***  08/05/99  *********************************
     C     ELKY2         READE(N)  PEL01                                  60
     C***  END CHG ******  08/18/00  *********************************
      *
     C                   ENDDO                                                  *IN60 DOWEQ *OFF
      *
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     ELPFDT        ENPFD1
     C                   ENDIF                                                  *IN60
      **** END   ADD **** 11/23/98 *********************************************
      *
     C     *IN41         IFEQ      *ON
     C                   MOVE      'Y'           ENADDF
     C                   MOVEL(P)  DXDVAD        ENRMCD
     C                   ELSE
     C                   MOVE      'Y'           ENDLDF
     C                   MOVEL(P)  DXDVDL        ENRMCD
     C                   ENDIF
      *
     C                   MOVEL     FNNAME        ENFNNM
     C                   MOVE      ##AGT#        ENAGT#
     C                   MOVEL     ##ANAM        ENAGNM
     C                   MOVEL     Q1USER        ENRUSR
      *
     C     LDMYRR        CHAIN     PMY01                              60
     C                   MOVEL     MYDSC1        ENDSC1
     C                   MOVEL     MYDSC2        ENDSC2
     C                   MOVEL     MYDSC3        ENDSC3
      *
     C     *IN41         IFEQ      *ON                                          ADD DIV MODE
     C                   MOVEL     EMNAME        ENNAM2
     C                   MOVEL     EMFDNM        ENFDN2
     C*** BEGIN ADD ***  05/14/04  ************************************
     C                   MOVEL     EMNAME        ENANM1
     C                   MOVEL(P)  EMNAM2        XXNM10           10
     C                   MOVE      XXNM10        ENANM1
     C                   MOVEL     EMFDNM        ENAFN1
     C                   MOVEL(P)  EMFDN2        XXNM10           10
     C                   MOVE      XXNM10        ENAFN1
     C                   MOVE      EMNAM2        ENANM2
     C                   MOVE      EMFDN2        ENAFN2
     C*** END ADD *****  05/14/04  ************************************
      ***** BEG DEL ******* 01/31/02 **********************************
     C*************        MOVELEMCONT    ENCON2
     C*************        MOVELEMATTN    ENATT2
     C*************        MOVELEMCNT3    ENCN32
      ***** END DEL ******* 01/31/02 **********************************
      ***** BEG ADD ******* 01/31/02 **********************************
     C                   MOVEL     ENCON1        ENCON2
     C                   MOVEL     ENATT1        ENATT2
     C                   MOVEL     ENCN31        ENCN32
      ***** END ADD ******* 01/31/02 **********************************
     C                   MOVE      EMFEIN        ENFEN2
     C                   MOVEL     EMTPOB        ENTOB2
     C                   MOVEL     EMMAD1        ENMA12
     C                   MOVEL     EMMAD2        ENMA22
     C                   MOVEL     EMMCTY        ENMCT2
     C                   MOVEL     EMMST         ENMST2
     C                   MOVEL     EMMZIP        ENMZP2
     C                   MOVEL     EMPAD1        ENPA12
     C                   MOVEL     EMPAD2        ENPA22
     C                   MOVEL     EMPCTY        ENPCT2
     C                   MOVEL     EMPST         ENPST2
     C                   MOVEL     EMPZIP        ENPZP2
     C                   MOVEL     EMCNTY        ENCNT2
     C                   ELSE                                                   DELETE MODE
     C                   MOVEL     *BLANKS       ENNAM2
     C                   MOVEL     *BLANKS       ENFDN2
     C                   MOVEL     *BLANKS       ENCON2
     C                   MOVEL     *BLANKS       ENATT2
     C                   MOVEL     *BLANKS       ENCN32
     C                   MOVE      *ZEROS        ENFEN2
     C                   MOVEL     *BLANKS       ENTOB2
     C                   MOVEL     *BLANKS       ENMA12
     C                   MOVEL     *BLANKS       ENMA22
     C                   MOVEL     *BLANKS       ENMCT2
     C                   MOVEL     *BLANKS       ENMST2
     C                   MOVEL     *BLANKS       ENMZP2
     C                   MOVEL     *BLANKS       ENPA12
     C                   MOVEL     *BLANKS       ENPA22
     C                   MOVEL     *BLANKS       ENPCT2
     C                   MOVEL     *BLANKS       ENPST2
     C                   MOVEL     *BLANKS       ENPZP2
     C                   MOVEL     *BLANKS       ENCNT2
     C                   ENDIF                                                  *IN41 = *ON
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENDT
      *
     C                   MOVEL     DSTIME        ENTIME
     C                   MOVEL     Q1USER        ENUSER
     C                   MOVEL     Q1PGM         ENPGM
     C                   MOVE      'A'           ENSTAT
     C                   MOVEL     Q1USER        ENRUSR
      *
      *  BUILD UP BEFORE/AFTER LINES, & WRITE WMENP RECORDS
      *
     C     EMPAD1        IFNE      *BLANKS                                      LOAD PHYS ADDR
     C                   MOVEL     EMPAD1        XXADR1
     C                   MOVEL     EMPAD2        XXADR2
     C                   MOVE      EMPZIP        XXZIP
     C     EMPCTY        CAT(P)    ',':0         XXADR3
      *** BEG CHG ***** 06/23/98 ***********************************************
      *****************    CAT  EMMST:1   XXADR3
     C                   CAT       EMPST:1       XXADR3
      *** END CHG ***** 06/23/98 ***********************************************
     C                   CAT       XXZIP5:1      XXADR3
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         XXADR3
     C                   CAT       XXZIP4        XXADR3
     C                   ENDIF
     C                   ELSE                                                   LOAD MAIL ADDR
     C                   MOVEL     EMMAD1        XXADR1
     C                   MOVEL     EMMAD2        XXADR2
     C                   MOVE      EMMZIP        XXZIP
     C     EMMCTY        CAT(P)    ',':0         XXADR3
     C                   CAT       EMMST:1       XXADR3
     C                   CAT       XXZIP5:1      XXADR3
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         XXADR3
     C                   CAT       XXZIP4        XXADR3
     C                   ENDIF
     C                   ENDIF
      *
     C     *IN41         IFEQ      *ON                                          ADD DIVISION
     C                   MOVE      *BLANKS       ENBLIN
      *  EMPLOYER NAME
     C                   MOVEL     EMNAME        ENALIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
      *  EMPLOYER FEDERAL NAME
     C                   MOVEL     EMFDNM        ENALIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
      *  EMPLOYER ADDRESS 1
     C                   MOVEL     XXADR1        ENALIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
      *  EMPLOYER ADDRESS 2
     C     XXADR2        IFNE      *BLANKS
     C                   MOVEL     XXADR2        ENALIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C                   ENDIF
      *  EMPLOYER CITY/ST/ZIP
     C                   MOVEL     XXADR3        ENALIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
      *
     C                   ELSE                                                   DELETE DEIVISION
      *
     C                   MOVE      *BLANKS       ENALIN
      *  EMPLOYER NAME
     C                   MOVEL     EMNAME        ENBLIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
      *  EMPLOYER FEDERAL NAME
     C                   MOVEL     EMFDNM        ENBLIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
      *  EMPLOYER ADDRESS 1
     C                   MOVEL     XXADR1        ENBLIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
      *  EMPLOYER ADDRESS 2
     C     XXADR2        IFNE      *BLANKS
     C                   MOVEL     XXADR2        ENBLIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C                   ENDIF
      *  EMPLOYER CITY/ST/ZIP
     C                   MOVEL     XXADR3        ENBLIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C                   ENDIF                                                  *IN41 = *ON
      *
     C                   ENDIF                                                  *IN41=*ON/@@MODE='E'
      *
     C                   ENDIF                                                  LDENDA <> *ZERO
      *
       //*** beg add ***** 10/22/18 ***********************
       // For a division ADD in mid-term endorsement,
       //   add rcd in workfile for future use
      /Free
       If ENADDF = 'Y';
          Chain DAKY pda0l;
          If NOT %found(WXDA1L);
             clear pda0p;
             PDCO#  = ENCO# ;
             PDFND  = ENFND ;
             PDEMP# = ENEMP#;
             PDDIV  = ENDIV ;
             PDFYR  = ENFYR ;
             PDNWDV = ENAGT2;   //new division#
             PDEND# = ENEND#;
             PDNAME = ENNAM1;   //employer name
             PDEFDT = ENEEDT;
             PDJOBN = Q1JOBN;
             PDUSER = Q1USER;
             PDPGM  = Q1PGM ;
             PDTIME = ENTIME;
             PDDT   = ENDT;
             PDNJBN = *ZERO;
             Write  pda0p;
             unlock wxda1p;
          EndIf;
       EndIf;
      /end-free
       //*** end add ***** 10/22/18 ***********************
      *
     C                   ENDSR
      *** END ADD ***** 02/03/98 ***********************************************
      *
      *** BEGIN ADD *** 06/23/99 ***********************************************
      *===============================================================
      * CPYEMP - SUBROUTINE FOR COPYING EMPLOYER/DIVISION PROCESSING
      *===============================================================
     C     CPYEMP        BEGSR
      *
     C*+*+ BEGIN DEL  ***  08/18/06  *********************************
     C*+*+*+*+             CLEARDBRCDF
      *+*+*+*+
     C*+*+*+*+   @@RRN     CHAINDBRCDF              N99
      *+*+*+*+
     C*+*+*+*+             Z-ADDEMFND     SCFFND
     C*+*+*+*+             Z-ADDEMEMP#    SCFEMP
     C*+*+*+*+             Z-ADDEMDIV     SCFDIV
     C*+*+*+*+             Z-ADD0         SCFND
     C*+*+*+*+             Z-ADD0         SCEMP#
     C*+*+*+*+             Z-ADD0         SCDIV
      *+*+*+*+
     C***  BEGIN CHG  ***  08/04/99  *********************************
     C********** FNKY1     CHAINPFN01                60
     C*+*+*+*+   FNKY1     CHAINPFN01               N60
     C***  END   CHG  ***  08/04/99  *********************************
      *+*+*+*+
     C*+*+*+*+             MOVELFNSHNM    SCFSNM    P
     C*+*+*+*+             MOVELEMNAME    SCESNM    P
      *+*+*+*+
     C*+*+*+*+   *IN90     DOUEQ*OFF
     C*+*+*+*+   *INKC     OREQ *ON
     C*+*+*+*+   *INKL     OREQ *ON
      *+*+*+*+
     C*+*+*+*+             MOVE *OFF      *IN90
      *+*+*+*+
     C*+*+*+*+             EXFMTSCRCOPY
      *+*+*+*+
     C*+*+*+*+   *INKJ     CASEQ*ON       KJDO           90
     C*+*+*+*+             ENDCS
      *+*+*+*+
     C*+*+*+*+   *IN90     IFEQ *OFF
     C*+*+*+*+   *INKC     ANDEQ*OFF
     C*+*+*+*+   *INKL     ANDEQ*OFF
      *+*+*+*+
      *+*+*+*+  O' group
     C*+*+*+*+   SCFND     IFEQ *ZEROS
     C*+*+*+*+             SETON                     24  90
     C*+*+*+*+             ENDIF
      *+*+*+*+
     C***  BEGIN CHG  ***  08/04/99  *********************************
     C********** FNKY1     CHAINPFN01                60
     C*+*+*+*+   FNKY1     CHAINPFN01               N60
     C***  END   CHG  ***  08/04/99  *********************************
      *+*+*+*+
     C*+*+*+*+ N90      *IN60     IFEQ *ON
     C*+*+*+*+             SETON                     23  90
     C*+*+*+*+             ENDIF                           *IN60=*ON
      *+*+*+*+
      *+*+*+*+   Edit 'TO' Employer/Division
     C*+*+*+*+             EXSR ERRCKW
      *+*+*+*+
     C*+*+*+*+             ENDIF                           *IN90=*OFF
      *+*+*+*+
     C*+*+*+*+             ENDDO                           *IN90=*OFF
      *+*+*+*+
     C*+*+*+*+   *INKC     IFEQ *OFF
     C*+*+*+*+   *INKL     ANDEQ*OFF
      *+*+*+*+
     C*+*+*+*+             CALL 'W0050R03'
     C*+*+*+*+             PARM           ##CO#
     C*+*+*+*+             PARM SCFFND    ##FFND
     C*+*+*+*+             PARM SCFEMP    ##FEMP
     C*+*+*+*+             PARM SCFDIV    ##FDIV
     C*+*+*+*+             PARM SCFND     ##TFND
     C*+*+*+*+             PARM SCEMP#    ##TEMP
     C*+*+*+*+             PARM SCDIV     ##TDIV
     C*+*+*+*+             PARM @@QTE     ##QTE
      *+*+*+*+
     C*+*+*+*+             ENDIF                           *INKC=*OFF=*INKL
      *+*+*+*+
     C*+*+*+*+             RETRN
      *+*+*+*+
     C*+*+ END   DEL  ***  08/18/06  *********************************
     C*+*+ BEGIN ADD  ***  08/18/06  *********************************
      *  Copy employer / division
      *
     C                   CALL      'W0050R03'
     C     *INKC         PARM      *BLANK        ##KC              1
     C     *INKL         PARM      *BLANK        ##KL              1
     C                   PARM      'A'           ##MODE            1
     C                   PARM      @@RRN         ##RRN             9 0
     C                   PARM      @@QTE         ##QTE             1
      *
     C                   SETON                                        LR
      *++++++++ Begin add +++++++++++ 12/01/06 ++++++++++++++++++++++++
     C                   RETURN
      *++++++++ End   add +++++++++++ 12/01/06 ++++++++++++++++++++++++
     C*+*+ END   ADD  ***  08/18/06  *********************************
     C                   ENDSR                                                  CPYEMP
      *** END   ADD *** 06/23/99 ***********************************************
      *===============================================================
      * DLTPRE - SUBROUTINE FOR PRIOR TO DELETION PROCESSING
      *===============================================================
      *
     C     DLTPRE        BEGSR
     C*** BEG ADD *** 04/12/00 *********************************
     C*
     C*  PROCESS DELETE ON THE MANAGED CARE SIDE ALSO IF INSTALLED
     C*
     C     XXMANC        IFEQ      *ON
     C                   MOVE      EMCO#         @@CO#
     C                   MOVE      EMFND         @@FND
     C                   MOVE      EMEMP#        @@EMP#
     C                   MOVE      EMDIV         @@DIV
     C*
     C                   CALL      'FA701R'
     C                   PARM      *BLANKS       @@RTCD
     C                   PARM                    @@CO#
     C                   PARM      *ZEROS        @@SUB#            5
     C                   PARM                    @@FND
     C                   PARM      *BLANKS       @@OUTS            1
     C*
     C     @@RTCD        IFNE      'F'
     C     @@OUTS        ANDEQ     'N'
     C*
     C                   CALL      'FA029R'
     C                   PARM      *BLANKS       @@RTCD            1
     C                   PARM                    @@CO#             3
     C                   PARM      *ZEROS        @@SUB#            5
     C                   PARM                    @@FND             3
     C                   PARM                    @@EMP#            9
     C                   PARM                    @@DIV             5
     C                   PARM      *ZEROS        @@RRN#            9 0
     C                   ENDIF                                                  LINKED
     C                   ENDIF                                                  XXMANC
     C*
     C*** END ADD *** 04/12/00 *********************************
     C***  Begin Del  ***  02/06/07  **************************************
      **********
      ***DELETED THE AGENT QUOTE RECORD
      **********
     C********** QTKY      CHAINPQT01                60
      **********
     C********** *IN60     DOWEQ*OFF
      **********
      *****BEGIN CHG  ***  11/07/97  *********************************
      **********           MOVE 'X'       QTTYPE
      **********           ADD  1         DSTIME
      **********           MOVE Q1USER    QTUSER
      **********           MOVE Q1PGM     QTPGM
      **********           Z-ADDDSDATE    SRDTI
      **********           CALL 'SRCVTDAT'DTCYMD
      **********           MOVELSRDTO     QTDT
      **********           Z-ADDDSTIME    QTTIME
      **********
      **********           UPDATPQT01
     C********** DBRRNQ    DELETPQT02                60
      *****END** CHG  ***  11/07/97  *********************************
      **********
     C********** QTKY      READEPQT01                    60
      **********
     C**********           ENDDO                           *IN60=*OFF
      **********
      ***DELETE*RECORDS FROM THE EMPLOYER PAYROLL FILE
      **********
     C********** *IN60     DOUEQ*ON
     C********** DLTKY     DELETPEP01                60
     C**********           ENDDO                           *IN60=*ON
      **********
      ***DELETE*RECORDS FROM THE EMPLOYER DETAIL FILE
      **********
     C********** *IN60     DOUEQ*ON
     C********** DLTKY     DELETPEL01                60
     C**********           ENDDO                           *IN60=*ON
      **********
      ***DELETE*RECORDS FROM THE EMPLOYER ACCOUNTS RECEIVABLE FILE
      **********
     C********** *IN60     DOUEQ*ON
     C********** DLTKY     DELETPEA01                60
     C**********           ENDDO                           *IN60=*ON
      **********
      ***DELETE*RECORDS FROM THE EMPLOYER AGENT ASSIGNMENT FILE
      **********
     C********** *IN60     DOUEQ*ON
     C********** DLTKY     DELETPAH01                60
     C**********           ENDDO                           *IN60=*ON
     C***  End   Del  ***  02/06/07  **************************************
      ***  BEGIN ADD  ***  02/12/98  *********************************
      *
      *  CALL PROGRAM TO DELTE RELATED FILES
      *
     C                   CALL      'W0050R01'                           99
     C                   PARM      EMCO#         ##CO#
     C                   PARM      EMFND         ##FND
     C                   PARM      EMEMP#        ##EMP#
     C                   PARM      EMDIV         ##DIV
      ***  END   ADD  ***  02/12/98  *********************************
      **** BEGIN ADD **** 12/22/99 *********************************************
     C                   PARM      @@QTE         ##QTE
      **** END   ADD **** 12/22/99 *********************************************
      ***  BEGIN ADD  ***  10/30/97  *********************************
      *
      *  DELETE RECORDS FROM THE QUOTE TRANSFER FILE
      *
     C     @@QTE         IFEQ      'Q'
      *
     C                   CALL      'W0050R17'                           99
     C                   PARM      'E'           ##MODE            1
     C                   PARM      EMCO#         ##CO#
     C                   PARM      EMFND         ##FND
     C                   PARM      EMEMP#        ##EMP#
     C                   PARM      EMDIV         ##DIV
     C                   PARM      *ON           ##ERR             1
      *
     C                   ENDIF                                                  @@QTE=Q
      ***  END   ADD  ***  10/30/97  *********************************
     C***  Begin Del  ***  02/06/07  **************************************
      **********
      ***EXECUTE THIS CODE ONLY IF DARTS IS INSTALLED
      **********
     C**********           MOVEL'SY'      XXSYK
      **********
     C********** SYKY      CHAINSSY01                60
      **********
     C********** *IN60     IFEQ *OFF
     C********** SYSCIF    ANDEQ'Y'
      **********
      ***INACTIVATE DARTS REMARKS AND THE CORRESPONDING ADDT'L RMKS
      ***AND*LOAD DIARY COMPLETION DATE TO DIARY ITEMS
      **********
      *****ESTABLISH KEY
      **********
     C**********           CLEARQCKEY
      **********
     C**********           Z-ADDEMCO#     QCCO#
     C**********           Z-ADDEMFND     QCFND
     C**********           Z-ADDEMEMP#    QCEMP#
     C**********           Z-ADDEMDIV     QCDIV
      ******BEG*ADD ******* 07/18/02 **********************************
     C**********           MOVE *BLANKS   QCQTE
      **********
     C********** @@QTE     IFEQ 'Q'
     C**********           MOVE 'Q'       QCQTE
     C**********           ENDIF                           @@QTE=Q
      ******END*ADD ******* 07/18/02 **********************************
      **********
      ***CHAIN*TO DARTS REMARKS DETAIL FILE
      **********
     C********** C3KY      CHAINPCM03                60
      **********
     C********** *IN60     DOWEQ*OFF
      ******BEG*DEL ******* 07/18/02 **********************************
      **********
     C********** C3DIFL    IFEQ 'Y'
      **********
     C**********           Z-ADDDSDATE    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     C3DCDT
      **********
     C**********           ELSE
      **********
     C**********           MOVE 'I'       C3STAT
      ******END*DEL ******* 07/18/02 **********************************
      **********
      ***CHAIN*TO ADDITIONAL REMARKS FILE
      **********
     C********** A2KY      CHAINPAR02                60
      **********
     C********** *IN60     DOWEQ*OFF
      **********
      ******BEG*DEL ******* 07/18/02 **********************************
      **********
     C**********           MOVE 'I'       A2STAT
     C**********           MOVELQ1PGM     A2PGM
     C**********           Z-ADDDSTIME    A2TIME
     C**********           Z-ADDDSDATE    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     A2DT
      **********
     C**********           UPDATPAR02
      ******END*DEL ******* 07/18/02 **********************************
      ******BEG*ADD ******* 07/18/02 **********************************
     C**********           DELETPAR02
      ******END*ADD ******* 07/18/02 **********************************
      **********
     C********** A2KY      READEPAR02                    60
     C**********           ENDDO                           *IN60=*OFF/A2KY
      **********
      ******BEG*DEL ******* 07/18/02 **********************************
     C**********           ENDIF                           C3DIFL=Y
      **********
     C**********           MOVELQ1PGM     C3PGM
     C**********           Z-ADDDSTIME    C3TIME
     C**********           Z-ADDDSDATE    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     C3DT
      **********
     C**********           UPDATPCM03
      ******END*DEL ******* 07/18/02 **********************************
      ******BEG*ADD ******* 07/18/02 **********************************
     C**********           DELETPCM03
      ******END*ADD ******* 07/18/02 **********************************
      **********
     C********** C3KY      READEPCM03                    60
     C**********           ENDDO                           *IN60=*OFF/C3KY
      ******BEG*ADD ******* 07/18/02 **********************************
      **********
      ***Delete*DARTS Custom Group ID records
      **********
     C********** *IN60     DOUEQ*ON
     C********** C3KY      DELETPHD01                60
     C**********           ENDDO                           *IN60=*ON
      **********
     C********** *IN60     DOUEQ*ON
     C********** C3KY      DELETPH101                60
     C**********           ENDDO                           *IN60=*ON
      **********
      ******END*ADD ******* 07/18/02 **********************************
      **********
     C**********           ENDIF                           *IN60=*OFF/SYSCIF=Y
     C***  End   Del  ***  02/06/07  **************************************
      *** BEG ADD ***** 02/03/98 ***********************************************
      *
      *  CHECK FOR REPORTABLE CHANGE AS MID TERM ENDORSEMENT
      *
     C                   EXSR      CKCHGS
      *** END ADD ***** 02/03/98 ***********************************************
      *
     C                   ENDSR                                                  DLTPRE
      *
      *===============================================================
      * DSPLYR - SUBROUTINE FOR DISPLAYING INFORMATION
      *===============================================================
      *
     C     DSPLYR        BEGSR
     C                   ENDSR                                                  DSPLYR
      *
      *===============================================================
      * ERRCK1 - SUBROUTINE FOR ERROR CHECKING SCR01
      *===============================================================
      *
     C     ERRCK1        BEGSR
      *
     C   09              MOVE      *ON           *IN90
      *
      *  DOING BUSINESS AS CANNOT BE BLANK
      *
     C*** BEGIN CHG ***  05/14/04  ************************************
     C**N90****  SCENAM    IFEQ *BLANKS
     C  N90SCNAM1        IFEQ      *BLANKS
     C*** END CHG *****  05/14/04  ************************************
      *
     C                   SETON                                        20  90
      *
     C                   ELSE
      *
      *  COMPRESS EMP#/DIV/NAME
      *
     C                   CALL      'WA005R'
     C***  Begin Chg  ***  05/04/10  **************************************
     C***                  PARM EMEMP#    XXEMP#
     C                   PARM      SCEMP#        XXEMP#
     C***  End   Chg  ***  05/04/10  **************************************
     C                   PARM      EMDIV         XXDIV
     C***  Begin Chg  ***  09/22/04  **************************************
     C**********           PARM SCENAM    XXNAME
     C                   PARM      SCNAM1        XXNAME
     C***  End   Chg  ***  09/22/04  **************************************
     C                   PARM                    XXEDN            80
      *
     C                   MOVEL(P)  XXEDN         SCEDN
      *
     C                   ENDIF                                                  SCENAM=*BLANKS
      ***** BEGIN ADD ***** 04/25/06 **********************************
      *
      * PERSONAL NAME FIELDS MUST BE VALID IF ENTERED.
      *
     C     *IN17         IFEQ      *ON
      *
     C  N90SCNAMP        IFNE      *BLANK
     C     SCNAMP        ANDNE     'Y'
     C                   SETON                                        18  90
     C                   ENDIF                                                  SCNAMP IFNE
      *
     C  N90SCFDNP        IFNE      *BLANK
     C     SCFDNP        ANDNE     'Y'
     C                   SETON                                        19  90
     C                   ENDIF                                                  SCNAMP IFNE
      *
     C                   ENDIF                                                  *IN17 IFEQ
      ***** END ADD ******* 04/25/06 **********************************
      *
      *  IF THE MAILING ADDRESS IS ENTERED:
      *
     C  N90SCMAD1        IFNE      *BLANKS
      *
      *    THE MAILING CITY MUST BE ENTERED
      *
     C  N90SCMCTY        IFEQ      *BLANKS
     C                   SETON                                        21  90
     C                   ENDIF                                                  SCMAD1<>*BLANKS
      *
      *    THE MAILING STATE MUST BE VALID
      *
     C                   MOVEL(P)  SCMST         XXKEY
     C     STKY          CHAIN     PST01                              60
     C  N90
     CAN 60              SETON                                        22  90
      *
      *    THE MAILING ZIP CODE MUST BE ENTERED
      *
     C  N90SCMZP1        IFLE      *ZEROS
     C                   SETON                                        23  90
     C                   ENDIF                                                  SCMZP1<=*ZERO
      *
      ***** BEG ADD ******* 10/30/08 **********************************
     C     CLTID         IFEQ      '136'
     C  N90SCMZP2        IFLE      *ZEROS
     C                   SETON                                        53  90
     C                   ENDIF                                                  SCMZP2<=*ZERO
     C                   ENDIF                                                  CLT = 136
      *
      ***** END ADD ******* 10/30/08 **********************************
     C                   ENDIF                                                  SCMAD1<>*BLANKS
      *
      *  THE FLAG MUST BE M OR P
      *
     C  N90SCMPFL        IFNE      'M'
     C     SCMPFL        ANDNE     'P'
     C                   SETON                                        29  90
     C                   ENDIF                                                  SCMPFL<>M/P
      *
      *  IF THE PHYSICAL ADDRESS IF ENTERED:
      *
     C  N90SCPAD1        IFNE      *BLANK
      *
      *    THE PHYSICAL CITY MUST BE ENTERED
      *
     C  N90SCPCTY        IFEQ      *BLANKS
     C                   SETON                                        24  90
     C                   ENDIF                                                  SCPCTY=*BLANKS
      *
      *    THE PHYSICAL STATE MUST BE VALID
      *
     C                   MOVEL(P)  SCPST         XXKEY
     C     STKY          CHAIN     PST01                              60
     C  N90
     CAN 60              SETON                                        25  90
      *
      *    THE PHYSICAL ZIP CODE MUST BE ENTERED
      *
     C  N90SCPZP1        IFLE      *ZERO
     C                   SETON                                        26  90
     C                   ENDIF                                                  SCPZP1<=*ZERO
      *
      ***** BEG ADD ******* 10/30/08 **********************************
     C     CLTID         IFEQ      '136'
     C  N90SCPZP2        IFLE      *ZERO
     C                   SETON                                        55  90
     C                   ENDIF                                                  SCPZP2<=*ZERO
     C                   ENDIF                                                  CLT = 136
      *
      ***** END ADD ******* 10/30/08 **********************************
     C                   ENDIF                                                  SCPAD1<>*BLANKS
      *
      *  THE MAILING ADDRESS LINE CANNOT BE BLANK IF CODE SELECTED
      *
     C  N90SCMPFL        IFEQ      'M'
     C     SCMAD1        ANDEQ     *BLANKS
     C                   SETON                                        30  90
     C                   ENDIF                                                  SCMPFL=M
      *
      *  THE PHYSICAL ADDRESS LINE CANNOT BE BLANK IF CODE SELECTED
      *
     C  N90SCMPFL        IFEQ      'P'
     C     SCPAD1        ANDEQ     *BLANKS
     C                   SETON                                        31  90
     C                   ENDIF                                                  SCMPFL=P
      *
     C*** BEG ADD ****** 03/23/10 ************************************
     C*
     C*  In ADD mode, check for default values for chapter, county,
     C*  region and loss rep in the city/demographic cross reference
     C*  file.  Use defaults if screen fields are blank.
     C*
     C     *IN90         IFEQ      *OFF
     C     CLTID         ANDEQ     '102'
     C     *IN40         ANDEQ     *ON
     C     SCPST         IFNE      *BLANKS
     C                   MOVE      SCPST         XXST
     C     LO:UP         XLATE     SCPCTY        XXCTY
     C                   ELSE
     C                   MOVE      SCMST         XXST
     C     LO:UP         XLATE     SCMCTY        XXCTY
     C                   ENDIF                                                  SCPST<>*BLAN
     C     CYKY          CHAIN     PCY01                              60
     C     *IN60         IFEQ      *OFF
     C     SCLREP        IFEQ      *BLANKS
     C                   MOVEL     CYLREP        SCLREP
     C                   WRITE     SCR02
     C                   ENDIF                                                  SCLREP=BLANK
     C     SCCNTY        IFEQ      *BLANKS
     C                   MOVE      CYCNTY        SCCNTY
     C                   WRITE     SCR02
     C                   ENDIF                                                  SCCNTY=BLANK
     C     SCRGN#        IFEQ      *ZEROS
     C                   MOVEL     CYRGN         SCRGN#
     C                   WRITE     SCR02
     C                   ENDIF                                                  SCRGN#=BLANK
     C     SCCHAP        IFEQ      *ZEROS
     C                   MOVEL     CYCHAP        SCCHAP
     C                   WRITE     SCR02
     C                   ENDIF                                                  SCCHAP=BLANK
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  *IN90=*OFF
     C*** END ADD ****** 03/23/10 ************************************
     *** BEG ADD *** 02/28/03 *****************************************
      *  If in ADD mode, preload the County Code in next screen SCCCCD
     C                   MOVE      *BLANKS       ##CNTY            2
     C                   MOVE      *BLANKS       ##CZST
     C                   MOVE      *ZEROS        ##ZIP5
     C     @@MODE        IFEQ      'A'
     C     *IN90         ANDEQ     *OFF
      ***** BEG ADD ***** 08/27/04 **********************************
      * Also in Chage mode, preload if blank
     C     @@MODE        OREQ      'C'
      ***** BEG ADD ***** 03/16/05 **********************************
     C*********  SCCNTY    ANDEQ*BLANKS
      ***** END ADD ***** 03/16/05 **********************************
     C     *IN90         ANDEQ     *OFF
      ***** END ADD ***** 08/27/04 **********************************
      * If Physical ST & Zip present, use it, else use Mailing ST, ZIP
     C     SCPST         IFGT      *BLANKS
     C     SCPZP1        ANDGT     *ZEROS
     C                   MOVE      SCPST         ##CZST
     C                   MOVE      SCPZP1        ##ZIP5
     C                   ELSE
     C                   MOVE      SCMST         ##CZST
     C                   MOVE      SCMZP1        ##ZIP5
     C                   ENDIF                                                  END SCPST=*
     C     CZKY          CHAIN     PCZ01                              60
     C     *IN60         IFEQ      *OFF
     C     CZSTAT        ANDEQ     'A'
     C*** BEG CHG ****** 03/23/10 ************************************
     C***********          MOVE CZCNTY    SCCNTY
     C***********          MOVE CZCNTY    SCCNTD
     C***********          MOVE CZCNTY    ##CNTY
     C                   SELECT
     C     CLTID         WHENEQ    '102'
     C     SCCNTY        IFEQ      *BLANK
     C                   MOVE      CZCNTY        SCCNTY
     C                   MOVE      CZCNTY        SCCNTD
     C                   MOVE      CZCNTY        ##CNTY
     C                   ENDIF
     C                   OTHER
     C                   MOVE      CZCNTY        SCCNTY
     C                   MOVE      CZCNTY        SCCNTD
     C                   MOVE      CZCNTY        ##CNTY
     C                   ENDSL
     C*** END CHG ****** 03/23/10 ************************************
     C                   ENDIF                                                  END-CZCNTY
     C                   ENDIF                                                  @@MODE=A/*IN09=*ON
     *** END ADD *** 02/28/03 *****************************************
      *
      *  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
      *
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
      *
     C***  BEGIN CHG  ***  01/04/00  *********************************
     C**N90***** EMDT      IFGT SAVEDT
     C  N90
     CANN41EMDT          IFGT      SAVEDT
     C***  END   CHG  ***  01/04/00  *********************************
     C     EMDT          OREQ      SAVEDT
     C     EMTIME        ANDGT     SAVETM
     C                   SETON                                        93  90
     C                   ENDIF                                                  EMDT>SAVEDT
      ***  BEG ADD ******  01/04/10  *********************************
     C                   EXSR      FLXHDG
      ***  END ADD ******  01/04/10  *********************************
      *
      *  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
      *
     C     @@MODE        IFEQ      'A'
     C     *IN09         ANDEQ     *ON
     C                   MOVE      *ON           OUTFLG
     C                   MOVE      *OFF          *IN90
     C                   ENDIF                                                  @@MODE=A/*IN09=*ON
      *
     C                   ENDSR                                                  ERRCK1
      *
      *===============================================================
      * ERRCK2 - SUBROUTINE FOR ERROR CHECKING SCR02
      *===============================================================
      *
     C     ERRCK2        BEGSR
      *
      *  EDIT EFFECTIVE DATE 1
      *
     C                   Z-ADD     SCEFD1        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFD1
      *
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        213190
     C                   ENDIF                                                  SRERR=*ON
      *****  BEG  ADD  03/06/06 ***************************************
      *
     C  N90@@QTE         IFEQ      'Q'                                          In Quote
     C     *IN41         ANDEQ     *ON                                          New Division
     C     SVRDFL        OREQ      'Y'
      *
     C                   Z-ADD     SCEFD1        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
      *
      ***Find*the employer which is the State Reporting division
      *
      ***** BEG   CHG **** 02/28/11 *******************************
     C*****      EMKY1     CHAINPEM01                60
     C     EMKY1         CHAIN(N)  PEM01                              60
      ***** END   CHG **** 02/28/11 ********************************
     C     *IN60         DOWEQ     *OFF
      *
      *** If this is the state reporting devision then, get the effective
      *** date from WMELP and check, effective date for this division
      *** must be after or equal to the effective date for lead division
      *
     C     EMSTRP        IFEQ      'Y'
      *
     C     ELKY3         CHAIN(N)  PEL01                              60
     C     *IN60         IFEQ      *OFF
     C     XXEFDT        ANDLT     ELPFDT
     C                   SETON                                        84  90
     C                   LEAVE
     C                   ENDIF                                                  XXEFDT>ELPFDT
      *
     C                   ENDIF
      *
     C  N60EMKY1         READE     PEM01                                  60
     C                   ENDDO
      *
     C                   ENDIF                                                  *IN41
      *****  END  ADD  03/06/06 ***************************************
      *
      *  INITIAL EFFECTIVE DATE CANNOT BE GREATER THAN EARLIEST POLICY
      *  FROM DATE, IF AT LEAST ONE DETAIL RECORD EXISTS.
      *
     C     *IN40         IFEQ      *OFF
      *
     C     ELKY          CHAIN(N)  PEL01                              60
      *
     C  N90*IN60         IFEQ      *OFF
      *
     C                   Z-ADD     SCEFD1        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
      *
     C  N90XXEFDT        IFGT      ELPFDT
     C                   SETON                                        20  90
     C                   ENDIF                                                  XXEFDT>ELPFDT
      *
     C                   ENDIF                                                  *IN60=*OFF
      *
     C                   ENDIF                                                  *IN40=*OFF
      *
      *
      *  EDIT EFFECTIVE CANCEL 1
      *
     C  N90SCCAD1        IFNE      *ZERO
      *
     C                   Z-ADD     SCCAD1        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCCAD1
      *
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        263190
     C                   ENDIF                                                  SRERR=*ON
      *
     C                   Z-ADD     SCEFD1        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT1
      *
     C                   Z-ADD     SCCAD1        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT2
      *
     C  N90XXDAT2        IFLT      XXDAT1
     C                   SETON                                        32  90
     C                   SETON                                        2126
     C                   ENDIF                                                  XXDAT2<XXDAT1
      *
     C                   ENDIF                                                  SCCAD1<>*ZEROS
      *
      *  EDIT EFFECTIVE/CANCEL DATES 2-5
      *
     C     2             DO        5             X                 3 0
      *
      *    EFFECTIVE DATE
      *
     C  N90SEF(X)        IFNE      *ZERO
      *
     C     X             ADD       20            X1                3 0
     C     X             ADD       25            X2                3 0
      *
     C                   Z-ADD     SEF(X)        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SEF(X)
      *
     C  N90SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN(X1)
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SRERR=*ON
      *
      *    CANCEL DATE
      *
     C  N90SCA(X)        IFNE      *ZERO
      *
     C                   Z-ADD     SCA(X)        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCA(X)
      *
     C  N90SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN(X2)
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SRERR=*ON
      *
     C                   Z-ADD     SEF(X)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT1
      *
     C                   Z-ADD     SCA(X)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT2
      *
     C  N90XXDAT2        IFLT      XXDAT1
     C                   MOVE      *ON           *IN(X1)
     C                   MOVE      *ON           *IN(X2)
     C                   SETON                                        30  90
     C                   ENDIF                                                  XXDAT2<XXDAT1
      *
     C                   ENDIF                                                  SCA,X<>*ZEROS
      *
     C                   ELSE
      *
     C  N90SCA(X)        IFNE      *ZERO
     C                   MOVE      *ON           *IN(X1)
     C                   SETON                                        31  90
     C                   ENDIF                                                  SCA,X<>*ZERO
      *
     C                   ENDIF                                                  SEF,X<>*ZEROS
      *
     C  N90              ENDDO                                                  2->X->5
      *
      *  IF A CANCEL DATE WAS SPECIFIED AND THERE ARE ANY CLAIMS THAT
      *  EXIST WITH AN ACCIDENT DATE GREATER THAN OR EQUAL TO THE
      *  CANCEL DATE, FLAG AN ERROR MESSAGE DUE TO BACKPOSTING CX DATE.
      *  ALLOW USER TO OVERRIDE ERROR MESSAGE.
      *
      *  GET LAST EFFECTIVE DATE
      *
     C                   Z-ADD     1             X
      *
     C     *ZERO         LOOKUP    SEF(X)                                 60
      *
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     5             X
     C                   ELSE
     C                   SUB       1             X
     C                   ENDIF                                                  *IN60=*OFF
      *
     C  N90SCA(X)        IFNE      *ZEROS
      *
     C                   Z-ADD     SCA(X)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXACDT
      *
     C     CMKY          SETLL     PCM01
     C     CMKY1         READE     PCM01                                  60
      *
     C  N90*IN60         DOWEQ     *OFF
     C     OVRFLG        ANDNE     'Y'
      *
     C                   XFOOT     AR            XXPMTS           13 2
      *
     C     CMSTAT        IFNE      'C'
     C     CMSTAT        OREQ      'C'
     C     XXPMTS        ANDNE     *ZEROS
      *
      *  ALLOW OVERRIDE ON ERROR MSG ONLY IF USER HAS PROPER AUTHORITY
      *
     C***  BEGIN CHG  ***  08/20/99  *********************************
     C********** UPLVL     IFGE WCSL03
     C                   MOVE      EXOVRL        ##CLVL
     C                   MOVE      EXOVRT        ##CTYP
     C                   MOVE      ##LVL         ##ULVL
     C                   MOVE      ##TYPE        ##UTYP
     C*
     C                   CALL      'SRCCSC'      PLCCSC
     C*
     C     ##RES         IFEQ      *OFF
     C***  END   CHG  ***  08/20/99  *********************************
      *
     C                   MOVE      'N'           SCEOVR
      *
     C                   EXFMT     WDWMSG
      *
     C     SCEOVR        IFEQ      'Y'
     C                   MOVE      'Y'           OVRFLG            1
     C                   SETON                                        7039
     C                   ELSE
     C                   MOVE      'N'           OVRFLG
     C                   SETOFF                                       7039
     C     25            ADD       X             X1
     C                   MOVE      *ON           *IN(X1)
     C                   SETON                                        33  90
     C                   ENDIF                                                  SCEOVR=Y
      *
     C                   ELSE
      *
     C                   MOVE      'N'           OVRFLG
     C     25            ADD       X             X1
     C                   MOVE      *ON           *IN(X1)
     C                   SETON                                        33  90
     C*
     C                   ENDIF                                                  ##RES=*OFF
      *
     C                   ENDIF                                                  CMSTAT<>C
      *
     C     CMKY1         READE     PCM01                                  60
      *
     C  N90              ENDDO                                                  *IN60=*OFF
      *
     C                   ENDIF                                                  SCA,X<>*ZERO
      *
      *  SHRT RATE/PRORATA FLAG MUST BE S OR P
      *
     C***  BEGIN ADD  ***  03/23/99  *********************************
     C     WCFL08        IFNE      'C'                                          NOT CLAIMS ONLY CLT
     C***  END   ADD  ***  03/23/99  *********************************
     C*
     C*** BEGIN DEL ***  09/16/99  ************************************
     C**N90**    SCSRPR    IFNE 'S'
     C*******    SCSRPR    ANDNE'P'
     C*******              SETON                     34  90
     C*******              ENDIF                           SCSRPR<>S/P
     C*** END DEL *****  09/16/99  ************************************
     C*
     C***  BEGIN ADD  ***  03/23/99  *********************************
     C                   ENDIF                                                  WCFL08<>C
     C***  END   ADD  ***  03/23/99  *********************************
      *
      *  VALIDATE POLICY SERVICES STAFF CODE, IF ENTERED
      *
     C  N90SCPSRV        IFNE      *BLANKS
     C     SCPSRV        CHAIN     PUP02                              60
     C   60              SETON                                        377090
     C                   ENDIF                                                  SCPSRV<>*BLANKS
      *
      *  VALIDATE UNDERWRITER STAFF CODE, IF ENTERED
      *
     C  N90SCUWTR        IFNE      *BLANKS
     C     SCUWTR        CHAIN     PUP02                              60
     C   60              SETON                                        377190
     C                   ENDIF                                                  SCUWTR<>*BLANKS
      *
      *  VALIDATE PREMIUM ACCT STAFF CODE, IF ENTERED
      *
     C  N90SCPACC        IFNE      *BLANKS
     C     SCPACC        CHAIN     PUP02                              60
     C   60              SETON                                        377290
     C                   ENDIF                                                  SCPACC<>*BLANKS
      *
      *  VALIDATE AUDIT REVIEWER STAFF CODE, IF ENTERED
      *
     C  N90SCARVW        IFNE      *BLANKS
     C     SCARVW        CHAIN     PUP02                              60
     C   60              SETON                                        377390
     C                   ENDIF                                                  SCARVW<>*BLANKS
      *
      *  VALIDATE AUDIT MANAGER STAFF CODE, IF ENTERED
      *
     C  N90SCAMGR        IFNE      *BLANKS
     C     SCAMGR        CHAIN     PUP02                              60
     C   60              SETON                                        377490
     C                   ENDIF                                                  SCAMGR<>*BLANKS
      *
      *  VALIDATE LOSS CONTROL REP STAFF CODE, IF ENTERED
      *
     C  N90SCLREP        IFNE      *BLANKS
     C     SCLREP        CHAIN     PUP02                              60
     C   60              SETON                                        377590
     C                   ENDIF                                                  SCLREP<>*BLANKS
      *
     C****** BEG DEL ***** 07/07/05 **************************************
      ***************************  VALIDATE ACTIVITY STATUS, IF ENTERED
     C********************
     C**N90******SCASTS*** IFNE *BLANKS
     C********************
     C******************** MOVELSCASTS    XXKEY     P
     C********************
     C***********TBKY***** CHAINPAV01                60
     C********************
     C************IN60**** IFEQ *ON
     C******************** SETON                     357690
     C******************** ELSE
     C******************** MOVELAVDESC    SCACTD    P
     C******************** ENDIF                           *IN60=*ON
     C********************
     C******************** ENDIF                           SCASTS<>*BLANKS
     C********************
      ****** BEG ADD ***** 06/07/05 **************************************
     C*  If activity Status Code is Blanks, be sure that Desc is blank too.
     C**N90******SCASTS*** IFEQ *BLANKS
     C******************** MOVEL*BLANKS   SCACTD    P
     C******************** ENDIF                           SCASTS = *BLANKS
     C********************
      ****** END ADD ***** 06/07/05 **************************************
     C****** END DEL ***** 07/07/05 **************************************
      *
      *  VALIDATE WATCH LIST (Y/N)
      *
     C     SCWALS        IFEQ      *BLANKS
     C                   MOVE      'N'           SCWALS
     C                   ENDIF                                                  SCWALS=*BLANKS
      *
     C  N90SCWALS        IFNE      'Y'
     C     SCWALS        ANDNE     'N'
      ***** BEG CHG ******* 06/14/02 **********************************
     C*********            SETON                     327790
     C                   SETON                                        367790
      ***** END CHG ******* 06/14/02 **********************************
     C                   ENDIF                                                  SCWALS<>Y/N
      *
      *  COUNTY CODE MUST BE VALID AND ACTIVE
      *
     ***** BEG ADD ***** 03/16/05 ***************************************
      *  If in ADD mode, preload the County Code in next screen SCCCCD
     C                   MOVE      *BLANKS       ##CNTY            2
     C                   MOVE      *BLANKS       ##CZST
     C                   MOVE      *ZEROS        ##ZIP5
     C     @@MODE        IFEQ      'A'
     C     *IN90         ANDEQ     *OFF
      * Also in Chage mode, preload if blank
     C     @@MODE        ORNE      'D'
     C     *IN90         ANDEQ     *OFF
      *
      * If Physical ST & Zip present, use it, else use Mailing ST, ZIP
     C     SCPST         IFGT      *BLANKS
     C     SCPZP1        ANDGT     *ZEROS
     C                   MOVE      SCPST         ##CZST
     C                   MOVE      SCPZP1        ##ZIP5
     C                   ELSE
     C                   MOVE      SCMST         ##CZST
     C                   MOVE      SCMZP1        ##ZIP5
     C                   ENDIF                                                  END SCPST=*
     C     CZKY          CHAIN     PCZ01                              60
      * If a zip code is in County Code file and active use it, Else is
      * an error if inactive.
     C     *IN60         IFEQ      *OFF
     C     CZSTAT        IFEQ      'I'
     C                   SETON                                        477890
     C                   ENDIF                                                  END-CZSTAT=I
     C     CZSTAT        IFEQ      'A'
     C                   MOVE      CZCNTY        SCCNTY
     C                   ENDIF                                                  END-CZSTAT=I
     C                   ENDIF                                                  END-*IN60=*ON
     C                   ENDIF                                                  @@MODE=A/*IN09=*ON
      ***** END ADD ***** 03/16/05 ***************************************
      *
      ***** BEG CHG ***** 03/16/05 ***************************************
     C********** SCPST     IFNE *BLANKS
     C*** BEG ADD ****** 08/21/08 ************************************
     C     *IN40         IFNE      *ON
     C*** END ADD ****** 08/21/08 ************************************
     C  N90SCPST         IFNE      *BLANKS
      ***** END CHG ***** 03/16/05 ***************************************
     C     SCPST         CAT       SCCNTY:0      ZZKEY             4
     C                   ELSE
     C     SCMST         CAT       SCCNTY:0      ZZKEY
     C                   ENDIF                                                  SCPST<>*BLANKS
      *
     C                   MOVEL(P)  ZZKEY         XXKEY
      *
     C  N90TBKY          CHAIN     PCN01                              60
      *
     C***** BEG DEL ***** 06/02/05 ***************************************
     C**N90*60*******      SETON                     357890
     C***** END DEL ***** 06/02/05 ***************************************
     C***** BEGIN ADD *** 06/08/07 ***************************************
     C  N90
     CAN 60              SETON                                        357890
     C***** END ADD ***** 06/08/07 ***************************************
      *
     C  N90
     CANN60CNSTAT        IFEQ      'I'
     C                   SETON                                        477890
     C                   ENDIF                                                  CNSTAT=I
      *
     C  N90
     CANN60              MOVEL     CNDESC        SCCNTD
     C*** BEG ADD ****** 08/21/08 ************************************
     C                   ENDIF                                                  *IN40<>*ON
     C*** END ADD ****** 08/21/08 ************************************
      *
      *  EDIT THE VIP CODE
      *
     C  N90SCVIP         IFNE      'Y'
     C     SCVIP         ANDNE     'N'
     C                   SETON                                        367990
     C                   ENDIF                                                  SCVIP<>Y/N
      *
      *  REGION MUST BE VALID AND ACTIVE
      *
     C*** BEG ADD ****** 08/21/08 ************************************
     C*** BEG DEL ****** 03/23/10 ************************************
     C**N90*******IN40     IFNE *ON
     C****END*ADD ****** 08/21/08 ************************************
     C**N90******REKY      CHAINWMREL                60
      ***********
     C**N90*60***          SETON                     358090
      ***********
     C**N90N60***RESTAT    IFEQ 'I'
     C***********          SETON                     388090
     C***********          ENDIF                           RESTAT=I
      ***********
     C**N90N60***          MOVELREDESC    SCRGND
     C****BEG*ADD ****** 08/21/08 ************************************
     C***********          ENDIF                           *IN40=*ON
     C*** END DEL ****** 03/23/10 ************************************
     C*** END ADD ****** 08/21/08 ************************************
     C*** BEG ADD ****** 03/23/10 ************************************
     C     *IN90         IFEQ      *OFF
     C     REKY          CHAIN     WMREL                              60
     C     *IN60         IFEQ      *OFF
     C     RESTAT        IFEQ      'I'
     C                   SETON                                        388090
     C                   ENDIF
     C                   ELSE
     C                   SETON                                        358090
     C                   ENDIF
     C                   ENDIF
     C*** END ADD ****** 03/23/10 ************************************
      *
      *  VALIDATE SPECIAL ACCEPTANCE (Y/N)
      *
     C     SCSPAC        IFEQ      *BLANKS
     C                   MOVE      'N'           SCSPAC
     C                   ENDIF                                                  SCSPAC=*BLANKS
      *
     C  N90SCSPAC        IFNE      'Y'
     C     SCSPAC        ANDNE     'N'
     C                   SETON                                        368190
     C                   ENDIF                                                  SCSPAC<>Y/N
      *
     C**** BEG ADD 02/23/10 **************************************
     C     CLTID         IFEQ      '133'
     C     SCCHAP        IFEQ      *ZEROS
     C                   SETON                                        67
     C                   SETON                                        90
     C                   ENDIF
     C                   ENDIF
     C**** END ADD 02/23/10 **************************************
      *  CHAPTER MUST BE VALID AND ACTIVE
      *
     C  N90SCCHAP        IFNE      *ZEROS
      *
     C     CPKY          CHAIN     WMCPL                              60
      *
     C  N90
     CAN 60              SETON                                        358290
      *
     C  N90
     CANN60CPSTAT        IFEQ      'I'
      **** BEGIN CHG **** 04/14/00 *********************************************
     C******************** SETON                     388290
     C                   SETON                                        358290
      **** END   CHG **** 04/14/00 *********************************************
     C                   ENDIF                                                  CPSTAT=I
      *
     C  N90
     CANN60              MOVEL     CPDESC        SCCHPD
      *
     C                   ENDIF                                                  SCCHAP<>*ZEROS
     C*** BEG ADD ****** 08/21/08 ************************************
     C*** BEG DEL ****** 03/23/10 ************************************
     C***********
     C***In*ADD*mode, check for default values for chapter, county,
     C***region*and loss rep in the city/demographic cross reference
     C***file.**Use defaults if screen fields are blank.
     C***********
     C************IN90     IFEQ *OFF
     C***********
     C************IN40     IFEQ *ON
     C***********
     C***********SCPST     IFNE *BLANKS
     C***********
     C***********          MOVE SCPST     XXST
     C***********LO:UP     XLATESCPCTY    XXCTY
     C***********
     C***********          ELSE
     C***********
     C***********          MOVE SCMST     XXST
     C***********LO:UP     XLATESCMCTY    XXCTY
     C***********
     C***********          ENDIF                           SCPST<>*BLAN
     C***********
     C***********CYKY      CHAINPCY01                60
     C************IN60     IFEQ *OFF
     C***********
     C***********SCLREP    IFEQ *BLANKS
     C***********          MOVELCYLREP    SCLREP
     C***********          ENDIF                           SCLREP=BLANK
     C***********
     C***********SCCNTY    IFEQ *BLANKS
     C***********          MOVE CYCNTY    SCCNTY
     C***********          ENDIF                           SCCNTY=BLANK
     C***********
     C***********SCRGN#    IFEQ *ZEROS
     C***********          MOVELCYRGN     SCRGN#
     C***********          ENDIF                           SCRGN#=BLANK
     C***********
     C***********SCCHAP    IFEQ *ZEROS
     C***********          MOVELCYCHAP    SCCHAP
     C***********          ENDIF                           SCCHAP=BLANK
     C***********
     C***********          ENDIF                           *IN60=*OFF
     C***********
     C***********          ENDIF                           *IN40=*ON
     C***********
     C***********          ENDIF                           *IN90=*OFF
     C*** END DEL ****** 03/23/10 ************************************
     C*** END ADD ****** 08/21/08 ************************************
     C*** BEGIN ADD *** 10/05/09 **************************************
     C*
     C* FLEX VALUE MUST BE VALID IF ENTERED
     C*
     C     *IN90         IFEQ      *OFF
     C     SCFLXV        ANDNE     *BLANKS
     C                   MOVEL(P)  EXFLXT        XZTBID
     C                   MOVEL(P)  SCFLXV        XXKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        8890
     C                   ELSE
     C                   MOVEL(P)  GTDESC        SCFLXD
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 10/05/09 **************************************
      *
      *  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
      *
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
      *
     C***  BEGIN CHG  ***  01/04/00  *********************************
     C**N90***** EMDT      IFGT SAVEDT
     C  N90
     CANN41EMDT          IFGT      SAVEDT
     C***  END   CHG  ***  01/04/00  *********************************
     C     EMDT          OREQ      SAVEDT
     C     EMTIME        ANDGT     SAVETM
     C                   SETON                                        93  90
     C                   ENDIF                                                  EMDT>SAVEDT
      *
     C****** BEG ADD ***** 07/06/07 **************************************
     C     *IN09         IFEQ      *OFF                                         NOT DISPLAY MODE
     C     *IN62         ANDEQ     *OFF                                         NOT QUOTE MODE
     C     SCPST         IFGT      *BLANKS
     C                   MOVE      SCPST         ##RRST
     C                   ELSE
     C                   MOVE      SCMST         ##RRST
     C                   ENDIF
     C                   ENDIF
     C*
     C     RRKY          CHAIN     PRR01                              60
     C     *IN60         IFEQ      *OFF
     C     RRNAIC        IFEQ      'Y'
     C                   MOVE      *ON           *IN98
     C                   ELSE
     C                   MOVE      *OFF          *IN98
     C                   ENDIF                                                  EXSICR=Y
     C     RRURFL        IFEQ      'Y'
     C                   MOVE      *ON           *IN97
     C                   ELSE
     C                   MOVE      *OFF          *IN97
     C                   ENDIF                                                  EXSICR=Y
     C                   ENDIF                                                  EXSICR=Y
     C****** END ADD ***** 07/06/07 **************************************
      *
      ***** BEG ADD ***** 03/28/08 ************************************
     C     CLTID         IFEQ      '114'
     C  N90SCDCRX        IFNE      'Y'
     C     SCDCRX        ANDNE     ' '
     C                   MOVE      *ON           *IN87
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C                   ENDIF
      ***** END ADD ***** 03/28/08 ************************************
      *
      *  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
      *
     C                   ENDSR                                                  ERRCK2
      *
      *===============================================================
      * ERRCK3 - SUBROUTINE FOR ERROR CHECKING SCR03
      *===============================================================
      *
     C     ERRCK3        BEGSR
      *
      *  EDIT THE STATE REPORTING FLAG (IF NOT CLAIMS ONLY)
      *    -IN ADD EMPLOYER MODE (*IN40=*ON) THE FLAG MUST BE 'Y'
      *    -IN ADD DIVISION MODE (*IN41=*ON) THE FLAG SHOULD BE 'N' IF
      *     ANOTHER DIVISION IS ALREADY FLAGGED 'Y'
      *    -WE HAVE ALLOWED THE USER TO CHANGE A 'Y' TO A 'N' IN UPDATE
      *     TO ELIMINATE THE HAVING TO GO IN AND OUT OF DIVISIONS (IE.
      *        THE USER WISHES TO ENTER A DIVISION 2 W/'Y'.  DIVISION 1
      *        IS ALREADY 'Y'.
      *       -W/O AVAILABILITY
      *          MUST ENTER DIVISION 2 W/'N', CHANGE DIVISION 1 TO 'N',
      *          AND THEN CHANGE DIVISION 2 TO 'Y'.
      *       -W/AVAILABILITY
      *          CHANGE DIVISION 1 TO 'N' AND ENTER DIVSION 2 W/'Y'.
      *
     C***  BEGIN ADD  ***  03/31/03  *********************************
     C*
     C*  CHECK TO SEE IF FEIN HAS BEEN USED IN THE CURRENT ENVIRONMENT
     C*
      *** BEG ADD ***** 04/18/03 *************************************
     C     SCFEIN        IFNE      S2FEIN
     C                   MOVEL     SCFEIN        S2FEIN
     C*
     C                   DO
      *
      ***** BEG ADD *****  01/05/06  *********************************
      * If the Group PEO flag is "M", then bypass this editing
     C     XXPEOF        IFEQ      'M'
     C                   LEAVE
     C                   ENDIF                                                  @@MODE
      ***** END ADD *****  01/05/06  *********************************
      *
     C     @@MODE        IFEQ      'C'
     C     SCFEIN        ANDEQ     EMFEIN
     C                   LEAVE
     C                   ENDIF                                                  @@MODE
      *** END ADD ***** 04/18/03 *************************************
     C                   Z-ADD     SCFEIN        XXFEIN
     C*
     C  N90EMKY2         CHAIN     PEM12                              60
     C*
     C*    CHECK THAT CHAIN NOT LOOKING AT CURRENT EMPLOYER
     C*
     C  N90*IN60         DOWEQ     *OFF
     C*
     C     SCFND         IFNE      EMFND
     C     SCEMP#        ORNE      EMEMP#
      *** BEG DEL ***** 04/18/03 *************************************
     C********** SCDIV     ORNE EMDIV
      *** END DEL ***** 04/18/03 *************************************
     C                   SETON                                        896090
     C                   ENDIF
     C*
      *** BEG CHG ***** 04/18/03 *************************************
     C********** EMKY2     READEPEM12                    60
     C  N60EMKY2         READE     PEM12                                  60
      *** END CHG ***** 04/18/03 *************************************
     C                   ENDDO
     C*
     C*  RELOAD CURRENT SCREEN EMPLOYER MASTER RECORD
     C*
      ***** BEG CHG ******* 08/22/05 **********************************
     C*********  @@RRN     CHAINDBRCDF               60
     C     @@RRN         CHAIN(N)  DBRCDF                             60
      ***** END CHG ******* 08/22/05 **********************************
     C*
     C*  CHECK TO SEE IF FEIN HAS BEEN USED IN QUOTE ENVIRONMENT
     C*
     C                   MOVE      ##CO#         #@CO#
     C                   MOVE      SCFEIN        #@FEIN
     C*
     C                   CALL      'WA032C'
     C                   PARM                    #@CO#             3
     C                   PARM                    #@FEIN            9
     C                   PARM      @@QTE         #@QTE             1
     C                   PARM      *BLANKS       #@ERR             1
     C*
     C  N90#@ERR         IFEQ      *ON
     C                   SETON                                        89  90
     C                   ENDIF                                                  #@ERR=*ON
     C***  END   ADD  ***  03/31/03  *********************************
      *
      *** BEG ADD ***** 04/18/03 *************************************
     C                   ENDDO
      *
     C                   ENDIF
      *** END ADD ***** 04/18/03 *************************************
      *
      **** BEGIN ADD *** 12/09/97 *************************************
     C  N90EXFIFG        IFEQ      'Y'
     C     SCFEIN        ANDEQ     *ZEROS
      ***  BEGIN ADD  ***  06/26/98  *********************************
     C     @@QTE         ANDNE     'Q'
      ***  END   ADD  ***  06/26/98  *********************************
      *
     C                   SETON                                        505190
      *
     C                   ENDIF                                                  EXFIFG IFEQ 'Y'
      **** END   ADD *** 12/09/97 *************************************
     C*
     C***  BEGIN ADD  ***  03/23/99  *********************************
     C     WCFL08        IFNE      'C'                                          NOT CLAIMS ONLY CLT
     C***  END   ADD  ***  03/23/99  *********************************
      *
     C  N90SCSTRP        IFNE      'Y'
     C     SCSTRP        ANDNE     'N'
     C                   SETON                                        20  90
     C                   ENDIF                                                  SCSTRP<>Y/N
      *
     C  N90OVRFLG        IFEQ      *OFF                                         OVERRIDE MSG
     C     @@MODE        ANDEQ     'C'                                          CHANGE/UPDATE
     C     EMSTRP        ANDEQ     'Y'
     C     SCSTRP        ANDEQ     'N'
     C                   SETON                                        82  90
     C                   MOVE      *ON           OVRFLG
     C                   ENDIF                                                  STOVRD=*OFF
      *
     C  N90              MOVE      *OFF          OVRFLG
     C                   MOVE      *OFF          STRPFL
      *
     C  N90
     CANN41
     CAN 40SCSTRP        IFEQ      'N'
     C*** BEG ADD *** 04/18/05 ****************************
     C     FNPEOF        ANDNE     'S'
     C*** END ADD *** 04/18/05 ****************************
     C                   SETON                                        80  90
     C                   ENDIF                                                  SCSTRP=N
      *
     C     *IN90         IFEQ      *OFF
      *
      ***** BEG   CHG **** 02/28/11 ******************************
     C*****      EMKY1     CHAINPEM01                60
     C     EMKY1         CHAIN(N)  PEM01                              60
      ***** END   CHG **** 02/28/11 ******************************
      *
     C     *IN60         DOWEQ     *OFF
      *
     C  N90EMSTRP        IFEQ      'Y'
      *
     C                   MOVE      *ON           STRPFL            1
      *
     C  N90SCSTRP        IFEQ      'Y'
      *
     C  N90*IN41         IFEQ      *OFF
     C     EMDIV         ANDNE     SCDIV
     C     *IN41         OREQ      *ON
     C                   SETON                                        81  90
     C                   ENDIF                                                  *IN41=*OFF
      *
     C                   ENDIF                                                  SCSTRP=Y
      *
     C                   ENDIF                                                  EMSTRP=Y
     C*** BEGIN DEL ****** 11/18/11 ************************************
     C* Move following window edit to after state reporting edits
     C****  BEGIN ADD **** 02/28/11 *********************************
     C* DISPLAY MESSAGE WINDOW IF REQUIRED
     C************
     C************         EXSR GETDIV
     C************
     C***********EMDIV     IFNE NXTDIV
     C*********  NXTDIV    SUB  1         X1DIV
     C*********
     C*********            ENDIF
     C*********
     C*********  @@MODE    IFEQ 'C'
     C*********  SCEACD    ANDNESVEACD
     C*********  XXPEOF    ANDNE*BLANKS
     C*********  EMSTRP    ANDEQ'Y'
     C*********  X1DIV     ANDGT0
     C*********
     C*********            EXFMTWDW02
     C*********
     C*********  *INKL     IFEQ *ON
     C*********            SETON                     90
     C*********            ENDIF
     C*********
     C*********            ENDIF
     C****  END   ADD **** 02/28/11 *********************************
     C*** END   DEL ****** 11/18/11 ************************************
      *
     C     EMKY1         READE     PEM01                                  60
      *
     C  N90              ENDDO                                                  *IN60=*OFF
      *
     C  N90OVRFLG        IFEQ      *OFF
     C     STRPFL        ANDEQ     *OFF
     C     SCSTRP        ANDNE     'Y'
     C                   SETON                                        82  90
     C                   MOVE      *ON           OVRFLG
     C                   ENDIF                                                  OVRFLG=*OFF
      *
     C  N90              MOVE      *OFF          OVRFLG
      *
      ****  BEG   CHG **** 02/28/11 *******************************
     C*****      EMKY      CHAINPEM01                60
     C     EMKY          CHAIN(N)  PEM01                              60
      ***** END   CHG **** 02/28/11 ******************************
      *
     C                   ENDIF                                                  *IN90=*OFF
     C*
     C***  BEGIN ADD  ***  03/23/99  *********************************
     C                   ENDIF                                                  WCFL08<>C
     C***  END   ADD  ***  03/23/99  *********************************
     C****  BEGIN ADD **** 11/18/11 *********************************
     C* Move this edit from within State reporting edit to the end
     C* DISPLAY MESSAGE WINDOW IF REQUIRED
     C*
     C     @@MODE        IFEQ      'C'
     C     SCEACD        ANDNE     SVEACD
     C     XXPEOF        ANDNE     *BLANKS
     C     EMSTRP        ANDEQ     'Y'
     C     XX#DIV        ANDGT     1
     C*
     C                   EXFMT     WDW02
     C*
     C     *INKL         IFEQ      *ON
     C                   SETON                                        90
     C                   ENDIF
     C*
     C                   ENDIF
     C****  END   ADD **** 11/18/11 *********************************
     C***  BEG   ADD  ***  04/28/09  *********************************
     C*CHECK THE "STAT RPT/RATING FILE" WDRRP FOR RISK ID REQUIREMENTS INFORMATI
     C*ON IN ADDITION TO THE ADMIN DARTS CONTROL FILE (WMEXP)
     C     SCPST         IFGT      *BLANKS
     C                   MOVE      SCPST         ##RRST
     C                   ELSE
     C                   MOVE      SCMST         ##RRST
     C                   ENDIF
     C*
     C     RRKY          CHAIN     PRR01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     XXSRRB        IFNE      'Y'
     C                   MOVE      RRSRRB        XXSRRB
     C                   ENDIF
     C*
     C     XXSRRQ        IFNE      'Y'
     C                   MOVE      RRSRRQ        XXSRRQ
     C                   ENDIF
     C*
     C     XXNRRB        IFNE      'Y'
     C                   MOVE      RRNRRB        XXNRRB
     C                   ENDIF
     C*
     C     XXNRRQ        IFNE      'Y'
     C                   MOVE      RRNRRQ        XXNRRQ
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN60 = 0
      *
     C***** END ADD *****  04/28/09  *********************************
     C***** BEG ADD *****  10/07/04  *********************************
      * Check State Risk ID# being required
      *
     C*
      * W0050R02 puts nine zeros in fields EMSEMP and EMRSK# if not entered, so
      * need to check against a nine position constant of zeros, this in additon
      * to the check for blanks, if is a change.
      *
     C     *IN40         IFEQ      *ON                                          ADD MODE
     C     @@MODE        OREQ      'C'                                          CHANGE MODE
      *
     C     @@QTE         IFEQ      'Q'                                          QUOTE EMPLOYER?
      *
     C                   MOVEL(P)  SCSEMP        XXRISK            9
      *
     C     SCSEMP        IFEQ      *BLANKS
     C     XXSRRQ        ANDEQ     'Y'
     C     XXRISK        OREQ      TZEROS                                       TEST Risk ID #
     C     XXSRRQ        ANDEQ     'Y'
     C                   SETON                                        90  30
     C                   ENDIF
      *
     C                   MOVEL(P)  SCRSK#        XXRISK
      *
     C  N90SCRSK#        IFEQ      *ZEROS
     C     XXNRRQ        ANDEQ     'Y'
     C     XXRISK        OREQ      TZEROS                                       TEST Risk ID #
     C     XXNRRQ        ANDEQ     'Y'
     C                   SETON                                        90  38
     C                   ENDIF
     C***** BEG ADD *****  04/28/09  *********************************
     C* UN-COMMENT VALIDATION FOR NON-QUOTE PROCESSING
     C                   ELSE
      *
     C                   MOVEL(P)  SCSEMP        XXRISK            9
      *
     C     SCSEMP        IFEQ      *BLANKS
     C     XXSRRB        ANDEQ     'Y'
     C     XXRISK        OREQ      TZEROS                                       TEST Risk ID #
     C     XXSRRB        ANDEQ     'Y'
     C                   SETON                                        90  29
     C                   ENDIF
      *
     C                   MOVEL(P)  SCRSK#        XXRISK
      *
     C  N90SCRSK#        IFEQ      *ZEROS
     C     XXNRRB        ANDEQ     'Y'
     C     XXRISK        OREQ      TZEROS                                       TEST Risk ID #
     C     XXNRRB        ANDEQ     'Y'
     C                   SETON                                        90  56
     C                   ENDIF
     C***** END ADD *****  04/28/09  *********************************
      *
     C*********            ELSE
      *
     C*********            MOVELSCSEMP    XXRISK  9 P
      *
     C*********  SCSEMP    IFEQ *BLANKS
     C*********  XXSRRB    ANDEQ'Y'
     C*********  XXRISK    OREQ TZEROS                     TEST Risk ID #
     C*********  XXSRRB    ANDEQ'Y'
     C*********            SETON                     90  29
     C*********            ENDIF
      *
     C**********           MOVELSCRSK#    XXRISK    P
      *
     C**N90****  SCRSK#    IFEQ *ZEROS
     C*********  XXNRRB    ANDEQ'Y'
     C*********  XXRISK    OREQ TZEROS                     TEST Risk ID #
     C*********  XXNRRB    ANDEQ'Y'
     C*********            SETON                     90  39
     C*********            ENDIF
      *
     C                   ENDIF                                                  @@QTE = Q
     C                   ENDIF                                                  *IN40 = *OFF
     C***** END ADD *****  10/07/04  *********************************
      *
      *  VALIDATE TYPE OF BUSINESS
      *
     C                   MOVEL(P)  'BUSI'        XZTBID
     C                   MOVEL(P)  SCTPOB        XXKEY
      *
     C                   MOVEL     *BLANKS       SCTPBD
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL(P)  GTDESC        SCTPBD
      *
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   SETON                                        21  90
     C                   ENDIF                                                  *IN60=*ON/GTSTAT<>A
      *
      *  VALIDATE SIC CODE, IF REQUIRED IN ADMIN/DARTS CONTROL FILE
      *
     C  N90SCSIC         IFEQ      *BLANKS
      *
     C     EXSICR        IFEQ      'Y'
      ***  BEGIN ADD  ***  06/26/98  *********************************
     C     @@QTE         ANDNE     'Q'
      ***  END   ADD  ***  06/26/98  *********************************
     C                   SETON                                        23  90
     C                   ENDIF                                                  EXSICR=Y
      *
     C                   ELSE
      *
      ***  BEGIN CHG  ***  11/07/97  *********************************
      **********           MOVELSCSIC     XXKEY     P
      **********
      ********** TBKY      CHAINPSC01                60
      **********
      **N90*60**           SETON                     23  90
      **********
      **N90N60**           MOVELSCDESC    SCSICD
     C                   MOVEL(P)  'SIC'         XZTBID
     C                   MOVEL(P)  SCSIC         XXKEY
      *
     C                   MOVEL     *BLANKS       SCSICD
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL(P)  GTDESC        SCSICD
      *
     C  N90*IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   SETON                                        23  90
     C                   ENDIF                                                  *IN60=*ON/GTSTAT<>A
      ***  END   CHG  ***  11/07/97  *********************************
      *
     C                   ENDIF                                                  SCSIC=*BLANKS
      *
      *  VALIDATE EMPLOYEE LEASING POLICY ID
      *
      *
     C                   MOVE      'NREL'        XZTBID
     C                   MOVEL(P)  SCLSID        XXKEY
      *
     C     GTKY          CHAIN     PGT01                              60
      *
     C  N90WCFL03        IFEQ      'Y'                                          NCCI ELCTRONIC
     C  N90
     CAN 60              SETON                                        22  90
     C                   ENDIF                                                  WCFL03=Y
      *
     C  N90
     CANN60              MOVEL     GTDESC        SCELDS
      *
      *  VALIDATE THE EXPERIENCE RATING
      *
     C                   MOVEL(P)  'EXRT'        XZTBID
     C                   MOVEL(P)  SCEXRT        XXKEY
      *
     C                   MOVEL     *BLANKS       SCEXDS
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL(P)  GTDESC        SCEXDS
      *
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   SETON                                        27  90
     C                   ENDIF                                                  *IN60=*ON/MASTAT<>A
     C*
     C*** BEG ADD *** 03/31/05 ****************************************
     C     *IN09         IFEQ      *OFF                                         NOT DISPLAY MODE
     C     *IN62         ANDEQ     *OFF                                         NOT QUOTE MODE
     C     SCPST         IFGT      *BLANKS
     C                   MOVE      SCPST         ##RRST
     C                   ELSE
     C                   MOVE      SCMST         ##RRST
     C                   ENDIF
     C*
     C     RRKY          CHAIN     PRR01                              60
     C     *IN60         IFEQ      *OFF
     C     RRNAIC        ANDEQ     'Y'
     C     SCNAIC        ANDEQ     *BLANKS
     C                   SETON                                        90  85
     C                   ENDIF
     C                   ENDIF
     C*
     C*** END ADD *** 03/31/05 ****************************************
     C***** BEGIN ADD **** 09/07/99 ***********************************
     C*  VALIDATE THE NAIC CODE
     C*
     C     SCNAIC        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  'NAIC'        XZTBID
     C                   MOVEL(P)  SCNAIC        XXKEY
     C                   MOVEL(P)  *BLANKS       SCNADS
     C*
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL(P)  GTDESC        SCNADS
     C*
     C  N90*IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   SETON                                        84  90
     C                   ENDIF                                                  *IN60=ON
     C*
     C                   ENDIF                                                  SCNAIC<>BLK
     C*
     C***** END   ADD **** 09/07/99 ***********************************
      *
      *  VALIDATE THE TPA OFFICE #
      *
     C     OFKY          CHAIN     POF01                              60
      *
     C  N90
     CAN 60              SETON                                        24  90
      *
     C  N90
     CANN60              MOVEL     OFNAME        SCTPAD
      *
      *  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
      *
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
      *
     C***  BEGIN CHG  ***  01/04/00  *********************************
     C**N90***** EMDT      IFGT SAVEDT
     C  N90
     CANN41EMDT          IFGT      SAVEDT
     C***  END   CHG  ***  01/04/00  *********************************
     C     EMDT          OREQ      SAVEDT
     C     EMTIME        ANDGT     SAVETM
     C                   SETON                                        93  90
     C                   ENDIF                                                  EMDT>SAVEDT
      *
      ***** BEG ADD ***** 03/04/03 ************************************
      * Check State Unemployment Flag for required field
     C     *IN90         IFEQ      *OFF
     C     *IN09         ANDEQ     *OFF                                         NOT DISPLAY MODE
     C     *IN62         ANDEQ     *OFF                                         NOT QUOTE MODE
     C     SCPST         IFGT      *BLANKS
     C                   MOVE      SCPST         ##RRST
     C                   ELSE
     C                   MOVE      SCMST         ##RRST
     C                   ENDIF                                                  END SCPST=*
     C     RRKY          CHAIN     PRR01                              60
     C     *IN60         IFEQ      *OFF
     C     RRURFL        ANDEQ     'Y'
     C     SCUE#         ANDEQ     *BLANKS
     C                   SETON                                        90  28
     C                   ENDIF                                                  END 60=OFF
     C                   ENDIF                                                  END 90=OFF
      ***** END ADD ***** 03/04/03 ************************************
      *
      *  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
      *
     C     *IN90         IFEQ      *OFF
      *
      *  IN ADD MODE, W/NO AUTO ASSIGNMENT DISPLAY WINDOW FOR ENTRY OF
      *  EMPLOYER/DIV
      *
     C     *IN40         IFEQ      *ON                                          ADD MODE
     C                   MOVE      *ON           AUTOFL            1
      *
     C     *IN41         IFEQ      *OFF                                         NEW EMPLOYER
      *
     C     FNAUTE        IFEQ      'N'
     C                   MOVE      *OFF          AUTOFL
     C                   Z-ADD     *ZEROS        SCEMP#
      *
     C     FNDIV0        IFEQ      'Y'
     C                   Z-ADD     *ZEROS        SCDIV
     C                   ELSE
     C                   Z-ADD     1             SCDIV
     C                   ENDIF                                                  FNDIV0=Y
      *
     C     *IN90         DOUEQ     *OFF
     C     *INKL         OREQ      *ON
     C                   EXFMT     WDWEMP#M
     C                   EXSR      ERRCKW
     C                   ENDDO                                                  *IN90=*OFF
      *
     C                   ENDIF                                                  FNAUTE=N
      *
     C                   ELSE
      *
     C     FNAUTD        IFEQ      'N'
     C                   MOVE      *OFF          AUTOFL
      *
      ***** BEG DEL ******* 06/29/07 **********************************
     C*********  FNDIV0    IFEQ 'Y'
     C*********            Z-ADD*ZEROS    SCDIV
     C*********            ELSE
     C*********            Z-ADD1         SCDIV
     C*********            ENDIF                           FNDIV0=Y
      ***** END DEL ******* 06/29/07 **********************************
      ***** BEG ADD ******* 06/29/07 **********************************
      *
     C                   EXSR      GETDIV
     C                   Z-ADD     NXTDIV        SCDIV
      *
      ***** END ADD ******* 06/29/07 **********************************
      *
     C     *IN90         DOUEQ     *OFF
     C     *INKL         OREQ      *ON
     C                   EXFMT     WDWEMP#M
     C                   EXSR      ERRCKW
     C                   ENDDO                                                  *IN90=*OFF
      *
     C                   ENDIF                                                  FNAUTD=N
      *
     C                   ENDIF                                                  *IN41=*OFF
      *
     C                   ENDIF                                                  *IN40=*ON
      *
     C                   ENDIF                                                  *IN90=*OFF
      *
     C                   ENDSR                                                  ERRCK3
      *
      *
      *===============================================================
      * ERRCKW - SUBROUTINE FOR ERROR CHECKING WDWEMP#M
      *===============================================================
      *
     C     ERRCKW        BEGSR
      *
     C                   MOVE      *OFF          *IN90
      *
     C     *INKL         COMP      *ON                                    90
      *
      *  THE EMPLOYER NUMBER CANNOT BE EQUAL TO ZERO
      *
     C  N90SCEMP#        IFLE      *ZERO
     C                   SETON                                        20  90
     C                   ENDIF                                                  SCEMP#<=*ZERO
      *
      *  THE DIVISION CANNOT BE ZERO IF A DIVISION ZERO IS NOT ALLOWED
      *
     C  N90FNDIV0        IFEQ      'N'
     C     SCDIV         ANDEQ     *ZEROS
     C                   SETON                                        21  90
     C                   ENDIF                                                  FNDIV0=N
      *
      ***** BEG   CHG **** 02/28/11 **********************************
     C**N90      EMKY      CHAINPEM01                60
     C  N90EMKY          CHAIN(N)  PEM01                              60
      **** END   CHG **** 02/28/11 ********************************
     C  N90
     CANN60              SETON                                        22  90
      ***** BEG ADD ******* 06/29/07 **********************************
     C  N90EMKY          CHAIN     PEMOTH                             60
     C  N90
     CANN60              SETON                                        22  90
      ***** END ADD ******* 06/29/07 **********************************
      *
      *  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
      *
     C                   ENDSR                                                  ERRCKW
      *
      *===============================================================
      * KIDO - SUBROUTINE FOR F9
      *===============================================================
      *
      *  F9=CX HISTORY
      *
     C     KIDO          BEGSR
      *
     C                   CALL      'W0168R'
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
      **** BEGIN CHG **** 01/25/00 *********************************************
     C******************** PARM @@MODE    ##MODE  1
     C                   PARM      '1'           ##MODE            1
      **** END   CHG **** 01/25/00 *********************************************
     C                   PARM      @@RRN         ##RRN             9 0
     C                   PARM      *ZEROS        ##RRN2            9 0
      *
     C                   MOVE      *ON           *IN90
      *
     C                   ENDSR                                                  KIDO
      *
      *===============================================================
      * KJDO - SUBROUTINE FOR F10
      *===============================================================
      *
      *  F10=SEARCH
      *
     C     KJDO          BEGSR
      *
     C                   MOVE      *ON           *IN39
      *
     C                   SELECT
      *
      *  STAFF CODES
      *
     C     HFLD          WHENEQ    'SCPSRV'
     C     HFLD          OREQ      'SCUWTR'
     C     HFLD          OREQ      'SCPACC'
     C     HFLD          OREQ      'SCARVW'
     C     HFLD          OREQ      'SCAMGR'
     C     HFLD          OREQ      'SCLREP'
      *
     C                   MOVE      *BLANKS       ##STAF
      *
     C                   CALL      'S0005R'      UPRMPL                 99
      *
     C                   Z-ADD     1             X1                3 0
     C     HFLD          LOOKUP    STAF(X1)                               60
      *
     C     ##RRN         IFNE      *ZEROS
      *
     C     ##RRN         CHAIN     PUP03                              99
      *
     C                   MOVEL     UPSTAF        STF(X1)
     C                   ADD       70            X1
     C                   ELSE
     C                   ADD       69            X1
     C                   ENDIF                                                  ##STAF<>*BLANKS
      *
     C****** BEG ADD ***** 07/06/07 **************************************
     C     HFLD          IFEQ      'SCLREP'
     C     SCLREP        ANDNE     ' '
     C                   MOVE      *ON           *IN78
     C                   ENDIF
     C****** BEG ADD ***** 07/06/07 **************************************
     C                   MOVE      *ON           *IN(X1)
      *
     C****** BEG DEL ***** 07/07/05 **************************************
      *************************************  ACTIVITY STATUS
      *
     C***********HFLD***** WHEQ 'SCASTS'
     C********************
     C******************** CLEAR##RRN
     C********************
     C******************** CALL 'W2164R'  ACDPL        99
     C********************
     C***********##RRN**** IFNE *ZEROS
     C******************** MOVE *ON       *IN77
     C***********##RRN**** CHAINWTAVP                60
     C******************** MOVELAVKEY     SCASTS    P
     C******************** MOVELAVDESC    SCACTD
     C******************** ELSE
     C******************** MOVE *ON       *IN76
     C******************** ENDIF                           XXKEY<>*BLANKS
     C****** END DEL ***** 07/07/05 **************************************
      *
      *  COUNTY CODE
      *
     C     HFLD          WHENEQ    'SCCNTY'
      *
     C     SCPST         IFNE      *BLANKS
     C                   MOVEL     SCPST         ZZST              2
     C                   ELSE
     C                   MOVEL     SCMST         ZZST
     C                   ENDIF                                                  SCPST<>*BLANKS
      *
     C                   CALL      'W2215R'      ACDPL4
      *
     C     ##RRN         CHAIN     WTCNP                              60
     C  N602             SUBST     CNKEY:3       SCCNTY
     C  N60              MOVEL(P)  CNDESC        SCCNTD
      *
     C****** BEG CHG ***** 07/06/07 **************************************
     C********   SCCNTY    COMP *BLANKS              797978
     C****** END CHG ***** 07/06/07 **************************************
     C****** BEG ADD ***** 07/06/07 **************************************
     C     ##RRN         IFNE      *ZEROS
     C                   MOVE      *ON           *IN77
     C                   ELSE
     C                   MOVE      *ON           *IN78
     C                   ENDIF
     C****** END ADD ***** 07/06/07 **************************************
      *
      *  REGION
      *
     C     HFLD          WHENEQ    'SCRGN#'
      *
     C                   Z-ADD     *ZEROS        ##RRN
      *
     C                   CALL      'W0520R'      ACDPL2
      *
     C     ##RRN         IFNE      *ZEROS
      *
     C****** BEG CHG ***** 07/06/07 **************************************
     C***********          MOVE *ON       *IN81
     C                   MOVE      *ON           *IN79
     C****** BEG CHG ***** 07/06/07 **************************************
      *
     C     ##RRN         CHAIN     PRE02                              60
     C  N60              MOVEL(P)  REDESC        SCRGND
     C  N60              MOVEL(P)  RERGN#        SCRGN#
      *
     C                   ELSE
      *
     C                   MOVE      *ON           *IN80
      *
     C                   ENDIF                                                  SCRGN#<>*ZEROS
      *
      *  CHAPTER
      *
     C     HFLD          WHENEQ    'SCCHAP'
      *
     C                   CALL      'W0720R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##RRN             9 0
     C                   PARM      DBRRN1        ##RRN2            9 0
     C                   PARM                    ##CHAP
      *
     C     ##CHAP        IFNE      *ZEROS
      *
     C                   Z-ADD     ##CHAP        SCCHAP
     C****** BEG CHG ***** 07/06/07 **************************************
     C*********            MOVE *ON       *IN83
     C                   MOVE      *ON           *IN81
     C****** END CHG ***** 07/06/07 **************************************
     C****** BEG ADD ***** 03/12/10 **************************************
     C                   MOVE      *OFF          *IN67
     C****** END ADD ***** 03/12/10 **************************************
      *
     C     CPKY          CHAIN     PCP01                              60
     C  N60              MOVEL(P)  CPDESC        SCCHPD
      *
     C                   ELSE
      *
     C                   MOVE      *ON           *IN82
      *
     C                   ENDIF                                                  SCCHAP<>*ZEROS
     C*** BEGIN ADD *** 10/05/09 **************************************
      *
     C     HFLD          WHENEQ    'SCFLXV'
      *
     C*
     C* FLEX VALUE SEARCH
      *
     C                   CALL      'W2493R'
     C                   PARM                    ##DATA
     C                   PARM                    @@MODE
     C                   PARM      EXFLXT        XZTBID
     C                   PARM      *BLANKS       XXKEY
     C                   PARM      *BLANKS       QQRTC             1
      *
     C     XXKEY         IFNE      *BLANKS
     C                   MOVE      *ON           *IN43
     C                   MOVEL(P)  XXKEY         SCFLXV
     C     GTKY          CHAIN     PGT01                              60
     C                   MOVEL(P)  GTDESC        SCFLXD
     C                   ELSE
     C                   MOVE      *ON           *IN42
     C                   ENDIF                                                  XXKEY<>*BLANKS
     C*** END   ADD *** 10/05/09 **************************************
      *
      *  TYPE OF BUSINESS
      *
     C     HFLD          WHENEQ    'SCTPOB'
      *
     C                   MOVEL(P)  'BUSI'        XZTBID
     C                   MOVE(P)   *BLANK        XXKEY
      *
     C                   CALL      'W2493R'
     C                   PARM                    ##DATA
     C                   PARM                    @@MODE
     C                   PARM                    XZTBID
     C                   PARM                    XXKEY
     C                   PARM                    QQRTC             1
      *
     C     XXKEY         IFNE      *BLANKS
     C                   MOVE      *ON           *IN33
     C                   MOVEL(P)  XXKEY         SCTPOB
     C     GTKY          CHAIN     PGT01                              60
     C                   MOVEL(P)  GTDESC        SCTPBD
     C                   ELSE
     C                   MOVE      *ON           *IN31
     C                   ENDIF                                                  XXKEY<>*BLANKS
      *
      *  SIC CODE
      *
     C     HFLD          WHENEQ    'SCSIC'
      *
      ***  BEGIN CHG  ***  11/07/97  *********************************
      **********           MOVE *BLANK    XXKEY
      **********
      **********           CALL 'W2216R1' ACDPL3
      **********
      ********** XXKEY     IFNE *BLANKS
      **********
      **********           MOVE *ON       *IN34
      **********           MOVELXXKEY     SCSIC
      **********
      **********           MOVELSCSIC     XXKEY     P
      ********** TBKY      CHAINPSC01                60
      **N60*****           MOVELSCDESC    SCSICD
      **********
      **********           ELSE
      **********
      **********           MOVE *ON       *IN33
      **********
      **********           ENDIF                           XXKEY<>*BLANKS
     C                   MOVEL(P)  'SIC'         XZTBID
     C                   MOVE(P)   *BLANK        XXKEY
      *
     C                   CALL      'W2493R'
     C                   PARM                    ##DATA
     C                   PARM                    @@MODE
     C                   PARM                    XZTBID
     C                   PARM                    XXKEY
     C                   PARM                    QQRTC             1
      *
     C     XXKEY         IFNE      *BLANKS
     C                   MOVE      *ON           *IN34
     C                   MOVEL(P)  XXKEY         SCSIC
     C     GTKY          CHAIN     PGT01                              60
     C                   MOVEL     GTDESC        SCSICD
     C                   ELSE
     C                   MOVE      *ON           *IN33
     C                   ENDIF                                                  XXKEY<>*BLANKS
      ***  END   CHG  ***  11/07/97  *********************************
      *
      *  EMPLOYEE LEASING ID
      *
     C     HFLD          WHENEQ    'SCLSID'
      *
      *
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS           40
     C                   PARM      '2'           QQCALL            1
     C                   PARM      'NREL'        QQID              4
     C                   PARM      *BLANKS       QQKEY            10
     C                   PARM                    QQRTCD            1
      *
     C     QQKEY         IFNE      *BLANKS
      *
     C                   MOVE      *ON           *IN37
     C                   MOVEL     QQKEY         SCLSID
     C                   MOVEL     QQID          XZTBID
     C                   MOVEL     QQKEY         XXKEY
      *
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL     GTDESC        SCELDS
      *
     C                   ELSE
      *
     C                   MOVE      *ON           *IN32
      *
     C                   ENDIF                                                  XXKEY<>*BLANKS
      *
      *  EXPERIENCE RATING
      *
     C     HFLD          WHENEQ    'SCEXRT'
      *
     C                   MOVEL(P)  'EXRT'        XZTBID
     C                   MOVE(P)   *BLANK        XXKEY
      *
     C                   CALL      'W2493R'
     C                   PARM                    ##DATA
     C                   PARM                    @@MODE
     C                   PARM                    XZTBID
     C                   PARM                    XXKEY
     C                   PARM                    QQRTC
      *
     C     XXKEY         IFNE      *BLANKS
     C***** BEGIN CHG **** 09/07/99 ***********************************
     C**********           MOVE *ON       *IN35
     C                   MOVE      *ON           *IN48
     C***** END   CHG **** 09/07/99 ***********************************
     C                   MOVEL(P)  XXKEY         SCEXRT
     C     GTKY          CHAIN     PGT01                              60
     C                   MOVEL(P)  GTDESC        SCEXDS
     C                   ELSE
     C                   MOVE      *ON           *IN37
     C                   ENDIF                                                  XXKEY<>*BLANKS
     C*
     C***** BEGIN ADD **** 09/07/99 ***********************************
      *  NAIC CODE
      *
     C     HFLD          WHENEQ    'SCNAIC'
      *
     C                   MOVEL(P)  'NAIC'        XZTBID
     C                   MOVE(P)   *BLANK        XXKEY
      *
     C                   CALL      'W2493R'
     C                   PARM                    ##DATA
     C                   PARM                    @@MODE
     C                   PARM                    XZTBID
     C                   PARM                    XXKEY
     C                   PARM                    QQRTC
      *
     C     XXKEY         IFNE      *BLANKS
     C                   MOVE      *ON           *IN35
     C                   MOVEL(P)  XXKEY         SCNAIC
     C     GTKY          CHAIN     PGT01                              60
     C                   MOVEL(P)  GTDESC        SCNADS
     C                   ELSE
     C                   MOVE      *ON           *IN48
     C                   ENDIF                                                  XXKEY<>*BLANKS
     C*
     C***** END   ADD **** 09/07/99 ***********************************
      *
      *  TPA OFFICE #
      *
     C     HFLD          WHENEQ    'SCOFF#'
      *
     C                   CALL      'W0002R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##OFF#            5
      *
     C     ##OFF#        IFNE      *BLANKS
      *
     C                   MOVE      *ON           *IN36
     C                   MOVEL     ##OFF#        SCOFF#
      *
     C     OFKY          CHAIN     POF01                              60
     C  N60              MOVEL(P)  OFNAME        SCTPAD
      *
     C                   ELSE
      *
     C                   MOVE      *ON           *IN35
      *
     C                   ENDIF                                                  ##OFF#<>*BLANKS
     C*** BEG ADD ****** 08/21/08 ************************************
     C*
     C*  Mailing or physical city/state search
     C*
     C     HFLD          WHENEQ    'SCMST'
     C     HFLD          OREQ      'SCMCTY'
     C     HFLD          OREQ      'SCPST'
     C     HFLD          OREQ      'SCPCTY'
     C*
     C                   CALL      'W1070R'      CYSPL
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     WMCYP                              99
     C*
     C     HFLD          IFEQ      'SCMST'
     C     HFLD          OREQ      'SCMCTY'
     C*
     C                   MOVE      CYST          SCMST
     C                   MOVEL     CYCTY         SCMCTY
     C                   SETON                                        32
     C*
     C                   ELSE
     C*
     C                   MOVE      CYST          SCPST
     C                   MOVEL     CYCTY         SCPCTY
     C                   SETON                                        33
     C*
     C                   ENDIF                                                  HFLD=SCMST
     C*
     C     *IN40         IFEQ      *ON
     C*
     C                   MOVE      CYCNTY        SCCNTY
     C                   MOVE      CYRGN         SCRGN#
     C                   MOVE      CYCHAP        SCCHAP
     C                   MOVE      CYLREP        SCLREP
     C*
     C                   ENDIF                                                  *IN40<>*ON
     C*
     C                   ENDIF                                                  ##RRN<>ZEROS
     C*** END ADD ****** 08/21/08 ************************************
      ***  BEGIN ADD  ***  10/13/98  *********************************
      *_______________________________________________________________
     C     HFLD          WHENEQ    'SCFND'
      *
     C     Q2RECF        IFEQ      'SCRCOPY'
      *
     C                   MOVE      ##CO#         #@CO#
      *
     C                   CALL      'W0010R'                             99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    #@CO#             3
     C                   PARM      *BLANKS       #@FND             3
      *
     C     #@FND         IFEQ      *BLANKS
     C                   MOVE      *ON           *IN25
     C                   ELSE
     C                   MOVE      #@FND         SCFND
     C                   MOVE      *ON           *IN26
     C                   ENDIF                                                  #@FND=*BLANKS
      *
     C                   ENDIF                                                  Q2RECF=SCRCOPY
      *** END   ADD *** 06/23/99 ***********************************************
      *
     C                   ENDSL
      *
     C                   ENDSR                                                  KJDO
      *
      *===============================================================
      * KMDO - SUBROUTINE FOR F13
      *===============================================================
      *
      *  F13=CHANGE USER PARMS
      *
     C     KMDO          BEGSR
      *
     C                   MOVE      *OFF          *IN09
      *
     C                   CALL      'W0006R'
      *
      *  GET THE USER PARMS
      *
     C     Q1USER        CHAIN     PUP01                              60
      *
     C                   Z-ADD     UPCO#         ##CO#
     C                   Z-ADD     UPFND         SCFND
     C                   Z-ADD     UPFND         ##FND
      *
      *  GET GROUP INFORMATION
      *
     C                   CLEAR                   SCNAME
     C     FNKY          CHAIN(N)  PFN01                              60
     C  N60              MOVEL     FNNAME        SCNAME
      **** BEGIN ADD **** 10/14/98 *********************************************
     C     F2KY          CHAIN     PF201                              60
      **** END   ADD **** 10/14/98 *********************************************
      *
      *  FORCE INQUIRY IF SYSTEM DATE IS > GROUP EXPIRATION DATE
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT
      *
     C  N09FNEXDT        IFNE      *ZEROS
     C     SYSDT         ANDGT     FNEXDT
     C     SCFND         OREQ      *ZEROS
      *
     C                   EXFMT     WDW01
     C  N65              MOVEL     FK1(5)        SCFKL1
     C   65              MOVEL     FK12(5)       SCFKL1
     C                   Z-ADD     1             #OFSCR
     C                   MOVE      *ON           *IN09
     C                   MOVE      *ON           *IN90
      *
     C                   ELSE
      *
     C                   Z-ADD     3             #OFSCR
     C  N65              MOVEL     FK1(2)        SCFKL1
     C   65              MOVEL     FK12(2)       SCFKL1
      *
     C                   ENDIF                                                  FNEXDT<>*ZEROS
      *** BEG ADD **** 04/29/02 **********************************************
     C     SCFND         IFNE      SVFND
     C*
     C                   Z-ADD     SCFND         SVFND
     C*
     C     EXKY          CHAIN     PEX01                              60
     C  N60              MOVEL     EXDOFF        SCOFF#
     C   60              MOVE      *BLANKS       SCOFF#
     C*
     C                   MOVEL     EXDRGN        SCRGN#
     C   60              MOVE      *BLANKS       SCRGN#
      *
     C                   MOVEL     *BLANKS       SCTPAD
     C     OFKY          CHAIN     POF01                              60
     C  N60              MOVEL     OFNAME        SCTPAD
      *
     C                   MOVEL     *BLANKS       SCRGND
     C     REKY          CHAIN     PRE01                              60
     C  N60              MOVEL(P)  REDESC        SCRGND
      *
     C                   ENDIF
      *** END ADD **** 04/29/02 **********************************************
     C*
      **** BEGIN ADD **** 03/04/02 ************************************
     C     *LOCK         IN        *DTAARA
     C                   OUT       *DTAARA
      *
     C     LDENDA        IFEQ      *ZEROS
     C     LDENDA        OREQ      *BLANKS
      **** END   ADD **** 03/04/02 ************************************
     C***  BEGIN ADD  ***  08/11/99  *********************************
     C                   EXSR      CHKSEC
     C***  END   ADD  ***  08/11/99  *********************************
      **** BEGIN ADD **** 03/04/02 ************************************
     C                   ENDIF                                                              OS
      **** END   ADD **** 03/04/02 ************************************
      *
     C                   ENDSR                                                  KMDO
      *
      *===============================================================
      * KNDO - SUBROUTINE FOR F14
      *===============================================================
      *
      *  F14=DARTS
      *
     C     KNDO          BEGSR
      *
      *  ESTABLISH KEY
      *
     C                   CLEAR                   QCKEY
      *
     C                   MOVE      ##CO#         QCCO#
     C                   MOVE      SCFND         QCFND
     C                   MOVE      SCEMP#        QCEMP#
     C                   MOVE      SCDIV         QCDIV
     C                   MOVE      *BLANKS       QCQTE
      *
     C     @@QTE         IFEQ      'Q'
     C                   MOVE      'Q'           QCQTE
     C                   ENDIF                                                  @@QTE=Q
      *
      *  CALL DARTS ENTRY PROGRAM
      *
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM      'W0050R'      XXPGM
      *
     C                   MOVE      *ON           *IN90
      *
     C                   ENDSR                                                  KNDO
      *
      *===============================================================
      * MCAPI - DATA INTERFACE TO MANAGED CARE - FMEMP
      *===============================================================
      *
     C     MCAPI         BEGSR
      *
     C                   MOVE      EMCO#         QQCO#
     C                   MOVE      EMFND         QQFND#
     C                   MOVE      EMEMP#        QQEMP#
     C                   MOVE      EMDIV         QQDIV
      *
     C                   MOVE      EMFEIN        QQFEIN
     C                   MOVE      EMMZIP        QQMZIP
     C                   MOVE      EMPZIP        QQPZIP
     C                   MOVE      EMAREA        QQAREA
     C                   MOVE      EMPHN#        QQPHN#
     C                   MOVE      EMRGN#        QQRGN#
      *
     C                   CALL      'FA010R'
      *
     C                   PARM      *BLANK        QQRTCD            1
      *
     C                   PARM                    QQCO#             3
     C                   PARM      *BLANK        QQSUB#            5
     C                   PARM                    QQFND#            3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
      *
     C                   PARM      EMNAME        QQNAME           40
     C                   PARM      EMMAD1        QQMAD1           40
     C                   PARM      EMMAD2        QQMAD2           40
     C                   PARM      EMMCTY        QQMCTY           25
     C                   PARM      EMMST         QQMST             2
     C                   PARM                    QQMZIP            9
     C                   PARM      EMPAD1        QQPAD1           40
     C                   PARM      EMPAD2        QQPAD2           40
     C                   PARM      EMPCTY        QQPCTY           25
     C                   PARM      EMPST         QQPST             2
     C                   PARM                    QQPZIP            9
     C                   PARM                    QQAREA            3
     C                   PARM                    QQPHN#            7
     C                   PARM                    QQRGN#            3
     C                   PARM                    QQFEIN            9
     C                   PARM      EMVIP         QQVIP             1
     C                   PARM      EMMPFL        QQMPFL            1
     C                   PARM      EMSTRP        QQSTRP            1
      ******************** PARM *BLANK    QQDBA  40        DEL 04/27/98
     C                   PARM      EMFDNM        QQDBA            40            ADD 04/27/98
     C                   PARM      EMNAOB        QQNOB            40
     C                   PARM      *BLANK        QQMCO            10
     C                   PARM      *BLANK        QQMCO#           20
      *
     C                   ENDSR
      *
      *===============================================================
      * MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
      *===============================================================
      *
     C     MVDBSC        BEGSR
      *
      ***  BEG ADD ******  03/06/06  *********************************
     C                   MOVE      EMRDFL        SVRDFL            1
      ***  END ADD ******  03/06/06  *********************************
     C                   MOVE      *OFF          OVRFLG
      *
      ***  BEG ADD ******  09/20/06  *********************************
      ***  BEG ADD ******  01/20/12  *********************************
     C     CLTID         IFEQ      '071'
      ***  END ADD ******  01/20/12  *********************************
     C                   MOVEL     EMDBA         SCGSI#
      ***  BEG ADD ******  01/20/12  *********************************
     C                   ENDIF
      ***  END ADD ******  01/20/12  *********************************
      ***  END ADD ******  09/20/06  *********************************
      ***** BEG ADD ***** 04/15/05 ************************************
     C                   MOVEL     EMLINK        SCLINK
      ***** END CHG ***** 04/15/05 ************************************
      ***** BEG ADD ***** 03/24/06 ************************************
     C     CLTID         IFEQ      '132'
     C                   MOVEL     EMSHFT        SCSHFT
     C                   ENDIF
      ***** END CHG ***** 03/24/06 ************************************
      *
     C*** BEGIN CHG ***  05/14/04  ************************************
     C*********            MOVELEMNAME    SCENAM
     C*********            MOVELEMFDNM    SCFDNM
     C                   MOVEL     EMNAME        SCNAM1
     C                   MOVEL(P)  EMNAM2        XXNM10           10
     C                   MOVE      XXNM10        SCNAM1
     C                   MOVEL     EMFDNM        SCFDN1
     C                   MOVEL(P)  EMFDN2        XXNM10           10
      ***** BEGIN ADD ***** 04/25/06 **********************************
     C                   MOVE      EMNAMP        SCNAMP
     C                   MOVE      EMFDNP        SCFDNP
      ***** END ADD ******* 04/25/06 **********************************
     C                   MOVE      XXNM10        SCFDN1
     C                   MOVE      EMNAM2        SCNAM2
     C                   MOVE      EMFDN2        SCFDN2
     C*** END CHG *****  05/14/04  ************************************
     C***  BEGIN ADD  ***  09/22/99  *********************************
     C                   MOVEL     EMFDNM        SVFDNM
     C***  END   ADD  ***  09/22/99  *********************************
     C*** BEG ADD ****  04/29/02 *************************************
     C                   Z-ADD     SCFND         SVFND             3 0
     C*** END ADD ****  04/29/02 *************************************
      *
     C*** BEGIN DEL *** 06/07/99 **************************************
     C* EMDBA is no longer being used in V3
     C********** EMFDNM    IFEQ *BLANKS
     C********** EMDBA     ANDNE*BLANKS
     C**********           MOVELEMDBA     SCFDNM
     C**********           ENDIF                           SCFDNM=*BLANKS
     C*** END   DEL *** 06/07/99 **************************************
      *
     C                   MOVEL     EMMAD1        SCMAD1
     C                   MOVEL     EMMAD2        SCMAD2
     C                   MOVEL     EMMCTY        SCMCTY
     C                   MOVEL     EMMST         SCMST
     C                   MOVEL     EMMZIP        SCMZP1
     C                   MOVE      EMMZIP        SCMZP2
     C                   MOVEL     EMMPFL        SCMPFL
     C                   MOVEL     EMPAD1        SCPAD1
     C                   MOVEL     EMPAD2        SCPAD2
     C                   MOVEL     EMPCTY        SCPCTY
     C                   MOVEL     EMPST         SCPST
     C                   MOVEL     EMPZIP        SCPZP1
     C                   MOVE      EMPZIP        SCPZP2
     C*** BEGIN ADD *** 10/05/09 **************************************
     C                   MOVEL(P)  EXFLXH        SCFLXH
     C                   MOVEL(P)  EXFLXT        XZTBID
     C                   MOVEL(P)  EMFLXV        XXKEY
     C                   MOVEL(P)  EMFLXV        SCFLXV
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCFLXD
     C                   ELSE
     C                   MOVEL     *BLANKS       SCFLXD
     C                   ENDIF
     C*** END   ADD *** 10/05/09 **************************************
     C                   MOVEL     EMIADR        SCIADR
      ***** BEG DEL ******* 01/31/02 **********************************
     C*********            MOVELEMCONT    SCCONT
     C*********            MOVELEMATTN    SCATTN
     C*********            MOVELEMCNT3    SCCNT3
      ***** END DEL ******* 01/31/02 **********************************
      ***** BEG ADD ******* 01/31/02 **********************************
      *
      * Get Employer Contact Names / phone
      *
     C                   MOVE      '1'           KKPCNT
     C                   EXSR      SRCONT
     C                   MOVEL(P)  XXCNT         SCCNT1
     C                   MOVEL(P)  XXCPH         SCCPH1
      *
     C                   MOVE      '2'           KKPCNT
     C                   EXSR      SRCONT
     C                   MOVEL(P)  XXCNT         SCCNT2
     C                   MOVEL(P)  XXCPH         SCCPH2
      *
     C                   MOVE      '3'           KKPCNT
     C                   EXSR      SRCONT
     C                   MOVEL(P)  XXCNT         SCCNT3
     C                   MOVEL(P)  XXCPH         SCCPH3
      *
      *** BEG ADD ***** 11/06/06 **************************************
     C     CLTID         IFEQ      '071'
     C                   MOVE      '4'           KKPCNT
     C                   EXSR      SRCONT
     C                   MOVEL(P)  XXCNT         S1CNT4
     C                   MOVEL(P)  XXCPH         S1CPH4
     C                   MOVEL(P)  SCCNT1        S1CNT1
     C                   MOVEL(P)  SCCNT2        S1CNT2
     C                   MOVEL(P)  SCCNT3        S1CNT3
     C                   MOVEL(P)  SCCPH1        S1CPH1
     C                   MOVEL(P)  SCCPH2        S1CPH2
     C                   MOVEL(P)  SCCPH3        S1CPH3
     C                   ENDIF
      *** END ADD ***** 11/06/06 **************************************
      *
      ***** END ADD ******* 01/31/02 **********************************
     C                   MOVE      EMAREA        SCAREA
     C                   MOVEL     EMPHN#        SCPHN1
     C                   MOVE      EMPHN#        SCPHN2
     C                   MOVE      EMFAXA        SCFAXA
     C                   MOVEL     EMFAX#        SCFAX1
     C                   MOVE      EMFAX#        SCFAX2
      *
     C                   DO        5             X                 3 0
      *
     C                   Z-ADD     EEF(X)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SEF(X)
      *
     C                   Z-ADD     ECA(X)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCA(X)
      *
     C                   ENDDO                                                  X->5
      *
     C*** BEGIN DEL ***  09/16/99  ************************************
     C*********            MOVELEMSRPR    SCSRPR
     C***40****            MOVEL'P'       SCSRPR
     C*** END DEL *****  09/16/99  ************************************
     C                   MOVEL     EMPSRV        SCPSRV
     C                   MOVEL     EMUWTR        SCUWTR
     C                   MOVEL     EMPACC        SCPACC
     C                   MOVEL     EMARVW        SCARVW
     C                   MOVEL     EMAMGR        SCAMGR
     C                   MOVEL     EMLREP        SCLREP
     C****** BEG DEL ***** 07/07/05 **************************************
     C******************** MOVELEMASTS    SCASTS
     C********************
     C******************** CLEARSCACTD
     C******************** MOVELEMASTS    XXKEY
     C***********TBKY***** CHAINPAV01                60
     C********************
      ****** BEG ADD ***** 06/07/05 **************************************
     C***60**********      MOVELAVDESC    SCACTD    P
     C**N60*************** MOVELAVDESC    SCACTD    P
      ****** END ADD ***** 06/07/05 **************************************
     C****** END DEL ***** 07/07/05 **************************************
      *
     C                   MOVEL     EMWALS        SCWALS
     C   40              MOVEL     'N'           SCWALS
      *
     C                   MOVEL     EMCNTY        SCCNTY
      *
     C     SCPST         IFNE      *BLANKS
     C     SCPST         CAT       SCCNTY:0      ZZKEY
     C                   ELSE
     C     SCMST         CAT       SCCNTY:0      ZZKEY
     C                   ENDIF                                                  SCPST<>*BLANKS
      *
     C                   MOVEL(P)  ZZKEY         XXKEY
      *
     C                   MOVEL     *BLANKS       SCCNTD
     C     TBKY          CHAIN     PCN01                              60
     C  N60              MOVEL(P)  CNDESC        SCCNTD
      *
     C                   MOVEL     EMVIP         SCVIP
     C   40              MOVEL     'N'           SCVIP
      *
     C***  BEGIN ADD  ***  02/09/00  *********************************
     C   40              Z-ADD     EXDRGN        EMRGN#
     C***  END   ADD  ***  02/09/00  *********************************
     C                   MOVEL     EMRGN#        SCRGN#
      *
     C                   MOVEL     *BLANKS       SCRGND
     C     REKY          CHAIN     PRE01                              60
     C  N60              MOVEL(P)  REDESC        SCRGND
      *
     C     *IN40         IFEQ      *OFF
     C                   MOVEL     EMSPAC        SCSPAC
     C                   ELSE
     C                   MOVEL     'N'           SCSPAC
     C                   ENDIF                                                  *IN40=*OFF
      *
     C                   MOVEL     EMCHAP        SCCHAP
      *
     C                   MOVEL     *BLANKS       SCCHPD
     C     CPKY          CHAIN     PCP01                              60
     C  N60              MOVEL     CPDESC        SCCHPD
      *
      *
     C                   Z-ADD     EMFEIN        SCFEIN
     C***  BEGIN ADD  ***  09/22/99  *********************************
     C                   Z-ADD     EMFEIN        SVFEIN
     C***  END   ADD  ***  09/22/99  *********************************
     C*
     C                   MOVEL     EMSEMP        SCSEMP
      **** BEGIN CHG **** 06/15/98 *********************************************
      ******************** MOVELEMRSK#    SCRSK#
     C                   MOVEL     EMRSK#        XXRSK#
     C     DIGITS        CHECK     XXRSK#                                 60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      XXRSK#        SCRSK#
     C                   ELSE
     C                   Z-ADD     0             SCRSK#
     C                   ENDIF                                                  *IN60 IFEQ *OFF
      **** END   CHG **** 06/15/98 *********************************************
      *
     C  N40
     CANN41              MOVEL     EMSTRP        SCSTRP
     C   40              MOVEL     'Y'           SCSTRP
     C   41              MOVEL     'N'           SCSTRP
      *
      **
     C                   MOVEL     EMTPOB        SCTPOB
     C                   CLEAR                   SCTPBD
      *
     C                   MOVEL     'BUSI'        XZTBID
     C                   MOVEL(P)  SCTPOB        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL     GTDESC        SCTPBD
      *
      **
     C                   MOVEL     EMSIC         SCSIC
     C                   CLEAR                   SCSICD
      *
      ***  BEGIN CHG  ***  11/07/97  *********************************
      **********           MOVELSCSIC     XXKEY     P
      ********** TBKY      CHAINPSC01                60
      **N60*****           MOVELSCDESC    SCSICD    P
     C                   MOVEL(P)  'SIC'         XZTBID
     C                   MOVEL(P)  SCSIC         XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL     GTDESC        SCSICD
      ***  END   CHG  ***  11/07/97  *********************************
      *
      **
     C                   MOVEL     EMLSID        SCLSID
     C                   CLEAR                   SCELDS
      *
     C                   MOVEL     'NREL'        XZTBID
     C                   MOVEL(P)  SCLSID        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL     GTDESC        SCELDS
      *
      **
     C     EMEXRT        IFEQ      *BLANKS
     C                   MOVEL     '3'           EMEXRT
     C                   ENDIF                                                  EMEXRT=*BLANKS
      *
     C                   MOVEL     EMEXRT        SCEXRT
      *
     C                   MOVEL     'EXRT'        XZTBID
     C                   MOVEL(P)  SCEXRT        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL     GTDESC        SCEXDS
     C*
     C***** BEGIN ADD **** 09/07/99 ***********************************
      ***** BEG ADD ******* 04/29/03 **********************************
     C                   MOVEL(P)  EMNAIC        SCNAIC
      ***** END ADD ******* 04/29/03 **********************************
     C*
     C     SCNAIC        IFNE      *BLANKS
     C*
      ***** BEG DEL ******* 04/29/03 **********************************
     C*********            MOVELEMNAIC    SCNAIC    P
      ***** END DEL ******* 04/29/03 **********************************
     C                   MOVEL(P)  'NAIC'        XZTBID
     C                   MOVEL(P)  SCNAIC        XXKEY
     C*
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL     GTDESC        SCNADS
     C*
     C                   ELSE
     C                   MOVEL(P)  *BLANKS       SCNADS
     C*
     C                   ENDIF                                                  SCNAIC<>BLKS
     C*
     C***** END   ADD **** 09/07/99 ***********************************
     C                   MOVEL     EMNAOB        SCNAOB
     C                   MOVEL     EMOFF#        SCOFF#
      *
      **
     C                   MOVEL     EMOFF#        XXOFF#
     C     SCOFF#        IFEQ      *BLANKS
      *** BEGIN ADD ***  07/16/98  ************************************
     C     @@MODE        ANDEQ     'A'
      *** END ADD *****  07/16/98  ************************************
     C                   MOVEL     EXDOFF        SCOFF#
     C                   ENDIF                                                  SCOFF#=*BLANKS
      *
     C                   MOVEL     *BLANKS       SCTPAD
     C     OFKY          CHAIN     POF01                              60
     C  N60              MOVEL     OFNAME        SCTPAD
      *
      **
     C                   MOVEL     EMCOM1        SCCOM1
     C                   MOVEL     EMCOM2        SCCOM2
     C                   MOVEL     EMCOM3        SCCOM3
      *
     C                   Z-ADD     EMLIDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCLIDT
      *
     C                   MOVEL     EMINDE        SCINDE
      *
     C     EMLIDT        COMP      *ZEROS                             4242
     C     EMINDE        COMP      *ZEROS                             4343
      ***  BEGIN CHG  ***  06/26/98  *********************************
      ********** EXSICR    COMP 'Y'                      46
     C     EXSICR        IFEQ      'Y'
     C     @@QTE         ANDNE     'Q'
     C                   MOVE      *ON           *IN46
     C                   ELSE
     C                   MOVE      *OFF          *IN46
     C                   ENDIF                                                  EXSICR=Y
      ***  END   CHG  ***  06/26/98  *********************************
      **** BEGIN ADD **** 07/15/98 *********************************************
     C                   MOVE      EMNAME        SVNAME
     C                   MOVE      EMCHAP        SVCHAP
      **** END   ADD **** 07/15/98 *********************************************
     C***** BEGIN ADD **** 03/10/00 ***********************************
     C                   MOVE      EMTPOB        SVTPOB
     C***** END   ADD **** 03/10/00 ***********************************
     C*** BEGIN ADD ***  04/25/06  ************************************
     C                   MOVE      EMNAMP        SVNAMP
     C                   MOVE      EMFDNP        SVFDNP
     C*** END ADD *****  04/25/06  ************************************
      *
     C**** BEGIN ADD **** 09/03/98 ************************************
     C     @@MODE        IFEQ      'A'
     C**** BEGIN ADD **** 02/15/06 ************************************
     C     CLTID         ANDNE     '139'
     C**** END   ADD **** 02/15/06 ************************************
      *
     C                   MOVE      '1'           SCLSID
     C                   ENDIF                                                  @@MODE = A
     C**** END   ADD **** 09/03/98 ************************************
      ***** BEG ADD ******* 09/20/99 **********************************
     C                   MOVE      EMUE#         SCUE#
      ***** END ADD ******* 09/20/99 **********************************
      *
      *** BEG ADD ***** 09/20/06 **************************************
     C     EXAHLC        IFNE      *BLANK
     C                   MOVEL     EXAHLC        SCAHLC
     C                   MOVE      ':'           SCAHLC
     C                   ELSE
     C                   MOVEL     XXAHLC        SCAHLC
     C                   ENDIF
      *
     C     EXAHWL        IFNE      *BLANK
     C                   MOVEL     EXAHWL        SCAHWL
     C                   MOVE      ':'           SCAHWL
     C                   ELSE
     C                   MOVEL     XXAHWL        SCAHWL
     C                   ENDIF
      *
     C     EXAHVP        IFNE      *BLANK
     C                   MOVEL     EXAHVP        SCAHVP
     C                   MOVE      ':'           SCAHVP
     C                   ELSE
     C                   MOVEL     XXAHVP        SCAHVP
     C                   ENDIF
      *
     C     EXAHSA        IFNE      *BLANK
     C                   MOVEL     EXAHSA        SCAHSA
     C                   MOVE      ':'           SCAHSA
     C                   ELSE
     C                   MOVEL     XXAHSA        SCAHSA
     C                   ENDIF
      *** END ADD ***** 09/20/06 **************************************
     C***** BEG ADD ******* 12/26/07 **********************************
     C                   MOVE      EMCUST        SCCUST
     C***** END ADD ******* 12/26/07 **********************************
      ***** BEG ADD ******* 11/23/07 **********************************
     C                   MOVE      EMNAME        HNAME
     C                   MOVE      EMNAM2        HNAM2
     C                   MOVE      EMFDNM        HFDNM
     C                   MOVE      EMFDN2        HFDN2
      ***** END ADD ******* 11/23/07 **********************************
      *
      ***** BEG ADD ***** 03/28/08 ************************************
     C     CLTID         IFEQ      '114'
     C                   MOVEL(P)  EMDCRX        SCDCRX
     C                   ENDIF
      ***** END ADD ***** 03/28/08 ************************************
     C*
     C***  Begin Add  ***  05/23/08  **************************************
     C     EMNAME        CAT(P)    EMNAM2        SVDBA
     C                   MOVE      EMFEIN        SVFID
     C*** Begin Add ** 08/20/13 ****************************
     C                   MOVE      EMDIV         SVDIV
     C*** End   Add ** 08/20/13 ****************************
     C***  End   Add  ***  05/23/08  **************************************
      ***  BEG ADD ******  01/04/10  *********************************
     C                   EXSR      FLXHDG
      ***  END ADD ******  01/04/10  *********************************
      **** BEG   ADD **** 02/28/11 *********************************
     C                   MOVE      EMEACD        SCEACD
     C                   MOVE      EMEACD        SVEACD            1
      ***  END   ADD **** 02/28/11 *******************************
      *
     C                   ENDSR                                                  MVDBSC
      *===============================================================
      * MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
      *===============================================================
      *
     C     MVSCDB        BEGSR
      *
      ***  BEG ADD ******  09/20/06  *********************************
      ***  BEG ADD ******  01/20/12  *********************************
     C     CLTID         IFEQ      '071'
      ***  END ADD ******  01/20/12  *********************************
     C                   MOVEL     SCGSI#        EMDBA
      ***  BEG ADD ******  01/20/12  *********************************
     C                   ENDIF
      ***  END ADD ******  01/20/12  *********************************
      ***  END ADD ******  09/20/06  *********************************
      ***** BEG ADD ***** 04/15/05 ************************************
     C                   MOVEL     SCLINK        EMLINK
      ***** END CHG ***** 04/15/05 ************************************
      ***** BEG ADD ***** 03/24/06 ************************************
     C     CLTID         IFEQ      '132'
     C                   MOVEL     SCSHFT        EMSHFT
     C                   ENDIF
      ***** END CHG ***** 03/24/06 ************************************
      *
     C*** BEGIN CHG ***  05/14/04  ************************************
     C*********            MOVELSCENAM    EMNAME
     C*********            MOVELSCFDNM    EMFDNM
     C                   MOVEL     SCNAM1        EMNAME
     C                   MOVE(P)   SCNAM1        XXNM10           10
     C                   MOVEL     XXNM10        EMNAM2
     C                   MOVE      SCNAM2        EMNAM2
     C                   MOVEL     SCFDN1        EMFDNM
     C                   MOVE(P)   SCFDN1        XXNM10           10
     C                   MOVEL     XXNM10        EMFDN2
     C                   MOVE      SCFDN2        EMFDN2
     C*** END CHG *****  05/14/04  ************************************
      ***** BEGIN ADD ***** 04/25/06 **********************************
     C                   MOVE      SCNAMP        EMNAMP
     C                   MOVE      SCFDNP        EMFDNP
      ***** END ADD ******* 04/25/06 **********************************
      *
      ***  BEGIN DEL  ***  01/21/98  *********************************
      ********** SCFDNM    IFEQ *BLANKS
      **********           MOVELSCENAM    EMFDNM
      **********           ENDIF                           SCFDNM=*BLANKS
      ***  END   DEL  ***  01/21/98  *********************************
      *
     C                   MOVEL     SCMAD1        EMMAD1
     C                   MOVEL     SCMAD2        EMMAD2
     C                   MOVEL     SCMCTY        EMMCTY
     C                   MOVEL     SCMST         EMMST
     C                   MOVEL     SCMZP1        EMMZIP
     C                   MOVE      SCMZP2        EMMZIP
     C                   MOVEL     SCMPFL        EMMPFL
     C                   MOVEL     SCPAD1        EMPAD1
     C                   MOVEL     SCPAD2        EMPAD2
     C                   MOVEL     SCPCTY        EMPCTY
     C                   MOVEL     SCPST         EMPST
     C                   MOVEL     SCPZP1        EMPZIP
     C                   MOVE      SCPZP2        EMPZIP
     C                   MOVEL     SCIADR        EMIADR
     C*** BEGIN ADD *** 10/05/09 **************************************
     C                   MOVEL(P)  SCFLXV        EMFLXV
     C*** END   ADD *** 10/05/09 **************************************
      ***** BEG DEL ******* 01/31/02 **********************************
     C*********            MOVELSCCONT    EMCONT
     C*********            MOVELSCATTN    EMATTN
     C*********            MOVELSCCNT3    EMCNT3
      ***** END DEL ******* 01/31/02 **********************************
     C                   MOVE      SCAREA        EMAREA
     C                   MOVEL     SCPHN1        EMPHN#
     C                   MOVE      SCPHN2        EMPHN#
     C                   MOVE      SCFAXA        EMFAXA
     C                   MOVEL     SCFAX1        EMFAX#
     C                   MOVE      SCFAX2        EMFAX#
      *
     C                   DO        5             X                 3 0
      *
     C                   Z-ADD     SEF(X)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EEF(X)
      *
     C                   Z-ADD     SCA(X)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ECA(X)
      *
     C                   ENDDO                                                  X->5
     *** BEG ADD *** 10/31/97 *****************************************
     C***  BEGIN CHG  ***  09/25/98  *********************************
     C**********           EXSR SRDIAR
     C  N62              EXSR      SRDIAR
     C***  END   CHG  ***  09/25/98  *********************************
     *** BEG ADD *** 10/31/97 *****************************************
     C*** BEGIN DEL ***  09/16/99  ************************************
     C************         MOVELSCSRPR    EMSRPR
     C*** END DEL *****  09/16/99  ************************************
     C                   MOVEL     SCPSRV        EMPSRV
     C                   MOVEL     SCUWTR        EMUWTR
     C                   MOVEL     SCPACC        EMPACC
     C                   MOVEL     SCARVW        EMARVW
     C                   MOVEL     SCAMGR        EMAMGR
     C                   MOVEL     SCLREP        EMLREP
     C****** BEG DEL ***** 07/07/05 **************************************
     C******************** MOVELSCASTS    EMASTS
     C****** END DEL ***** 07/07/05 **************************************
     C****** BEG DEL ***** 07/07/05 **************************************
     C********************
     C****** END DEL ***** 07/07/05 **************************************
     C                   MOVEL     SCWALS        EMWALS
     C                   MOVEL     SCCNTY        EMCNTY
     C                   MOVEL     SCVIP         EMVIP
     C                   MOVEL     SCRGN#        EMRGN#
     C                   MOVEL     SCSPAC        EMSPAC
     C                   MOVEL     SCCHAP        EMCHAP
      *
     C                   Z-ADD     SCFEIN        EMFEIN
     C                   MOVEL     SCSEMP        EMSEMP
      **** BEGIN CHG **** 06/15/98 *********************************************
      ******************** MOVELSCRSK#    EMRSK#
     C                   MOVEL(P)  SCRSK#        EMRSK#
      **** END   CHG **** 06/15/98 *********************************************
     C                   MOVEL     SCSTRP        EMSTRP
     C                   MOVEL     SCTPOB        EMTPOB
     C                   MOVEL     SCSIC         EMSIC
     C                   MOVEL     SCNAOB        EMNAOB
     C                   MOVEL     SCLSID        EMLSID
     C                   MOVEL     SCEXRT        EMEXRT
     C                   MOVEL     SCOFF#        EMOFF#
     C                   MOVEL     SCCOM1        EMCOM1
     C                   MOVEL     SCCOM2        EMCOM2
     C                   MOVEL     SCCOM3        EMCOM3
      *
     C                   Z-ADD     SCLIDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EMLIDT
      *
     C                   MOVEL     SCINDE        EMINDE
     C*
     C***** BEGIN ADD **** 09/07/99 ***********************************
     C                   MOVEL(P)  SCNAIC        EMNAIC
     C***** END   ADD **** 09/07/99 ***********************************
      ***** BEG ADD ******* 09/20/99 **********************************
     C                   MOVE      SCUE#         EMUE#
      ***** END ADD ******* 09/20/99 **********************************
     C***** BEG ADD ******* 12/26/07 **********************************
     C                   MOVE      SCCUST        EMCUST
     C***** END ADD ******* 12/26/07 **********************************
      *
      ***** BEG ADD ***** 03/28/08 ************************************
     C     CLTID         IFEQ      '114'
     C                   MOVEL(P)  SCDCRX        EMDCRX
     C                   ENDIF
      ***** END ADD ***** 03/28/08 ************************************
      ***  BEG   ADD **** 02/28/11 *********************************
     C                   MOVE      SCEACD        EMEACD
      ***  END   ADD **** 02/28/11 *********************************
      *
     C                   ENDSR                                                  1VSCDB
      *
      *
      *
      *===============================================================
      * OFFCHG - SUBROUTINE FOR PROCESSING CHANGE OF OFFICE
      *===============================================================
      *
     C     OFFCHG        BEGSR
      *
     C     AFKY          CHAIN     PAF01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     OFPREV        IFEQ      EMOFF#
      *
     C                   DELETE    PAF01
      *
     C                   ELSE
      *
     C                   MOVEL     EMOFF#        OFCURR
      *
     C                   MOVEL     Q1USER        OFUSER
     C                   MOVEL     Q1PGM         OFPGM
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         OFDT
     C                   MOVEL     DSTIME        OFTIME
      *
     C                   UPDATE    PAF01
      *
     C                   ENDIF                                                   OFPREV=EMOFF#
      *
     C                   ELSE
      *
     C                   Z-ADD     EMCO#         OFCO#
     C                   Z-ADD     EMFND         OFFND
     C                   Z-ADD     EMEMP#        OFEMP#
     C                   Z-ADD     EMDIV         OFDIV
     C                   MOVEL     XXOFF#        OFPREV
     C                   MOVEL     EMOFF#        OFCURR
      *
     C                   MOVEL     Q1USER        OFIUSR
     C                   MOVEL     Q1USER        OFUSER
      *
     C                   MOVEL     Q1PGM         OFIPGM
     C                   MOVEL     Q1PGM         OFPGM
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         OFIDT
     C                   MOVEL     SRDTO         OFDT
     C                   MOVEL     DSTIME        OFITM
     C                   MOVEL     DSTIME        OFTIME
      *
     C                   WRITE     PAF01
      *
     C                   ENDIF                                                   *IN60=*OFF
      *
     C                   ENDSR                                                   OFFCHG
      *
      *===============================================================
      * OTHERR - SUBROUTINE FOR PROCESSING OTHER MODES
      *===============================================================
      *
     C     OTHERR        BEGSR
      *
     C     *INKC         CASEQ     *ON           FINISH
     C                   ENDCS
      *
     C                   ENDSR                                                   OTHERR
      *
      *===============================================================
      * OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
      *===============================================================
      *
     C     OTHFKS        BEGSR
      *
     C     *INKI         CASEQ     *ON           KIDO                     90
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C     *INKM         CASEQ     *ON           KMDO                     90
     C     *INKN         CASEQ     *ON           KNDO                     90
     C                   ENDCS
      *
     C                   ENDSR                                                  OTHFKS
      *
      *===============================================================
      * PGMEND - SUBROUTINE FOR ENDING THE PROGRAM
      *===============================================================
      *
     C     PGMEND        BEGSR
      *
      *  IF THE OFFICE HAS CHANGED CALL SUBROUTINE TO HANDLE THIS
      *
     C     XXOFF#        IFNE      EMOFF#
     C                   EXSR      OFFCHG
     C                   ENDIF                                                  XXOFF#<>EMOFF#
      *
      *  IN ADD MODE CALL OWNER/OFFICER PROGRAM
      *
     C     @@MODE        IFEQ      'A'
      **** BEGIN ADD **** 04/02/02 ************************************
      *
      *  IN ADD MODE CALL EMPLOYER CONTACTS PROGRAM
      *
     C     *INKC         IFNE      *ON
     C     *INKL         ANDNE     *ON
     C     *IN09         ANDEQ     *OFF
      *
     C                   CALL      'W0077R'                             99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      ' '           ##CALL            1
     C                   PARM                    ##RRN             9 0
     C                   PARM      DBRRN         ##RRN2            9 0
      *
     C                   ENDIF                                                  *INKC<>*ON<>*INKL
      *
      **** END   ADD **** 04/02/02 ************************************
      *
     C     *INKC         IFNE      *ON
     C     *INKL         ANDNE     *ON
     C     *IN09         ANDEQ     *OFF
      *
     C                   CALL      'W1057R'                             99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      ' '           ##CALL            1
     C                   PARM      DBRRN         ##RRN             9 0
     C                   PARM      ' '           ##ELFL            1
      ********* BEGIN ADD ***** 08/07/98 ******************************
     C                   PARM      @@QTE         ##QTE             1
      ********* END   ADD ***** 08/07/98 ******************************
      *
     C                   ENDIF                                                  *INKC<>*ON<>*INKL
      *
     C     *INKC         IFNE      *ON
     C     *INKL         ANDNE     *ON
     C     *IN09         ANDEQ     *OFF
      *
     C                   CALL      'W0051R'                             99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      ' '           ##CALL            1
     C                   PARM                    ##RRN             9 0
     C                   PARM      DBRRN         ##RRN2            9 0
     C                   PARM      @@QTE         ##QTE             1
      *
     C                   ENDIF                                                  *INKC<>*ON<>*INKL
      *
      **** BEGIN ADD **** 11/05/08 *********************************************
     C*
     C                   MOVE      EMCO#         @1CO#
     C                   MOVE      EMFND         @1FND
     C                   MOVE      EMEMP#        @1EMP#
     C                   MOVE      EMDIV         @1DIV
     C                   MOVE      ELFYR         @1FYR
     C*
     C                   CALL      'WA071R'
     C                   PARM                    @1CO#             3
     C                   PARM                    @1FND             3
     C                   PARM                    @1EMP#            9
     C                   PARM                    @1DIV             5
     C                   PARM                    @1FYR             3
      **** END   ADD **** 11/05/08 *********************************************
     C                   ENDIF                                                  @@MODE=A
      *
      *** BEG ADD ***** 04/18/03 **********************************
     C                   CLEAR                   S2FEIN
      *** END ADD ***** 04/18/03 **********************************
     C*** BEG ADD *** 08/02/05 *********************************
     C     @@MODE        IFEQ      'A'
     C*** BEG CHG *** 11/29/05 **********************************
     C*******    FNKY      CHAINPFN01                60
     C     FNKY          CHAIN(N)  PFN01                              60
     C*** END CHG *** 11/29/05 **********************************
     C     FNPEOF        IFEQ      'S'
     C                   MOVE      UPFND         SCFND
     C                   MOVE      *ALL'0'       SCEMP#
     C                   MOVE      1             SCEMP#
     C                   MOVE      *ALL'0'       SCDIV
      ***  BEG   CHG **** 02/28/11 *********************************
     C*****      EMKY      CHAINPEM01                60
     C     EMKY          CHAIN(N)  PEM01                              60
      **** END   CHG **** 02/28/11 ************************************
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           *INKC
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*** END ADD *** 08/02/05 *********************************
      *
      ***** BEG ADD ******* 06/29/07 **********************************
      *
      *  Delete Override for WMEMLQ
      *
     C                   CLOSE     WMEMLQ
      *** BEG CHG ***** 08/26/07 *******************************
     C*****************    MOVELDLTOV1    XXOVRD 50 P
     C                   MOVEL(P)  DLTOV1        XXOVRD           80
      *** END CHG ***** 08/26/07 *******************************
      *
     C     ##LNM         IFNE      *BLANKS
     C                   CALL      'QCMDEXC'
      *** BEG CHG ***** 08/26/07 *******************************
     C*****************    PARM XXOVRD    CMDSTR 50
     C*****************    PARM 20        LENGTH 155
     C                   PARM      XXOVRD        CMDSTR           80
     C                   PARM      80            LENGTH           15 5
      *** END CHG ***** 08/26/07 *******************************
     C                   ENDIF
     C*** begin add ***  11/19/18  ************************************
     C     SCREEN        IFEQ      *ON
     C                   CLOSE     W0050F02
     C                   ENDIF
     C*** end add *****  11/19/18  ************************************
      *
      ***** END ADD ******* 06/29/07 **********************************
      ***** BEG ADD ******* 08/22/05 **********************************
     C                   MOVE      *ON           *INLR
      ***** END ADD ******* 08/22/05 **********************************
     C                   ENDSR                                                  PGMEND
      *
      *===============================================================
      * PGMSTR - SUBROUTINE FOR STARTING THE PROGRAM
      *===============================================================
      *
     C     PGMSTR        BEGSR
      /If Not Defined (WEB)
      /Free
       useDots = *on;
      /End-free
      /Endif
      *
     C*** begin add ***  11/19/18  **************************************
     C                   MOVE      *OFF          SCREEN
     C                   MOVEL     'WISBMQTE'    ##PGMN           10
     C                   CALL      'SRCHKPGM'
     C                   PARM                    ##PGMN           10
     C                   PARM                    ##PGMX            1
     C     ##PGMX        IFEQ      *OFF
     C                   MOVE      *ON           SCREEN            1
     C                   OPEN      W0050F02
     C                   ENDIF
      *
     C*** end add *****  11/19/18  **************************************
      ***** BEG ADD ******* 06/08/01 **********************************
      *
      * Gets the Id for this client
      *
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      *
      ***** END ADD ******* 06/08/01 **********************************
      *** BEG ADD ***** 09/20/06 **************************************
      *
      *   ENABLE GROUP SELF INSURED # ON SCR03 FOR CLT071
      *
     C     CLTID         IFEQ      '071'
     C                   MOVE      *ON           *IN54
     C                   ELSE
     C                   MOVE      *OFF          *IN54
     C                   ENDIF
      *** END ADD ***** 09/20/06 **************************************
      *** BEG ADD ***** 05/21/09 **************************************
     C     CLTID         IFEQ      '102'
     C                   MOVE      *ON           *IN65
     C                   ELSE
     C                   MOVE      *OFF          *IN65
     C                   ENDIF
      *** END ADD ***** 05/21/09 **************************************
      *
      ***** BEGIN ADD ***** 04/25/06 **********************************
      *
      * PRINT PERSONAL NAME FIELDS FOR CA CLIENTS.
      *
     C     CLTID         IFEQ      '132'
     C     CLTID         OREQ      '136'
     C                   MOVE      *ON           *IN17
     C                   ELSE
     C                   MOVE      *OFF          *IN17
     C                   ENDIF
      ***** END ADD ******* 04/25/06 **********************************
      *
      ***** BEG ADD ******* 03/24/06 **********************************
     C     CLTID         IFEQ      '132'
     C                   MOVE      *ON           *IN44
     C                   ELSE
     C                   MOVE      *OFF          *IN44
     C                   ENDIF
      ***** END ADD ******* 03/24/06 **********************************
      *
      *** BEG ADD ***** 03/28/08 **************************************
      *
      *   ENABLE 'DCRx Export exclude' FIELD ON SCR02 FOR CLT114
      *
     C                   MOVE      *OFF          *IN52
     C     CLTID         IFEQ      '114'
     C                   MOVE      *ON           *IN52
     C                   ENDIF
      *** END ADD ***** 03/28/08 **************************************
      *
      ***** BEG ADD ******* 06/08/01 **********************************
      *
      * Check for Daily Batch running
      *
     C     @@MODE        IFEQ      'A'
     C                   CALL      'WA044R'
     C                   PARM      *BLANK        #1RTCD            1
      *
     C     #1RTCD        IFEQ      'F'
     C                   RETURN
     C                   ENDIF                                                  #1RTCD
      *
     C                   ENDIF                                                  @@MODE = A
      *
      ***** END ADD ******* 06/08/01 **********************************
      **** BEGIN ADD **** 05/02/01 *********************************************
      *
      *  Retrieve group/employer # for primary employer record for
      *  processing to retreive state reporting division (see code
      *  added 8/18/00 below)
      *
      ***** BEG CHG ******* 08/22/05 **********************************
     C*********  @@RRN     CHAINDBRCDF               99
     C     @@RRN         CHAIN(N)  DBRCDF                             99
      ***** END CHG ******* 08/22/05 **********************************
     C                   Z-ADD     EMFND         SCFND
     C                   Z-ADD     EMEMP#        SCEMP#
      **** END ADD ****** 05/02/01 *********************************************
      *
     C*** BEGIN ADD ****** 11/18/11 ************************************
     C* # of divisions
     C                   Z-ADD     0             XX#DIV            5 0
     C*** END   ADD ****** 11/18/11 ************************************
      **** BEGIN ADD **** 08/18/00 *********************************************
      *
      *  DETERMINE THE STATE REPORTING DIVISION
      *
     C                   Z-ADD     *ZEROS        SVDIV
     C     EMKY1         CHAIN(N)  PEM01                              60
     C     *IN60         DOWEQ     '0'
     C     EMSTRP        IFEQ      'Y'
     C                   Z-ADD     EMDIV         SVDIV
     C                   ENDIF
     C*** BEGIN ADD ****** 11/18/11 ************************************
     C* # of divisions
     C                   ADD       1             XX#DIV
     C*** END   ADD ****** 11/18/11 ************************************
     C     EMKY1         READE(N)  PEM01                                  60
     C                   ENDDO
      **** END ADD ****** 08/18/00 *********************************************
     C                   CLEAR                   SCFKL1
     C                   Z-ADD     3             #OFSCR            2 0
      *
     C     @@MODE        COMP      '1'                                    09    INQUIRY
     C     @@MODE        COMP      '2'                                    09    INQUIRY
     C     @@MODE        COMP      '3'                                    09    INQUIRY
     C     @@MODE        COMP      'D'                                    09    INQUIRY
      ******** BEGIN ADD ****** 08/07/98 ******************************
     C     @@QTE         COMP      'Q'                                    62    QUOTE
      ******** END   ADD ****** 08/07/98 ******************************
      *
      *** BEGIN ADD *** 06/23/99 ***********************************************
     C     @@MODE        CASEQ     'P'           CPYEMP
     C                   ENDCS
      *** END   ADD *** 06/23/99 ***********************************************
      *
     C                   MOVE      *OFF          *IN41
     C     @@MODE        IFEQ      'V'                                          ADD DIVISION
     C                   MOVEL     'A'           @@MODE
     C                   MOVE      *ON           *IN41
     C                   ENDIF                                                  @@MODE=V
      *
     C     @@MODE        COMP      'A'                                    40    ADD
      *
     C                   MOVEL(P)  OPTNM         ##DATA
      *
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
      *
     C                   CLEAR                   DBRCDF
      *
      **** BEGIN CHG **** 09/05/01 ************************************
     C********** @@RRN     CHAINDBRCDF               99
      ***** BEG DEL ******* 08/22/05 **********************************
     C**N41****  @@RRN     CHAINDBRCDF               99
     C***41****  @@RRN     CHAINDBRCDF              N99
      ***** END DEL ******* 08/22/05 **********************************
      ***** BEG ADD ******* 08/22/05 **********************************
     C     @@RRN         CHAIN(N)  DBRCDF                             99
      ***** END ADD ******* 08/22/05 **********************************
      **** END   CHG **** 09/05/01 ************************************
      *
     C                   Z-ADD     EMFND         SCFND
     C                   Z-ADD     EMEMP#        SCEMP#
      *
     C                   CLEAR                   XXDIV
     C                   CLEAR                   XXNAME
      *
     C                   CALL      'WA005R'
     C***  Begin Chg  ***  05/04/10  **************************************
     C***                  PARM EMEMP#    XXEMP#
     C                   PARM      SCEMP#        XXEMP#
     C***  End   Chg  ***  05/04/10  **************************************
     C                   PARM                    XXDIV
     C                   PARM                    XXNAME
     C                   PARM                    XXEDN            80
      *
     C                   MOVEL(P)  XXEDN         SCEDN
      *
     C     *IN41         IFEQ      *OFF
      *
     C                   Z-ADD     EMDIV         SCDIV
      *
      *  COMPRESS EMP#/DIV/NAME
      *
     C                   CALL      'WA005R'
     C***  Begin Chg  ***  05/04/10  **************************************
     C***                  PARM EMEMP#    XXEMP#
     C                   PARM      SCEMP#        XXEMP#
     C***  End   Chg  ***  05/04/10  **************************************
     C                   PARM      EMDIV         XXDIV
     C                   PARM      EMNAME        XXNAME
     C                   PARM                    XXEDN            80
      *
     C                   MOVEL(P)  XXEDN         SCEDN
      *
      **** BEGIN DEL **** 03/05/98 *********************************************
      ********** *IN40     IFEQ *OFF
      *
      ******************** DO   5         I1      30
      *
      ********** ECA,I1    IFNE *ZERO
      ******************** Z-ADDECA,I1    SRDTI
      ******************** CALL 'SRCVTDAT'DTMDYY
      ******************** MOVELSRDTO     SCCXDT
      ******************** ELSE
      ******************** LEAVE
      ******************** ENDIF                           CX,I1<>*ZERO
      *
      ******************** ENDDO                           I1->5
      *
      ******************** ENDIF                           *IN40=*OFF
      **** END   DEL **** 03/05/98 *********************************************
      **** BEGIN ADD **** 03/05/98 *********************************************
     C                   MOVE      *OFF          *IN69
     C                   Z-ADD     0             SCCXDT
     C                   Z-ADD     1             I1
      *
     C     *ZERO         LOOKUP    EEF(I1)                                61
      *
     C     *IN61         IFEQ      *ON
     C                   SUB       1             I1
     C                   ELSE
     C                   Z-ADD     5             I1
     C                   ENDIF                                                  *IN61 IFEQ *ON
      *
     C     I1            IFNE      0
      *
     C     ECA(I1)       IFNE      *ZERO
     C                   Z-ADD     ECA(I1)       SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCXDT
     C                   MOVE      *ON           *IN69
     C                   ENDIF                                                  ECA,I1 IFNE *ZERO
      *
     C                   ENDIF                                                  I1 IFNE 0
      **** END   ADD **** 03/05/98 *********************************************
      *
     C                   ENDIF                                                  *IN41=*OFF
      *
      *  GET THE USER PARMS
      *
     C     Q1USER        CHAIN     PUP01                              60
      *
     C     *IN40         IFEQ      *ON
     C     *IN41         ANDEQ     *OFF
     C                   Z-ADD     UPFND         SCFND
     C                   ENDIF                                                  *IN40=*ON/*IN41=*OFF
      *
      *  GET GROUP INFORMATION
      *
     C                   CLEAR                   SCNAME
     C     FNKY          CHAIN(N)  PFN01                              60
     C  N60              MOVEL     FNNAME        SCNAME
      *
      ***** BEG ADD *****  01/05/06  *********************************
      * Save the Group PEO Flag for override of FEIN on Add
     C                   MOVE      *BLANKS       XXPEOF
     C  N60              MOVE      FNPEOF        XXPEOF
      ***** END ADD *****  01/05/06  *********************************
      *
      *  FORCE INQUIRY IF SYSTEM DATE IS > GROUP EXPIRATION DATE
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT
      *
     C  N09FNEXDT        IFNE      *ZEROS
     C     SYSDT         ANDGT     FNEXDT
     C     SCFND         OREQ      *ZEROS
      *
     C     @@MODE        IFEQ      'A'
     C                   EXFMT     WDW01
     C  N65              MOVEL     FK1(5)        SCFKL1
     C   65              MOVEL     FK12(5)       SCFKL1
     C                   Z-ADD     1             #OFSCR
      ****** BEG CHG **** 01/27/04 ************************************
      * If there is no user parm for group (GROUP BLANK) then return to
      * the W/W program W0050R.
     C***************      MOVE *ON       *IN90
     C                   RETURN
      ****** END CHG **** 01/27/04 ************************************
     C                   ELSE
     C                   MOVE      'D'           @@MODE
     C                   ENDIF                                                  @@MODE=A
      *
     C                   MOVE      *ON           *IN09
      *
     C                   ENDIF                                                  FNEXDT<>*ZEROS
     C*
      **** BEGIN ADD **** 03/04/02 ************************************
     C     *LOCK         IN        *DTAARA
     C                   OUT       *DTAARA
      *
     C     LDENDA        IFEQ      *ZEROS
     C     LDENDA        OREQ      *BLANKS
      **** END   ADD **** 03/04/02 ************************************
     C***  BEGIN ADD  ***  08/11/99  *********************************
     C                   EXSR      CHKSEC
     C***  END   ADD  ***  08/11/99  *********************************
      **** BEGIN ADD **** 03/04/02 ************************************
     C                   ENDIF                                                              OS
      **** END   ADD **** 03/04/02 ************************************
      *
      *  GET AGENT/PRODUCER INFORMATION
      *
     C                   Z-ADD     *ZEROS        ##FYR
      *
     C                   CALL      AGTAPI        AGTPL
      *
     C                   MOVEL     ##ANAM        SCANAM
     C                   MOVEL     ##PNAM        SCPNAM
      *
      *  GET ADMIN/DARTS INFORMATION & LOAD CONTACT HEADINGS
      *
     C     EXKY          CHAIN     PEX01                              60
      *
     C   60              MOVEL     CNT1          EXCNT1
     C   60              MOVEL     CNT2          EXCNT2
     C   60              MOVEL     CNT3          EXCNT3
     C***** BEG ADD *****  11/06/06  *********************************
     C   60              MOVEL     CNT4          EXCNT4
     C***** end ADD *****  11/06/06  *********************************
      *
     C***** BEG ADD *****  10/07/04  *********************************
      * Save the Admin/DARTS control flags for State and NCCI ID#'s
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EXSRRB        XXSRRB
     C                   MOVE      EXSRRQ        XXSRRQ
     C                   MOVE      EXNRRB        XXNRRB
     C                   MOVE      EXNRRQ        XXNRRQ
     C                   ENDIF
     C***** END ADD *****  10/07/04  *********************************
     C***** BEG ADD *****  10/05/09  *********************************
     C*
     C* CHECK FLEX HEADING
     C*
     C                   SETOFF                                       16
     C     EXFLXH        IFNE      *BLANKS
     C*** Begin Del *** 09/08/10 **************************************
     C********** CLTID     ANDEQ'139'
     C*** End   Del *** 09/08/10 **************************************
     C                   SETON                                        16
     C                   ENDIF
     C***** END ADD *****  10/05/09  *********************************
      *
      *    CONTACT HEADING 1
      *
     C     ' '           CHECKR    EXCNT1        XXLEN             3 0
     C                   DIV       2             XXLEN
     C                   MVR                     XXLEN
      *
     C     XXLEN         IFEQ      *ZERO
     C     EXCNT1        CAT       DOTS:1        SCCHD1
     C                   ELSE
     C     EXCNT1        CAT       DOTS:0        SCCHD1
     C                   ENDIF                                                  XXLEN=*ZERO
      /Free
       Select;
       When not useDots;
          scCHd1 = EXcnt1;
       Endsl;
      /End-free
      *
      *    CONTACT HEADING 2
      *
     C     ' '           CHECKR    EXCNT2        XXLEN
     C                   DIV       2             XXLEN
     C                   MVR                     XXLEN
      *
     C     XXLEN         IFEQ      *ZERO
     C     EXCNT2        CAT       DOTS:1        SCCHD2
     C                   ELSE
     C     EXCNT2        CAT       DOTS:0        SCCHD2
     C                   ENDIF                                                  XXLEN=*ZERO
      /Free
       Select;
       When not useDots;
          scCHd2 = EXcnt2;
       Endsl;
      /End-free
      *
      *    CONTACT HEADING 3
      *
     C     ' '           CHECKR    EXCNT3        XXLEN             3 0
     C                   DIV       2             XXLEN
     C                   MVR                     XXLEN
      *
     C     XXLEN         IFEQ      *ZERO
     C     EXCNT3        CAT       DOTS:1        SCCHD3
     C                   ELSE
     C     EXCNT3        CAT       DOTS:0        SCCHD3
     C                   ENDIF                                                  XXLEN=*ZERO
      /Free
       Select;
       When not useDots;
          scCHd3 = EXcnt3;
       Endsl;
      /End-free
      *
      *    CONTACT HEADING 4
      *
     C     CLTID         IFEQ      '071'
      *
     C     ' '           CHECKR    EXCNT4        XXLEN             3 0
     C                   DIV       2             XXLEN
     C                   MVR                     XXLEN
      *
     C     XXLEN         IFEQ      *ZERO
     C     EXCNT4        CAT       DOTS:1        S1CHD4
     C                   ELSE
     C     EXCNT4        CAT       DOTS:0        S1CHD4
     C                   ENDIF                                                  XXLEN=*ZERO
      /Free
       Select;
       When not useDots;
          s1CHd4 = EXcnt4;
       Endsl;
      /End-free
      *
     C                   MOVEL     SCCHD1        S1CHD1
     C                   MOVEL     SCCHD2        S1CHD2
     C                   MOVEL     SCCHD3        S1CHD3
     C                   ENDIF                                                  CLTID = '071'
      *
      *     WCMC Installed determination
      *
     C                   CALL      'SA001R'
     C                   PARM                    QQWCIF            1
     C                   PARM                    QQFBIF            1
     C                   PARM                    QQFL01            1
      *
     C     QQFBIF        IFEQ      'Y'
     C     QQFL01        ANDEQ     'Y'
     C                   MOVE      *ON           XXMANC
     C                   ELSE
     C                   MOVE      *OFF          XXMANC
     C                   ENDIF
      *
      *  GET SYSTEM VALUES
      *
     C                   MOVEL     'WC'          XXSYK
     C     SYKY          CHAIN     SWC01                              60
     C     WCFL07        COMP      'Y'                                    66
     C     WCSL04        COMP      UPLVL                              67  67
      *
      *  LOAD F KEY LINES FOR SCR01 & SCR02
      *
     C     SCFKL1        IFEQ      *BLANKS
     C  N09
     CANN65
     CANN40              MOVEL     FK1(1)        SCFKL1
     C  N09
     CAN 65
     CANN40              MOVEL     FK12(1)       SCFKL1
     C  N09
     CAN 40
     CANN65
     CANN41              MOVEL     FK1(2)        SCFKL1
     C  N09
     CAN 40
     CAN 65
     CANN41              MOVEL     FK12(2)       SCFKL1
     C  N09
     CAN 40
     CANN65
     CAN 41              MOVEL     FK1(3)        SCFKL1
     C  N09
     CAN 40
     CAN 65
     CAN 41              MOVEL     FK12(3)       SCFKL1
     C  N65
     CAN 09              MOVEL     FK1(4)        SCFKL1
     C   65
     CAN 09              MOVEL     FK12(4)       SCFKL1
     C                   ENDIF                                                  SCFKL1=*BLANKS
      *
      ***** BEG DEL ******* 08/15/00 **********************************
     C**N09N40*****        MOVELFK2,1     SCFKL2
     C**N09*40*****        MOVELFK2,2     SCFKL2
     C***09********        MOVELFK2,3     SCFKL2
      *****BEGIN*ADD *** 08/07/98 *************************************
     C***62********        MOVELFK2,4     SCFKL2
      *****END***ADD *** 08/07/98 *************************************
      ***** END DEL ******* 08/15/00 **********************************
      *
      **** BEGIN ADD *** 12/09/97 *************************************
      *
     C     EXKY          CHAIN     PEX01                              60
      *
     C  N90
     CANN60EXFIFG        IFEQ      'Y'
      ***  BEGIN ADD  ***  06/26/98  *********************************
     C     @@QTE         ANDNE     'Q'
      ***  END   ADD  ***  06/26/98  *********************************
      *
     C                   SETON                                        505190
      *
     C                   ENDIF                                                  EXFIFG IFEQ 'Y'
      *
      **** END   ADD *** 12/09/97 *************************************
      *
      *** BEGIN ADD *** 03/23/98 **************************************
      *
      *  DETERMINE IF CLIENT IS ADMIN-ONLY OR CLAIMS-ONLY
      *
      *  Access System Control file 'WC' record
      *
     C                   MOVEL     'WC'          XSKEY             2
      *
     C     XSKEY         CHAIN     SWC01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     WCFL08        IFNE      *BLANK
      *
     C     WCFL08        IFEQ      'A'
     C                   MOVE      *ON           *IN94                          ADMIN ONLY
     C                   ENDIF                                                  WCFL08=A
      *
     C     WCFL08        IFEQ      'C'
     C                   MOVE      *ON           *IN95                          CLAIMS ONLY
     C                   ENDIF                                                  WCFL08=C
      *
     C                   ENDIF                                                  WCFL08<>*BLANK
      *
     C                   ENDIF                                                  *IN60=*OFF
      *
      *** END   ADD *** 03/23/98 **************************************
      *
      ***** BEG ADD ******* 08/10/98 **********************************
      *
      *  Get Employer Cross Reference info for header
      *
     C     ZEXKY         CHAIN     PEX02                              63
      ***** BEG ADD ******* 06/22/99 **********************************
     C  N63              Z-ADD     EXOFND        SCOFND
      ***** END ADD ******* 06/22/99 **********************************
     C  N63              Z-ADD     EXOEMP        SCOEMP
     C  N63              Z-ADD     EXODIV        SCODIV
      *
      ***** END ADD ******* 08/10/98 **********************************
      ***** BEG ADD ******* 06/29/07 **********************************
      *
      *  Override WMEMLQ to other environment (QUOTE/LIVE)
      *
     C                   MOVEL(P)  '*OTHER'      ##RTVT
      *
     C                   CALL      RTVLIB
     C                   PARM                    ##RTVT           10
     C                   PARM      *BLANKS       ##LNM            10
     C                   PARM      *BLANKS       ##MLNM           10
      *
      *** BEG CHG ***** 08/26/07 *************************************
     C********   OVRDB1    CAT  ##LNM:0   XXOVRD 50 P
     C     OVRDB1        CAT(P)    ##LNM:0       XXOVRD           80
      *** END CHG ***** 08/26/07 *************************************
     C                   CAT(P)    OVRDB2:0      XXOVRD
      *
     C     ##LNM         IFNE      *BLANKS
     C                   CALL      'QCMDEXC'
      *** BEG CHG ***** 08/26/07 *************************************
     C*****************    PARM XXOVRD    CMDSTR 50
     C*****************    PARM 45        LENGTH 155
     C                   PARM      XXOVRD        CMDSTR           80
     C                   PARM      80            LENGTH           15 5
      *** END CHG ***** 08/26/07 *************************************
     C                   ENDIF
      *
     C                   OPEN      WMEMLQ
      *
      ***** END ADD ******* 06/29/07 **********************************
      *
     C                   ENDSR                                                  PGMSTR
      *
      *===============================================================
      * PREOTH - SUBROUTINE FOR OTHER MODE PRE-PROCESSING
      *===============================================================
      *
     C     PREOTH        BEGSR
     C                   ENDSR
     C*
      ***** BEG ADD ******* 03/25/04 **********************************
      *
      *
      *
      *===============================================================
      * SRNBTR - Create New Business Tracking Record
      *===============================================================
      *
     C     SRNBTR        BEGSR
      *
     C                   MOVEL     'WC'          XSKEY             2
     C     XSKEY         CHAIN     SWC01                              60
      *
     C     *IN60         IFEQ      *OFF
     C     WCFL26        ANDEQ     'Y'
      *
     C     NBKY          CHAIN     PNB01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   DELETE    PNB01
     C                   ENDIF
      *
     C                   Z-ADD     EMCO#         NBCO#
     C                   Z-ADD     EMFND         NBFND
     C                   Z-ADD     EMEMP#        NBEMP#
      *
     C                   MOVEL     Q1USER        NBIUSR
     C                   MOVEL     Q1USER        NBUSER
     C                   MOVEL     Q1PGM         NBIPGM
     C                   MOVEL     Q1PGM         NBPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         NBIDT
     C                   MOVEL     SRDTO         NBDT
     C                   MOVEL     DSTIME        NBITM
     C                   MOVEL     DSTIME        NBTIME
      *
     C                   Z-ADD     NBDT          NBENDT
     C                   WRITE     PNB01
     C                   ENDIF                                                  WCFL26
      *
     C                   ENDSR                                                  SRNBTR
      *
      *
      ***** END ADD ******* 03/25/04 **********************************
      *
      *****BEGIN ADD***** 03/20/07 ************************************
     C     SRACLR        BEGSR
     C                   MOVE      EMCO#         ALCO#
     C                   MOVE      EMFND         ALFND
     C                   MOVE      EMEMP#        ALEMP#
     C                   MOVE      EMDIV         ALDIV
      *****BEGIN CHG***** 03/20/07 ************************************
     C***                  MOVE 'L'       ALELTP
     C                   MOVE      FNACLR        ALELTP
      *****END   CHG***** 03/20/07 ************************************
     C                   MOVE      TONE          ALELID
     C                   MOVE      0             ALDLDT
     C                   MOVE      *BLANKS       ALDLRS
     C                   MOVE      EMEFD1        ALEFDT
     C                   MOVE      EMNAME        ALNAME
     C**** BEG ADD *** 04/13/09 ***********************************************
     C                   MOVE      EMNAM2        ALNAM2
     C**** END ADD *** 04/13/09 ***********************************************
     C                   MOVE      *BLANKS       ALDBA
     C                   MOVE      EMMAD1        ALMAD1
     C                   MOVE      EMMAD2        ALMAD2
     C                   MOVE      EMMCTY        ALMCTY
     C                   MOVE      EMMST         ALMST
     C                   MOVE      EMMZIP        ALMZIP
     C                   MOVE      EMPAD1        ALPAD1
     C                   MOVE      EMPAD2        ALPAD2
     C                   MOVE      EMPCTY        ALPCTY
     C                   MOVE      EMPST         ALPST
     C                   MOVE      EMPZIP        ALPZIP
     C                   MOVE      EMTPOB        ALTPOB
     C                   MOVE      EMFEIN        ALFEIN
     C                   MOVE      *BLANKS       ALRMCD
     C                   MOVE      'N'           ALNCCI
     C                   MOVE      EMAREA        ALAREA
     C                   MOVE      EMPHN#        ALPHN#
     C                   MOVE      EMFAXA        ALFARA
     C                   MOVE      EMFAX#        ALFAX#
     C                   MOVE      EMCNTY        ALCNTY
     C                   MOVE      EMFDNM        ALFDNM
     C**** BEG ADD *** 04/13/09 ***********************************************
     C                   MOVE      EMFDN2        ALFDN2
     C**** END ADD *** 04/13/09 ***********************************************
     C                   MOVE      EMSEMP        ALSEMP
     C                   MOVE      *BLANKS       ALGSI#
     C                   MOVE      'A'           ALSTAT
     C                   MOVE      EMUSER        ALUSER
     C                   MOVE      EMPGM         ALPGM
     C                   MOVE      EMTIME        ALTIME
     C                   MOVE      EMDT          ALDT
     C                   MOVE      EMIUSR        ALIUSR
     C                   MOVE      EMIDT         ALIDT
     C                   MOVE      EMIPGM        ALIPGM
     C                   MOVE      EMITIM        ALITM
     C                   MOVE      *BLANKS       ALCNT1
     C                   MOVE      *BLANKS       ALCNT2
     C                   MOVE      *BLANKS       ALCNT3
     C                   MOVE      EMSIC         ALSIC
     C                   MOVE      EMUE#         ALUE#
     C                   MOVE      EMNAOB        ALNAOB
     C                   MOVE      EMSHFT        ALSHFT
     C                   MOVE      EMNAIC        ALNAIC
     C                   MOVE      EMNAMP        ALNAMP
     C                   MOVE      EMFDNP        ALFDNP
     C                   MOVE      *BLANKS       ALSTEM
     C*
     C                   WRITE     PAL02
     C                   ENDSR                                                  SRACLR
      *****END   ADD***** 03/20/07 ************************************
      ***** BEG ADD ******* 01/31/02 **********************************
      *
      *
      *
      *===============================================================
      * SRCONT - Get Employer Contact Names and phone
      *===============================================================
      *
     C     SRCONT        BEGSR
     C                   CLEAR                   XXCNT
     C                   CLEAR                   XXCPH
      *
      * Get Contact Name
      *
     C     ECKY          CHAIN     PEC01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     ECFSN         IFNE      *BLANKS
     C     ECFSN         CAT       ECLSN:1       XXCNT            50
      *
     C                   ELSE
      *
     C     ECTITL        CAT       ECLSN:1       XXCNT
     C                   ENDIF
      *
     C                   SELECT
      *****
      *
      * No Phone entered
      *
     C     ECPHN#        WHENEQ    *ZERO
     C     ECCPHN        ANDEQ     *ZERO
     C                   MOVEL(P)  *BLANKS       XXCPH            20
      *****
      *
      * Cell phone
      *
     C     ECPHN#        WHENEQ    *ZERO
     C     ECCPHN        ANDNE     *ZERO
     C     ECPHN#        ORNE      *ZERO
     C     ECCPHN        ANDNE     *ZERO
     C     ECPPHF        ANDEQ     'C'
      *
     C     ECCARA        IFNE      *ZERO
     C                   MOVEL(P)  ECCARA        XXTMP             5
     C     '('           CAT(P)    XXTMP:0       XXCPH
     C                   CAT(P)    ')':0         XXCPH
     C                   ENDIF
      *
     C                   MOVEL(P)  ECCPHN        XXTMP1            3
     C                   MOVE(P)   ECCPHN        XXTMP2            4
     C                   CAT(P)    XXTMP1:0      XXCPH
     C                   CAT(P)    '-':0         XXCPH
     C                   CAT(P)    XXTMP2:0      XXCPH
      *****
      *
      * Otherwise regular phone
      *
     C                   OTHER
      *
     C     ECAREA        IFNE      *ZERO
     C                   MOVEL(P)  ECAREA        XXTMP             5
     C     '('           CAT(P)    XXTMP:0       XXCPH
     C                   CAT(P)    ')':0         XXCPH
     C                   ENDIF
      *
     C                   MOVEL(P)  ECPHN#        XXTMP1            3
     C                   MOVE(P)   ECPHN#        XXTMP2            4
     C                   CAT(P)    XXTMP1:0      XXCPH
     C                   CAT(P)    '-':0         XXCPH
     C                   CAT(P)    XXTMP2:0      XXCPH
      *
     C     ECEXT#        IFNE      *ZERO
     C                   MOVEL(P)  ECEXT#        XXTMP3            5
     C     '0'           CHECK     XXTMP3        X
     C                   SUBST(P)  XXTMP3:X      XXTMP             5
     C                   CAT(P)    'x':0         XXCPH
     C                   CAT(P)    XXTMP:0       XXCPH
     C                   ENDIF
      *
     C                   ENDSL
      *
     C                   ENDIF                                                  Emp Cont 1
      *
     C                   ENDSR                                                  SRCONT
      *
      *
      ***** END ADD ******* 01/31/02 **********************************
     C***  BEGIN ADD  ***  07/19/99  *********************************
     C*===============================================================
     C* UPDTAB - SUBROUTINE FOR UPDATING DATA BASE RECORDS (WDABP)
     C*===============================================================
     C*
     C     UPDTAB        BEGSR
     C*
     C     ABKY          CHAIN     PAB01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C                   MOVEL(P)  EMCHAP        ABCHAP
     C                   MOVEL(P)  Q1PGM         ABPGM
     C                   MOVEL(P)  Q1USER        ABUSER
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ABDT
     C*
     C                   MOVEL     DSTIME        ABTIME
     C*
     C                   UPDATE    PAB01
     C*
     C     ABKY          READE     PAB01                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  UPDTAB
     C***  END   ADD  ***  07/19/99  *********************************
      *
      *===============================================================
      * UPDATR - SUBROUTINE FOR UPDATING DATA BASE RECORDS
      *===============================================================
      *
     C     UPDATR        BEGSR
      *
      *
     C     @@RRN         CHAIN     DBRCDF                             99
      *
B02  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
      *
      *** BEG ADD ***** 02/03/98 ***********************************************
      *
      *  CHECK FOR REPORTABLE CHANGE AS MID TERM ENDORSEMENT
      *
     C                   EXSR      CKCHGS
      *** END ADD ***** 02/03/98 ***********************************************
      *
     C                   MOVEL     Q1USER        EMUSER
     C                   MOVEL     Q1PGM         EMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EMDT
     C                   MOVEL     DSTIME        EMTIME
      *
     C                   UPDATE    DBRCDF
     C*
     C***  Begin Add  ***  05/23/08  **************************************
     C                   EXSR      CICNTR
     C***  End   Add  ***  05/23/08  **************************************
     C**** BEG   ADD **** 02/28/11 *********************************************
      *
      *  UPDATE EXCLUDE FROM AUTO CASH DISSOLVE FALG FOR ALL DIVISIONS
      *
     C     SCUPDT        IFEQ      'Y'
     C                   EXSR      SRUPDT
     C                   ENDIF
     C**** END   ADD **** 02/28/11 *********************************************
      ***  BEGIN ADD  ***  11/07/97  *********************************
      *
      *  UPDATE QUOTE STATUS RECORD IF NAME OR CHAPTER CHANGED
      *
      **** BEGIN DEL **** 07/15/98 *********************************************
      ******************** MOVELEMNAME    SVNAME    P
      **** END   DEL **** 07/15/98 *********************************************
      **** BEGIN ADD **** 07/15/98 *********************************************
     C     EMNAME        IFNE      SVNAME
     C     EMCHAP        ORNE      SVCHAP
     C                   MOVEL     EMNAME        SVNAM2
      **** END   ADD **** 07/15/98 *********************************************
      *
     C     QTKY          CHAIN     PQT01                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
      **** BEGIN CHG **** 07/15/98 *********************************************
      ********** QTNAME    IFNE SVNAME
     C     QTNAME        IFNE      SVNAM2
      **** END   CHG **** 07/15/98 *********************************************
     C     QTCHAP        ORNE      EMCHAP
      *
     C     DBRRNQ        CHAIN     PQT02                              60
      *
     C                   MOVEL(P)  EMNAME        QTNAME
     C                   MOVEL(P)  EMCHAP        QTCHAP
     C                   MOVE      Q1USER        QTUSER
     C                   MOVE      Q1PGM         QTPGM
     C                   ADD       1             DSTIME
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         QTDT
      *
     C                   Z-ADD     DSTIME        QTTIME
      *
     C                   UPDATE    PQT02
      *
     C                   ENDIF                                                  QTNAME<>SVNAME
      *
     C     QTKY          READE     PQT01                                  60
      *
     C                   ENDDO                                                  *IN60=*OFF
      ***  END   ADD  ***  11/07/97  *********************************
      **** BEGIN ADD **** 07/15/98 *********************************************
     C                   ENDIF                                                  EMNAME IFNE SVNAME
      *
      * IF EMPLOYER NAME HAS CHANGED, UPDATE ALL LOCATION RECORDS THAT HAVE
      * NOT BEEN DELETED AS OF THE CURRENT DATE.
      * NOTE: THE LOGICAL USED (WMALL4) SELECTS ALELTP = 'L'.
      *
     C     EMNAME        IFNE      SVNAME
     C***  BEGIN ADD  ***  09/22/99  *********************************
     C     EMFDNM        ORNE      SVFDNM
     C     EMFEIN        ORNE      SVFEIN
     C***  END   ADD  ***  09/22/99  *********************************
     C***** BEGIN ADD **** 03/10/00 ***********************************
     C     EMTPOB        ORNE      SVTPOB
     C***** END   ADD **** 03/10/00 ***********************************
     C*** BEGIN ADD ***  04/25/06  ************************************
     C     EMNAMP        ORNE      SVNAMP
     C     EMFDNP        ORNE      SVFDNP
     C*** END ADD *****  04/25/06  ************************************
     C*** BEGIN ADD ***  12/27/11  ************************************
     C                   CALL      'WA085R'
     C                   PARM      'E'           XXTYPE            1
     C                   PARM      DBRRN         XXRRN             9 0
     C                   PARM      *BLANK        XXRTN             1
     C*** END ADD *****  12/27/11  ************************************
      *
     C     ALKY          CHAIN     PAL01                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     ALDLDT        IFEQ      0
     C     ALDLDT        ORGT      EMDT
      *
      *** BEG CHG ***** 09/03/08 *************************************
     C********** ALNAME    IFNE EMNAME
     C***  BEGIN ADD  ***  09/22/99  *********************************
     C********** ALFDNM    ORNE EMFDNM
     C********** ALFEIN    ORNE EMFEIN
     C***  END   ADD  ***  09/22/99  *********************************
     C***** BEGIN ADD **** 03/10/00 ***********************************
     C********** ALTPOB    ORNE EMTPOB
     C***** END   ADD **** 03/10/00 ***********************************
     C*** BEGIN ADD ***  04/25/06  ************************************
     C********** ALNAMP    ORNE EMNAMP
     C********** ALFDNP    ORNE EMFDNP
     C*** END ADD *****  04/25/06  ************************************
     C**********
     C**********           MOVE EMNAME    ALNAME
     C***  BEGIN ADD  ***  09/22/99  *********************************
     C**********           MOVELEMFDNM    ALFDNM
     C**********           MOVELEMFEIN    ALFEIN
     C***** BEGIN ADD **** 03/10/00 ***********************************
     C**********           MOVELEMTPOB    ALTPOB
     C***** END   ADD **** 03/10/00 ***********************************
     C*** BEGIN ADD ***  04/25/06  ************************************
     C**********           MOVE EMNAMP    ALNAMP
     C**********           MOVE EMFDNP    ALFDNP
     C*** END ADD *****  04/25/06  ************************************
     C**********           MOVELQ1USER    ALUSER    P
     C**********           MOVELQ1PGM     ALPGM     P
     C**********           ADD  1         DSTIME
     C**********           Z-ADDDSTIME    ALTIME
     C**********
     C**********           Z-ADDDSDATE    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     ALDT
     C***  END   ADD  ***  09/22/99  *********************************
     C**********
     C**********           UPDATPAL01
     C**********
     C**********           ENDIF                           ALNAME IFNE EMNAME
      *
     C     ALFEIN        IFNE      EMFEIN
     C     ALTPOB        ORNE      EMTPOB
     C                   MOVEL     EMFEIN        ALFEIN
     C                   MOVEL     EMTPOB        ALTPOB
     C                   MOVEL(P)  Q1USER        ALUSER
     C                   MOVEL(P)  Q1PGM         ALPGM
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSTIME        ALTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ALDT
     C                   UPDATE    PAL01
     C                   ENDIF                                                  ALFEIN
      *** END CHG ***** 09/03/08 *************************************
     C                   ENDIF                                                  ALDLDT IFEQ 0
      *
     C     ALKY          READE     PAL01                                  60
      *
     C                   ENDDO                                                  *IN60 DOWEQ *OFF
      *
     C                   ENDIF                                                  EMNAME IFNE SVNAME
      **** END   ADD **** 07/15/98 *********************************************
      *
      *    Call API to update Managed Care - FMCMP
      *
     C     XXMANC        IFEQ      *ON
      ***  BEGIN ADD  ***  11/04/97  *********************************
     C     @@QTE         ANDNE     'Q'
      ***  END   ADD  ***  11/04/97  *********************************
     C                   EXSR      MCAPI
     C                   ENDIF                                                  MANC ON
      *
E02  C                   ENDIF                                                  *IN99=*OFF
     C*
     C***  BEGIN ADD  ***  07/19/99  *********************************
     C     SVCHAP        IFNE      EMCHAP
     C                   EXSR      UPDTAB
     C                   ENDIF                                                  SVCHAP<>EMCHAP
     C***  END   ADD  ***  07/19/99  *********************************
      **** BEG ADD ****** 11/23/07 *********************************************
     C     @@QTE         IFNE      'Q'
     C     XXCNDR        IFNE      *BLANKS
     C     EMNAME        IFNE      HNAME
     C     EMNAM2        ORNE      HNAM2
     C     EMFDNM        ORNE      HFDNM
     C     EMFDN2        ORNE      HFDN2
     C                   EXSR      DARTS
     C                   ENDIF                                                  EMNAME  <>
     C                   ENDIF                                                  XXCNDR  <>
     C                   ENDIF                                                  @@QTE   <>
      **** END ADD ****** 11/23/07 *********************************************
      *
     C                   MOVE      *ON           OUTFLG
      *
     C                   ENDSR                                                  UPDATR
      *
      **** BEG ADD ****** 11/23/07 *********************************************
     C*================================================================
     C* DARTS  - WRITE DART REMAARKS
     C*================================================================
     C     DARTS         BEGSR
    C*
     C                   Z-ADD     EMCO#         @#CO#             3 0
     C                   Z-ADD     EMFND         @#FND             3 0
     C                   Z-ADD     EMEMP#        @#EMP#            9 0
     C                   Z-ADD     EMDIV         @#DIV             5 0
     C*
     C     XXCNDR        CHAIN     PCT01                              89
     C     XXCNST        CHAIN     PUP05                              90
     C*
     C                   MOVE      *BLANK        QCQTE
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         ##CDAT
     C*
     C                   MOVEL(P)  CTDESC        ##COMM
     C                   CAT       'STAFF:':5    ##COMM
     C                   CAT       XXCNST:1      ##COMM
     C                   MOVE      'Y'           ##DIFL
     C*
     C     *IN89         IFEQ      *OFF
     C     *IN90         ANDEQ     *OFF
     C*
     C                   CALL      'DA700R'
     C                   PARM                    QCKEY1           40
     C                   PARM                    ##COMM          256
     C                   PARM                    ##CDAT            8
     C                   PARM      *BLANKS       ##DCID           12
     C                   PARM      'W0050R'      ##PNA1           10
     C                   PARM      '000'         ##SECL            3
      ***** BEG CHG ******* 04/21/09 **********************************
     C*********            PARM Q1USER    ##USER 10
     C                   PARM      UPUSR         ##USER           10
      ***** END CHG ******* 04/21/09 **********************************
     C                   PARM      Q1PGM         ##PGM            10
     C                   PARM      *BLANKS       ##FLR            80
     C                   PARM      'Y'           ##DIFL            1
     C                   PARM      Q1PGM         ##IPGM           10
     C                   PARM      *BLANKS       ##FL01            1
     C                   PARM      *BLANKS       ##FL02            1
     C                   PARM      XXCNDR        ##RMCD            5
     C                   PARM      '00000'       ##SEQ#            5
     C                   PARM      'N'           Q#SBYL            1
     C                   PARM      *BLANKS       Q#COM2           19
     C                   PARM      *BLANKS       Q#APID            2
     C                   PARM      *BLANKS       Q#IDOC           12
     C                   PARM      *BLANKS       Q#IQUE           10
     C                   PARM      'O'           Q#IMGF            1
     C                   PARM      *BLANKS       Q#IPTH           10
     C                   PARM      *BLANKS       Q#PIDX            5
     C                   PARM      *BLANK        Q#QPTY            1
     C                   PARM      *BLANKS       Q#DCTP            5
     C                   PARM      *BLANKS       Q#CSID            2
     C                   PARM      *BLANKS       Q#CODE            2
     C*
     C                   ENDIF                                                  *IN90
     C                   ENDSR
      **** END ADD ****** 11/23/07 *********************************************
      *===============================================================
      * WRITER - SUBROUTINE FOR WRITING DATA BASE RECORDS
      *===============================================================
      *
     C     WRITER        BEGSR
      *
     C                   CLEAR                   DBRCDF
      *
     C     *IN09         IFEQ      *OFF
      *
     C                   MOVE      ##CO#         EMCO#
     C                   MOVE      SCFND         EMFND
      *
      *  IF THE EMPLOYER WAS ADDED W/AUTO # GENERATION GET THE #'S
      *
     C     AUTOFL        IFEQ      *ON
      *
     C     *IN41         IFEQ      *ON                                          ADD DIVISION
      ***** BEG DEL ******* 06/29/07 **********************************
      *********
      ***GET*LAST DIVISION AND ADD 1
      *********
     C*********            MOVE *HIVAL    SCDIV
     C*********  EMKY      SETGTPEM01
     C*********  EMKY1     REDPEPEM01                    60
      *********
     C*********  EMDIV     ADD  1         SCDIV
      ***** END DEL ******* 06/29/07 **********************************
      ***** BEG ADD ******* 06/29/07 **********************************
      *
      * GET NEXT AVAILABLE DIVISION
      *
     C                   EXSR      GETDIV
     C                   Z-ADD     NXTDIV        SCDIV
      *
      ***** END ADD ******* 06/29/07 **********************************
      *
     C                   ELSE
      *
      *  GET THE NEXT AVAILABLE EMP# FROM FUND MASTER
      *
     C     FNKY          CHAIN     PFN01                              60
      *
     C     *IN60         IFEQ      *OFF
      ****** Beg Add ******* 04/26/12 ********************************
     C                   MOVE      *OFF          *IN57
     C     *IN57         DOWEQ     *OFF
      ****** End Add ******* 04/26/12 ********************************
      *
     C     @@QTE         IFNE      'Q'
      *
     C     FNNAEN        IFEQ      *ZERO
     C                   Z-ADD     1             FNNAEN
     C                   ENDIF                                                  FNNAEN=*ZERO
      *
     C                   Z-ADD     FNNAEN        SCEMP#
     C                   ADD       1             FNNAEN
      *
     C                   ELSE
      *
     C     FNNAQN        IFEQ      *ZERO
     C                   Z-ADD     1             FNNAQN
     C                   ENDIF                                                  FNNAQN=*ZERO
      *
     C                   Z-ADD     FNNAQN        SCEMP#
     C                   ADD       1             FNNAQN
      *
     C                   ENDIF                                                  @@QTE<>Q
     C*
      ****** Beg Add ******* 04/26/12 ********************************
     C     EMKY          CHAIN     PEM01                              57
     C                   ENDDO
      ****** End Add ******* 04/26/12 ********************************
     C***  BEGIN ADD  ***  07/23/99  *********************************
     C                   MOVEL     Q1USER        FNUSER
     C                   MOVEL     Q1PGM         FNPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FNDT
     C*
     C                   MOVEL     DSTIME        FNTIME
     C***  END   ADD  ***  07/23/99  *********************************
      *
     C                   UPDATE    PFN01
      *
     C                   ENDIF                                                  *IN60=*OFF
      *
     C     FNDIV0        IFEQ      'Y'
     C                   Z-ADD     *ZEROS        SCDIV
     C                   ELSE
     C                   Z-ADD     1             SCDIV
     C                   ENDIF                                                  FNDIV0=Y
      *
     C                   ENDIF                                                  *IN41=*ON
      *
     C                   ENDIF                                                  AUTOFL=*ON
      *
     C                   MOVE      SCEMP#        EMEMP#
     C                   MOVE      SCDIV         EMDIV
      *
     C                   EXSR      MVSCDB
      *
      *** BEG ADD ***** 02/03/98 ***********************************************
      *
      *  CHECK FOR REPORTABLE CHANGE AS MID TERM ENDORSEMENT
      *
     C                   EXSR      CKCHGS
      *** END ADD ***** 02/03/98 ***********************************************
      *
     C                   MOVEL     Q1USER        EMIUSR
     C                   MOVEL     Q1USER        EMUSER
     C                   MOVEL     Q1PGM         EMIPGM
     C                   MOVEL     Q1PGM         EMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EMIDT
     C                   MOVEL     SRDTO         EMDT
     C                   MOVEL     DSTIME        EMITIM
     C                   MOVEL     DSTIME        EMTIME
      *
      ***** BEG ADD ******* 03/06/06 **********************************
     C                   MOVEL     *BLANKS       EMRDFL
      *
      * If adding a division? set the EMRDFL = 'Y' to signal that this
      * is a division added through Quote and not the state reporting
      *
     C     @@QTE         IFEQ      'Q'
     C     *IN41         ANDEQ     *ON
     C                   MOVEL     'Y'           EMRDFL
     C                   ENDIF
      *
      ***** END ADD ******* 03/06/06 **********************************
      *
     C                   WRITE     DBRCDF
      ***** BEG ADD ******* 03/25/04 **********************************
      *
      *    Create New Business Tracking Record
      *
     C                   EXSR      SRNBTR
      *
      ***** END ADD ******* 03/25/04 **********************************
      *
      *****BEGIN ADD***** 08/26/09 ************************************
     C     FNACLR        IFEQ      'Y'
     C                   MOVEL     'L'           FNACLR
     C                   ENDIF
      *****END   ADD***** 08/26/09 ************************************
      *****BEGIN ADD***** 03/20/07 ************************************
      *****BEGIN CHG***** 08/26/09 ************************************
     C***        FNACLR    IFEQ 'Y'
     C     FNACLR        IFEQ      'E'
     C     FNACLR        OREQ      'L'
      *****END   CHG***** 08/26/09 ************************************
     C                   EXSR      SRACLR
     C                   ENDIF
      *****END   ADD***** 03/20/07 ************************************
      *
      *
      *    Call API to update Managed Care - FMCMP
      *
     C     XXMANC        IFEQ      *ON
      ***  BEGIN ADD  ***  11/04/97  *********************************
     C     @@QTE         ANDNE     'Q'
      ***  END   ADD  ***  11/04/97  *********************************
     C                   EXSR      MCAPI
     C                   ENDIF                                                  MANC ON
      *
     C                   FEOD      WMEMP
      *
     C     AUTOFL        IFEQ      *ON
     C                   EXFMT     WDWEMP#A
     C                   ENDIF                                                  AUTOFL=*ON
      *
     C                   ENDIF                                                  *IN09=*OFF
      *
     C                   MOVE      *ON           OUTFLG
      *
     C                   ENDSR                                                  WRITER
     *** BEG ADD *** 10/31/97 ****************************************
      *===============================================================
      * SUBROUTIN SRDIAR- EMPLOYER STAFF CODE DIARY TRANSFER
      *===============================================================
     C     SRDIAR        BEGSR
      *
     C                   MOVE      EMCO#         DTCO#
     C                   MOVE      EMFND         DTFND
     C                   MOVE      Q1USER        DTUSER
     C                   MOVE      Q1PGM         DTPGM
     C                   MOVE      EMEMP#        DTEMP#
     C                   MOVE      EMDIV         DTDIV
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DTTIME
      *
      *  CONVERT THE DATE TO CENTURY/YEAR/MONTH/DAY
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DTDT
      *
     C     1             DO        6             D                 2 0
      *
      *  IS THE STAFF CODE ENTERED DIFFERENT FROM THE OLD
      *
     C     STF(D)        IFNE      OSTF(D)
     C     STF(D)        ANDNE     *BLANKS
     C     OSTF(D)       ANDNE     *BLANKS
      *
     C                   MOVE      STF(D)        DTNSTF
     C                   MOVE      OSTF(D)       DTOSTF
     C                   MOVE      CTGY(D)       DTCTGY
     C                   WRITE     PDT01
      *
     C                   ENDIF                                                  SFT,N NE OSTF,N
      *
     C                   ENDDO                                                              F,N
      *
     C                   ENDSR
      *================================================================
     *** END ADD *** 10/31/97 ****************************************
      ***** BEG ADD ******* 06/29/07 **********************************
      *
      *
      *===============================================================
      * GETDIV - Get next available division number for employer
      *===============================================================
     C     GETDIV        BEGSR
     C                   Z-ADD     *ZEROS        NXTDIV            5 0
      *
      * CURRENT ENVIRONMENT
      *
     C     EMKY1         SETGT     PEM01
     C*** BEGIN CHG ****** 12/26/11 ************************************
     C******     EMKY1     REDPEPEM01                    60
     C     EMKY1         READPE(N) PEM01                                  60
     C*** END   CHG ****** 12/26/11 ************************************
      *
     C     *IN60         IFEQ      *ON
      *
     C     FNDIV0        IFEQ      'Y'
     C                   Z-ADD     *ZEROS        NXTDIV            5 0
     C                   ELSE
     C                   Z-ADD     1             NXTDIV
     C                   ENDIF                                                  FNDIV0=Y
      *
     C                   ELSE
      *
     C     EMDIV         ADD       1             NXTDIV
     C                   ENDIF
      *
      *  OTHER ENVIRONMENT
      *
     C     ##LNM         IFNE      *BLANKS
     C     EMKY1         SETGT     PEMOTH
     C     EMKY1         READPE    PEMOTH                                 60
      *
     C     *IN60         IFEQ      *OFF
     C     EMDIV         ANDGE     NXTDIV
     C     EMDIV         ADD       1             NXTDIV
     C                   ENDIF
      *
     C                   ENDIF
      *
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C                   ENDSR                                                  GETDIV
      *
      ***** END ADD ******* 06/29/07 **********************************
      ***  BEG ADD ******  01/04/10  *********************************
      *===============================================================
      * FLXHDG - "State Risk ID #" heading on SCR03 will be soft-coded
      *          " If file field WDRRP/RRSEMH is blank then display "State Risk
      *          ID #" as heading.
      *===============================================================
     C     FLXHDG        BEGSR
     C     SCPST         IFNE      *BLANKS
     C                   MOVE      SCPST         ##RRST
     C                   ELSE
     C                   MOVE      SCMST         ##RRST
     C                   ENDIF
     C*
     C     RRKY          CHAIN     PRR01                              60
     C     *IN60         IFEQ      *OFF
     C     RRSEMH        IFNE      ' '
     C*
     C     ' '           CHECKR    RRSEMH        XXLEN             3 0
     C                   DIV       2             XXLEN
     C                   MVR                     XXLEN
     C*
     C     XXLEN         IFEQ      *ZERO
     C     RRSEMH        CAT       DOTS:1        SCSEMH
     C                   ELSE
     C     RRSEMH        CAT       DOTS:0        SCSEMH
     C                   ENDIF                                                  XXLEN=*ZERO
      /Free
       Select;
       When not useDots;
          scSemH = RRsemH;
       Endsl;
      /End-free
     C*
     C                   MOVE      *OFF          *IN44
     C*
     C                   ELSE
     C*
     C                   MOVE      *ON           *IN44
     C*
     C                   ENDIF                                                  RRSEMH=' '
     C                   ENDIF                                                  IN60 = *OFF
     C                   ENDSR
      ***  END ADD ******  01/04/10  *********************************
     C***  BEG   ADD **** 02/28/11 ************************************
     C*===============================================================
     C* SRUPDT - Loop thru all records for current employer in
     C*          employer master and update new flag.
     C*===============================================================
     C     SRUPDT        BEGSR
     C*
     C     SCUPDT        IFEQ      'Y'
     C*
     C     EMKY1         CHAIN     PEM01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C                   MOVEL     Q1USER        EMUSER
     C                   MOVEL     Q1PGM         EMPGM
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EMDT
     C                   MOVEL     DSTIME        EMTIME
     C*
     C                   MOVE      SCEACD        EMEACD
     C                   UPDATE    PEM01
     C*
     C     EMKY1         READE     PEM01                                  60
     C*
     C                   ENDDO
     C* TO RESET POINTER
     C     EMKY          CHAIN(N)  PEM01                              60
     C                   ENDIF
     C*
     C                   ENDSR
      ***  END   ADD **** 02/28/11 ************************************
**  FK1
F3=Exit   F5=Refresh   F12=Cancel   F14=DARTS
F3=Exit   F12=Cancel   F13=Change user parms
F3=Exit   F12=Cancel
F3=Exit   F12=Cancel   F14=DARTS
F3=Exit   F12=Cancel   F13=Change user parms
**  FK12
F3=Exit   F5=Refresh   F10=Search   F12=Cancel   F14=DARTS
F3=Exit   F10=Search   F12=Cancel   F13=Change user parms
F3=Exit   F10=Search   F12=Cancel
F3=Exit   F10=Search   F12=Cancel   F14=DARTS
F3=Exit   F10=Search   F12=Cancel   F13=Change user parms
**  STAF
SCPSRV
SCUWTR
SCPACC
SCARVW
SCAMGR
SCLREP
** CTG
PWTRML
