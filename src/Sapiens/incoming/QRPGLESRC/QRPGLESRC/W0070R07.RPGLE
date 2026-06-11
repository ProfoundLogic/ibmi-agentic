     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H*****************************************************************
     H/TITLE W0070R07 - CLAIMANT / EMPLOYMENT INFORMATION
     H*****************************************************************
     H*                                                               *
     H*  DESCRIPTION:  THIS PROGRAM WILL DISPLAY CLAIMANT INFORMATION.*
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0070R - CLAIM MASTER SEARCH                               *
     H*                                                               *
     H*  CALLS TO:  FA002R - API to update MC demographics FMCDP      *
     H*             FA003R - API to update MC claim master FMCMP      *
     H*             SA001R - API to determine if MC installed         *
     H*                                                               *
     H*  CREATED: 01/20/97  PROGRAMMER: C:47  PROJECT: 60ANC          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  05/25/12 C:282 Project: WC2109                               *
     H*                 Added Claim Contacts email field              *
     H*                                                               *
     H*  01/09/12 C:189  SUPPORT: 78849                               *
     H*                  Indicator 85 added under 133WKZ was already  *
     H*                  being used on SCR 02 and created edit issue  *
     H*                                                               *
     H*  12/27/11 C:282 Project: WC2005                               *
     H*                Add API WA085R to change EFT payee name/FEIN   *
     H*
     H*  11/14/11 C:062  PROJECT: WC133 78497                         *
     H*                  Correct call of CALLR.                       *
     H*
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
     H*  09/27/11 C:278  Support WA099 77969                          *
     H*                  Added a fix to make the age blank if no DOB  *
     H*                  is entered.                                  *
     H*
     H*  06/28/11 C: 53 Project: SY255
     H*                 -Correct CMS flag
     H*                                                               *
     H*  7/14/11  C:189  SUPPORT:WC133 77309/77489                    *
     H*                  The reason window should be displayed if the *
     H*                  2nd line  of address is changed which was    *
     H*                  previously blank.                            *
     H*                                                               *
     H*  3/29/11  C: 58  Project: 133WKZ                              *
     H*                  Require DOH, DOB and or GNDR based on new    *
     H*                  WMCXP flags.                                 *
     H*                                                               *
     H* 02/02/11 C:235 PROJECT 114WXO                                 *
     H*                Claim Dept. Letter to Gather CMS Info.         *
     H*                                                               *
     H* 10/05/10 C:251 PROJECT WC1944                                 *
     H*                CMS Medicare Reporting 3.1                     *
     H*                -Added soft Edit (WDW06) for fields :Date of   *
     H*                 Birth and Gender entry                        *
     H*                                                               *
     H* 08/04/10 C:235 PROJECT: 139WNF Priss#32                       *
     H*                                                               *
     H* 05/17/10 C:235 PROJECT 139WNF  Additional Changes             *
     H*                All New field and function keys will be        *
     H*                qualified on the WCMS EDI install flag='Y'     *
     H*                                                               *
     H* 05/03/10 C:190 PROJECT WC1859                                 *
     H*                Add the field "Late Reason Code".              *
     H*                                                               *
     H* 03/23/10 C:235 PROJECT 139WNF                                 *
     H*                EDI SCREEN DESIGN                              *
     H*                                                               *
     F* 12/21/09 C:180 SUPPORT 71105
     F*                Client-flag edit for occupation description.
     F*
     H* 10/22/09 C:189 SEQ#70040                                      *
     H*                Additional screens being displayed which       *
     H*                has not been set up in Group set up.           *
     H*                                                               *
     H* 08/28/09 C:189 Project: 133WHE                                *
     H*                Call the appropriate state program so as to    *
     H*                inset client specific screen between the       *
     H*                Claimant Info and Accident Info screens.       *
     H*                                                               *
     H* 07/14/09 C:189 Project: 114WOJ                                *
     H*                  Change of Address Tracking/Force new Darts   *
     H*                  Remark                                       *
     H*                                                               *
     H* 04/20/09 C:190 PROJECT WC1473                                 *
     H*                Add Class Extension field
     H*                                                               *
     H*                                                               *
     F* 04/06/09 C:189 SEQ 67952                                      *
     F*                Do not allow periods in First and Last name    *
     H*                                                               *
     F* 03/23/09 C:220 SEQ#66901                                      *
     F*                Do not allow special chars in Occupation       *
     F*                description                                    *
     F*                                                               *
     H* 02/04/09 C:190 SEQ#66834                                      *
     H*                Display description for 'Type of Coverage'.    *
     H*                                                               *
     H* 02/27/08 C:180 PROJECT 65WGI - PRISS # 6                      *
     H*                Addt'l client-flag changes for 65WGI
     H*
     H* 02/20/08 C:180 PROJECT 65WGI                                  *
     H*                CLIENT-FLAG CHANGES FOR 65WGI                  *
     H*
     H* 02/18/08 C:180 SUPPORT 62389, 62390                           *
     H*                DISPLAY FILE CHANGES ONLY.
     H*                                                               *
     H* 09/10/07 C:65  PROJECT: SUPP#59990                            *
     H*                DO NOT USE A SETLL FOR CHECKING S/R PAYROLLS   *
     H*                AND VERIFY DATE VS RPMY OR TODT AS NEEDED.     *
     H*                                                               *
     H* 08/20/07 C:193 PROJECT: 65WGI                                 *
     H*                EDI Rel 3.0 Impl/Setup/Training                *
     H*                                                               *
     H* 07/25/07 C:62  PROJECT: WC1177                                *
     H*                ADD F8=WITHNESS INFO                           *
     H*                                                               *
     H* 06/04/07 C:079 PROJECT: FL061                                 *
     H*                Add policy state parm to call of FA003R        *
     H*                                                               *
     H* 05/25/07 C:191 PROJECT: WC1515
     H*                Update claimant name in WMCUP
     H*
     H* 05/16/07 C:183 PROJECT: 114WOC                                *
     H*                Address History records to be added to WDAHP.  *
     H*                In case of a 'WRITE' to WMCDP a new record is  *
     H*                added to WDAHP and in case of an 'UPDATE' to   *
     H*                WMCDP 2 records are added, both the before and *
     H*                after image.                                   *
     H*
     H* 11/01/06 C:53  PROJECT: 114WMF   114WBY 05/15/01
     H*                Back to base - Block User Access to Claims
     H*              - Require entry of SCCTG2 if Group = 350, 400    *
     H*                410, 420, 430, 440, 450, 460, 385.             *
     H*              - Add search on SCCTG2.                          *
     H*                                                               *
     H* 06/01/06 C:65  PROJECT WC945                                  *
     H*                CHAIN FOR PAYROLL EXTENSION DESCRIPTION USING  *
     H*                EFFECTIVE DATE.                                *
     H*                                                               *
     H* 01/24/06 C:168 Support: 132WBU                                *
     H*              - If "Date reported to employer" is changed and  *
     H*                "Delayed flag" = Y, then "Decision Date" =     *
     H*                "Date reported" + 90 days                      *
     H*              - It will also add a Diary remark for this adjustr
     H*              - Change the code to get the *date to use the    *
     H*                *time instead to use correct date if they don't*
     H*                log out at night                               *
     H*                                                               *
     H* 05/09/05 C:53  Support: 46938                                 *
     H*                Rmv edit on County Code in.                    *
     H*                                                               *
     H* 03/17/05 C:132 WC1118                                         *
     H*                Add new edits to County Code in Chg mode.      *
     H*                                                               *
     H* 02/23/05 C:58  WC1128                                         *
     H*                Add call to WA700R if N & A change.
     H*                                                               *
     H* 01/05/05 C:15  Support: WC129/45312                           *
     H*                Added code to change request pmts for other    *
     H*                claims with the same SS#, if the claimant's    *
     H*                name or address changes.                       *
     H*                                                               *
     H* 08/27/04 C:132 PROJECT WC1118                                 *
     H*                Auto Assign County Code in Change Mode if      *
     H*                County Code in Employer is Blank.              *
     H*                                                               *
     H* 06/23/04 C:15  Support:  WC114/42983                          *
     H*                Added code to clear XTYPE1 and XTYPE2 to       *
     H*                prevent erroneous records from being written   *
     H*                to WDNTP.                                      *
     H*                                                               *
     H* 04/26/04 C:136 PROJECT: WC1169                                *
     H*                Do not allow an occupational disease payroll   *
     H*                class to be selected.                          *
     H*                                                               *
     H* 02/09/04 C:15  PROJECT: WC1159                                *
     H*                Chgd payroll edit to allow users with valid    *
     H*                security to override payroll edits, based on   *
     H*                coverage edit security levels.                 *
     H*                                                               *
     H* 10/20/03 C:53  Project: WC909                                 *
     H*                Added country info.                            *
     H*                                                               *
     H* 10/14/03 C:136 PROJECT: WC1159                                *
     H*                Add edit to prevent adding claim to payroll    *
     H*                class with a zero amount.                      *
     H*                                                               *
     H* 04/17/03 C:53  PROJECT: WC577                                 *
     H*                CHANGE TO METHOD OF CONTACT IN DSPF            *
     H*                                                               *
     H* 02/28/03 C:132 PROJECT WC1118                                 *
     H*                In Add Mode, check ZIPCODE BY COUNTY file for  *
     H*                valid county code, and load into SCCCCD on the *
     H*                claimant screen. THIS IS A NEW FILE - WMCZP/L  *
     H*                                                               *
     H* 05/01/02 C:15  Support: WC065/32304                           *
     H*                Chgd key used to retrieve the class code desc. *
     H*                                                               *
     H* 01/09/02 C:15  Support: WC090/29015                           *
     H*                Added code to send a msg if non payroll amount *
     H*                exists on the entered class code.              *
     H*                                                               *
     H* 12/06/00 C:15  Support: WC094/25282                           *
     H*                Added code to prevent the user from being able *
     H*                to do a payroll search if there is not an Emp  *
     H*                Detail record for the claim's FYR.             *
     H*                                                               *
     H*  08/25/00 C:062 PROJECT: MC232                                *
     H*                 ADD JURISDICTION STATE TO PARM LIST OF FA003R.*
     H*                                                               *
     H* 08/15/00 C:53  PROJECT: WC577
     H*                Swap out wtscl for the generic table file
     H*                                                               *
     H* 07/07/00 C:15  Project: WC913                                 *
     H*                Added table backup for 'Method of contact'.    *
     H*                Table ID: MCON                                 *
     H*                                                               *
     H* 05/14/00 C:53  PROJECT: WC998                                 *
     H*                Added edit for the payroll class code to not   *
     H*                be the aircraft surcharge code.                *
     H*                                                               *
     H* 04/27/00 C:53  SUPPORT: WC065/19768
     H*                SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
     H*                                                               *
     H*  04/26/00 C:31  Project: DR097
     H*                 Chg ALL PDS pgms
     H*                 1. Replace write to SDCMP with API DA700R
     H*
     H* 04/03/00 C:118 PROJECT: WC854                                 *
     H*                Added parm to call of FA003R to pass WMCMP/    *
     H*                CMIMP.                                         *
     H*                                                               *
     H* 11/09/99 C:58  Support: 20551 (WC090)                         *
     H*               Correct zip code in Payment Request update.
     H*               Make name rearrangement same as request create
     H*               program.
     H*                                                               *
     H* 10/06/99 C:90 PROJECT: WC945                                  *
     H*                FIND VALID EXTENSION WITH STATE EQUAL TO BLANK *
     H*                IF NOT VALID, THEN FIND VALID EXT WITH STATE   *
     H*                EQUAL TO THE RATING STATE.                     *
     H*                                                               *
     H* 09/16/99 C:61  PROJECT WC577                                  *
     H*                Add employer cancel date to header info.       *
     H*                                                               *
     H* 09/02/99 C:112 Support: 19256 (MC048)                         *
     H*                Change QMSCL# from 10A to 25A                  *
     H*                                                               *
     H* 08/04/99 C:58 Project: WC855                                  *
     H*               Change name and/or address in Payment Request
     H*               file (WARPLC) also.
     H*                                                               *
     H* 07/01/99 C:118 SUPPORT: DR095/13079,DR048/13846,DR087/16339   *
     H*                              DR048/17294                      *
     H*                Added FEOD SDCM* to WRITEPCM0* so DARTS updates*
     H*                immediately.                                   *
     H*
     H*  06/22/99 C:61 PROJECT 48WQJ                                  *
     H*                Add previous fund number to header info.       *
     H*                                                               *
     H* 11/20/98 C:15 Support: WC060/15073                            *
     H*               Change pgm to process claimant name change for  *
     H*               other claims with the same SS# in ADD mode as   *
     H*               well as UPDATE mode.                            *
     H*                                                               *
     H* 11/16/98 C:15 Project: WC577                                  *
     H*               Changed the bar code process.  A bar code API   *
     H*               is now called to return certain parameters.     *
     H*                                                               *
     H* 08/27/98 C:65 Project: SUPPORT                                *
     H*               MOVED CODE C:61 ADDED ON 8/10/98, IT WAS ADDED  *
     H*               BETWEEN A DATA FILE AND ITS DATA STRUCTURE.     *
     H*                                                               *
     H* 08/25/98 C:15 Support: WC021/#13429                           *
     H*               Changed pgm to release a lock on claim record   *
     H*               when the claimant name is changed.              *
     H*                                                               *
     H* 08/12/98 C:65 Project: WC843                                  *
     H*               USE EXTENSION DESCRIPTION IF SPECIFIED.         *
     H*                                                               *
     H* 08/11/98 C:62 Project: MC155                                  *
     H*               ADDED PARM FOR CMFEAD TO FA003R                 *
     H*                                                               *
     H* 08/10/98 C:61 Project: 48WLC                                  *
     H*               Display old Emp#/Div from Employer Cross        *
     H*               Reference (WZEXP).                              *
     H*                                                               *
     H* 08/04/98 C:15 Project: WC577                                  *
     H*               Fixed the claim record lockout routine.         *
     H*                                                               *
     H* 05/04/98 C:77 Project: WC577                                  *
     H*               Research time usage-  Changed to use information*
     H*               from correct claims master file.                *
     H*                                                               *
     H* 04/29/98 C:15 SEQ#: WC095/11098                               *
     H*               Fixed the claim master time usage.              *
     H*                                                               *
     H* 04/23/98 C:33 SEQ#: WC078/11268                               *
     H*               RECOMILED DUE TO ERROR MESSAGE WC31299 BEING    *
     H*               ADDED TO SCHIDT IN THE SCREEN FILE              *
     H*                                                               *
     H* 04/22/98 C:15 Project: MC155                                  *
     H*               Changed the display file to protect the claimant*
     H*               address if UPFL2 = 'N'.                         *
     H*                                                               *
     H* 04/07/98 C:62 Project: MC155                                  *
     H*               Added additional fields to FA003R parm list.    *
     H*                                                               *
     H* 03/25/98 C:15 Project: WC577                                  *
     H*               Changed pgm to update all claims with the same  *
     H*               SS# if the claimant name is changed.            *
     H*                                                               *
     H* 02/23/98 C:85 Project: WC577                                  *
     H*               CHANGED THE FIELD MOVED TO CMSMCD               *
     H*               FROM *BLANKS TO CTSMCD.                         *
     H*                                                               *
     H* 02/20/98 C:47 Project: WC577                                  *
     H*               Fixed the loading of the payroll class code     *
     H*               desc.  Removed emp eff date override process.   *
     H*                                                               *
     H* 02/10/98 C:47 Project: WC577                                  *
     H*               THE WRITE TO THE CLAIM HISTORY FILE HAS BEEN    *
     H*               REMOVED AND ADDED TO W0070R01.                  *
     H*                                                               *
     H* 11/05/97 C:15 Project: WC729                                  *
     H*               Changed the format of date fields used in the   *
     H*               parm lists for pgms FA002R and FA003R from      *
     H*               MDCY to CYMD.                                   *
     H*                                                               *
     H* 05/15/97 C:62 PROJECT WC640                                   *
     H*               "CAT1" TO "LOCATION"; "CAT2" = CXCAT2;          *
     H*               "ADJUSTER" TO CXADJ.                            *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  07 - EDI INSTALLED                                           *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H*  18 - INVALID CURSOR POSITION FOR SEARCH                      *
     H*  40 - ADD MODE                                                *
     H* N40 - UPDATE MODE                                             *
     H*  61 - Chain/READE WARPLC
     H*  63 - Old Emp/Div displayed in Header info                    *
     H*  64 - CLAIM WAS CONVERTED FROM ANOTHER SYSTEM                 *
     H*  66 - COMMERCIAL APPLICATION INSTALLED                        *
     H*  67 - SECURITY LEVEL VALID                                    *
     H* N67 - SECURITY LEVEL INVALID                                  *
     H*  69 - EXCESS REINSURANCE                                      *
     H*  70 - BAR CODE INSTALLED                                      *
     H*                                                               *
     H*  SCR01   ==partial==                                          *
     H*  83 - DTOB WCR0502 cannot be < or = 0                         *
     H*  84 - DTOB WCR0502 cannot be < or = 0                         *
     H*                                                               *
     H*  SCR02   ==partial==                                          *
     H*  85 - HIDT WCR0502 cannot be < or = 0                         *
     H*                                                               *
     H* ***************************************************************
     F*
     FW0070F07  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMCMP     UF   E             DISK
     F                                     RENAME(PCM01:DBRCDF)
     F                                     INFDS(DBFDS)
     FWMEML     IF   E           K DISK
     F                                     INFDS(DBFDS4)
     FWMFNL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMCDL     UF A E           K DISK
      *
     FWMPXL1    IF   E           K DISK
      *
     FWMPXP     IF   E             DISK
     F                                     RENAME(PPX01:PPX02)
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PSY01)
     F                                     IGNORE(PTR01)
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
      *  EMPLOYER PAYROLL
     FWMEPL7E   IF   E           K DISK
     FWMEPP     IF   E             DISK
     F                                     RENAME(PEP01:PEP0P)
      *  EMPLOYER DETAIL
     FWDELL     IF   E           K DISK
      *
      *  Employer Cross Reference
     F                                     INFDS(DBFDS2)
     FWZEXL     IF   E           K DISK
      *  RATE SET CODE FILE
     FWMRSL     IF   E           K DISK
      *
      *  PAYROLL CLASS CODE FILE
     FWMPRL1    IF   E           K DISK
      *  EMPLOYER SELF REPORTING TRANS FILE
     FWASRL4    IF   E           K DISK
      *  COUNTY CODE FILE
     FWTCNP     IF   E             DISK
     F                                     INFDS(DBFDS3)
     F                                     RENAME(PCN01:PCN0P)
      *  WORKERS COMP DEPARTMENT MASTER
     FWMDPL     IF   E           K DISK
     FWMDPP     IF   E             DISK
     F                                     RENAME(PDP01:PDP0P)
      *  OCCUPATION MASTER
     FWTOCL     IF   E           K DISK
      *  GENERIC TABLE FILE
     FWTGTL     IF   E           K DISK
      *  STATE MASTER
     FSTSTL3    IF   E           K DISK
      *  STANDARD INDUSTRY CODE
      *  STATISTICAL REPORTING/RATING FILE
     FWDRRL     IF   E           K DISK
      *  CLAIMANT/NAME/ADDRESS CHANGE TRACKING FILE
     FWDNTP     O    E             DISK
      *  CLAIM/DARTS CONTROL FILE
     FWMCXL     IF   E           K DISK
     FWMCMLB    UF   E           K DISK
     F                                     RENAME(PCM01:PCM0B)
     F*
     F*     Payment request file.
     FWARPLC    UF   E           K DISK
      *
      * Category 2 code
     FWTC2L     IF   E           K DISK
      ***** Beg Del ***** 05/25/12 ***************************************
     F*****                                RENAME(PC201:PC401)
      ***** Beg Del ***** 05/25/12 ***************************************
      *
      * Zip Codes by County (CO#/STATE/ZIPCODE)
     FWMCZL     IF   E           K DISK
     FWDAHP     O    E             DISK
     F*** Claims Security file
     FWMCUL     UF   E           K DISK
      *  CLAIM/EDI CONTROL FILE
     FWMIXL     IF   E           K DISK
     F*     MEDICARE EXTRACT LOG FILE
     FWDD1L     UF   E           K DISK
     F*
      *
      *  EMPLOYER'S EFFECTIVE DATES
      *
      *
      *  CLAIMANT/NAME/ADDRESS CHANGE ARRAY
      *
     D DRT             S             21    DIM(5) CTDATA PERRCD(1)
      *
      *  MONTH DAY ARRAY
      *
     D MTH             S              2  0 DIM(12) CTDATA PERRCD(1)
     D DAY             S              2  0 DIM(12) ALT(MTH)
     D*  Occupation Desc Array
     D OCD             S              1    DIM(40)
     D*  first name last name array
     D OCD1            S              1    DIM(30)
      *
     D @INF            C                   CONST('Info Received')
      *
     D DBFDS           DS
     D  FLSTCM           *STATUS
     D  DBRRN                397    400B 0
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D DBFDS3          DS
     D  DBRRN3               397    400B 0
     D DBFDS4          DS
     D  DBRRN4               397    400B 0
      *
     D DATEDS          DS                  INZ
     D  CYMD                   1      8  0
     D  CC1                    1      2  0
     D  CY1                    1      4
     D  YY1                    3      4  0
     D  YMD                    3      8  0
     D  MMDD                   5      8  0
     D  MM                     5      6
     D  DD                     7      8
     D  CC2                    9     10  0
     D  CY2                    9     12
     D  YY2                   11     12  0
     D  MDCY                   5     12  0
      *
     D                 DS                  INZ
     D  ED                     1     35  0
     D                                     DIM(5) ASCEND                        EFFECT. DATE
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0
     D                 DS                  INZ
     D  CD                     1     35  0
     D                                     DIM(5) ASCEND                        CANCEL  DATE
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
     D                 DS                  INZ
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS                  INZ
      ***** Beg Chg ***** 05/25/12 ***************************************
     D*****CMSS#                  1      9
     D  ##SS#                  1      9
      ***** End Chg ***** 05/25/12 ***************************************
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9
     D*
     D*   Parameters for MC Claim Master API        QMxxxx
     D*
     D PARMCL          DS                  INZ
     D  QMRTCD                 1      1
     D  QMSS#                  2     10
     D  QMACDT                11     17
     D  FILL1                 18     18
     D  QMCO#                 19     21
     D  QMSUB#                22     26
     D  QMFND                 27     29
     D  QMFYR                 30     32
     D  QMCASE                33     39
     D  QMEMP#                40     48
     D  QMDIV                 49     53
     D  QMACTM                54     59
     D  QMDTRP                60     66
     D  FILL2                 67     67
     D  QMDTLW                68     74
     D  FILL3                 75     75
     D  QMACDS                76    135
     D  QMMCON               136    136
     D  QMENTD               137    143
     D  FILL4                144    144
     D  QMRCVD               145    151
     D  FILL5                152    152
     D  QMMMI                153    159
     D  FILL6                160    160
     D  QM1A1                161    161
     D  QM10A1               162    171
     D  QMLTFL               172    172
     D  QMSCL#               173    197
     D  QMMCTM               198    201
     D  QMSTWK               202    203
     D  QMDEDT               204    210
     D  FILL7                211    211
     D  QMDEEM               212    213
     D  QMCMNU               214    216
     D  QMCMSU               217    219
     D  QMCLAD               220    222
     D  QMRRN                223    231  0
     D  QMLSN                232    261
     D  QMFSN                262    276
     D  QMMID                277    277
     D  QMDTRE               278    284
     D  FILL8                282    282
     D  QMCLSU               285    287
     D  QM#DEP               288    290
     D*
     D*  EMPLOYER PAYROLL EFFECTIVE DATE
     D                 DS                  INZ
     D  EPEFDT                 1      7  0
     D  XXRPMY                 1      5  0
      *  ACCIDENT DATE
     D                 DS                  INZ
     D  XXACD3                 1      7  0
     D  XXACYR                 1      3  0
     D  XXACMO                 4      5  0
     D  XXACDY                 6      7  0
      *  HIRE DATE
     D                 DS                  INZ
     D  XXHID3                 1      7  0
     D  XXHIYR                 1      3  0
     D  XXHIMO                 4      5  0
     D  XXHIDY                 6      7  0
      *  TENURE
     D                 DS                  INZ
     D  XXTENR                 1      7  0
     D  XXTEYR                 2      3
     D  XXTEMO                 4      5
      *  DARTS
     D                 DS                  INZ
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCFYR                  7      9  0
     D  QCCASE                10     16  0
      *  CLAIMANT/NAME/ADDRESS DS
     D                 DS
     D  QCLNEW                 1     81
     D  QCDRT4                 1     21
     D  QCLNAM                22     81
      *
     D*  Occupation Desc Const
     D CONOCD          C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ1234567890 ')
     D*
     D*  first name last name
     D CONOC1          C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ''- abcdefgh-
     D                                     ijklmnopqrstuvwxyz')
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
      *
     D OPTNM           C                   CONST('Claimant / Employmen-
     D                                     t Info')
     D XCEXRE          C                   CONST('EXCS REIN ')
     D XCBC01          C                   CONST('CLAIM FILE STATUS IS-
     D                                      OUT, USER -')
     D XCBC02          C                   CONST('          CLAIM FILE-
     D                                      STATUS IS IN')
     D COMAPI          C                   CONST('WA001R')
     D DOTS            C                   CONST(' . . . . . . . . . .-
     D                                      . . .')
     D XCORGO          C                   CONST('Original open of -
     D                                     claim')
     D*
     D* CATEGORY 2 HEADING
     D*
     D XXCAT2          C                   CONST('Category 2')
     D*
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D*
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D*
     D                 DS
     D  SYFL12                 1     10
     D  XXCMS                  2      2
      ***** Beg Add ***** 05/25/12 ***************************************
     d  dsWMCMP      e ds                  Extname(wmcmp)
     d  x_stat       e                     extfld(cmstat)
     d  x_user       e                     extfld(cmuser)
     d  x_dt         e                     extfld(cmdt)
     d  x_time       e                     extfld(cmtime)
      ***** End Add ***** 05/25/12 ***************************************
      *
      ***** Beg Del ***** 05/25/12 ***************************************
     I*****DBRCDF
     I*****         CMSTAT                      C2STAT
     I*****         CMUSER                      C2USER
     I*****         CMDT                        C2DT
     I*****         CMTIME                      C2TIME
      *
     I*****PC401
     I*****         C2CO#                       C4CO#
     I*****         C2FND                       C4FND
     I*****         C2CTG2                      C4CTG2
     I*****         C2DESC                      C4DESC
     I*****         C2STAT                      C4STAT
     I*****         C2USER                      C4USER
     I*****         C2PGM                       C4PGM
     I*****         C2TIME                      C4TIME
     I*****         C2DT                        C4DT
     I*****         C2IUSR                      C4IUSR
     I*****         C2IPGM                      C4IPGM
     I*****         C2ITIM                      C4ITIM
     I*****         C2IDT                       C4IDT
      ***** End Del ***** 05/25/12 ***************************************
      *
      ***** Beg Del ***** 05/25/12 ***************************************
     I*****PSR01
     I*****         SRTIME                      S1TIME
     I*****         SRCADT                      S1CADT
      ***** End Del ***** 05/25/12 ***************************************
      *
      ***** Beg Del ***** 05/25/12 ***************************************
     I*****/COPY SCOPYMT01I
      ***** End Del ***** 05/25/12 ***************************************
      * ================================================================
      ***** Beg Add ***** 05/25/12 ***************************************
     C/COPY SCOPYMT05C
      ***** End Add ***** 05/25/12 ***************************************
      *
      *  CLAIMANT DEMOGRAPHICS FILE
      *
     C     CDKY          KLIST
     C                   KFLD                    XXSS#
     C*
     C*    Statistical Reporting/Rating file
     C     RRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXJRST
     C                   KFLD                    ELPFDT
     C*
     C*    Statistical Reporting/Rating file (partial)
     C     RRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXJRST
      *
      *  Key list for the Claim Master file
      *
     C     CMKYB         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSS#
     C*
     C*
     C*     Payment Request File
     C     RPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    CMCASE
     C*
     C*
     C     RPKYB         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
      *
      *   KEY LIST FOR THE CLAIM/DARTS CONTROL FILE
      *
     C     COKY          KLIST
     C                   KFLD                    XXCO#
      *
      *  REMARK CODE TABLE FILE
      *
     C     CTKY          KLIST
     C                   KFLD                    XXRMCD            5
      *
      *   CLAIMS/DARTS CONTROL KEY LIST
      *
     C     CXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      *  WORKERS COMP DEPARTMENT MASTER FILE
      *
     C     DPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXDEPT
      *
      *   KEY LIST FOR THE EMPLOYER DETAIL FILE
      *
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
      *
      *  Employer Cross Reference
      *
     C     EXKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
      *
      *  EMPLOYER MASTER FILE
      *
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
      *
      *  EMPLOYER PAYROLL FILE
      *
     C     EPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    SCCLAS
     C                   KFLD                    SCSPEC
     C                   KFLD                    SCEXT
     C                   KFLD                    XXEFDT
      *
     C     EPKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    SCCLAS
     C                   KFLD                    SCSPEC
     C                   KFLD                    SCEXT
      *
      *   KEY LIST FOR THE FUND
      *
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      *   KEY LIST FOR THE FUND DETAIL FILE
      *
     C     F1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
      *
      *   KEY LIST FOR THE GENERIC TABLE FILE
      *
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
      *
      *   KEY LIST FOR OCCUPATION CODE FILE
      *
     C     OCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
      *
      *  PAYROLL DESCRIPTION EXTENSION FILE
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
      *   KEY LIST FOR THE PAYROLL CLASS FILE
      *
     C     PRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    SCCLAS
     C                   KFLD                    XXEFDT
      *
     C     PRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    SCCLAS
      *
      *   KEY LIST FOR THE RATE SET CODE FILE
      *
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELRTCD
      *
      *   KEY LIST FOR THE STANDARD INDUSTRY FILE
      *
     C     SCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
      *
      *   KEY LIST FOR THE EMP SELF REPORTING FILE
      *
     C     SRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    SCCLAS
     C                   KFLD                    SCSPEC
     C                   KFLD                    XXRPMY
      *
     C     SRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    SCCLAS
     C                   KFLD                    SCSPEC
      *
      *
      * Key List for Zip Codes by County File
      *
     C     CZKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##CZST            2
     C                   KFLD                    ##ZIP5            5 0
      *
      *   KEY LIST FOR THE STATE FILE
      *
     C     STKY          KLIST
     C                   KFLD                    XXKEY
      *
      *   KEY LIST FOR THE SYSTEM CONTROL FILE
      *
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
      *
      *  TABLE FILE
      *
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
      *
      *  USER PARAMETER FILE
      *
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
      *
      *  CLAIM/EDI CONTROL FILE
      *
     C     IXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJRST
     C                   KFLD                    XX148             3
      *
      *
      *  Category 2 Definition
      *
      ***** Beg Chg ***** 05/25/12 ***************************************
     C*****C4KY          KLIST
     C     C2KY          KLIST
      ***** Beg Chg ***** 05/25/12 ***************************************
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCCTG2
      *
     C     CUKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
      *
      *
     C     *LIKE         DEFINE    OCKEY         @@KEY
     C     *LIKE         DEFINE    OCDESC        @@DESC
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    CMCO#         XXCO1#
     C     *LIKE         DEFINE    CMFND         XXFND1
     C     *LIKE         DEFINE    CMFYR         XXFYR1
     C     *LIKE         DEFINE    CMCASE        XXCAS1
     C     *LIKE         DEFINE    CMCASE        SVCASE
     C     *LIKE         DEFINE    CMEMP#        XXEMP#
     C     *LIKE         DEFINE    CMEMP#        SVEMP#
     C     *LIKE         DEFINE    CMDIV         XXDIV
     C     *LIKE         DEFINE    CMDIV         SVDIV
     C     *LIKE         DEFINE    CMSS#         XXSS#
     C     *LIKE         DEFINE    CMACDT        XXDT                           SYSTEM DT
     C     *LIKE         DEFINE    CMDT          SVDATE                         CURRENT DATE
     C     *LIKE         DEFINE    CMTIME        SVTIME                         CURRENT TIME
     C     *LIKE         DEFINE    CMACDT        XEEFDT                         EMP EFF DT
     C     *LIKE         DEFINE    CMACDT        XECADT                         EMP CX DT
     C     *LIKE         DEFINE    CMACDT        XXEFDT                         EFFECTIVE DT
     C     *LIKE         DEFINE    CMACDT        XXACDT                         ACC DT
     C     *LIKE         DEFINE    SCACD2        HACDT
     C     *LIKE         DEFINE    CMACDT        XXCADT                         EMP CX DATE
     C     *LIKE         DEFINE    CMACDT        XXDTOB                         DT OF BIRTH
     C     *LIKE         DEFINE    CMACDT        XXPFDT                         POLICY FROM
     C     *LIKE         DEFINE    CMACDT        XXHIDT                         HIRE
     C     *LIKE         DEFINE    CMACDT        XXDTRP                         DT RPT TO EMP
     C     *LIKE         DEFINE    CMACDT        XXRCVD                         CARRIER RCVD
     C     *LIKE         DEFINE    CMACDT        XXTPAD                         TPA CONTACT DT
     C     *LIKE         DEFINE    DPDEPT        XXDEPT
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         QQKEY
     C     *LIKE         DEFINE    GTKEY         XXKEY
     C     *LIKE         DEFINE    GTTBID        QQTBID
      * CLAIMANT NAME/ADDRES CHG
     C     *LIKE         DEFINE    CDLSN         SVLSN
     C     *LIKE         DEFINE    CDMID         SVMID
     C     *LIKE         DEFINE    CDFSN         SVFSN
     C     *LIKE         DEFINE    CDADR1        SVADR1
     C     *LIKE         DEFINE    CDADR2        SVADR2
     C     *LIKE         DEFINE    CDST          SVST
     C     *LIKE         DEFINE    CDCTY         SVCTY
     C     *LIKE         DEFINE    CDZIP         SVZIP
     C     *LIKE         DEFINE    CDZIP         SCZIP
     C     *LIKE         DEFINE    CDZIP         XXZIP
     C     *LIKE         DEFINE    NTTYPE        XTYPE1
     C     *LIKE         DEFINE    NTTYPE        XTYPE2
     C     *LIKE         DEFINE    CMDTLW        XXHDLW
     C     *LIKE         DEFINE    CMRCVD        XXCDAT
     C     *LIKE         DEFINE    CMLTFL        XXLTFL
     C     *LIKE         DEFINE    UPUSNM        QCUSER
     C     *LIKE         DEFINE    Q1PGM         XXPGM
     C     *LIKE         DEFINE    RSJRST        XXJRST
     C     *LIKE         DEFINE    *IN60         XXMANC
     C     *LIKE         DEFINE    PXST          XXST
     C     *LIKE         DEFINE    *IN60         XXEDTL
     C     *LIKE         DEFINE    D1EXDT        XXEXDT
     C*** BEG ADD *** 05/25/12 **************************************
     C     *LIKE         DEFINE    CDEMAD        SVEMAD
     C*** END ADD *** 05/25/12 **************************************
      *
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@CONT            1
      *
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      *ZERO         ##RRN             9 0
     C                   PARM                    ##RRN2            9 0
      *
     C     ACDPL1        PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      *ZERO         ##RRN             9 0
     C                   PARM                    XXKEY
     C                   PARM                    SCCST
      *
     C     ACDPL2        PLIST
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##RRN
     C                   PARM                    XXKEY
      *
     C     ACDPL3        PLIST
     C     *INKC         PARM                    @@KC
     C     *INKL         PARM                    @@KL
     C                   PARM                    @@MODE
     C                   PARM                    @@RRN
     C                   PARM                    @@CONT
      *
     C     ACDPL4        PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      *ZERO         ##RRN             9 0
     C                   PARM                    XXKEY
     C                   PARM                    SCCST
     C                   PARM      2             ##VIEW            1 0
      *
     C     ACDPL5        PLIST
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##RRN
     C                   PARM                    XXKEY
     C                   PARM      2             ##VIEW            1 0
      *
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##LVL             3 0
     C                   PARM                    ##TYPE            1
      *
      *******Beg Del********05/25/12**********************************
     C*****/COPY SCOPYMT01C
      *******End Del********05/25/12**********************************
      *
      *===============================================================
      * CALLR - SUBROUTINE FOR CALLING THE NEXT PROGRAM
      *===============================================================
      *
     C     CALLR         BEGSR
      *
     C     SCRN#         IFEQ      #OFSCR
      *
     C     Q1PRMS        IFGE      5
     C     @@CONT        ANDEQ     'A'
     C                   MOVE      'A'           @@MODE
     C                   END
      *
      *  THIS IS FOR A POSSIBLE ERROR SITUATION:
      *  IF IN CHANGE MODE BUT WMCDP WAS JUST WRITTEN, CONTINUE
      *  ON IN ADD MODE.
      *
     C     Q1PRMS        IFGE      5
     C     *IN40         ANDEQ     *ON
     C                   MOVE      'A'           @@MODE
     C                   END
      *
      *
     C     CLTID         IFEQ      '133'
      *
     C     Q1PRMS        IFGE      5
     C     @@CONT        ANDNE     *BLANK
     C                   SELECT
     C     @@MODE        WHENEQ    'A'
     C                   MOVEL     'A'           QQCALL
     C     @@MODE        WHENEQ    'C'
     C                   MOVEL     'Y'           QQCALL
     C     @@MODE        WHENEQ    'D'
     C                   MOVEL     'Z'           QQCALL
     C                   ENDSL
     C                   ENDIF
      *
     C                   MOVE      CMCO#         QACO#
     C                   MOVE      CMFND         QAFND
     C                   MOVE      CMFYR         QAFYR
     C                   MOVE      CMCASE        QACASE
     C                   MOVEL     CMJRST        XXJRST
     C                   MOVEL     '148'         XX148
     C     IXKY          CHAIN     PIX01                              60
     C     *IN60         IFEQ      *OFF
     C     'W0070R'      CAT       IXFROI:0      XXPGM
     C                   ELSE
     C     'W0070R'      CAT       CMJRST:0      XXPGM
     C                   END
     C     2             SUBST(P)  XXPGM:7       XXST
      *
     C     @@MODE        IFEQ      'A'
     C     CXSTSP        ANDEQ     'Y'
      *
     C     XXST          IFNE      *BLANKS
     C                   CALL      XXPGM                                99
     C                   PARM                    QQCALL            1
     C                   PARM                    QACO#             3
     C                   PARM                    QAFND             3
     C                   PARM                    QAFYR             3
     C                   PARM                    QACASE            7
     C                   PARM                    QQRTCD            1
     C                   PARM                    @@CONT            1
     C                   ELSE
     C                   MOVE      '1'           *IN99
     C                   ENDIF
      *
     C                   ENDIF                                                  @@MODE = 'A'
      *
     C     @@MODE        IFNE      'A'
     C     XXST          IFNE      *BLANKS
     C                   CALL      XXPGM                                99
     C                   PARM                    QQCALL            1
     C                   PARM                    QACO#             3
     C                   PARM                    QAFND             3
     C                   PARM                    QAFYR             3
     C                   PARM                    QACASE            7
     C                   PARM                    QQRTCD            1
     C                   PARM                    @@CONT            1
     C                   ELSE
     C                   MOVE      '1'           *IN99
     C                   ENDIF
      *
     C                   ENDIF                                                  @@MODE <> 'A'
      *
     C     QQRTCD        IFEQ      'B'
     C                   MOVE      *ON           *INKL
     C                   ENDIF
      *
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   ENDIF
      *
      * If no state program exists continue with current call
     C     *IN99         IFEQ      *ON
     C     Q1PRMS        ANDGE     5
     C                   CALL      'W0070R08'    ACDPL3
     C                   ENDIF
      *
      * If Add mode & no state program flag not set then
      * continue with current call
     C     @@MODE        IFEQ      'A'
     C     CXSTSP        ANDNE     'Y'
     C     *IN99         ANDEQ     *OFF
     C     Q1PRMS        ANDGE     5
     C                   CALL      'W0070R08'    ACDPL3
     C                   ENDIF
      *
     C                   END
      *
      *
     C     Q1PRMS        IFGE      5
     C     CLTID         ANDNE     '133'
     C                   CALL      'W0070R08'    ACDPL3
     C                   END
      *
     C     *INKL         IFEQ      *ON
     C     @@MODE        IFEQ      'A'
     C                   MOVEL     'C'           @@MODE
     C                   END
     C     @@MODE        CASEQ     'C'           PRECHG
     C     @@MODE        CASEQ     'D'           PREDSP
     C                   CAS                     PREOTH
     C                   ENDCS
      *
     C                   MOVE      *ON           *IN90
     C                   END
     C                   END
     C                   ENDSR                                                  CALLR
      *
      *===============================================================
      * DLTPRE - SUBROUTINE FOR PRIOR TO DELETION PROCESSING
      *===============================================================
      *
     C     DLTPRE        BEGSR
     C                   ENDSR                                                  DLTPRE
      *
      *===============================================================
      * DSPLYR - SUBROUTINE FOR DISPLAYING INFORMATION
      *===============================================================
      *
     C     DSPLYR        BEGSR
     C     1             SUBST(P)  Q2RECF:5      SCRN#A            1
     C                   MOVE      SCRN#A        SCRN#
      *
     C     Q1PRMS        IFGE      5
     C     @@CONT        ANDNE     *BLANK
     C     SCRN#         IFEQ      #OFSCR
     C                   EXSR      CALLR
     C                   END
     C                   ENDIF
      *
     C                   ENDSR                                                  DSPLYR
      *
      *===============================================================
      * ERRCK1 - SUBROUTINE FOR ERROR CHECKING SCR01
      *===============================================================
      *
     C     ERRCK1        BEGSR
      *
      *  DROP OUT OF LOOP IF *IN90 IS SET ON.
      *  INDICATORS 71 - 90 ARE RESET IN COPYBOOK SCOPYMT01C.
      *
     C                   DO
      *
     C                   MOVEA     '0000000'     *IN(20)                        *IN20 - 26
     C                   MOVEA     '0000000'     *IN(27)                           27 - 33
     C                   MOVEA     '00'          *IN(34)                           34 - 35
     C                   MOVEA     '00'          *IN(54)                           54 - 55
     C                   MOVEA     '000'         *IN(57)                           57 - 59
     C                   MOVEA     '0'           *IN(68)                           68
     C                   MOVEA     '0'           *IN(91)                        ADD 06/14/00
     C                   MOVEA     '0'           *IN(16)                           16
     C                   MOVEA     '0'           *IN(62)                           62
     C                   MOVEA     '0'           *IN(93)                           93
     C                   MOVEA     '0'           *IN(94)                           94
     C                   MOVEA     '0'           *IN(95)                           95
      *
      *  CLAIMANT NAME CAN NOT BE BLANK
      *
     C     SCLSN         IFEQ      *BLANKS
     C                   SETON                                        9020
     C                   LEAVE
     C                   ENDIF
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
     C                   SETON                                        9074
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ENDDO
     C   74              LEAVE
      *
     C     SCFSN         IFEQ      *BLANKS
     C                   SETON                                        9021
     C                   LEAVE
     C                   ENDIF
     C                   MOVEL(P)  SCFSN         XXFSN            30
     C                   MOVEA     XXFSN         OCD1
     C                   Z-ADD     1             I                 2 0
     C                   Z-ADD     0             SNPOS             2 0
     C                   DO        30
     C*
     C     OCD1(I)       SCAN      CONOC1        SNPOS             2 0
     C*
     C                   ADD       1             I
     C*
     C     SNPOS         IFEQ      0
     C                   SETON                                        9075
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ENDDO
     C   75              LEAVE
      *
      *  CHECK ADDRESS 1
      *
     C     SCCAD1        IFEQ      *BLANKS
     C                   SETON                                        9022
     C                   LEAVE
     C                   ENDIF
      *
      *
      *  EITHER THE C,S,Z IS ENTERED OR THE OUT OF COUNTRY ADDRESS
      *
     C     SCCCTY        IFEQ      *BLANKS
     C     SCCST         ANDEQ     *BLANKS
     C     SCCZP1        ANDEQ     *ZERO
     C     SCCTRY        ANDEQ     *BLANK
     C     SCADR3        ANDEQ     *BLANK
     C                   SETON                                        9029
     C                   LEAVE
     C                   ENDIF
      *
     C     SCCTRY        IFNE      *BLANK
     C     SCCTRY        ANDNE     'USA'
      *
     C     SCADR3        IFEQ      *BLANK
     C                   SETON                                        9029
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
      *
      * BOTH THE C,S,Z AND THE OUT OF COUNTRY ADDRESS CANNOT BE ENTERED
      *
     C     SCCTRY        IFNE      'USA'
     C     SCCTRY        ANDNE     *BLANK
      *
     C     SCCCTY        IFNE      *BLANKS
     C     SCCST         ORNE      *BLANKS
     C     SCCZP1        ORNE      *ZERO
     C                   SETON                                        9029
     C                   ENDIF
      *
     C     SCADR3        IFEQ      *BLANK
     C                   SETON                                        9029
     C                   ENDIF
      *
     C                   ELSE
      *
     C     SCADR3        IFNE      *BLANKS
     C                   SETON                                        9029
     C                   ENDIF
     C                   ENDIF
      *
     C     SCCTRY        IFEQ      *BLANKS
     C     SCCTRY        OREQ      'USA'
      *
      *  CHECK CITY
      *
     C     SCCCTY        IFEQ      *BLANKS
     C                   SETON                                        9023
     C                   LEAVE
     C                   ENDIF
      *
      *  VALIDATE STATE
      *
     C                   MOVEL(P)  SCCST         XXKEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        9024
     C                   LEAVE
     C                   END
      *
      *  CHECK ZIP CODE
      *
     C     SCCZP1        IFEQ      *ZERO
     C                   SETON                                        9025
     C                   LEAVE
     C                   ENDIF
      *
     C                   ENDIF
      *
      *  CHECK COUNTY CODE
      *
     C     SCCCCD        IFNE      *BLANK
     C     SCCST         CAT(P)    SCCCCD:0      XXKEY
     C     TBKY          CHAIN     PCN01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        9055
     C                   LEAVE
     C                   ENDIF
     C                   ELSE
     C     *IN40         IFEQ      *ON                                          ADD MODE
      * Also in Chage mode, preload if blank
     C     @@MODE        OREQ      'C'
     C     SCCCCD        ANDEQ     *BLANKS
     C                   MOVE      SCCST         ##CZST
     C                   MOVE      SCCZP1        ##ZIP5
     C     CZKY          CHAIN     PCZ01                              60
     C     *IN60         IFEQ      *OFF
     C     CZSTAT        ANDEQ     'A'
     C                   MOVE      CZCNTY        SCCCCD
     C                   ENDIF                                                  END-CZCNTY
     C                   ENDIF                                                  END-40 = ON
      *
     C                   ENDIF
     C*
     C*
     C     SCCTRY        IFNE      *BLANK
     C                   MOVEL(P)  'CTRY'        XXTBID
     C                   MOVEL(P)  SCCTRY        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   SETON                                        9057
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCADR3        IFEQ      *BLANK
     C     SCCTRY        IFNE      *BLANK
     C     SCCTRY        ANDNE     'USA'
     C                   SETON                                        9058
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*
      *
      *  EDIT ACCIDENT TIME
      *
     C     SCACTM        IFNE      *ZEROS
     C                   MOVEL     SCACTM        XXTIME            6 0
     C                   MOVEL     XXTIME        SRTIME
      ***** Beg Add ***** 05/25/12 ***************************************
     C                   MOVEL     SRTIME        ##TIME
      ***** End Add ***** 05/25/12 ***************************************
     C                   CALL      'SRTME'
      ***** Beg Chg ***** 05/25/12 ***************************************
     C*****              PARM                    SRTIME
     C                   PARM                    ##TIME            6
      ***** End Chg ***** 05/25/12 ***************************************
     C                   PARM      ' '           SRERR
     C                   PARM      ' '           SRTERM            1
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN32
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
     C                   ELSE                                                   ACTM = *ZERO
     C     CLTID         IFEQ      '065'
     C                   MOVE      *ON           *IN62
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   ENDIF
     C                   END
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  CHECK EMPLOYER EFFECTIVE & CANCEL DATES.
      *  ALLOW OVERRIDE ON ERROR MSG ONLY IF USER HAS PROPER AUTHORITY.
      *
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  PAYROLL CLASS CODE
      *
      *  CHECK GROUP DETAIL
      *    TO SEE IF WE ARE TO VALIDATE PAYROLL CLASS CD
      *
     C     F1RLXC        IFNE      'Y'                                          DO COV EDITS
      *
      *  ACCIDENT DATE MUST FALL WITHIN LIMITS IN EMPLOYER PAYROLL
      *
     C                   MOVE      *OFF          *IN60
     C                   MOVE      *BLANK        XXRD01            1
     C                   Z-ADD     CMACDT        XXEFDT
     C     EPKY          SETGT     PEP01
     C     *IN60         DOUEQ     *ON
     C     EPKY1         READPE    PEP01                                  60
      *
     C     *IN60         IFEQ      *OFF
     C     EPSTAT        IFEQ      'A'
     C                   MOVE      'Y'           XXRD01                         GOOD RECORD
     C                   LEAVE
     C                   END
     C                   END
     C                   ENDDO
      *
      *  EXTENSION FIELD IS VALID OR NOT
      *
     C     SCEXT         IFNE      *BLANKS
     C                   MOVEL     *BLANKS       XXST
     C                   MOVE      SCEXT         EPEXT
     C     PXKY          SETGT     PPX01
     C     PXKY1         READPE    PPX01                                  60
      *
     C   60RSKY          CHAIN     PRS01                              61
     C  N61
     CAN 60              MOVE      RSJRST        XXST
      *
     C  N61
     CAN 60PXKY          SETGT     PPX01
     C  N61
     CAN 60PXKY1         READPE    PPX01                                  60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        82  90
     C                   ENDIF                                                  *IN60 IFEQ
     C     *IN60         IFEQ      *OFF
     C     PXSTAT        ANDEQ     'I'
     C                   SETON                                        11  90
     C                   ENDIF                                                  *IN60 = *OFF
     C                   ENDIF                                                  SCEXT <> ' '
     C     XXRD01        IFEQ      *BLANK
      *
      *  Display override window if the user has valid security level.
      *
     C                   MOVE      '1'           SRRES                          Authorized to
     C     CXKY          CHAIN     PCX01                              99
      *
     C     *IN99         IFEQ      *OFF
     C                   MOVE      ##LVL         SRULVL
     C                   MOVE      CXAUSL        SRCLVL
      *
     C                   CALL      'SRCCSC'
     C                   PARM      ##TYPE        SRUTYP
     C                   PARM                    SRULVL
     C                   PARM      CXAUT1        SRCTYP
     C                   PARM                    SRCLVL
     C                   PARM      '1'           SREXEC
     C                   PARM                    SRRES
     C                   PARM      CXAUT2        SRCTP2
     C                   PARM      CXAUT3        SRCTP3
     C                   PARM      CXAUT4        SRCTP4
     C                   PARM      CXAUT5        SRCTP5
     C                   PARM      ' '           SRCTP6
      *
     C                   ENDIF                                                  *IN99
      *
      *         SRRES = '0' if "Passed" Security Check
      *                 '1' if "Did Not Pass" Security Check
      *
     C                   MOVE      'N'           SCEOVR
     C     SRRES         IFEQ      '0'
     C                   EXFMT     WDW05
     C                   ENDIF
      *
     C     SCEOVR        IFEQ      'Y'
     C                   GOTO      EPYRL
     C                   ELSE
      *
     C                   MOVE      *ON           *IN28                          INVALID OR
     C                   MOVE      *ON           *IN90                           INACTIVE CLASS
     C                   LEAVE
     C                   ENDIF
     C                   END
      *
      *  THE FOLLOWING EDIT (AIRCRAFT SURCHARGE) RELIES ON THE
      *  JURISDICTION STATE BEING RETREIVED FROM WMRSP BASED ON
      *  THE RATE CODE FROM WDELP.  THIS IS BEING DONE IN MVDBSC.
      *  IF THAT RETREIVAL CHANGES, WE MUST GET THAT INFO HERE.
      *
      *  VERIFY THAT THE PAYROLL CLASS CODE IS NOT AN AIRCRAFT
      *  SURCHARGE CODE.                                               .
      *
     C     RRKY          SETGT     PRR01
     C     RRKY1         READPE    PRR01                                  60
     C     *IN60         IFEQ      *OFF
     C     SCCLAS        IFEQ      RRDCCD
     C                   MOVEA     '11'          *IN(90)
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
      *
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  60
     C     *IN60         IFEQ      *OFF
     C     PROCCD        ANDEQ     'Y'
     C                   SETON                                        76  90
     C                   ENDIF
      *
      *  CHECK EMP PAYROLL CLASS CODE CX DATE AGAINST ACCIDENT DATE
      *  IF PAYROLL CLASS HAS BEEN CANCELLED, SEND INFO MSG (NOT ERROR).
      *
     C     EPCADT        IFLE      CMACDT
     C     EPCADT        ANDGT     *ZERO
     C                   EXFMT     WDW01
     C                   END                                                    EPCADT LE CMACDT
      *
      *  IF THIS IS A SELF REPORTING EMP AND NO PAYROLLS EXIST,
      *  DSP WINDOW: THERE IS NO MONEY IN THIS PAYROLL CLASS
      *  AND YOU MAY NOT WANT TO COVER THE CLAIM.
      *
     C     ELBSCD        IFNE      *ZERO
     C                   MOVEL(P)  ELBSCD        XXKEY
     C     TBKY          CHAIN     PBS01                              60
     C     *IN60         IFEQ      *OFF
      *
     C                   Z-ADD     *ZERO         XXAMTP           13 2
     C*
     C     BSSEFL        IFEQ      'S'
     C     SRKY1         CHAIN     PSR01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     SRRPMY        IFGE      XXRPMY
     C     SRTODT        ORGE      EPEFDT
     C                   ADD       SRAMTP        XXAMTP
     C                   ENDIF
     C*
     C     SRKY1         READE     PSR01                                  60
     C                   ENDDO
     C*
     C                   ELSE
     C                   ADD       EPEAPR        XXAMTP
     C                   ADD       EPAAPR        XXAMTP
     C                   END
      *
     C     XXAMTP        IFEQ      *ZERO
     C                   EXFMT     WDW02
     C                   END
      *
     C                   END
     C                   END                                                    ELBSCD NE BLANK
      *
      *
      *  Prevent adding claim to payroll class with no payroll.
      *
     C  N90BSSEFL        IFEQ      'E'
      *
     C     ELFAF         IFEQ      'A'
     C     EPAAPR        ANDEQ     0
     C     SCCLAS        ANDNE     CMCLAS
     C     ELFAF         ORNE      'A'
     C     EPEAPR        ANDEQ     0
     C     SCCLAS        ANDNE     CMCLAS
     C                   SETON                                        73
      *
     C     CXKY          CHAIN     PCX01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      ##LVL         SRULVL
     C                   MOVE      CXAUSL        SRCLVL
      *
     C                   CALL      'SRCCSC'
     C                   PARM      ##TYPE        SRUTYP            1
     C                   PARM                    SRULVL            3
     C                   PARM      CXAUT1        SRCTYP            1
     C                   PARM                    SRCLVL            3
     C                   PARM      '1'           SREXEC            1
     C                   PARM                    SRRES             1
     C                   PARM      CXAUT2        SRCTP2            1
     C                   PARM      CXAUT3        SRCTP3            1
     C                   PARM      CXAUT4        SRCTP4            1
     C                   PARM      CXAUT5        SRCTP5            1
     C                   PARM      ' '           SRCTP6            1
      *
      *         SRRES = '0' if "Passed" Security Check
      *                 '1' if "Did Not Pass" Security Check
      *
     C     SRRES         IFEQ      '0'
     C                   MOVE      'Y'           XBAUTH            1            Authorized to
     C                   ENDIF                                                   overide edit
      *
     C     XBAUTH        IFEQ      'Y'                                          IF AUTH
     C                   MOVE      'N'           SCEOVR
     C                   EXFMT     WDW04
      *
     C     SCEOVR        IFEQ      'Y'
     C                   SETOFF                                       73        OK
     C                   ELSE
     C                   SETON                                        90
     C                   ENDIF                                                  SCEOVR
      *
     C                   ELSE                                                   NOT AUTHORIZED
     C                   SETON                                        74  90
     C                   MOVE      'N'           SCEOVR
     C                   EXFMT     WDW04
     C                   SETOFF                                       74
     C                   ENDIF                                                  XBAUTH = 'Y'
      *
     C                   ENDIF                                                  *IN60
      *
     C                   ENDIF                                                  ELFAF = 'A'
      *
     C                   ENDIF                                                  BSSEFL = 'E'
     C     EPYRL         TAG
     C                   END                                                    F1RXLC NE Y
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  VALIDATE SIC CODE, IF REQUIRED
      *
      *  EDIT BIRTH DATE
      *
     C     SCDTOB        IFNE      *ZERO
     C                   Z-ADD     SCDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         SCDTOB
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN26
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
      *
      * DATE OF BIRTH
     C                   Z-ADD     SCDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD                         MDY --> YMD
     C                   MOVEL     SRDTO         XXDTOB
      *
      *  THE DATE OF BIRTH CANNOT BE > THE SYSTEM DATE.
      *
     C     XXDTOB        IFGT      XXDT
     C                   MOVE      *ON           *IN31
     C                   MOVE      *ON           *IN26
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
      *
     C                   ELSE                                                   DOB = *ZEROS
     C     CLTID         IFEQ      '065'
     C                   MOVE      *ON           *IN16
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   ENDIF
     C     CXKY          CHAIN     PCX01                              60
     C     *IN60         IFEQ      *OFF
     C     CXDOBR        ANDEQ     'Y'
     C                   SETON                                        9083
     C                   LEAVE
     C                   ENDIF                                                  e- 60 = OFF ...
     C                   END
      *
      *  THE DATE OF BIRTH CANNOT BE > THE ACCIDENT DATE
      *
     C                   Z-ADD     SCACD2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         XXACDT
      *
     C     XXDTOB        IFGT      XXACDT
     C                   MOVE      *ON           *IN31
     C                   MOVE      *ON           *IN68
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
      *
      *  CALCULATE THE CLAIMANT'S AGE AT TIME OF ACCIDENT
      *
     C     SCDTOB        IFNE      01011901
     C     CMACDT        SUB       XXDTOB        XXAGE             7 0
     C                   MOVEL(P)  XXAGE         XX3A              3
     C                   MOVE      XX3A          SCCAGE
     C                   END
      *  DON'T CALCULATE AGE IF THE DEFAULT WAS ENTERED.
     C     SCDTOB        IFEQ      *ZERO
     C                   MOVE      *BLANK        SCCAGE
     C                   ENDIF
      *
     C*
     C*    If flag CXGDRR = Y, Gender cannot be blank
     C     CXKY          CHAIN     PCX01                              60
     C     *IN60         IFEQ      *OFF
     C     CXGDRR        ANDEQ     'Y'
     C     SCSEX         ANDEQ     *BLANK
     C                   SETON                                        9084
     C                   LEAVE
     C                   ENDIF                                                  e- 60 = OFF ...
      *
      *  GENDER MUST BE F, M, OR BLANK
      *
     C     SCSEX         IFNE      *BLANK
     C     SCSEX         ANDNE     'F'
     C     SCSEX         ANDNE     'M'
     C                   SETON                                        9027
     C                   LEAVE
     C                   ENDIF
     C     SCSEX         IFEQ      *BLANK                                       SEX = *BLANK
     C     CLTID         ANDEQ     '065'
     C                   MOVE      *ON           *IN93
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   ENDIF
      *
      *  CHECK MARITAL STATUS
      *
     C     SCMSTS        IFNE      *BLANKS
     C                   MOVEL(P)  'MSTS'        XXTBID
     C                   MOVEL(P)  SCMSTS        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   SETON                                        9054
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
      *
      ** EMPLOYEE ID TYPE QUALIFIER
      *
     C     SCEITQ        IFGT      *BLANKS
     C     SCEITQ        OREQ      *BLANKS
     C     SCEID         ANDGT     *BLANKS
     C                   MOVEL(P)  'EIDQ'        XXTBID
     C                   MOVEL(P)  SCEITQ        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        9043
     C                   LEAVE
     C                   ENDIF
     C     SCEID         IFEQ      *BLANKS
     C                   SETON                                        9042
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*** If Medicare Secondary Payer is installed
     C*
     C     1             SUBST     SYFL12:2      XXFL12            1
     C     XXFL12        IFEQ      'Y'
     C*
     C** Soft edit for Date of Birth Entry .Bypass if already displayed once
     C*
     C     SCDTOB        IFEQ      *ZEROS
     C     FLGDOB        ANDEQ     'N'
     C*
     C                   SETON                                        94
     C                   EXFMT     WDW06
     C                   SETON                                        90
     C                   MOVE      'Y'           FLGDOB
     C*
     C                   SETOFF                                       94
     C                   ENDIF
     C*
     C     SCSEX         IFEQ      *BLANKS
     C     FLGSEX        ANDEQ     'N'
     C*
     C                   SETON                                        95
     C                   EXFMT     WDW06
     C                   SETON                                        90
     C                   MOVE      'Y'           FLGSEX
     C*
     C                   SETOFF                                       95
     C                   ENDIF
     C*
     C                   ENDIF
      *
     C                   ENDDO                                                  *IN90
      *
      *  If another user has updated this claim, send an error message.
      *
     C     @@RRN         CHAIN(N)  DBRCDF                             99
      *
      ***** Beg Chg ***** 05/25/12 ***************************************
     C*****  N99SVDATE        IFNE      C2DT
     C*****  SVTIME        ORNE      C2TIME
     C  N99SVDATE        IFNE      x_DT
     C     SVTIME        ORNE      x_TIME
      ***** Beg Chg ***** 05/25/12 ***************************************
      *
     C                   CALL      'WA017R'
      ***** Beg Chg ***** 05/25/12 ***************************************
     C*****              PARM                    C2USER
     C                   PARM                    x_USER
      ***** End Chg ***** 05/25/12 ***************************************
     C                   PARM                    QQRTCD
      *
     C                   MOVE      *ON           *IN90
     C                   ENDIF
      *
      *  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
      *
     C                   Z-ADD     1             SCRN#             1 0
      *
     C     CLTID         IFEQ      '114'
      *
      *  Require entry of SCCTG2 for certain Groups
      *
     C     SCFND         IFEQ      350
     C     SCFND         OREQ      385
     C     SCFND         OREQ      400
     C     SCFND         OREQ      410
     C     SCFND         OREQ      420
     C     SCFND         OREQ      430
     C     SCFND         OREQ      440
     C     SCFND         OREQ      450
     C     SCFND         OREQ      460
     C                   MOVE      X'26'         ATR
     C                   MOVE      'Y'           FLG114
      *
     C                   ELSE
      *
     C                   MOVE      'N'           FLG114
     C                   ENDIF
     C                   ENDIF
      *
     C     CXKY          CHAIN     PCX01                              99
      *
     C     CXNMAD        IFNE      *BLANKS
     C     *IN90         ANDEQ     *OFF
     C                   EXSR      AUDIT
      *
     C     @@AFL         IFEQ      'Y'
     C     ##RTN         ANDNE     'Y'
     C                   SETON                                        90
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDSR                                                  ERRCK1
      *
      *===============================================================
      * ERRCK2 - SUBROUTINE FOR ERROR CHECKING SCR02
      *===============================================================
      *
     C     ERRCK2        BEGSR
      *
      *  DROP OUT OF LOOP IF *IN90 IS SET ON.
      *  INDICATORS 71 - 90 ARE RESET IN COPYBOOK SCOPYMT01C.
      *
     C                   DO
      *
     C                   MOVEA     '0000000'     *IN(20)                        *IN20 - 26
     C                   MOVEA     '0000000'     *IN(27)                           27 - 33
     C                   MOVEA     '00'          *IN(34)                           34 - 35
     C                   MOVEA     '00'          *IN(54)                           54 - 55
     C                   MOVEA     '000'         *IN(57)                           57 - 59
     C                   MOVEA     '0'           *IN(68)                           68
     C                   MOVEA     '000'         *IN(94)                           94 - 96
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *
      *  IF NOT EQUAL TO BLANKS, OCCUPATION CODE MUST BE ON FILE
      *
     C     SCOCCD        IFNE      *BLANKS
     C                   MOVEL(P)  SCOCCD        XXKEY
     C     OCKY          CHAIN     POC01                              60
     C     *IN60         IFEQ      *ON
     C     OCSTAT        OREQ      'I'
     C                   MOVE      *ON           *IN75
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   ELSE
     C                   MOVEL(P)  OCDESC        SCOCD2
     C                   END
     C                   ELSE                                                   SCOCCD=*BLANK
     C     CLTID         IFEQ      '065'
     C                   MOVE      *ON           *IN95
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   ENDIF
     C                   END                                                    SCOCCD
     C     CLTID         IFEQ      '132'
     C                   MOVEL(P)  SCOCDE        XXOCDS           40
     C     LO:UP         XLATE(P)  XXOCDS        XXOCDS
     C                   MOVEA     XXOCDS        OCD
     C                   Z-ADD     1             I                 2 0
     C                   Z-ADD     0             SNPOS             2 0
     C                   DO        40
     C*
     C     OCD(I)        SCAN      CONOCD        SNPOS             2 0
     C*
     C                   ADD       1             I
     C*
     C     SNPOS         IFEQ      0
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN21
     C                   MOVE      *ON           *IN22
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ENDDO
     C                   ENDIF
      *
      *  IF ENTERED, DEPARTMENT MUST BE ON FILE
      *
     C     SCDEPT        IFNE      *BLANK
     C                   MOVEL(P)  SCDEPT        XXDEPT
     C     DPKY          CHAIN     PDP01                              60
     C     *IN60         IFEQ      *ON
     C     DPSTAT        OREQ      'I'
     C                   MOVE      *ON           *IN79
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
     C                   END                                                    SCDEPT
      *
      *  EDIT DATE OF HIRE
      *
      *    ERHIDT WILL = Y AFTER RETURNING FROM A SEARCH AND KEYING
      *    IN AN INVALID DATE.
      *
     C     SCHIDT        IFEQ      *ZERO
     C     CLTID         ANDEQ     '065'
     C                   MOVE      *ON           *IN96
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   ENDIF
     C*
     C*    If flag CXDOHR = Y, Hire date cannot be zeroes
     C     CXKY          CHAIN     PCX01                              60
     C     *IN60         IFEQ      *OFF
     C     CXDOHR        ANDEQ     'Y'
     C     SCHIDT        ANDEQ     *ZERO
     C                   SETON                                        9036
     C                   LEAVE
     C                   ENDIF                                                  e- 60 = OFF ...
     C     SCHIDT        IFNE      *ZERO
     C     ERHIDT        OREQ      'Y'
     C                   Z-ADD     SCHIDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK                         DATE CHECK
     C                   MOVEL(P)  SRDTO         SCHIDT
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN87
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
      *
      * HIRE DATE
     C                   Z-ADD     SCHIDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         XXHIDT
      *
      *  THE DATE OF HIRE CANNOT BE > THE ACCIDENT DATE
      *
     C     XXHIDT        IFGT      XXACDT
     C                   MOVE      *ON           *IN31
     C                   MOVE      *ON           *IN87
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
      *
      *  THE DATE OF BIRTH CANNOT BE > THE DATE OF HIRE
      *
     C     XXDTOB        IFGT      XXHIDT
     C                   MOVE      *ON           *IN31
     C                   MOVE      *ON           *IN26
     C                   MOVE      *ON           *IN68
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
     C                   END
      *
      *  CALCULATE THE CLAIMANT TENURE AT TIME OF ACCIDENT
      *
     C     SCHIDT        IFNE      *ZERO
      *
     C                   Z-ADD     SCHIDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         XXHID3
     C                   Z-ADD     CMACDT        XXACD3
      *
     C     XXHIDY        IFGT      XXACDY
     C                   Z-ADD     1             X                 3 0
     C     XXACMO        LOOKUP    MTH(X)                                 60
     C     *IN60         IFEQ      *ON
     C                   ADD       DAY(X)        XXACDY
     C                   SUB       1             XXACMO
     C     XXACMO        IFEQ      *ZERO
     C                   SUB       1             XXACYR
     C                   ADD       12            XXACMO
     C                   END
     C                   END
     C                   END
      *
      *
     C     XXHIMO        IFGT      XXACMO
     C                   SUB       1             XXACYR
     C                   ADD       12            XXACMO
     C                   END
      *
     C     XXACD3        SUB       XXHID3        XXTENR
     C                   MOVE      XXTEYR        SCTEYR
     C                   MOVE      XXTEMO        SCTEMO
     C                   END
      *
      *  EMPLOYMENT STATUS, DESC
      *
     C     SCCES         IFNE      *BLANK
     C                   MOVEL(P)  'EMST'        XXTBID
     C                   MOVEL(P)  SCCES         XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   MOVE      *ON           *IN83
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   ELSE
     C                   MOVEL(P)  GTDESC        SCCESD
     C                   END
     C                   ELSE                                                   SCCES=*BLANK
     C     CLTID         IFEQ      '065'
     C                   MOVE      *ON           *IN94
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   ENDIF
     C                   END
      *
      *  Category 2 entry must be entered for groups: 350, 400, 410,
      *  420, 430, 440
      *
     C     CLTID         IFEQ      '114'
      *
     C     *IN90         IFEQ      *OFF
     C     FLG114        ANDEQ     'Y'
     C     SCCTG2        ANDEQ     *BLANKS
     C                   SETON                                        12  90
     C                   ENDIF
      *
      *  Category 2 Code must be valid if entered.
      *
     C     *IN90         IFEQ      *OFF
     C     SCCTG2        ANDNE     *BLANKS
      ***** Beg Chg ***** 05/25/12 ***************************************
     C*****C4KY          CHAIN     PC401                              60
     C     C2KY          CHAIN     PC201                              60
      ***** End Chg ***** 05/25/12 ***************************************
      *
     C     *IN60         IFEQ      *ON
      ***** Beg Chg ***** 05/25/12 ***************************************
     C*****C4STAT        OREQ      'I'
     C     C2STAT        OREQ      'I'
      ***** End Chg ***** 05/25/12 ***************************************
     C                   SETON                                        14  90
     C                   ENDIF
      *
     C                   ENDIF
     C                   ENDIF
      *
      *
      *  THE # DAYS/WEEK MUST BE 0-7
      *
     C     SCDAYS        IFLT      *ZERO
     C     SCDAYS        ORGT      7
     C                   MOVE      *ON           *IN30
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
      *
      *  THE HOURS PER DAYS MUST NOT BE GREATER THAN 24
      *
      *
      *  CHECK SHIFT
      *
     C     SCSHF         IFNE      *BLANK
     C                   MOVEL(P)  'SHFT'        XXTBID
     C                   MOVEL(P)  SCSHF         XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   MOVE      *ON           *IN84                          INVALID TAB VAL
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
      *
     C                   END
     C*
     C* RELEASE SS# AUTHORIZATION MUST BE Y OR BLANK
     C*
     C     SCSSRL        IFNE      'Y'
     C     SCSSRL        ANDNE     ' '
     C                   SETON                                        8090
     C                   LEAVE
     C                   ENDIF
      *
      *  EDIT DATE REPORTED TO EMPLOYER
      *
     C                   Z-ADD     SCDTRP        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK                         DATE CHECK
     C                   MOVEL(P)  SRDTO         SCDTRP
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN34
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
      *
      * DATE REPORTED TO EMPLOYER
     C                   Z-ADD     SCDTRP        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         XXDTRP
      *
     C     XXDTRP        IFLT      XXACDT
     C                   MOVE      *ON           *IN88
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
      *
      *  THE DATE REPORTED CANNOT BE > THE SYSTEM DATE.
      *
     C     XXDTRP        IFGT      XXDT
     C                   MOVE      *ON           *IN34
     C                   MOVE      *ON           *IN88
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
      *
      *  THE METHOD OF CONTACT MUST BE:  M=MAIL,
      *      T=TELEPHONE,  F=FAX,  OR  P=IN PERSON
      *
     C     SCMCON        IFNE      *BLANK
     C                   MOVEL(P)  'MCON'        XXTBID
     C                   MOVEL(P)  SCMCON        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   MOVE      *ON           *IN35
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
     C                   END
      *
      *  EDIT CARRIER RECEIVED DATE
      *
     C                   Z-ADD     SCRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK                         DATE CHECK
     C                   MOVEL(P)  SRDTO         SCRCVD
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN89
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
      *
      * CARRIER RECEIVED DATE
     C                   Z-ADD     SCRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         XXRCVD
     C     XXRCVD        IFLT      XXACDT
     C                   MOVE      *ON           *IN85
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
      *
      *  THE DATE RECEIVED CANNOT BE > THE SYSTEM DATE.
      *
     C     XXRCVD        IFGT      XXDT
     C                   MOVE      *ON           *IN85
     C                   MOVE      *ON           *IN89
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
      *  Late Reason Code
     C     SCLTRE        IFNE      *BLANK
     C                   MOVEL(P)  'LRSN'        XXTBID
     C                   MOVEL(P)  SCLTRE        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   MOVE      *ON           *IN42                          INVALID TAB VAL
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   ENDIF                                                  *IN60 = *ON
     C                   ENDIF                                                  SCLERE <> *BLANKS
      *
      *  TPA CONTACT DATE
      *
     C     SCTPAD        IFNE      *ZEROS
     C     ERTPAD        OREQ      'Y'
     C                   Z-ADD     SCTPAD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK                         DATE CHECK
     C                   MOVEL(P)  SRDTO         SCTPAD
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN33
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
      *
      * TPA CONTACT DATE
     C                   Z-ADD     SCTPAD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         XXTPAD
      *
     C     XXTPAD        IFLT      XXACDT
     C                   MOVE      *ON           *IN86
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
      *
      *  THE TPA DATE CANNOT BE > THE SYSTEM DATE.
      *
     C     XXTPAD        IFGT      XXDT
     C                   MOVE      *ON           *IN33
     C                   MOVE      *ON           *IN86
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
     C                   END                                                    SCTPAD
      *
     C                   ENDDO
      *
      *  If another user has updated this claim, send an error message.
      *
     C     @@RRN         CHAIN(N)  DBRCDF                             99
      *
      ***** Beg Chg ***** 05/25/12 ***************************************
     C*****  N99SVDATE        IFNE      C2DT
     C*****     SVTIME        ORNE      C2TIME
     C  N99SVDATE        IFNE      x_DT
     C     SVTIME        ORNE      x_TIME
      ***** End Chg ***** 05/25/12 ***************************************
      *
     C                   CALL      'WA017R'
      ***** Beg Chg ***** 05/25/12 ***************************************
     C*****              PARM                    C2USER
     C                   PARM                    x_USER
      ***** End Chg ***** 05/25/12 ***************************************
     C                   PARM                    QQRTCD
      *
     C                   MOVE      *ON           *IN90
     C                   ENDIF
      *
      *  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
      *
     C                   Z-ADD     2             SCRN#
     C                   ENDSR                                                  ERRCK2
      *
      *===============================================================
      * ERRCK3 - SUBROUTINE FOR ERROR CHECKING SCR03
      *===============================================================
      *
     C     ERRCK3        BEGSR
      *
      *  If another user has updated this claim, send an error message.
      *
     C     @@RRN         CHAIN(N)  DBRCDF                             99
      *
      ***** Beg Chg ***** 05/25/12 ***************************************
     C*****  N99SVDATE        IFNE      C2DT
     C*****  SVTIME        ORNE      C2TIME
     C  N99SVDATE        IFNE      x_DT
     C     SVTIME        ORNE      x_TIME
      ***** End Chg ***** 05/25/12 ***************************************
      *
     C                   CALL      'WA017R'
      ***** Beg Chg ***** 05/25/12 ***************************************
     C*****              PARM                    C2USER
     C                   PARM                    x_USER
      ***** End Chg ***** 05/25/12 ***************************************
     C                   PARM                    QQRTCD
      *
     C                   MOVE      *ON           *IN90
     C                   ENDIF
      *
      *  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
      *
     C                   Z-ADD     3             SCRN#
     C                   ENDSR                                                  ERRCK3
      *** Beg Add ***** 05/25/12 **************************************
      ****************************************************************
      *  ERRCK4 - SUBROUTINE FOR ERROR CHECKING SCR04                *
      ****************************************************************
      /Free
       BegSr  Errck4;
       Endsr;
      /End-Free
      ****************************************************************
      *  ERRCK5 - SUBROUTINE FOR ERROR CHECKING SCR03                *
      ****************************************************************
      /Free
       BegSr  Errck5;
       Endsr;
      /End-Free
      *** End Add ***** 05/25/12 **************************************
      *===============================================================
      * KGDO - SUBROUTINE FOR F7
      *===============================================================
      *
      *  F7=CLAIMANT FINANCIAL INFO WINDOW
      *
     C     KGDO          BEGSR
      *
     C                   CALL      'WA009R'
     C     *INKC         PARM                    @@KC
     C     *INKL         PARM                    @@KL
     C                   PARM                    @@MODE
     C                   PARM                    @@RRN
      *
      * IF F3=EXIT IS HIT, RETURN TO CLAIM MENU.
      *
     C     *INKC         IFEQ      *ON
     C                   MOVE      *OFF          *IN90
     C                   MOVE      'D'           @@MODE
     C     Q2RECF        IFEQ      'SCR01'
     C                   Z-ADD     1             #OFSCR
     C                   END
     C     Q2RECF        IFEQ      'SCR02'
     C                   Z-ADD     2             #OFSCR
     C                   END
     C                   END
      *
     C                   ENDSR
      *
     C*===============================================================
     C* KHDO - F8=WITNESS INFO
     C*===============================================================
     C     KHDO          BEGSR
      *
     C                   MOVE      CMFND         #2FND
     C                   MOVE      CMFYR         #2FYR
     C                   MOVE      CMCASE        #2CASE
      *
     C                   CALL      'W0070RA'
     C                   PARM      *BLANKS       ##PRDS           40
     C                   PARM      @@CALL        ##CALL            1
     C                   PARM                    #2FND             3
     C                   PARM                    #2FYR             3
     C                   PARM                    #2CASE            7
     C                   PARM      *BLANKS       ##RTCD            1
      *
     C                   ENDSR
      *
      *===============================================================
      * KJDO - SUBROUTINE FOR F10
      *===============================================================
      *
      *  F10=TABLE SEARCH
      *
     C     KJDO          BEGSR
     C                   MOVEA     '00'          *IN(18)
     C                   MOVEA     '0'           *IN(28)
     C                   MOVEA     '0000'        *IN(36)                           36 - 39
     C                   MOVEA     '0000000'     *IN(41)                           41 - 47
     C                   MOVEA     '00000'       *IN(48)                           48 - 52
     C                   MOVEA     '00'          *IN(55)                           55 - 56
     C                   MOVEA     '0'           *IN(82)                           82
     C                   MOVEA     '0'           *IN(11)                           11
     C                   MOVEA     '0'           *IN(65)                           65
     C                   MOVEL     ##DATA        QQPRDS           40
     C                   MOVE      *BLANK        QQRTCD            1
     C                   MOVE      *BLANK        QQCALL            1
      *
      *  CLAIMANT COUNTY
      *
     C                   SELECT
     C     HFLD          WHENEQ    'SCCCCD'
     C                   MOVE      '2'           ##MODE
     C                   MOVE      *BLANK        XXKEY
     C                   CALL      'W2215R'      ACDPL4                 99
     C     ##RRN         CHAIN     PCN0P                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN48                          POS CUSR
     C                   ELSE
     C                   MOVE(P)   CNKEY         XX8A              8
     C                   MOVEL(P)  XX8A          SCCCCD
     C                   MOVEL(P)  CNKEY         XXKEY
     C                   MOVEL(P)  CNDESC        SCCCDE
     C                   MOVE      *ON           *IN65                          POS CUSR
     C                   END
     C*
     C*
     C     HFLD          WHENEQ    'SCCTRY'
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
     C                   MOVE      *ON           *IN38                          POS CUSR
     C                   END
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  QQKEY         SCCTRY
     C                   MOVEL(P)  'CTRY'        XXTBID
     C                   MOVEL(P)  QQKEY         XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCCTRD
     C                   MOVE      *ON           *IN39                          POS CUSR
     C                   END
     C                   END
     C*
     C*
      *
      *  PAYROLL CLASS CODE
      *
     C     HFLD          WHENEQ    'SCCLAS'
      *
     C     XXEDTL        IFEQ      '0'
     C                   MOVE      '2'           ##MODE
     C                   Z-ADD     DBRRN2        ##RRN2
     C                   CALL      'W0060R'      ACDPL                  99
      *
      *  GET THE PAYROLL CLASS CODE
      *
     C     ##RRN         CHAIN     PEP0P                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     EPCLAS        SCCLAS
     C                   MOVEL     EPSPEC        SCSPEC
     C                   MOVEL     EPEXT         SCEXT
     C                   MOVE      *ON           *IN41                          DT OF BIRTH
     C                   ELSE
     C                   MOVE      *ON           *IN71                          POS CUSR
     C                   END
      *
      *  GET MOST CURRENT CLASS CODE DESCRIPTION
      *
     C                   Z-ADD     CMACDT        XXEFDT
     C                   MOVE      *BLANK        SCCLAD
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  60
     C     *IN60         IFEQ      *OFF
     C     PRSTAT        ANDEQ     'A'
     C                   MOVEL(P)  PRDESC        SCCLAD
     C                   END
     C     EPEXT         IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXST
     C     PXKY          SETGT     PPX01
     C     PXKY1         READPE    PPX01                                  60
     C  N60              MOVEL(P)  PXDESC        SCCLAD
     C   60RSKY          CHAIN     PRS01                              61
     C  N61
     CAN 60              MOVE      RSJRST        XXST
     C  N61
     CAN 60PXKY          SETGT     PPX01
     C  N61
     CAN 60PXKY1         READPE    PPX01                                  60
     C  N61
     CANN60              MOVEL(P)  PXDESC        SCCLAD
     C                   ENDIF                                                  EPEXT IFNE
     C                   ELSE
     C                   MOVE      *ON           *IN92
     C                   ENDIF
      *
     C     HFLD          WHENEQ    'SCEXT'
      *
     C                   CALL      'W0650R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    ##RRN             9 0
     C                   PARM                    XXCO#
     C                   PARM                    SCCLAS
     C                   PARM                    RSJRST
      *
     C     *INKC         IFEQ      *ON
     C                   MOVE      *ON           *INKC
     C                   ENDIF
      *
     C     ##RRN         IFEQ      *ZEROS
     C                   MOVE      *ON           *IN77
     C                   ELSE
      *
     C     ##RRN         CHAIN     PPX02                              99
     C     *IN99         IFEQ      *OFF
     C     PXSTAT        ANDEQ     'I'
     C                   SETON                                        11  90
     C                   SETON                                        71
     C                   ENDIF
      *
     C                   MOVE      PXEXT         SCEXT
     C                   MOVEL(P)  PXDESC        SCCLAD
     C                   ENDIF
      *  GET THE STANDARD INDUSTRY CODE
      *
      *  MARITAL STATUS
      *
     C     HFLD          WHENEQ    'SCMSTS'
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
     C                   MOVE      *ON           *IN47                          POS CUSR
     C                   END
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  QQKEY         SCMSTS
     C                   MOVEL(P)  'MSTS'        XXTBID
     C                   MOVEL(P)  QQKEY         XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCMSTD
     C                   MOVE      *ON           *IN37                          POS CUSR
     C                   END
     C                   END
     C*
     C*  Method of contact
     C*
     C     HFLD          WHENEQ    'SCMCON'
     C                   MOVEL(P)  'MCON'        QQTBID
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
     C                   MOVE      *ON           *IN49                          POS CUSR
     C                   END
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  QQKEY         SCMCON
     C                   MOVEL(P)  'MCON'        XXTBID
     C                   MOVEL(P)  QQKEY         XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCMCND
     C                   MOVE      *ON           *IN50                          POS CUSR
     C                   END
     C                   END
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * SCREEN 2
      *
      *  OCCUPATIONAL CODE
      *
     C     HFLD          WHENEQ    'SCOCCD'
      *
     C                   MOVEL     *BLANKS       @@KEY
     C                   MOVEL     *BLANKS       @@DESC
      *
     C                   CALL      'W2265R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    @@KC
     C     *INKL         PARM                    @@KL
     C                   PARM                    @@KEY
     C                   PARM                    @@DESC
      *
     C     @@KEY         IFNE      *BLANKS
     C                   MOVEL(P)  @@KEY         SCOCCD
     C                   MOVEL(P)  @@DESC        SCOCD2
      *
     C                   MOVE      *ON           *IN43                          POS CUSR
     C                   ELSE
     C                   MOVE      *ON           *IN38                          POS CUSR
     C                   ENDIF                                                  @@KEY
      *
      *  DEPARTMENT
      *
     C     HFLD          WHENEQ    'SCDEPT'
      *
     C                   MOVE      '2'           ##MODE
     C                   Z-ADD     DBRRN4        ##RRN2
     C                   CALL      'W1050R'      ACDPL
      *
     C     ##RRN         IFNE      *ZERO
     C                   MOVE      *ON           *IN39
     C     ##RRN         CHAIN     PDP0P                              60
     C                   MOVEL     DPDEPT        SCDEPT
     C                   MOVEL     DPDESC        SCDEPD
     C                   ELSE
     C                   MOVE      *ON           *IN43
     C                   ENDIF                                                  ##RRN<>*BLANKS
      *
      *  EMPLOYMENT STATUS
      *
     C     HFLD          WHENEQ    'SCCES'
     C                   MOVEL(P)  'EMST'        QQTBID
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
     C                   MOVE      *ON           *IN51                          POS CUSR
     C                   END
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  QQKEY         SCCES
     C                   MOVEL(P)  'EMST'        XXTBID
     C                   MOVEL(P)  QQKEY         XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCCESD
     C                   MOVE      *ON           *IN44                          POS CUSR
     C                   END
     C                   END
      *  ID TYPE QUALIFIER
      *
     C     HFLD          WHENEQ    'SCEITQ'
     C                   CALL      'W2493R'
     C                   PARM                    ##PRDS
     C                   PARM      '2'           ##CALL
     C                   PARM      'EIDQ'        XXTBID
     C                   PARM      *BLANKS       XXKEY
     C                   PARM      *BLANKS       ##RTCD
     C     XXKEY         IFNE      *BLANKS
     C                   MOVEL     XXKEY         SCEITQ
     C                   MOVE      *ON           *IN44
     C                   ENDIF
      *
      *  Category 2
      *
     C     HFLD          WHENEQ    'SCCTG2'
     C     CLTID         ANDEQ     '114'
     C                   CALL      'W2340R'
     C                   PARM      *BLANK        #1KC              1
     C                   PARM      *BLANK        #1KL              1
     C                   PARM      '2'           #1CALL            1
     C                   PARM      *ZERO         #1RRN             9 0          WTC2P
     C                   PARM      SCFND         #1FND             3 0          SUBSET GROUP
     C                   PARM      *BLANKS       #1CTG2           10
      *
     C     #1CTG2        IFEQ      *BLANKS
     C                   SETON                                        44
      *
     C                   ELSE
      *
     C                   MOVEL(P)  #1CTG2        SCCTG2
      ***** Beg Chg ***** 05/25/12 ***************************************
     C*****C4KY          CHAIN     PC401                              60
     C*****              MOVEL(P)  C4DESC        SCCOVN
     C     C2KY          CHAIN     PC201                              60
     C                   MOVEL(P)  C2DESC        SCCOVN
      ***** End Chg ***** 05/25/12 ***************************************
     C                   SETON                                        45
     C                   ENDIF                                                  #1RRN
      *
      *
      *  SHIFT
      *
     C     HFLD          WHENEQ    'SCSHF'
     C                   MOVEL(P)  'SHFT'        QQTBID
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
     C                   MOVE      *ON           *IN52                          POS CUSR
     C                   END
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  QQKEY         SCSHF
     C                   MOVE      *ON           *IN56                          POS CUSR
     C                   MOVEL(P)  'SHFT'        XXTBID
     C                   MOVEL(P)  QQKEY         XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCSHFD
     C                   END
     C                   END
      *  Late Reason Code
     C     HFLD          WHENEQ    'SCLTRE'
     C                   MOVEL(P)  'LRSN'        QQTBID
     C                   MOVE(P)   *BLANK        QQKEY
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQTBID
     C                   PARM                    QQKEY
     C                   PARM                    QQRTCD
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   ENDIF                                                  QQRTCD = 'A'
     C     QQRTCD        IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
     C                   MOVE      *ON           *IN47                          POS CUSR
     C                   ENDIF                                                  QQRTCD = 'B'
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  QQKEY         SCLTRE
     C                   MOVEL(P)  'LRSN'        XXTBID
     C                   MOVEL(P)  QQKEY         XXKEY
     C                   MOVE      *ON           *IN37                          POS CUSR
     C                   ENDIF                                                  QQRTCD = '*'
     C*
      *
     C                   OTHER
     C                   MOVE      *ON           *IN18
     C                   ENDSL
      *
     C                   ENDSR                                                  KJDO
      *
      *===============================================================
      * KNDO - SUBROUTINE FOR F14 = DARTS
      *===============================================================
      *
     C     KNDO          BEGSR
      *
      *  ESTABLISH KEY
      *
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      CMCO#         QCCO#
     C                   MOVE      CMFND         QCFND
     C                   MOVE      CMFYR         QCFYR
     C                   MOVE      CMCASE        QCCASE
      *
      *  CALL DARTS ENTRY PROGRAM
      *
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM      'W0070R'      XXPGM
      *
     C                   ENDSR                                                  KNDO
      *
      *===============================================================
      * MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
      *===============================================================
      *
     C     MVDBSC        BEGSR
      *
     C                   MOVEA     '0000000'     *IN(20)                        *IN20 - 26
     C                   MOVEA     '0000000'     *IN(27)                           27 - 33
     C                   MOVEA     '00'          *IN(34)                           34 - 35
     C                   MOVEA     '00'          *IN(54)                           54 - 55
     C                   MOVEA     '000'         *IN(57)                           57 - 59
     C                   MOVEA     '0'           *IN(68)                           68
      *
     C     *IN40         IFEQ      *ON
     C                   MOVEL     MODEA         SCMODE                         ADD MODE
     C                   END
      *
      *  LOAD KEYS
     C                   Z-ADD     CMCO#         XXCO#
     C                   Z-ADD     CMFND         XXFND
     C                   Z-ADD     CMEMP#        XXEMP#
     C                   Z-ADD     CMEMP#        SVEMP#
     C                   Z-ADD     CMDIV         XXDIV
     C                   Z-ADD     CMDIV         SVDIV
     C                   Z-ADD     CMFYR         XXFYR
     C                   Z-ADD     CMCASE        XXCASE
     C                   Z-ADD     CMCASE        SVCASE
      ***** Beg Chg ***** 05/25/12 ***************************************
     C*****              Z-ADD     C2DT          SVDATE
     C*****              Z-ADD     C2TIME        SVTIME
     C                   Z-ADD     x_DT          SVDATE
     C                   Z-ADD     x_TIME        SVTIME
      ***** End Chg ***** 05/25/12 ***************************************
      *
      *  GET EXCESS REINSURANCE INFO
      *
     C                   MOVE      *BLANK        SCMSG1
     C                   MOVE      *OFF          *IN69
     C     CMEXCS        IFNE      'Y'
     C     @@RRN         IFNE      *ZERO
     C                   CALL      'WA010R'
     C                   PARM                    @@RRN                          FOR WMCMP
     C                   PARM                    XXFNDR           13 2
     C                   PARM                    XXEXRE            1
     C                   END
     C                   END
      *
     C     XXEXRE        IFEQ      'Y'
     C     CMEXCS        OREQ      'Y'
     C                   MOVEL(P)  XCEXRE        SCMSG1
     C                   MOVE      *ON           *IN69
     C                   END
      *
      * GET SYSTEM CONTROL FILE
      *
     C                   MOVEL(P)  'SY'          XXSYK
     C     SYKY          CHAIN     SSY01                              60
     C     *IN60         IFEQ      *OFF
      *
      * BAR CODE INSTALLED
      *
     C     SYBCIF        IFEQ      'Y'
     C                   MOVE      XXCO#         QQCO#
     C                   MOVE      XXFND         QQFND
     C                   MOVE      XXFYR         QQFYR
     C                   MOVE      XXCASE        QQCASE
     C                   CALL      'BA001R'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQBCFL            1
     C                   PARM                    QQTRTP            1
     C                   PARM                    QQTUSR           10
     C                   PARM                    QQRTCD            1
      *
     C                   MOVE      *OFF          *IN70
     C                   MOVE      *BLANK        SCMSG2
     C     QQBCFL        IFEQ      'Y'
     C                   MOVE      *ON           *IN70
     C     QQTRTP        IFEQ      'I'
     C                   MOVEL(P)  XCBC02        SCMSG2
     C                   ELSE
     C                   MOVEL(P)  XCBC01        SCMSG2
     C                   CAT       QQTUSR:1      SCMSG2
     C                   END
     C                   END
     C                   END
      *
     C                   END                                                    *IN60  SSY01
      *
      *  GET THE EMPLOYER DETAIL RECORD
      *
     C     ELKY          CHAIN     PEL01                              60
     C                   MOVE      *IN60         XXEDTL
      *
      *  GET THE DARTS SETUP CONTROL RECORD
      *
     C     CXKY          CHAIN     PCX01                              60
      *
      *  RETRIEVE CATEGORY 2 HEADING FROM CLAIMS/DARTS CONTROL FILE.
      *
      /If Not Defined (WEB)
     C                   MOVE(P)   DOTS          SCCAT2
      /Endif
     C     *IN60         IFEQ      *OFF
     C     CXCAT2        ANDNE     *BLANKS
     C     CXCAT2        CAT       ' ':0         SCCAT2
     C                   ELSE
     C     XXCAT2        CAT       ' ':0         SCCAT2
     C                   ENDIF
      *
      * SCREEN DISPLAY RULE: YOU CAN'T HAVE ONLY ONE DOT IN PROMPT TEXT
      *
     C     ' '           CHECKR    CXCAT2        XXLEN             3 0
     C     XXLEN         IFEQ      24
     C     XXLEN         OREQ      23
     C                   MOVEL(P)  CXCAT2        SCCAT2
     C                   END
      *
     C                   Z-ADD     CMFND         SCFND
     C                   MOVE      *BLANK        SCFNAM
      *
      *  GET FUND NAME & IF FUND IS EXPIRED, THEN DISPLAY IN INQUIRY MODE
      *
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  FNNAME        SCFNAM
     C     FNEXDT        IFGT      *ZERO
     C     FNEXDT        IFLE      XXDT
     C                   MOVE      *ON           *IN09
     C                   MOVE      'D'           @@MODE
     C                   MOVEL(P)  MODED         SCMODE
     C                   END
     C                   END
     C                   END
      *
      *  GET GROUP DETAIL RECORD
      *    IF RELAX CLAIM COVERAGE EDITS (F1RLXC) NOT 'Y',
      *       DO NOT HIGHLIGHT FIELD; IS IT NOT REQUIRED.
      *
     C     F1KY          CHAIN     PF101                              60
     C     *IN60         IFEQ      *OFF
     C     F1RLXC        IFNE      'Y'
     C                   MOVE      *ON           *IN53                          DSPATR(HI)
     C                   END
     C                   END
      *
      *  GET EMPLOYER MASTER RECORD
      *
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
      *
      * CONCATENATE EMP #, DIV, & NAME
      *
     C                   CALL      'WA005R'
     C                   PARM                    XXEMP#
     C                   PARM                    XXDIV
     C                   PARM                    EMNAME
     C     SCEDN         PARM                    XXEDN            80
      *
      * Get employer cancel date
      *
     C                   Z-ADD     5             X                 3 0
      *
     C     ED(X)         DOWEQ     *ZERO
     C     X             ANDGT     1
     C                   SUB       1             X
     C                   ENDDO
      *
     C     CD(X)         IFNE      *ZERO
     C                   SETOFF                                       17
     C                   Z-ADD     CD(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         SCCXDT
      *
     C                   ELSE
      *
     C                   SETON                                        17
     C                   ENDIF                                                  CD,X
      *
     C                   ELSE
      *
     C                   SETON                                        17
     C                   END                                                    WMEMP
      *
      *  CLAIMANT SS#
      *
     C     CMSS#         IFNE      *BLANK
      ***** Beg Add ***** 05/25/12 ***************************************
     c                   movel     cmSS#         ##SS#
      ***** End Add ***** 05/25/12 ***************************************
     C     SS1           CAT(P)    '-':0         SCSS#
     C     SCSS#         CAT       SS2:0         SCSS#
     C     SCSS#         CAT       '-':0         SCSS#
     C     SCSS#         CAT       SS3:0         SCSS#
      *
      *  CLAIMANT NAME
      *
     C                   MOVE      *BLANK        SCCNAM
      *
     C                   CLEAR                   PCD01
     C                   MOVE      *OFF          *IN81
     C                   MOVEL(P)  CMSS#         XXSS#
     C     CDKY          CHAIN(N)  PCD01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN81
     C     CDLSN         CAT(P)    ',':0         SCCNAM                         LAST, FIRST M.
     C     SCCNAM        CAT       CDFSN:1       SCCNAM
     C     CDMID         IFNE      *BLANK
     C     SCCNAM        CAT       CDMID:1       SCCNAM
     C     SCCNAM        CAT       '.':0         SCCNAM
     C                   END
      *
     C                   ELSE
     C     *IN09         IFEQ      *OFF
     C                   MOVE      *ON           *IN40
     C                   MOVEL     MODEA         SCMODE                         ADD MODE
     C                   END
     C                   END                                                    *IN60
      *
     C                   END                                                    CMSS# = BLANK
      *  ACCIDENT DATE
      *
     C     CMACDT        IFNE      *ZERO
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                         YMD --> MDY
     C                   MOVEL(P)  SRDTO         SCACDT
     C                   ELSE
     C                   MOVE      *BLANK        SCACDT
     C                   END
      *
      *  IF CLAIM WAS CONVERTED FROM ANOTHER SYSTEM, DISPLAY OLD CLAIM #
      *
     C     CMCVT#        IFNE      *BLANK
     C                   MOVEL     CMCVT#        SCCVT
     C                   MOVE      *ON           *IN64
     C                   END
      *  FYR-CASE
     C                   MOVE(P)   CMFYR         XAFYR             3
     C                   MOVE(P)   CMCASE        XACASE            7
     C     XAFYR         CAT(P)    '-':0         SCYRCS
     C     SCYRCS        CAT       XACASE:0      SCYRCS
      *
     C     CMFYR         IFEQ      *ZERO
     C     CMCASE        OREQ      *ZERO
     C                   MOVE      *BLANK        SCYRCS
     C                   END
      *
      *  AGENGY CLAIM# (OR, SATE CLAIM#)
      *
     C                   MOVEL(P)  CMACL#        SCSCL#
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  THE EDIT (AIRCRAFT SURCHARGE) RELIES ON THE FOLLOWING CODE
      *  TO GET THE XXJRST KEY FIELD.
      *  IF THIS RETREIVAL CHANGES, WE MUST GET THAT INFO IN ERRCK1.
      *
      *  GET THE JURIDICTION STATE FROM THE
      *          EMPLOYER DETAIL & RATE SET CODE FILES
      *
     C                   MOVE      *BLANK        XXJRST
     C     ELKY          CHAIN     PEL01                              60
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     RSJRST        XXJRST
     C                   END
      *
      *  CLAIMANT NAME
     C                   MOVEL(P)  CDLSN         SCLSN                          LAST NAME
     C                   MOVEL(P)  CDFSN         SCFSN                          FIRST
     C                   MOVEL(P)  CDMID         SCMID                          MIDDLE INITIAL
     C                   MOVEL(P)  CDSEX         SCSEX
      *
     C                   MOVEL(P)  CDMSTS        SCMSTS                         MARITAL STATUS
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
      *  DATE OF BIRTH
     C     CDDTOB        IFNE      *ZERO
     C                   Z-ADD     CDDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         SCDTOB
      *
      *  CALCULATE THE CLAIMANT'S AGE AT TIME OF ACCIDENT
      *
     C     CMACDT        SUB       CDDTOB        XXAGE             7 0
     C                   MOVEL(P)  XXAGE         XX3A              3
     C                   MOVE      XX3A          SCCAGE
     C                   END                                                    CDDTOB
      *
      *  DON'T CALCULATE AGE IF THE DEFAULT WAS ENTERED.
      *
     C     SCDTOB        IFEQ      *ZERO
     C                   MOVE      *BLANK        SCCAGE
     C                   END
      *
     C                   MOVEL(P)  CDADR1        SCCAD1
      *
     C                   MOVEL(P)  CDADR2        SCCAD2
     C                   MOVEL(P)  CDCTY         SCCCTY
     C                   MOVEL(P)  CDST          SCCST
     C                   MOVEL(P)  CDZIP         SCCZP1
     C                   MOVE(P)   CDZIP         SCCZP2
      *  HOME PHONE
     C                   Z-ADD     CDAREA        SCAREA
     C                   MOVEL     CDPHN#        SCHPN1
     C                   MOVE      CDPHN#        SCHPN2
      *
      *  GET COUNTY DESCRIPTION
      *
     C                   MOVE(P)   *BLANK        SCCCCD
     C                   MOVE(P)   *BLANK        SCCCDE
     C                   MOVEL(P)  CDCNTY        SCCCCD
     C     CDST          CAT(P)    CDCNTY:0      XXKEY
     C     TBKY          CHAIN     PCN01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  CNDESC        SCCCDE
     C                   END
     C*
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
     C*
      * NUMBER OF DEPENDENTS
     C                   MOVEL(P)  CM#DEP        SCCDEP
      *  WORK PHONE
     C                   Z-ADD     CDWARA        SCWARA
     C                   MOVEL     CDWPH#        SCWPN1
     C                   MOVE      CDWPH#        SCWPN2
      *
      * OTHER EMPLOYEE ID
     C                   MOVEL     CMEID         SCEID
     C                   MOVEL     CMEITQ        SCEITQ
      *  ACCIDENT DATE, TIME
     C     CMACDT        IFNE      *ZERO
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         SCACD2
     C                   MOVEL     CMACTM        SCACTM
     C                   ELSE
     C                   MOVE      *BLANK        SCACD2
     C                   MOVE      *BLANK        SCACTM
     C                   END
      *
      *   GET THE MOST CURRENT EMPLOYER EFFECTIVE & CANCEL DATES
      *
      *
      *  COMPARE THE ACCIDENT DATE TO THE EFFECTIVE DATE
      *  IF ACC DATE IS >= TO EFF DATE, GET OUT OF LOOP.
      *
      *
      *  VALIDATE THE EFFECTIVE DATE
      *
      *
      *
      *  CHECK FOR A CANCEL DATE
      *
      *
      *
      *
      *
      *
      *  GET PAYROLL CLASS CODE & SPECIAL DESIGNATION
      *
     C                   Z-ADD     CMCLAS        SCCLAS
     C                   MOVEL     CMSPEC        SCSPEC
     C                   MOVEL     CMEXT         SCEXT
      *
      *  GET MOST CURRENT CLASS CODE DESCRIPTION
      *
     C                   Z-ADD     CMACDT        XXEFDT
     C                   MOVE      *BLANK        SCCLAD
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  60
     C     *IN60         IFEQ      *OFF
     C     PRSTAT        ANDEQ     'A'
     C                   MOVEL(P)  PRDESC        SCCLAD
     C                   END
     C     EPKY1         CHAIN     PEP01                              60
     C     *IN60         IFEQ      *OFF
     C     EPEXT         ANDNE     *BLANKS
     C     PXKY          SETGT     PPX01
     C     PXKY1         READPE    PPX01                                  60
     C  N60              MOVEL(P)  PXDESC        SCCLAD
     C   60RSKY          CHAIN     PRS01                              61
     C  N61
     CAN 60              MOVE      RSJRST        XXST
     C  N61
     CAN 60PXKY          SETGT     PPX01
     C  N61
     CAN 60PXKY1         READPE    PPX01                                  60
     C  N61
     CANN60              MOVEL(P)  PXDESC        SCCLAD
     C                   ENDIF                                                  *IN60 IFEQ
      *
      *
      *  GET SIC CODE & DESC
      *
     C                   MOVE      *BLANK        SCSIC
     C                   MOVE      *BLANK        SCSICD
     C     CMSIC         IFEQ      *BLANK
     C                   MOVEL     EMSIC         SCSIC
     C                   END
      *
     C                   MOVEL(P)  SCSIC         XXKEY
     C                   MOVEL(P)  'SIC'         XXTBID
     C     GTKY          CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCSICD
     C                   ENDIF
      *
      *  GET OCCUPATION DESC
     C                   MOVE      *BLANK        SCOCDE
     C                   MOVE      *BLANK        SCOCCD
     C                   MOVE      *BLANK        SCOCD2
     C                   MOVEL     CMOCDS        SCOCDE                         40A FREE FORM
     C                   MOVEL(P)  CMOCCD        SCOCCD                          3A OCC CODE
      *
     C     CMOCCD        IFNE      *BLANK                                        3A
     C                   MOVEL(P)  CMOCCD        XXKEY
     C     OCKY          CHAIN     POC01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  OCDESC        SCOCD2
     C                   END
     C                   END
      *
      *  GET DEPARTMENT DESC
     C                   MOVEL(P)  CMDEPT        SCDEPT
     C                   MOVE      *BLANK        SCDEPD
     C                   MOVEL(P)  CMDEPT        XXDEPT
     C     DPKY          CHAIN     PDP01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  DPDESC        SCDEPD
     C                   END
      *
     C                   MOVEL(P)  CMCTG2        SCCTG2
      ***** Beg Chg ***** 05/25/12 ***************************************
     C*****C4KY          CHAIN     PC401                              60
     C*****              MOVEL(P)  C4DESC        SCCOVN
     C     C2KY          CHAIN     PC201                              60
     C                   MOVEL(P)  C2DESC        SCCOVN
      ***** End Chg ***** 05/25/12 ***************************************
      *  HIRE DATE
      *
     C                   Z-ADD     CMHIDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         SCHIDT
      *
      *  EMPLOYMENT STATUS, DESC
     C                   MOVEL(P)  CMESTS        SCCES
     C                   MOVE      *BLANK        SCCESD
      *
     C     CMESTS        IFNE      *BLANK
     C                   MOVEL(P)  'EMST'        XXTBID
     C                   MOVEL(P)  CMESTS        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCCESD
     C                   END
     C                   END
      *
     C                   Z-ADD     CMDAYS        SCDAYS                         DAYS PER WEEK
     C                   MOVEL(P)  CMSHFT        SCSHF                          SHIFT
     C                   MOVE      *BLANK        SCSHFD
      *
     C     CMSHFT        IFNE      *BLANK
     C                   MOVEL(P)  'SHFT'        XXTBID
     C                   MOVEL(P)  CMSHFT        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCSHFD
     C                   END
     C                   END
      *
     C                   Z-ADD     CMHRSD        SCHRSD                         HOURS PER DAY
      *
      *  CALCULATE THE CLAIMANT TENURE AT TIME OF ACCIDENT
      *
     C                   MOVE      *BLANK        SCTEYR
     C                   MOVE      *BLANK        SCTEMO
      *
     C     CMHIDT        IFNE      *ZERO
     C                   Z-ADD     CMHIDT        XXHID3
     C                   Z-ADD     CMACDT        XXACD3
      *
     C     XXHIDY        IFGT      XXACDY
     C                   Z-ADD     1             X                 3 0
     C     XXACMO        LOOKUP    MTH(X)                                 60
     C     *IN60         IFEQ      *ON
     C                   ADD       DAY(X)        XXACDY
     C                   SUB       1             XXACMO
     C     XXACMO        IFEQ      *ZERO
     C                   SUB       1             XXACYR
     C                   ADD       12            XXACMO
     C                   END
     C                   END
     C                   END
      *
     C     XXHIMO        IFGT      XXACMO
     C                   SUB       1             XXACYR
     C                   ADD       12            XXACMO
     C                   END
      *
     C     XXACD3        SUB       XXHID3        XXTENR
     C                   MOVE      XXTEYR        SCTEYR
     C                   MOVE      XXTEMO        SCTEMO
     C                   END
      *
      *  TPA CONTACT DATE
      *
     C                   Z-ADD     CMTPAD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         SCTPAD
      *
     C                   MOVEL(P)  CMMCON        SCMCON                         CONTACT METHOD
     C*  Get method of contact description.
     C*
     C     SCMCON        IFNE      *BLANK
     C                   MOVEL(P)  'MCON'        XXTBID
     C                   MOVEL(P)  SCMCON        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCMCND
     C                   END
     C                   END
     C                   MOVEL     CMSSRL        SCSSRL
      *
      *  DATE REPORTED TO EMPLOYER
      *
     C                   Z-ADD     CMDTRP        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         SCDTRP
      *
     C                   MOVEL     SRDTO         ##DTRP            8 0
     C                   MOVEL(P)  CMDLY         ##MDLY            1
      *
      *  DATE CARRIER RECEIVED
      *
     C                   Z-ADD     CMRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         SCRCVD
      *
     C                   MOVE      'N'           XXWARN            1
     C                   MOVE      'N'           OVRFLG            1
      *  DARTS
     C                   MOVEL(P)  SCLSN         SVLSN
     C                   MOVEL(P)  SCFSN         SVFSN
     C                   MOVEL(P)  SCMID         SVMID
     C                   MOVEL(P)  SCCAD1        SVADR1
     C                   MOVEL(P)  SCCAD2        SVADR2
     C                   MOVEL(P)  SCCCTY        SVCTY
     C                   MOVEL(P)  SCCST         SVST
     C                   MOVEL(P)  SCCZP1        SVZIP
     C                   MOVE      SCCZP2        SVZIP
     C                   MOVE      *ON           *IN19
      *
     C                   MOVEL     SCCZP1        SVZIP1            5 0
     C                   MOVE      SCCZP2        SVZIP2            4 0
     C                   MOVEL     CMLTRE        SCLTRE
      ***** Beg Add ***** 05/25/12 ***************************************
      /free
       scEmad = CDemad;
      /end-free
      ***** End Add ***** 05/25/12 ***************************************
     C                   ENDSR                                                  MVDBSC
      *
      *===============================================================
      * MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
      *===============================================================
      *
     C     MVSCDB        BEGSR
      *
     C                   MOVEL     SCSCL#        CMACL#
      *  CLAIM DEMOGRAPHICS FILE
     C     CDKY          CHAIN     PCD01                              60
      *
     C                   MOVEL(P)  SCCZP1        XXZIP
     C                   MOVE      SCCZP2        XXZIP
     C                   MOVE      'N'           @WRTAH            1
     C                   TIME                    CHGDTM           14 0
     C                   MOVEL     CHGDTM        @AHTIM            6 0
     C                   MOVE      CHGDTM        @AHDT             8 0
     C                   Z-ADD     @AHDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         @@AHDT            7 0
      *
     C     SCFSN         IFNE      CDFSN
     C     SCMID         ORNE      CDMID
     C     SCLSN         ORNE      CDLSN
     C     SCCAD1        ORNE      CDADR1
     C     SCCAD2        ORNE      CDADR2
     C     SCCCTY        ORNE      CDCTY
     C     SCCST         ORNE      CDST
     C     XXZIP         ORNE      CDZIP
     C*
     C*   First make change to Payment Requests on this claim.
     C                   EXSR      SRPRCH
     C*
     C     *IN40         IFEQ      *OFF                                         NOT ADD MODE
     C                   EXSR      AUDRT                                        Auto DARTS
     C                   ENDIF
     C                   TIME                    CHGDTM           14 0
     C                   MOVEL     CHGDTM        @AHTIM            6 0
     C                   MOVE      CHGDTM        @AHDT             8 0
     C                   Z-ADD     @AHDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         @@AHDT            7 0
     C                   MOVE      'N'           @WRTAH            1
     C     *IN60         IFEQ      *OFF
     C                   MOVE      'Y'           @WRTAH
     C                   MOVE      'B'           @AHBAF            1
     C                   EXSR      ADDREC
     C                   ENDIF
     C                   ENDIF
      *
     C                   MOVEL(P)  CMSS#         CDSS#
     C                   MOVEL(P)  SCLSN         CDLSN
     C                   MOVEL(P)  SCFSN         CDFSN
     C                   MOVEL(P)  SCMID         CDMID
     C                   MOVEL(P)  SCCAD1        CDADR1
     C                   MOVEL(P)  SCCAD2        CDADR2
     C                   MOVEL(P)  SCCCTY        CDCTY
     C                   MOVEL(P)  SCCST         CDST
     C                   MOVEL(P)  SCCZP1        CDZIP
     C                   MOVE      SCCZP2        CDZIP
     C                   MOVEL(P)  SCSEX         CDSEX
     C                   MOVEL(P)  SCMSTS        CDMSTS
      *
     C                   Z-ADD     SCDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         CDDTOB
      *
     C                   Z-ADD     SCAREA        CDAREA
     C                   MOVEL     SCHPN1        CDPHN#
     C                   MOVE      SCHPN2        CDPHN#
     C                   Z-ADD     SCWARA        CDWARA
     C                   MOVEL     SCWPN1        CDWPH#
     C                   MOVE      SCWPN2        CDWPH#
     C                   MOVEL(P)  SCCCCD        CDCNTY
     C                   MOVEL(P)  SCCTRY        CDCTRY
     C                   MOVEL(P)  SCADR3        CDADR3
      *
     C                   MOVE      *BLANK        CDSTAT
     C                   MOVEL(P)  Q1USER        CDUSER
     C                   MOVEL(P)  Q1PGM         CDPGM
     C                   Z-ADD     DSTIME        CDTIME
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         CDDT
      *
     C     *IN60         IFEQ      *OFF
     C                   UPDATE    PCD01
     C     XXCMS         IFEQ      'Y'
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         XXEXDT
     C*
     C     RPKEY         SETGT     PD101
     C     RPKEY         READPE    PD101                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     D1TYPE        IFEQ      'I'
     C     D1RSDT        ANDEQ     *ZEROS
     C*
     C     CDSEX         IFNE      ' '
     C     CDDTOB        ANDNE     *ZEROS
     C                   MOVEL     @INF          D1RSPD
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         D1RSDT
     C                   UPDATE    PD101
     C                   ENDIF                                                  CDSEX <> ' '
     C*
     C                   LEAVE
     C*
     C                   ENDIF                                                  D1TYPE = 'I'
     C*
     C     RPKEY         READPE    PD101                                  60
     C*
     C                   ENDDO                                                  *IN60 = *OFF
     C*
     C                   ENDIF                                                  WCFL46 = 'Y'
     C     @WRTAH        IFEQ      'Y'
     C                   MOVE      'A'           @AHBAF
     C                   EXSR      ADDREC
     C                   ENDIF
     C                   ELSE
      *
     C                   MOVE      *BLANK        CDSTAT
     C                   MOVEL(P)  Q1USER        CDIUSR
     C                   MOVEL(P)  Q1PGM         CDIPGM
     C                   Z-ADD     DSTIME        CDITM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         CDIDT
     C                   WRITE     PCD01
     C                   MOVE      'B'           @AHBAF
     C                   EXSR      ADDREC
     C                   END
      *
      *    Call API to update Managed Care - FMCDP
      *
     C     XXMANC        IFEQ      *ON
     C                   MOVE      XXCO#         QQCO#
     C                   MOVE      XXFND         QQFND
     C                   MOVE      XXFYR         QQFYR
     C                   MOVE      XXCASE        QQCASE
      *
     C                   MOVE      CMACDT        QQACDT
      *
     C                   MOVE      CDZIP         QQZIP
     C                   Z-ADD     SCDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         QQDTOB
     C                   MOVE      CDAREA        QQAREA
     C                   MOVE      CDPHN#        QQPHN#
      *
     C                   MOVE      CDWARA        QQWARA
     C                   MOVE      CDWPH#        QQWPH#
      *
     C                   EXSR      MCAPID
     C                   ENDIF
      *
      *
      *  CLAIM MASTER FILE
     C                   Z-ADD     SCCDEP        CM#DEP
      *
     C                   MOVEL(P)  SCACTM        CMACTM
     C                   Z-ADD     SCCLAS        CMCLAS
     C                   MOVEL     SCSPEC        CMSPEC
     C                   MOVEL     SCEXT         CMEXT
      *
      * OTHER EMPLOYEE ID
     C                   MOVEL     SCEID         CMEID
     C                   MOVEL     SCEITQ        CMEITQ
      *  OCCUPATION DESC
     C                   MOVEL(P)  SCOCCD        CMOCCD
     C     *IN90         IFEQ      *OFF
     C                   MOVEL(P)  SCOCDE        CMOCDS
     C                   ENDIF
      *
     C                   MOVEL     SCDEPT        CMDEPT
     C                   MOVEL     SCCTG2        CMCTG2
      *
     C                   Z-ADD     SCHIDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         CMHIDT
      *
     C                   MOVEL     SCCES         CMESTS
     C                   Z-ADD     SCDAYS        CMDAYS
     C                   MOVEL     SCSHF         CMSHFT
     C                   MOVEL     SCSSRL        CMSSRL
     C                   Z-ADD     SCHRSD        CMHRSD
      *
     C                   Z-ADD     SCTPAD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         CMTPAD
      *
     C                   MOVEL     SCMCON        CMMCON
      *
     C                   Z-ADD     SCDTRP        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         CMDTRP
      *
      *
     C                   MOVEL     'N'           XXDTCG            1
      *
     C     SCDTRP        IFNE      ##DTRP
     C     SCDTRP        ANDNE     *ZEROS
     C     ##MDLY        ANDEQ     'Y'
      *
      * Calculate the "Decision date" to be "Date reported to employer"
      * plus 90 days
     C                   MOVEL     SCDTRP        SRDATE
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE            8 0
     C                   PARM      90            SRDAYS            5 0
     C                   PARM      *BLANKS       SRERR             1
      *
     C                   Z-ADD     SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         CMDCSN
      *
      * Create the Adjuster Comp diary
      *
     C                   MOVEL     'Y'           XXDTCG
     C                   Z-ADD     XXCO#         XXCO1#
     C                   Z-ADD     XXFND         XXFND1
     C                   Z-ADD     XXFYR         XXFYR1
     C                   Z-ADD     XXCASE        XXCAS1
     C                   EXSR      SRCLCH
     C                   ENDIF
      *
      *
     C                   Z-ADD     SCRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         CMRCVD
     C                   MOVEL(P)  SCLTRE        CMLTRE
      *
     C                   MOVEL(P)  SCLSN         CMLSN
     C                   MOVEL(P)  SCFSN         CMFSN
     C                   MOVEL(P)  SCMID         CMMID
      ***** Beg Add ***** 05/25/12 ***************************************
      /free
       CDemad = scEmad;
      /end-free
      ***** End Add ***** 05/25/12 ***************************************
     C                   ENDSR                                                  MVSCDB
     C*===============================================================
     C*   AUDIT  WRITE DART IF CLAIMANT INFO CHANGES
     C*===============================================================
     C*
     C     AUDIT         BEGSR
      *
     C                   MOVE      'N'           @@CHG             1
     C                   MOVE      ' '           @@AFL             1
     C     SCLSN         IFNE      SVLSN
     C     SVLSN         ANDNE     *BLANK
     C     SCFSN         ORNE      SVFSN
     C     SVFSN         ANDNE     *BLANK
     C     SCMID         ORNE      SVMID
     C     SVMID         ANDNE     *BLANK
     C     SCCAD1        ORNE      SVADR1
     C     SVADR1        ANDNE     *BLANK
     C     SCCAD2        ORNE      SVADR2
     C     *IN40         ANDEQ     *OFF
     C     SCCCTY        ORNE      SVCTY
     C     SVCTY         ANDNE     *BLANK
     C     SCCST         ORNE      SVST
     C     SVST          ANDNE     *BLANK
     C     SCCZP1        ORNE      SVZIP1
     C     SVZIP1        ANDNE     *ZERO
     C     SCCZP2        ORNE      SVZIP2
     C     SVZIP2        ANDNE     *ZERO
     C*
     C                   MOVE      'Y'           @@AFL
     C     SCCAD1        IFEQ      SVADR1
     C     SCCAD2        ANDEQ     SVADR2
     C     SCCCTY        ANDEQ     SVCTY
     C     SCCST         ANDEQ     SVST
     C     SCCZP1        ANDEQ     SVZIP1
     C     SCCZP2        ANDEQ     SVZIP2
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
     C     SCMID         ORNE      SVMID
     C     SVMID         ANDNE     *BLANK
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
     C                   ENDSR                                                  AUDIT
     C*
     C*===============================================================
     C*   AUDRT  CALL AUTO DARTS
     C*===============================================================
     C*
     C     AUDRT         BEGSR
     C                   CALL      'WA700R'
     C                   PARM      CMCO#         @@CO#             3 0
     C                   PARM      CMFND         @@FND             3 0
     C                   PARM      CMFYR         @@FYR             3 0
     C                   PARM      CMCASE        @@CASE            7 0
     C                   PARM      'CLMCHG01'    @@DKEY           10
     C                   PARM                    @@COMM          256
     C*
     C                   ENDSR                                                  AUDRT
     C*
     C*===============================================================
      *===============================================================
      * OTHERR - SUBROUTINE FOR PROCESSING OTHER MODES
      *===============================================================
      *
     C     OTHERR        BEGSR
      *
     C                   ENDSR                                                   OTHERR
      *
      *===============================================================
      * OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
      *===============================================================
      *
     C     OTHFKS        BEGSR
      *
     C                   MOVEA     '00'          *IN(18)
     C                   MOVEA     '0000000'     *IN(20)                        *IN20 - 26
     C                   MOVEA     '0000000'     *IN(27)                           27 - 33
     C                   MOVEA     '00'          *IN(34)                           34 - 35
     C                   MOVEA     '00'          *IN(54)                           54 - 55
     C                   MOVEA     '000'         *IN(57)                           57 - 59
     C                   MOVEA     '0'           *IN(68)                           68
     C                   MOVEA     '0'           *IN(11)                           11
      *
     C     *INKG         CASEQ     *ON           KGDO                     90     F7 FIN SUM
     C     *INKH         CASEQ     *ON           KHDO                     90     F8 WITNESS
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C     *INKN         CASEQ     *ON           KNDO                     90     DARTS
     C                   ENDCS
      *
      * CONVERT ALL DATES FIRST
      * DATE OF BIRTH
     C                   Z-ADD     SCDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK                         MDY --> YMD
     C                   MOVEL     SRDTO         SCDTOB
      *
      * HIRE DATE
     C                   MOVE      *BLANK        ERHIDT            1
     C     SCHIDT        IFNE      *ZERO
     C                   Z-ADD     SCHIDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         SCHIDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      'Y'           ERHIDT
     C                   END
     C                   END
      *
      * TPA CONTACT DATE
     C                   MOVE      *BLANK        ERTPAD            1
     C     SCTPAD        IFNE      *ZERO
     C                   Z-ADD     SCTPAD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         SCTPAD
     C     SRERR         IFEQ      '1'
     C                   MOVE      'Y'           ERTPAD
     C                   END
     C                   END
      *
      * DATE REPORTED TO EMPLOYER
     C                   Z-ADD     SCDTRP        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         SCDTRP
      * CARRIER RECEIVED DATE
     C                   Z-ADD     SCRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         SCRCVD
      *
     C                   ENDSR                                                  OTHFKS
      *
      *===============================================================
      * PGMEND - SUBROUTINE FOR ENDING THE PROGRAM
      *===============================================================
      *
     C     PGMEND        BEGSR
      *
      *  DELETE THE CLAIM RECORD IF THE USER PRESSED F3 IN ADD MODE
      *
     C     *IN40         IFEQ      *ON
     C                   MOVE      'A'           @@MODE
     C                   END
      *
      *  CLEAR THE ENTIRE IBM ARRAY (*IN)
      *
     C                   CLEAR                   *IN
      *
     C                   RETURN
     C                   ENDSR
      *
      *===============================================================
      * PGMSTR - SUBROUTINE FOR STARTING THE PROGRAM
      *===============================================================
      *
     C     PGMSTR        BEGSR
     C                   Z-ADD     2             #OFSCR            1 0          # OF SCREENS
     C                   MOVE      ' '           FLG114            1
      *
      *  CHECK THE MODE.  A=ADD, C=CHANGE, D=DISPLAY
      *
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
      *
     C     @@MODE        IFEQ      'A'
     C                   MOVE      'C'           @@MODE
     C                   MOVEL     MODEA         SCMODE
     C                   END
      *
     C     *IN09         IFEQ      *ON
     C                   MOVEL(P)  MODED         SCMODE
     C                   END
      *
     C                   MOVE      *ON           DSPTYP
      *
      * GET WORKERS COMP CONTROL FILE
      *
     C                   MOVEL(P)  'WC'          XXSYK
     C     SYKY          CHAIN     SWC01                              60
     C     *IN60         IFEQ      *OFF
      *
      * COMMERCIAL APPLICATION INSTALLED
      *
     C                   MOVE      *OFF          *IN66
     C     WCFL07        IFEQ      'Y'
     C                   MOVE      *ON           *IN66
     C                   END
      *
     C                   END
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
      *
      *  GET THE USER PARAMETER FILE
      *
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      *OFF
      *
      *  GET SECURITY LEVEL OF THE USER
      *
     C                   MOVE      *BLANK        XXAUTH            1
     C                   MOVE      *OFF          *IN67
     C     UPLVL         IFGE      WCSL03
     C                   MOVE      *ON           *IN67
     C                   MOVE      'Y'           XXAUTH                         AUTHORIZED
     C                   END
      *
      *  IF IN UPDATE MODE, CHECK IF USER HAS AUTHORITY TO UPDATE
      *  CLAIMANT'S NAME.
      *
     C                   MOVE      *OFF          *IN15
     C     UPFL2         IFNE      'Y'
     C     *IN40         ANDEQ     *OFF
     C                   MOVE      *ON           *IN15
     C                   END
      *
     C                   MOVEL     UPUSNM        QCUSER
     C                   END
      *
      *  GET CURRENT DATE, CONVERT TO 7,0  TO CHAIN TO FILES
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
      *
      *  CLEAR ALL SCREEN FIELDS
      *
     C                   MOVE      *BLANK        SCSCL#
     C                   MOVE      *BLANK        SCLSN
     C                   MOVE      *BLANK        SCFSN
     C                   MOVE      *BLANK        SCMID
     C                   MOVE      *BLANK        SCSEX
     C                   MOVE      *BLANK        SCMSTS
     C                   MOVE      *BLANK        SCDTOB
     C                   MOVE      *BLANK        SCCAD1
     C                   MOVE      *BLANK        SCCAD2
     C                   MOVE      *BLANK        SCCAGE
     C                   MOVE      *BLANK        SCCCTY
     C                   MOVE      *BLANK        SCCST
     C                   MOVE      *BLANK        SCCZP1
     C                   MOVE      *BLANK        SCCZP2
     C                   MOVE      *BLANK        SCAREA
     C                   MOVE      *BLANK        SCHPN1
     C                   MOVE      *BLANK        SCHPN2
     C                   MOVE      *BLANK        SCCCCD
     C                   MOVE      *BLANK        SCCTRY
     C                   MOVE      *BLANK        SCADR3
     C                   MOVE      *BLANK        SCCCDE
     C                   MOVE      *BLANK        SCCDEP
     C                   MOVE      *BLANK        SCWARA
     C                   MOVE      *BLANK        SCWPN1
     C                   MOVE      *BLANK        SCWPN2
     C                   MOVE      *BLANK        SCACD2
     C                   MOVE      *BLANK        SCACTM
     C                   MOVE      *BLANK        SCCLAS
     C                   MOVE      *BLANK        SCSPEC
     C                   MOVE      *BLANK        SCOCDE
     C                   MOVE      *BLANK        SCOCCD
     C                   MOVE      *BLANK        SCDEPT
     C                   MOVE      *BLANK        SCDEPD
     C                   MOVE      *BLANK        SCCTG2
     C                   MOVE      *BLANK        SCHIDT
     C                   MOVE      *BLANK        SCCES
     C                   MOVE      *BLANK        SCCESD
     C                   MOVE      *BLANK        SCDAYS
     C                   MOVE      *BLANK        SCSHF
     C                   MOVE      *BLANK        SCSSRL
     C                   MOVE      *BLANK        SCHRSD
     C                   MOVE      *BLANK        SCTPAD
     C                   MOVE      *BLANK        SCMCON
     C                   MOVE      *BLANK        SCDTRP
     C                   MOVE      *BLANK        SCRCVD
     C                   MOVE      *BLANK        SCCOVN
     C                   MOVE      *BLANK        SCLTRE
      *
     C* Initialize by-pass flags for Date of birth and gender soft edits
     C                   MOVE      'N'           FLGDOB            1
     C                   MOVE      'N'           FLGSEX            1
      *
      *  Get Employer Cross Reference info for header
      *
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C     EXKY          CHAIN     PEX01                              63
     C  N63              Z-ADD     EXOFND        SCOFND
     C  N63              Z-ADD     EXOEMP        SCOEMP
     C  N63              Z-ADD     EXODIV        SCODIV
      *
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C     CLTID         IFEQ      '065'
     C                   MOVE      *ON           *IN98
     C                   ELSE
     C                   MOVE      *OFF          *IN98
     C                   ENDIF
      *
      *
     C                   CALL      'S6000R'
     C                   PARM                    @@WCMC            1            WCMC INSTALLED
     C                   PARM                    @@CHK             1            CHECK REC
     C                   PARM                    @@BAR             1            INTRFACD W/WCMS
     C                   PARM                    @@DED             1
     C                   PARM                    @@ADMN            1
     C                   PARM                    @@CLM             1
     C                   PARM                    @@EDI             1
     C                   PARM                    @@MICR            1
     C     @@EDI         IFEQ      'Y'
     C                   MOVE      *ON           *IN13
     C                   ELSE
     C                   MOVE      *OFF          *IN13
     C                   ENDIF
      ***** Beg Add ***** 05/25/12 ***************************************
      /free
       x_stat = cmstat;
       x_user = cmuser;
       x_dt   = cmdt;
       x_time = cmtime;
       svEmad = CDemad;
      /end-free
      ***** End Add ***** 05/25/12 ***************************************
     C                   ENDSR                                                  PGMSTR
      *
      *===============================================================
      * PREOTH - SUBROUTINE FOR OTHER MODE PRE-PROCESSING
      *===============================================================
      *
     C     PREOTH        BEGSR
     C                   ENDSR
      *
      *===============================================================
      * UPDATR - SUBROUTINE FOR UPDATING DATA BASE RECORDS
      *===============================================================
      *
     C     UPDATR        BEGSR
      *
     C     @@RRN         CHAIN     DBRCDF                             9997
      *
B02  C     *IN97         IFEQ      *ON
B02  C     FLSTCM        ANDEQ     01218
     C                   CALL      'WA017R'
     C                   PARM      *BLANK        QQUSER           10
     C                   PARM                    QQRTCD
     C                   MOVE      *ON           *IN99                          SKIP TO BOTTOM
     C                   END
      *
B02  C     *IN99         IFEQ      *OFF
      *
      *
     C                   EXSR      MVSCDB
      *
      ***** Beg Chg ***** 05/25/12 ***************************************
     C*****              MOVEL     Q1USER        C2USER
     C                   MOVEL     Q1USER        x_USER
      ***** End Chg ***** 05/25/12 ***************************************
     C                   MOVEL     Q1PGM         CMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
      ***** Beg Chg ***** 05/25/12 ***************************************
     C*****              MOVEL     SRDTO         C2DT
     C*****              MOVEL     DSTIME        C2TIME
     C                   MOVEL     SRDTO         x_DT
     C                   MOVEL     DSTIME        x_TIME
      ***** End Chg ***** 05/25/12 ***************************************
      *
     C                   UPDATE    DBRCDF
      *
      *
      *  WRITE DARTS RECORD IF CLAIMANT NAME  WAS CHANGED
      *
     C     *IN40         IFEQ      *OFF
     C                   Z-ADD     XXCO#         XXCO1#
     C                   Z-ADD     XXFND         XXFND1
     C                   Z-ADD     XXFYR         XXFYR1
     C                   Z-ADD     XXCASE        XXCAS1
     C                   EXSR      SRCLCH
     C                   EXSR      SRUPCU
     C                   END
      *
      *  WRITE  CLAIMANT NAME CHANGE TRACKING RECORD WHEN NAME/ADDR CHG
      *
     C     *IN40         IFEQ      *OFF
     C                   EXSR      SRCLNT
     C                   END
      *
     C*  Changes the claimant's name in other claim records with the
     C*    same SS#, if the name is different.
      *
     C                   EXSR      SRNMCH
      *
     C                   MOVE      *ON           OUTFLG
      *
      *    Call API to update Managed Care Claim Master - FMCMP
      *
     C     XXMANC        IFEQ      *ON
     C                   CLEAR                   PARMCL
     C                   EXSR      MCAPIC
     C                   ENDIF                                                  XXMANC ON   DATE
      *
      *
      * CALL NEXT PROGRAM IF OPT 2 OR 5 WAS SELECTED FROM CLM MASTER
      *
     C     Q1PRMS        IFGE      5
     C     @@CONT        ANDNE     *BLANK
     C                   EXSR      CALLR
     C                   END
      *
     C     CMLSN         IFNE      SVLSN
     C     CMFSN         ORNE      SVFSN
     C     CMMID         ORNE      SVMID
     C                   CALL      'WA085R'
     C                   PARM      'C'           XXTYPE            1
     C                   PARM      DBRRN         XXRRN             9 0
     C                   PARM      *BLANK        XXRTN             1
     C                   ENDIF
E02  C                   ENDIF                                                  *IN99=*OFF
      *
     C                   ENDSR                                                  UPDATR
      *
      *===============================================================
      * WRITER - SUBROUTINE FOR WRITING DATA BASE RECORDS
      *===============================================================
      *
     C     WRITER        BEGSR
      *
     C                   ENDSR                                                  WRITER
      ****************************************************************
      *  SRUPCU - SUBROUTINE TO UPDATE CLAIM SECURITY FILE
      ****************************************************************
     C     SRUPCU        BEGSR
     C*** Get claims security record
     C     CUKY          CHAIN     PCU01                              60
     C     *IN60         IFEQ      *OFF
     C*** Build new claimant name
     C     SCLSN         CAT(P)    ',':0         XXCNM1           45
     C                   CAT       SCFSN:1       XXCNM1
     C                   CAT       SCMID:1       XXCNM1
     C                   ENDIF                                                  *IN60 = *OFF
     C     *IN60         DOWEQ     *OFF
     C*** Update all records for this claim
     C                   MOVEL(P)  XXCNM1        CUCNAM
     C                   UPDATE    PCU01
     C     CUKY          READE     PCU01                                  60
     C                   ENDDO                                                  *IN60 = *OFF
     C                   ENDSR
      *
      ****************************************************************
      *  SRCLCH - SUBROUTINE TO WRITE DARTS
      ****************************************************************
      *
     C     SRCLCH        BEGSR
      *
     C     SCLSN         IFNE      SVLSN
     C     SVLSN         ANDNE     *BLANK
     C     SCFSN         ORNE      SVFSN
     C     SVFSN         ANDNE     *BLANK
     C     SCMID         ORNE      SVMID
     C     SVMID         ANDNE     *BLANK
     C     XXDTCG        OREQ      'Y'
     C                   MOVE      *BLANKS       QCKEY
     C                   Z-ADD     XXCO1#        QCCO#
     C                   Z-ADD     XXFND1        QCFND
     C                   Z-ADD     XXFYR1        QCFYR
     C                   Z-ADD     XXCAS1        QCCASE
      *
     C*
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C                   MOVEL(P)  QCKEY         Q#KEY            40
      *
     C     XXDTCG        IFEQ      'Y'
     C                   MOVEL(P)  CXDSRC        Q#RMCD            5
     C                   MOVE      *BLANKS       QCLNEW
     C                   MOVEL     DRT(5)        QCDRT4
     C                   MOVEL(P)  QCLNEW        Q#COMM          256
     C                   MOVEL     'N'           XXDTCG
     C                   ELSE
      *
     C                   MOVEL(P)  CXRCNC        Q#RMCD            5
     C                   MOVE      *BLANKS       QCLNEW
     C                   MOVEL     DRT(4)        QCDRT4
     C                   MOVE      *BLANKS       QQFMN            15
     C                   MOVE      *BLANKS       QQFSN            15
     C                   MOVE      *BLANKS       QQMID            56
     C                   MOVE      '1'           QQTERM            1
     C                   MOVEL     SVFSN         QQFSN
     C                   MOVEL     SVMID         QQMID
     C                   CALL      'SRFRLS'
     C                   PARM                    QQMID
     C                   PARM                    QQFSN
     C                   PARM                    QQTERM
     C                   MOVEL     QQMID         QQFMN
     C                   MOVEL     '1'           QQTERM            1
     C                   MOVE      *BLANK        QQLSN            56
     C                   MOVEL     SVLSN         QQLSN
     C                   CALL      'SRNAME'
     C                   PARM                    QQLSN
     C                   PARM                    QQFMN
     C                   PARM                    QQTERM
     C                   MOVEL     QQLSN         QCLNAM
     C                   MOVEL(P)  QCLNEW        Q#COMM          256
     C                   ENDIF
      *
     C                   MOVE      *BLANKS       Q#COM2           19
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         Q#CDAT            8
     C                   MOVEL(P)  *BLANKS       Q#DCID           12
     C                   MOVEL(P)  'W0070R'      Q#PNAM           10
     C                   MOVE      *ZEROS        Q#SECL            3
     C                   MOVEL(P)  Q1USER        Q#USER           10
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C                   MOVEL(P)  *BLANKS       Q#FOLD           80
     C                   MOVEL(P)  *BLANK        Q#DIFL            1
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C                   MOVEL(P)  *BLANK        Q#FL01            1
     C                   MOVEL(P)  *BLANK        Q#FL02            1
     C                   MOVEL(P)  CXRCNC        Q#RMCD            5
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
      *
     C                   ENDSR
      *
      ****************************************************************
      *  SRCLNT - SUBR TO WRITE CLAIMANT NAME CHANGE TRACKING FILE
      ****************************************************************
      *
     C     SRCLNT        BEGSR
      *
     C                   MOVE      *BLANK        XTYPE1
     C                   MOVE      *BLANK        XTYPE2
     C                   MOVE      *BLANK        NTTYPE
      *
     C     SCLSN         IFNE      SVLSN
     C     SVLSN         ANDNE     *BLANK
     C     SCFSN         ORNE      SVFSN
     C     SVFSN         ANDNE     *BLANK
     C     SCMID         ORNE      SVMID
     C     SVMID         ANDNE     *BLANK
     C                   MOVE      'N'           XTYPE1
     C                   MOVE      'N'           NTTYPE
     C                   END
      *
     C                   MOVEL     SCCZP1        SCZIP
     C                   MOVE      SCCZP2        SCZIP
      *
     C     SCCAD1        IFNE      SVADR1
     C     SVADR1        ANDNE     *BLANK
     C     SCCAD2        ORNE      SVADR2
     C     SVADR2        ANDNE     *BLANK
     C     SCCCTY        ORNE      SVCTY
     C     SVCTY         ANDNE     *BLANK
     C     SCCST         ORNE      SVST
     C     SVST          ANDNE     *BLANK
     C     SCZIP         ORNE      SVZIP
     C     SVZIP         ANDNE     *ZERO
     C                   MOVE      'A'           XTYPE2
     C                   MOVE      'A'           NTTYPE
     C                   END
      *
     C     XTYPE1        IFNE      *BLANK
     C     XTYPE2        ANDNE     *BLANK
     C                   MOVE      'B'           NTTYPE
     C                   END
     C     NTTYPE        IFNE      *BLANK
     C                   MOVE      *BLANKS       QQFMN            15
     C                   MOVE      *BLANKS       QQFSN            15
     C                   MOVE      *BLANKS       QQMID            56
     C                   MOVE      '1'           QQTERM            1
     C                   MOVEL     SVFSN         QQFSN                          ORIGINAL FIRST
     C                   MOVEL     SVMID         QQMID                                   MID
     C                   CALL      'SRFRLS'
     C                   PARM                    QQMID
     C                   PARM                    QQFSN
     C                   PARM                    QQTERM
     C                   MOVEL     QQMID         QQFMN
      *
     C                   MOVEL     '1'           QQTERM            1
     C                   MOVE      *BLANK        QQLSN            56
     C                   MOVEL     SVLSN         QQLSN
     C                   CALL      'SRNAME'                                     ORIG LAST,FIRST
     C                   PARM                    QQLSN
     C                   PARM                    QQFMN
     C                   PARM                    QQTERM
     C                   MOVEL     QQLSN         NTONAM
      *
     C                   MOVE      *BLANKS       QQFMN
     C                   MOVE      *BLANKS       QQFSN
     C                   MOVE      *BLANKS       QQMID
     C                   MOVE      '1'           QQTERM
     C                   MOVEL     SCFSN         QQFSN                          NEW FIRST MID
     C                   MOVEL     SCMID         QQMID
     C                   CALL      'SRFRLS'
     C                   PARM                    QQMID
     C                   PARM                    QQFSN
     C                   PARM                    QQTERM
     C                   MOVEL     QQMID         QQFMN
      *
     C                   MOVEL     '1'           QQTERM
     C                   MOVE      *BLANK        QQLSN
     C                   MOVEL     SCLSN         QQLSN                          NEW LAST
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
     C                   MOVEL     SCCAD1        NTNAD1
     C                   MOVEL     SCCAD2        NTNAD2
     C                   MOVEL     SCCST         NTNST
     C                   MOVEL     SCCCTY        NTNCTY
     C                   MOVEL     SCCZP1        NTNZIP
     C                   MOVE      SCCZP2        NTNZIP
     C                   Z-ADD     XXCO#         NTCO#
     C                   Z-ADD     XXFND         NTFND
     C                   Z-ADD     XXFYR         NTFYR
     C                   Z-ADD     XXCASE        NTCASE
      ***** Beg Add ***** 05/25/12 ***************************************
      /free
       NTomad = svEmad;
       NTnmad = scEmad;
      /end-free
      ***** End Add ***** 05/25/12 ***************************************
     C                   MOVE      Q1USER        NTUSER
     C                   MOVE      'A'           NTSTAT
     C                   TIME                    DSDTTM
     C                   Z-ADD     XXDT          NTDT
     C                   MOVE      NTDT          NTTRDT
     C                   MOVEL     Q1PGM         NTPGM
     C                   Z-ADD     DSTIME        NTTIME
     C                   WRITE     PNT01
     C                   END
      *
     C*  If the claimant name was changed, update all claims with the
     C*    same SS#.
     C*  Reset the claim fields back to the original claim.
     C**********
      *
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
     C     RPKYB         CHAIN     PRP01                              61
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
     C                   MOVEL     SCCAD1        RPADR1
     C                   MOVEL     SCCAD2        RPADR2
     C                   MOVEL     SCCCTY        RPCTY
     C                   MOVEL     SCCST         RPST
     C                   MOVEL(P)  SCCZP1        RPZIP
     C                   MOVE      SCCZP2        RPZIP
     C                   MOVEL(P)  SCCTRY        RPCTRY
     C                   MOVEL(P)  SCADR3        RPADR3
     C*
     C*    Use new name for payment record
     C                   MOVEL     *BLANKS       XXPNAM           40
     C     SCFSN         CAT       SCMID:1       XXPNAM
     C                   CAT       SCLSN:1       XXPNAM
     C                   MOVEL     XXPNAM        RPPNAM
     C*
     C*
     C                   MOVEL     Q1USER        RPUSER
     C                   MOVEL     Q1PGM         RPPGM
     C                   Z-ADD     XXDT          RPDT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        RPTIME
     C                   UPDATE    PRP01
     C                   ENDIF                                                   . RPNAM=CD
     C                   ENDIF                                                   . RPADR=CD
     C*
     C     RPKYB         READE     PRP01                                  61
     C                   ENDDO                                                   . IN60 OFF
     C*
     C     CMKYB         READE(N)  PCM0B                                  99
     C                   ENDDO                                                     IN99 OFF
     C*
     C     @@RRN         CHAIN     DBRCDF                             9997
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C* SRNMCH - This subroutine changes the claimant's name in
     C*   other claim records with the same SS#.
     C*===============================================================
     C     SRNMCH        BEGSR
     C*
     C*  If the claimant name was changed, update all claims with the
     C*    same SS#.
     C*
     C                   MOVEL     CMSS#         XXSS#
     C     CMKYB         CHAIN     PCM0B                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     SCFSN         IFNE      CMFSN
     C     SCLSN         ORNE      CMLSN
     C     SCMID         ORNE      CMMID
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
     C                   EXSR      SRCLCH
     C                   ENDIF
     C*
     C     CMKYB         READE     PCM0B                                  60
     C                   ENDDO
     C*
     C*  Reset the claim fields back to the original claim.
     C*
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C                   ENDSR
      *===============================================================
      * MCAPID - DATA INTERFACE: MC DEMOGRAPHICS - FMCDP
      *===============================================================
      *
     C     MCAPID        BEGSR
      *
     C                   CALL      'FA002R'
      *
     C                   PARM      *BLANK        QQRTCD            1
      *
     C                   PARM      CDSS#         QQSS#             9
     C                   PARM                    QQACDT            7
      *
     C                   PARM                    QQCO#             3
     C                   PARM      *BLANK        QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
      *
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
     C                   ENDSR
      *
      *===============================================================
      * MCAPIC - DATA INTERFACE: MC CLAIM MASTER - FMCMP
      *===============================================================
      *
     C     MCAPIC        BEGSR
      *
     C                   MOVE      CMACDT        QMACDT
      *
     C                   MOVE      CMEMP#        QMEMP#
     C                   MOVE      CMDIV         QMDIV
      *
     C                   MOVE      XXCO#         QMCO#
     C                   MOVE      *BLANKS       QMSUB#
     C                   MOVE      XXFND         QMFND
     C                   MOVE      XXFYR         QMFYR
     C                   MOVE      XXCASE        QMCASE
      *
     C                   MOVE      CMACTM        QMACTM                         Accidnt time
     C                   MOVE(P)   CMDTRP        QMDTRP                         Reported
     C                   MOVE(P)   CMDTLW        QMDTLW                         Last worked
     C                   MOVE(P)   CMENTD        QMENTD                         ENTERED
     C                   MOVE(P)   CMRCVD        QMRCVD                         Received
     C                   MOVE(P)   CMMMI         QMMMI                          Max med impr
     C                   MOVE(P)   CMDOD         QMDEDT                         Death date
     C     CMDTH         IFEQ      'Y'                                          Death flag
     C                   MOVEL     'DI'          QMDEEM
     C                   ELSE
     C                   MOVEL     *BLANKS       QMDEEM
     C                   ENDIF
      *
     C                   Z-ADD     DBRRN         QMRRN
      *
     C                   MOVE(P)   CMDTRE        QMDTRE                         Rehabilitation
     C                   MOVE      CM#DEP        QM#DEP                         # dependents
     C                   MOVEL(P)  CMSHFT        QQWKPT                         Shift
      *
      *=====================================================================
     C                   CALL      'FA003R'
      *
     C                   PARM      *BLANK        QMRTCD            1
     C                   PARM      CMSS#         QMSS#             9
     C                   PARM                    QMACDT            7
      *
     C                   PARM                    QMCO#             3
     C                   PARM      *BLANK        QMSUB#            5
     C                   PARM                    QMFND             3
     C                   PARM                    QMFYR             3
     C                   PARM                    QMCASE            7
      *
     C                   PARM                    QMEMP#            9
     C                   PARM                    QMDIV             5
     C                   PARM                    QMACTM            6
     C                   PARM                    QMDTRP            7
     C                   PARM                    QMDTLW            7
     C                   PARM      CMACDS        QMACDS           60            Acc description
     C                   PARM      CMMCON        QMMCON            1            Method contact
     C                   PARM                    QMENTD            7
     C                   PARM                    QMRCVD            7
     C                   PARM                    QMMMI             7
     C                   PARM      *BLANKS       QM1A1             1            Unavailable
     C                   PARM      *BLANKS       QM10A1           10            Unavailable
     C                   PARM      CMLTFL        QMLTFL            1            Lost time flag
     C                   PARM      CMACL#        QMSCL#           25            State claim numbr
     C                   PARM      *BLANKS       QMMCTM            4            Unavailable
     C                   PARM      *BLANKS       QMSTWK            2            Unavailable
     C                   PARM                    QMDEDT            7
     C                   PARM                    QMDEEM            2            Death claim flag
     C                   PARM      CMNRSE        QMCMNU            3            Nurse
     C                   PARM      *BLANKS       QMCMSU            3            Unavailable
     C                   PARM      CMCLEX        QMCLAD            3            Claim examiner
     C                   PARM                    QMRRN             9 0
     C                   PARM      CMLSN         QMLSN            30            Last name
     C                   PARM      CMFSN         QMFSN            15            First name
     C                   PARM      CMMID         QMMID             1            Middle initial
     C                   PARM                    QMDTRE            7
     C                   PARM      CMSUPR        QMCLSU            3            Supervisor
     C                   PARM                    QM#DEP            3
     C                   PARM      CMOCDS        QQOCDS           40            OCDS
     C                   PARM      CMHIDT        QQHIDT            7 0          HIDT
     C                   PARM      CMESTS        QQESTS            2            ESTS
     C                   PARM                    QQWKPT            2            SHFT
     C                   PARM      CMPREX        QQPREX            1            PREX
     C                   PARM      CMACD2        QQACD2           60            ACD2
     C                   PARM      CMCVFL        QQCVFL            1            CVFL
     C                   PARM      CMMCO         QQMCA             1            MCO
     C                   PARM      CMIPCT        QQMMIP            5 2          IPCT
     C                   PARM      CMODIS        QQODIS            1            ODIS
     C                   PARM      CMYRLX        QQYRLX            4 0          YRLX
     C                   PARM      CMDBDT        QQDBDT            7 0          DBDT
     C                   PARM      CMHRSD        QQHRDY            3 0          HRSD
     C                   PARM      CMDAYS        QQDYWK            1 0          DAYS
     C                   PARM      CMFEAD        QQCLFA            3            FEAD
     C                   PARM      CMDNY         QQDNY             1             DENIED
     C                   PARM      CMIMP         QQIMP             1             IMPAIRMENT
     C                   PARM      CMJRST        QQJRST            2            JUR STATE
     C                   PARM      CMJRST        QQPOST            2            POLICY STE
      *
     C                   ENDSR
      *
      *===============================================================
      * ADDREC - Store the Image in WDAHP                            *
      *===============================================================
      *
     C     ADDREC        BEGSR
      *
     C                   CLEAR                   PAH01
     C                   MOVE      XXCO#         AHCO#
     C                   MOVE      XXFND         AHFND
     C                   MOVE      XXFYR         AHFYR
     C                   MOVE      CMCASE        AHCASE
     C                   MOVE      @AHBAF        AHBAFL
     C                   MOVE      CDLSN         AHLSN
     C                   MOVE      CDFSN         AHFSN
     C                   MOVE      CDMID         AHMID
     C                   MOVE      CDADR1        AHADR1
     C                   MOVE      CDADR2        AHADR2
     C                   MOVE      CDCTY         AHCTY
     C                   MOVE      CDST          AHST
     C                   MOVE      CDZIP         AHZIP
     C                   MOVE      CDCTRY        AHCTRY
     C                   MOVE      CDADR3        AHADR3
     C                   MOVE      ' '           AHCKAL
     C                   MOVE      Q1PGM         AHPGM
     C                   MOVE      Q1USER        AHUSER
     C                   MOVE      @AHTIM        AHTIME
     C                   MOVE      @@AHDT        AHDT
     C                   WRITE     PAH01
      *
     C                   ENDSR
      *===============================================================
      *
**  ARRAY DRT
CLAIMANT NAME & CITY:
ACCIDENT DATE:
USER NAME:
OLD CLAIMANT NAME:
DECISION DATE CHANGE
**  ARRAY MM
0131
0229
0331
0430
0531
0630
0731
0831
0930
1031
1130
1231
