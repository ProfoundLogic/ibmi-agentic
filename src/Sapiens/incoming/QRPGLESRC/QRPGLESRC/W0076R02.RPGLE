     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0076R02 - ENTITY/LOCATION INFORMATION
     H*****************************************************************
     H*                                                               *
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
     H*                OR ADD RECORDS IN THE ENTITY/LOCATION FILE.    *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0076R - WORK WITH ENTITY/LOCATION                         *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*  CREATED: 07/24/97  PROGRAMMER: C:33  PROJECT: 60AND          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  07/06/11 C:33  Project: 25WRE      SY255 Priss #119          *
     H*                 Make sure the proper heading is displayed for *
     H*                  the 'State Risk ID #' everytime.             *
     H*                                                               *
     H*  01/04/10 C:190 PROJECT: 25WRE                                *
     H*                 "State Risk ID #" heading on SCR02 is         *
     H*                 soft-coded to use field WDRRP/RRSEMH.         *
     H*                 If file field WDRRP/RRSEMH is blank then      *
     H*                 display "State Risk ID #" as heading.         *
     H*                                                               *
     H* 09/09/09 C:33  Project: WC1118   SY184 Priss#4                *
     H*                Update the county code as appropriate.         *
     H*                                                               *
     H*  04/13/09 C:190 PROJECT 114WTU                                *
     H*                 Changed the prompt entries for the DBA and    *
     H*                 Federal names to be two lines of 50 characters*
     H*                 each.                                         *
     H*                                                               *
     H* 03/26/09 C:227 SUPPORT: WA099 62414                           *
     H*                AUTO POPULATE NAICS CODE WHEN ADDING ENTITY/   *
     H*                LOCATION FOR EMPLOYER.                         *
     H*                                                               *
     H* 07/23/08 C:136 SUPPORT: WA132 64464                           *
     H*                ENSURE THAT SIC CODE IS POPULATED ONLY IF      *
     H*                ADMIN/DARTS IS FLAGGED TO REQUIRE IT           *
     H*                                                               *
     H* 07/22/08 C:136 PROJECT: WC1708 PRISS 3                        *
     H*                MAKE GROUP SELF INSURED # AND STATE EMP #      *
     H*                CLIENT SPECIFIC TO ECM.                        *
     H*                                                               *
     H* 07/16/08 C:136 SUPPORT: WA099 63588                           *
     H*                POSITION CURSOR TO EFF DT ON ADD PER GW        *
     H*                                                               *
     H* 07/14/08 C:136 PROJECT: WC1708                                *
     H*                DEFAULT  STATE UNEMPLOYMENT #, NAIC CODE, AND  *
     H*                GROUP SELF-INSURED # FROM WMEMP WHEN ADDING    *
     H*                A LOCATION                                     *
     H*                                                               *
     H* 06/20/08 C:189 PROJECT: SY156                                 *
     H*                IN case of LOCATION,RISK ID will be defaulted  *
     H*                from WMEMP and stored in WMALP                 *
     H*
     H* 07/18/07 C:180 PROJECT: PRISS3 - WC1527
     H*                Allow lower case entry for
     H*                DOING BUSINESS AS and FEDERAL NAME.
     H*                NO CHANGES TO PGM, JUST DISPLAY FILE W0076F02.
     H*
     H*                                                               *
     H* 06/15/07 C:136 SUPPORT: WA136 58686                           *
     H*                PROTECT ALL FIELDS WHEN DELETING               *
     H*                                                               *
     H*  05/25/07 C:190 PROJECT WC1527                                *
     H*                 Remove nm/addr restriction on Location.       *
     H*                                                               *
     H* 11/16/06 C:136 SUPPORT: WA139 55534                           *
     H*                CHANGE USE OF *IN40 ON ZIP CODE EDIT TO *IN72  *
     H*                TO ALLOW MID-TERM ENDORSEMENTS TO PROCESS      *
     H*                                                               *
     H* 09/20/06 C:136 PROJECT: 71WHE (BACK TO BASE)                  *
     H*               -ADD STATE UNEMPLOYMENT #                       *
     H*               -ADD GROUP SELF-INSURED #                       *
     H*                                                               *
     H* 09/13/06 C:132 PROJECT: WA109  54759                          *
     H*              - Change display of NAIC Code to highlight if    *
     H*                required, and always display the Field on 2nd  *
     H*                screen.                                        *
     H*                NOTE:  NO CHANGES TO PGM CODE, JUST COMPILED.  *
     H*                                                               *
     H* 05/12/06 C:132 PROJECT: WC1432                                *
     H*              - ADD NEW FIELD FOR NAICS CODE TO SECOND SCREEN. *
     H*                CHECK STAT RPT/RATING TO DETERMINE IF THE      *
     H*                NAICS CODE IS REQUIRED.                        *
     H*              - PROVIDE SEARCH FOR THE NAICS CODE.             *
     H*                                                               *
     H* 04/25/06 C:65  PROJECT: 132WAY                                *
     H*                ADD NEW FIELDS TO DESIGNATE EMPLOYER NAMES AS  *
     H*                PERSONAL NAMES.  FLAGGED FOR CLIENTS 132 AND   *
     H*                136 AS THIS IS A CA REQUIREMENT.               *
     H*                                                               *
     H* 03/24/06 C:168 PROJECT: 132WET                                *
     H*                Added new field (# of employees per shift)     *
     H*                For earthquake insurance.                      *
     H*                                                               *
     H* 10/12/05 C:65  PROJECT: 132WAY                                *
     H*                ADD NATURE OF BUSINESS TO ENTITY SETUP.        *
     H*                                                               *
     H* 05/10/05 C:132 PROJECT WC1118 - WA114 - 46977                 *
     H*                REMOVE Rest of edits on County Code added on   *
     H*                03/17/05                                       *
     H*                                                               *
     H* 04/25/05 C:132 PROJECT WC1118 - WA071 - 46556 & 46650         *
     H*                REMOVE some of edits on County Code added on   *
     H*                03/17/05, allow for missing Zip Codes          *
     H*                                                               *
     H* 03/07/05 C:65  PROJECT: WC1202                                *
     H*                ADD PROCESSING TO POPULATE EXTENDED EMPLR NAMES*
     H*                                                               *
     H* 03/17/05 C:132 PROJECT WC1118                                 *
     H*                ADD new edits on County Code                   *
     H*                                                               *
     H* 10/17/03 C:61  PROJECT WC1152                                 *
     H*                Auto fill Location data on entry (ADD mode).   *
     H*                                                               *
     H* 09/30/03 C:61  SUPPORT WA129 39275 WC1119                     *
     H*                Correct state unemployement # edit.            *
     H*                                                               *
     H* 07/16/03 C:136 Support: WA129 #38008                          *
     H*                Edit for state unemployment # invoked          *
     H*                incorrectly.                                   *
     H*                                                               *
     H* 05/06/03 C:132 PROJECT WC1118 & WC1119 Changes                *
     H*                PROJECT WC1118                                 *
     H*                In Add Mode, check ZIPCODE BY COUNTY file for  *
     H*                valid county code, and load into SCCNTY on the *
     H*                second screen. Get from file WMCZP             *
     H*                                                               *
     H*                PROJECT WC1119                                 *
     H*                In Add or Change Mode, Check for the state     *
     H*                unemployment # flag in WDRPP/RRURFL to see if  *
     H*                state uemployment # is required. If so, issue  *
     H*                error message if blank, field on screen two.   *
     H*                                                               *
     H* 02/23/01  C:35   PROJECT: WC577                               *
     H*                  ADDED CODE FOR STATE UNEMPLOYMENT NUMBER.    *
     H*                                                               *
     H* 01/02/01 C:61  PROJECT WC577                                  *
     H*              - Clear screens after update/display.            *
     H*                                                               *
     H* 10/11/00  C:90  PROJECT: WC952                                *
     H*                 ALLOW ENTRY/DISPLAY OF "STATE RISK ID#".      *
     H*                                                               *
     H* 04/27/00  C:53  SUPPORT: WC065/19768                          *
     H*                 SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.*
     H*                                                               *
     H* 03/10/00  C:118 SUPPORT: WA048/21972                          *
     H*                 WHEN UPDATING OR ADDING A LOCATION, THE TYPE  *
     H*                 OF BUSINESS FIELD (SCTPOB) SHOULD BE DISPLAY  *
     H*                 ONLY.                                         *
     H*                                                               *
     H* 11/18/99  C:33  SEQ#: WA107/20382                             *
     H*                 PASS THE CORRECT PARMS TO SUBSEQUENT PROGRAMS *
     H*                                                               *
     H* 11/16/99  C:09   PROJECT: WC878                               *
     H*                  Only access entity/location detail if the    *
     H*                  system control flag (SWCPF/WCFL14) = "Y".    *
     H*                                                               *
     H* 08/17/99 C:61    PROJECT WC577                                *
     H*                  Don't call WW Owner/Officer (W1057R) if      *
     H*                  type = L - Location.                         *
     H*                                                               *
     H* 04/30/99  C:09   PROJECT: CM027                               *
     H*                  MIDTERM ENTITY/LOCATION ENDORSEMENT REWRITE. *
     H*                                                               *
     H* 02/23/99  C:90   SUPPORT: WA048/16623                         *
     H*                  CHANGED EDITS TO REQUIRE EITHER THE MAILING  *
     H*                  OR PHYSICAL ADDRESS TO BE ENTERED.           *
     H*                                                               *
     H* 02/05/99  C:87   PROJECT: WC878                               *
     H*                  Add an entry to screen 2 under "Type of      *
     H*                  Business".  This field is rqrd if client is  *
     H*                  reporting IAIABC/POC (WCFL14='Y'), else it   *
     H*                  will not show on the screen.  Provide F10=   *
     H*                  Search capability on this field.             *
     H*                                                               *
     H* 12/23/98  C:09   PROJECT: SUPPORT WA095 15681                 *
     H*                  NEED TO BE SURE WE ARE USING THE FUND FOR THE*
     H*                  RECORD BEING PROCESSING (NOT UPFND) WHEN     *
     H*                  RETRIEVING THE CURRENT AGENT.                *
     H*                                                               *
     H* 12/16/98  C:35   PROJECT: SUPPORT WA091 13352                 *
     H*                  GET GROUP AUXILIARY INFORMATION BEFORE       *
     H*                  CALLING SFTDLT SUBROUTINE.                   *
     H*                                                               *
     H* 11/23/98  C:35   PROJECT: SUPPORT WA090 15125                 *
     H*                  INITIALIZE POLICY EFFECTIVE DATE IN THE      *
     H*                  ENDORSEMENT RECORDS FROM THE EMPLOYER DETAIL *
     H*                  FILE.                                        *
     H*                                                               *
     H* 10/14/98  C:35   PROJECT: SUPPORT WA091 13352                 *
     H*                  INITIALIZE CARRIER NUMBER IN THE ENDORSEMENT *
     H*                  RECORDS FROM THE VALUE IN FUND AUXILIARY.    *
     H*                                                               *
     H*  09/22/98 C:33 PROJECT: WC577                                 *
     H*               -EFF DT >= INCETPTION DATE                      *
     H*               -DEFAULT EFF DT TO 0 IN ADD MODE                *
     H*               -DEFAULT NCCI FLAG TO 'Y' IN ADD MODE           *
     H*                                                               *
     H*  09/01/98 C:33 SEQ#: DR095/12876                              *
     H*                CHECK Q1PRMS PRIOR TO PROCESSING @@QTE.        *
     H*                                                               *
     H* 08/10/98  C:09  PROJECT: SUPP   WA033  SEQ# 12684             *
     H*                 IF DELETING ENT/LOC THRU MIDTERM ENDORSEMENTS *
     H*                 USE THE ENDORSEMENT EFFECTIVE DATE AS DEFAULT *
     H*                                                               *
     H*  08/07/98 C:78 SEQ#: DR095/12876                              *
     H*                ADD PARAMETER ##QTE WHEN CALLING W1057R.       *
     H*                MOVE VALUE 'Q' OR *BLANKS TO QCQTE FOR QTE MODE*
     H*                                                               *
     H* 04/10/98  C:33  PROJECT: WC577                                *
     H*                 RECOMPILED DUE TO CHANGE IN SCREEN.           *
     H*                                                               *
     H*  04/03/98 C:78 PROJECT WC825                                  *
     H*                ALLOW ACCESS TO DARTS. F14 ADDED.              *
     H*                                                               *
     H*  03/10/98 C:85 PROJECT WC577                                  *
     H*                VERSION 3 STANDARDS, AND CHANGED SEARCH FOR    *
     H*                TYPE OF BUSINESS (SCTPOB).                     *
     H*                                                               *
     H*  03/05/98  C:57  CM027                                        *
     H*                  FORCE OWNER/OFFICER MAINT IN ADD MODE>       *
     H*                                                               *
     H*  02/12/98  C:33  WC577                                        *
     H*                  DELETE RELATED FILES.                        *
     H*                                                               *
     H*  01/30/98  C:57  CM027                                        *
     H*            TRACK MID TERM CHANGES WHEN CALLED FROM MID TERM   *
     H*            ENDORSEMENT PROCESSING.                            *
     H*                                                               *
     H*  01/21/98  C:33  WC577                                        *
     H*                  CHANGE INDICATORS SO THAT THE COPY BOOK WILL *
     H*                  NOT MANIPULATE THEM.                         *
     H*                                                               *
     H*  12/31/97  C:79  WC801                                        *
     H*            DISPLAY 3 FLEXIBLE CONTACT HEADINGS ON SCR02.      *
     H*            HEADINGS ARE PULLED FROM WMEXP.  ADD THREE NEW     *
     H*            CONTACT FIELDS TO WMALP.                           *
     H*                                                               *
     H*  12/09/97  C:80  WC778                                        *
     H*            CHANGED ERROR CHECKING FOR SCR01 TO REQUIRE ENTRY  *
     H*            OF FEIN IF SO DESIGNATED IN ADMIN/DARTS.           *
     H*
     H*  12/08/97  C:57  CM027                                        *
     H*            CHG VALIDATION OF SCTPOB TO PROCESS AGAINST        *
     H*            GENERIC TABLE ENTRIES 'BUSI'.                      *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  @@MODE = 'A' IF ADD MODE                                     *
     H*  @@MODE = 'C' IF CHANGE MODE                                  *
     H*  @@MODE = 'D' IF INQUIRY MODE                                 *
     H*  @@MODE = 'E' IF DELETE MODE                                  *
     H*                                                               *
     H*  64   USED TO DISTINGUISH BETWEEN THE ADDITION OF ENTITY VS.  *
     H*       THE ADDITION OF A LOCATION.                             *
     H*  45   CONTROLS THE DISPLAY OF "DELETION DATE" & "REASON"      *
     H*  76   USED IN CODE TO CHECK FOR MESSAGE OVERRIDE              *
     H*  77   USED IN CODE TO CHECK FOR MESSAGE OVERRIDE              *
     H*                                                               *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H*  40 - ADD MODE                                                *
     H* N40 - UPDATE MODE                                             *
     H*  65 - SECURITY LEVEL VALID                                    *
     H* N65 - SECURITY LEVEL INVALID                                  *
     H*  90 - GENERAL ERROR INDICATOR                                 *
     H*  91 - GENERAL POSITION CURSOR                                 *
     H*  93 - SCREEN WAS UPDATE WHILE USER WAS IN IT                  *
     H*                                                               *
     H*  WDW01:                                                       *
     H*  ERROR:                                                       *
     H*  21 - (WCR0950) VALIDATE ENTITY/LOCATION TYPE FLAG            *
     H*                                                               *
     H*  SCR01:                                                       *
     H*  ERROR:                                                       *
     H*  20 - (WCR0500) Deletion date invalid                         *
     H*  21 - (WCR0503) Deletion reason not entered                   *
     H*  22 - (WCR0500) Effective date invalid                        *
     H*  23 - (WCR0503) Entity/Location name not entered              *
     H*  24 - (WCR0503) Mailing city not entered                      *
     H*  25 - (WCR0504) Mailing state invalid                         *
     H*  26 - (WCR0502) Mailing zip <= zero                           *
     H*  27 - (WCR0503) Physical city not entered                     *
     H*  28 - (WCR0504) Physical state not valid                      *
     H*  29 - (WCR0502) Physical zip <= zero                          *
     H*  30 - (WCR0501) NCCI flag not 'Y' or 'N'                      *
     H*  31 - (WCR1226) Deletion date less than effective date        *
     H*  32 - (WCR0503) ID # not entered                              *
     H*  33 - (WCR1225) Record already exists                         *
     H*  35 - (WCR0513) County code invalid                           *
     H*  36 - (WCR0690) County code inactive                          *
     H*  50 - Federal Id required                                     *
     H*  51 - (WC30649) Federal Id required                           *
     H*  76 - (WCR1061) Deletion date invalid, claims exist           *
     H*  POSITION CURSOR:                                             *
     H*  81 - (SCCNTY) County code                                    *
     H*  82 - (SCAREA) Area code                                      *
     H*                                                               *
     H*  SCR02:                                                       *
     H*  ERROR:                                                       *
     H*  34 - (WCR0588) Type of buisness not '1', '2', '3' or '4'     *
     H*  POSITION CURSOR:                                             *
     H*  82 - (SCTPOB) Type of business                               *
     H*                                                               *
     H*  SCR03:                                                       *
     H*  ERROR:                                                       *
     H*  POSITION CURSOR:                                             *
     H*****************************************************************
     FW0076F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWMALP     UF A E             DISK
     F*** BEG ADD ***** 03/05/98 ***********************************************
     F                                     INFDS(DBFDSB)
     F*** END ADD ***** 03/05/98 ***********************************************
     F                                     RENAME(PAL01:DBRCDF)
     FWMALL     IF   E           K DISK
     F                                     INFDS(DBFDSA)
     FWMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PEM02)
     FWMEML     IF   E           K DISK
     F*
     F**** BEGIN ADD *** 12/09/97 *************************************
     FWMEXL     IF   E           K DISK
     F**** END   ADD *** 12/09/97 *************************************
     F*
     FWMFNL     IF   E           K DISK
     FWMCMLC    IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PCS01)
     F                                     IGNORE(PBC01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PPC01)
     F                                     IGNORE(PSY01)
     FWTCNP     IF   E             DISK
     F                                     RENAME(PCN01:PCN02)
     F*** BEG CHG *** 04/27/00 **********************************
     F**STABL***IF**E********** K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END CHG *** 04/27/00 **********************************
     FSSYLF     IF   E           K DISK
     F*** BEG ADD ***** 12/08/97 ***********************************************
     FWTGTL     IF   E           K DISK
     F*** END ADD ***** 12/08/97 ***********************************************
     F*** BEG ADD ***** 01/30/98 ***********************************************
     FWTMYP     IF   E             DISK
     FWMDXL     IF   E           K DISK
     FWMENL3    IF A E           K DISK
     F*** END ADD ***** 01/30/98 ***********************************************
     F***  BEGIN ADD  ***  02/12/98  *********************************
     FWMOOL     UF   E           K DISK
     F***  END   ADD  ***  02/12/98  *********************************
      **** BEGIN ADD **** 10/14/98 *********************************************
     FWDF2L     IF   E           K DISK
      **** END   ADD **** 10/14/98 *********************************************
      **** BEGIN ADD **** 11/23/98 *********************************************
     FWDELL     IF   E           K DISK
      **** END   ADD **** 11/23/98 *********************************************
      *
     F** BEG ADD *** 05/06/03 *****************************************
      * Zip Codes by County (CO#/STATE/ZIPCODE)
     FWMCZL     IF   E           K DISK
      *
      * Statistical Reporting/Rating file
     FWDRRL1    IF   E           K DISK
      /COPY SCOPYMT01D
     D** END ADD *** 05/06/03 *****************************************
      *
     D*
     D***  BEGIN ADD  ***  09/22/98  *********************************
     D***  END   ADD  ***  09/22/98  *********************************
     D** BEG ADD *** 12/31/97 *****************************************
     D XXHD            S             19    DIM(3) CTDATA PERRCD(1)              CONTACT HDG
     D DOTS            C                   CONST('. . . . . . . . . .')
     D** END ADD *** 12/31/97 *****************************************
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDSA          DS
     D  DBRRNA               397    400B 0
     D*** BEG ADD ***** 03/05/98 ***********************************************
     D DBFDSB          DS
     D  DBRRNB               397    400B 0
     D*** END ADD ***** 03/05/98 ***********************************************
     D***  BEGIN ADD  ***  09/22/98  *********************************
     D                 DS
     D  EF                     1     35  0
     D                                     DIM(5)                               EMPLOYER EFF DTS
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0
     D***  END   ADD  ***  09/22/98  *********************************
      **** BEGIN ADD ***** 04/03/98 **********************************
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D  QCQTE                 40     40
      **** END   ADD ***** 04/03/98 **********************************
     D*** BEGIN ADD ***  04/13/09  ************************************
     D                 DS
     D  NAME                   1    100
     D  NAME1                  1     40
     D  NAME2                 41    100
     D                 DS
     D  SNAME                  1    100
     D  SNAM1                  1     50
     D  SNAM2                 51    100
     D*
     D                 DS
     D  NAMEF                  1    100
     D  NAME1F                 1     40
     D  NAME2F                41    100
     D                 DS
     D  SNAMEF                 1    100
     D  SNAM1F                 1     50
     D  SNAM2F                51    100
     D*** END   ADD ***  04/13/09  ************************************
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*
     D OPTNM           C                   CONST('Entity/Location Info-
     D                                     rmation')
     D***  Begin Add  ***  09/09/09  **************************************
     D CGCNTY          C                   CONST('MESSAGE: Please note-
     D                                      the defaulted -
     D                                     change to the county-
     D                                      code.')
     D***  End   Add  ***  09/09/09  **************************************
     D***  Begin Add  ***  07/06/11  **************************************
     D XXSEMH          C                   CONST('State risk ID #')
     D***  End   Add  ***  07/06/11  **************************************
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(2)
     D DLTRSN          C                   CONST('Opt 4 was used on th-
     D                                     is record.')
     D XXDPGM          C                   CONST('XXXXXXXXXX')
     D*** BEG ADD ***** 01/30/98 ***********************************************
     D AGTAPI          C                   CONST('WA002R')
     D*** END ADD ***** 01/30/98 ***********************************************
     D*
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
     D** BEG ADD *** 12/31/97 *****************************************
     D*
     D                 DS
     D  HDG                    1     57
     D                                     DIM(3)                               CONTACT HDG
     D  SCHDG1                 1     19
     D  SCHDG2                20     38
     D  SCHDG3                39     57
     D*
     D                 DS
     D  CNT                    1     45
     D                                     DIM(3)                               CONTACT NAME
     D  EXCNT1                 1     15
     D  EXCNT2                16     30
     D  EXCNT3                31     45
     D** END ADD *** 12/31/97 *****************************************
     D*** BEG ADD ***** 01/30/98 ***********************************************
     D                 DS                  INZ
     D  SCMZIP                 1      9  0
     D  SCMZP1                 1      5  0
     D  SCMZP2                 6      9  0
     D                 DS                  INZ
     D  SCPZIP                 1      9  0
     D  SCPZP1                 1      5  0
     D  SCPZP2                 6      9  0
     D                 DS
     D  XXZIP                  1      9
     D  XXZIP5                 1      5
     D  XXZIP4                 6      9
     D                 DS
     D  XXFEIN                 1      9
     D  XXFED2                 1      2
     D  XXFED7                 3      9
     D*
     D SVLDA           DS          1024
     D  LDEND#                 1      5  0
     D  LDENDA                 1      5
     D  LDMYRR                 6     14  0
     D  LDEEDT                15     22  0
     D  LDFYR                 23     25  0
     D  LDPYRL                26     26
      *** BEGIN ADD *** 08/10/98 ***********************************************
     D  LDPBCG                27     27
     D  LDOWCG                28     28
      *** END ADD ***** 08/10/98 ***********************************************

     I*** END ADD ***** 01/30/98 ***********************************************

     C*
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0
      ****** BEGIN ADD ***** 08/07/98 *********************************
     C                   PARM                    @@QTE             1
      ****** END   ADD ***** 08/07/98 *********************************
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##LVL             3 0
     C*** BEG ADD ***** 01/30/98 ***********************************************
     C*
     C*  Agent/Producer retrieve program parameter list
     C*
     C     AGTPL         PLIST
     C                   PARM                    ##CO#
     C*** BEGIN CHG *** 12/23/98 ***********************************************
     C*********************PARM           ##FND
     C                   PARM      SCFND         ##FND
     C*** END ADD ***** 12/23/98 ***********************************************
     C                   PARM      SCEMP#        ##EMP#            9 0
     C                   PARM      SCDIV         ##DIV             5 0
     C                   PARM      LDFYR         ##FYR             3 0
     C                   PARM                    ##AGT#            5 0
     C                   PARM                    ##ANAM           60
     C                   PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
     C*** END ADD ***** 01/30/98 ***********************************************
     C***  BEGIN ADD  ***  04/30/99  **********************************
     C     PL0075        PLIST
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C***  BEGIN CHG  ***  11/18/99  *********************************
     C**********           PARM @@MODE    ##MODE  1
     C                   PARM                    ##CALL            1
     C***  END   CHG  ***  11/18/99  *********************************
     C                   PARM      DBRRNB        ##RRN             9 0
     C***  END   ADD  ***  04/30/99  **********************************
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  KEY LIST FOR THE GROUP MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      **** BEGIN ADD **** 10/14/98 *********************************************
      *
      * KEY LIST FOR THE GROUP AUXILIARY FILE
      *
     C     F2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      **** END   ADD **** 10/14/98 *********************************************
     C*
     C*  KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
      **** BEGIN ADD **** 11/23/98 *********************************************
      *
      * KEY LIST FOR THE EMPLOYER DETAIL FILE
      *
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
      **** END   ADD **** 11/23/98 *********************************************
     C*
     C*  KEY LIST FOR THE ENTITY/LOCATION MASTER FILE
     C*
     C     ALKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXELID
     C*
     C     ALKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  KEY LIST FOR THE OWNER/OFFICER FILE
     C*
     C     OOKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXDLDT
     C*
     C     CMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  TABLE FILE KEY
     C*
     C     TBKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXKEY
     C*
     C*  SYSTEM TABLE FILE
     C*
     C     STKY          KLIST
     C                   KFLD                    XXKEY
     C*
     C*  SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*** BEG ADD ***** 12/08/97 ***********************************************
     C*
     C*  KEY LIST FOR THE GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XTBID1
     C                   KFLD                    XTBID2
     C*** END ADD ***** 12/08/97 ***********************************************
     C*
     C*** BEGIN ADD *** 03/10/98 **************************************
     C*
     C     GTKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XZTBID
     C                   KFLD                    XXKEY
     C*
     C*** END   ADD *** 03/10/98 **************************************
     C*
     C**** BEGIN ADD *** 12/09/97 *************************************
     C*  KEY LIST FOR ADMIN/DARTS FILE
     C*
     C     EXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C**** END   ADD *** 12/09/97 *************************************
     C*** BEG ADD ***** 01/30/98 ***********************************************
     C*
     C*  ENDORS/DARTS CONTROL DEFINITION
     C*
     C     DXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C*
     C*  ENDORSEMENT REQUEST WORKFILE
     C*
     C     ENKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    LDFYR
     C                   KFLD                    Q1JOBN
     C                   KFLD                    LDEND#
     C*** END ADD ***** 01/30/98 ***********************************************
     C***  BEGIN ADD  ***  02/12/98  *********************************
     C*
     C*  DELETION OF ENTITY/LOCATION RELATED FILES
     C*
     C     DLTKY         KLIST
     C                   KFLD                    ALCO#
     C                   KFLD                    ALFND
     C                   KFLD                    ALEMP#
     C                   KFLD                    ALDIV
     C                   KFLD                    ALELID
     C***  END   ADD  ***  02/12/98  *********************************
     C*
     *** BEG ADD *** 05/06/03 *****************************************
      *
      * Zip Codes by County File
      *
     C     CZKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##CZST            2
     C                   KFLD                    ##ZIP5            5 0
      *
     C     RRKY          KLIST
     C                   KFLD                    ##CO#
     *** BEG ADD ***** 07/16/03 ***************************************
     C******************** KFLD           ##FND
      ***** BEG CHG ******* 09/30/03 **********************************
     C*********            KFLD           XXFND
     C                   KFLD                    SCFND
      ***** END CHG ******* 09/30/03 **********************************
     *** END ADD ***** 07/16/03 ***************************************
     C                   KFLD                    ##RRST            2
     *** END ADD *** 05/06/03 *****************************************
      *
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    ALCO#         XXCO#
     C     *LIKE         DEFINE    ALFND         XXFND
     C     *LIKE         DEFINE    ALEMP#        XXEMP#
     C     *LIKE         DEFINE    ALDIV         XXDIV
     C     *LIKE         DEFINE    ALELTP        XXELTP
     C     *LIKE         DEFINE    ALELID        XXELID
     C     *LIKE         DEFINE    ALDT          XXDLDT
     C     *LIKE         DEFINE    ALDT          XXEFDT
     C     *LIKE         DEFINE    SCELID        SVELID
     C     *LIKE         DEFINE    SCEFDT        XSDATE
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    STKEY         XXKEY
     C*** BEG ADD ***** 12/08/97 ***********************************************
     C     *LIKE         DEFINE    GTTBID        XTBID1
     C     *LIKE         DEFINE    GTKEY         XTBID2
     C*** END ADD ***** 12/08/97 ***********************************************
     C*** BEG ADD ***** 01/30/98 ***********************************************
     C     *LIKE         DEFINE    ALFEIN        SVFEIN
     C     *LIKE         DEFINE    ALNAME        SVEMNM
     C     *LIKE         DEFINE    ALFDNM        SVFDNM
     C     *LIKE         DEFINE    ALMAD1        SVMAD1
     C     *LIKE         DEFINE    ALMAD2        SVMAD2
     C     *LIKE         DEFINE    ALMCTY        SVMCTY
     C     *LIKE         DEFINE    ALMST         SVMST
     C     *LIKE         DEFINE    ALMZIP        SVMZIP
     C     *LIKE         DEFINE    ALPAD1        SVPAD1
     C     *LIKE         DEFINE    ALPAD2        SVPAD2
     C     *LIKE         DEFINE    ALPCTY        SVPCTY
     C     *LIKE         DEFINE    ALPST         SVPST
     C     *LIKE         DEFINE    ALPZIP        SVPZIP
     C     *LIKE         DEFINE    ALCNTY        SVCNTY
     C     *LIKE         DEFINE    SCCNTD        SVCNTD
     C     *LIKE         DEFINE    ALCNT1        SVCNT1
     C     *LIKE         DEFINE    ALCNT2        SVCNT2
     C     *LIKE         DEFINE    ALCNT3        SVCNT3
     C*** END ADD ***** 01/30/98 ***********************************************
     C*** BEGIN ADD *** 03/10/98 **************************************
     C     *LIKE         DEFINE    GTTBID        XZTBID
     C*** END   ADD *** 03/10/98 **************************************
      **** BEGIN ADD ***** 04/03/98 **********************************
     C     *LIKE         DEFINE    Q1PGM         XXPGM
     C**** END   ADD ***** 04/03/98 **********************************
     C***  BEGIN ADD  ***  09/01/98  *********************************
     C     *LIKE         DEFINE    @@QTE         XXQTE
     C***  END   ADD  ***  09/01/98  *********************************
     C***  BEGIN ADD  ***  09/22/98  *********************************
     C     *LIKE         DEFINE    EMEFD1        SVEFDT
     C     *LIKE         DEFINE    ALEFDT        X7EFDT
     C***  END   ADD  ***  09/22/98  *********************************
     C*
     C/COPY SCOPYMT01C
     C*** BEGIN DEL *** 04/30/99 ***********************************************
     C*
     C*  **** SUBROUTINE NO LONGER BEING USED ****
     C*** BEG ADD ***** 01/30/98 ***********************************************
     C*===============================================================
     C*  CKCHGS - SUBROUTINE TO CREATE MID-TERM ENDORSEMENT RECORDS
     C*===============================================================
     C     CKCHGS        BEGSR
     C*
     C*** BEGIN DEL *** 08/10/98 ***********************************************
     C************NAMVAR   DEFN *LDA      SVLDA
     C************LOCK     IN   *NAMVAR
     C************         OUT  *NAMVAR
     C*** END DEL ***** 08/10/98 ***********************************************
     C*** END DEL ***** 04/30/99 ***********************************************
     C*
     C     LDENDA        IFNE      *ZEROS
     C     LDENDA        ANDNE     *BLANKS
     C*
     C     ALFEIN        IFNE      SVFEIN
     C     ALNAME        ORNE      SVEMNM
     C     ALFDNM        ORNE      SVFDNM
     C     ALMAD1        ORNE      SVMAD1
     C     ALMAD2        ORNE      SVMAD2
     C     ALMCTY        ORNE      SVMCTY
     C     ALMST         ORNE      SVMST
     C     ALMZIP        ORNE      SVMZIP
     C     ALPAD1        ORNE      SVPAD1
     C     ALPAD2        ORNE      SVPAD2
     C     ALPCTY        ORNE      SVPCTY
     C     ALPST         ORNE      SVPST
     C     ALPZIP        ORNE      SVPZIP
     C     ALCNTY        ORNE      SVCNTY
     C     ALCNT1        ORNE      SVCNT1
     C     ALCNT2        ORNE      SVCNT2
     C     ALCNT3        ORNE      SVCNT3
     C*
     C*  GET ENDORS/DARTS INFORMATION
     C*
     C     DXKY          CHAIN     PDX01                              60
     C*
     C     @@RRN2        CHAIN     WMEMP                              60
     C*
     C                   CALL      AGTAPI        AGTPL
     C*
     C*  SETUP FOR NEXT REQUEST NUMBER
     C*
     C     ENKY          SETGT     PEN01
     C     ENKY          READPE    PEN01                                  60
     C   60              Z-ADD     *ZERO         ENREQ#
     C                   Z-ADD     *ZERO         ENSEQ#
     C*
     C                   MOVE      EMCO#         ENCO#
     C                   MOVE      EMFND         ENFND
     C                   MOVE      EMEMP#        ENEMP#
     C                   MOVE      EMDIV         ENDIV
     C                   MOVE      LDFYR         ENFYR
     C                   MOVE      Q1JOBN        ENJOB
     C                   MOVE      LDEND#        ENEND#
     C*
     C                   MOVE      LDEEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENEEDT
     C*
     C                   MOVEL     EMNAME        ENNAM1
     C                   MOVEL     EMFDNM        ENFDN1
     C*** BEGIN ADD ***  03/07/05  ************************************
     C*
     C                   MOVEL     EMNAME        ENENM1
     C                   MOVEL(P)  EMNAM2        XXNM10           10
     C                   MOVE      XXNM10        ENENM1
     C                   MOVE      EMNAM2        ENENM2
     C*
     C                   MOVEL     EMFDNM        ENEFN1
     C                   MOVEL(P)  EMFDN2        XXNM10           10
     C                   MOVE      XXNM10        ENEFN1
     C                   MOVE      EMFDN2        ENEFN2
     C*
     C*** END ADD *****  03/07/05  ************************************
     C                   MOVEL     EMCONT        ENCON1
     C                   MOVEL     EMATTN        ENATT1
     C                   MOVEL     EMCNT3        ENCN31
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
     C*
     C                   MOVEL     SCELTP        ENELTP
     C                   MOVE      SCELID        ENELID
     C*
     C*  LOAD BEFORE FIELDS
     C*
     C                   MOVE      SVFEIN        ENOFI1
     C                   MOVEL     SVEMNM        ENOEN1
     C                   MOVEL     SVFDNM        ENOFN1
     C                   MOVEL     SVMAD1        ENOM11
     C                   MOVEL     SVMAD2        ENOM21
     C                   MOVEL     SVMCTY        ENOMC1
     C                   MOVEL     SVMST         ENOMS1
     C                   MOVE      SVMZIP        ENOMZ1
     C                   MOVEL     SVPAD1        ENOP11
     C                   MOVEL     SVPAD2        ENOP21
     C                   MOVEL     SVPCTY        ENOPC1
     C                   MOVEL     SVPST         ENOPS1
     C                   MOVEL     SVPZIP        ENOPZ1
     C                   MOVE      SVCNTY        ENOCN1
     C                   MOVEL     SVCNT1        ENOC11
     C                   MOVEL     SVCNT2        ENOC21
     C                   MOVEL     SVCNT3        ENOC31
     C*
     C*  LOAD AFTER FIELDS
     C*
     C                   MOVE      ALFEIN        ENOFI2
     C                   MOVEL     ALNAME        ENOEN2
     C                   MOVEL     ALFDNM        ENOFN2
     C                   MOVEL     ALMAD1        ENOM12
     C                   MOVEL     ALMAD2        ENOM22
     C                   MOVEL     ALMCTY        ENOMC2
     C                   MOVEL     ALMST         ENOMS2
     C                   MOVE      ALMZIP        ENOMZ2
     C                   MOVEL     ALPAD1        ENOP12
     C                   MOVEL     ALPAD2        ENOP22
     C                   MOVEL     ALPCTY        ENOPC2
     C                   MOVEL     ALPST         ENOPS2
     C                   MOVEL     ALPZIP        ENOPZ2
     C                   MOVE      ALCNTY        ENOCN2
     C                   MOVEL     ALCNT1        ENOC12
     C                   MOVEL     ALCNT2        ENOC22
     C                   MOVEL     ALCNT3        ENOC32
     C*
     C                   MOVEL     FNNAME        ENFNNM
     C                   MOVE      ##AGT#        ENAGT#
     C                   MOVEL     ##ANAM        ENAGNM
     C                   MOVEL     Q1USER        ENRUSR
     C*
     C     LDMYRR        CHAIN     PMY01                              60
     C                   MOVEL     MYDSC1        ENDSC1
     C                   MOVEL     MYDSC2        ENDSC2
     C                   MOVEL     MYDSC3        ENDSC3
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENDT
     C*
     C                   MOVEL     DSTIME        ENTIME
     C                   MOVEL     Q1USER        ENUSER
     C                   MOVEL     Q1PGM         ENPGM
     C                   MOVE      'A'           ENSTAT
     C                   MOVEL     Q1USER        ENRUSR
      **** BEGIN ADD **** 10/14/98 *********************************************
     C                   MOVE      F2CAR#        ENCAR1
      **** END   ADD **** 10/14/98 *********************************************
      **** BEGIN ADD **** 11/23/98 *********************************************
     C     ELKY          CHAIN     PEL01                              60
      *
     C     *IN60         DOWEQ     *OFF
     C     ELPTDT        ANDLE     ENEEDT
      *
     C     ELKY          READE     PEL01                                  60
      *
     C                   ENDDO                                                  *IN60 DOWEQ *OFF
      *
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     ELPFDT        ENPFD1
     C                   ENDIF                                                  *IN60 IFEQ *OFF
      **** END   ADD **** 11/23/98 *********************************************
     C*
     C*  LOAD CHG TYPE FLAG, REMARK CODE, BEFORE AND AFTER LINES, WRITE RECS
     C*
     C                   SELECT
     C*
     C*  ADD OF ENT/LOC
S1   C     *IN40         WHENEQ    *ON                                          ADD
     C                   MOVE      'Y'           ENAELF
     C                   ADD       1             ENREQ#
     C                   Z-ADD     0             ENSEQ#
B2   C     SCELTP        IFEQ      'E'
     C                   MOVEL(P)  DXENAD        ENRMCD
     C                   ELSE
     C                   MOVEL(P)  DXLCAD        ENRMCD
E2   C                   ENDIF
     C*
     C*  LOAD UP BEFORE/AFTER FIELDS AND WRITE RECORD
     C*
     C                   MOVE      *BLANKS       ENBLIN
     C*
     C                   MOVEL(P)  ALNAME        ENALIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C*
     C                   MOVEL(P)  ALFDNM        ENALIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C*
     C     ALPAD1        IFNE      *BLANKS
     C                   MOVEL(P)  ALPAD1        ENALIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C*
     C     ALPAD2        IFNE      *BLANKS
     C                   MOVEL(P)  ALPAD2        ENALIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C                   ENDIF
     C*
     C     ALPCTY        IFNE      *BLANKS
     C                   MOVEL     ALPZIP        XXZIP
     C     ALPCTY        CAT(P)    ',':0         ENALIN
     C                   CAT       ALPST:1       ENALIN
     C                   CAT       XXZIP5:1      ENALIN
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         ENALIN
     C                   CAT       XXZIP4:0      ENALIN
     C                   ENDIF
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  ALMAD1        ENALIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C*
     C     ALMAD2        IFNE      *BLANKS
     C                   MOVEL(P)  ALMAD2        ENALIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C                   ENDIF
     C*
     C                   MOVEL     ALMZIP        XXZIP
     C     ALMCTY        CAT(P)    ',':0         ENALIN
     C                   CAT       ALMST:1       ENALIN
     C                   CAT       XXZIP5:1      ENALIN
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         ENALIN
     C                   CAT       XXZIP4:0      ENALIN
     C                   ENDIF
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C                   ENDIF
     C*
     C*  DELETE OF ENT/LOC
     C     @@MODE        WHENEQ    'E'                                          DELETE
     C                   ADD       1             ENREQ#
     C                   Z-ADD     *ZERO         ENSEQ#
     C                   MOVE      'Y'           ENDELF
     C                   MOVE      ALDLDT        ENELDD
     C                   MOVEL     ALDLRS        ENELDR
     C     SCELTP        IFEQ      'E'
     C                   MOVEL(P)  DXENDL        ENRMCD
     C                   ELSE
     C                   MOVEL(P)  DXLCDL        ENRMCD
     C                   ENDIF
     C*
     C*  LOAD UP BEFORE/AFTER FIELDS AND WRITE RECORD
     C*
     C                   MOVE      *BLANKS       ENALIN
     C*
     C                   MOVEL(P)  SVEMNM        ENBLIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C*
     C                   MOVEL(P)  SVFDNM        ENBLIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C*
     C     SVPAD1        IFNE      *BLANKS
     C                   MOVEL(P)  SVPAD1        ENBLIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C*
     C     SVPAD2        IFNE      *BLANKS
     C                   MOVEL(P)  SVPAD2        ENBLIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C                   ENDIF
     C*
     C                   MOVEL     SVPZIP        XXZIP
     C     SVPCTY        CAT(P)    ',':0         ENBLIN
     C                   CAT       SVPST:1       ENBLIN
     C                   CAT       XXZIP5:1      ENBLIN
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         ENBLIN
     C                   CAT       XXZIP4:0      ENBLIN
     C                   ENDIF
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  SVMAD1        ENBLIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C*
     C     SVMAD2        IFNE      *BLANKS
     C                   MOVEL(P)  SVMAD2        ENBLIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C                   ENDIF
     C*
     C                   MOVEL     SVMZIP        XXZIP
     C     SVMCTY        CAT(P)    ',':0         ENBLIN
     C                   CAT       SVMST:1       ENBLIN
     C                   CAT       XXZIP5:1      ENBLIN
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         ENBLIN
     C                   CAT       XXZIP4:0      ENBLIN
     C                   ENDIF
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C                   ENDIF
     C*
     C*  CHANGE OF ENT/LOC
     C                   OTHER
     C                   MOVE      'Y'           ENCELF                         CHANGE
     C*    NAME CHANGE
     C     ALFEIN        IFNE      SVFEIN
     C     ALNAME        ORNE      SVEMNM
     C     ALFDNM        ORNE      SVFDNM
     C     ALCNT1        ORNE      SVCNT1
     C     ALCNT2        ORNE      SVCNT2
     C     ALCNT3        ORNE      SVCNT3
     C*
     C                   ADD       1             ENREQ#
     C                   Z-ADD     *ZERO         ENSEQ#
     C*
     C     SCELTP        IFEQ      'E'
     C                   MOVEL(P)  DXENNC        ENRMCD
     C                   ELSE
     C                   MOVEL(P)  DXLCNC        ENRMCD
     C                   ENDIF
     C*
     C*  LOAD UP BEFORE/AFTER FIELDS AND WRITE RECORD
     C*
     C                   MOVEL(P)  SVEMNM        ENBLIN
     C                   MOVEL(P)  ALNAME        ENALIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C*
     C                   MOVEL(P)  SVFDNM        ENBLIN
     C                   MOVEL(P)  ALFDNM        ENALIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C*
     C     ALFEIN        IFNE      SVFEIN
     C*
     C     SVFEIN        IFEQ      *ZERO
     C                   MOVEL     *BLANKS       ENBLIN
     C                   ELSE
     C                   MOVE      SVFEIN        XXFEIN
     C     XXFED2        CAT(P)    '-':0         ENBLIN
     C                   CAT       XXFED7:0      ENBLIN
     C                   ENDIF
     C*
     C     ALFEIN        IFEQ      *ZERO
     C                   MOVEL     *BLANKS       ENALIN
     C                   ELSE
     C                   MOVE      ALFEIN        XXFEIN
     C     XXFED2        CAT(P)    '-':0         ENALIN
     C                   CAT       XXFED7:0      ENALIN
     C                   ENDIF
     C*
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C                   ENDIF
     C*
     C     ALCNT1        IFNE      SVCNT1
     C                   MOVEL(P)  SVCNT1        ENBLIN
     C                   MOVEL(P)  ALCNT1        ENALIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C                   ENDIF
     C*
     C     ALCNT2        IFNE      SVCNT2
     C                   MOVEL(P)  SVCNT2        ENBLIN
     C                   MOVEL(P)  ALCNT2        ENALIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C                   ENDIF
     C*
     C     ALCNT3        IFNE      SVCNT3
     C                   MOVEL(P)  SVCNT3        ENBLIN
     C                   MOVEL(P)  ALCNT3        ENALIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*    MAILING ADDRESS CHANGE
     C     ALMAD1        IFNE      SVMAD1
     C     ALMAD2        ORNE      SVMAD2
     C     ALMCTY        ORNE      SVMCTY
     C     ALMST         ORNE      SVMST
     C     ALMZIP        ORNE      SVMZIP
     C*
     C                   ADD       1             ENREQ#
     C                   Z-ADD     *ZERO         ENSEQ#
     C*
     C     SCELTP        IFEQ      'E'
     C                   MOVEL(P)  DXENMA        ENRMCD
     C                   ELSE
     C                   MOVEL(P)  DXLCMA        ENRMCD
     C                   ENDIF
     C*
     C*  LOAD UP BEFORE/AFTER FIELDS AND WRITE RECORD
     C*
     C                   MOVEL(P)  SVEMNM        ENBLIN
     C                   MOVEL(P)  ALNAME        ENALIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C*
     C                   MOVEL(P)  SVFDNM        ENBLIN
     C                   MOVEL(P)  ALFDNM        ENALIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C*
     C                   MOVEL(P)  SVMAD1        ENBLIN
     C                   MOVEL(P)  ALMAD1        ENALIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C*
     C     SVMAD2        IFNE      *BLANKS
     C     ALMAD2        ORNE      *BLANKS
     C                   MOVEL(P)  SVMAD2        ENBLIN
     C                   MOVEL(P)  ALMAD2        ENALIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C                   ENDIF
     C*
     C     SVMCTY        IFEQ      *BLANKS
     C                   MOVEL     *BLANKS       ENBLIN
     C                   ELSE
     C                   MOVEL     SVMZIP        XXZIP
     C     SVMCTY        CAT(P)    ',':0         ENBLIN
     C                   CAT       SVMST:1       ENBLIN
     C                   CAT       XXZIP5:1      ENBLIN
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         ENBLIN
     C                   CAT       XXZIP4:0      ENBLIN
     C                   ENDIF
     C                   ENDIF
     C*
     C     ALMCTY        IFEQ      *BLANKS
     C                   MOVEL     *BLANKS       ENALIN
     C                   ELSE
     C                   MOVEL     ALMZIP        XXZIP
     C     ALMCTY        CAT(P)    ',':0         ENALIN
     C                   CAT       ALMST:1       ENALIN
     C                   CAT       XXZIP5:1      ENALIN
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         ENALIN
     C                   CAT       XXZIP4:0      ENALIN
     C                   ENDIF
     C                   ENDIF
     C*
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C                   ENDIF
     C*
     C*    PHYSICAL ADDRESS CHANGE
     C     ALPAD1        IFNE      SVPAD1
     C     ALPAD2        ORNE      SVPAD2
     C     ALPCTY        ORNE      SVPCTY
     C     ALPST         ORNE      SVPST
     C     ALPZIP        ORNE      SVPZIP
     C     ALCNTY        ORNE      SVCNTY
     C*
     C                   ADD       1             ENREQ#
     C                   Z-ADD     *ZERO         ENSEQ#
     C*
     C     SCELTP        IFEQ      'E'
     C                   MOVEL(P)  DXENPA        ENRMCD
     C                   ELSE
     C                   MOVEL(P)  DXLCPA        ENRMCD
     C                   ENDIF
     C*
     C*  LOAD UP BEFORE/AFTER FIELDS AND WRITE RECORD
     C*
     C                   MOVEL(P)  SVEMNM        ENBLIN
     C                   MOVEL(P)  ALNAME        ENALIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C*
     C                   MOVEL(P)  SVFDNM        ENBLIN
     C                   MOVEL(P)  ALFDNM        ENALIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C*
     C     ALCNTY        IFNE      SVCNTY
     C     SVCNTY        IFEQ      *BLANKS
     C                   MOVEL     *BLANKS       ENBLIN
     C                   ELSE
     C                   MOVEL(P)  SVCNTY        ENBLIN
     C                   CAT       SVCNTD:2      ENBLIN
     C                   ENDIF
     C     ALCNTY        IFEQ      *BLANKS
     C                   MOVEL     *BLANKS       ENALIN
     C                   ELSE
     C                   MOVEL(P)  ALCNTY        ENALIN
     C                   CAT       SCCNTD:2      ENALIN
     C                   ENDIF
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C                   ENDIF
     C*
     C                   MOVEL(P)  SVPAD1        ENBLIN
     C                   MOVEL(P)  ALPAD1        ENALIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C*
     C     SVPAD2        IFNE      *BLANKS
     C     ALPAD2        ORNE      *BLANKS
     C                   MOVEL(P)  SVPAD2        ENBLIN
     C                   MOVEL(P)  ALPAD2        ENALIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C                   ENDIF
     C*
     C     SVPCTY        IFEQ      *BLANKS
     C                   MOVEL     *BLANKS       ENBLIN
     C                   ELSE
     C                   MOVEL     SVPZIP        XXZIP
     C     SVPCTY        CAT(P)    ',':0         ENBLIN
     C                   CAT       SVPST:1       ENBLIN
     C                   CAT       XXZIP5:1      ENBLIN
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         ENBLIN
     C                   CAT       XXZIP4:0      ENBLIN
     C                   ENDIF
     C                   ENDIF
     C*
     C     ALPCTY        IFEQ      *BLANKS
     C                   MOVEL     *BLANKS       ENBLIN
     C                   ELSE
     C                   MOVEL     ALPZIP        XXZIP
     C     ALPCTY        CAT(P)    ',':0         ENALIN
     C                   CAT       ALPST:1       ENALIN
     C                   CAT       XXZIP5:1      ENALIN
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         ENALIN
     C                   CAT       XXZIP4:0      ENALIN
     C                   ENDIF
     C                   ENDIF
     C*
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C*
     C                   ENDIF
     C                   ENDSL
     C*
     C                   ENDIF                                                  ALFEIN <> SVFEIN
     C*
     C                   ENDIF                                                  LDEND# <>0,' '
     C*
     C                   ENDSR
     C*** END ADD ***** 01/30/98 ***********************************************
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C*
     C     DLTPRE        BEGSR
     C***  BEGIN ADD  ***  02/12/98  *********************************
     C*
     C*  DELETE RECORDS FROM THE OWNER/OFFICER FILE
     C*
     C     *IN60         DOUEQ     *ON
     C     DLTKY         DELETE    POO01                              60
     C                   ENDDO                                                  *IN60=*ON
     C***  END   ADD  ***  02/12/98  *********************************
     C*
     C                   ENDSR                                                  DLTPRE
     C*
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C*
     C     DSPLYR        BEGSR
     C                   ENDSR                                                  DSPLYR
     C*
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C*** BEGIN ADD *** 04/30/99 ***********************************************
     C     *IN63         IFEQ      *OFF
     C*** END ADD ***** 04/30/99 ***********************************************
     C  N90*IN40         IFEQ      *ON
     C*
     C  N90SCELID        IFEQ      *ZEROS
     C     SCELID        OREQ      *BLANKS
     C                   SETON                                        32  90
     C                   ENDIF                                                  *ZEROS=SCELID=*BLANK
     C*
     C  N90SCELID        IFNE      SVELID
     C*
     C*  SINCE WE ALLOW USERS TO OVERRIDE THE SYSTEM-ASSIGNED ID#
     C*  ON AN ADD MODE, DON'T ALLOW AN ID# THAT ALREADY EXISTS.
     C*
     C                   Z-ADD     SCEMP#        XXEMP#
     C                   Z-ADD     SCDIV         XXDIV
     C                   MOVEL     SCELTP        XXELTP
     C                   MOVEL     SCELID        XXELID
     C*
     C     ALKY          CHAIN     PAL01                              60
     C*
     C  N90*IN60         IFEQ      *OFF
     C                   SETON                                        33  90
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C*  O.K. TO OVERRIDE, UPDATE ID# IN DUMMY RECORD
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   MOVEL     SVELID        XXELID
     C                   MOVEL     SCELID        SVELID                         SAVE NEW ID#
     C*
     C     ALKY          CHAIN     PAL01                              60
     C*
     C  N60ALPGM         IFEQ      XXDPGM
     C     DBRRNA        CHAIN     WMALP                              60
     C                   MOVEL     SCELID        ALELID
     C                   UPDATE    DBRCDF
     C                   ENDIF                                                  ALPGM=XXDPGM
     C*
     C                   ENDIF                                                  *IN90=*OFF
     C*
     C                   ENDIF                                                  SCELID<>SVELID
     C*
     C                   ENDIF                                                  *IN40=*ON
     C*** BEGIN ADD *** 04/30/99 ***********************************************
     C                   ENDIF                                                  *IN63=*OFF
     C*** END ADD ***** 04/30/99 ***********************************************
     C*
     C*  VALIDATE DELETION DATE/REASON
     C*
     C     SCDLDT        IFNE      *ZERO
     C*** BEGIN ADD *** 04/30/99 ***********************************************
     C     *IN63         OREQ      '1'
     C*** END ADD ***** 04/30/99 ***********************************************
     C*
     C*  DELETION DATE MUST BE VALID
     C*
     C                   Z-ADD     SCDLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDLDT
     C*
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        20  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C*  DELETION DATE CANNOT BE LESS THAN EFFECTIVE DATE.
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   Z-ADD     SCDLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDLDT
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C     XXDLDT        IFLT      XXEFDT
     C     XXDLDT        ANDNE     *ZERO
     C                   SETON                                        31  90
     C                   ENDIF                                                  XXDLDT<XXEFDT
     C*
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C*  IF A DELETE DATE WAS SPECIFIED AND THERE IS ANY CLAIM THAT
     C*  EXISTS WITH AN ACCIDENT DATE GREATER THAN OR EQUAL TO THE DELETE
     C*  DATE, FLAG AN ERROR MESSAGE BUT ALLOW USER TO OVERRIDE.
     C*
     C  N90OVRFLG        IFEQ      'N'
     C     SCDLDT        ANDNE     *ZERO
     C*
     C     CMKY          SETLL     PCM01
     C     CMKY1         READE     PCM01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C                   XFOOT     AR            XXPMTS           11 2
     C*
     C     CMSTAT        IFNE      'C'
     C     CMCTG1        ANDEQ     SCELID
     C     CMSTAT        OREQ      'C'
     C     XXPMTS        ANDNE     *ZEROS
     C     CMCTG1        ANDEQ     SCELID
     C*
     C*  ALLOW OVERRIDE ON ERROR MSG ONLY IF USER HAS PROPER AUTHORITY
     C*
     C                   MOVEL     'WC'          XXSYK
     C     SYKY          CHAIN     SWC01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     ##LVL         ANDGE     WCSL03
     C*
     C                   MOVE      'N'           SCEOVR
     C*
     C                   EXFMT     WDWMSG
     C*
     C     SCEOVR        IFEQ      'Y'
     C                   MOVE      'Y'           OVRFLG            1
     C                   MOVE      *ON           *IN77
     C                   ELSE
     C                   MOVE      'N'           OVRFLG
     C                   MOVE      *OFF          *IN77
     C                   SETON                                        766090
     C                   ENDIF                                                  SCEOVR=Y
     C*
     C                   ELSE
     C*
     C                   MOVE      'N'           OVRFLG
     C                   SETON                                        766090
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  CMSTAT<>C
     C*
     C  N90CMKY1         READE     PCM01                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  OVRTAG=N
     C*
     C*  DELETION REASON CANNOT BE BLANK
     C*
     C  N90SCDLRS        IFEQ      *BLANKS
     C                   SETON                                        21  90
     C                   ENDIF                                                  SCDLRS=*BLANKS
     C*
     C                   ENDIF                                                  SCDLDT <> 0
     C*
     C*** BEGIN ADD *** 04/30/99 ***********************************************
     C     *IN63         IFEQ      *OFF
     C*** END ADD ***** 04/30/99 ***********************************************
     C*
     C*  EFFECTIVE DATE MUST BE VALID
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        22  90
     C                   ENDIF                                                  SRERR=*ON
     C***  BEGIN ADD  ***  09/22/98  *********************************
     C*
     C*  EFFECTIVE DATE MUST BE GREATER THAN INCEPTION DATE
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7EFDT
     C*
     C  N90SVEFDT        IFGT      X7EFDT
     C                   SETON                                        71  90
     C                   ENDIF                                                  SVEFDT>X7EFDT
     C***  END   ADD  ***  09/22/98  *********************************
     C*
     C**** BEGIN ADD *** 12/09/97 *************************************
     C*
     C*  IF FEDERAL ID REQUIRED, FEIN MUST NOT BE BLANK
     C*
     C     EXKY          CHAIN     PEX01                              60
     C*
     C  N90
     CANN60EXFIFG        IFEQ      'Y'
     C     SCELTP        ANDEQ     'E'
     C     SCFEIN        ANDEQ     *ZEROS
     C*
     C                   SETON                                        505190
     C*
     C                   ENDIF                                                  EXFIFG IFEQ 'Y'
     C**** END   ADD *** 12/09/97 *************************************
     C*
     C*  ENTITY/LOCATION NAME MUST BE ENTERED
      ***** BEG CHG ***** 05/25/07 **********************************
     C*
     C**N90***   *IN64     IFNE *ON
     C********   SCEMNM    ANDEQ*BLANKS
     C     SCEMNM        IFEQ      *BLANKS
     C                   SETON                                        23  90
     C                   ENDIF
      ***** END CHG ***** 05/25/07 **********************************
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
     C**** BEGIN ADD **** 02/23/99 ************************************
     C*
     C*  REQUIRE EITHER MAILING OR PHYSICAL ADDRESS TO BE ENTERED
     C*
     C  N90SCMAD1        IFEQ      *BLANK
     C     SCPAD1        ANDEQ     *BLANK
     C                   SETON                                        38  90
     C                   ENDIF                                                  SCMAD1 = *BLANK
     C**** END   ADD **** 02/23/99 ************************************
     C*
     C*  VALIDATE MAILING ADDRESS IF REQUIRED
     C*
     C  N90SCMAD1        IFNE      *BLANK
     C*
     C*  MAILING CITY CANNOT BE BLANK
     C*
     C  N90SCMCTY        IFEQ      *BLANK
     C                   SETON                                        24  90
     C                   ENDIF                                                  SCMCTY=*BLANDK
     C*
     C*  THE STATE MUST BE VALID
     C*
     C                   MOVEL(P)  SCMST         XXKEY
     C     STKY          CHAIN     PST01                              60
     C  N90*IN60         IFEQ      *ON
     C                   SETON                                        25  90
     C                   ENDIF                                                  *IN60=*ON
     C*
     C*  THE ZIP CODE CANNOT BE ZERO
     C*
     C  N90SCMZP1        IFLE      *ZERO
     C                   SETON                                        26  90
     C                   ENDIF                                                  SCMZP1<=*ZERO
     C*
     C                   ENDIF                                                  SCMAD1<>*BLANK
     C*
     C*  VALIDATE PHYSICAL ADDRESS IF REQUIRED
     C*
     C  N90SCPAD1        IFNE      *BLANK
     C*
     C*  PHYSICAL CITY CANNOT BE BLANK
     C*
     C  N90SCPCTY        IFEQ      *BLANK
     C                   SETON                                        27  90
     C                   ENDIF                                                  SCPCTY=*BLANK
     C*
     C*  THE STATE MUST BE VALID
     C*
     C                   MOVEL(P)  SCPST         XXKEY
     C     STKY          CHAIN     PST01                              60
     C  N90*IN60         IFEQ      *ON
     C                   SETON                                        28  90
     C                   ENDIF                                                  *IN60=*ON
     C*
     C*  THE ZIP CODE CANNOT BE ZERO
     C*
     C  N90SCPZP1        IFLE      *ZERO
     C                   SETON                                        29  90
     C                   ENDIF                                                  SCPZP1<=*ZERO
     C*
     C                   ENDIF                                                  SCPAD1<>*BLANK
     C*
     C***  Begin Add  ***  09/09/09  **************************************
     C*  Default county code
     C  N90SCPST         IFGT      *BLANKS
     C     SCPZP1        ANDGT     *ZEROS
     C                   MOVE      SCPST         ##CZST
     C                   MOVE      SCPZP1        ##ZIP5
     C                   ELSE
     C                   MOVE      SCMST         ##CZST
     C                   MOVE      SCMZP1        ##ZIP5
     C                   ENDIF
     C*
     C     CZKY          CHAIN     PCZ01                              60
     C     *IN60         IFEQ      *OFF
     C     CZSTAT        ANDEQ     'A'
     C     CZCNTY        IFNE      SCCNTY
     C                   MOVEL(P)  CGCNTY        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      78            ##LENG            3 0
     C                   MOVEL     ##DATA        SCMSG
     C                   SETON                                        808190
     C                   ENDIF
     C                   MOVE      CZCNTY        SCCNTY
     C                   ENDIF
     C***  End   Add  ***  09/09/09  **************************************
     C*
     C*  COUNTY CODE MUST BE VALID AND ACTIVE, IF ENTERED
     C*
     C     SCCNTY        IFNE      *BLANKS
     C*
     C***  Begin Chg  ***  09/09/09  **************************************
     C********** SCPST     IFEQ *BLANKS
     C  N90SCPST         IFEQ      *BLANKS
     C***  End   Chg  ***  09/09/09  **************************************
     C     SCMST         ANDEQ     *BLANKS
     C                   SETON                                        37  90
     C                   ENDIF                                                  SCPST=*BLANKS=SCMST
     C*
      ***** BEG ADD ***** 03/17/05 *********************************************
      * If add/chgge, Check screen County Code against one just retrieved
     C     @@MODE        IFEQ      'A'
     C     *IN90         ANDEQ     *OFF
     C     @@MODE        OREQ      'C'
     C     *IN90         ANDEQ     *OFF
     C     SCPST         IFGT      *BLANKS
     C     SCPZP1        ANDGT     *ZEROS
     C                   MOVE      SCPST         ##CZST
     C                   MOVE      SCPZP1        ##ZIP5
     C                   ELSE
     C                   MOVE      SCMST         ##CZST
     C                   MOVE      SCMZP1        ##ZIP5
     C                   ENDIF                                                  END SCPST=*
     C     CZKY          CHAIN     PCZ01                              60
     C                   SELECT
      ***** BEG DEL ***** 04/25/05 *********************************************
     C********** *IN60     WHEQ *ON
     C********** SCPZP1    ANDGT*ZEROS
     C**********           SETON                     42  90
     C********** *IN60     WHEQ *ON
     C********** SCMZP1    ANDGT*ZEROS
     C**********           SETON                     41  90
      ***** END DEL ***** 04/25/05 *********************************************
     C     *IN60         WHENEQ    *OFF
     C     CZSTAT        ANDNE     'A'
     C     SCPZP1        ANDGT     *ZEROS
      *** BEG CHG ***** 11/16/06 ***********************************************
     C*****************    SETON                     40  90
     C                   SETON                                        72  90
      *** END CHG ***** 11/16/06 ***********************************************
     C     *IN60         WHENEQ    *OFF
     C     CZSTAT        ANDNE     'A'
     C     SCMZP1        ANDGT     *ZEROS
     C                   SETON                                        39  90
     C                   OTHER
      ***** BEG DEL ***** 03/17/05 *********************************************
     C************         MOVE CZCNTY    SCCNTY
      ***** END DEL ***** 03/17/05 *********************************************
     C                   ENDSL                                                  CNCNTY<>SCCNTY
     C                   ENDIF                                                  @@MODE=A/*IN09=*ON
      ***** END ADD ***** 03/17/05 *********************************************
     C*
     C***  Begin Chg  ***  09/09/09  **************************************
     C**N90***** SCPST     IFNE *BLANKS
     C     SCPST         IFNE      *BLANKS
     C***  End   Chg  ***  09/09/09  **************************************
     C     SCPST         CAT       SCCNTY:0      ZZKEY             4
     C                   ELSE
     C     SCMST         CAT       SCCNTY:0      ZZKEY
     C                   ENDIF                                                  SCPST<>*BLANKS
     C*
     C                   MOVEL(P)  ZZKEY         XXKEY
     C*
     C***  Begin Chg  ***  09/09/09  **************************************
     C**N90***** TBKY      CHAINPCN01                60
     C                   CLEAR                   CNDESC
     C     TBKY          CHAIN     PCN01                              60
     C                   MOVEL(P)  CNDESC        SCCNTD
     C***  End   Chg  ***  09/09/09  **************************************
     C*
      ***** BEG ADD ***** 04/25/05 *********************************************
     C  N90*IN60         IFEQ      *OFF
     C                   MOVEL(P)  CNDESC        SCCNTD
     C                   ENDIF                                                  *IN60=*OFF
      ***** END ADD ***** 04/25/05 *********************************************
     C*
      ***** BEG DEL ***** 04/25/05 *********************************************
      **********
     C**N90***** *IN60     IFEQ *ON
     C**********
     C**********           SETON                     35  90
     C**********
     C**********           ELSE
     C**********
     C********** CNSTAT    IFEQ 'I'
     C**********           SETON                     36  90
     C**********           ENDIF                           CNSTAT=I
     C**********
      ******BEG*CHG ***** 03/17/05 *********************************************
     C**N90*************   MOVELCNDESC    SCCNTD
     C**N90*****           MOVELCNDESC    SCCNTD    P
      ******END*CHG ***** 03/17/05 *********************************************
     C**********
     C**********           ENDIF                           *IN60=*ON
      ***** END DEL ***** 04/25/05 *********************************************
     C*
     C                   ENDIF                                                  SCCNTY<>*BLANKS
     C*** BEGIN ADD *** 04/30/99 ***********************************************
     C                   ENDIF                                                  *IN63=*OFF
     C*** END ADD ***** 04/30/99 ***********************************************
     C*
     *** BEG ADD *** 05/06/03 *****************************************
      *  If in ADD/CHG MODE, preload the County Code in screen
     C                   MOVE      *BLANKS       ##CNTY            2
     C                   MOVE      *BLANKS       ##CZST
     C                   MOVE      *ZEROS        ##ZIP5
     C     @@MODE        IFEQ      'A'
     C     *IN90         ANDEQ     *OFF
     C     @@MODE        ORNE      'D'
     C     *IN90         ANDEQ     *OFF
      ***** BEG ADD ***** 03/17/05 *********************************************
     C     SCCNTY        IFEQ      *BLANKS
      ***** END ADD ***** 03/17/05 *********************************************
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
      ***** BEG ADD ***** 03/17/05 *********************************************
     C*
      ***** BEG DEL ***** 04/25/05 *********************************************
     C**********           SELEC
     C********** *IN60     WHEQ *ON
     C********** SCPZP1    IFGT *ZEROS
     C**********           SETON                     42  90
     C**********           ELSE
     C**********           SETON                     41  90
     C**********           ENDIF                           *IN60 = *OFF
     C********** CZSTAT    WHEQ 'I'
     C********** SCPZP1    IFGT *ZEROS
     C**********           SETON                     40  90
     C**********           ELSE
     C**********           SETON                     39  90
     C**********           ENDIF                           CZSTAT = 'I'
     C**********           ENDSL                           *IN60 = *ON
      ***** END ADD ***** 03/17/05 *********************************************
      ***** END DEL ***** 04/25/05 *********************************************
     C     *IN60         IFEQ      *OFF
     C     CZSTAT        ANDEQ     'A'
     C                   MOVE      CZCNTY        SCCNTY
      ***** BEG DEL ***** 03/17/05 *********************************************
     C****************     MOVE CZCNTD    SCCNTD
      ***** END DEL ***** 03/17/05 *********************************************
     C                   MOVE      CZCNTY        ##CNTY
     C                   ENDIF                                                  END-CZCNTY
      ***** BEG ADD ***** 03/17/05 *********************************************
     C  N90SCPST         IFNE      *BLANKS
     C     SCPST         CAT       SCCNTY:0      ZZKEY             4
     C                   ELSE
     C     SCMST         CAT       SCCNTY:0      ZZKEY
     C                   ENDIF                                                  SCPST<>*BLANKS
     C*
     C                   MOVEL(P)  ZZKEY         XXKEY
     C  N90TBKY          CHAIN     PCN01                              60
     C*
      ***** BEG ADD ***** 04/25/05 *********************************************
     C  N90*IN60         IFEQ      *OFF
     C                   MOVEL(P)  CNDESC        SCCNTD
     C                   ENDIF                                                  *IN60=*OFF
      ***** END ADD ***** 04/25/05 *********************************************
      *
      ***** BEG DEL ***** 04/25/05 *********************************************
     C**N90***** *IN60     IFEQ *ON
     C**********           SETON                     35  90
     C**********
     C**********           ELSE
     C**********
     C********** CNSTAT    IFEQ 'A'
     C**********           MOVELCNDESC    SCCNTD    P
     C**********           ELSE
     C**********           SETON                     36  90
     C**********           ENDIF                           CNSTAT=A
     C**********           ENDIF                           END *IN60 = *OFF
      ***** END DEL ***** 04/25/05 *********************************************
     C                   ENDIF                                                  SCCNTY = *BLANKS
      ***** END ADD ***** 03/17/05 *********************************************
     C                   ENDIF                                                  @@MODE=A/*IN09=*ON
     *** END ADD *** 05/06/03 *****************************************
      *
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90ALDT          IFGT      SAVEDT
     C     ALDT          OREQ      SAVEDT
     C     ALTIME        ANDGT     SAVETM
     C                   SETON                                        9093
     C                   ENDIF                                                  ALDT>SAVEDT
     C*
     C*** BEGIN ADD *** 04/30/99 ***********************************************
     C     *IN90         IFEQ      *OFF
     C     *IN63         ANDEQ     *ON
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      GETDTM
     C*
     C                   MOVEL     SCDLRS        ALDLRS
     C                   Z-ADD     SCDLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ALDLDT
     C*
     C                   MOVEL     Q1USER        ALUSER
     C                   MOVEL     Q1PGM         ALPGM
     C                   Z-ADD     SAVEDT        ALDT
     C                   Z-ADD     SAVETM        ALTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   MOVE      '1'           *INLR
     C*
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDIF                                                  *IN90/*IN63
     C*** END ADD ***** 04/30/99 ***********************************************
     C*
      ***  BEG ADD ******  01/04/10  *********************************
     C                   EXSR      FLXHDG
      ***  END ADD ******  01/04/10  *********************************
     C                   ENDSR                                                  ERRCK1
     C*
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C*
     C     ERRCK2        BEGSR
     C*
     C*** BEGIN ADD *** 04/30/99 ***********************************************
     C     *IN63         IFEQ      *OFF
     C*** END ADD ***** 04/30/99 ***********************************************
     C*
     C*  VALIDATE - REPORT TO NCCI FLAG
     C*
     C  N90SCNCCI        IFNE      'Y'
     C     SCNCCI        ANDNE     'N'
     C                   SETON                                        30  90
     C                   ENDIF                                                  SCNCCI<>Y/N
     C*
     C*  TYPE OF BUSINESS MUST BE VALID
     C*
     C*** BEG CHG ***** 12/08/97 ***********************************************
     C**N90 74   SCTPOB    IFLT '1'
     C***        SCTPOB    ORGT '4'
     C***                  SETON                     34  90
     C***                  ENDIF                           1>SCTPOB>4
     C     *IN90         IFEQ      *OFF
     C*
     C     *IN64         IFEQ      *OFF
     C     SCTPOB        ORNE      *BLANKS
     C*
     C                   MOVEL(P)  'BUSI'        XTBID1
     C                   MOVEL(P)  SCTPOB        XTBID2
     C*
     C     GTKY          CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   SETON                                        34  90
     C                   ELSE
     C                   MOVEL(P)  GTDESC        SCTPBD
     C                   ENDIF                                                  *IN60=*ON/GTSTAT<>A
     C*
     C                   ENDIF                                                  *IN64 = *OFF
     C*
     C                   ENDIF                                                  *IN90 = *OFF
     C*** END CHG ***** 12/08/97 ***********************************************
     C*
     C***  BEGIN ADD  ***  02/05/99  **********************************
     C*
     C*  SIC code must be valid, IF REQUIRED IN ADMIN/DARTS CONTROL
     C*
     C*
     C  N90*IN94         IFEQ      *ON
     C*
     C     SCSIC         IFEQ      *BLANK
      *** BEG ADD ***** 07/23/08 **************************************
     C***********          SETON                     87  90
     C     EXSICR        IFEQ      'Y'
     C                   SETON                                        87  90
     C                   ENDIF
      *** BEG ADD ***** 07/23/08 **************************************
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  'SIC'         XTBID1
     C                   MOVEL(P)  SCSIC         XTBID2
     C*
     C     GTKY          CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   SETON                                        89  90
     C                   ELSE
     C                   MOVEL(P)  GTDESC        SCSICD
     C                   ENDIF                                                  *IN60=*ON/GTSTAT<>A
     C*
     C                   ENDIF                                                  SCSIC=*BLANK
     C*
     C                   ENDIF                                                  *IN94 = *ON
     C***  END   ADD  ***  02/05/99  **********************************
      ***** BEG ADD ***** 05/12/06 ************************************
     C*
     C* The NAIC code Must be a Valid code
     C*
     C     *IN90         IFEQ      *OFF
     C     *IN09         ANDEQ     *OFF                                         NOT DISPLAY MODE
     C*
     C     SCPST         IFGT      *BLANKS
     C                   MOVE      SCPST         ##RRST
     C                   ELSE
     C                   MOVE      SCMST         ##RRST
     C                   ENDIF                                                  END SCPST=*
     C     RRKY          CHAIN     PRR01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     RRNAIC        ANDEQ     'Y'
     C*
     C     SCNAIC        IFEQ      *BLANKS
     C                   SETON                                        90  35
     C                   ELSE
     C                   MOVEL(P)  'NAIC'        XTBID1
     C                   MOVEL(P)  SCNAIC        XTBID2
     C     GTKY          CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   SETON                                        36  90
     C                   ELSE
     C                   MOVEL(P)  GTDESC        SCSICD
     C                   ENDIF                                                  *IN60=*ON/GTSTAT<>A
     C*
     C                   ENDIF                                                  SCNAIC=BLANKS
     C*
     C                   ENDIF                                                  END 60=OFF
     C                   ENDIF                                                  END 90=OFF
      ***** END ADD ***** 05/12/06 ************************************
     C*
     C*** BEGIN ADD *** 04/30/99 ***********************************************
     C*** BEG DEL ***** 06/12/06 ***********************************************
     C*****************    ENDIF                            *IN63
     C*** END DEL ***** 06/12/06 ***********************************************
     C*** END ADD ***** 04/30/99 ***********************************************
     C*
     *** BEG ADD *** 05/06/03 *****************************************
      * Check State Unemployment Flag for required field
     C     *IN90         IFEQ      *OFF
     C     *IN09         ANDEQ     *OFF                                         NOT DISPLAY MODE
     C     *IN41         ANDEQ     *OFF                                         NOT QUOTE MODE
     C     SCPST         IFGT      *BLANKS
     C                   MOVE      SCPST         ##RRST
     C                   ELSE
     C                   MOVE      SCMST         ##RRST
     C                   ENDIF                                                  END SCPST=*
     C     RRKY          CHAIN     PRR01                              60
     C     *IN60         IFEQ      *OFF
     C     RRURFL        ANDEQ     'Y'
     C     SCUE#         ANDEQ     *BLANKS
     C                   SETON                                        90  83
     C                   ENDIF                                                  END 60=OFF
     C                   ENDIF                                                  END 90=OFF
     *** END ADD *** 05/06/03 *****************************************
      *
     C*** BEG ADD ***** 06/12/06 ***********************************************
     C                   ENDIF                                                   *IN63
     C*** END ADD ***** 06/12/06 ***********************************************
      *
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90ALDT          IFGT      SAVEDT
     C     ALDT          OREQ      SAVEDT
     C     ALTIME        ANDGT     SAVETM
     C                   SETON                                        9093
     C                   ENDIF                                                  ALDT>SAVEDT
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C*
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*
     C*================================================================
     C* ERRCKW - Error checking for window 1 (WDW01)
     C*================================================================
     C*
     C     ERRCKW        BEGSR
     C*
     C*  ENTITY/LOCATION TYPE MUST BE EITHER 'E' OR 'L'
     C*
     C  N90SCELTP        IFNE      'E'
     C     SCELTP        ANDNE     'L'
     C                   SETON                                        21  90
     C                   ENDIF                                                  SCELTP<>E/L
     C*
     C                   ENDSR                                                  ERRCKW
     C*
     C*================================================================
     C* KJDO - Processing for F10
     C*================================================================
     C*
     C*  F10=SEARCH
     C*
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C*_______________________________________________________________
     C*
     C*  COUNTY CODE
     C*
     C     HFLD          WHENEQ    'SCCNTY'
     C*
     C     SCPST         IFEQ      *BLANKS
     C     SCMST         ANDEQ     *BLANKS
     C*
     C                   SETON                                        37
     C*
     C                   ELSE
     C*
     C     SCPST         IFNE      *BLANKS
     C                   MOVEL     SCPST         ZZST              2
     C                   ELSE
     C                   MOVEL     SCMST         ZZST
     C                   ENDIF                                                  SCPST<>*BLANKS
     C*
     C                   CALL      'W2215R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM                    XXKEY
     C                   PARM      ZZST          ##ST              2
     C                   PARM      2             ##VIEW            1 0
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     WTCNP                              60
     C  N602             SUBST     CNKEY:3       SCCNTY
     C  N60              MOVEL(P)  CNDESC        SCCNTD
     C                   MOVE      *ON           *IN82
     C*
     C                   ELSE
     C*
     C                   MOVE      *ON           *IN81
     C*
     C                   ENDIF                                                  ##RRN<>*ZEROS
     C*
     C                   ENDIF                                                  SCPST=*BLANKS=SCMST
     C*
     C*** BEGIN DEL *** 03/10/98 **************************************
     C*
     C*** BEG ADD ***** 12/08/97 ***********************************************
     C**********______________________________________________________
     C**********
     C*****TYPE*OF BUSINESS CODE SEARCH
     C**********
     C********** HFLD      WHEQ 'SCTPOB'
     C**********
     C**********           MOVEL'BUSI'    XTBID1    P
     C**********           MOVE *BLANK    XTBID2    P
     C**********
     C**********           CALL 'W2493R'
     C**********           PARM           ##DATA
     C**********           PARM '2'       ##CALL
     C**********           PARM           XTBID1
     C**********           PARM           XTBID2
     C**********           PARM           ##RTC   1
     C**********
     C********** XTBID2    IFNE *BLANKS
     C**********           MOVELXTBID2    SCTPOB    P
     C********** GTKY      CHAINPGT01                60
     C**********           MOVELGTDESC    SCTPBD    P
     C**********           ENDIF                           XXKEY<>*BLANKS
     C*** END ADD ***** 12/08/97 ***********************************************
     C*
     C*** END   DEL *** 03/10/98 **************************************
     C*
     C*** BEGIN ADD *** 03/10/98 **************************************
     C*
     C*   SCREEN 2
     C**    ERROR CHECKING - NOT ON VALID FIELD FOR SEARCH
     C*
     C     HFLD          WHENEQ    'SCNCCI'
     C                   MOVE      *ON           *IN53
     C*
     C     HFLD          WHENEQ    'SCCNT1'
     C                   MOVE      *ON           *IN54
     C*
     C     HFLD          WHENEQ    'SCCNT2'
     C                   MOVE      *ON           *IN55
     C*
     C     HFLD          WHENEQ    'SCCNT3'
     C                   MOVE      *ON           *IN56
     C*
     C*  TYPE OF BUSINESS -  SEARCH
     C*
     C     HFLD          WHENEQ    'SCTPOB'
     C*
     C                   MOVEL(P)  'BUSI'        XZTBID
     C*
     C                   CALL      'W2493R'
     C                   PARM                    ##DATA
     C                   PARM                    @@MODE
     C                   PARM                    XZTBID
     C                   PARM      *BLANKS       XXKEY
     C                   PARM      *BLANK        QQRTC             1
     C*
     C     QQRTC         IFEQ      'A'
     C                   MOVE      *ON           *INLR
     C                   ENDIF                                                  QQRTC=A
     C*
     C     QQRTC         IFEQ      '*'
     C     XXKEY         ANDNE     *BLANKS
     C                   MOVEL(P)  XXKEY         SCTPOB
     C*
     C                   MOVE      *BLANKS       SCTPBD
     C     GTKY1         CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCTPBD
     C                   ENDIF                                                  *IN60=OFF
     C*
     C***  BEGIN CHG  ***  02/05/99  **********************************
     C*******************  MOVE *ON       *IN52            SCCNT1 POSITION
     C*** BEGIN CHG ***  10/12/05  ************************************
     C*********            MOVE *ON       *IN88            SCSIC POSITION
     C     *IN64         IFEQ      *OFF
     C                   MOVE      *ON           *IN70                          SCNAOB POSITION
     C                   ELSE
     C                   MOVE      *ON           *IN88                          SCSIC POSITION
     C                   ENDIF                                                  *IN64 IFEQ
     C*** END CHG *****  10/12/05  ************************************
     C***  END   CHG  ***  02/05/99  **********************************
     C                   ENDIF                                                  QQRTC=*
     C*
     C     QQRTC         IFEQ      'B'
     C     XXKEY         OREQ      *BLANKS
     C                   MOVE      *ON           *IN82                          SCTPOB POSITION
     C                   ENDIF                                                  QQRTC=B
     C*
     C*** END   ADD *** 03/10/98 **************************************
     C*
     C***  BEGIN ADD  ***  02/05/99  **********************************
     C*   SIC code search
     C     HFLD          WHENEQ    'SCSIC'
     C*
     C                   MOVEL(P)  'SIC'         XZTBID
     C*
     C                   CALL      'W2493R'
     C                   PARM                    ##DATA
     C                   PARM                    @@MODE
     C                   PARM                    XZTBID
     C                   PARM      *BLANKS       XXKEY
     C                   PARM      *BLANK        QQRTC
     C*
     C     QQRTC         IFEQ      'A'
     C                   MOVE      *ON           *INLR
     C                   ENDIF                                                  QQRTC=A
     C*
     C     QQRTC         IFEQ      '*'
     C     XXKEY         ANDNE     *BLANKS
     C                   MOVEL(P)  XXKEY         SCSIC
     C*
     C                   MOVE      *BLANKS       SCSICD
     C     GTKY1         CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCSICD
     C                   ENDIF                                                  *IN60=OFF
     C*
      ***** BEG CHG ***** 05/12/06 ************************************
     C***************      MOVE *ON       *IN52            SCCNT1 POSITION
     C                   MOVE      *ON           *IN84                          SCNAIC POSITION
      ***** END CHG ***** 05/12/06 ************************************
     C                   ENDIF                                                  QQRTC=*
     C*
     C     QQRTC         IFEQ      'B'
     C     XXKEY         OREQ      *BLANKS
     C                   MOVE      *ON           *IN88                          SCSIC POSITION
     C                   ENDIF                                                  QQRTC=B
     C***  END   ADD  ***  02/05/99  **********************************
     C*
      ***** BEG ADD ***** 05/12/06 ************************************
     C*  NAIC code search
     C     HFLD          WHENEQ    'SCNAIC'
     C*
     C                   MOVEL(P)  'NAIC'        XZTBID
     C*
     C                   CALL      'W2493R'
     C                   PARM                    ##DATA
     C                   PARM                    @@MODE
     C                   PARM                    XZTBID
     C                   PARM      *BLANKS       XXKEY
     C                   PARM      *BLANK        QQRTC
     C*
     C     QQRTC         IFEQ      'A'
     C                   MOVE      *ON           *INLR
     C                   ENDIF                                                  QQRTC=A
     C*
     C     QQRTC         IFEQ      '*'
     C     XXKEY         ANDNE     *BLANKS
     C                   MOVEL(P)  XXKEY         SCNAIC
     C*
     C                   MOVE      *BLANKS       SCNADS
     C     GTKY1         CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCNADS
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   MOVE      *ON           *IN52                          SCCNT1 POSITION
     C                   ENDIF                                                  QQRTC=*
     C*
     C     QQRTC         IFEQ      'B'
     C     XXKEY         OREQ      *BLANKS
     C                   MOVE      *ON           *IN84                          SCNAIC POSITION
     C                   ENDIF                                                  QQRTC=B
      ***** END ADD ***** 05/12/06 ************************************
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*
     C***  BEGIN ADD  ***  04/03/98  *********************************
     C*===============================================================
     C* KNDO - SUBROUTINE FOR F14
     C*===============================================================
     C*
     C*  F14=DARTS
     C*
     C     KNDO          BEGSR
     C*
     C*  ESTABLISH KEY
     C*
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      ##CO#         QCCO#
     C                   MOVE      SCFND         QCFND
     C                   MOVE      SCEMP#        QCEMP#
     C                   MOVE      SCDIV         QCDIV
      ****1*** BEGIN ADD ******** 08/07/98 ****************************
     C  N41              MOVE      *BLANKS       QCQTE
     C   41              MOVE      'Q'           QCQTE
      ******** BEGIN ADD ******** 08/07/98 ****************************
     C*
     C*  CALL DARTS ENTRY PROGRAM
     C*
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM      'W0050R'      XXPGM
     C*
     C                   ENDSR                                                  KNDO
     C***  END   ADD  ***  03/30/98  *********************************
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C*
     C     MVDBSC        BEGSR
     C*
      ***** BEG ADD ***** 03/24/06 ************************************
     C     CLTID         IFEQ      '132'
     C                   MOVEL     ALSHFT        SCSHFT
     C                   ENDIF
      ***** END CHG ***** 03/24/06 ************************************
     C***  BEGIN ADD  ***  09/22/98  *********************************
     C     *IN40         IFEQ      *ON
     C                   MOVE      'Y'           ALNCCI
     C                   ENDIF                                                  *IN40=*ON
     C***  END   ADD  ***  09/22/98  *********************************
     C*
     C     ALEFDT        IFEQ      *ZERO
     C***  BEGIN ADD  ***  09/22/98  *********************************
     C     *IN40         ANDEQ     *OFF
     C***  END   ADD  ***  09/22/98  *********************************
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ALEFDT
     C                   ENDIF                                                  ALEFDT=*ZERO
     C*
     C                   Z-ADD     ALDLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDLDT
     C*
     C                   Z-ADD     ALEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   MOVEL     ALDLRS        SCDLRS
     C*** BEGIN CHG ***  04/13/09  ************************************
     C*************        MOVELALNAME    SCEMNM
     C*************        MOVELALFDNM    SCFDNM
     C                   MOVEL(P)  ALNAME        NAME1
     C                   MOVEL(P)  ALNAM2        NAME2
     C                   MOVE      NAME          SNAME
     C                   MOVEL(P)  SNAM1         SCEMNM
     C                   MOVEL(P)  SNAM2         SCEMN2
     C*
     C                   MOVEL(P)  ALFDNM        NAME1F
     C                   MOVEL(P)  ALFDN2        NAME2F
     C                   MOVE      NAMEF         SNAMEF
     C                   MOVEL(P)  SNAM1F        SCFDNM
     C                   MOVEL(P)  SNAM2F        SCFDN2
      ***** END   CHG ***** 04/13/09 **********************************
      ***** BEGIN ADD ***** 04/25/06 **********************************
     C                   MOVE      ALNAMP        SCNAMP
     C                   MOVE      ALFDNP        SCFDNP
      ***** END ADD ******* 04/25/06 **********************************
     C                   MOVEL     ALMAD1        SCMAD1
     C                   MOVEL     ALMAD2        SCMAD2
     C                   MOVEL     ALMCTY        SCMCTY
     C                   MOVEL     ALMST         SCMST
     C                   MOVEL     ALMZIP        SCMZP1
     C                   MOVE      ALMZIP        SCMZP2
     C                   MOVEL     ALPAD1        SCPAD1
     C                   MOVEL     ALPAD2        SCPAD2
     C                   MOVEL     ALPCTY        SCPCTY
     C                   MOVEL     ALPST         SCPST
     C                   MOVEL     ALPZIP        SCPZP1
     C                   MOVE      ALPZIP        SCPZP2
     C*
     C                   MOVEL     ALCNTY        SCCNTY
     C*
      ***** BEG ADD ***** 05/12/06 ************************************
     C*
     C     SCPST         IFGT      *BLANKS
     C                   MOVE      SCPST         ##RRST
     C                   ELSE
     C                   MOVE      SCMST         ##RRST
     C                   ENDIF                                                  END SCPST=*
      * IF ADD USE EMPLOYER STATE
     C     @@MODE        IFEQ      'A'                                          ADD MODE
     C     EMPST         IFGT      *BLANKS
     C                   MOVE      EMPST         ##RRST
     C                   ELSE
     C                   MOVE      EMMST         ##RRST
     C                   ENDIF                                                  END SCPST=*
     C                   ENDIF                                                  @@MODE=A
      *
     C     RRKY          CHAIN     PRR01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     RRNAIC        ANDEQ     'Y'
     C                   MOVE      *ON           *IN95
     C                   ENDIF                                                  SCNAIC=BLANKS
     C*
      *** BEG CHG ***** 07/14/08 **********************************
     C****************     MOVELALNAIC    SCNAIC
     C  N40              MOVEL     ALNAIC        SCNAIC
      *** BEG CHG ***** 07/14/08 **********************************
     C*
     C                   CLEAR                   SCNADS
     C                   MOVEL(P)  'NAIC'        XTBID1
     C                   MOVEL(P)  SCNAIC        XTBID2
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL     GTDESC        SCNADS
      ***** END ADD ***** 05/12/06 ************************************
     C*
     C     SCPST         IFNE      *BLANKS
     C     SCPST         CAT       SCCNTY:0      ZZKEY
     C                   ELSE
     C     SCMST         CAT       SCCNTY:0      ZZKEY
     C                   ENDIF                                                  SCPST<>*BLANKS
     C*
     C                   MOVEL(P)  ZZKEY         XXKEY
     C*
     C                   MOVEL     *BLANKS       SCCNTD
     C     TBKY          CHAIN     PCN01                              60
     C  N60              MOVEL(P)  CNDESC        SCCNTD
     C*
     C                   Z-ADD     ALAREA        SCAREA
     C                   MOVEL     ALPHN#        SCPHN1
     C                   MOVE      ALPHN#        SCPHN2
     C                   Z-ADD     ALFARA        SCFAXA
     C                   MOVEL     ALFAX#        SCFAX1
     C                   MOVE      ALFAX#        SCFAX2
     C                   MOVE      ALTPOB        SCTPOB
     C*** BEGIN ADD ***  10/12/05  ************************************
     C                   MOVEL     ALNAOB        SCNAOB
     C*** END ADD *****  10/12/05  ************************************
     C                   Z-ADD     ALFEIN        SCFEIN
     C                   MOVE      ALNCCI        SCNCCI
     *** BEG ADD *** 12/31/97 *****************************************
     C                   MOVE      ALCNT1        SCCNT1
     C                   MOVE      ALCNT2        SCCNT2
     C                   MOVE      ALCNT3        SCCNT3
     *** END ADD *** 12/31/97 *****************************************
     **** BEGIN ADD**** 10/11/00 *************************************
     C     *IN64         IFEQ      *ON
     *** BEG ADD *** 06/20/08 *****************************************
     C     *IN40         IFEQ      *ON
     *** END ADD *** 06/20/08 *****************************************
     C                   MOVE      EMSEMP        SCTXID
     C                   ELSE
     C                   MOVE      ALSEMP        SCTXID
     C                   ENDIF
     *** BEG ADD *** 06/20/08 *****************************************
     C                   ELSE
     C     *IN40         IFEQ      *ON
     C                   MOVE      '  '          SCTXID
     C                   ELSE
     C                   MOVE      ALSEMP        SCTXID
     C                   ENDIF
     C                   ENDIF
     *** END ADD *** 06/20/08 *****************************************
     **** END   ADD**** 10/11/00 *************************************
      **** BEGIN ADD **** 02/23/01 *********************************************
     C     *IN64         IFEQ      *ON
      ***** BEG ADD ******* 09/30/03 **********************************
     C     *IN40         ANDEQ     *ON
      ***** END ADD ******* 09/30/03 **********************************
     C                   MOVEL(P)  EMUE#         SCUE#
     C                   ELSE
     C                   MOVEL(P)  ALUE#         SCUE#
     C                   ENDIF
      **** END   ADD **** 02/23/01 *********************************************
     C*
     C*  IF ADDING A LOCATION, BE SURE TO LOAD THE PROTECTED FIELDS:
     C*  NAME, TYPE OF BUSINESS AND FEDERAL ID# WITH
     C*  VALUES FROM EMPLOYER MASTER; THESE CANNOT BE CHANGED
     C*
     C     SCELTP        IFEQ      'L'
     C     *IN40         ANDEQ     *ON
     C*** BEGIN CHG ***  04/13/09  ************************************
     C*************        MOVELEMNAME    SCEMNM
     C*************        MOVELEMFDNM    SCFDNM
     C                   MOVEL(P)  EMNAME        NAME1
     C                   MOVEL(P)  EMNAM2        NAME2
     C                   MOVE      NAME          SNAME
     C                   MOVEL(P)  SNAM1         SCEMNM
     C                   MOVEL(P)  SNAM2         SCEMN2
     C*
     C                   MOVEL(P)  EMFDNM        NAME1F
     C                   MOVEL(P)  EMFDN2        NAME2F
     C                   MOVE      NAMEF         SNAMEF
     C                   MOVEL(P)  SNAM1F        SCFDNM
     C                   MOVEL(P)  SNAM2F        SCFDN2
      ***** END   CHG ***** 04/13/09 **********************************
      ***** BEGIN ADD ***** 04/25/06 **********************************
     C                   MOVE      EMNAMP        SCNAMP
     C                   MOVE      EMFDNP        SCFDNP
      ***** END ADD ******* 04/25/06 **********************************
     C                   Z-ADD     EMFEIN        SCFEIN
     C                   MOVEL     EMTPOB        SCTPOB
     C                   ENDIF                                                   SCELTP=L/*IN40=*ON
     C*
     C*  IF ADDING AN ENTITY/LOCATION THEN ONLY SHOW "DELETION DATE"
     C*  & "REASON" IF NOT ZERO/BLANK, OTHERWISE IF DELETING AN
     C*  ENTITY/LOCATION ALWAYS SHOW "DELETION DATE" & "REASON"
     C*
     C     SCDLDT        IFNE      *ZEROS
     C     SCDLRS        ORNE      *BLANKS
     C                   MOVE      *OFF          *IN45
     C                   ELSE
     C                   MOVE      *ON           *IN45
     C                   END
     C*
     C*  DON'T DISPLAY DELETION DATE & REASON IF IN ADD MODE.
     C*
     C     @@MODE        IFEQ      'A'                                          ADD MODE
     C                   MOVE      *ON           *IN45
     C                   ENDIF                                                  @@MODE=A
     C*
     C*** BEGIN ADD *** 04/30/99 ***********************************************
     C     *IN63         IFEQ      *ON
     C                   MOVE      *OFF          *IN45
     C                   END
     C*** END ADD ***** 04/30/99 ***********************************************
     C*
     C     FIRST         IFEQ      *ON
     C                   MOVEL     SCELID        SVELID
     C                   MOVE      *OFF          FIRST
     C                   ENDIF                                                  FIRST=*ON
     C*** BEG ADD ***** 12/08/97 ***********************************************
     C*
     C*  LOAD THE TYPE OF BUSINESS DESCRIPTION
     C*
     C                   CLEAR                   SCTPBD
     C*
     C                   MOVEL     'BUSI'        XTBID1
     C                   MOVEL(P)  SCTPOB        XTBID2
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL     GTDESC        SCTPBD
     C*** END ADD ***** 12/08/97 ***********************************************
     C*
     C***  BEGIN ADD  ***  02/05/99  **********************************
     C*  Load the SIC code DESCRIPTION
     C*
     C                   MOVE      ALSIC         SCSIC
     C                   CLEAR                   SCSICD
     C*
      ***** BEG CHG ***** 05/12/06 ************************************
     C**************       MOVEL'SIC'     XTBID1
     C                   MOVEL(P)  'SIC'         XTBID1
      ***** END CHG ***** 05/12/06 ************************************
     C                   MOVEL(P)  SCSIC         XTBID2
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL     GTDESC        SCSICD
     C***  END   ADD  ***  02/05/99  **********************************
      ***** BEG ADD ******* 10/17/03 **********************************
      *
      * Auto fill fields from Employer master on ADD of location
      *  (Mailing addr, phone #, Fax #, SIC code)
      *
     C     SCELTP        IFEQ      'L'
     C     *IN40         ANDEQ     *ON
      *
     C                   MOVEL     EMMAD1        SCMAD1
     C                   MOVEL     EMMAD2        SCMAD2
     C                   MOVEL     EMMCTY        SCMCTY
     C                   MOVEL     EMMST         SCMST
     C                   MOVEL     EMMZIP        SCMZP1
     C                   MOVE      EMMZIP        SCMZP2
      *
     C                   Z-ADD     EMAREA        SCAREA
     C                   MOVEL     EMPHN#        SCPHN1
     C                   MOVE      EMPHN#        SCPHN2
     C                   Z-ADD     EMFAXA        SCFAXA
     C                   MOVEL     EMFAX#        SCFAX1
     C                   MOVE      EMFAX#        SCFAX2
      *
     C                   MOVE      EMSIC         SCSIC
     C                   CLEAR                   SCSICD
     C*
      ***** BEG CHG ***** 05/12/06 ************************************
     C**************       MOVEL'SIC'     XTBID1
     C                   MOVEL(P)  'SIC'         XTBID1
      ***** END CHG ***** 05/12/06 ************************************
     C                   MOVEL(P)  SCSIC         XTBID2
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL     GTDESC        SCSICD
      ***** BEG ADD ***** 03/26/09 ************************************
     C                   MOVE      EMNAIC        SCNAIC
     C                   CLEAR                   SCNADS
     C*
     C                   MOVEL(P)  'NAIC'        XTBID1
     C                   MOVEL(P)  SCNAIC        XTBID2
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL     GTDESC        SCNADS
      ***** END ADD ***** 03/26/09 ************************************
      *** BEG ADD ***** 07/14/08 **************************************
     C     CLTID         IFEQ      '071'
     C                   MOVEL     EMDBA         XXDBA             7 0
     C     XXDBA         IFNE      0
     C                   MOVEL(P)  EMDBA         SCGSI#
     C                   ENDIF
     C                   ENDIF
     C                   MOVEL(P)  EMNAIC        SCNAIC
      *** END ADD ***** 07/14/08 **************************************
     C                   ENDIF                                                   SCELTP
      *
      ***** END ADD ******* 10/17/03 **********************************
      *** BEG ADD ***** 09/20/06 **************************************
      *** BEG CHG ***** 07/14/08 **************************************
     C************         MOVELALGSI#    SCGSI#    P
     C************         MOVELALSTEM    SCSTEM    P
     C  N40              MOVEL(P)  ALGSI#        SCGSI#
     C  N40              MOVEL(P)  ALSTEM        SCSTEM
      *** END CHG ***** 07/14/08 **************************************
      *** END ADD ***** 09/20/06 **************************************
     C*
      ***  BEG ADD ******  01/04/10  *********************************
     C                   EXSR      FLXHDG
      ***  END ADD ******  01/04/10  *********************************
     C                   ENDSR                                                  MVDBSC
     C*
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C*
     C     MVSCDB        BEGSR
     C*
      ***** BEG ADD ***** 03/24/06 ************************************
     C     CLTID         IFEQ      '132'
     C                   MOVEL     SCSHFT        ALSHFT
     C                   ENDIF
      ***** END CHG ***** 03/24/06 ************************************
     C*
     C                   Z-ADD     SCDLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ALDLDT
     C*
     C                   MOVEL     SCDLRS        ALDLRS
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ALEFDT
     C*
     C*** BEGIN CHG ***  04/13/09  ************************************
     C*************        MOVELSCEMNM    ALNAME
     C*************        MOVELSCFDNM    ALFDNM
     C                   MOVEL(P)  SCEMNM        SNAM1
     C                   MOVEL(P)  SCEMN2        SNAM2
     C                   MOVE      SNAME         NAME
     C                   MOVEL(P)  NAME1         ALNAME
     C                   MOVEL(P)  NAME2         ALNAM2
     C*
     C                   MOVEL(P)  SCFDNM        SNAM1F
     C                   MOVEL(P)  SCFDN2        SNAM2F
     C                   MOVE      SNAMEF        NAMEF
     C                   MOVEL(P)  NAME1F        ALFDNM
     C                   MOVEL(P)  NAME2F        ALFDN2
      ***** END   ADD ***** 04/13/09 **********************************
      ***** BEGIN ADD ***** 04/25/06 **********************************
     C                   MOVE      SCNAMP        ALNAMP
     C                   MOVE      SCFDNP        ALFDNP
      ***** END ADD ******* 04/25/06 **********************************
     C                   MOVEL     SCMAD1        ALMAD1
     C                   MOVEL     SCMAD2        ALMAD2
     C                   MOVEL     SCMCTY        ALMCTY
     C                   MOVEL     SCMST         ALMST
     C                   MOVEL     SCMZP1        ALMZIP
     C                   MOVE      SCMZP2        ALMZIP
     C                   MOVEL     SCPAD1        ALPAD1
     C                   MOVEL     SCPAD2        ALPAD2
     C                   MOVEL     SCPCTY        ALPCTY
     C                   MOVEL     SCPST         ALPST
     C                   MOVEL     SCPZP1        ALPZIP
     C                   MOVE      SCPZP2        ALPZIP
     C                   MOVEL     SCCNTY        ALCNTY
     C                   Z-ADD     SCAREA        ALAREA
     C                   MOVEL     SCPHN1        ALPHN#
     C                   MOVE      SCPHN2        ALPHN#
     C                   Z-ADD     SCFAXA        ALFARA
     C                   MOVEL     SCFAX1        ALFAX#
     C                   MOVE      SCFAX2        ALFAX#
     C                   MOVE      SCTPOB        ALTPOB
     C*
      ***** BEG ADD ***** 05/12/06 ************************************
     C                   MOVEL     SCNAIC        ALNAIC
      ***** END ADD ***** 05/12/06 ************************************
     C*
     C*** BEGIN ADD ***  10/12/05  ************************************
     C                   MOVEL     SCNAOB        ALNAOB
     C*** END ADD *****  10/12/05  ************************************
     C***  BEGIN ADD  ***  02/05/99  **********************************
     C                   MOVE      SCSIC         ALSIC
     C***  END   ADD  ***  02/05/99  **********************************
     C                   Z-ADD     SCFEIN        ALFEIN
     C                   MOVE      SCNCCI        ALNCCI
     *** BEG ADD *** 12/31/97 *****************************************
     C                   MOVE      SCCNT1        ALCNT1
     C                   MOVE      SCCNT2        ALCNT2
     C                   MOVE      SCCNT3        ALCNT3
     *** END ADD *** 12/31/97 *****************************************
     **** BEGIN ADD**** 10/11/00 *************************************
     C                   MOVE      SCTXID        ALSEMP
     **** END   ADD**** 10/11/00 *************************************
      **** BEGIN ADD **** 02/23/01 *********************************************
     C                   MOVEL(P)  SCUE#         ALUE#
      **** END   ADD **** 02/23/01 *********************************************
     C                   MOVE      Q1USER        ALUSER
     C                   MOVE      Q1PGM         ALPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ALDT
     C*
     C                   Z-ADD     DSTIME        ALTIME
     C*** BEGIN ADD *** 04/30/99 ***********************************************
     C     LDENDA        IFNE      *ZEROS
     C     LDENDA        ANDNE     *BLANKS
     C*
     C     *IN40         IFEQ      *ON
     C                   MOVE      'A'           ALSTAT
     C                   ELSE
     C     *IN63         IFEQ      *ON
     C                   MOVE      'X'           ALSTAT
     C                   ELSE
     C                   MOVE      'C'           ALSTAT
     C                   END
     C                   END
     C*
     C                   END
     C*** END ADD ***** 04/30/99 ***********************************************
      *** BEG ADD ***** 09/20/06 **************************************
     C                   MOVEL(P)  SCGSI#        ALGSI#
     C                   MOVEL(P)  SCSTEM        ALSTEM
      *** END ADD ***** 09/20/06 **************************************
     C*
     C                   ENDSR                                                  MVSCDB
     C*
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C*
     C     OTHERR        BEGSR
     C                   ENDSR                                                  OTHERR
     C*
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C*
     C     OTHFKS        BEGSR
     C*
     C     *INKJ         CASEQ     *ON           KJDO                     90
      ******* BEGIN ADD ***** 04/03/98 ********************************
     C     *INKN         CASEQ     *ON           KNDO                     90
      ******* END   ADD ***** 04/03/98 ********************************
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C*
     C     PGMEND        BEGSR
     C*** BEG ADD ***** 01/30/98 ***********************************************
     C*
     C*  CREATE MID-TERM ENDORSEMENT RECORDS
     C*
     C*** BEGIN DEL *** 04/30/99 ***********************************************
     C*********************EXSR CKCHGS
     C*** END DEL ***** 04/30/99 ***********************************************
     C*** END ADD ***** 01/30/98 ***********************************************
     C*
     C*  IF NOT YET DELETED, DELETE THE DUMMY RECORD
     C*
     C                   Z-ADD     SCEMP#        XXEMP#
     C                   Z-ADD     SCDIV         XXDIV
     C                   MOVEL     SCELTP        XXELTP
     C                   MOVEL     SVELID        XXELID
     C*
     C     ALKY          CHAIN     PAL01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     ALPGM         ANDEQ     XXDPGM
     C     DBRRNA        DELETE    DBRCDF                             60
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C*** BEG ADD ***** 03/05/98 ***********************************************
     C*
     C*  IF ADD MODE FORCE CALL TO OWNER/OFFICER WORK WITH
     C*
     C     *IN40         IFEQ      *ON
     C     *INKC         ANDEQ     *OFF
     C     *INKL         ANDEQ     *OFF
     C*
     C***  BEGIN CHG ***  11/18/99  **********************************
     C**********           MOVE @@MODE    ##CALL
     C  N09              MOVE      ' '           ##CALL
     C   09              MOVE      '1'           ##CALL
     C***  END   CHG ***  11/18/99  **********************************
     C*
     C     SCELTP        IFEQ      'L'
     C                   MOVE      '1'           ##CALL
     C                   ENDIF                                                  SCELTP=L
     C*
      ***** BEG ADD ******* 08/17/99 **********************************
     C     SCELTP        IFNE      'L'
      ***** END ADD ******* 08/17/99 **********************************
     C*
     C                   CALL      'W1057R'                               99
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM                    ##CALL            1
     C                   PARM      DBRRNB        ##RRN             9 0
     C                   PARM      SCELTP        ##ELFL            1
     C***  BEGIN CHG  ***  09/01/98  *********************************
     C********BEGIN ADD ******** 08/07/98 *****************************
     C**********           PARM @@QTE     ##QTE   1
     C********END   ADD ******** 08/07/98 *****************************
     C                   PARM      XXQTE         ##QTE             1
     C***  END   CHG  ***  09/01/98  *********************************
      ***** BEG ADD ******* 08/17/99 **********************************
      *
     C                   ENDIF                                                  ELTP NE L
      ***** END ADD ******* 08/17/99 **********************************
     C*
     C*** BEGIN ADD *** 11/16/99 ***********************************************
     C     WCFL14        IFEQ      'Y'
     C*** END ADD ***** 11/16/99 ***********************************************
     C*
     C***  BEGIN ADD ***  11/18/99  **********************************
     C  N09              MOVE      ' '           ##CALL
     C   09              MOVE      '1'           ##CALL
     C***  END   ADD ***  11/18/99  **********************************
     C*
     C*** BEGIN ADD *** 04/30/99 ***********************************************
     C                   CALL      'W0075R'      PL0075                   99
     C*** END ADD ***** 04/30/99 ***********************************************
     C*
     C*** BEGIN ADD *** 11/16/99 ***********************************************
     C                   END
     C*** END ADD ***** 11/16/99 ***********************************************
     C*
     C                   ENDIF
     C*** END ADD ***** 03/05/98 ***********************************************
     C*
      ***** BEG CHG ******* 01/02/01 **********************************
     C***40********        MOVE *ON       *INLR
     C                   MOVE      *ON           *INLR
      ***** END CHG ******* 01/02/01 **********************************
     C*
     C                   ENDSR                                                  PGMEND
     C*
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C*
     C     PGMSTR        BEGSR
      ***** BEG ADD ******* 03/24/06 **********************************
      *
      * Gets the Id for this client
      *
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
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
     C     CLTID         IFEQ      '132'
     C                   MOVE      *ON           *IN44
     C                   ELSE
     C                   MOVE      *OFF          *IN44
     C                   ENDIF
      ***** END ADD ******* 03/24/06 **********************************
      *** BEG ADD ***** 07/22/08 **************************************
     C     CLTID         IFEQ      '071'
     C                   MOVE      *ON           *IN46
     C                   ELSE
     C                   MOVE      *OFF          *IN46
     C                   ENDIF
      *** END ADD ***** 07/22/08 **************************************
      *** BEGIN ADD *** 08/10/98 ***********************************************
     C*
     C*  RETRIEVE MIDTERM ENDORSEMENT INFORMATION
     C*
     C                   MOVE      *ZEROS        SVLDA
     C     *DTAARA       DEFINE    *LDA          SVLDA
     C     *LOCK         IN        *DTAARA
     C                   OUT       *DTAARA
      *** END ADD ***** 08/10/98 ***********************************************
     C*
     C                   Z-ADD     ##CO#         XXCO#
     C*
     C     @@MODE        IFEQ      'A'
     C*
     C     @@RRN2        CHAIN     WMEMP                              60
     C*
     C                   Z-ADD     EMCO#         XXCO#
     C                   Z-ADD     EMFND         XXFND
     C                   Z-ADD     EMEMP#        SCEMP#
     C                   Z-ADD     EMDIV         SCDIV
     C*
     C*** BEG ADD ***** 01/30/98 ***********************************************
     C*
     C*  CLEAR THE SAVED DATA BASE VALUES
     C*
     C                   MOVE      *ZEROS        SVFEIN
     C                   MOVEL     *BLANKS       SVEMNM
     C                   MOVEL     *BLANKS       SVFDNM
     C                   MOVEL     *BLANKS       SVMAD1
     C                   MOVEL     *BLANKS       SVMAD2
     C                   MOVEL     *BLANKS       SVMCTY
     C                   MOVEL     *BLANKS       SVMST
     C                   MOVE      *ZEROS        SVMZIP
     C                   MOVEL     *BLANKS       SVPAD1
     C                   MOVEL     *BLANKS       SVPAD2
     C                   MOVEL     *BLANKS       SVPCTY
     C                   MOVEL     *BLANKS       SVPST
     C                   MOVE      *ZEROS        SVPZIP
     C                   MOVE      *ZEROS        SVCNTY
     C                   MOVEL     *BLANKS       SVCNTD
     C                   MOVEL     *BLANKS       SVCNT1
     C                   MOVEL     *BLANKS       SVCNT2
     C                   MOVEL     *BLANKS       SVCNT3
     C                   MOVEL     *BLANKS       SVCNTD
     C*** END ADD ***** 01/30/98 ***********************************************
     C*
     C                   ELSE
     C*
     C     @@RRN         CHAIN     WMALP                              60
     C*
     C                   Z-ADD     ALCO#         XXCO#
     C                   Z-ADD     ALFND         XXFND
     C                   Z-ADD     ALEMP#        SCEMP#
     C                   Z-ADD     ALDIV         SCDIV
     C                   MOVEL     ALELTP        SCELTP
     C                   MOVEL     ALELID        SCELID
     C*** BEG ADD ***** 01/30/98 ***********************************************
     C*
     C*  SAVE THE OLD DATA BASE VALUES
     C*
     C                   MOVE      ALFEIN        SVFEIN
     C                   MOVEL     ALNAME        SVEMNM
     C                   MOVEL     ALFDNM        SVFDNM
     C                   MOVEL     ALMAD1        SVMAD1
     C                   MOVEL     ALMAD2        SVMAD2
     C                   MOVEL     ALMCTY        SVMCTY
     C                   MOVEL     ALMST         SVMST
     C                   MOVE      ALMZIP        SVMZIP
     C                   MOVEL     ALPAD1        SVPAD1
     C                   MOVEL     ALPAD2        SVPAD2
     C                   MOVEL     ALPCTY        SVPCTY
     C                   MOVEL     ALPST         SVPST
     C                   MOVE      ALPZIP        SVPZIP
     C                   MOVE      ALCNTY        SVCNTY
     C                   MOVEL     ALCNT1        SVCNT1
     C                   MOVEL     ALCNT2        SVCNT2
     C                   MOVEL     ALCNT3        SVCNT3
     C*
     C     ALPST         IFNE      *BLANKS
     C     ALPST         CAT(P)    ALCNTY:0      XXKEY
     C                   ELSE
     C     ALMST         CAT       ALCNTY:0      XXKEY
     C                   ENDIF                                                  ALPST<>*BLANKS
     C*
     C     TBKY          CHAIN     PCN01                              60
     C  N60              MOVEL(P)  CNDESC        SVCNTD
     C*** END ADD ***** 01/30/98 ***********************************************
     C*
     C                   ENDIF                                                  @@MODE=A
     C*
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
      *** BEG ADD ***** 07/16/08 **************************************
     C     @@MODE        COMP      'A'                                    77    PC TO EFF DT
      *** END ADD ***** 07/16/08 **************************************
     C***  BEGIN CHG  ***  09/01/98  *********************************
     C**********BEGIN ADD ********** 08/07/98 *************************
     C********** @@QTE     COMP 'Q'                      41QUOTE
     C**********END   ADD ********** 08/07/98 *************************
     C*
     C     Q1PRMS        IFGE      6
     C*** BEGIN CHG *** 04/30/99 ***********************************************
     C     @@QTE         IFEQ      'A'
     C     @@QTE         OREQ      'C'
     C     @@QTE         OREQ      'X'
     C                   MOVE      *BLANKS       XXQTE
     C                   ELSE
     C                   MOVE      @@QTE         XXQTE
     C                   END
     C*** END CHG ***** 04/30/99 ***********************************************
     C                   ELSE
     C                   MOVEL     *BLANKS       XXQTE
     C                   ENDIF                                                  Q1PRMS>=6
     C*
     C     XXQTE         COMP      'Q'                                    41    QUOTE
     C***  END   CHG  ***  09/01/98  *********************************
      **** BEGIN ADD **** 12/16/98 *********************************************
     C     F2KY          CHAIN     PF201                              60
      **** END   ADD **** 12/16/98 *********************************************
     C*
     C*** BEGIN CHG *** 04/30/99 ***********************************************
     C                   MOVE      *OFF          *IN63
     C     @@MODE        IFEQ      'E'                                          DELETE
     C*********************EXSR SFTDLT
     C*********************RETRN
     C                   MOVE      *ON           *IN63
     C                   MOVE      'C'           @@MODE
     C                   ENDIF                                                  @@MODE=E
     C*** END CHG ***** 04/30/99 ***********************************************
     C*
     C                   MOVEL     SCELID        SVELID
     C*
     C*  GET GROUP INFORMATION
     C*
     C     FNKY          CHAIN     PFN01                              60
     C                   Z-ADD     FNFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
      **** BEGIN DEL **** 12/16/98 *********************************************
      **** BEGIN ADD **** 10/14/98 *********************************************
     C********** F2KY      CHAINPF201                60
      **** END   ADD **** 10/14/98 *********************************************
      **** END   DEL **** 12/16/98 *********************************************
     C*
     C*  GET EMPLOYER INFORMATION
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C*  COMPRESS EMP#/DIV/NAME
     C*
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        XXEMP#
     C                   PARM      EMDIV         XXDIV
     C                   PARM      EMNAME        XXNAME
     C                   PARM                    XXEDN            80
     C*
     C                   MOVEL     XXEDN         SCEDN
     C*
     C     @@MODE        IFEQ      'A'
     C*
     C     *IN90         DOUEQ     *OFF
     C                   MOVE      *OFF          *IN90
     C                   EXFMT     WDW01
     C     *INKC         IFEQ      *ON
     C     *INKL         OREQ      *ON
     C                   RETURN
     C                   ELSE
     C                   EXSR      ERRCKW
     C                   ENDIF                                                  *INKC=*ON=*INKL
     C                   ENDDO                                                  *IN90=*OFF
     C*
     C*  ASSIGN ID# AND WRITE A DUMMY RECORD
     C*
     C                   MOVE      *OFF          *IN99
     C*
     C     ALKY2         SETGT     PAL01
     C     ALKY2         READPE    PAL01                                  60
     C*
     C     *IN99         DOWEQ     *OFF
     C     *IN60         ANDEQ     *OFF
     C                   TESTN                   ALELID               9999
     C  N99ALKY2         READPE    PAL01                                  60
     C                   ENDDO                                                  *IN99=*OFF
     C*
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     1             XXWK10           10 0
     C                   MOVEL     XXWK10        SCELID
     C                   ELSE
     C                   MOVE      ALELID        XXWK10
     C                   ADD       1             XXWK10
     C                   MOVEL     XXWK10        SCELID
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   Z-ADD     XXCO#         ALCO#
     C                   Z-ADD     XXFND         ALFND
     C                   Z-ADD     SCEMP#        ALEMP#
     C                   Z-ADD     SCDIV         ALDIV
     C                   MOVEL     SCELTP        ALELTP
     C                   MOVEL     SCELID        ALELID
     C                   MOVEL     Q1USER        ALUSER
     C                   MOVEL     Q1USER        ALIUSR
     C                   MOVEL     XXDPGM        ALPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ALDT
     C*
     C                   Z-ADD     DSTIME        ALTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ALIDT
     C*
     C                   WRITE     DBRCDF
     C*
     C                   ENDIF                                                  @@MODE=A
     C*
     C*  DETERMINE WHETHER PROGRAM WAS CALLED TO ADD/DEL AN
     C*  ENTITY OR A LOCATION
     C*
     C     SCELTP        COMP      'L'                                    64
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C**** BEGIN ADD *** 12/09/97 *************************************
     C     EXKY          CHAIN     PEX01                              60
     C*
     C  N90
     CANN60
     CANN64EXFIFG        IFEQ      'Y'
     C*
     C                   SETON                                        50  90
     C*
     C                   ENDIF                                                  EXFIFG IFEQ 'Y'
     C**** END   ADD *** 12/09/97 *************************************
     *** BEG ADD *** 12/31/97 *****************************************
      ***************************************************************** -
      *
      * LOAD FLEXIBLE FIELD FROM ADMIN CONTROL FILE
      *
     C                   DO        3             I                 2 0
      *
     C                   MOVE(P)   DOTS          HDG(I)
      *
     C     CNT(I)        IFNE      *BLANK
     C     CNT(I)        CAT       ' ':0         HDG(I)
      *
     C                   ELSE
     C     XXHD(I)       CAT       ' ':0         HDG(I)
      *
     C                   ENDIF
      *
      * SCREEN DISPLAY RULE: YOU CAN'T HAVE ONLY ONE DOT IN PROMPT TEXT
      *
     C     ' '           CHECKR    CNT(I)        XXLEN             3 0
      *
     C     XXLEN         IFGE      16
      *
     C                   MOVEL(P)  CNT(I)        HDG(I)
      *
     C                   ENDIF
      *
     C                   ENDDO                                                  DO I= 1 TO 3
     *** END ADD *** 12/31/97 *****************************************
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C*
     C                   MOVE      *ON           FIRST             1
     C                   MOVE      'N'           OVRFLG
     C***  BEGIN ADD  ***  09/22/98  *********************************
     C*
     C*  SAVE INCEPTION DATE
     C*
     C                   Z-ADD     1             I1                1 0
     C*
     C     *ZERO         LOOKUP    EF(I1)                                 99
     C*
     C     *IN99         IFEQ      *ON
     C                   SUB       1             I1
     C                   ELSE
     C                   Z-ADD     5             I1
     C                   ENDIF                                                  *IN61 IFEQ *ON
     C*
     C                   Z-ADD     EF(I1)        SVEFDT
     C***  END   ADD  ***  09/22/98  *********************************
     C*
     C***  BEGIN ADD  ***  02/05/99  **********************************
     C                   MOVEL     'WC'          XXSYK
     C     SYKY          CHAIN     SWC01                              60
     C     WCFL14        COMP      'Y'                                    94
     C***  END   ADD  ***  02/05/99  **********************************
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C*
     C     PREOTH        BEGSR
     C                   ENDSR
     C*
     C*================================================================
     C* SFTDLT - Processing to handle Opt 4 from Work with program
     C*================================================================
     C*
     C*  INSTEAD OF A HARD DELETE WHEN OPT 4 IS SELECTED FROM THE WORK
     C*  WITH PROGRAM A DELETION DATE AND REASON WILL BE FORCED INTO
     C*  THE RECORD
     C*
     C     SFTDLT        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      GETDTM
     C*
     C     SAVEDT        IFGE      ALEFDT
     C                   Z-ADD     SAVEDT        ALDLDT
     C                   ELSE
     C                   Z-ADD     ALEFDT        ALDLDT
     C                   ENDIF                                                  SAVEDT>=ALEFDT
     C*** BEGIN ADD *** 08/10/98 ***********************************************
     C*
     C*  IF PROCESSING FOR MIDTERM END OVERRIDE WITH END EFF DATE
     C*
     C     LDENDA        IFNE      *BLANKS
     C     LDEEDT        IFNE      *ZEROS
     C                   Z-ADD     LDEEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ALDLDT
     C                   END
     C                   END
     C*** END ADD ***** 08/10/98 ***********************************************
     C*
     C                   MOVEL     DLTRSN        ALDLRS
     C*
     C                   MOVEL     Q1USER        ALUSER
     C                   MOVEL     Q1PGM         ALPGM
     C                   Z-ADD     SAVEDT        ALDT
     C                   Z-ADD     SAVETM        ALTIME
     C*
     C                   UPDATE    DBRCDF
     C*** BEG ADD ***** 01/30/98 ***********************************************
     C*
     C*  CLR THE FLDS THAT WILL BE CHECKED TO ALLOW DELETE TO SHOW CORRECTLY
     C*
     C                   CLEAR                   ALFEIN
     C                   CLEAR                   ALNAME
     C                   CLEAR                   ALFDNM
     C                   CLEAR                   ALMAD1
     C                   CLEAR                   ALMAD2
     C                   CLEAR                   ALMCTY
     C                   CLEAR                   ALMST
     C                   CLEAR                   ALMZIP
     C                   CLEAR                   ALPAD1
     C                   CLEAR                   ALPAD2
     C                   CLEAR                   ALPCTY
     C                   CLEAR                   ALPST
     C                   CLEAR                   ALPZIP
     C                   CLEAR                   ALCNTY
     C                   CLEAR                   ALCNT1
     C                   CLEAR                   ALCNT2
     C                   CLEAR                   ALCNT3
     C                   CLEAR                   SCCNTD
     C*
     C*  CREATE MID-TERM ENDORSEMENT RECORDS
     C*
     C*** BEGIN DEL *** 04/30/99 ***********************************************
     C*********************EXSR CKCHGS
     C*** END DEL ***** 04/30/99 ***********************************************
     C*** END ADD ***** 01/30/98 ***********************************************
     C*
E01  C                   ENDIF                                                  *IN99=*OFF
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C*
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C*
     C                   ENDSR                                                  UPDATR
     C*
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C*
     C     WRITER        BEGSR
     C*
     C                   Z-ADD     SCEMP#        XXEMP#
     C                   Z-ADD     SCDIV         XXDIV
     C                   MOVEL     SCELTP        XXELTP
     C                   MOVEL     SCELID        XXELID
     C*
     C     ALKY          CHAIN     PAL01                              60
     C  N60DBRRNA        CHAIN     DBRCDF                             60
     C*
     C   60              CLEAR                   DBRCDF
     C*
     C                   Z-ADD     XXCO#         ALCO#
     C                   Z-ADD     XXFND         ALFND
     C                   Z-ADD     SCEMP#        ALEMP#
     C                   Z-ADD     SCDIV         ALDIV
     C                   MOVEL     SCELTP        ALELTP
     C                   MOVEL     SCELID        ALELID
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVE      Q1USER        ALIUSR
     C                   MOVE      Q1PGM         ALIPGM
     C                   MOVEL     SRDTO         ALIDT
     C                   Z-ADD     DSTIME        ALITM
     C*
     C   60              WRITE     DBRCDF
     C  N60              UPDATE    DBRCDF
     C*
     C                   FEOD      WMALP
     C*
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDSR                                                  WRITER
      ***  BEG ADD ******  01/04/10  *********************************
      *===============================================================
      * FLXHDG - "State Risk ID #" heading on SCR03 will be soft-coded
      *          " If file field WDRRP/RRSEMH is blank then display "State Risk
      *          ID #" as heading.
      *===============================================================
     C     FLXHDG        BEGSR
     C*
     C     SCPST         IFGT      *BLANKS
     C                   MOVE      SCPST         ##RRST
     C                   ELSE
     C                   MOVE      SCMST         ##RRST
     C                   ENDIF
     C*
     C***  Begin Chg  ***  07/06/11  **************************************
     C********** RRKY      CHAINPRR01                60
     C********** *IN60     IFEQ *OFF
     C********** RRSEMH    IFNE ' '
     C                   CLEAR                   RRSEMH
     C     RRKY          CHAIN     PRR01                              60
     C     RRSEMH        IFEQ      *BLANKS
     C                   MOVEL(P)  XXSEMH        RRSEMH
     C                   ENDIF
     C***  End   Chg  ***  07/06/11  **************************************
     C*
     C                   MOVE(P)   DOTS          SCSEMH
     C*
     C     RRSEMH        CAT       ' ':0         SCSEMH
     C     ' '           CHECKR    RRSEMH        XXLEN             3 0
     C*
     C***  Begin Chg  ***  07/06/11  **************************************
     C********** XXLEN     IFGE 20
     C     XXLEN         IFGE      18
     C***  End   Chg  ***  07/06/11  **************************************
     C                   MOVEL(P)  RRSEMH        SCSEMH
     C                   ENDIF                                                  IFGE 20
     C*
     C***  Begin Del  ***  07/06/11  **************************************
     C**********           ELSE
     C**********
     C**********           MOVE *ON       *IN77
     C**********
     C**********           ENDIF                           RRSEMH=' '
     C**********           ENDIF                           IN60 = *OFF
     C***  End   Del  ***  07/06/11  **************************************
     C                   ENDSR
      *
      ***  END ADD ******  01/04/10  *********************************
**
CONTACT 1
CONTACT 2
CONTACT 3
