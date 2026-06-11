     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*****************************************************************
     H/TITLE W0553R02 - AGENT HISTORY INFORMATION
     H*****************************************************************
     H*                                                               *
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
     H*                OR ADD RECORDS IN THE AGENT HISTORY FILE.      *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0553R - WORK WITH AGENT HISTORY                           *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    W4150R - QUOTE STATUS (F9)                                 *
     H*                                                               *
     H*  CREATED: 07/22/97  PROGRAMMER: C:33  PROJECT: 60AND          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 09/18/13 C:180 Support WA146 82907                            *
     H*                New renewal quote statuses.                    *
     H*                                                               *
     H* 09/07/10 C:189 Support: 73642/73469                           *
     H*                System allows a user to add an Agent History   *
     H*                record for a year, regardless of the           *
     H*                fact that policy year exists for the policy    *
     H*                or not.                                        *
     H*                                                               *
     H* 06/07/10 C:65  PROJECT: 139WNQ/Support                        *
     H*                Fix the conversion to rpg4 that was done by    *
     H*                this project.  It broke the renaming of the    *
     H*                employer master fields.  I prefixed the entire *
     H*                record format instead.                         *
     H*                                                               *
     H*  03/01/10  C:246  PROJECT 139WNQ - PRISS 10                   *
     H*                   Fix Positon Cursor issue for Tertiary Agent.*
     H*                                                               *
     H* 01/22/10 C:246 PROJECT: 139WNQ                                *
     H*                ADD TERTIARY AGENT IN THE AGENT HISTORY SCREEN.*
     H*                                                               *
     H* 05/22/09 C:062 PROJECT: 133WGW                                *
     H*                ADD CONTROL AGENT FLAG PROCESSING.             *
     H*                                                               *
     H* 05/20/08 C:136 WA132 63574                                    *
     H*                DEFAULT EFF DT BEING OVERWRITTEN WITH ZEROS    *
     H*                IN ADD MODE                                    *
     H*                                                               *
     H* 04/18/08 C:61  WC1537 PRISS #6                                *
     H*                Show policy year in header when called from    *
     H*                W0051R->W0553R->W0553R02 in ADD mode.          *
     H*                                                               *
     H* 04/02/08 C:136 WA132 60309                                    *
     H*                EFF DT NOT DISPLAYING WHEN CALLED FROM POLICY  *
     H*                YEARS. TURNED ON *INLR TO MAKE SURE PROGRAM    *
     H*                FIELDS CLEAR EACH TIME THE PROGRAM IS CALLED.  *
     H*                                                               *
     H* 12/31/07 C:136 WA132 60309                                    *
     H*                - USE SYSTEM DATE OR POLICY EFF DATE FOR AGENT *
     H*                HISTORY EFF DT, WHICHEVER IS LESS, WHEN        *
     H*                ADDING A RECORD ON INITIAL POLICY YEAR ENTRY.  *
     H*                IF AN AGENT HISTORY RECORD ALREADY EXISTS,     *
     H*                USE THE SYSTEM DATE AS THE DEFAULT EFF DATE.   *
     H*                - DO NOT ALLOW INFO ON RECORD TO BE CHANGED    *
     H*                IF COMMISSION EXISTS FOR AGENT RECORD IN A     *
     H*                PRIOR MONTH                                    *
     H*                - CHANGE THE EDIT THAT PREVENTS PRIMARY        *
     H*                AGENT AND PRODUCER FROM BEING THE SECONDARY ON *
     H*                ANOTHER RECORD TO A SOFT EDIT SO THAT THE      *
     H*                USER CAN REPLACE RECORDS BY ADDING A GOOD ONE  *
     H*                AND INACTIVIATING THE BAD ONE.                 *
     H*                                                               *
     H* 11/30/07 C:33  Project: WC1478  Priss# 15                     *
     H*               -Changes from 07/05/07 causes incorrect agent   *
     H*                 history records to be displayed.  Change code *
     H*                 to follow intended logic needed for WC1061.   *
     H*                                                               *
     H* 09/28/07 C:65  PROJECT: SUPP# 60309                           *
     H*                PERFORM A RECORD EXCPT TO REMOVE THE LOCK ON   *
     H*                WAQTP IN THE EMPAGT SUBROUTINE, WHEN FILE IS   *
     H*                NOT BEING UPDATED. DO NOT LOCK THE RECORD READ *
     H*                IN PGMSTR SUBROUTINE EITHER.                   *
     H*                                                               *
     H* 07/05/07 C:132 Project: WC1423                                *
     H*              - Revise Edits after QA testing for V7R1 CD      *
     H*              - If ADD, check existing records for same agent/ *
     H*                prod as a secondary agent/prod.                *
     H*              - If secondary producer changes, chedk to see if *
     H*                any existing Commission records for producter. *
     H*              - If secondary agent is cleared, & no commission *
     H*                records exist, allow it to be cleared.         *
     H*                                                               *
     H* 01/15/07 C:132 Project: WC1423                                *
     H*              - Revise Edits after QA testing                  *
     H*              - CHG Maintenance fields at bottom of screen 01  *
     H*                in display/change mode, not ADD                *
     H*
     H* 04/26/07 C:189 SUPPORT WA136 53647                            *
     H*                Divisons that exist in Quote do not have a     *
     H*                correct Quote type assigned for new Div        *
     H*                                                               *
     H* 04/12/07 C:185 Project: 139WCE                                *
     H*                Commenting out IMPLEMENT "SPLIT" PLAN.         *
     H*                Deleted following Compile time Array entries   *
     H*                SSPLIT RATE       03/02/06                     *
     H*                FUTURE USE ***    03/02/06                     *
     H*                FUTURE USE ***    03/02/06                     *
     H*                FUTURE USE ***    03/02/06                     *
     H*                FUTURE USE ***    03/02/06                     *
     H*                FUTURE USE ***    03/02/06                     *
     H*                FUTURE USE ***    03/02/06                     *
     H*                                                               *
     H* 03/06/06 C:168 PROJECT: WC1416                                *
     H*                Defaults the Activity type to 'F' if in quote  *
     H*                and the division was added through QUOTE,      *
     H*                EMRQFL='Y' and EMRDFL='Y'                      *
     H*
     H* 02/06/07 C:180 SUPPORT WA129 53118                            *
     H*                Populate field WAQTP/QTWKDT when WMAHP record
     H*                status changes.
     H*                                                               *
     H* 11/28/06 C:132 Project: WC1423                                *
     H*              - ADD Edits against Secondary Agent & Producer   *
     H*              - ADD Maintenance fields to bottom of screen 01  *
     H*                in display/change mode, not ADD                *
     H*
     H* 11/07/06 C:61  SUPPORT WA129 53118                            *
     H*                If WMAHP record status change results in       *
     H*                change of current agent, then write transfer   *
     H*                records to Quote Status WAQTP file.            *
     H*                                                               *
     H* 07/10/06 C:33  Seq#: WA099/53369                              *
     H*                Do not change from 'Pending' for 'Renew to     *
     H*                 Quote' employers.                             *
     H*                                                               *
     H* 03/02/06 C:33  Project: 139WCE                                *
     H*                Split commissions. {CLT139 only}               *
     H*                                                               *
     H* 02/23/04 C:33  Support: WA121/41255                           *
     H*                Get the latest Quote Tracking record for the   *
     H*                 latest agent.                                 *
     H*                                                               *
     H* 06/21/02 C:132 PROJECT: WA081  SUPPORT: #32382                *
     H*                If change the status on existing agent history *
     H*                record to "I", then do F6 to add a new record, *
     H*                would put the "I" in new record. Changed to    *
     H*                default to an "A" on add new record            *
     H*                                                               *
     H*                                                               *
     H* 05/17/02 C:61  SUPPORT WA100 32553 WC1068                     *
     H*                Chg Agt Producer licensed for employer rating  *
     H*                state edit.  If security level/type for edit   *
     H*                not entered, then do not invoke edit.          *
     H*                                                               *
     H* 04/30/02 C:132 WC1071                                         *
     H*                Fix for Producer Cancellation Date < = the     *
     H*                screen date - unequal fields.                  *
     H*                                                               *
     H* 03/07/02 C:132 WC1071                                         *
     H*          C:61  Check for Producer Cancellation Date < = the   *
     H*                Agent History Effective Date.                  *
     H*                                                               *
     H* 01/14/02 C:61  PROJECT WC1068                                 *
     H*                Add Agt Producer licensed for employer rating  *
     H*                state edit.                                    *
     H*                                                               *
     H* 11/12/01 C:142 WC1061                                         *
     H*                Preload effective date on screen as            *
     H*                appropriate.                                   *
     H*                                                               *
     H* 10/26/01 C:61  SUPPORT WA107 29472                            *
     H*                Update maintenance date and time fields        *
     H*                correctly.                                     *
     H*                                                               *
     H*  10/17/01 C:62 PROJECT: WC1057                                *
     H*                CHANGE EDIT, THAT CHECKS TO SEE IF AGENT IS    *
     H*                EFFECTIVE AND GIVES AN INFORMATIONAL MESSAGE   *
     H*                THAT CAN BE OVERRIDEN, SO AN AGENT CAN'T BE    *
     H*                ENTERED IF NOT EFFECTIVE.                      *
     H*                                                               *
     H*  12/04/00 C:33 PROJECT: WC825                                 *
     H*                LOAD VALUES TO SCEMP# & SCDIV.                 *
     H*                                                               *
     H* 12/01/00 C:33  PROJECT: WC1019                                *
     H*                IF THE RATE CHANGE CAUSES A CHANGE IN PLAN TYPE*
     H*                ALLOW FOR OVERRIDE.                            *
     H*                                                               *
     H* 11/01/00 C:33  PROJECT: WC1008                                *
     H*                WHEN TRANSFERRING, IF LAST STATUS IS 'C' OR 'R'*
     H*                LOAD TO QTXFER                                 *
     H*                                                               *
     H*  08/10/00 C:33 SEQ#: WA109/23913                              *
     H*                CHECK FOR INACTIVE RATE CODES.                 *
     H*                                                               *
     H* 07/13/00 C:33  SEQ#: WA102/23358                              *
     H*                CORRECTLY LOAD TRANSFER RECORDS.               *
     H*                                                               *
     H* 10/19/99 C:33  PROJECT WC948                                  *
     H*                MOVE AGENT OF RECORD TO SCR02 AND ADD SECONDARY*
     H*                AGENT OF RECORD.                               *
     H*                                                               *
     H* 10/13/99 C:61  PROJECT WC948                                  *
     H*                Add ability to designate 'Agent of record'     *
     H*                 - rate and producer number                    *
     H*                                                               *
     H* 09/20/99 C:61  PROJECT WC948                                  *
     H*                Add ability to designate 'Agent of record'.    *
     H*                                                               *
     H*  07/19/99  C:33  PROJECT: WC899                               *
     H*                  ADD PLAN TYPE & DESCRIPTION TO SCREEN.       *
     H*                                                               *
     H*  06/22/99 C:61 PROJECT 48WQJ                                  *
     H*              - Add previous fund number to header info.       *
     H*                                                               *
     H*  02/24/99  C:33  PROJECT: WC577                               *
     H*                  CORRECT 2ND AGENT EDITING.                   *
     H*                                                               *
     H*  02/23/99  C:33  PROJECT: 48WPC                               *
     H*                  GIVE A WARNING MSG TO USER INSTEAD OF AN     *
     H*                  ERROR WHEN THE THE EFFECTIVE DATE ENTERED IS *
     H*                  OUTSIDE OF THE DATE RANGE OF THE AGENT.      *
     H*                                                               *
     H*  01/28/99  C:33  SEQ#: WA033/15777                            *
     H*                  IF THERE IS A WABLP RECORD FOR THE EMPLOYER  *
     H*                  UPDATE IT WITH THE LATEST AGENT #.           *
     H*                                                               *
     H*  08/10/98 C:61 Project: 48WLC                                 *
     H*               Display old Emp#/Div from Employer Cross        *
     H*               Reference (WZEXP).                              *
     H*                                                               *
     H*  08/07/98 C:78 SEQ#: DR095/12876                              *
     H*                LOAD VALUE 'Q' OR BLANKS IN QCQTE(DARTS)       *
     H*                DEPENDING IF IT IS IN QUOTE MODE OR NOT.       *
     H*                                                               *
     H*  07/10/98  C:33  SEQ#: WA091/12598                            *
     H*                  AVOID WRITING MULTIPLE RECORDS TO AGENT QUOTE*
     H*                  TRACKING FILE.                               *
     H*                                                               *
     H*  06/07/98  C:33  PROJECT: WC577                               *
     H*                  THE AGENT MUST BE EFFECTIVE FOR THE DATE IN  *
     H*                  WHICH IT IS ASSIGNED TO THE EMPLOYER.        *
     H*                                                               *
     H*  04/10/98  C:33  PROJECT: WC395                               *
     H*                  ALLOW FOR TRANSFER OF AGENT IN QUOTE TRACKING*
     H*                                                               *
     H*  04/03/98  C:78  PROJECT: WC825                               *
     H*                  ALLOW ACCESS TO DARTS. F14 ADDED.            *
     H*                                                               *
     H*  02/19/98  C:33  PROJECT: WC577                               *
     H*                  CODE FROM 01/21/98 NEEDED TO BE CHANGED FROM *
     H*                  NE TO EQ.                                    *
     H*                                                               *
     H*  02/18/98  C:33  PROJECT: WC577                               *
     H*                  DO NOT PRELOAD THE QUOTE STATUS RECORD IN    *
     H*                  INQUIRY MODE.                                *
     H*                                                               *
     H*  01/21/98  C:33  PROJECT: WC577                               *
     H*                  DO NOT USE DEFAULT RATE IF RATE IS ENTERED.  *
     H*                                                               *
     H*  12/15/97  C:33  PROJECT: WC577                               *
     H*                 -GET DEFAULT AGENT RATE FROM WMAAP            *
     H*                 -USE NEW AGENT PRODUCER SEARCHES              *
     H*                                                               *
     H*  12/11/97  C:33  PROJECT: WC577                               *
     H*                  ONLY ADD ONE RECORD AND RETURN TO WW SCREEN  *
     H*                                                               *
     H*  11/04/97  C:33  PROJECT: WC577                               *
     H*                 -NEW CALL PARMS FOR W4150R                    *
     H*                 -LOAD NEW FIELDS FOR WAQTP                    *
     H*                                                               *
     H*  PLEASE ADD TO TOP OF REVISIONS                               *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H*  40 - ADD MODE                                                *
     H* N40 - UPDATE MODE                                             *
     H*  41 - QUOTE MODE                                              *
     H* N41 - LIVE MODE                                               *
     H*  42 - CALLED FROM EMPLOYER DETAIL                             *
     H* N42 - CALLED FROM EMPLOYER MASTER                             *
     H*  50 - DISPLAY PRIMARY NEW & RENEW RATES                       *
     H*  51 - DISPLAY SECONDARY NEW & RENEW RATES                     *
     H*  52 - DISPLAY 'AGT OF RECORD' NEW & RENEW RATES               *
     H*  63 - Display old Emp#/Div in header info                     *
     H*  65 - SECURITY LEVEL VALID                                    *
     H* N65 - SECURITY LEVEL INVALID                                  *
     H*  90 - GENERAL ERROR INDICATOR                                 *
     H*  91 - GENERAL POSITION CURSOR                                 *
     H*  92 - GENERAL F KEY PRESSED                                   *
     H*  93 - SCREEN WAS UPDATE WHILE USER WAS IN IT                  *
     H*                                                               *
     H*  SCR01:                                                       *
     H*  ERROR:                                                       *
     H*  20 - (WCR0517) Year invalid for fund {ADD mode only}         *
     H*  21 - (WCR0500) Effective date invalid                        *
     H*  22 - (WCR0530) Agent #/effective date not valid              *
     H*  23 - (WC30005) Record cannot be used more than once          *
     H*  24 - (WCR0532) Invalid rate #                                *
     H*  25 - (WCR0531) Producer invalid                              *
     H*  26 - (WCR0530) Agent #/effective date not valid              *
     H*  27 - (WCR0532) Invalid rate #                                *
     H*  28 - (WCR0531) Producer invalid                              *
     H*  29 - (WCR0530) Invalid agent of record                       *
     H*  30 - (WCR0532) Invalid rate # - agent of record              *
     H*  32 - (WCR0531) Producer invalid - agent of record            *
     H*  36 - (WCR3157) Producer Cancel Date <= Agt Hsty Eff Date     *
     H*  37 - (WCR3157) Producer Cancel Date <= Agt Hsty Eff Date     *
     H*  87 - (WC30172) Agent effective date range not valid for date *
     H*                 entered                                       *
     H*  88 - (WC30172) Agent effective date range not valid for date *
     H*                 entered                                       *
     H*  89 - (WC30172) Agent effective date range not valid for date *
     H*                 entered                                       *
     H*                                                               *
     H*  POSITION CURSOR:                                             *
     H*  71 - (SCAGT#) Primary agent                                  *
     H*  72 - (SCRT#)  Primary rate #                                 *
     H*  73 - (SCPROD) Primary producer                               *
     H*  74 - (SCAGT2) Secondary agent                                *
     H*  75 - (SCRT#2) Secondary rate #                               *
     H*  76 - (SCPRD2) Secondary producer                             *
     H*  78 - (SCAGT2) agent #  - agent of record                     *
     H*  79 - (SCRT#2) rate #   - agent of record                     *
     H*  80 - (SCPRD2) producer - agent of record                     *
     H*                                                               *
     H*  SCR02:                                                       *
     H*  ERROR:                                                       *
     H*  36 - (WCR3157) Producer Cancel Date <= Agt Hsty Eff Date     *
     H*  37 - (WCR3157) Producer Cancel Date <= Agt Hsty Eff Date     *
     H*  POSITION CURSOR:                                             *
     H*                                                               *
     H*  SCR03:                                                       *
     H*  ERROR:                                                       *
     H*  POSITION CURSOR:                                             *
     H*                                                               *
     H*****************************************************************
      *
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FW0553F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWMAHP     UF A E             DISK
     F                                     RENAME(PAH01:DBRCDF)
     FWMAHL     IF   E           K DISK
     F                                     INFDS(AHFDS)
      *
     FWMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PEM02)
      *
     FWMEMLQ    IF   E           K DISK
     F                                     RENAME(PEM01:PEM03)
     f*** begin add ***  06/07/10  ************************************************
     f                                     prefix(E03)
     f*** begin add ***  06/07/10  ************************************************
      *
     FWDELP     IF   E             DISK
     FWDELL     IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
      *
      * Producer State Info
     FWMPSL     IF   E           K DISK
      *
      * Rate Set Code
     FWMRSL     IF   E           K DISK
      *
     FWMEML     IF   E           K DISK
      *
      *  Employer Cross Reference
     FWZEXL     IF   E           K DISK
     F                                     RENAME(PEX01:PEX02)
      *
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS1)
      *
     FWDF1L     IF   E           K DISK
      *
     FWMAAL1    IF   E           K DISK
     F                                     INFDS(DBFDSA)
      *
     FWMAAP     IF   E             DISK
     F                                     RENAME(PAA01:PAA02)
      *
     FWMAGL     IF   E           K DISK
     F                                     INFDS(DBFDSM)
      *
     FWMARL     IF   E           K DISK
     FWMARP     IF   E             DISK
     F                                     RENAME(PAR01:PAR02)
      *
     FWMAPL     IF   E           K DISK
     FWMAPP     IF   E             DISK
     F                                     RENAME(PAP01:PAP02)
      *
     FWMEXL     IF   E           K DISK
     FWAQTL7    UF A E           K DISK
     FWABLL     UF   E           K DISK
     FWMEAL     IF   E           K DISK
     FSTCTL1    IF   E           K DISK
     FSSYLF     IF   E           K DISK
     F*
      * Agent Commission File
     FWMACL     IF   E           K DISK
     D PL              S              1    DIM(3) CTDATA PERRCD(1)              Plan types, desc
     D PLD             S             40    DIM(3) ALT(PL)
     D CHG             S              1    DIM(2)
     D MSG             S             60    DIM(30) CTDATA PERRCD(1)             INFORMATION EDITS
     D*
     D DBFDSA          DS
     D  DBRRNA               397    400B 0
     D DBFDSM          DS
     D  DBRRNM               397    400B 0
     D DBFDS1          DS
     D  DBRRN1               397    400B 0
     D AHFDS           DS
     D  AHRRN                397    400B 0
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D  QCQTE                 40     40
     D*
     D                 DS
     D  SVPL                   1      2
     D                                     DIM(2)
     D  SVPLAN                 1      1
     D  SVPLN2                 2      2
     D                 DS
     D  X8EFDT                 1      8  0
     D  X8DAY                  3      4  0
     D                 DS
     D  ACPODT                 1      7  0
     D  XYYYMM                 1      5  0
     D                 DS
     D  SYSDT                  1      7  0
     D  SYYYMM                 1      5  0
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*
     D OPTNM           C                   CONST('Agent History Inform-
     D                                     ation')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(2)
     D CHGAPI          C                   CONST('SA020R')
     D ACCDIR          C                   CONST('Account Current to -
     D                                     Direct Bill.')
     D DIRACC          C                   CONST('Direct Bill to -
     D                                     Account Current.')
     D PRIM            C                   CONST('PRIMARY AGENT:')
     D SEC             C                   CONST('SECONDARY AGENT:')
     d*** begin del ***  06/07/10  ************************************************
     D*****DPEM03         E DS                  EXTNAME(WMEMP)
     D*****EMSTR1         E                     EXTFLD(EMSTRP)
     D*****EMDIV1         E                     EXTFLD(EMDIV)
     D*****EMRQF1         E                     EXTFLD(EMRQFL)
     d*** end del *****  06/07/10  ************************************************
      *BEG ADD 01/22/10 ********************************************
     *================================================================
     * Stand Alone Fields - TOP
     *================================================================
     D SAMEAG          S              1    inz('0')
     D NOCHK           S              1    inz('0')
     C*** Copy Book
     C/COPY SCOPYMT01C
     C*END ADD 01/22/10 ********************************************
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
     C                   PARM                    @@QTE             1
     C                   PARM                    @@CALL            1
     C*
     C*
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C*
     C*  NOTE:  IF @@CALL IS PASSED (AND 'Y') @@RRN2 MUST BE THE
     C*         RELETIVE RECORD # OF WDELP OTHERWISE @@RRN2 SHOULD BE
     C*         WMEMP
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##LVL             3 0
     C                   PARM                    ##TYPE            1
     C*
     C     LVLPL         PLIST
     C                   PARM                    ##UTYP            1            User Type
     C                   PARM                    ##ULVL            3            User Level
     C                   PARM                    ##CTYP            1            Client Type
     C                   PARM                    ##CLVL            3            Client Level
     C                   PARM                    ##EXEC            1            Execution Type
     C                   PARM                    ##RES             1            Result Flag
     C*
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*  EMPLOYER MASTER
     C*
     C     EMKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C     EMKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C*
     C     QTKY4         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    XXDIV1
     C                   KFLD                    XXSTT1
     C                   KFLD                    ##AGT#
     C                   KFLD                    XXDATE
     C*
     C     QTKY3         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    XXDIV1
     C                   KFLD                    XXSTT1
     C                   KFLD                    ##AGT#
     C*
      *
      *  Employer Detail
      *
     C     ELKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    SCFYR
      *
      * Rating State
      *
     C     RSKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
      *
      *
      *  Employer Cross Reference
      *
     C     ZEXKY         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C*
     C*  GROUP MASTER
     C*
     C     FNKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C*
     C*  CLAIMS/DARTS CONTROL FILE
     C*
     C     EXKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C*
     C*  KEY LIST FOR THE FUND DETAIL
     C*
     C     F1KY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    SCFYR
     C*
     C*  AGENT HISTORY FILE
     C*
     C     AHKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    SCFYR
     C                   KFLD                    XXEFDT
     C                   KFLD                    SCAGT#
      *
     C     AHKY1         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    SCFYR
     C     AHKY2         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    SCFYR
     C                   KFLD                    XXEFDT
     C*
     C*  AGENT ASSIGNMENT FILE
     C*
     C     AAKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    XXAGT#
     C*
     C*  KEY FOR AGENTS MASTER
     C*
     C     AGKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C*
     C*  AGENT PRODUCER FILE
     C*
     C     APKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C                   KFLD                    XXPROD
      *
      *  Producer State Info
      *
     C     PSKY          KLIST
     C                   KFLD                    APCO#
     C                   KFLD                    APFEIN
     C                   KFLD                    APMOD
     C                   KFLD                    APPROD
      *
     C*
     C*  AGENT RATE FILE
     C*
     C     ARKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    XXRT#
     C                   KFLD                    XXEFDT
     C*
     C     ARKY1         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    XXRT#
     C*
     C*  AGENT QUOTE FILE
     C*
     C     QTKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    XXSTAT
     C*
     C     QTKY1         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    XXSTAT
     C                   KFLD                    SCAGT#
     C*
     C     QTKY2         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    XXSTAT
     C                   KFLD                    XXAGT1
     C*
     C*  BILL TRANSACTION FILE
     C*
     C     BLKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    SVFYR
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C     EAKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    SCFYR
     C*
     C     EAKY1         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    SCFYR
     C                   KFLD                    X7EFDT
     C*
     C*  Agent Commission file
     C     ACKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    SCFYR
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    SCAGT#
     C                   KFLD                    SVPROD
     C*
     C     ACKY1         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    SCFYR
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    SVAGT2
     C                   KFLD                    SVPRD2
     C*
     C     ACKY2         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    SCFYR
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    SVAGT2
     C                   KFLD                    SVPRD2
     C     ACKY3         KLIST
     C                   KFLD                    AHCO#
     C                   KFLD                    AHFND
     C                   KFLD                    AHFYR
     C                   KFLD                    AHEMP#
     C                   KFLD                    AHDIV
     C*
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    AHAGT#        XXAGT#
     C     *LIKE         DEFINE    AHEFDT        XXEFDT
     C     *LIKE         DEFINE    AHEFDT        ##SCDT
     C     *LIKE         DEFINE    APPROD        XXPROD
     C     *LIKE         DEFINE    AHRT#         XXRT#
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    SCAGT#        ##AGT#
     C     *LIKE         DEFINE    SCRT#         ##RT#
     C     *LIKE         DEFINE    SCEFDT        ##EFDT
     C     *LIKE         DEFINE    SCEFDT        SVEFDT
     C     *LIKE         DEFINE    SCEFDT        SVEFD2
     C     *LIKE         DEFINE    SCEFDT        SVEFD3
     C     *LIKE         DEFINE    SCEFDT        SVEFD4
     C     *LIKE         DEFINE    SCPROD        ##PROD
     C     *LIKE         DEFINE    AGNAME        XANAME
     C     *LIKE         DEFINE    ARNWRT        XXNWRT
     C     *LIKE         DEFINE    ARRNRT        XXRNRT
     C     *LIKE         DEFINE    SCAGT#        SVAGT#
     C     *LIKE         DEFINE    SCAGT2        SVAGT2
     C     *LIKE         DEFINE    SCAGT2        SVAGT3
     C     *LIKE         DEFINE    AHPROD        SVPROD
     C     *LIKE         DEFINE    AHSPRD        SVPRD2
     C     *LIKE         DEFINE    AH3PRD        SVPRD3
     C     *LIKE         DEFINE    SCAGRC        SVAGRC
     C     *LIKE         DEFINE    SC2ARC        SV2ARC
     C     *LIKE         DEFINE    SC3ARC        SV3ARC
     C     *LIKE         DEFINE    Q1PGM         XXPGM
     C     *LIKE         DEFINE    QTDT          X7DATE
     C     *LIKE         DEFINE    QTSTAT        XXSTAT
     C     *LIKE         DEFINE    ARPLAN        XXPLAN
     C     *LIKE         DEFINE    ARPLAN        SVPLN3
     C     *LIKE         DEFINE    SCPLDS        XXPLDS
     C     *LIKE         DEFINE    SCFYR         SVFYR
     C     *LIKE         DEFINE    SCFYR         ##FYR
     C     *LIKE         DEFINE    SCAGT#        XXAGT1
     C     *LIKE         DEFINE    SCAGT#        XXAGT2
     C     *LIKE         DEFINE    AHEFDT        X7EFDT
     C     *LIKE         DEFINE    ARPLAN        OLDPL
     C*
     C     *LIKE         DEFINE    EMDIV         XXDIV1
     C     *LIKE         DEFINE    QTTYPE        XXQTYP
     C     *LIKE         DEFINE    QTSTAT        XXSTT1
     C*================================================================
     C* AGTNM - Get agent name
     C*================================================================
     C*
     C     AGTNM         BEGSR
     C*
     C                   MOVEL     *BLANKS       XANAME
     C     AAKY          CHAIN     PAA01                              60
     C  N60AGKY          CHAIN     PAG01                              60
     C  N60              MOVEL     AGNAME        XANAME
     C*
     C                   ENDSR                                                  AGTNM
     C*
     C*================================================================
     C* BILLNG - Update the Bill Transaction File
     C*================================================================
     C*
     C     BILLNG        BEGSR
     C*
     C     BLKY          CHAIN     PBL01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   CALL      'WA002R'                             99
     C                   PARM                    EMCO#
     C                   PARM                    EMFND
     C                   PARM                    EMEMP#
     C                   PARM                    EMDIV
     C                   PARM                    SVFYR
     C                   PARM                    ##AGT#
     C                   PARM                    ##ANAM           60
     C*
     C                   Z-ADD     ##AGT#        BLAGT#
     C                   MOVEL     Q1USER        BLUSER
     C                   MOVEL     Q1PGM         BLPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BLDT
     C*
     C                   MOVEL     DSTIME        BLTIME
     C*
     C                   UPDATE    PBL01
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  BILLNG
     C*
     C*===============================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*===============================================================
     C*
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             60
     C*
     C     *IN60         IFEQ      *OFF
      *
      *  Save Current Agent # before change in status
      *
     C                   EXSR      GETAGT
     C                   Z-ADD     ##AGT#        XXAGT1
     C*
     C     AHSTAT        IFEQ      'I'
     C                   MOVE      'A'           AHSTAT
     C                   ELSE
     C                   MOVE      'I'           AHSTAT
     C                   ENDIF                                                  AHSTAT=I
     C*
     C                   MOVEL     Q1USER        AHUSER
     C                   MOVEL     Q1PGM         AHPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AHDT
     C*
     C                   MOVEL     DSTIME        AHTIME
     C*
     C                   UPDATE    DBRCDF
     C*
      *
      *  Update Quote Status to account for change of current agent
      *  (if necessary)
      *
     C                   EXSR      UPDSTS
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  CHGSTS
      *
      ***** BEG ADD ******* 11/07/06 **********************************
      *
      *===============================================================
      * UPDSTS - Update Quote Status records (if necessary)
      *===============================================================
     C     UPDSTS        BEGSR
     C                   EXSR      GETAGT
      *
      * Write Quote Status transfer records if change in current agent
      * as a result of change in status of agent history record.
      *
     C     XXAGT1        IFNE      0
     C     ##AGT#        ANDNE     0
     C     XXAGT1        ANDNE     ##AGT#
     C     QTKY2         SETGT     PQT01
     C     QTKY2         READPE(N) PQT01                                  60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     QTTYPE        IFNE      'X'
     C     QTTYPE        ANDNE     'T'
     C                   MOVE(P)   QTTYPE        QTXFER
     C                   ENDIF                                                  QTTYPE
      *
     C                   MOVE(P)   'X'           QTTYPE
     C                   EXSR      WRITEQ
     C                   MOVE(P)   'T'           QTTYPE
     C                   Z-ADD     ##AGT#        QTAGT#
     C                   EXSR      WRITEQ
     C                   ENDIF                                                  QTKY2
      *
     C                   ENDIF                                                  X1AGT#
      *
     C                   ENDSR                                                  UPDSTS
      *
     C*
     C*================================================================
     C* CHKAR - Check for A/R in a prior month for year
     C*================================================================
     C*
     C     CHKAR         BEGSR
     C*
     C                   MOVE      *OFF          PRIOR             1
     C*
     C                   Z-ADD     SCEFDT        X8EFDT
     C                   Z-ADD     1             X8DAY
     C                   Z-ADD     X8EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7EFDT
     C*
     C     EAKY1         SETGT     WMEAL
     C     EAKY          READPE    WMEAL                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C     EATRDT        ANDEQ     X7EFDT
     C     EAKY          READPE    WMEAL                                  60
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           PRIOR
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  CHKAR
     C*
     C*================================================================
     C* CHKLST - Check for last agent history record for year
     C*================================================================
     C*
     C     CHKLST        BEGSR
     C*
     C                   MOVE      *OFF          OLDPLN            1
     C*
     C     AHKY1         SETGT     PAH01
     C     AHKY1         READPE    PAH01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C     AHRRN         ANDNE     @@RRN
     C*
     C                   SELECT
     C     P             WHENEQ    1
     C     AHRT#         ANDNE     *ZERO
     C                   Z-ADD     AHRT#         XXRT#
     C                   EXSR      RATES
     C                   MOVE      *ON           OLDPLN
     C     P             WHENEQ    2
     C     AHSRT#        ANDNE     *ZERO
     C                   Z-ADD     AHSRT#        XXRT#
     C                   EXSR      RATES
     C                   MOVE      *ON           OLDPLN
     C                   ENDSL
     C*
     C                   MOVEL(P)  XXPLAN        OLDPL
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  CHKLST
     C*
     C*================================================================
     C* CHKLVL - Check levls
     C*================================================================
     C*
     C     CHKLVL        BEGSR
     C*
     C                   MOVE      ##TYPE        ##UTYP
     C                   MOVE      ##LVL         ##ULVL
     C*_______________________________________________________________
     C*
     C*  OVERRIDE PLAN CHANGE
     C*
     C                   MOVE      EXPLNT        ##CTYP
     C                   MOVE      EXPLNL        ##CLVL
     C                   MOVE      '1'           ##EXEC
     C*
     C                   CALL      'SRCCSC'      LVLPL                  99
     C*
     C                   SELECT
     C     ##RES         WHENEQ    *OFF
     C                   MOVE      *ON           PLNOVR            1
     C                   OTHER
     C                   MOVE      *OFF          PLNOVR            1
     C                   ENDSL
     C*_______________________________________________________________
     C*
     C                   ENDSR                                                  CHKLVL
     C*
     C*================================================================
     C* CHKPLN - Check for plan changes
     C*================================================================
     C*
     C     CHKPLN        BEGSR
     C*
     C   40              EXSR      CHKLST
     C*
     C     ARPLAN        IFNE      SVPL(P)
     C     *IN40         ANDEQ     *OFF
     C     ARPLAN        ORNE      OLDPL
     C     OLDPLN        ANDEQ     *ON
     C*
     C                   EXSR      CHKAR
     C*
     C     PRIOR         IFEQ      *ON
     C*
     C                   SELECT
     C     ARPLAN        WHENEQ    'A'
     C     SVPL(P)       ANDEQ     *BLANK
     C                   MOVEL(P)  DIRACC        SCCHG
     C     ARPLAN        WHENEQ    *BLANK
     C     SVPL(P)       ANDEQ     'A'
     C                   MOVEL(P)  ACCDIR        SCCHG
     C                   OTHER
     C                   CLEAR                   SCCHG
     C                   ENDSL
     C*
     C     SCCHG         IFNE      *BLANKS
     C*
     C                   MOVEL(P)  'N'           SCOVR
     C*
     C     PLNOVR        IFEQ      *ON
     C                   EXFMT     PLANWDW
     C                   ENDIF                                                  PLNOVR=*ON
     C*
     C                   SELECT
     C     SCOVR         WHENEQ    'Y'
     C                   MOVEL(P)  ARPLAN        SVPL(P)
     C                   MOVE      *ON           CHG(P)
     C     SCOVR         WHENEQ    'N'
     C     33            ADD       P             IN                3 0
     C                   MOVE      *ON           *IN(IN)
     C                   SETON                                        33  90
     C                   ENDSL
     C*
     C                   ENDIF                                                  SCCHG<>*BLANKS
     C*
     C                   ENDIF                                                  PRIOR=*ON
     C*
     C                   ENDIF                                                  ARPLAN<>SVPL,P
     C*
     C                   ENDSR                                                  CHKPLN
     C*================================================================
     C* DARTS - Write a DARTS remark
     C*================================================================
     C*
     C     DARTS         BEGSR
     C*
     C     CHG(P)        IFEQ      *ON
     C*
     C     EXPLDR        CHAIN     PCT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   CLEAR                   QCKEY
     C*
     C                   MOVE      ##CO#         QCCO#
     C                   MOVE      SCFND         QCFND
     C                   MOVE      SCEMP#        QCEMP#
     C                   MOVE      SCDIV         QCDIV
     C   41              MOVE      @@QTE         QCQTE
     C  N41              MOVE      *BLANKS       QCQTE
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         ##CDAT
     C*_______________________________________________________________
     C*
     C                   MOVEL(P)  CTDESC        ##COMM
     C*
     C                   CAT       'YEAR:':5     ##COMM
     C*
     C                   MOVEL(P)  SCFYR         XXNUM
     C                   MOVEL(P)  *BLANKS       EDTMSK
     C                   CALL      CHGAPI        CHGPL                  99
     C                   CAT       XXNUM:1       ##COMM
     C*
     C                   CAT       'EFF DA':2    ##COMM
     C                   CAT       'TE:':0       ##COMM
     C*
     C                   MOVEL(P)  SCEFDT        XXNUM
     C                   MOVEL(P)  'DATE'        EDTMSK
     C                   CALL      CHGAPI        CHGPL                  99
     C                   CAT       XXNUM:1       ##COMM
     C*
     C                   SELECT
     C     P             WHENEQ    1
     C                   CAT       PRIM:2        ##COMM
     C                   MOVEL(P)  SCAGT#        XXNUM
     C     P             WHENEQ    2
     C                   CAT       SEC:2         ##COMM
     C                   MOVEL(P)  SCAGT2        XXNUM
     C                   ENDSL
     C*
     C                   MOVEL(P)  *BLANKS       EDTMSK
     C                   CALL      CHGAPI        CHGPL                  99
     C                   CAT       XXNUM:1       ##COMM
     C*
     C                   CAT       'USER:':5     ##COMM
     C                   CAT       Q1USER:1      ##COMM
     C                   CAT       'DATE:':2     ##COMM
     C*
     C                   MOVEL(P)  DSDATE        XXNUM
     C                   MOVEL(P)  'DATE'        EDTMSK
     C                   CALL      CHGAPI        CHGPL                  99
     C                   CAT       XXNUM:1       ##COMM
     C*_______________________________________________________________
     C*
     C                   CALL      'DA700R'
     C                   PARM      QCKEY         ##KEY            40
     C                   PARM                    ##COMM          256
     C                   PARM                    ##CDAT            8
     C                   PARM      *BLANKS       ##DCID           12
     C                   PARM      'W0050R'      ##PNAM           10
     C                   PARM      *ZEROS        ##SECL            3
     C                   PARM      Q1USER        ##USER           10
     C                   PARM      Q1PGM         ##PGM            10
     C                   PARM      *BLANKS       ##FOLD           80
     C                   PARM      *BLANKS       ##DIFL            1
     C                   PARM      Q1PGM         ##IPGM           10
     C                   PARM      *BLANKS       ##FL01            1
     C                   PARM      *BLANKS       ##FL02            1
     C                   PARM      EXPLDR        ##RMCD            5
     C                   PARM      '00000'       ##SEQ#            5
     C                   PARM      'N'           ##SBYL            1
     C                   PARM                    ##COM2           19
     C                   PARM      *BLANKS       ##APID            2
     C                   PARM      *BLANKS       ##IDOC           12
     C                   PARM      *BLANKS       ##IQUE           10
     C                   PARM      *BLANKS       ##IMGF            1
     C                   PARM      *BLANKS       ##IPTH           10
     C                   PARM      *ZEROS        ##PIDX            5
     C                   PARM      *BLANKS       ##QPTY            1
     C                   PARM      *BLANKS       ##DCTP            5
     C                   PARM      *BLANKS       ##CSID            2
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  CHG,P=*ON
     C*
     C                   ENDSR
     C***  END   ADD  ***  12/01/00  *********************************
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C*
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C*
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*
     C*================================================================
     C* EMPAGT - Check to see if this emp already has an agent assigned
     C*================================================================
     C*
     C     EMPAGT        BEGSR
     C*
     C                   MOVE      'N'           OLDAGT            1
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7DATE
     C*
     C*  IF RECORDS ARE MANUALLY DELETED, IT COULD PRODUCE UNEXPECTED
     C*   RESULTS.
     C*
     C     *IN40         IFEQ      *ON
     C     QTKY2         SETGT     PQT01
     C     QTKY          READPE    PQT01                                  60
     C                   ELSE
     C     QTKY          CHAIN     PQT01                              60
     C                   ENDIF                                                  *IN40=*ON
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     @@QTE         IFEQ      'Q'
     C*
     C                   Z-ADD     SCAGT#        QTAGT#
     C*
     C                   MOVEL(P)  Q1USER        QTUSER
     C                   MOVEL(P)  Q1PGM         QTPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        QTTIME
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         QTDT
      *
     C                   UPDATE    PQT01
     C                   ELSE
     C                   EXCEPT    PQT01E
     C                   SELECT
     C     QTTYPE        WHENEQ    'C'
     C     QTTYPE        OREQ      'R'
     C                   MOVE      QTTYPE        QTXFER
     C                   OTHER
     C                   MOVE      *BLANKS       QTXFER
     C                   ENDSL
     C*
     C                   MOVE      'X'           QTTYPE
     C*
     C                   EXSR      WRITEQ
     C*
     C                   Z-ADD     SCAGT#        QTAGT#
     C                   MOVE      'T'           QTTYPE
     C*
     C                   EXSR      WRITEQ
     C*
     C                   CLEAR                   QTXFER
     C*
     C                   ENDIF                                                  @@QTE=Q
     C*
     C                   MOVE      'Y'           OLDAGT
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  EMPAGT
     C*
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C*
     C                   MOVE      *OFF          *IN32
     C                   CLEAR                   WDMSG1
     C                   CLEAR                   WDMSG2
     C                   CLEAR                   WDMSG3
     C                   CLEAR                   WDMSG4
     C                   CLEAR                   WDMSG5
      *** BEG ADD ****** 01/22/10 *************************************
     C                   MOVE      *OFF          SAMECH            1
     C     NOCHK         IFEQ      *OFF
     C** BEG DEL 03/01/10
     C**                 EXSR      SAMCHG
     C** END DEL 03/01/10
     C                   ENDIF
     C     *IN90         IFEQ      *OFF
     C     *IN40         ANDEQ     *OFF
     C     SAMECH        ANDEQ     *ON
     C                   MOVE      *ON           *IN32
     C                   MOVE      *ON           *IN90
     C     *INKG         DOWEQ     *OFF
     C     *INKL         ANDEQ     *OFF
     C     NOTDSP        ANDEQ     *OFF
     C                   EXFMT     WDWMSG
     C                   ENDDO
     C*
     C     *INKL         IFEQ      *ON
     C                   MOVE      *OFF          SAMECH
     C                   ENDIF
     C     *INKG         IFEQ      *ON
     C                   MOVE      *ON           NOCHK
     C                   ENDIF
     C                   ENDIF
     C                   MOVE      *OFF          NOTDSP            1            ONLY DISPLAY ONCE
      *** END ADD ****** 01/22/10 *************************************
     C     PRCOMM        IFEQ      'Y'
     C     *IN40         ANDEQ     *OFF
     C     SCRT#         IFNE      S1RT#
     C     SCPROD        ORNE      S1PROD
     C     SCAGT2        ORNE      S1AGT2
     C     SCRT#2        ORNE      S1RT#2
     C     SCPRD2        ORNE      S1PRD2
     C                   MOVEL(P)  MSG(7)        WDMSG1
     C                   MOVEL(P)  MSG(8)        WDMSG2
     C                   MOVEL(P)  MSG(9)        WDMSG3
     C                   MOVEL(P)  MSG(10)       WDMSG4
     C                   MOVEL(P)  MSG(11)       WDMSG5
     C                   MOVE      *ON           *IN32
     C                   MOVE      *ON           *IN90
     C                   EXFMT     WDWMSG
     C                   MOVE(P)   S1RT#         SCRT#
     C                   MOVE(P)   S1PROD        SCPROD
     C                   MOVE(P)   S1AGT2        SCAGT2
     C                   MOVE(P)   S1RT#2        SCRT#2
     C                   MOVE(P)   S1PRD2        SCPRD2
     C                   ENDIF
     C                   ENDIF
     C*
     C     *IN40         IFEQ      *ON
     C*
     C*  CHECK FOR VALID YEAR
     C*
     c*** Begin add ***  09/07/10  ************************************************
     C     ELKY          CHAIN     PEL02                              60
     C  N90*IN60         IFEQ      *ON
     C                   SETON                                        57  90
     C                   ENDIF                                                  *IN60=*ON
     C*
     c*** End add ***   09/07/10 ************************************************
     C     F1KY          CHAIN     WDF1L                              60
     C  N90*IN60         IFEQ      *ON
     C                   SETON                                        20  90
     C                   ENDIF                                                  *IN60=*ON
     C*
     C*  EDIT EFFECTIVE DATE
     C*
     C*
     C*
     C                   ENDIF                                                  *IN40=*ON
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C*  VALIDATE PRIMARY AGENT NUMBER
     C*
     C                   MOVE      SCAGT#        XXAGT#
     C*
     C     AAKY          CHAIN     PAA01                              60
     C*
     C  N90*IN60         IFEQ      *ON
     C     AASTAT        OREQ      'I'
     C                   SETON                                        22  90
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   EXSR      SAMAGT
     C     *IN90         IFEQ      *OFF
     C     SAMEAG        ANDEQ     *ON
     C     *INKG         DOWEQ     *OFF
     C     *INKL         ANDEQ     *OFF
     C     NOTDSP        ANDEQ     *OFF
     C                   EXFMT     WDWMSG
     C                   ENDDO
     C*
     C     *INKL         IFEQ      *ON
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C     *INKG         IFEQ      *ON
     C                   MOVE      *ON           NOTDSP            1            ONLY DISPLAY ONCE
     C                   ENDIF
     C                   ENDIF                                                  *IN60=*OFF
      *
      * INFORM USER THAT ANOTHER ACTIVE RECORD EXISTS WITH SAME EFF DATE
      *
     C                   Z-ADD     *ZERO         EFDCNT            1 0
     C     *IN90         IFEQ      *OFF
     C     *IN40         ANDEQ     *ON
     C     AHKY2         SETLL     PAH01
     C     AHKY2         READE     PAH01                                  60
     C     *IN60         DOWEQ     *OFF
     C     AHSTAT        IFEQ      'A'
     C                   ADD       1             EFDCNT
     C                   ENDIF
     C     EFDCNT        IFEQ      1
     C                   LEAVE
     C                   ENDIF
     C     AHKY2         READE     PAH01                                  60
     C                   ENDDO
      *
     C     EFDCNT        IFEQ      1
     C                   MOVEL(P)  MSG(5)        WDMSG1
     C                   MOVEL(P)  MSG(6)        WDMSG2
     C     *INKG         DOWEQ     *OFF
     C     *INKL         ANDEQ     *OFF
     C     NOTDSQ        ANDEQ     *OFF
     C                   EXFMT     WDWMSG
     C                   ENDDO
     C     *INKL         IFEQ      *ON
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C     *INKG         IFEQ      *ON
     C                   MOVE      *ON           NOTDSQ            1            ONLY DISPLAY ONCE
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C  N90              EXSR      AGTNM
     C  N90              MOVEL     AGNAME        SCAGNM
     C*
      * Primary and secondary agent/producer cannot be same.
     C     *IN90         IFEQ      *OFF
     C     SCAGT#        ANDEQ     SCAGT2
     C     SCPROD        ANDEQ     SCPRD2
     C                   SETON                                        43  90
     C                   ENDIF                                                  SCAGT#=SCAGT2
     C*
     C     SCAGT#        IFNE      SVAGT#
     C*
     C     AAEXDT        IFEQ      *ZEROS
     C     AAEXDT        ORGT      SYSDT
     C     SCRT#         IFEQ      *ZERO
     C                   Z-ADD     AADRT#        SCRT#
     C                   ENDIF                                                  SCRT#=*ZERO
     C*
     C                   Z-ADD     SCRT#         XXRT#
     C                   EXSR      RATES
     C                   Z-ADD     XXNWRT        SCNWRT
     C                   Z-ADD     XXRNRT        SCRNRT
     C                   MOVEL(P)  XXPLAN        SCPLAN
     C                   MOVEL(P)  XXPLDS        SCPLDS
     C*
     C                   ENDIF                                                  SYSDT<=AAEXDT=*ZEROS
     C*
     C     SCRT#         COMP      *ZERO                              5050
     C*
     C                   Z-ADD     SCAGT#        SVAGT#
     C                   SETON                                        72  90
     C*
     C                   ENDIF                                                  SCAGT#<>SVAGT#
     C*
      ***** BEG ADD ***** 11/28/06 ***************************************
      * IF Producer changes & saved producer is not zeros, check for a
      *  Commission record for this year. If so, cannot change prod #.
     C  N90SCPROD        IFNE      SVPROD
     C     SVPROD        ANDNE     *ZEROS
     C     ACKY          CHAIN     WMACL                              60
     C  N60              SETON                                        45  90
     C                   ENDIF                                                  SCPROD<>SVPROD
     C*
     C*
     C  N90AAEFDT        IFGT      XXEFDT
     C                   SETON                                        88  90
     C                   ENDIF                                                  AAEFDT>XXEFDT
     C*
     C  N90AACNDT        IFNE      *ZERO
     C     AACNDT        ANDLT     XXEFDT
     C                   SETON                                        88  90
     C                   ENDIF                                                  AAEFDT>XXEFDT
     C*
     C*
     C*
     C                   Z-ADD     SCRT#         XXRT#
     C     ARKY          SETGT     PAR01
     C     ARKY1         READPE    PAR01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C     ARSTAT        ANDEQ     'I'
     C     ARKY1         READPE    PAR01                                  60
     C                   ENDDO                                                  *IN60=*OFF
     C  N90
     CAN 60              SETON                                        24  90
     C  N90
     CANN60              Z-ADD     ARNWRT        XXNWRT
     C  N90
     CANN60              Z-ADD     ARRNRT        XXRNRT
     C                   Z-ADD     1             P                 3 0
     C  N90              EXSR      CHKPLN
     C*
     C*  EDIT PRIMARY PRODUCER NUMBER
     C*
     C                   Z-ADD     SCPROD        XXPROD
     C     APKY          CHAIN     PAP01                              60
     C  N90*IN60         IFEQ      *ON
     C     APSTAT        OREQ      'I'
     C                   SETON                                        25  90
     C                   ENDIF                                                  *IN60=*ON
     C*
      * If Producer has Cancel Date, check against Agent Hist Eff Date
     C  N90*IN60         IFEQ      *OFF
     C     APCXDT        ANDGT     *ZEROS
      *
     C                   Z-ADD     *ZEROS        ##EFDT
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ##EFDT
      *
     C*
     C                   Z-ADD     *ZEROS        ##SCDT
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ##SCDT
     C*
     C     APCXDT        IFLE      ##SCDT
     C     ##SCDT        ANDGT     *ZEROS
     C                   SETON                                        36  90
     C                   ENDIF                                                  APCXDT > EFDT
      *
     C                   ENDIF                                                  *IN60=*ON
      *                                                                ***
     C  N90              EXSR      PRODNM
     C  N90              MOVEL     XPNAME        SCPNAM
      *
      * Producer must have license for employer rating state.
      *
     C  N90              EXSR      PRLIC
      *
     C  N90XXLIC         IFEQ      'N'
     C                   SETON                                        79  90
     C                   ENDIF
     C*
     C* VALIDATE CONTROL FLAG
     C*
     C     *IN90         IFEQ      *OFF
     C     SCCAF1        ANDNE     'Y'
     C     SCCAF1        ANDNE     ' '
     C                   SETOFF                                       82
     C                   SETON                                        8190
     C                   ENDIF
     C*
     C     SCAGT2        IFNE      *ZEROS
     C     SCRT#2        ORNE      *ZEROS
     C     SCPRD2        ORNE      *ZEROS
     C     SVAGT2        ORNE      *ZEROS
     C     SVPRD2        ORNE      *ZEROS
     C*
     C*  VALIDATE SECONDARY AGENT NUMBER
     C*
      * IF 2nd Agt# changes & saved 2nd Agt# is not zeros, check for a
      *  Commission record for this year. If so, cannot change prod #.
      * If secondary agent is zeros, bypass remainder of edits for it.
     C  N90SCAGT2        IFNE      *ZEROS
     C                   MOVE      SCAGT2        XXAGT#
     C     AAKY          CHAIN     PAA01                              60
     C  N90*IN60         IFEQ      *ON
     C     AASTAT        OREQ      'I'
     C                   SETON                                        26  90
     C                   ENDIF                                                  *IN60=*ON
     C*
     C  N90              EXSR      AGTNM
     C  N90              MOVEL     AGNAME        SCANM2
     C*
     C  N90SCAGT2        IFNE      SVAGT2
     C*
     C     AAEXDT        IFEQ      *ZEROS
     C     AAEXDT        ORGT      SYSDT
     C*
     C     SCRT#2        IFEQ      *ZERO
     C                   Z-ADD     AADRT#        SCRT#2
     C                   ENDIF                                                  SCRT#2=*ZERO
     C*
     C                   Z-ADD     SCRT#2        XXRT#
     C                   EXSR      RATES
     C                   Z-ADD     XXNWRT        SCNWR2
     C                   Z-ADD     XXRNRT        SCRNR2
     C                   MOVEL(P)  XXPLAN        SCPLN2
     C                   MOVEL(P)  XXPLDS        SCPLD2
     C*
     C*
     C                   ENDIF                                                  SYSDT<=AAEXDT=*ZEROS
     C*
     C     SCRT#2        COMP      *ZERO                              5151
     C*
     C                   Z-ADD     SCAGT2        SVAGT2
     C                   SETON                                        75  90
     C*
     C                   ENDIF                                                  SCAGT2<>SVAGT2
     C*
     C*
     C  N90AAEFDT        IFGT      XXEFDT
     C                   SETON                                        89  90
     C                   ENDIF                                                  AAEFDT>XXEFDT
     C*
     C  N90AACNDT        IFNE      *ZERO
     C     AACNDT        ANDLT     XXEFDT
     C                   SETON                                        89  90
     C                   ENDIF                                                  AAEFDT>XXEFDT
     C*
     C*
     C*  EDIT SECONDARY AGENT RATE NUMBER
     C*
     C                   Z-ADD     SCRT#2        XXRT#
     C     ARKY          SETGT     PAR01
     C     ARKY1         READPE    PAR01                                  60
     C*
     C  N90*IN60         DOWEQ     *OFF
     C     ARSTAT        ANDEQ     'I'
     C     ARKY1         READPE    PAR01                                  60
     C                   ENDDO                                                  *IN60=*OFF
     C  N90
     CAN 60              SETON                                        27  90
     C  N90
     CANN60              Z-ADD     ARNWRT        SCNWR2
     C  N90
     CANN60              Z-ADD     ARRNRT        SCRNR2
     C                   Z-ADD     2             P
     C  N90              EXSR      CHKPLN
     C*
     C*  EDIT SECONDARY PRODUCER NUMBER
     C*
     C                   Z-ADD     SCPRD2        XXPROD
     C     APKY          CHAIN     PAP01                              60
     C  N90*IN60         IFEQ      *ON
     C     APSTAT        OREQ      'I'
     C                   SETON                                        28  90
     C                   ENDIF                                                  *IN60=*ON
      *
     C* If Producer has Cancel Date, check against Agent Hist Eff Date
     C  N90*IN60         IFEQ      *OFF
     C     APCXDT        ANDGT     *ZEROS
     C     APCXDT        ANDLE     AHEFDT
     C                   SETON                                        37  90
     C                   ENDIF                                                  *IN60=*ON
     C*
     C  N90              EXSR      PRODNM
     C  N90              MOVEL     XPNAME        SCPNM2
      *
      * Producer must have license for employer rating state.
      *
     C*
     C  N90              EXSR      PRLIC
      *
     C  N90XXLIC         IFEQ      'N'
     C                   SETON                                        80  90
     C                   ENDIF
     C*
     C                   ENDIF                                                  SCAGT2<>*ZEROS
     C*
     C                   ENDIF                                                  SCAGT2<>*ZEROS
     C*
     C* VALIDATE CONTROL FLAG
     C*
     C     *IN90         IFEQ      *OFF
     C     SCCAF2        ANDNE     'Y'
     C     SCCAF2        ANDNE     ' '
     C                   SETOFF                                       81
     C                   SETON                                        8290
     C                   ENDIF
     C*
     C* ONLY ALLOW PRIMARY OR SECONDARY CONTROL FLAG TO BE SET
     C*
     C     *IN90         IFEQ      *OFF
     C     SCCAF1        ANDEQ     'Y'
     C     SCCAF2        ANDEQ     'Y'
     C                   SETON                                        818290
     C                   ENDIF
     C*  VALIDATE TERTIARY AGENT
     C     SCAGT3        IFNE      *ZEROS
     C     SCRT#3        ORNE      *ZEROS
     C     SCPRD3        ORNE      *ZEROS
     C     SVAGT3        ORNE      *ZEROS
     C     SVPRD3        ORNE      *ZEROS
     C*  VALIDATE TERTIARY  AGENT NUMBER
      * If Tertiary  agent is zeros, bypass remainder of edits for it.
     C  N90SCAGT3        IFNE      *ZEROS
     C                   MOVE      SCAGT3        XXAGT#
     C     AAKY          CHAIN     PAA01                              60
     C  N90*IN60         IFEQ      *ON
     C     AASTAT        OREQ      'I'
     C                   SETON                                        26  90
     C                   ENDIF                                                  *IN60=*ON
     C*
     C  N90              EXSR      AGTNM
     C  N90              MOVEL     AGNAME        SCANT3
     C  N90SCAGT3        IFNE      SVAGT3
     C*
     C     AAEXDT        IFEQ      *ZEROS
     C     AAEXDT        ORGT      SYSDT
     C*
     C     SCRT#3        IFEQ      *ZERO
     C                   Z-ADD     AADRT#        SCRT#3
     C                   ENDIF                                                  SCRT#2=*ZERO
     C*
     C                   Z-ADD     SCRT#3        XXRT#
     C                   EXSR      RATES
     C                   Z-ADD     XXNWRT        SCNWT3
     C                   Z-ADD     XXRNRT        SCRNT3
     C                   MOVEL(P)  XXPLAN        SCPLT3
     C                   MOVEL(P)  XXPLDS        SCPLT3
     C*
     C                   ENDIF                                                  SYSDT<=AAEXDT=*ZEROS
     C*
     C**** BEG CHG 03/01/10 *******************************************
     C*    SCRT#3        COMP      *ZERO                              5151
     C     SCRT#3        COMP      *ZERO                              5454
     C**** END CHG 03/01/10 *******************************************
     C*
     C                   Z-ADD     SCAGT3        SVAGT3
     C**** BEG CHG 03/01/10 *******************************************
     C*                  SETON                                        75  90
     C                   SETON                                        66  90
     C**** END CHG 03/01/10 *******************************************
     C*
     C                   ENDIF                                                  SCAGT2<>SVAGT2
     C*
     C  N90AAEFDT        IFGT      XXEFDT
     C                   SETON                                        89  90
     C                   ENDIF                                                  AAEFDT>XXEFDT
     C*
     C  N90AACNDT        IFNE      *ZERO
     C     AACNDT        ANDLT     XXEFDT
     C                   SETON                                        89  90
     C                   ENDIF                                                  AAEFDT>XXEFDT
     C*
     C*  EDIT TERTIARY  AGENT RATE NUMBER
     C*
     C                   Z-ADD     SCRT#3        XXRT#
     C     ARKY          SETGT     PAR01
     C     ARKY1         READPE    PAR01                                  60
     C*
     C  N90*IN60         DOWEQ     *OFF
     C     ARSTAT        ANDEQ     'I'
     C     ARKY1         READPE    PAR01                                  60
     C                   ENDDO                                                  *IN60=*OFF
     C  N90
     CAN 60              SETON                                        27  90
     C  N90
     CANN60              Z-ADD     ARNWRT        SCNWT3
     C  N90
     CANN60              Z-ADD     ARRNRT        SCRNT3
     C*
     C                   Z-ADD     2             P
     C  N90              EXSR      CHKPLN
     C*
     C*  EDIT TERTIARY  PRODUCER NUMBER
     C                   Z-ADD     SCPRD3        XXPROD
     C     APKY          CHAIN     PAP01                              60
     C  N90*IN60         IFEQ      *ON
     C     APSTAT        OREQ      'I'
     C                   SETON                                        28  90
     C                   ENDIF                                                  *IN60=*ON
      *
     C* If Producer has Cancel Date, check against Agent Hist Eff Date
     C  N90*IN60         IFEQ      *OFF
     C     APCXDT        ANDGT     *ZEROS
     C     APCXDT        ANDLE     AHEFDT
     C                   SETON                                        37  90
     C                   ENDIF                                                  *IN60=*ON
     C*BEG  CHG ***** 03/01/10 *****************************
     C* N90              EXSR      PRODNM
     C* N90              MOVEL     XPNAME        SCNAM3
     C  N90              EXSR      PRODNM
     C  N90              MOVEL     XPNAME        SCPNT3
     C*END  CHG ***** 03/01/10 *****************************
      * Producer must have license for employer rating state.
      *
     C**** BEG ADD 03/01/10**************************
     C     SCPNT3        IFEQ      *BLANKS
     C                   Z-ADD     SCPRD3        XXPROD
     C                   EXSR      PRODNM
     C                   MOVEL     XPNAME        SCPNT3
     C                   ENDIF
     C**** END ADD 03/01/10**************************
     C*
     C  N90              EXSR      PRLIC
      *
     C  N90XXLIC         IFEQ      'N'
     C                   SETON                                        80  90
     C                   ENDIF
     C                   ENDIF                                                  SCAGT2<>*ZEROS
     C                   ENDIF                                                  SCAGT2<>*ZEROS
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90AHDT          IFGT      SAVEDT
     C     AHDT          OREQ      SAVEDT
     C     AHTIME        ANDGT     SAVETM
     C                   SETON                                        93  90
     C                   ENDIF                                                  AHDT>SAVEDT
      ***Agent*of record must be valid, if entered
     C                   MOVE      '0'           SAMEAG
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCK1
     C*
     C*================================================================
     C*=====BEG ADD 01/22/10 ==========================================
     C* SAMAGT - Verify the Agent is alread Active
     C*================================================================
     C     SAMAGT        BEGSR
     C                   CLEAR                   WDMSG1
     C                   CLEAR                   WDMSG2
     C                   CLEAR                   WDMSG3
     C                   CLEAR                   WDMSG4
     C                   CLEAR                   WDMSG5
     C     AHKY1         CHAIN     PAH01                              60
     C     *IN60         DOWEQ     *OFF
     C     AHSTAT        IFEQ      'A'
     C                   SELECT
     C     SCAGT#        WHENEQ    AHSAGT
     C     SCPROD        ANDEQ     AHSPRD
     C                   MOVE      '1'           SAMEAG
     C                   MOVEL(P)  MSG(12)       WDMSG3
     C                   MOVEL(P)  MSG(13)       WDMSG4
     C     SCAGT#        WHENEQ    AH3AGT
     C     SCPROD        ANDEQ     AH3PRD
     C                   MOVE      '1'           SAMEAG
     C                   MOVEL(P)  MSG(14)       WDMSG3
     C                   MOVEL(P)  MSG(15)       WDMSG4
     C     SCAGT2        WHENEQ    AHAGT#
     C     SCPRD2        ANDEQ     AHPROD
     C     SCAGT2        IFNE      0
     C                   MOVE      '1'           SAMEAG
     C                   MOVEL(P)  MSG(16)       WDMSG3
     C                   MOVEL(P)  MSG(17)       WDMSG4
     C                   ENDIF
     C     SCAGT2        WHENEQ    AH3AGT
     C     SCPRD2        ANDEQ     AH3PRD
     C     SCAGT2        IFNE      0
     C                   MOVE      '1'           SAMEAG
     C                   MOVEL(P)  MSG(18)       WDMSG3
     C                   MOVEL(P)  MSG(19)       WDMSG4
     C                   ENDIF
     C     SCAGT3        WHENEQ    AHAGT#
     C     SCPRD3        ANDEQ     AHPROD
     C     SCAGT3        IFNE      0
     C                   MOVE      '1'           SAMEAG
     C                   MOVEL(P)  MSG(20)       WDMSG3
     C                   MOVEL(P)  MSG(21)       WDMSG4
     C                   ENDIF
     C     SCAGT3        WHENEQ    AHSAGT
     C     SCPRD3        ANDEQ     AHSPRD
     C     SCAGT3        IFNE      0
     C                   MOVE      '1'           SAMEAG
     C                   MOVEL(P)  MSG(22)       WDMSG3
     C                   MOVEL(P)  MSG(23)       WDMSG4
     C                   ENDIF
     C                   ENDSL
     C                   ENDIF
     C     AHKY1         READPE    PAH01                                  60
     C                   ENDDO
     C                   ENDSR
     C*================================================================
     C*=====END ADD 01/22/10 ==========================================
     C*=====BEG ADD 01/22/10 ==========================================
     C* SAMCHG - Verify the Agent is alread Active
     C*================================================================
     C     SAMCHG        BEGSR
     C                   CLEAR                   WDMSG1
     C                   CLEAR                   WDMSG2
     C                   CLEAR                   WDMSG3
     C                   CLEAR                   WDMSG4
     C                   CLEAR                   WDMSG5
     C     PRCOMM        IFNE      'Y'
     C                   MOVE      *OFF          SAMECH
     C                   ENDIF
     C                   SELECT
     C     SCRT#         WHENNE    S1RT#
     C                   MOVEL(P)  MSG(24)       WDMSG1
     C                   MOVE      *ON           SAMECH
     C     SCAGT2        WHENNE    S1AGT2
     C                   MOVEL(P)  MSG(25)       WDMSG1
     C                   MOVE      *ON           SAMECH
     C     SCAGT3        WHENNE    S1AGT3
     C                   MOVEL(P)  MSG(26)       WDMSG1
     C                   MOVE      *ON           SAMECH
     C                   ENDSL
     C                   ENDSR
     C*================================================================
     C*=====END ADD 01/22/10 ==========================================
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C*
     C     ERRCK2        BEGSR
     C*
     C                   MOVE      *OFF          NOTDSP
     C                   MOVE      *OFF          NOTDSQ
     C*
     C*  AGENT OF RECORD MUST BE VALID, IF ENTERED
     C*
     C  N90SCAGRC        IFNE      *ZEROS
     C     SCRTRC        ORNE      *ZEROS
     C     SCPRRC        ORNE      *ZEROS
     C*
     C*    AGENT NUMBER - AGENT OF RECORD
     C*
     C                   Z-ADD     SCAGRC        XXAGT#
     C*
     C     AAKY          CHAIN     PAA01                              60
     C*
     C  N90*IN60         IFEQ      *ON
     C     AASTAT        OREQ      'I'
     C                   SETON                                        71  90
     C                   ENDIF                                                  *IN60=*ON
     C*
     C  N90              EXSR      AGTNM
     C*
     C  N90              MOVEL     AGNAME        SCANM3
     C*
     C     SCAGRC        IFNE      SVAGRC
     C*
     C     AAEXDT        IFEQ      *ZEROS
     C     AAEXDT        ORGT      SYSDT
     C*
     C     SCRTRC        IFEQ      *ZERO
     C                   Z-ADD     AADRT#        SCRTRC
     C                   ENDIF                                                  SCRTRC=*ZERO
     C*
     C                   Z-ADD     SCRTRC        XXRT#
     C                   EXSR      RATES
     C                   Z-ADD     XXNWRT        SCNWR3
     C                   Z-ADD     XXRNRT        SCRNR3
     C                   MOVEL(P)  XXPLAN        SCPLN3
     C                   MOVEL(P)  XXPLDS        SCPLD3
     C*
     C*
     C                   ENDIF                                                  SYSDT<=AAEXDT=*ZEROS
     C*
     C     SCRTRC        COMP      *ZERO                              5252
     C*
     C                   Z-ADD     SCAGRC        SVAGRC
     C*
     C                   SETON                                        82  90
     C*
     C                   ENDIF                                                  SCAGRC<>SVAGRC
     C*
     C     SC2ARC        IFNE      SV2ARC
     C                   SETON                                            90
     C                   ENDIF                                                  SC2ARC<>SV2ARC
     C*
     C*
     C  N90AAEFDT        IFGT      XXEFDT
     C                   SETON                                        74  90
     C                   ENDIF                                                  AAEFDT>XXEFDT
     C*
     C  N90AACNDT        IFNE      *ZERO
     C     AACNDT        ANDLT     XXEFDT
     C                   SETON                                        74  90
     C                   ENDIF                                                  AAEFDT>XXEFDT
     C*
     C*
     C*    RATE - AGENT OF RECORD
     C*
     C                   Z-ADD     SCRTRC        XXRT#
     C*
     C     ARKY          SETGT     PAR01
     C     ARKY1         READPE    PAR01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C     ARSTAT        ANDEQ     'I'
     C     ARKY1         READPE    PAR01                                  60
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C  N90
     CAN 60              SETON                                        72  90
     C  N90
     CANN60              Z-ADD     ARNWRT        SCNWR3
     C  N90
     CANN60              Z-ADD     ARRNRT        SCRNR3
     C*
     C*    PRODUCER - AGENT OF RECORD
     C*
     C                   Z-ADD     SCPRRC        XXPROD
     C*
     C     APKY          CHAIN     PAP01                              60
     C*
     C  N90*IN60         IFEQ      *ON
     C     APSTAT        OREQ      'I'
     C                   SETON                                        73  90
     C                   ENDIF                                                  *IN60=*ON
     C* If Producer has Cancel Date, check against Agent Hist Eff Date
     C  N90*IN60         IFEQ      *OFF
     C     APCXDT        ANDGT     *ZEROS
     C     APCXDT        ANDLE     AHEFDT
     C                   SETON                                        36  90
     C                   ENDIF                                                  *IN60=*ON
     C*
     C  N90              EXSR      PRODNM
     C  N90              MOVEL     XPNAME        SCPNM3
      *
      * Producer must have license for employer rating state.
      *
     C  N90              EXSR      PRLIC
      *
     C  N90XXLIC         IFEQ      'N'
     C                   SETON                                        79  90
     C                   ENDIF
      *
     C*
     C                   ENDIF                                                  SCAGT2<>*ZEROS
     C*
     C* VALIDATE CONTROL FLAG
     C*
     C     *IN90         IFEQ      *OFF
     C     SCCRF1        ANDNE     'Y'
     C     SCCRF1        ANDNE     ' '
     C                   SETOFF                                       89
     C                   SETON                                        8890
     C                   ENDIF
     C*
     C     SC2ARC        IFNE      *ZEROS
     C     SC2RRC        ORNE      *ZEROS
     C     SC2PRC        ORNE      *ZEROS
     C*
     C*  SECONDARY AGENT OF RECORD MUST BE VALID, IF ENTERED
     C*
     C                   Z-ADD     SC2ARC        XXAGT#
     C*
     C     AAKY          CHAIN     PAA01                              60
     C*
     C  N90*IN60         IFEQ      *ON
     C     AASTAT        OREQ      'I'
     C                   SETON                                        75  90
     C                   ENDIF                                                  *IN60=*ON
     C*
     C  N90              EXSR      AGTNM
     C  N90              MOVEL     AGNAME        SCANM4
     C*
     C     SC2ARC        IFNE      SV2ARC
     C*
     C     AAEXDT        IFEQ      *ZEROS
     C     AAEXDT        ORGT      SYSDT
     C*
     C     SC2RRC        IFEQ      *ZERO
     C                   Z-ADD     AADRT#        SC2RRC
     C                   ENDIF                                                  SC2RRC=*ZERO
     C*
     C                   Z-ADD     SC2RRC        XXRT#
     C                   EXSR      RATES
     C                   Z-ADD     XXNWRT        SCNWR4
     C                   Z-ADD     XXRNRT        SCRNR4
     C                   MOVEL(P)  XXPLAN        SCPLN4
     C                   MOVEL(P)  XXPLDS        SCPLD4
     C*
     C                   ENDIF                                                  SYSDT<=AAEXDT=*ZEROS
     C*
     C     SC2RRC        COMP      *ZERO                              5353
     C*
     C                   Z-ADD     SC2ARC        SV2ARC
     C*
     C                   SETON                                        86  90
     C*
     C                   ENDIF                                                  SC2ARC<>SVAGT2
     C*
     C*
     C  N90AAEFDT        IFGT      XXEFDT
     C                   SETON                                        78  90
     C                   ENDIF                                                  AAEFDT>XXEFDT
     C*
     C  N90AACNDT        IFNE      *ZERO
     C     AACNDT        ANDLT     XXEFDT
     C                   SETON                                        78  90
     C                   ENDIF                                                  AAEFDT>XXEFDT
     C*
     C*
     C*
     C*    RATE - SECONDARY AGENT OF RECORD
     C*
     C                   Z-ADD     SC2RRC        XXRT#
     C*
     C     ARKY          SETGT     PAR01
     C     ARKY1         READPE    PAR01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C     ARSTAT        ANDEQ     'I'
     C     ARKY1         READPE    PAR01                                  60
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C  N90
     CAN 60              SETON                                        76  90
     C  N90
     CANN60              Z-ADD     ARNWRT        SCNWR4
     C  N90
     CANN60              Z-ADD     ARRNRT        SCRNR4
     C                   Z-ADD     SC2PRC        XXPROD
     C*
     C     APKY          CHAIN     PAP01                              60
     C*
     C  N90*IN60         IFEQ      *ON
     C     APSTAT        OREQ      'I'
     C                   SETON                                        77  90
     C                   ENDIF                                                  *IN60=*ON
     C* If Producer has Cancel Date, check against Agent Hist Eff Date
     C  N90*IN60         IFEQ      *OFF
     C     APCXDT        ANDGT     *ZEROS
     C     APCXDT        ANDLE     AHEFDT
     C                   SETON                                        37  90
     C                   ENDIF                                                  *IN60=*ON
     C*
     C  N90              EXSR      PRODNM
     C  N90              MOVEL     XPNAME        SCPNM4
      *
      * Producer must have license for employer rating state.
      *
      *
     C  N90              EXSR      PRLIC
      *
     C  N90XXLIC         IFEQ      'N'
     C                   SETON                                        80  90
     C                   ENDIF
      *
     C*
     C* VALIDATE CONTROL FLAG
     C*
     C     *IN90         IFEQ      *OFF
     C     SCCRF2        ANDNE     'Y'
     C     SCCRF2        ANDNE     ' '
     C                   SETOFF                                       88
     C                   SETON                                        8990
     C                   ENDIF
     C                   ENDIF                                                  SC2ARC<>*ZEROS
     C*
     C* ONLY ALLOW PRIMARY OR SECONDARY CONTROL FLAG TO BE SET
     C*
     C     *IN90         IFEQ      *OFF
     C     SCCRF1        ANDEQ     'Y'
     C     SCCRF2        ANDEQ     'Y'
     C                   SETON                                        888990
     C                   ENDIF
      ***** BEG ADD ******* 01/22/10 **********************************
     C     SC3ARC        IFNE      *ZEROS
     C     SC3RRC        ORNE      *ZEROS
     C     SC3PRC        ORNE      *ZEROS
     C*
     C*  TERTIARY  AGENT OF RECORD MUST BE VALID, IF ENTERED
     C*
     C                   Z-ADD     SC3ARC        XXAGT#
     C*
     C     AAKY          CHAIN     PAA01                              60
     C*
     C  N90*IN60         IFEQ      *ON
     C     AASTAT        OREQ      'I'
     C**** BEG CHG 03/01/10 *******************************************
     C**                 SETON                                        75  90
     C                   SETON                                        66  90
     C**** END CHG 03/01/10 *******************************************
     C                   ENDIF                                                  *IN60=*ON
     C*
     C  N90              EXSR      AGTNM
     C  N90              MOVEL     AGNAME        SCANM5
     C*
     C     SC3ARC        IFNE      SV3ARC
     C*
     C     AAEXDT        IFEQ      *ZEROS
     C     AAEXDT        ORGT      SYSDT
     C*
     C     SC3RRC        IFEQ      *ZERO
     C                   Z-ADD     AADRT#        SC3RRC
     C                   ENDIF                                                  SC2RRC=*ZERO
     C*
     C                   Z-ADD     SC3RRC        XXRT#
     C                   EXSR      RATES
     C***BEG CHG 03/01/10*******************************************
     C***                Z-ADD     XXNWRT        SCNWRT
     C***                Z-ADD     XXRNRT        SCRNRT
     C                   Z-ADD     XXNWRT        SC3NWR
     C                   Z-ADD     XXRNRT        SC3NWR
     C***END CHG 03/01/10******************************************
     C                   MOVEL(P)  XXPLAN        SCPLN5
     C                   MOVEL(P)  XXPLDS        SCPLD5
     C*
     C                   ENDIF                                                  SYSDT<=AAEXDT=*ZEROS
     C*
     C     SC3RRC        COMP      *ZERO                              5353
     C*
     C                   Z-ADD     SC3ARC        SV3ARC
     C*
     C                   SETON                                        86  90
     C*
     C                   ENDIF                                                  SC2ARC<>SVAGT2
     C*
     C*
     C  N90AAEFDT        IFGT      XXEFDT
     C                   SETON                                        78  90
     C                   ENDIF                                                  AAEFDT>XXEFDT
     C*
     C  N90AACNDT        IFNE      *ZERO
     C     AACNDT        ANDLT     XXEFDT
     C                   SETON                                        78  90
     C                   ENDIF                                                  AAEFDT>XXEFDT
     C*
     C*    RATE - TERTIARY  AGENT OF RECORD
     C*
     C                   Z-ADD     SC3RRC        XXRT#
     C*
     C     ARKY          SETGT     PAR01
     C     ARKY1         READPE    PAR01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C     ARSTAT        ANDEQ     'I'
     C     ARKY1         READPE    PAR01                                  60
     C                   ENDDO                                                  *IN60=*OFF
     C  N90
     C**** BEG CHG 03/01/10 *******************************************
     C*N 60              SETON                                        76  90
     CAN 60              SETON                                        67  90
     C**** END CHG 03/01/10 *******************************************
     C  N90
     CANN60              Z-ADD     ARNWRT        SCNWRT
     C  N90
     CANN60              Z-ADD     ARRNRT        SCRNRT
     C*    PRODUCER - AGENT OF RECORD
     C*
     C                   Z-ADD     SC3PRC        XXPROD
     C*
     C     APKY          CHAIN     PAP01                              60
     C*
     C  N90*IN60         IFEQ      *ON
     C     APSTAT        OREQ      'I'
     C                   SETON                                        77  90
     C                   ENDIF                                                  *IN60=*ON
     C* If Producer has Cancel Date, check against Agent Hist Eff Date
     C  N90*IN60         IFEQ      *OFF
     C     APCXDT        ANDGT     *ZEROS
     C     APCXDT        ANDLE     AHEFDT
     C                   SETON                                        37  90
     C                   ENDIF                                                  *IN60=*ON
     C*
     C  N90              EXSR      PRODNM
     C  N90              MOVEL     XPNAME        SCPNM5
      * Producer must have license for employer rating state.
      *
     C  N90              EXSR      PRLIC
      *
     C  N90XXLIC         IFEQ      'N'
     C                   SETON                                        80  90
     C                   ENDIF
     C                   END
      *
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90AHDT          IFGT      SAVEDT
     C     AHDT          OREQ      SAVEDT
     C     AHTIME        ANDGT     SAVETM
     C                   SETON                                        93  90
     C                   ENDIF                                                  AHDT>SAVEDT
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
     C***  BEGIN ADD  ***  07/13/00  *********************************
     C*================================================================
     C* GETAGT - Get the latest agent for employer
     C*================================================================
     C*
     C     GETAGT        BEGSR
     C*
     C                   CALL      'WA002R'                             99
     C                   PARM                    EMCO#
     C                   PARM                    EMFND
     C                   PARM                    EMEMP#
     C                   PARM                    EMDIV
     C                   PARM      *ZERO         ##FYR
     C                   PARM      *ZERO         ##AGT#
     C                   PARM      *BLANK        ##ANAM
     C*
     C                   ENDSR                                                  GETAGT
     C*
     C*================================================================
     C* KIDO - Processing for F9
     C*================================================================
     C*
     C     KIDO          BEGSR
     C*
     C                   EXSR      QUOTE
     C*
     C   09              MOVE      '1'           ##CALL
     C  N09              MOVE      ' '           ##CALL
     C*
     C                   CALL      'W4150R'                             99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##CALL            1
     C                   PARM      DBRRNA        ##RRN             9 0
     C                   PARM      @@QTE         ##QTE             1
     C*
     C     *INKC         COMP      *ON                                9090
     C*
     C                   ENDSR                                                  KIDO
     C*
     C*================================================================
     C* KJDO - Processing for F10
     C*================================================================
     C*
     C     KJDO          BEGSR
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCAGT#'
     C     *IN40         ANDEQ     *ON
     C     HFLD          OREQ      'SCAGT2'
     C     HFLD          OREQ      'SCAGRC'
     C     HFLD          OREQ      'SC2ARC'
     C***  BEGIN ADD  ***  01/22/10  *********************************
     C     HFLD          OREQ      'SCAGT3'
     C***  END   ADD  ***  01/22/10  *********************************
     C     HFLD          OREQ      'SC3ARC'
     C                   CALL      'W0040R'
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      '2'           ##CALL
     C                   PARM      DBRRN1        ##RRN
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     WMAAP                              60
     C*
     C                   Z-ADD     AAAGT#        XXAGT#
     C                   EXSR      AGTNM
     C*
     C     HFLD          IFEQ      'SCAGT#'
     C*
     C                   Z-ADD     AAAGT#        SCAGT#
     C                   MOVEL     XANAME        SCAGNM
     C                   MOVE      *ON           *IN72
     C*
     C     AAEXDT        IFEQ      *ZEROS
     C     AAEXDT        ORGT      SYSDT
     C*
     C     AAAGT#        IFNE      SVAGT#
     C     AADRT#        ANDNE     *ZEROS
      *
     C                   Z-ADD     AADRT#        SCRT#
     C                   Z-ADD     AADRT#        XXRT#
     C                   EXSR      RATES
     C                   Z-ADD     XXNWRT        SCNWRT
     C                   Z-ADD     XXRNRT        SCRNRT
     C                   MOVEL(P)  XXPLAN        SCPLAN
     C                   MOVEL(P)  XXPLDS        SCPLDS
     C                   ENDIF                                                  AAAGT#<>SVAGT#
     C*
     C                   ENDIF                                                  SYSDT<=AAEXDT=*ZEROS
     C*
     C     SCRT#         COMP      *ZERO                              5050
     C                   Z-ADD     AAAGT#        SCAGT#
     C                   Z-ADD     AAAGT#        SVAGT#
     C*
     C                   ENDIF                                                  HFLD=SCAGT#
     C*
     C     HFLD          IFEQ      'SCAGT2'
     C*
     C                   Z-ADD     AAAGT#        SCAGT2
     C                   MOVEL     XANAME        SCANM2
     C                   MOVE      *ON           *IN75
     C*
     C     AAEXDT        IFEQ      *ZEROS
     C     AAEXDT        ORGT      SYSDT
     C*
     C     AAAGT#        IFNE      SVAGT2
     C     AADRT#        ANDNE     *ZEROS
      *
     C                   Z-ADD     AADRT#        SCRT#2
     C                   Z-ADD     AADRT#        XXRT#
     C                   EXSR      RATES
     C                   Z-ADD     XXNWRT        SCNWR2
     C                   Z-ADD     XXRNRT        SCRNR2
     C                   MOVEL(P)  XXPLAN        SCPLN2
     C                   MOVEL(P)  XXPLDS        SCPLD2
     C                   ENDIF                                                  AAAGT#<>SVAGTS
     C*
     C                   ENDIF                                                  SYSDT<=AAEXDT=*ZEROS
     C*
     C     SCRT#2        COMP      *ZERO                              5151
     C                   Z-ADD     AAAGT#        SCAGT2
      * If SVAGT2 is not ZEROS, don't change it for new edits
     C     SVAGT2        IFEQ      *ZEROS
     C                   Z-ADD     AAAGT#        SVAGT2
     C                   ENDIF                                                  SVAGT2=*ZEROS
     C*
     C                   ENDIF                                                  HFLD=SCAGT2
     C*
     C***  BEGIN ADD  ***  01/22/10  *********************************
      *
     C     HFLD          IFEQ      'SCAGT3'
     C                   Z-ADD     AAAGT#        SCAGT3
     C                   MOVEL     XANAME        SCANT3
     C***  BEG   CHG  ***  03/01/10  *********************************
     C**                 MOVE      *ON           *IN75
     C                   MOVE      *ON           *IN66
     C***  END   CHG  ***  03/01/10  *********************************
     C     AAEXDT        IFEQ      *ZEROS
     C     AAEXDT        ORGT      SYSDT
     C     AAAGT#        IFNE      SVAGT2
     C     AADRT#        ANDNE     *ZEROS
     C                   Z-ADD     AADRT#        SCRT#3
     C                   Z-ADD     AADRT#        XXRT#
     C                   EXSR      RATES
     C                   Z-ADD     XXNWRT        SCNWT3
     C                   Z-ADD     XXRNRT        SCRNT3
     C                   MOVEL(P)  XXPLAN        SCPNT3
     C                   MOVEL(P)  XXPLDS        SCPLT3
     C                   ENDIF                                                  AAAGT#<>SVAGTS
     C                   ENDIF                                                  SYSDT<=AAEXDT=*ZEROS
     C***  BEG   CHG  ***  03/01/10  *********************************
     C*    SCRT#3        COMP      *ZERO                              5151
     C     SCRT#3        COMP      *ZERO                              5454
     C***  BEG   CHG  ***  03/01/10  *********************************
     C                   Z-ADD     AAAGT#        SCAGT3
      * If SVAGT3 is not ZEROS, don't change it for new edits
     C*          SVAGT3    IFEQ *ZEROS
     C*                    Z-ADDAAAGT#    SVAGT3
     C*                    ENDIF                           SVAGT2=*ZEROS
     C                   ENDIF                                                  HFLD=SCAGT2
      *
      *
     C     HFLD          IFEQ      'SCAGRC'
      *
     C                   Z-ADD     AAAGT#        SCAGRC
     C                   MOVEL     XANAME        SCANM3
     C                   MOVE      *ON           *IN82
      *
     C     AAEXDT        IFEQ      *ZEROS
     C     AAEXDT        ORGT      SYSDT
      *
     C     AAAGT#        IFNE      SVAGRC
     C     AADRT#        ANDNE     *ZEROS
      *
     C                   Z-ADD     AADRT#        SCRTRC
     C                   Z-ADD     AADRT#        XXRT#
     C                   EXSR      RATES
     C                   Z-ADD     XXNWRT        SCNWR3
     C                   Z-ADD     XXRNRT        SCRNR3
     C                   MOVEL(P)  XXPLAN        SCPLN3
     C                   MOVEL(P)  XXPLDS        SCPLD3
     C                   ENDIF                                                  AAAGT#<>SVAGRC
      *
     C                   ENDIF                                                  SYSDT<=AAEXDT=*ZEROS
      *
     C     SCRTRC        COMP      *ZERO                              5252
     C                   Z-ADD     AAAGT#        SCAGRC
     C                   Z-ADD     AAAGT#        SVAGRC
     C                   ENDIF                                                  HFLD=SCAGRC
      *
     C     HFLD          IFEQ      'SC2ARC'
     C*
     C                   Z-ADD     AAAGT#        SC2ARC
     C                   MOVEL     XANAME        SCANM4
     C                   MOVE      *ON           *IN86
     C*
     C     AAEXDT        IFEQ      *ZEROS
     C     AAEXDT        ORGT      SYSDT
     C*
     C     AAAGT#        IFNE      SV2ARC
     C     AADRT#        ANDNE     *ZEROS
      *
     C                   Z-ADD     AADRT#        SC2RRC
     C                   Z-ADD     AADRT#        XXRT#
     C                   EXSR      RATES
     C                   Z-ADD     XXNWRT        SCNWR4
     C                   Z-ADD     XXRNRT        SCRNR4
     C                   MOVEL(P)  XXPLAN        SCPLN4
     C                   MOVEL(P)  XXPLDS        SCPLD4
     C                   ENDIF                                                  AAAGT#<>SV2ARC
     C*
     C                   ENDIF                                                  SYSDT<=AAEXDT=*ZEROS
     C*
     C     SC2RRC        COMP      *ZERO                              5353
     C                   Z-ADD     AAAGT#        SC2ARC
     C                   Z-ADD     AAAGT#        SV2ARC
     C*
     C                   ENDIF                                                  HFLD=SC2ARC
     C*
     C***  BEGIN ADD  ***  01/22/99  *********************************
     C     HFLD          IFEQ      'SC3ARC'
     C*
     C                   Z-ADD     AAAGT#        SC3ARC
     C                   MOVEL     XANAME        SCANM5
     C                   MOVE      *ON           *IN86
     C*
     C     AAEXDT        IFEQ      *ZEROS
     C     AAEXDT        ORGT      SYSDT
     C*
     C     AAAGT#        IFNE      SV2ARC
     C     AADRT#        ANDNE     *ZEROS
      *
     C                   Z-ADD     AADRT#        SC3RRC
     C                   Z-ADD     AADRT#        XXRT#
     C                   EXSR      RATES
     C***BEG CHG 03/01/10*******************************************
     C***                Z-ADD     XXNWRT        SCNWRT
     C***                Z-ADD     XXRNRT        SCRNRT
     C                   Z-ADD     XXNWRT        SC3NWR
     C                   Z-ADD     XXRNRT        SC3NWR
     C***END CHG 03/01/10******************************************
     C                   MOVEL(P)  XXPLAN        SCPLN5
     C                   MOVEL(P)  XXPLDS        SCPLD5
     C                   ENDIF                                                  AAAGT#<>SV2ARC
     C*
     C                   ENDIF                                                  SYSDT<=AAEXDT=*ZEROS
     C*
     C     SC3RRC        COMP      *ZERO                              5353
     C                   Z-ADD     AAAGT#        SC3ARC
     C                   Z-ADD     AAAGT#        SV3ARC
     C*
     C                   ENDIF                                                  HFLD=SC2ARC
     C***  END   ADD  ***  01/22/10  *********************************
     C                   ELSE
     C*
     C     HFLD          COMP      'SCAGT#'                               71
     C     HFLD          COMP      'SCAGT2'                               74
     C     HFLD          COMP      'SCAGRC'                               81
     C     HFLD          COMP      'SC2ARC'                               85
     C*
     C                   ENDIF                                                  ##RRN<>*ZEROS
     C*
     C**
     C     HFLD          WHENEQ    'SCRT#'
     C     HFLD          OREQ      'SCRT#2'
     C     HFLD          OREQ      'SCRT#3'
     C     HFLD          OREQ      'SCRTRC'
     C     HFLD          OREQ      'SC3RRC'
      ***** BEG ADD ******* 01/22/10 **********************************
     C     HFLD          OREQ      'SCRT#3'
      ***** BEG END ******* 01/22/10 **********************************
     C*
     C*
     C                   CALL      'W0640R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           @#CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      DBRRN1        ##RRN2            9 0
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     WMARP                              60
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C                   Z-ADD     ARRT#         XXRT#
     C                   EXSR      RATES
     C*
     C     HFLD          IFEQ      'SCRT#'
     C                   Z-ADD     ARRT#         SCRT#
     C                   Z-ADD     ARNWRT        SCNWRT
     C                   Z-ADD     ARRNRT        SCRNRT
     C                   MOVEL(P)  XXPLAN        SCPLAN
     C                   MOVEL(P)  XXPLDS        SCPLDS
     C                   MOVE      *ON           *IN73
     C                   ENDIF                                                  HFLD=SCRT#
     C*
     C     HFLD          IFEQ      'SCRT#2'
     C                   Z-ADD     ARRT#         SCRT#2
     C                   Z-ADD     ARNWRT        SCNWR2
     C                   Z-ADD     ARRNRT        SCRNR2
     C                   MOVEL(P)  XXPLAN        SCPLN2
     C                   MOVEL(P)  XXPLDS        SCPLD2
     C                   MOVE      *ON           *IN76
     C                   ENDIF                                                  HFLD=SCRT#2
     C***  BEGIN ADD  ***  01/22/10  *********************************
     C     HFLD          IFEQ      'SCRT#3'
     C                   Z-ADD     ARRT#         SCRT#3
     C                   Z-ADD     ARNWRT        SCNWT3
     C                   Z-ADD     ARRNRT        SCRNT3
     C                   MOVEL(P)  XXPLAN        SCPLT3
     C                   MOVEL(P)  XXPLDS        SCPDT3
     C     SCRT#3        COMP      *ZERO                              5454
     C***  BEG   CHG  ***  03/01/10  *********************************
     C*                  MOVE      *ON           *IN76
     C                   MOVE      *OFF          *IN66
     C                   MOVE      *ON           *IN67
     C***  END   CHG  ***  03/01/10  *********************************
     C                   ENDIF                                                  HFLD=SCRT#2
     C***  END   ADD  ***  01/22/10  *********************************
      *
     C     HFLD          IFEQ      'SCRTRC'
     C                   Z-ADD     ARRT#         SCRTRC
     C                   Z-ADD     ARNWRT        SCNWR3
     C                   Z-ADD     ARRNRT        SCRNR3
     C                   MOVEL(P)  XXPLAN        SCPLN3
     C                   MOVEL(P)  XXPLDS        SCPLD3
     C                   MOVE      *ON           *IN83
     C                   ENDIF                                                  HFLD=SCRTRC
      *
     C     HFLD          IFEQ      'SC2RRC'
     C                   Z-ADD     ARRT#         SC2RRC
     C                   Z-ADD     ARNWRT        SCNWR4
     C                   Z-ADD     ARRNRT        SCRNR4
     C                   MOVEL(P)  XXPLAN        SCPLN4
     C                   MOVEL(P)  XXPLDS        SCPLD4
     C                   MOVE      *ON           *IN87
     C                   ENDIF                                                  HFLD=SC2RRC
     C***  BEGIN ADD  ***  01/22/10  *********************************
     C     HFLD          IFEQ      'SC3RRC'
     C                   Z-ADD     ARRT#         SC3RRC
     C***BEG CHG 03/01/10*******************************************
     C**                 Z-ADD     ARNWRT        SCNWRT
     C**                 Z-ADD     ARRNRT        SCRNRT
     C                   Z-ADD     ARNWRT        SC3NWR
     C                   Z-ADD     ARRNRT        SC3NWR
     C***END CHG 03/01/10******************************************
     C                   MOVEL(P)  XXPLAN        SCPLN5
     C                   MOVEL(P)  XXPLDS        SCPLD5
     C**** BEG DEL 03/01/10 *******************************************
     C**                 MOVE      *ON           *IN87
     C**** END DEL 03/01/10 *******************************************
     C                   ENDIF                                                  HFLD=SC2RRC
     C*
     C                   ELSE
     C*
     C     HFLD          COMP      'SCRT#'                                72
     C     HFLD          COMP      'SCRT#2'                               75
     C     HFLD          COMP      'SCRTRC'                               82
     C     HFLD          COMP      'SC2RRC'                               86
     C*
     C                   ENDIF                                                  ##RT#<>*ZEROS
     C*
     C     SCRT#         COMP      *ZERO                              5050
     C     SCRT#2        COMP      *ZERO                              5151
     C     SCRTRC        COMP      *ZERO                              5252
     C     SC2RRC        COMP      *ZERO                              5353
     C**** BEGIN ADD  ***  01/22/10  *********************************
     C     SC3RRC        COMP      *ZERO                              5353
     C**** END   ADD  ***  01/22/10  *********************************
     C*
     C     HFLD          WHENEQ    'SCPROD'
     C     HFLD          OREQ      'SCPRD2'
     C     HFLD          OREQ      'SCPRD3'
     C     HFLD          OREQ      'SCPRRC'
     C     HFLD          OREQ      'SC2PRC'
     C***  BEGIN ADD  ***  01/22/10  *********************************
     C     HFLD          OREQ      'SC3PRC'
     C***  END   ADD  ***  01/22/10  *********************************
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCPROD'
     C                   Z-ADD     SCAGT#        XXAGT#
     C     HFLD          WHENEQ    'SCPRD2'
     C                   Z-ADD     SCAGT2        XXAGT#
     C******BEG*ADD ******* 01/22/10 **********************************
     C     HFLD          WHENEQ    'SCPRD3'
     C                   Z-ADD     SCAGT3        XXAGT#
     C******END*ADD ******* 01/22/10 **********************************
     C     HFLD          WHENEQ    'SCPRRC'
     C                   Z-ADD     SCAGRC        XXAGT#
     C     HFLD          WHENEQ    'SC2PRC'
     C                   Z-ADD     SC2ARC        XXAGT#
     C     HFLD          WHENEQ    'SC3PRC'
     C                   Z-ADD     SC3ARC        XXAGT#
     C                   ENDSL
     C*
     C     AAKY          CHAIN     PAA01                              60
     C*
     C     *IN60         IFEQ      *ON
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCPROD'
     C                   SETON                                        22  90
     C     HFLD          WHENEQ    'SCPRD2'
     C                   SETON                                        26  90
     C***  BEGIN ADD  ***  01/22/10  *********************************
     C     HFLD          WHENEQ    'SCPRD3'
     C                   SETON                                        26  90
     C***  BEGIN END  ***  01/22/10  *********************************
     C     HFLD          WHENEQ    'SCPRRC'
     C                   SETON                                        71  90
     C     HFLD          WHENEQ    'SC2PRC'
     C                   SETON                                        75  90
     C***  BEGIN ADD  ***  01/22/10  *********************************
     C     HFLD          WHENEQ    'SC3PRC'
     C***  BEGIN CHG  ***  03/01/10  *********************************
     C*                  SETON                                        75  90
     C                   SETON                                        66  90
     C***  END   CHG  ***  03/01/10  *********************************
     C***  END   ADD  ***  01/22/10  *********************************
     C                   ENDSL
     C*
     C                   ELSE
     C  N60AGKY          CHAIN     PAG01                              60
     C*
     C                   CALL      'W0042R'
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      '2'           ##CALL
     C                   PARM      DBRRNM        ##RRN
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     WMAPP                              60
     C*
     C                   Z-ADD     APPROD        XXPROD
     C                   EXSR      PRODNM
     C*
     C     HFLD          IFEQ      'SCPROD'
     C                   Z-ADD     APPROD        SCPROD
     C                   MOVEL     XPNAME        SCPNAM
     C                   MOVE      *ON           *IN86
     C                   ENDIF                                                   HFLD=SCPROD
     C*
     C     HFLD          IFEQ      'SCPRD2'
     C                   Z-ADD     APPROD        SCPRD2
     C                   MOVEL     XPNAME        SCPNM2
     C                   MOVE      *ON           *IN87
     C                   ENDIF                                                   HFLD=SCPRD2
     C***  BEGIN ADD  ***  01/22/10  *********************************
     C     HFLD          IFEQ      'SCPRD3'
     C                   Z-ADD     APPROD        SCPRD3
     C                   MOVEL     XPNAME        SCPNT3
     C**** BEG DEL 03/01/10 *******************************************
     C**                 MOVE      *ON           *IN87
     C**** END DEL 03/01/10 *******************************************
     C                   ENDIF                                                   HFLD=SCPRD2
     C***  BEGIN END  ***  01/22/10  *********************************
      *
     C     HFLD          IFEQ      'SCPRRC'
     C                   Z-ADD     APPROD        SCPRRC
     C                   MOVEL     XPNAME        SCPNM3
     C                   MOVE      *ON           *IN43
     C                   ENDIF                                                   HFLD=SCPRRC
      *
     C     HFLD          IFEQ      'SC2PRC'
     C                   Z-ADD     APPROD        SC2PRC
     C                   MOVEL     XPNAME        SCPNM4
     C                   MOVE      *ON           *IN44
     C                   ENDIF                                                   HFLD=SC2PRC
     C*
     C***  BEGIN ADD  ***  01/22/10  *********************************
     C     HFLD          IFEQ      'SC3PRC'
     C                   Z-ADD     APPROD        SC3PRC
     C                   MOVEL     XPNAME        SCPNM5
     C                   MOVE      *ON           *IN44
     C                   ENDIF                                                   HFLD=SC2PRC
     C***  BEGIN ADD  ***  01/22/10  *********************************
      *
     C                   ELSE
     C*
     C     HFLD          COMP      'SCPROD'                               73
     C     HFLD          COMP      'SCPRRC'                               83
     C     HFLD          COMP      'SCPRD2'                               76
     C     HFLD          COMP      'SC2PRC'                               87
     C*
     C                   ENDIF                                                  ##RRN<>*ZEROS
     C*
     C                   ENDIF                                                  *IN60=*ON
     C*
      *
      *  Agent of record search
      *
     C     HFLD          WHENEQ    'SCAGRC'
     C                   CALL      'W0040R'
     C                   PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      DBRRN1        ##RRN             9 0
     C                   PARM      2             ##VIEW            1 0
      *
     C     ##RRN         IFEQ      *ZERO
     C                   SETON                                        78
     C                   ELSE
     C     ##RRN         CHAIN     PAA02                              60
     C                   MOVE      AAAGT#        SCAGRC
     C                   Z-ADD     AAAGT#        XXAGT#
     C                   EXSR      AGTNM
     C                   MOVEL(P)  XANAME        SCANM3
     C                   ENDIF
      *
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  KJDO
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
     C   41              MOVE      @@QTE         QCQTE
     C  N41              MOVE      *BLANKS       QCQTE
     C*
     C*  CALL DARTS ENTRY PROGRAM
     C*
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM      'W0050R'      XXPGM
     C*
     C                   ENDSR                                                  KNDO
     C*
      /EJECT
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C   40              MOVE      'A'           AHSTAT
     C*
     C     *IN40         IFEQ      *OFF
     C                   Z-ADD     AHFYR         SCFYR
     C                   Z-ADD     AHEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C                   ENDIF
     C     *IN40         IFEQ      *ON
     C     AHKY1         CHAIN     PAH01                              60
     C                   CLEAR                   DBRCDF
     C                   ENDIF
     C                   Z-ADD     SCEFDT        SVEFDT
     C                   Z-ADD     SCEFDT        SVEFD2
     C                   Z-ADD     SCEFDT        SVEFD3
     C                   Z-ADD     SCEFDT        SVEFD4
     C*
     C                   Z-ADD     AHEFDT        XXEFDT
     C*
     C                   Z-ADD     AHAGT#        SCAGT#
     C                   Z-ADD     AHAGT#        SVAGT#
     C                   Z-ADD     AHAGT#        XXAGT#
     C                   EXSR      AGTNM
     C                   MOVEL     XANAME        SCAGNM
     C*
     C                   Z-ADD     AHPROD        SVPROD
     C*
     C                   Z-ADD     AHRT#         SCRT#
     C                   Z-ADD     AHRT#         XXRT#
     C                   EXSR      RATES
     C                   Z-ADD     XXNWRT        SCNWRT
     C                   Z-ADD     XXRNRT        SCRNRT
     C                   MOVEL(P)  XXPLAN        SCPLAN
     C                   MOVEL(P)  XXPLDS        SCPLDS
     C                   MOVEL(P)  SCPLAN        SVPLAN
     C*
     C     SCRT#         COMP      *ZERO                              5050
     C*
     C                   Z-ADD     AHPROD        SCPROD
     C                   Z-ADD     AHPROD        XXPROD
     C                   EXSR      PRODNM
     C                   MOVEL     XPNAME        SCPNAM
     C*
     C                   Z-ADD     AHSAGT        SCAGT2
     C                   Z-ADD     AH3AGT        SCAGT3
     C                   Z-ADD     AH3AGT        SVAGT3
     C                   Z-ADD     AH3PRD        SVPRD3
     C                   Z-ADD     AHSAGT        SVAGT2
     C                   Z-ADD     AHSPRD        SVPRD2
     C                   Z-ADD     AHAGRC        SVAGRC
     C                   Z-ADD     AH2ARC        SV2ARC
     C                   Z-ADD     AH3ARC        SV3ARC
     C                   Z-ADD     AHSAGT        XXAGT#
     C                   EXSR      AGTNM
     C                   MOVEL     XANAME        SCANM2
     C*
     C                   Z-ADD     AHSRT#        SCRT#2
     C                   Z-ADD     AHSRT#        XXRT#
     C                   EXSR      RATES
     C                   Z-ADD     XXNWRT        SCNWR2
     C                   Z-ADD     XXRNRT        SCRNR2
     C                   MOVEL(P)  XXPLAN        SCPLN2
     C                   MOVEL(P)  XXPLDS        SCPLD2
     C                   MOVEL(P)  SCPLN2        SVPLN2
     C                   MOVEL     AHCAF1        SCCAF1
     C                   MOVEL     AHCAF2        SCCAF2
     C                   MOVEL     AHCRF1        SCCRF1
     C                   MOVEL     AHCRF2        SCCRF2
     C*
     C     SCRT#2        COMP      *ZERO                              5151
     C*
     C                   Z-ADD     AHSPRD        SCPRD2
     C                   Z-ADD     AHSPRD        XXPROD
     C                   EXSR      PRODNM
     C                   MOVEL     XPNAME        SCPNM2
     C***  BEG   ADD  ***  01/28/99  *********************************
     C                   Z-ADD     AH3AGT        XXAGT#
     C                   EXSR      AGTNM
     C                   MOVEL     XANAME        SCANT3
     C                   Z-ADD     AH3RT#        SCRT#3
     C                   Z-ADD     AH3RT#        XXRT#
     C                   EXSR      RATES
     C                   Z-ADD     XXNWRT        SCNWT3
     C                   Z-ADD     XXRNRT        SCRNT3
     C                   MOVEL(P)  XXPLAN        SCPLT3
     C                   MOVEL(P)  XXPLDS        SCPDT3
     C                   MOVEL(P)  SCPLN2        SVPLN3
     C***  BEG   CHG  ***  03/01/10  *********************************
     C     SCRT#3        COMP      *ZERO                              5454
     C*          SCRT#3  COMP      *ZERO                5151
     C***  BEG   CHG  ***  03/10/10  *********************************
     C                   Z-ADD     AH3PRD        SCPRD3
     C                   Z-ADD     AH3PRD        XXPROD
     C                   EXSR      PRODNM
     C                   MOVEL     XPNAME        SCPNT3
     C*
      * On add, default screen status to and "A"
     C  N40              MOVE      AHSTAT        SCSTAT
     C   40              MOVE      'A'           SCSTAT
     C*
      *
      *  Agent of record
      *
     C                   MOVE      AHAGRC        SCAGRC
     C                   Z-ADD     AHAGRC        XXAGT#
     C                   EXSR      AGTNM
     C                   MOVEL(P)  XANAME        SCANM3
      *
      *  RATE OF  RECORD
      *
     C                   MOVE      AHRTRC        SCRTRC
     C                   Z-ADD     AHRTRC        XXRT#
     C                   EXSR      RATES
     C                   Z-ADD     XXNWRT        SCNWR3
     C                   Z-ADD     XXRNRT        SCRNR3
     C                   MOVEL(P)  XXPLAN        SCPLN3
     C                   MOVEL(P)  XXPLDS        SCPLD3
     C*
     C     SCRTRC        COMP      *ZERO                              5252
      *
      *  PRODUCER OF RECORD
      *
     C                   MOVE      AHPRRC        SCPRRC
     C                   Z-ADD     AHPRRC        XXPROD
     C                   EXSR      PRODNM
     C                   MOVEL     XPNAME        SCPNM3
     C*
     C*  SECONDARY AGENT OF RECORD
     C*
     C                   MOVE      AH2ARC        SC2ARC
     C                   Z-ADD     AH2ARC        XXAGT#
     C                   EXSR      AGTNM
     C                   MOVEL(P)  XANAME        SCANM4
     C*
     C*  RATE - SECONDARY AGENT OF RECORD
     C*
     C                   MOVE      AH2RRC        SC2RRC
     C                   Z-ADD     AH2RRC        XXRT#
     C                   EXSR      RATES
     C                   Z-ADD     XXNWRT        SCNWR4
     C                   Z-ADD     XXRNRT        SCRNR4
     C                   MOVEL(P)  XXPLAN        SCPLN4
     C                   MOVEL(P)  XXPLDS        SCPLD4
     C*
     C     SC2RRC        COMP      *ZERO                              5353
     C*
     C*  PRODUCER - SECONDARY AGENT OF RECORD
     C*
     C                   MOVE      AH2PRC        SC2PRC
     C                   Z-ADD     AH2PRC        XXPROD
     C                   EXSR      PRODNM
     C                   MOVEL     XPNAME        SCPNM4
     C***  BEGIN ADD  ***  01/22/10  *********************************
     C*
     C*  TERTIARY  AGENT OF RECORD
     C*
     C                   MOVE      AH3ARC        SC3ARC
     C                   Z-ADD     AH3ARC        XXAGT#
     C                   EXSR      AGTNM
     C                   MOVEL(P)  XANAME        SCANM5
     C*
     C*  RATE - TERTIARY  AGENT OF RECORD
     C*
     C                   MOVE      AH3RRC        SC3RRC
     C                   Z-ADD     AH3RRC        XXRT#
     C                   EXSR      RATES
     C***BEG CHG 03/01/10*******************************************
     C***                Z-ADD     XXNWRT        SCNWRT
     C***                Z-ADD     XXRNRT        SCRNRT
     C                   Z-ADD     XXNWRT        SC3NWR
     C                   Z-ADD     XXRNRT        SC3NWR
     C***END CHG 03/01/10******************************************
     C                   MOVEL(P)  XXPLAN        SCPLN5
     C                   MOVEL(P)  XXPLDS        SCPLD5
     C*
     C     SC3RRC        COMP      *ZERO                              5353
     C*
     C*  PRODUCER - TERTIARY  AGENT OF RECORD
     C*
     C                   MOVE      AH3PRC        SC3PRC
     C                   Z-ADD     AH3PRC        XXPROD
     C                   EXSR      PRODNM
     C                   MOVEL     XPNAME        SCPNM5
     C***  END   ADD   **  01/22/10  **************************************
     C*
     C*
     C                   Z-ADD     AHDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDT
     C                   MOVEL(P)  AHUSER        SCUSER
     C                   MOVEL(P)  AHPGM         SCPGM
     C                   Z-ADD     AHTIME        SCTIME
      *
     C                   Z-ADD     AHIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCIDT
     C                   MOVEL(P)  AHIUSR        SCIUSR
     C                   MOVEL     AHIPGM        SCIPGM
     C                   Z-ADD     AHITM         SCITM
      *
     C     *LIKE         DEFINE    SCRT#         S1RT#
     C     *LIKE         DEFINE    SCPROD        S1PROD
     C     *LIKE         DEFINE    SCAGT2        S1AGT2
     C     *LIKE         DEFINE    SCRT#2        S1RT#2
     C     *LIKE         DEFINE    SCPRD2        S1PRD2
      *** BEG ADD ***** 01/22/10 *************************************
     C     *LIKE         DEFINE    SCAGT3        S1AGT3
     C     *LIKE         DEFINE    SCRT#3        S1RT#3
     C     *LIKE         DEFINE    SCPRD3        S1PRD3
      *** END ADD ***** 01/22/10 *************************************
     C     *IN40         IFEQ      *OFF
     C                   Z-ADD     SCRT#         S1RT#
     C                   Z-ADD     SCPROD        S1PROD
     C                   Z-ADD     SCAGT2        S1AGT2
     C                   Z-ADD     SCRT#2        S1RT#2
     C                   Z-ADD     SCPRD2        S1PRD2
     C                   Z-ADD     SCAGT3        S1AGT3
     C                   Z-ADD     SCRT#3        S1RT#3
     C                   Z-ADD     SCPRD3        S1PRD3
     C                   ENDIF
     C*
     C                   ENDSR                                                  MVDBSC
     C*
      /EJECT
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C*
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     SCFYR         SVFYR
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AHEFDT
     C*
     C                   Z-ADD     SCAGT#        AHAGT#
     C                   Z-ADD     SCRT#         AHRT#
     C                   Z-ADD     SCPROD        AHPROD
     C                   Z-ADD     SCAGT2        AHSAGT
     C                   Z-ADD     SCRT#2        AHSRT#
     C                   Z-ADD     SCPRD2        AHSPRD
     C                   MOVE      SCSTAT        AHSTAT
     C                   MOVEL     Q1USER        AHUSER
     C                   MOVEL     Q1PGM         AHPGM
     C***  BEGIN ADD  ***  01/22/10  *********************************
     C                   Z-ADD     SCAGT3        AH3AGT
     C                   Z-ADD     SCRT#3        AH3RT#
     C                   Z-ADD     SCPRD3        AH3PRD
     C***  END   ADD  ***  01/22/10  *********************************
     C                   MOVEL     SCCAF1        AHCAF1
     C                   MOVEL     SCCAF2        AHCAF2
     C                   MOVEL     SCCRF1        AHCRF1
     C                   MOVEL     SCCRF2        AHCRF2
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AHDT
     C*
     C                   MOVEL     DSTIME        AHTIME
     C*
     C                   MOVE      SCAGRC        AHAGRC
     C                   MOVE      SCRTRC        AHRTRC
     C                   MOVE      SCPRRC        AHPRRC
     C*
     C                   Z-ADD     SC2ARC        AH2ARC
     C                   Z-ADD     SC2RRC        AH2RRC
     C                   Z-ADD     SC2PRC        AH2PRC
     C***  BEGIN ADD  ***  01/22/10  *********************************
     C                   Z-ADD     SC3ARC        AH3ARC
     C                   Z-ADD     SC3RRC        AH3RRC
     C                   Z-ADD     SC3PRC        AH3PRC
     C***  END   ADD  ***  01/22/10  *********************************
     C*
     C                   ENDSR                                                  MVSCDB
     C*
      /EJECT
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C*
     C     OTHERR        BEGSR
     C                   ENDSR
     C*
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C*
     C     OTHFKS        BEGSR
     C*
     C     *INKI         CASEQ     *ON           KIDO                     90
     C     *INKJ         CASEQ     *ON           KJDO                     90
      ******* BEGIN ADD ***** 04/03/98 ********************************
     C     *INKN         CASEQ     *ON           KNDO                     90
      ******* END   ADD ***** 04/03/98 ********************************
     C                   ENDCS
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C*
     C     PGMEND        BEGSR
     C*
     C                   EXSR      GETAGT
     C                   Z-ADD     ##AGT#        XXAGT1
     C  NKC
     CANNKL              EXSR      QUOTE
     C  NKC
     CANNKL              EXSR      BILLNG
     C                   DO        2             P
     C                   EXSR      DARTS
     C                   ENDDO                                                  P->2
     C                   MOVE      *ON           *INLR
     C                   ENDSR                                                  PGMEND
      /EJECT
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C                   MOVE      *OFF          NOTDSP
     C                   MOVE      *OFF          NOTDSQ
     C                   CLEAR                   SVFYR
     C*
     C     @@MODE        COMP      '1'                                    09    INQUIRY
     C     @@MODE        COMP      '2'                                    09    INQUIRY
     C     @@MODE        COMP      '3'                                    09    INQUIRY
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C     @@QTE         COMP      'Q'                                    41    QUOTE
     C*
     C                   CLEAR                   WCFL15
     C     'WC'          CHAIN     SWC01                              60
     C     WCFL15        COMP      'Y'                                    96
     C*
     C     *LIKE         DEFINE    ELRQFL        XXRQFL
     C                   CLEAR                   XXRQFL
     C*
     C                   MOVE      *OFF          *IN42
     C*
     C     Q1PRMS        IFGE      7
     C*
     C     @@CALL        IFEQ      'Y'
     C     @@RRN2        CHAIN     WDELP                              60
     C     EMKY          CHAIN     PEM01                              60
     C                   Z-ADD     ELFYR         SCFYR
     C                   MOVE      *ON           *IN42
     C                   ENDIF                                                  @@CALL=Y
     C*
     C                   ENDIF                                                  Q1PRMS>=7
     C*
     C*  GET EMPLOYER INFORMATION
     C*
     C  N42@@RRN2        CHAIN     WMEMP                              60
     C*
     C                   MOVE      EMRQFL        XXRQFL
     C*
     C*
     C                   MOVE      '0'           XXFLG             1
     C                   MOVE      0             XXDIV1
     C                   MOVE      ' '           XXQTYP
     C                   MOVE      'Q'           XXSTT1
     C*
     C     EMKY1         SETLL     PEM03
     C     EMKY1         READE     PEM03                                  55
     C     *IN55         DOWEQ     *OFF
     c*** begin chg ***  06/07/10  ******************************************
     C*****EMSTR1        IFEQ      'Y'
     C*****              Z-ADD     EMDIV1        XXDIV1
     C     E03EMSTRP     IFEQ      'Y'
     C                   Z-ADD     E03EMDIV      XXDIV1
     c*** end chg *****  06/07/10  ******************************************
     C                   MOVE      '1'           XXFLG
     C                   LEAVE
     C                   ENDIF
     C     EMKY1         READE     PEM03                                  55
     C                   ENDDO
     C*
     C     XXFLG         IFEQ      '1'
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE            7 0
     C*
     C                   CALL      'WA002R'                             99
     C                   PARM                    ELCO#
     C                   PARM                    ELFND
     C                   PARM                    ELEMP#
     C                   PARM                    XXDIV1
     C                   PARM                    ELFYR
     C                   PARM                    ##AGT#
     C                   PARM                    ##ANAM           60
     C*
     C*
     C     QTKY4         SETGT     PQT01
     C     QTKY3         READPE(N) PQT01                                  56
     C     *IN56         IFEQ      *OFF
     C                   MOVE      QTTYPE        XXQTYP
     C                   ENDIF
     C                   ENDIF
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
     C                   Z-ADD     EMEMP#        SCEMP#
     C                   Z-ADD     EMDIV         SCDIV
      *
      * Remebers if this entry was a division added through QUOTES?
      *
     C                   MOVEL     EMRQFL        XXRQFL
     C                   MOVEL     EMRDFL        XXRDFL            1
     C*  GET GROUP INFORMATION
     C*
     C                   Z-ADD     EMFND         SCFND
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVEL     FNNAME        SCNAME
     C*
     C* GET ALLOW AGENT 2 FLAG
     C*
     C                   SETOFF                                       31
     C                   SETOFF                                       49
     C*
     C                   CLEAR                   EXPRSL
     C                   CLEAR                   EXPRST
     C     EXKY          CHAIN     PEX01                              99
     C*
     C  N99EXA2FL        IFEQ      'Y'
     C*
     C                   SETON                                        31
     C*
     C                   ENDIF                                                  EXA2FL EQ 'Y'
     C                   SETON                                        64
     C  N99EX3AGT        IFEQ      'Y'
     C*
     C                   SETOFF                                       64
     C                   SETON                                        69
     C                   SETON                                        49
     C*
     C                   ENDIF                                                  EX3AGT EQ 'Y'
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT
      *
      * DO NOT ALLOW UPDATE IF COMMISSION RECORD EXISTS IN PRIOR MONTH OR
      * IF COMMISSION HAS BEEN PAID IN THE CURRENT MONTH. ANY CHANGES
      * TO AGENT HISTORY WILL HAVE TO BE ACCOMPLISHED IN INACTIVATING
      * THIS RECORD AND WRITING ANOTHER
      *
     C                   MOVE      'N'           PRCOMM            1
     C     *IN40         IFEQ      *OFF
     C     @@RRN         CHAIN(N)  WMAHP                              60
     C     ACKY3         SETLL     WMACL
     C     ACKY3         READE     WMACL                                  60
     C     *IN60         DOWEQ     *OFF
     C     XYYYMM        IFLT      SYYYMM
     C                   MOVE      'Y'           PRCOMM
     C                   LEAVE
     C                   ENDIF
     C     ACCOTP        IFEQ      'P'
     C     XYYYMM        ANDEQ     SYYYMM
     C                   MOVE      'Y'           PRCOMM
     C                   LEAVE
     C                   ENDIF
     C     ACKY3         READE     WMACL                                  60
     C                   ENDDO
     C                   ENDIF
      *
      * Default eff date
      *
     C     *IN40         IFEQ      *ON
     C     Q1PRMS        ANDGE     7
     C     @@CALL        ANDEQ     'Y'
     C     ELKY          CHAIN     PAH01                              60
     C     *IN60         IFEQ      *ON
     C     ELKY          CHAIN     PEL02                              60
     C     ELPFDT        IFLT      SYSDT
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C                   ELSE
     C                   Z-ADD     DSDATE        SCEFDT
     C                   ENDIF                                                  ELPFDT<SYSDT
     C                   ELSE
     C                   Z-ADD     DSDATE        SCEFDT
     C                   ENDIF                                                  *IN60=*ON
     C                   ENDIF                                                  *IN40=*ON
     C*
     C                   SETOFF                                       505152
     C                   SETOFF                                       53
     C**** BEG ADD 03/01/10 *******************************************
     C                   SETOFF                                         54
     C**** END ADD 03/01/10 *******************************************
     C*
     C     @@QTE         IFEQ      'Q'
     C                   MOVE      'Q'           XXSTAT
     C                   ELSE
     C                   MOVE      'W'           XXSTAT
     C                   ENDIF                                                   @@QTE=Q
     C*
      *
      *  Get Employer Cross Reference info for header
      *
     C     ZEXKY         CHAIN     PEX02                              63
     C  N63              Z-ADD     EXOFND        SCOFND
     C  N63              Z-ADD     EXOEMP        SCOEMP
     C  N63              Z-ADD     EXODIV        SCODIV
      *
     C*
     C                   EXSR      CHKLVL
     C* IF ADD CLEAR SCREEN FIELDS BEFORE DISPLAY ADD SCREEN
     C     *IN40         IFEQ      *ON
     C                   ENDIF                                                  *IN40=*ON
      ***** END ADD ***** 07/05/07 ***************************************
     C*
     C                   ENDSR                                                  PGMSTR
     C*
      /EJECT
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C*
     C     PREOTH        BEGSR
     C*
     C                   SELECT
     C*
     C     @@MODE        WHENEQ    'S'
     C                   EXSR      CHGSTS
     C                   RETURN
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  PREOTH
      *
      *
      *
      *================================================================
      * PRLIC - Determine if Agt Producer is licensed for emp rating st
      *================================================================
     C     PRLIC         BEGSR
      *
     C     EXPRSL        IFNE      *ZERO                                        FILE LEVEL
     C     EXPRST        ORNE      *BLANK                                       FILE TYPE
     C                   MOVE      'N'           XXLIC             1
     C                   SETOFF                                       60
      *
      * Get Employer Detail record
      *
     C     *IN42         IFEQ      *OFF
     C     ELKY          CHAIN     PEL02                              60
     C                   ENDIF
      *
     C     *IN60         IFEQ      *OFF
      *
      * Get employer rating state
      *
     C     RSKY          CHAIN     PRS01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
      * Find if producer licensed for emp rating state
      *
     C     APLFLG        IFEQ      'Y'
     C     APRST         ANDEQ     RSJRST
     C     APSTAT        ANDEQ     'A'
     C                   MOVE      'Y'           XXLIC
     C                   ENDIF
      *
     C     XXLIC         IFEQ      'N'
     C     PSKY          CHAIN     PPS01                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     PSLFLG        IFEQ      'Y'
     C     PSST          ANDEQ     RSJRST
     C     PSSTAT        ANDEQ     'A'
     C                   MOVE      'Y'           XXLIC
     C                   LEAVE
     C                   ENDIF
      *
     C     PSKY          READE     PPS01                                  60
     C                   ENDDO
      *
     C                   ENDIF                                                  XXLIC = N
      *
     C                   ENDIF                                                  RSKY
      *
     C                   ENDIF                                                  ELKY
      *
      *  Check for user override
      *
     C     XXLIC         IFEQ      'N'
      *
     C                   CALL      'SA006R'
     C                   PARM      ##LVL         #1ULVL            3 0          USER LEVEL
     C                   PARM      ##TYPE        #1UTYP            1            USER TYPE
     C                   PARM      EXPRSL        #1FLVL            3 0          FILE LEVEL
     C                   PARM      EXPRST        #1FTYP            1            FILE TYPE
     C                   PARM      *BLANK        #1RTCD            1            RETURN CODE
      *
     C     #1RTCD        IFNE      'F'
     C                   Z-ADD     APPROD        SC#PRD
     C                   MOVE      RSJRST        SC#ST
     C                   CLEAR                   SCLIC
     C                   EXFMT     WDW01
      *
     C     SCLIC         IFEQ      'Y'
     C                   MOVE      SCLIC         XXLIC
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ENDIF                                                  XXLIC = N
      *
      *
     C                   ELSE
      *
     C                   MOVE      'Y'           XXLIC             1
     C                   ENDIF                                                  prsl prst
      *
     C                   ENDSR                                                  PRLIC
      *
      *
      *
     C*
     C*================================================================
     C* PRODNM - Get producer name
     C*================================================================
     C*
     C     PRODNM        BEGSR
     C*
     C                   MOVEL     *BLANKS       XPNAME           40
     C     AAKY          CHAIN     PAA01                              60
     C  N60APKY          CHAIN     PAP01                              60
     C  N60APFSN         IFEQ      *BLANKS
     C*
     C                   MOVEL     APLSN         XPNAME
     C*
     C                   ELSE
     C*
     C**** BEG ADD 03/01/10 *******************************************
     C                   MOVEL     *BLANKS       QQLAST           56
     C                   MOVEL     *BLANKS       QQFRST           15
     C**** END ADD 03/01/10 *******************************************
     C**** BEG CHG 03/01/10 *******************************************
     C****               MOVEL     APLSN         QQLAST           56
     C****               MOVEL     APFSN         QQFRST           15
     C                   MOVEL     APLSN         QQLAST
     C                   MOVEL     APFSN         QQFRST
     C**** END CHG 03/01/10 *******************************************
     C*
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM      '1'           QQTERM            1
     C*
     C                   MOVEL     QQLAST        XPNAME
     C*
     C                   ENDIF                                                  APFSN=*BLANKS
     C*
     C                   ENDSR                                                  PRODNM
     C*
     C*================================================================
     C* QUOTE - Add record to quote file if it does not exist
     C*================================================================
     C*
     C     QUOTE         BEGSR
     C*
     C*  IF IT DOESN'T ALREADY EXIST CREATE AN AGENT QUOTE RECORD
     C*
     C                   MOVE      SCFND         QQFND             3 0
     C*
     C     QTKY1         CHAIN(N)  PQT01                              60        CHAIN W/ AGT# TOO
     C*
     C                   EXSR      GETAGT
     C                   Z-ADD     ##AGT#        XXAGT2
     C*
     C   60SCAGT#        IFNE      XXAGT2
     C                   MOVE      *OFF          *IN60
     C                   ENDIF                                                  SCAGT#<>XXAGT2
     C*
     C     XXAGT1        IFNE      XXAGT2
     C     SCAGT#        ANDEQ     XXAGT2
     C     *IN40         ANDEQ     *ON                                          ADD MODE
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  XXAGT1<>XXAGT2=SCAGT
     C*
     C     *IN60         IFEQ      *ON
     C     *IN09         ANDEQ     *OFF                                         NOT INQUIRY
     C*
     C                   EXSR      EMPAGT
     C*
     C     OLDAGT        IFEQ      'N'
     C*
     C                   CLEAR                   PQT01
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     EMCO#         QTCO#
     C                   Z-ADD     EMFND         QTFND
     C                   Z-ADD     EMEMP#        QTEMP#
     C                   Z-ADD     EMDIV         QTDIV
     C                   Z-ADD     SCAGT#        QTAGT#
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         QTWKDT
     C                   MOVEL     SRDTO         QTORDT
     C*
     C                   Z-ADD     EMEFD1        QTEFDT
     C                   Z-ADD     EMCHAP        QTCHAP
     C                   MOVEL     EMNAME        QTNAME
     C*
     C     @@QTE         IFEQ      'Q'
     C                   SELECT
     C     XXRQFL        WHENEQ    'Y'
      *** Begin change *** 09/18/13 *********************************
     C****               MOVE      'F'           QTTYPE
      /free
         QTTYPE = 'p';
      /End-free
      *** End change   *** 09/18/13 *********************************
     C                   OTHER
     C     XXQTYP        IFNE      ' '
     C                   MOVE      XXQTYP        QTTYPE
     C                   ELSE
     C                   MOVE      'P'           QTTYPE
     C                   ENDIF
     C                   ENDSL
      *
     C                   ELSE
     C                   MOVE      'W'           QTTYPE
     C                   MOVEL     SRDTO         QTNBDT
     C                   ENDIF                                                  @@QTE=Q
     C*
     C                   Z-ADD     *ZEROS        QTPREM
     C                   MOVE      *BLANKS       QTCARR
     C                   MOVE      *BLANKS       QTCOMM
     C*
     C                   TIME                    DSDTTM
     C*
     C     @@QTE         IFEQ      'Q'
     C                   MOVE      'Q'           QTSTAT
     C                   ELSE
     C                   MOVE      'W'           QTSTAT
     C                   ENDIF                                                   @@QTE=Q
     C*
     C*
     C                   MOVEL(P)  Q1USER        QTUSER
     C                   MOVEL(P)  Q1USER        QTIUSR
     C                   MOVEL(P)  Q1PGM         QTPGM
     C                   MOVEL(P)  Q1PGM         QTIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        QTTIME
     C                   Z-ADD     DSTIME        QTITM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         QTDT
     C                   MOVEL     SRDTO         QTIDT
      *
     C                   WRITE     PQT01
     C*
     C                   ENDIF                                                  OLDAGT=N
     C*
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   ENDSR                                                  QUOTE
     C*
     C*================================================================
     C* RATES - Get new and renew rates for rate number
     C*================================================================
     C*
     C     RATES         BEGSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C                   Z-ADD     *ZEROS        XXNWRT
     C                   Z-ADD     *ZEROS        XXRNRT
     C                   CLEAR                   XXPLAN
     C                   CLEAR                   XXPLDS
     C*
     C     ARKY          SETGT     PAR01
     C     ARKY1         READPE    PAR01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C     ARSTAT        ANDEQ     'I'
     C     ARKY1         READPE    PAR01                                  60
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     ARNWRT        XXNWRT
     C                   Z-ADD     ARRNRT        XXRNRT
     C                   MOVEL(P)  ARPLAN        XXPLAN
     C*
     C                   Z-ADD     1             X                 3 0
     C     XXPLAN        LOOKUP    PL(X)                                  60
     C   60              MOVEL(P)  PLD(X)        XXPLDS
     C*
     C                   ENDIF                                                  *IN60=*OFF
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
     C*
     C                   EXSR      MVSCDB
     C*
     C                   UPDATE    DBRCDF
     C*
E01  C                   ENDIF                                                  *IN99=*OFF
     C*
     C                   EXSR      QUOTE
     C*
     C                   ENDSR
     C*
     C***  BEGIN ADD  ***  04/10/98  *********************************
     C*================================================================
     C* WRITEQ - Write records to the WAQTP file
     C*================================================================
     C*
     C     WRITEQ        BEGSR
     C*
     C                   MOVEL(P)  Q1USER        QTUSER
     C                   MOVEL(P)  Q1USER        QTIUSR
     C                   MOVEL(P)  Q1PGM         QTPGM
     C                   MOVEL(P)  Q1PGM         QTIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        QTTIME
     C                   Z-ADD     DSTIME        QTITM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         QTDT
     C                   MOVEL     SRDTO         QTIDT
      *
     C                   MOVEL     SRDTO         QTWKDT
     C                   WRITE     PQT01
     C*
     C                   ENDSR                                                  WRITEQ
     C*
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C*
     C     WRITER        BEGSR
     C*
     C                   EXSR      GETAGT
     C                   Z-ADD     ##AGT#        XXAGT1
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   Z-ADD     EMCO#         AHCO#
     C                   Z-ADD     EMFND         AHFND
     C                   Z-ADD     EMEMP#        AHEMP#
     C                   Z-ADD     EMDIV         AHDIV
     C                   Z-ADD     SCFYR         AHFYR
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        AHIUSR
     C                   MOVEL     Q1PGM         AHIPGM
     C                   MOVEL     SRDTO         AHIDT
     C                   MOVEL     DSTIME        AHITM
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      WMAHP
     C*
     C                   EXSR      QUOTE
     C*
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDSR
     C*
     OPQT01     E            PQT01E
**
AAgency bill
DDirect bill (pre-paid)
PBased on A/R payments
**
The primary agent and producer are the secondary
agent and producer in another active record or the
secondary agent and producer are the primary agent
and producer on another active record.
An active record with the same effective date
already exists for this policy.
Commission records exist on the policy in a
prior month. The primary and secondary agent, rate,
and producer information cannot be changed. If
change is needed, inactivate this record and add
a new one with the desired information.
The Primary agent and producer are the secondary
agent and producer for another active record.
The Primary agent and producer are the Teriary
agent and producer for another active record.
The Secondary agent and producer are the primary
agent and producer for another active record.
The Secondary agent and producer are the Tertiary
agent and producer for another active record.
The Tertiary  agent and producer are the primary
agent and producer for another active record.
The Tertiary agent and producer are the Secondary
agent and producer for another active record.
Primary Agent information changed.
Secondary Agent information changed.
Tertiary Agent information is changed.
