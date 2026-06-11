     H Copyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0010R - WORK WITH GROUPS
     H*****************************************************************
     H*                                                               *
     H*  DESCRIPTION:  THIS PROGRAM WILL DISPLAY A LIST OF ALL GROUPS *
     H*                BY NAME OR NUMBER.  EACH GROUP CAN BE ACCESSED *
     H*                FOR UPDATE, DISPLAY OR SELECTION DEPENDING UPON*
     H*                THE CALLING PROGRAM.                           *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    MENU                                                       *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    W0010R02 - GROUP MASTER INFORMATION (OPT 2/5)              *
     H*    W0010R07 - GROUP AUXILIARY INFORMATION (OPT 7)             *
     H*    W0011R   - WORK WITH POLICY YEARS (OPT 8)                  *
     H*    W0520R   - REGION MAINTENANCE (OPT 9)                      *
     H*    W2185R   - INCREASED LIMITS MAINTENANCE (OPT 10)           *
     H*    W0220R   - CHAPTER MAINTENANCE (OPT 11)                    *
     H*    W0140R   - AGENT RATE MAINTENANCE (OPT 12)                 *
     H*    W0015R   - RESERVE CUSHION MAINTENANCE (OPT 13)            *
     H*    SC010R   - DARTS (OPT 14)                                  *
     H*    W2157R   - BILLING AUXILIARY MAINTENANCE (OPT 15)          *
     H*    W0049R   - ADMIN/DARTS (OPT 16)                            *
     H*    W0069R   - CLAIM/DARTS (OPT 17)                            *
     H*    W0013R   - EXPENSE CONSTANT DEFINITION MAINTENANCE (OPT 18)*
     H*    W0019R   - CX/REIN STATE DEFINITION (OPT 19)               *
     H*    W2420R   - BANK INFORMATION (OPT 20)                       *
     H*    W0021R   - HARBOR EXPOSURE (OPT 21)                        *
     H*    W0022R   - STATE COMPENSATION BENEFITS (OPT 22)            *
     H*    W0023R   - SHORT RATES (OPT 23)                            *
     H*    W8081R   - COMPENSATION PAYMENT CONTROL MAINTENANCE (OPT24)*
     H*    W0972R   - WW AUTO DARTS CONTROL(OPT 31)
     H*    W0973R   - WW AUTO CMS LETTERS  (OPT 32)
     F***  Beg Add  *****  06/27/18  *********************************
     H*    W1056R   - Midterm Endorsement forms    (OPT 53)           *
     F***  End Add  *****  06/27/18  *********************************
     H*    W0014R   - STATISTICAL REPORTING/RATING (OPT 54)           *
     H*    W0155R   - DEDUCTIBLE DISCOUNTS (OPT 55)                   *
     H*    W0054R   - ENDORSEMENT STAT INFO (OPT 56)                  *
     H*    W0010R80 - CLIENT CONTROL INFORMATION (OPT 80)             *
     H*    W0089R   - ENDORS/DARTS (OPT 57)                           *
     H*    W0045R   - PREMIUM FINANCE COMPANIES (OPT 45)              *
     H*    W0017R   - LOSS CONSTANT DEFINITIONS (OPT 59)
     H*    W1062R   - WORK WITH GROUP RATE SET CODE (OPT 62)
     H*                                                               *
     H*  CREATED: 12/16/96  PROGRAMMER: C:33  PROJECT: WC577          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*
     H* 03/12/20 C:291 Project: WC2608                                *
     H*                Call new Endorsement table setup program       *
     H*                WTGRPEND for option 52.                        *
     H*                                                               *
     H* 11/27/18 C:33  Project: SY406
     H*                Add Option 75 = Template Associations
     H*                                                               *
     H* 06/27/18 C:267 Project: WC2584                                *
     H*                Recompile only - added debug view *all         *
     H*                Note: program W1056R will now call             *
     H*                new Midterm Endorsement program WTGRPMEND.     *
     H*                                                               *
     H* 02/16/17 C:136 Support: 45752                                 *
     H*                Call Group Aux in add mode if no Group Aux     *
     H*                exists if not in inquiry mode                  *
     H*                                                               *
     H* 04/21/16 C:65  PROJECT: WC2427                                *
     H*                Remove CMS auto letter option.  These will be  *
     H*                handled by triggers in future.                 *
     H*                                                               *
     H* 09/01/15 C:279 Support: PHW31936                              *
     H*                Remove Audit Tracking (opt=41) from work with  *
     H*                groups screen as this option is now available  *
     H*                from menu.                                     *
     H*                                                               *
     H* 08/22/11 C:136 PROJECT: WC1918                                *
     H*                INHIBIT WC1918 FOR CLIENTS WITH MOD CODE       *
     H*                (065 & 093 AT THE MOMENT)                      *
     H*                                                               *
     H* 07/08/11 C: 53 Project: SY255
     H*                -Correct CMS flag
     H*                                                               *
     H* 02/25/11 C:65  PROJECT: 132WOJ/SUPPORT                        *
     H*                ADD SECURITY TO DISABLE ANY NEW OPTIONS THAT   *
     H*                ARE ADDED TO THE PROGRAM TO DISABLE THEM IF    *
     H*                THE OPTION PROGRAM DOES NOT EXIST.  THIS WILL  *
     H*                BE DONE BY ADDING THE RECORD FOR THE NEW OPTION*
     H*                TO A 2ND ARRAY THAT CONTAINS THE PROGRAM NAME. *
     H*                                                               *
     H* 02/02/11 C:235 PROJECT 114WXO                                 *
     H*                Claim Dept. Letter to Gather CMS Info.         *
     H*                Added option 32 = Auto CMS Letters.            *
     H*                                                               *
     H* 12/31/10 C:255 PROJECT: WC1918                                *
     H*                ADD NEW OPTION 65 FOR "POLICY PAGE 3C" TO      *
     H*                CALL W0010R65 PROGRAM.                         *
     H*                                                               *
     H* 12/30/10 Support 75013 :  Change parameter list in call of    *
     H*                           W0010R64.                           *
     H*                                                               *
     H* 11/10/10 C:33  Project: 139WPL                                *
     H*                Add opt 91 for HCN Program (CLT139 only)       *
     H*                * * Clean-up comment lines for OFD             *
     H*                                                               *
     H* 10/18/10 C:255 PROJECT: WC1939                                *
     H*                ADD NEW OPTION 47 FOR STATE RATE CHANGE TO  CA-*
     H*                LL W0047R PROGRAM.                             *
     H*                                                               *
     H* 09/29/10 C:180 SUPPORT 74189                                  *
     H*                Fix empty screen 05 - TEMPORARY ADD OF
     H*                OPTION 64 UNTIL SPECS FOR OPT 63 ARE RECEIVED.
     H*
     H*
     H* 08/17/10 C:61  Project: SY190 PRISS 66                        *
     H*                Change option to read "Group Contact Info".    *
     H*                                                               *
     H* 07/28/10 C:255 PROJECT: 132WOJ                                *
     H*                ADD NEW OPTION 64 FOR FLEXIBLE INFORMATION TO  *
     H*                CALL W0010R64 PROGRAM.                         *
     H*                                                               *
     H* 07/02/10 C:257 PROJECT: WC1790                                *
     H*                ADD NEW OPTION 29-EDI Submission info.         *
     H*                This option will call W0066R .                 *
     H*                                                               *
     H* 03/10/10 C:242 Project: 25WRK                                 *
     H*                Claims Diary Retrofit, from 25WKD and 25WJD    *
     H*                Add option 90=New clm diary DARTS rm           *
     H*                Call program W0069R90, TASB Claim Control      *
     H*                Maintenance                                    *
     H*                Add code to protect fields in Display only mode*
     H*                                                               *
     H* 01/25/10 C:232 PROJECT: WC1851                                *
     H*                ADD NEW OPTION 63-FEE INFORMATION. IF WCFL44   *
     H*                IS 'Y'. AND OPTION WILL CALL W0063R.           *
     H*                                                               *
     H* 11/13/09 C:136 PROJECT: WC1806                                *
     H*                DISABLE OPT 26 IF WC1806 IS INSTALLED          *
     H*                                                               *
     H* 06/26/09 C:198 Project: WC1694                                *
     H*                Added option 27 = Policy export.               *
     H*                                                               *
     H* 06/25/09 C:220 Project: 114WTR                                *
     H*                Interface with Zurich                          *
     H*                                                               *
     H* 03/28/08 C:65  PROJECT: CI019                                 *
     H*                ENABLE OPTION 26 FOR ACH IF ONLINE PROCESSING. *
     H*                                                               *
     H* 02/15/08 C:190 Project: WC1425                                *
     H*                Change the description for "Opt 56" from       *
     H*               "Endorsement Info" to "Endors/Policy Form Info. *
     H*                                                               *
     H* 01/24/08 C:189 Project: WC1621                                *
     H*                PID GROUP COPY PHASE -1                        *
     H*                                                               *
     H* 07/18/07 C:136 PROJECT: WC1491                                *
     H*                ADD OPTION TO CALL RETURN FUNDS                *
     H*                                                               *
     H*  08/28/06 C:180 PROJECT: 114WMF                               *
     H*                 - Add option 51: Internal Comm rates          *
     H*                                                               *
     H*  08/24/06 C:180 PROJECT: 114WMF                               *
     H*                 - Add option 62 work with group rate set codes*
     H*
     H*  03/16/06 C:168 SUPPORT: WA139:51238                          *
     H*               - option 30 is now based on run type (D/U)      *
     H*               - option 92 is now based on run type (D/U)      *
     H*                                                               *
     H*  06/12/06 C:168 PROJECT: 136WBG                               *
     H*               - option 27 - work with deposit                 *
     H*                                                               *
     H*  02/09/06 C:167 Project:  139WCD                              *
     H*                 WW/Groups:Add'l info - short name             *
     H*                                                               *
     H*  01/26/06 C:132 PROJECT:  wa135 - 50966                       *
     H*                 Change Call for Option 40 to load the ##Mode  *
     H*                 parm with the current status: Inquiry or Chg. *
     H*                                                               *
     H*  12/18/05 C:136 PROJECT: 136WBH                               *
     H*                 CLIENT SPECIFIC: ADD OPT 93 - COMMISSION MOD  *
     H*                 FACTOR                                        *
     H*                                                               *
     H*  03/05/05 C:132 Project: WA099  SEQ#: 45931                   *
     H*                Do not change to ADD mode if no record exists  *
     H*                for the group.                                  *
     H*                                                               *
     H*  2/14/05 C:58  PROJECT: WC1128 - Auto DARTS                   *
     H*                Add Opt 31=Call WW Auto DARTS Control          *
     H*                                                               *
     H* 09/07/04 C:132 PROJECT: WC1179 - Auto Cash Dissolve           *
     H*                Add Opt 61=Call Auto Cash Dissolve Program     *
     H*                                                               *
     H*                                                               *
     H* 02/10/04 C:65  PROJECT: 114WAK                                *
     H*                Add Opt 92=EXCLUDED STATES/WCPOLS              *
     H*                                                               *
     H* 11/13/03 C: 65 PROJECT: WC1167                                *
     H*                Add option 48=BILLING OPTIONS                  *
     H*                                                               *
     H* 04/22/03 C: 53 PROJECT: WC1121                                *
     H*                Add option 30=Auto Claim letters               *
     H*                                                               *
     H* 04/11/03 C:136 PROJECT: WC1116                                *
     H*                Add option 26=Direct debit control             *
     H*                                                               *
     H* 12/12/02 C:132 PROJECT: WC1064                                *
     H*                Add option 60=Installment fees.                *
     H*                Delete option 90=Installment fees.             *
     H*                                                               *
     H* 03/04/02 C:65  PROJECT: WC1064                                *
     H*                Add option 90=Installment fees.                *
     H*                                                               *
     H* 10/15/01 C:90  PROJECT: WC1054                                *
     H*                Add Opt 40 - Carrier Information.              *
     H*                                                               *
     H* 09/28/01 C:136 Project WC577                                  *
     H*                Add Opt 59 - Work With Loss Constants          *
     H*                                                               *
     H* 12/06/00 C:61  PROJECT WC577                                  *
     H*                Allow opt 25 in inquiry mode.                  *
     H*                                                               *
     H* 10/29/99 C:61  PROJECT WC958                                  *
     H*                Add option 46=MCO group code.                  *
     H*                                                               *
     H* 09/23/99 C:118 PROJECT: WC939                                 *
     H*                Added Option 45=Premium Finance Companies.     *
     H*                                                               *
     * 08/17/99 C:113 PROJECT: WC904                                 *
     *                Moved endorsement tables to options in Work    *
     *                With Groups since these are group specific:    *
     *                -Option 52=Policy Page Endorsement Maint.      *
     *                -Option 53=Midterm Endorsement Maint.          *
     H*                                                               *
     H*  06/02/99  C:61  PROJECT: 101AT                               *
     H*                - Modify OPT25 for Inquiry.                    *
     H*                - Add code for group restrictions.             *
     H*                                                               *
     H*  02/19/99 C:112 PROJECT: 25WJW                                *
     H*           Add code to protect fields in Display only mode     *
     H*                                                               *
     H*  10/01/98  C:87  PROJECT: WC762                               *
     H*                  ADD PROCESSING FOR OPTION 25=CX CONFIRMATION *
     H*                  CONTROL                                      *
     H*                                                               *
     H*  09/22/98  C:112 WORK WITH GROUPS (OPTION 2O OF W2420R THEN   *
     H*                  F17)  INITIALIZE APPLICATION FIELD TO BLANKS *
     H*                  SO FIELD DISPLAY ON 'SUBSET BANK             *
     H*                  INFORMATION' (SCR03) IS BLANK                *
     H*                                                               *
     H*  06/25/98  C:87  PROJECT: WC630                               *
     H*                  REMOVE SYSTEM CONTROL FLAG PROCESSING FOR    *
     H*                  OPTION 41=AUDIT TRACKING. IT IS A PART OF    *
     H*                  BASE PRODUCT, NO LONGER A SEPARATE MODULE.   *
     H*                                                               *
     H*  06/08/98  C:87  PROJECT: ???                                 *
     H*                  ADDED SUBROUTINE TO HANDLE CHANGING OPTION   *
     H*                  AND FUNCTION KEYS DISPLAYED ON SCREEN.       *
     H*                  ELIMINATES HARDCODING ARRAY.                 *
     H*                                                               *
     H*  05/26/98  C:87  PROJECT: WC773                               *
     H*                  IF CLIENT IS ADMIN ONLY THEN DISABLE         *
     H*                  OPTIONS 13, 17, 22, 24.                      *
     H*                  IF CLIENT IS CLAIMS ONLY THEN DISABLE        *
     H*                  OPTS 12, 15, 18, 19, 21, 23, 50, 54, 55, 56  *
     H*                                                               *
     H*  03/12/98  C:85  PROJECT: WC577                               *
     H*                  CHANGED THE SLMSGI CONSTANT FROM PRJ9998     *
     H*                  TO WC39998,AND THE SLMSGF CONST. FROM PJMESG *
     H*                  TO WMESGG.                                   *
     H*                                                               *
     H*  03/04/98  C:87  PROJECT: WC630                               *
     H*                  ADDED OPTION 41, PARM LIST AND PROCESSING    *
     H*                  AUDIT TRACKING CONTROL MAINTENANCE           *
     H*                  ADDED OPTION 58,                             *
     H*                  AUDIT TRACKING DARTS                         *
     H*                                                               *
     H*  01/28/98  C:57  PROJECT: CM027                               *
     H*                  ADDED OPTION 57,                             *
     H*                  ENDORSEMENT/DARTS CONTROL FILE MAINT.        *
     H*                                                               *
     H*  01/22/98  C:85  PROJECT: WC577                               *
     H*                  ADDED OPTION 24,                             *
     H*                  COMPENSATION PAYMENT CONTROL MAINTENANCE.    *
     H*                                                               *
     H*  01/20/98  C:33  PROJECT: WC577                               *
     H*                  DO NOT DISPLAY OPTION 80 (DUE TO 12/04 CHG)  *
     H*                                                               *
     H*  12/04/97  C:57  PROJECT: CM027                               *
     H*                  ADD OPT 55 ENDORSEMENT STAT INFO.            *
     H*                                                               *
     H*  11/12/97  C:33  PROJECT: WC577                               *
     H*                  DO NOT DISPLAY OPTION 80 IF NO HEADER RECORDS*
     H*                  ARE AVAIALBE FOR THE USER PARM CO#.          *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
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
     H*  20 - Number Order Search                                     *
     H* N20 - Alphabetic Order Search                                 *
     H*  30 - Called as a search                                      *
     H*  31 - Called as a search w/YR                                 *
     H*  40 - More Option Controller                                  *
     H*  41 - More Option Controller                                  *
     H*  71 - These indicators are to used as error indicators for    *
     H*       thru the various screens. They will set off after       *
     H*       displaying each input capible screen.                   *
     H*  89 - No update authorization for record.                     *
     H*  90 - Used to general purpose error indicator                 *
     H*  91 - Used to display CPF5203; roll up/down past last record  *
     H*  93 - Used to display CPF6A7C; Option invalid                 *
     H*  94 - Used to display CPF5204; Function Key invalid           *
     H*  95 - Used to condition SFLEND; also used to indicate EOF and *
     H*       BOF on the file being read.                             *
     H*                                                               *
     H*****************************************************************
      *
      *
     FW0010F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     FWMFNP     IF   E             DISK
     F                                     RENAME(PFN01:PFRCDF)
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PFN01:DBRCDF)
     FWMFNL2    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PFN01:DBRCDF2)
     FWDF2L     IF   E           K DISK
     F***  BEGIN ADD  ***  11/12/97  *********************************
     FWMHXL     IF   E           K DISK
     F***  END   ADD  ***  11/12/97  *********************************
     F***  BEGIN ADD  ***  07/02/10  *********************************
     FSWCPF     IF   E             DISK
     F                                     RENAME(SWC01:SWC02)
     F***  END   ADD  ***  07/02/10  *********************************
     F***  BEGIN ADD  ***  03/04/98  *********************************
     FSSYLF     IF   E           K DISK
     F***** BEG DEL ***** 01/24/08 ***************************************
     F****        SSY01                             KIGNORE
     F***** END DEL ***** 01/24/08 ***************************************
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
     F***  END   ADD  ***  03/04/98  *********************************
      /COPY SCOPYWW10D
     D*
     D HD              S             74    DIM(8) CTDATA PERRCD(1)              HEADERS
     D OF              S              1    DIM(70) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(70) ALT(OF)
     D*** BEGIN ADD ***  02/25/11  ************************************
     D OF2             S              1    DIM(70) CTDATA PERRCD(1)             Options, Function EX
     D OFE             S             12    DIM(70) ALT(OF2)
     D*** END ADD *****  02/25/11  ************************************
     D*
     D***PY SCOPYWW10E
     D*
     D***PY SCOPYWW10I
     D*
     D                 DS
     D  DATA                   1     35
     D  MF                    28     28
     D***  Begin add  ***  02/09/06  *********************************
     D*
     D SCRTDA          DS
     D  CHEFDT                 1     10
     D  FILLER                11     14
     D  CHEXDT                15     24
     D***  End   add  ***  02/09/06  *********************************
     D*
     D***  Begin add  ***  02/09/06  *********************************
     D WKEFDT          DS
     D  WKEFMM                 1      2
     D  WKEFDD                 3      4
     D  WKEFCY                 5      8
     D*
     D W2EFDT          DS
     D  W2EFMM                 1      2
     D  W2SLH1                 3      3
     D  W2EFDD                 4      5
     D  W2SLH2                 6      6
     D  W2EFCY                 7     10
     D*
     D WKEXDT          DS
     D  WKEXMM                 1      2
     D  WKEXDD                 3      4
     D  WKEXCY                 5      8
     D*
     D W2EXDT          DS
     D  W2EXMM                 1      2
     D  W2SLHA                 3      3
     D  W2EXDD                 4      5
     D  W2SLHB                 6      6
     D  W2EXCY                 7     10
     D*
     D***  End   add  ***  02/09/06  *********************************
     D OPTNM           C                   CONST('Groups')
     D DELETD          C                   CONST('*Deleted')
     D ACDPGM          C                   CONST('W0010R02')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D COMAPI          C                   CONST('WA001R')
     D CHGNUM          C                   CONST('WA003R')
     D USPAPI          C                   CONST('W0006R')
     D VIEWMX          C                   CONST(2)
     D***  Begin del  ***  02/09/06  *********************************
     D****          1                     C         INFOMX
     D***  End   del  ***  02/09/06  *********************************
     D***  Begin add  ***  02/09/06  *********************************
     D INFOMX          C                   CONST(2)
     D***  End   add  ***  02/09/06  *********************************
     D***  BEGIN ADD  ***  11/12/97  *********************************
     D CLRFKY          C                   CONST('                    -
     D                                            NNN')
     D***  END   ADD  ***  11/12/97  *********************************
     D*
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D*** BEGIN ADD *** 07/08/11 **************************************
     D                 DS
     D  SYFL12                 1     10
     D  XXCMS                  2      2

     I*** END   ADD *** 07/08/11 **************************************
      *** beg add ***** 02/16/17 ******************
     d GroupAux        pr                  extpgm('W0010R07')
     d  inkc                           n
     d  inkl                           n
     d  mode                          1    const
     d  rrnOfWmfnp                    9  0 const
      *** end add ***** 02/16/17 ******************
     D***  Begin Add  ***  11/27/18  *************************************
      /copy WITMPLTD
      /copy sPrModule
     D***  End   Add  ***  11/27/18  *************************************

     C*
     C     *LIKE         DEFINE    FNCO#         KKCO#
     C     *LIKE         DEFINE    FNFND         KKFND
     C     *LIKE         DEFINE    FNNAME        KKNAME
     C     *LIKE         DEFINE    FNNAME        U1PDSC
     C     *LIKE         DEFINE    FNNAME        U2PDSC
      ***** BEG ADD ******* 06/02/99 **********************************
     C     *LIKE         DEFINE    @@CALL        SVCALL
     C     *LIKE         DEFINE    *IN09         SVIN09
     C     *LIKE         DEFINE    FNFND         #@FND
      ***** END ADD ******* 06/02/99 **********************************
     C*
     C*** BEGIN ADD *** 06/08/98 *************************************
     C*  Define and init fields for CHGOFA subroutine
     C                   MOVE      *BLANKS       XXOPFN            1
     C                   MOVE      *BLANKS       XXNMBR            2
     C                   MOVE      *BLANKS       XXMIS             3
     C*** END   ADD *** 06/08/98 *************************************
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C*
     C     ACDPL1        PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##FYR             3 0
     C*
     C     ACDPL2        PLIST
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    ##MODE            1
     C                   PARM                    ##CO#
     C                   PARM                    SCFND
     C                   PARM                    ##KEY            10
     C*
     C     ACDPL3        PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM                    ##RRN             9 0
     C                   PARM      SCDBRR        ##RRN2            9 0
     C*
     C     ACDPL4        PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM                    ##RRN             9 0
     C                   PARM      SCDBRR        ##RRN2            9 0
     C                   PARM                    ##KEYS            3 0
     C*
     C     ACDPL5        PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM                    ##YR              3 0
      ***** BEG ADD ******* 10/09/01 **********************************
     C     ACDPL6        PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
      ***** END ADD ******* 10/09/01 **********************************
     C*
      ***** BEG ADD ******* 07/02/10 **********************************
     C     ACDPL7        PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##CALL
     C                   PARM      'X'           ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
      ***** END ADD ******* 07/02/10 **********************************
     C     ACDPLS        PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      '  '          ##ST              2
     C*
     C***  BEGIN ADD  ***  03/04/98  *********************************
     C     ADTRPL        PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      *ZEROS        ##RRN2            9 0
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C***  END   ADD  ***  03/04/98  *********************************
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
     C*
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
      ***** BEG ADD ******* 06/02/99 **********************************
      *
     C     RSTPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM      Q1USER        ##USR            10
     C                   PARM      'W'           ##APPL            1
     C                   PARM      *OFF          ##RCDS            1
     C*
     C     RSTPL1        PLIST
     C                   PARM                    ##CO#
     C                   PARM      Q1USER        ##USR
     C                   PARM      'W'           ##APPL
     C                   PARM      *OFF          ##RTCD            1
     C     SCAUTH        PARM      *BLANK        ##AUTH            1
     C                   PARM      FNFND         #@FND
      *
      ***** END ADD ******* 06/02/99 **********************************
     *** BEGIN ADD *** 08/17/99 **************************************
     C     PLPPE         PLIST                                                  Policy Page Endorsem
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     *** END   ADD *** 08/17/99 **************************************
     E***** BEG ADD ***** 02/14/05 ***************************************
     C     PLAD          PLIST                                                  Auto DARTS  Endorsem
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      FNFND         #@FND
     E***** END ADD ***** 02/14/05 ***************************************
     C*
     C*  GROUP AUXILIARY
     C*
     C     F2KY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C*
     C*  REPOSITION KEYS
     C*
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C     KEY1B         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKNAME
     C*
     C     KEY1C         KLIST
     C                   KFLD                    KKCO#
     C***  BEGIN ADD  ***  03/04/98  *********************************
     C*
     C*   KEY LIST FOR THE SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C***  END   ADD  ***  03/04/98  *********************************
      ***** BEG ADD ******* 06/02/99 **********************************
      *
     C                   MOVE      'Y'           XX1ST             1
      ***** END ADD ******* 06/02/99 **********************************
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM                    @@VIEW            1 0
     C*
     C/COPY SCOPYWW10C
     C*
      *
      *
     C*** BEGIN ADD *** 06/08/98 *************************************
     C*===============================================================
     C* CHGOFA - SUBROUTINE TO CHANGE OPTION/FUNCTION ARRAY
     C*===============================================================
     C*  Before calling this subroutine make the following assignments:
     C*       XXOPFN = '1' if Option, '2' if Function
     C*       XXNMBR = Option/Function number to change
     C*            NOTE: Pass option/function numbers as 2 char fields.
     C*                  e.g. MOVE '05' XXNMBR or MOVE '20' XXNMBR
     C*       XXMIS = 'XXX' where each 'X' is Y or N depending on change rqrd
     C*
     C*  This subroutine when called evaluates array OF for XXOPFN and checks
     C*  OFD to see if it is the option/function that matches XXNMBR. If so, it
     C*  replaces 'XXX' with XXMIS.
     C*
     C     CHGOFA        BEGSR
     C*
     C                   MOVE      *OFF          FOUND             1
     C                   Z-ADD     1             I                 2 0
     C*
     C*  Go through array looking for a match
     C     FOUND         DOWEQ     *OFF
     C     OF(I)         ANDNE     '3'
     C*
     C     OF(I)         IFEQ      XXOPFN
     C*
     C                   MOVEL     OFD(I)        DUMNUM            2
     C*
     C     XXNMBR        IFEQ      DUMNUM
     C*
     C                   MOVEL(P)  OFD(I)        DUMOFD           30
     C                   MOVE      XXMIS         DUMOFD
     C                   MOVEL     DUMOFD        OFD(I)
     C                   MOVE      *ON           FOUND
     C*
     C                   ENDIF                                                  ZZNUM=DUMNUM
     C*
     C                   ENDIF                                                  OF,I=XXOPFN
     C*
     C                   ADD       1             I                              INCREMENT COUNT
     C*
     C                   ENDDO                                                  FOUND DOWEQ *OFF
     C*
     C                   ENDSR                                                  CHGOFA
     C*** END   ADD *** 06/08/98 *************************************
      *
      *
     C*===============================================================
     C* CHKSSM - SUBROUTINE FOR SUBSET MESSAGE
     C*===============================================================
     C*
     C     CHKSSM        BEGSR
     C*
     C*
     C                   ENDSR                                                  CHKSSM
     C*
      *
      *
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
     C* ERRCK1 - SUBROUTINE TO CHECK SCREEN SELECTIONS
     C*===============================================================
     C*
     C     ERRCK1        BEGSR
      *
      ***** BEG ADD ******* 06/02/99 **********************************
      *
     C                   SELECT
      *_______________________________________________________________
      *
     C     SCSEL         WHENEQ    '2'
     C     SCAUTH        ANDEQ     'I'
     C     ##RCDS        ANDEQ     *ON
     C                   SETON                                        89  90
      *
      *_______________________________________________________________
     C                   ENDSL
      ***** END ADD ******* 06/02/99 **********************************
      *
     C                   ENDSR                                                  ERRCK1
      *
      *
      *
     C*===============================================================
     C* ERRCK2 - SUBROUTINE TO CHECK SUBFILE SELECTIONS
     C*===============================================================
     C*
     C     ERRCK2        BEGSR
     C                   ENDSR                                                  ERRCK2
     C*
      *
      *
     C*===============================================================
     C* INZSST - SUBROUTINE TO INITIALIZE SUBSET SELECTIONS
     C*===============================================================
     C*
     C     INZSST        BEGSR
     C                   ENDSR                                                  INZSST
     C*
      *
      *
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
     C                   MOVE      *OFF          ##RCDS
     C                   ENDIF                                                  @@CALL
      *
     C                   ENDIF                                                  XX1ST
      *
      ***** END ADD ******* 06/02/99 **********************************
     C                   MOVE      *LOVAL        KKFND
     C                   MOVE      *LOVAL        KKNAME
     C*
     C     ##XFND        IFEQ      'Y'
      ***** BEG ADD ******* 06/02/99 **********************************
     C     ##RCDS        ANDEQ     *OFF
      ***** END ADD ******* 06/02/99 **********************************
     C                   MOVE      ##FND         KKFND
     C                   MOVEL(P)  ##FNAM        KKNAME
     C                   ENDIF
     C*
     C                   ENDSR                                                  KEYINZ
     C*
      *
      *
     C*===============================================================
     C* KEYSET - SUBROUTINE TO SET THE REPOSITION KEYS
     C*===============================================================
     C*
     C     KEYSET        BEGSR
     C   21              MOVE      SCPST1        KKFND
     C   22              MOVEL     SCPST2        KKNAME
     C                   ENDSR                                                  KEYSET
     C*
      *
      *
     C*===============================================================
     C* KEYSFL - SUBROUTINE TO SET THE REPOSITION KEYS (USER DEFINED)
     C*===============================================================
     C*
     C     KEYSFL        BEGSR
     C                   MOVE      SCFND         KKFND
     C                   MOVEL     SCNAME        KKNAME
     C                   ENDSR                                                  KEYSFL
     C*
      *
      *
     C*===============================================================
     C* MVDBSC - SUBROUTINE TO LOAD SUBFILE FIELDS
     C*===============================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C                   Z-ADD     FNFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
     C                   MOVEL(P)  FNNAME        XXDATA           41
     C*
     C                   MOVEL     FNFND         XXNUM
     C                   MOVEL     *BLANKS       EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXFND             3
     C*
     C                   SELECT
B01  C     VIEW#         WHENEQ    1
     C                   MOVEL(P)  XXFND         SCDATA
     C                   MOVE      XXDATA        SCDATA
X01  C     VIEW#         WHENEQ    2
     C                   MOVEL(P)  XXDATA        SCDATA
     C                   MOVE      XXFND         SCDATA
E01  C                   ENDSL
     C*
     C***  Begin add  ***  02/09/06  *********************************
     C                   SELECT
     C     INFO#         WHENEQ    1
     C***  End   add  ***  02/09/06  *********************************
     C                   Z-ADD     FNEFDT        SRDTI
     C                   EXSR      CVTDT
     C***  Begin del  ***  02/09/06  *********************************
     C*****                MOVELSRDTO     SCEFDT
     C***  End   del  ***  02/09/06  *********************************
     C***  Begin add  ***  02/09/06  *********************************
     C                   MOVEL     SRDTO         WKEFDT
     C                   MOVE      WKEFMM        W2EFMM
     C                   MOVE      WKEFDD        W2EFDD
     C                   MOVE      WKEFCY        W2EFCY
     C     FNEFDT        IFEQ      0
     C                   MOVE      ' '           W2SLH1
     C                   MOVE      ' '           W2SLH2
     C                   ELSE
     C                   MOVE      '/'           W2SLH1
     C                   MOVE      '/'           W2SLH2
     C                   ENDIF
     C                   MOVEL     W2EFDT        CHEFDT
     C***  End   add  ***  02/09/06  *********************************
     C*
     C                   Z-ADD     FNEXDT        SRDTI
     C                   EXSR      CVTDT
     C***  Begin del  ***  02/09/06  *********************************
     C*****                MOVELSRDTO     SCEXDT
     C***  End   del  ***  02/09/06  *********************************
     C***  Begin add  ***  02/09/06  *********************************
     C                   MOVEL     SRDTO         WKEXDT
     C                   MOVE      WKEXMM        W2EXMM
     C                   MOVE      WKEXDD        W2EXDD
     C                   MOVE      WKEXCY        W2EXCY
     C     FNEXDT        IFEQ      0
     C                   MOVE      ' '           W2SLHA
     C                   MOVE      ' '           W2SLHB
     C                   ELSE
     C                   MOVE      '/'           W2SLHA
     C                   MOVE      '/'           W2SLHB
     C                   ENDIF
     C                   MOVEL     W2EXDT        CHEXDT
     C     INFO#         WHENEQ    2
     C                   MOVEL     FNSHNM        SCRTDA
     C                   ENDSL
     C***  End   add  ***  02/09/06  *********************************
     C*
     C                   ENDSR                                                  MVDBSC
     C*
      *
      *
     C*===============================================================
     C* OPT7 - SUBROUTINE FOR OPTION 7
     C*===============================================================
     C*
     C* OPTION 7=AUXILIARY DETAIL
     C*
     C     OPT7          BEGSR
     C*
     C  N09              MOVE      'C'           ##MODE
     C   09              MOVE      'D'           ##MODE
     C*
      *** beg chg ***** 02/16/17 *****************************
     C*****F2KY          CHAIN     PF201                              60
      ***** BEG DEL ***** 03/05/05 **************************************
     C***60****            MOVE 'A'       ##MODE
      ***** END DEL ***** 03/05/05 **************************************
     C*********          CALL      'W0010R07'    ACDPL                  99

      /Free
       If Not(*in09);
          Chain f2ky pf201;
          If Not(%Found);
             GroupAux(*inkc:*inkl:'A':scdbrr);
          Else;
             GroupAux(*inkc:*inkl:'C':scdbrr);
          Endif;
       Else;
          GroupAux(*inkc:*inkl:'D':scdbrr);
       Endif;
      /End-Free
      *** end chg ***** 02/16/17 *****************************
     C*
     C                   ENDSR                                                  OPT7
     C*
      *
      *
     C*===============================================================
     C* OPT8 - SUBROUTINE FOR OPTION 8
     C*===============================================================
     C*
     C* OPTION 8=FYR DETAIL
     C*
     C     OPT8          BEGSR
     C*
     C                   MOVE      @@CALL        ##MODE
     C*
     C     *IN30         IFEQ      *ON
     C     *IN31         OREQ      *ON
     C     SCSEL         ANDEQ     '8'
     C                   MOVE      '1'           ##MODE
     C                   ENDIF                                                   *IN30=*ON
     C*
     C                   CALL      'W0011R'      ACDPL5                 99
     C*
     C                   ENDSR                                                  OPT8
     C*
      *
      *
     C*===============================================================
     C* OPT9 - SUBROUTINE FOR OPTION 9
     C*===============================================================
     C*
     C* OPTION 9=REGIONS
     C*
     C     OPT9          BEGSR
     C*
     C                   CALL      'W0520R'      ACDPL4                 99
     C*
     C                   ENDSR                                                  OPT9
     C*
      *
      *
     C*===============================================================
     C* OPT10 - SUBROUTINE FOR OPTION 10
     C*===============================================================
     C*
     C* OPTION 10=INCREASED LIMITS
     C*
     C     OPT10         BEGSR
     C*
     C                   CALL      'W2185R'      ACDPLS                 99
     C*
     C                   ENDSR                                                  OPT10
     C*
      *
      *
     C*===============================================================
     C* OPT11 - SUBROUTINE FOR OPTION 11
     C*===============================================================
     C*
     C* OPTION 11=CHAPTERS
     C*
     C     OPT11         BEGSR
     C*
     C                   CALL      'W0720R'      ACDPL4                 99
     C*
     C                   ENDSR                                                  OPT11
     C*
      *
      *
     C*===============================================================
     C* OPT12 - SUBROUTINE FOR OPTION 12
     C*===============================================================
     C*
     C* OPTION 12=AGENT RATES
     C*
     C     OPT12         BEGSR
     C*
     C                   CALL      'W0640R'      ACDPL3                 99
     C*
     C                   ENDSR                                                  OPT12
     C*
      *
      *
     C*===============================================================
     C* OPT13 - SUBROUTINE FOR OPTION 13
     C*===============================================================
     C*
     C* OPTION 13=RESERVE CUSHION
     C*
     C     OPT13         BEGSR
     C*
     C  N09              MOVE      ' '           ##MODE
     C   09              MOVE      '1'           ##MODE
     C*
     C                   CALL      'W0015R'      ACDPL3                 99
     C*
     C                   ENDSR                                                  OPT13
     C*
      *
      *
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
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      ##CO#         QCCO#
     C                   MOVE      SCFND         QCFND
     C*
     C*  CALL DARTS ENTRY PROGRAM
     C*
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM                    Q1PGM
     C*
     C                   ENDSR                                                  OPT14
     C*
      *
      *
     C*===============================================================
     C* OPT15 - SUBROUTINE FOR OPTION 15
     C*===============================================================
     C*
     C* OPTION 15=BILLING AUX
     C*
     C     OPT15         BEGSR
     C*
     C                   CALL      'W2157R'                             99
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      @@CALL        ##CALL            1
     C                   PARM                    ##RRN
     C                   PARM      SCDBRR        ##RRN2            9 0
     C                   PARM      ' '           ##BSTY            1
     C                   PARM      '        '    ##PFDT            8
     C*
     C                   ENDSR                                                  OPT15
     C*
      *
      *
     C*===============================================================
     C* OPT16 - SUBROUTINE FOR OPTION 16
     C*===============================================================
     C*
     C* OPTION 16=ADMIN/DARTS
     C*
     C     OPT16         BEGSR
     C*
     C  N09              MOVE      'C'           ##MODE
     C   09              MOVE      'D'           ##MODE
     C*
     C                   CALL      'W0049R'      ACDPL                  99
     C*
     C                   ENDSR                                                  OPT16
     C*
      *
      *
     C*===============================================================
     C* OPT17 - SUBROUTINE FOR OPTION 17
     C*===============================================================
     C*
     C* OPTION 17=CLAIM/DARTS
     C*
     C     OPT17         BEGSR
     C*
     C  N09              MOVE      'C'           ##MODE
     C   09              MOVE      'D'           ##MODE
     C*
     C                   CALL      'W0069R'      ACDPL                  99
     C*
     C                   ENDSR                                                  OPT17
     C*
      *
      *
     C*===============================================================
     C* OPT18 - SUBROUTINE FOR OPTION 18
     C*===============================================================
     C*
     C* OPTION 18=EXPENSE CONSTANT DEFINITION
     C*
     C     OPT18         BEGSR
     C*
     C                   CALL      'W0013R'      ACDPLS                 99
     C*
     C                   ENDSR                                                  OPT18
     C*
      *
      *
     C*===============================================================
     C* OPT19 - SUBROUTINE FOR OPTION 19
     C*===============================================================
     C*
     C* OPTION 19=CX/REIN STATE DEFINITION
     C*
     C     OPT19         BEGSR
     C*
     C                   CALL      'W0019R'      ACDPLS                 99
     C*
     C                   ENDSR                                                  OPT19
     C*
      *
      *
     C*===============================================================
     C* OPT20 - SUBROUTINE FOR OPTION 20
     C*===============================================================
     C*
     C* OPTION 20=BANK INFORMATION
     C*
     C     OPT20         BEGSR
     C*
     C  N09              MOVE      ' '           ##MODE
     C   09              MOVE      '1'           ##MODE
     C                   MOVE      SCFND         @@FND
     C*
     C                   CALL      'W2420R'                             99
     C                   PARM                    ##MODE            1
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FND             3
     C                   PARM      *BLANKS       @@ACCT           20
     C                   PARM      *BLANKS       @@EFDT            8
     C*** BEGIN CHANGE **** 09/22/98 ******************************
     C*******************  PARM 'WC'      @@APPL  2
     C                   PARM      '  '          @@APPL            2
     C*** END CHANGE **** 09/22/98 ******************************
     C                   PARM      *BLANKS       @@CKTP           10
     C*
     C                   ENDSR                                                  OPT20
     C*
      *
      *
     C*===============================================================
     C* OPT21 - SUBROUTINE FOR OPTION 21
     C*===============================================================
     C*
     C* OPTION 21=HARBOR EXPOSURE
     C*
     C     OPT21         BEGSR
     C*
     C                   CALL      'W0021R'      ACDPLS                 99
     C*
     C                   ENDSR                                                  OPT21
     C*
      *
      *
     C*===============================================================
     C* OPT22 - SUBROUTINE FOR OPTION 22
     C*===============================================================
     C*
     C* OPTION 22=STATE COMPENSATION BENEFITS
     C*
     C     OPT22         BEGSR
     C*
     C                   CALL      'W0022R'      ACDPLS                 99
     C*
     C                   ENDSR                                                  OPT22
     C*
      *
      *
     C*===============================================================
     C* OPT23 - SUBROUTINE FOR OPTION 23
     C*===============================================================
     C*
     C* OPTION 23=SHORT RATES
     C*
     C     OPT23         BEGSR
     C*
     C                   CALL      'W0023R'      ACDPL                  99
     C*
     C                   ENDSR                                                  OPT23
     C*
     C*** BEGIN ADD *** 01/22/98 ***********************************************
      *
     C*===============================================================
     C* OPT24 - SUBROUTINE FOR OPTION 24
     C*===============================================================
     C*
     C* OPTION 24=COMPENSATION PAYMENT CONTROL MAINTENANCE
     C*
     C     OPT24         BEGSR
     C*
     C                   CALL      'W8081R'      ACDPL                  99
     C*
     C                   ENDSR                                                  OPT24
     C*
     C*** END   ADD *** 01/22/98 ***********************************************
     C*                                                                *********
     C*** BEGIN ADD *** 10/01/98 ***********************************************
     C*===============================================================
     C* OPT25 - SUBROUTINE FOR OPTION 25
     C*===============================================================
     C*
     C* OPTION 25=CX CONFIRMATION CONTROL
     C*
     C     OPT25         BEGSR
     C*
     C                   MOVE      'C'           ##MODE
      ***** BEG ADD ******* 06/02/99 **********************************
      *
     C     SCAUTH        IFEQ      'I'
      ***** BEG ADD ******* 12/06/00 **********************************
     C     *IN09         OREQ      *ON
      ***** END ADD ******* 12/06/00 **********************************
     C                   MOVE      'D'           ##MODE
     C                   ENDIF                                                  SCAUTH=I
      *
      ***** END ADD ******* 06/02/99 **********************************
     C                   CALL      'W0029R'      ACDPL                  99
     C*
     C                   ENDSR                                                  OPT25
     C*
     C*** END   ADD *** 10/01/98 ***********************************************
     C*                                                                *********
     C**** BEGIN ADD **** 10/15/01 ************************************
      *** BEG ADD ******* 04/11/03 ************************************
     C*===============================================================
     C* OPT26 - SUBROUTINE FOR DIRECT DEBIT CONTROL
     C*===============================================================
     C     OPT26         BEGSR
      *
      *** BEG  ADD 03/16/06 *******************************************
     C  N09              MOVE      'U'           ##MODE                         CHANGE
     C   09              MOVE      'D'           ##MODE                         DISPLAY
      *** END  ADD 03/16/06 *******************************************
     C*
     C                   CALL      'W0010R26'    ACDPL                  99
     C                   ENDSR                                                  OPT26
      *** END ADD ******* 04/11/03 ************************************
     C*** BEG ADD *** 06/26/09 ***************************************
     C*===============================================================
     C* OPT27 - SUBROUTINE FOR POLICY EXPORT
     C*===============================================================
     C     OPT27         BEGSR
     C*
     C  N09              MOVE      'U'           ##MODE                         CHANGE
     C   09              MOVE      'D'           ##MODE                         DISPLAY
     C*
     C                   CALL      'W0010R27'    ACDPL                  99
     C                   ENDSR                                                  OPT27
     C*
     C*** END ADD *** 06/26/09 ***************************************
      *** BEG ADD ******* 06/25/09 ************************************
     C*===============================================================
     C* OPT28 - SUBROUTINE FOR GROUP CONTACT INFORMATION
     C*===============================================================
     C     OPT28         BEGSR
      *
     C  N09              MOVE      'U'           ##MODE                         CHANGE
     C*  09                MOVE 'D'       ##MODE           DISPLAY
     C*
     C                   CALL      'W0012R02'                           99
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    ##MODE
     C                   PARM      @@CALL        ##CALL
     C                   PARM      SCDBRR        ##RRN
     C                   PARM      ##CO#         ##CO#             3 0
     C                   PARM      SCFND         ##FND             3 0
     C*
     C                   ENDSR                                                  OPT28
      *** END ADD ******* 06/25/09 ************************************
      *** BEG ADD ******* 07/02/10 ************************************
     C*===============================================================
     C* OPT29 - EDI submission info
     C*===============================================================
     C     OPT29         BEGSR
     C     WCFL09        IFEQ      'Y'
     C*
     C                   CALL      'W0066R'      ACDPL7
     C*
     C                   ENDIF
     C                   ENDSR                                                  OPT29
      *** END ADD ******* 07/02/10 ************************************
     C*** BEG ADD *** 04/22/03 **************************************  *********
     C*
     C*===============================================================
     C* OPT30 - SUBROUTINE FOR OPTION 30
     C*===============================================================
     C*
     C* OPTION 30=COMPENSATION PAYMENT CONTROL MAINTENANCE
     C*
     C     OPT30         BEGSR
      *
      *** BEG  ADD 03/16/06 *******************************************
     C  N09              MOVE      ' '           ##MODE                         CHANGE
     C   09              MOVE      '1'           ##MODE                         DISPLAY
      *** END  ADD 03/16/06 *******************************************
     C*
     C                   CALL      'W0971R'      ACDPL                  99
     C*
     C                   ENDSR                                                  OPT30
     C*
     C*** END ADD *** 04/22/03 **************************************  *********
     C*
     C***** BEG ADD ******* 02/14/05 **********************************
     C*===============================================================
     C* OPT31 - SUBROUTINE FOR OPTION 62 - AUTO DARTS CONTROL
     C*===============================================================
     C     OPT31         BEGSR
     C*
     C                   MOVE      SCFND         #KFND
     C*
     C                   CALL      'W0972R'      PLAD                   99
     C*
     C                   ENDSR                                                  OPT31
     C***** END ADD ******* 02/14/05 **********************************
      *** BEG ADD ***** 02/02/11 *************************************
     C*
     C*===============================================================
     C* OPT32 - SUBROUTINE FOR OPTION 32
     C*===============================================================
     C*
     C* OPTION 30=CMS AUTO LETTER
     C*
     C     OPT32         BEGSR
      *
      *** BEG CHG ***** 07/08/11 *************************************
     C********** WCFL46    IFEQ 'Y'
     C     XXCMS         IFEQ      'Y'
      *** END CHG ***** 07/08/11 *************************************
     C*
     C  N09              MOVE      ' '           ##MODE                         CHANGE
     C   09              MOVE      '1'           ##MODE                         DISPLAY
     C*
     C                   CALL      'W0973R'      ACDPL                  99
     C*
     C                   ENDIF
     C                   ENDSR                                                  OPT32
     C*
      *** END ADD ***** 02/02/11 *************************************
      *** BEG ADD ***** 07/18/07 *************************************
     C*===============================================================
     C* OPT36 - SUBROUTINE FOR OPTION 36
     C*===============================================================
     C     OPT36         BEGSR
     C                   CALL      'W0505R'
     C                   PARM      'G'           ##CALL
     C                   PARM      SCDBRR        ##RRN
     C                   ENDSR                                                  OPT36
      *** END ADD ***** 07/18/07 *************************************
     C*===============================================================
     C* OPT40 - SUBROUTINE FOR OPTION 40
     C*===============================================================
     C*
     C* OPTION 40=CARRIER INFORMATION
     C*
     C     OPT40         BEGSR
     C*
      ***** BEG ADD ***** 01/26/06 ***********************************
      * Pass in the current status of Change or Add
     C  N09              MOVE      'C'           ##MODE
     C   09              MOVE      'D'           ##MODE
      ***** END ADD ***** 01/26/06 ***********************************
     C                   CALL      'W0010R40'    ACDPL6                 99
     C                   ENDSR                                                  OPT45
     C*
     C**** END   ADD **** 10/15/01 ************************************
     C*** BEGIN ADD *** 03/04/98 **************************************
     C*===============================================================
     C* OPT41 - SUBROUTINE FOR OPTION 41
     C*===============================================================
     C*
     C* OPTION 41=AUDIT TRACKING CONTROL MAINTENANCE
     C*
     C     OPT41         BEGSR
     C*
     C                   CALL      'W4700R'      ADTRPL                 99
     C*
     C                   ENDSR                                                  OPT41
     C*
     C*** END   ADD *** 03/04/98 *************************************
     C*                                                                *********
     C***** BEGIN ADD **** 09/23/99 **********************************
     C*===============================================================
     C* OPT45 - SUBROUTINE FOR OPTION 45
     C*===============================================================
     C*
     C* OPTION 45=PREMIUM FINANCE COMPANIES
     C*
     C     OPT45         BEGSR
     C*
     C                   CALL      'W0045R'      ACDPL                  99
     C*
     C                   ENDSR                                                  OPT45
     C*                                                                *********
     C***** END   ADD **** 09/23/99 **********************************
      ***** BEG ADD ******* 10/28/99 **********************************
      *                                                                *********
      *                                                                *********
      *                                                                *********
      *===============================================================
      * OPT46 - MCO Group Codes
      *===============================================================
      *
     C     OPT46         BEGSR
      *
     C                   CALL      'W2295R'                             99
     C     *INKC         PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM      @@CALL        ##CALL            1
     C                   PARM      *ZERO         ##RRN1            9 0          WMMGP
     C                   PARM      SCDBRR        ##RRN2            9 0          WMFNP
     C                   PARM      *ZERO         ##EFDT            7 0
      *
     C                   ENDSR                                                  OPT46
      *
      ***** END ADD ******* 10/28/99 **********************************
      *                                                                    *****
     C****** BEG ADD 10/18/10 *************************************
     C*===============================================================
     C* OPT47 - SUBROUTINE FOR OPTION 47 - STATE RATE CHANGE
     C*===============================================================
     C     OPT47         BEGSR
     C*
     C                   MOVE      @@CALL        ##MODE
     C*
     C                   CALL      'W0047R'                             99
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    ##MODE
     C                   PARM      ' '           ##GRP             3
     C                   PARM      SCDBRR        ##RRN
     C                   ENDSR                                                  OPT47
     C****** END ADD 10/18/10 *************************************
     C*** BEGIN ADD ***  11/13/03  ************************************
      *
     C*===============================================================
     C* OPT48 - SUBROUTINE FOR OPTION 48
     C*===============================================================
     C*
     C* OPTION 48=BILLING OPTIONS
     C*
     C     OPT48         BEGSR
     C*
     C  N09              MOVE      'C'           ##MODE
     C   09              MOVE      'D'           ##MODE
     C*
     C                   CALL      'W0048R'      ACDPL                  99
     C*
     C                   ENDSR                                                  OPT48
     C*
     C*** END ADD *****  11/13/03  ************************************
     C*
     C*===============================================================
     C* OPT50 - SUBROUTINE FOR OPTION 50
     C*===============================================================
     C*
     C* OPTION 50=STOCK DISCOUNTS
     C*
     C     OPT50         BEGSR
     C*
     C                   CALL      'W0150R'      ACDPLS                 99
     C*
     C                   ENDSR                                                  OPT50
      *
     C*===============================================================
     C* OPT51 - SUBROUTINE FOR OPTION 51 - INTERNAL ACCOUNT COMM RATES
     C*===============================================================
     C     OPT51         BEGSR
     C*
     C                   CALL      'W0151R'      ACDPL                  99
     C*
     C                   ENDSR
      *
     C*
     *** BEGIN ADD *** 08/17/99 **************************************
      *=========================================================================
      * OPT52 - Subroutine for Option 52 Policy Page Endorsement Maintenance
      *=========================================================================
     C     OPT52         BEGSR
      *
      *** Beg Chg ***** 03/12/20 *******************************
      ***                CALL      'W1055R'      PLPPE
     C                   Call      'WTGRPEND'
     C                   Parm                    ##CO#             3 0
     C                   Parm                    ##FND             3 0
     C                   Parm                    ##MODE            1
     C                   Parm      SCDBRR        ##RRN             9 0
      *** End Chg ***** 03/12/20 *******************************
      *
     C                   ENDSR                                                  OPT52
      *=========================================================================
      * OPT53 - Subroutine for Option 53 Midterm Endorsement Maintenance
      *=========================================================================
     C     OPT53         BEGSR
      *
     C                   CALL      'W1056R'      PLPPE
      *
     C                   ENDSR                                                  OPT53
     *** END   ADD *** 08/17/99 **************************************
     C*===============================================================
     C* OPT54 - SUBROUTINE FOR OPTION 54
     C*===============================================================
     C*
     C* OPTION 54=STATISTICAL REPORTING/RATING
     C*
     C     OPT54         BEGSR
     C*
     C                   CALL      'W0014R'      ACDPLS                 99
     C*
     C                   ENDSR                                                  OPT54
     C*
      *
      *
     C*===============================================================
     C* OPT55 - SUBROUTINE FOR OPTION 55
     C*===============================================================
     C*
     C* OPTION 55=DEDUCTIBLE DISCOUNTS
     C*
     C     OPT55         BEGSR
     C*
     C                   CALL      'W0155R'      ACDPLS                 99
     C*
     C                   ENDSR                                                  OPT55
     C*
     C*** BEG ADD ***** 12/04/97 ***********************************************
      *
     C*===============================================================
     C* OPT56 - SUBROUTINE FOR OPTION 56
     C*===============================================================
     C*
     C* OPTION 56=ENDORSEMENT STATISTICS INFO
     C*
     C     OPT56         BEGSR
     C*
     C                   CALL      'W0054R'      ACDPLS                 99
     C*
     C                   ENDSR                                                  OPT56
     C*
     C*** END ADD ***** 12/04/97 ***********************************************
     C*** BEG ADD ***** 01/28/98 ***********************************************
     C*===============================================================
     C* OPT57 - SUBROUTINE FOR OPTION 57
     C*===============================================================
     C*
     C* OPTION 57=ENDORS/DARTS
     C*
     C     OPT57         BEGSR
     C*
     C  N09              MOVE      'C'           ##MODE
     C   09              MOVE      'D'           ##MODE
     C*
     C                   CALL      'W0089R'      ACDPL                  99
     C*
     C                   ENDSR                                                  OPT57
     C*
     C*** END ADD ***** 01/28/98 ***********************************************
     C*** BEGIN ADD *** 03/04/98 **************************************
     C*===============================================================
     C* OPT58 - SUBROUTINE FOR OPTION 58
     C*===============================================================
     C*
     C* OPTION 58=AUDIT TRACKING/ DARTS CONTROL INFORMATION
     C*
     C     OPT58         BEGSR
     C*
     C*** BEGIN CHANGE *** 02/19/99 ***********************************
     C     @@CALL        IFEQ      '1'
     C                   MOVE      'D'           ##MODE
     C                   ELSE
     C                   MOVE      'C'           ##MODE
     C                   ENDIF
     C******************** MOVE 'C'       ##MODE
     C*** END CHANGE ***** 02/19/99 ***********************************
     C*
     C                   CALL      'W0026R'      ACDPL                  99
     C*
     C                   ENDSR                                                  OPT58
     C*
     C*** END   ADD *** 03/04/98 *************************************
     C*** BEG ADD **** 09/28/01 **************************************
     C*===============================================================
     C* OPT59 - SUBROUTINE FOR OPTION 59 - LOSS CONSTANT DEFINITION
     C*===============================================================
     C     OPT59         BEGSR
     C                   CALL      'W0017R'      ACDPL                  99
     C                   ENDSR                                                  OPT59
     C*** END ADD **** 09/28/01 **************************************
      *
     C**** BEGIN ADD **** 12/12/02 ************************************
     C*===============================================================
     C* OPT60 - SUBROUTINE FOR OPTION 60
     C*===============================================================
     C*
     C* OPTION 60=INSTALLMENT FEES
     C*
     C     OPT60         BEGSR
     C*
     C                   CALL      'W0016R'      ACDPLS                 99
     C*
     C                   ENDSR                                                  OPT90
     C*
     C**** END ADD **** 12/12/02 ************************************
      *
     E***** BEG ADD ***** 09/07/04 ***************************************
     C*===============================================================
     C* OPT61 - SUBROUTINE FOR OPTION 61 - AUTO CASH DISSOLVE
     C*===============================================================
     C     OPT61         BEGSR
     C  N09              MOVE      'C'           ##MODE
     C   09              MOVE      'D'           ##MODE
     C                   CALL      'W0010R61'    ACDPL                  99
     C                   ENDSR                                                  OPT61
      *
     C*+*+*+ BEG ADD 08/24/06 *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*
     C*===============================================================
     C* OPT62 - SUBROUTINE FOR OPTION 62 - RATE SET CODES
     C*===============================================================
     C     OPT62         BEGSR
     C                   CALL      'W1062R'                             99
     C     *INKC         PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM      @@CALL        ##CALL            1
     C                   PARM      *ZERO         ##RRN             9 0
     C                   PARM      SCDBRR        ##RRN1            9 0
     C                   ENDSR                                                  OPT61
     C*+*+*+ END ADD 08/24/06 *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*
      *
     C****** BEG ADD 01/25/10 *************************************
     C*===============================================================
     C* OPT63 - SUBROUTINE FOR OPTION 63 - FEE INFORMATION
     C*===============================================================
     C     OPT63         BEGSR
     C                   CALL      'W0063R'                             99
     C     *INKC         PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM      @@CALL        ##CALL            1
     C                   PARM      SCDBRR        ##RRN1            9 0
     C                   ENDSR                                                  OPT63
     C****** END ADD 01/25/10 *************************************
      *
     C****** BEG ADD 07/28/10 *************************************
     C*===============================================================
     C* OPT64 - SUBROUTINE FOR OPTION 64 - FLEXIBLE INFORMATION
     C*===============================================================
     C     OPT64         BEGSR
     C****** BEG CHG 12/30/10 *************************************
     C  N09              MOVE      'C'           ##MODE
     C****** END CHG 12/30/10 *************************************
     C   09              MOVE      'D'           ##MODE
     C                   CALL      'W0010R64'    ACDPL                  99
     C                   ENDSR                                                  OPT64
     C****** END ADD 07/28/10 *************************************
     C****** Beg Add 12/31/10 *************************************
     C*===============================================================
     C* OPT65 - SUBROUTINE FOR OPTION 65 - Policy page 3c information
     C*===============================================================
     C     OPT65         BEGSR
     C                   MOVE      @@CALL        ##MODE
     C*
     C     *IN30         IFEQ      *ON
     C     *IN31         OREQ      *ON
     C     SCSEL         ANDEQ     '65'
     C                   MOVE      '1'           ##MODE
     C                   ENDIF                                                   *IN30=*ON
     C*
     C                   CALL      'W0010R65'    ACDPL                  99
     C*
     C                   ENDSR                                                  OPT65
     C****** End Add 12/31/10 *************************************
     c***  Begin Add  ***  11/27/18  *************************************
    C*================================================================
    C* Opt75 - Process other function keys
    C*================================================================
      /Free
       Begsr Opt75;
        Select;
        When @@call = ' ';
           ##Mode = 'C';
        Other;
           ##Mode = 'D';
        Endsl;
        Reset dsWITMPLTparms;
        dsWITMPLTparms.co# = kkCo#;
        dsWITMPLTparms.fnd = scFnd;
        TemplateAssociations(dsWITMPLTparms:'Group':##Mode);
       Endsr;
      /End-free
     c***  End   Add  ***  11/27/18  *************************************
     E***** END ADD ***** 09/07/04 ***************************************
     C***** BEG ADD ******* 02/10/04 **********************************
     C*===============================================================
     C* OPT92 - SUBROUTINE FOR OPTION 92 - EXCLUDED STATES FOR WCPOLS
     C*===============================================================
     C     OPT92         BEGSR
     C*
     C                   MOVE      ##CO#         #KCO#
     C                   MOVE      SCFND         #KFND
      *
      *** BEG  ADD 03/16/06 *******************************************
     C  N09              MOVE      ' '           ##CALL                         CHANGE
     C   09              MOVE      '1'           ##CALL                         DISPLAY
      *** END  ADD 03/16/06 *******************************************
     C*
     C                   CALL      'W7419R'                             99
     C                   PARM                    ##CALL            1
     C     *INKC         PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM      *ZERO         ##RRN             9 0          WDXSP (Return)
     C                   PARM      *ZERO         ##RRN1            9 0          WDELP
     C                   PARM                    #KCO#             3
     C                   PARM                    #KFND             3
     C*
     C                   ENDSR                                                  OPT92
      ***** END ADD ******* 02/10/04 **********************************
      *** BEG ADD ***** 12/18/05 **************************************
     C*===============================================================
     C* OPT93 - COMMISSION MOD FACTORS
     C*===============================================================
     C     OPT93         BEGSR
     C                   CALL      'W2143R'      ACDPL                  99
     C                   ENDSR                                                  OPT93
      *** END ADD ***** 12/18/05 **************************************
     C*===============================================================
     C* OPT80 - SUBROUTINE FOR OPTION 80
     C*===============================================================
     C*
     C* OPTION 80=CLIENT CONTROL INFORMATION
     C*
     C     OPT80         BEGSR
     C*
     C                   CALL      'W0010R80'    ACDPL                  99
     C*
     C                   ENDSR                                                  OPT80
     C*
     C**** BEGIN ADD **** 03/04/02 ************************************
     C**** BEGIN DEL **** 12/12/02 ************************************
     C*===============================================================
     C*********** OPT90 - SUBROUTINE FOR OPTION 90
     C*===============================================================
     C********** OPTION 90=INSTALLMENT FEES
     C********** OPT90     BEGSR
     C**********           CALL 'W0016R'  ACDPLS       99
     C**********           ENDSR                           OPT90
     C**** END   DEL **** 12/12/02 ************************************
     C**** END   ADD **** 03/04/02 ************************************
      *
      **** BEGIN ADD **** 03/10/10 ***********************************
     C*===============================================================
     C* OPT90 - SUBROUTINE FOR OPTION 90
     C*===============================================================
     C*
     C* OPTION 90=New clm diary DARTS rm
      *
     C     OPT90         BEGSR
      *
     C     @@CALL        IFEQ      '1'
     C                   MOVE      'D'           ##MODE
     C                   ELSE
     C                   MOVE      'C'           ##MODE
     C                   ENDIF
      *
     C                   CALL      'W0069R90'    ACDPL                  99
      *
     C                   ENDSR                                                  OPT90
     C*
     C*** END ADD **** 03/10/10 ************************************
     C***  Begin Add  ***  11/10/10  **************************************
     C*===============================================================
     C* OPT91 - Subroutine for option 91=HCN Program
     C*===============================================================
     C     OPT91         BEGSR
     C*
     C                   CALL      'W0191R'                             99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      @@CALL        ##CALL            1
     C                   PARM      SCDBRR        ##RRN             9 0
      *
     C                   ENDSR                                                  OPT91
     C***  End   Add  ***  11/10/10  **************************************
     **
     C*===============================================================
     C* OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
     C*===============================================================
     C*
     C     OTHFKS        BEGSR
     C*
     C     *INKC         CASEQ     *ON           KCDO
     C     *INKX         CASEQ     *ON           KXDO
     C                   CAS                     ENTRDO
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*
      *
      *
     C*===============================================================
     C* OTHOPT - SUBROUTINE FOR OTHER OPTIONS
     C*===============================================================
     C*
     C     OTHOPT        BEGSR
      *
      ***** BEG ADD ******* 06/02/99 **********************************
      *
      * Check if only authorized to display mode
      *
     C                   MOVE      @@CALL        SVCALL
     C                   MOVE      *IN09         SVIN09
      *
     C     SCAUTH        IFEQ      'I'
     C                   MOVE      *ON           *IN09
     C                   MOVE      '1'           @@CALL
     C                   ENDIF                                                  SCAUTH=I
      *
      ***** END ADD ******* 06/02/99 **********************************
     C*
     C                   MOVE      ##CO#         ##CO#
     C                   MOVE      SCFND         ##FND
     C                   MOVE      *ZEROS        ##FYR
     C                   MOVE      @@CALL        ##MODE
     C*
     C     SCSEL         CASEQ     '7'           OPT7
     C     SCSEL         CASEQ     '8'           OPT8
     C     SCSEL         CASEQ     '9'           OPT9
     C     SCSEL         CASEQ     '10'          OPT10
     C     SCSEL         CASEQ     '11'          OPT11
     C     SCSEL         CASEQ     '12'          OPT12
     C     SCSEL         CASEQ     '13'          OPT13
     C     SCSEL         CASEQ     '14'          OPT14
     C     SCSEL         CASEQ     '15'          OPT15
     C     SCSEL         CASEQ     '16'          OPT16
     C     SCSEL         CASEQ     '17'          OPT17
     C     SCSEL         CASEQ     '18'          OPT18
     C     SCSEL         CASEQ     '19'          OPT19
     C     SCSEL         CASEQ     '20'          OPT20
     C     SCSEL         CASEQ     '21'          OPT21
     C     SCSEL         CASEQ     '22'          OPT22
     C     SCSEL         CASEQ     '23'          OPT23
     C*                                                                *********
     C*** BEGIN ADD *** 01/22/98 ***********************************************
     C     SCSEL         CASEQ     '24'          OPT24
     C*** END   ADD *** 01/22/98 ***********************************************
     C*                                                                *********
     C*** BEGIN ADD *** 10/01/98 ***********************************************
     C     SCSEL         CASEQ     '25'          OPT25
     C*** END   ADD *** 10/01/98 ***********************************************
     C*** BEG ADD ***** 04/11/03 ***********************************************
     C     SCSEL         CASEQ     '26'          OPT26
     C*** END ADD ***** 04/11/03 ***********************************************
     C*** BEG ADD ***** 06/26/09 ***********************************************
     C     SCSEL         CASEQ     '27'          OPT27
     C*** END ADD ***** 06/26/09 ***********************************************
     C*** BEG ADD ***** 06/25/09 ***********************************************
     C     SCSEL         CASEQ     '28'          OPT28
     C*** END ADD ***** 06/25/09 ***********************************************
     C*** BEG ADD ***** 07/02/10 ***********************************************
     C     SCSEL         CASEQ     '29'          OPT29
     C*** END ADD ***** 07/02/10 ***********************************************
     C*** BEG ADD ***** 04/22/03 ***********************************************
     C     SCSEL         CASEQ     '30'          OPT30
     C*** END ADD ***** 04/22/03 ***********************************************
     C***** BEG ADD ******* 02/14/05 **********************************
     C     SCSEL         CASEQ     '31'          OPT31
     C***** END ADD ******* 02/14/05 **********************************
     C***** BEG ADD ******* 02/02/11 **********************************
     C     SCSEL         CASEQ     '32'          OPT32
     C***** END ADD ******* 02/02/11 **********************************
     C*** BEG ADD ***** 07/18/07 *************************************
     C     SCSEL         CASEQ     '36'          OPT36
     C*** END ADD ***** 07/18/07 *************************************
     C**** BEGIN ADD **** 10/15/01 ************************************
     C     SCSEL         CASEQ     '40'          OPT40
     C**** END   ADD **** 10/15/01 ************************************
     C*** BEGIN ADD *** 03/04/98 *************************************
     C     SCSEL         CASEQ     '41'          OPT41
     C*** END   ADD *** 03/04/98 *************************************
     C***** BEGIN ADD **** 09/23/99 ***********************************
     C     SCSEL         CASEQ     '45'          OPT45
     C***** END   ADD **** 09/23/99 ***********************************
      ***** BEG ADD ******* 10/28/99 **********************************
     C     SCSEL         CASEQ     '46'          OPT46
      ***** END ADD ******* 10/28/99 **********************************
      ***** BEG ADD ******* 10/18/10 **********************************
     C     SCSEL         CASEQ     '47'          OPT47
      ***** END ADD ******* 10/18/10 **********************************
     C*** BEGIN ADD ***  11/13/03  ************************************
     C     SCSEL         CASEQ     '48'          OPT48
     C*** END ADD *****  11/13/03  ************************************
     C     SCSEL         CASEQ     '50'          OPT50
      *
     C*++++++ BEG ADD 08/28/06 ++++++++++++++++++++++++++++++++++++++
     C     SCSEL         CASEQ     '51'          OPT51
     C*++++++ END ADD 08/28/06 ++++++++++++++++++++++++++++++++++++++
      *
     *** BEGIN ADD *** 08/17/99 ***********************************************
     C     SCSEL         CASEQ     '52'          OPT52
     C     SCSEL         CASEQ     '53'          OPT53
     *** END   ADD *** 08/17/99 ***********************************************
     C     SCSEL         CASEQ     '54'          OPT54
     C     SCSEL         CASEQ     '55'          OPT55
     C*** BEG ADD ***** 12/04/97 ***********************************************
     C     SCSEL         CASEQ     '56'          OPT56
     C*** END ADD ***** 12/04/97 ***********************************************
     C*** BEG ADD ***** 01/28/98 ***********************************************
     C     SCSEL         CASEQ     '57'          OPT57
     C*** END ADD ***** 01/28/98 ***********************************************
     C*** BEGIN ADD *** 03/04/98 *************************************
     C     SCSEL         CASEQ     '58'          OPT58
     C*** END   ADD *** 03/04/98 *************************************
     C*** BEG ADD **** 09/28/01 **************************************
     C     SCSEL         CASEQ     '59'          OPT59
     C**** BEGIN ADD **** 12/12/02 ************************************
     C     SCSEL         CASEQ     '60'          OPT60
     C**** END   ADD **** 12/12/02 ************************************
     E***** BEG CHG ***** 09/07/04 ***************************************
     C     SCSEL         CASEQ     '61'          OPT61
     E***** END CHG ***** 09/07/04 ***************************************
      *
     C*+*+*+* BEG ADD 08/24/06 *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
     C     SCSEL         CASEQ     '62'          OPT62
     C*+*+*+* BEG ADD 08/24/06 *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
      *
     C***** BEG ADD ***** 01/25/10 ***************************************
     C     SCSEL         CASEQ     '63'          OPT63
     C***** END ADD ***** 01/25/10 ***************************************
      *
     C***** BEG ADD ***** 07/28/10 ***************************************
     C     SCSEL         CASEQ     '64'          OPT64
     C***** END ADD ***** 07/28/10 ***************************************
     C****** Beg Add 12/31/10 *************************************
     C     SCSEL         CASEQ     '65'          OPT65
     C****** End Add 12/31/10 *************************************
     c***  Begin Add  ***  11/27/18  *************************************
     c     scsel         caseq     '75'          opt75
     c***  End   Add  ***  11/27/18  *************************************
     C*** END ADD **** 03/04/02 **************************************
     C     SCSEL         CASEQ     '80'          OPT80
     C**** BEGIN ADD **** 03/04/02 ************************************
     C**** BEGIN DEL **** 12/12/02 ************************************
     C********   SCSEL     CASEQ'90'      OPT90
     C*** BEGIN ADD **** 03/10/10 *************************************
     C     SCSEL         CASEQ     '90'          OPT90
     C*** END ADD ****** 03/10/10 ************************************
     C**** END DEL ****** 12/12/02 ************************************
     C***  Begin Add  ***  11/10/10  **************************************
     C     SCSEL         CASEQ     '91'          OPT91
     C***  End   Add  ***  11/10/10  **************************************
      ***** BEG ADD ******* 02/10/04 **********************************
     C     SCSEL         CASEQ     '92'          OPT92
      ***** END ADD ******* 02/10/04 **********************************
      ***** BEG ADD ******* 12/18/05 **********************************
     C     SCSEL         CASEQ     '93'          OPT93
      ***** END ADD ******* 12/18/05 **********************************
     C**** END   ADD **** 03/04/02 ************************************
     C                   ENDCS
     C*
      ***** BEG ADD ******* 06/02/99 **********************************
     C                   MOVE      SVCALL        @@CALL
     C                   MOVE      SVIN09        *IN09
      ***** END ADD ******* 06/02/99 **********************************
      *
     C                   ENDSR                                                  OTHOPT
     C*
      *
      *
     C*===============================================================
     C* PGMEND - SUBROUTINE TO END THE PROGRAM
     C*===============================================================
     C*
     C     PGMEND        BEGSR
     C                   ENDSR
     C*
      *
      *
     C*===============================================================
     C* PGMSTR - SUBROUTINE TO START THE PROGRAM
     C*===============================================================
     C*
     C     PGMSTR        BEGSR
     C*
      *** BEG ADD ***** 12/18/05 **************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      *
     C     CLTID         IFEQ      '136'
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '93'          XXNMBR
     C                   MOVE      'YYN'         XXMIS
     C                   EXSR      CHGOFA
     C                   ENDIF
      *** END ADD ***** 12/18/05 **************************************
     C*
      *++ BEG ADD +++++ 08/28/06 ++++++++++++++++++++++++++++++++++++++
     C     CLTID         IFEQ      '114'
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '51'          XXNMBR
     C                   MOVE      'YYN'         XXMIS
     C                   EXSR      CHGOFA
     C                   ENDIF
      *++ END ADD +++++ 08/28/06 ++++++++++++++++++++++++++++++++++++++
      *** BEG ADD ***** 08/22/11 **************************************
     C     CLTID         IFEQ      '065'
     C     CLTID         OREQ      '093'
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '65'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      CHGOFA
     C                   ENDIF
      *** END ADD ***** 08/22/11 **************************************
      *++ BEG ADD +++++ 03/10/10 ++++++++++++++++++++++++++++++++++++++
     C     CLTID         IFEQ      '025'
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '90'          XXNMBR
     C                   MOVE      'YYN'         XXMIS
     C                   EXSR      CHGOFA
     C                   ENDIF
      *++ END ADD +++++ 03/10/10 ++++++++++++++++++++++++++++++++++++++
      *
     C***  Begin Add  ***  11/10/10  **************************************
     C     CLTID         IFEQ      '139'
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '91'          XXNMBR
     C                   MOVE      'YYN'         XXMIS
     C                   EXSR      CHGOFA
     C                   ENDIF
     C***  End   Add  ***  11/10/10  **************************************
     C*
     C****** BEG ADD 12/30/10 *************************************
     C                   CALL      'SRCHKOBJ'
     C                   PARM      'W0010R64'    OBJNAM           10
     C                   PARM      '*PGM'        XXTYP             7
     C                   PARM                    XXOBJ             1
     C     XXOBJ         IFEQ      '0'
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '64'          XXNMBR
     C                   MOVE      'YYN'         XXMIS
     C                   EXSR      CHGOFA
     C                   ENDIF
     C****** END ADD 12/30/10 *************************************
     C                   SETOFF                                       45
     C     ##XFND        IFEQ      'Y'
      ***** BEG ADD ******* 06/02/99 **********************************
     C     ##RCDS        IFEQ      *OFF
      ***** END ADD ******* 06/02/99 **********************************
     C                   SETON                                        45
      ***** BEG ADD ******* 06/02/99 **********************************
     C                   ENDIF                                                  ##RCDS=*OFF
      ***** END ADD ******* 06/02/99 **********************************
     C***  BEGIN DEL  ***  06/08/98  *********************************
     C*****BEGIN*CHG  ***  01/22/98  *********************************
     C************         MOVE OFD,29    DATA             F6=ADD
     C****BEG*CHG****** 01/28/98 ***********************************************
     C************         MOVE OFD,31    DATA             F6=ADD
     C****BEGIN*CHG *** 03/04/98 *************************************
     C******************** MOVE OFD,32    DATA             F6=ADD
     C************         MOVE OFD,34    DATA             F6=ADD
     C****END***CHG *** 03/04/98 *************************************
     C****END*CHG****** 01/28/98 ***********************************************
     C*****END***CHG  ***  01/22/98  *********************************
     C************         MOVE 'N'       MF
     C*****BEGIN*CHG  ***  01/22/98  *********************************
     C************         MOVE DATA      OFD,29
     C************         MOVE OFD,33    DATA             F11=CHG VIEW
     C****BEG*CHG****** 01/28/98 ***********************************************
     C************         MOVE DATA      OFD,31           F6=ADD
     C************         MOVE OFD,35    DATA             F11=CHG VIEW
     C****BEGIN*CHG *** 03/04/98 *************************************
     C******************** MOVE DATA      OFD,32           F6=ADD
     C******************** MOVE OFD,36    DATA             F11=CHG VIEW
     C************         MOVE DATA      OFD,34           F6=ADD
     C************         MOVE OFD,38    DATA             F11=CHG VIEW
     C****END***CHG *** 03/04/98 *************************************
     C****END*CHG****** 01/28/98 ***********************************************
     C*****END***CHG  ***  01/22/98  *********************************
     C************         MOVE 'N'       MF
     C*****BEGIN*CHG  ***  01/22/98  *********************************
     C************         MOVE DATA      OFD,33
     C****BEG*CHG****** 01/28/98 ***********************************************
     C************         MOVE DATA      OFD,35           F11=CHG VIEW
     C****BEGIN*CHG *** 06/08/98 *************************************
     C*******************  MOVE DATA      OFD,36           F11=CHG VIEW
     C************         MOVE DATA      OFD,38           F11=CHG VIEW
     C****END***CHG *** 06/08/98 *************************************
     C****END*CHG****** 01/28/98 ***********************************************
     C*****END***CHG  ***  01/22/98  *********************************
     C***  END   DEL  ***  06/08/98  *********************************
     C*
     C***  BEGIN ADD  ***  06/08/98  *********************************
     C*  CHANGE MAINTENACE FLAG TO 'N' FOR FUNCTION F6=ADD
     C                   MOVE      '2'           XXOPFN
     C                   MOVE      '06'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      CHGOFA
      ***** BEG ADD ******* 06/02/99 **********************************
     C     ##RCDS        IFEQ      *OFF
      ***** END ADD ******* 06/02/99 **********************************
     C*
     C*  CHANGE MAINTENACE FLAG TO 'N' FOR FUNCTION F11=CHANGE VIEW
     C*
     C                   MOVE      '2'           XXOPFN
     C                   MOVE      '11'          XXNMBR
     C                   MOVE      'NYY'         XXMIS
     C                   EXSR      CHGOFA
      ***** BEG ADD ******* 06/02/99 **********************************
     C                   ENDIF                                                  ##RCDS=*OFF
      ***** END ADD ******* 06/02/99 **********************************
     C***  END   ADD  ***  06/08/98  *********************************
     C                   ENDIF
     C*
     C***  BEGIN ADD  ***  11/12/97  *********************************
     C     ##CO#         CHAIN     WMHXL                              60
     C*
     C***  BEGIN CHG  ***  01/20/98  *********************************
     C***60*****           MOVELCLRFKY    OFD,24           OPTION 80
     C***  BEGIN CHG  ***  01/22/98  *********************************
     C***60*****           MOVELCLRFKY    OFD,25           OPTION 80
     C*** BEG CHG ***** 01/28/98 ***********************************************
     C***60                MOVELCLRFKY    OFD,26           OPTION 80
     C*** BEGIN CHG *** 06/08/98 *************************************
     C***60**************  MOVELCLRFKY    OFD,27           OPTION 80
     C     *IN60         IFEQ      *ON
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '80'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      CHGOFA
     C                   ENDIF
     C*** END   CHG *** 06/08/98 *************************************
     C*** END CHG ***** 01/28/98 ***********************************************
     C***  END   CHG  ***  01/22/98  *********************************
     C***  END   CHG  ***  01/20/98  *********************************
     C***  END   ADD  ***  11/12/97  *********************************
     C*
     C*
     C                   MOVE      *ON           *IN20
     C                   Z-ADD     10            SF1PAG            5 0
     C*
     C     Q1PRMS        IFGT      6
     C     @@VIEW        ANDLE     VIEWMX
     C     @@VIEW        ANDGT     *ZERO
     C                   Z-ADD     @@VIEW        VIEW#
     C                   ENDIF                                                  Q1PRMS>6
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    30    SEARCH
     C     @@CALL        COMP      'Y'                                    31    SEARCH W/YR
     C*
     C   31              MOVEL     '2'           @@CALL
     C*
     C     @@CO#         IFNE      *BLANKS
     C                   MOVEL     @@CO#         KKCO#
     C                   ELSE
     C                   MOVEL     ##CO#         KKCO#
     C     ##FND         IFNE      *ZERO
     C                   MOVE      ##FND         KKFND
     C                   MOVEL     ##FNAM        KKNAME
     C                   ENDIF                                                  ##FND<>*ZERO
     C                   ENDIF                                                  ##FND<>*ZERO
     C*
     C*** BEGIN DEL *** 06/25/98 *************************************
     C****BEGIN ADD *** 03/04/98 *************************************
     C***CANNOT EXECUTE OPT41 IF CLIENT NOT ENROLLED IN "AUDIT TRACKING"
     C*********
     C*********            MOVEL'WC'      XXSYK
     C*********  SYKY      CHAINSWC01                60
     C*********  *IN60     IFEQ *ON
     C*********  *IN60     OREQ *OFF
     C*********  WCFL12    ANDNE'Y'                        NOT ENROLLED,
     C*********
     C***REMOVE OPTION 41=AUDIT TRACKING
     C*********
     C****BEGIN CHG *** 06/08/98 *************************************
     C******************** MOVELCLRFKY    OFD,22           OPTION 41
     C*********            MOVE '1'       XXOPFN
     C*********            MOVE '41'      XXNMBR
     C*********            MOVE 'NNN'     XXMIS
     C*********            EXSR CHGOFA
     C****END** CHG *** 06/08/98 *************************************
     C*********
     C*********            ENDIF                           WCFL12<>Y
     C****END** ADD *** 03/04/98 *************************************
     C*** END   DEL *** 06/25/98 *************************************
     C*** BEGIN ADD *** 05/26/98 *************************************
     C*
     C*  DETERMINE IF CLIENT IS ADMIN-ONLY OR CLAIMS-ONLY
     C*
     C*** BEGIN ADD *** 06/25/98 *************************************
     C                   MOVEL     'WC'          XXSYK
     C     SYKY          CHAIN     SWC01                              60
     C*** END   ADD *** 06/25/98 *************************************
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C*** BEGIN ADD ***  03/04/02  ************************************
     C     WCFL21        IFNE      'Y'
     C                   MOVE      '1'           XXOPFN
     C*** BEGIN CHG ***  12/12/02  ************************************
     C* Change from Option 90 to Option 60
     C***********          MOVE '90'      XXNMBR
     C                   MOVE      '60'          XXNMBR
     C*** END CHG *****  12/12/02  ************************************
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      CHGOFA
     C                   ENDIF                                                  WCFL21 IFNE
     C*** END ADD *****  03/04/02  ************************************
     C*
     C*** BEG ADD *****  04/11/03  ************************************
     C     WCFL24        IFNE      'Y'
     C*** BEGIN ADD ***  03/28/08  ************************************
     C     WCFL38        ANDNE     'Y'
     C*** END ADD *****  03/28/08  ************************************
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '26'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      CHGOFA
     C*** BEG ADD ***** 11/13/09 **************************************
     C                   ELSE
     C                   MOVEL     'WA073R'      OBJNAM           10
     C                   MOVEL     '*PGM'        OBJTYP            7
     C                   CALL      'SRCHKOBJ'
     C                   PARM                    OBJNAM           10
     C                   PARM                    OBJTYP            7
     C                   PARM                    OBJERR            1
     C     OBJERR        IFEQ      *OFF
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '26'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      CHGOFA
     C                   ENDIF
     C*** END ADD ***** 11/13/09 **************************************
     C                   ENDIF                                                  WCFL24 IFNE
     C*** END ADD *****  04/11/03  ************************************
     C*
     C     WCFL08        IFNE      *BLANK
     C*
     C*  ADMIN CLIENT
     C     WCFL08        IFEQ      'A'
     C*** BEGIN CHG *** 06/08/98 *************************************
     C******************** MOVELCLRFKY    OFD,10           OPT 13
     C                   MOVE      '13'          XXNMBR
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      CHGOFA
     C******************** MOVELCLRFKY    OFD,14           OPT 17
     C                   MOVE      '17'          XXNMBR
     C                   EXSR      CHGOFA
     C******************** MOVELCLRFKY    OFD,19           OPT 22
     C                   MOVE      '22'          XXNMBR
     C                   EXSR      CHGOFA
     C******************** MOVELCLRFKY    OFD,21           OPT 24
     C                   MOVE      '24'          XXNMBR
     C                   EXSR      CHGOFA
     C*** END   CHG *** 06/08/98 *************************************
     C                   ENDIF                                                  WCFL08=A
     C*
     C*  CLAIMS CLIENT
     C     WCFL08        IFEQ      'C'
     C*** BEGIN CHG *** 06/08/98 *************************************
     C******************** MOVELCLRFKY    OFD,9            OPT 12
     C                   MOVE      '12'          XXNMBR
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      CHGOFA
     C******************** MOVELCLRFKY    OFD,12           OPT 15
     C                   MOVE      '15'          XXNMBR
     C                   EXSR      CHGOFA
     C******************** MOVELCLRFKY    OFD,15           OPT 18
     C                   MOVE      '18'          XXNMBR
     C                   EXSR      CHGOFA
     C******************** MOVELCLRFKY    OFD,16           OPT 19
     C                   MOVE      '19'          XXNMBR
     C                   EXSR      CHGOFA
     C******************** MOVELCLRFKY    OFD,18           OPT 21
     C                   MOVE      '21'          XXNMBR
     C                   EXSR      CHGOFA
     C******************** MOVELCLRFKY    OFD,20           OPT 23
     C                   MOVE      '23'          XXNMBR
     C                   EXSR      CHGOFA
     C******************** MOVELCLRFKY    OFD,23           OPT 50
     C                   MOVE      '50'          XXNMBR
     C                   EXSR      CHGOFA
     C******************** MOVELCLRFKY    OFD,24           OPT 54
     C                   MOVE      '54'          XXNMBR
     C                   EXSR      CHGOFA
     C******************** MOVELCLRFKY    OFD,25           OPT 55
     C                   MOVE      '55'          XXNMBR
     C                   EXSR      CHGOFA
     C******************** MOVELCLRFKY    OFD,26           OPT 56
     C                   MOVE      '56'          XXNMBR
     C                   EXSR      CHGOFA
     C*** END   CHG *** 06/08/98 *************************************
     C                   ENDIF                                                  WCFL08=C
     C*
     C                   ENDIF                                                  WCFL08<>*BLANK
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C*** END   ADD *** 05/26/98 *************************************
     C*
     c***  Begin Add  ***  11/27/18  *************************************
     c* Opt 75 is only allowed if module installed
      /Free
       If isModEnabled('Portal Module');
          xxOpFn = '1';
          xxNmbr = '75';
          xxMIS  = 'YYN';
          Exsr UpdOfA;
       Endif;
      /End-free
     c***  End   Add  ***  11/27/18  *************************************
      *
     C*** BEG   ADD *** 01/24/08 *************************************
     C                   MOVEL(P)  'SY'          XXSYK
     C     SYKY          CHAIN     SSY01                              60
     C                   MOVEL     SYFL12        XXFL12            1
     C     ##XFND        IFEQ      'N'
     C     XXFL12        IFEQ      'Y'
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '03'          XXNMBR
     C                   MOVE      'YNN'         XXMIS
     C                   EXSR      CHGOFA
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 01/24/08 *************************************
     C*
     C*** BEG ADD ***** 01/25/10 *************************************
     C     WCFL44        IFEQ      'Y'
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '63'          XXNMBR
     C                   MOVE      'YYN'         XXMIS
     C                   EXSR      CHGOFA
     C                   ENDIF
     C*** END ADD ***** 01/25/10 *************************************
     C*** begin del ***  04/21/16  ***********************************
     C*** BEG ADD ***** 02/02/11 *************************************
      *** BEG CHG ***** 07/08/11 *************************************
     C********** WCFL46    IFEQ 'Y'
     C*****XXCMS         IFEQ      'Y'
      *** END CHG ***** 07/08/11 *************************************
     C*******            MOVE      '1'           XXOPFN
     C*******            MOVE      '32'          XXNMBR
     C*******            MOVE      'YYN'         XXMIS
     C*******            EXSR      CHGOFA
     C*******            ENDIF
     C*** END ADD ***** 02/02/11 *************************************
     C*** end del *****  04/21/16  ***********************************
      /free
       //****** beg add ******* 09/01/15 *****************************
       xxOpFn = '1';
       xxNmbr = '41';
       xxMis  = 'NNN';
       Exsr ChgOfA;
       //****** end add ******* 09/01/15 *****************************
      /end-free
      *** BEG ADD ******* 07/02/10 ************************************
     C*===============================================================
     C* FOR OPT29 - EDI submission info
     C*===============================================================
     C                   READ      SWC02                                  95
     C     WCFL09        IFNE      'Y'
     C*
     C                   MOVE      '29'          XXNMBR
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      CHGOFA
     C                   ENDIF
      *** END ADD ******* 07/02/10 ************************************
     C*
     C*** BEGIN ADD ***  02/25/11  ************************************
     C                   Z-ADD     1             I2                2 0
     C*
     C     OF2(I2)       DOWNE     '3'
     C*
     C                   MOVEL     OFE(I2)       DUMNUM            2
     C                   MOVE(P)   OFE(I2)       XXPGM            10
     C*
     C     XXPGM         IFNE      *BLANKS
     C*
     C                   MOVEL     XXPGM         OBJNAM           10
     C                   MOVEL     '*PGM'        OBJTYP            7
     C                   CALL      'SRCHKOBJ'
     C                   PARM                    OBJNAM           10
     C                   PARM                    OBJTYP            7
     C                   PARM                    OBJERR            1
     C     OBJERR        IFEQ      *ON
     C                   MOVE      OF2(I2)       XXOPFN
     C                   MOVE      DUMNUM        XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      CHGOFA
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXPGM IFNE
     C*
     C                   ADD       1             I2                             INCREMENT COUNT
     C*
     C                   ENDDO                                                  OF2,I DOWNE
     C*
     C*** END ADD *****  02/25/11  ************************************
     C                   ENDSR                                                  PGMSTR
     C*
      *
      *
     C*===============================================================
     C* READ - SUBROUTINE TO READ THE FILE
     C*===============================================================
     C*
     C     READ          BEGSR
     C*
     C   21KEY1C         READE     DBRCDF                                 95
     C   22KEY1C         READE     DBRCDF2                                95
     C*
     C     *IN95         IFEQ      *ON
     C     ##XFND        ANDEQ     'Y'
      ***** BEG ADD ******* 06/02/99 **********************************
     C     ##RCDS        ANDEQ     *OFF
      ***** END ADD ******* 06/02/99 **********************************
     C                   MOVE      *ON           SNGITM
     C                   ENDIF
     C*
     C                   ENDSR                                                  READ
     C*
      *
      *
     C*===============================================================
     C* READP - SUBROUTINE TO READ THE FILE BACKWARDS
     C*===============================================================
     C*
     C     READP         BEGSR
     C*
     C   21KEY1C         READPE    DBRCDF                                 95
     C   22KEY1C         READPE    DBRCDF2                                95
     C*
     C                   ENDSR                                                  READP
     C*
      *
      *
     C*===============================================================
     C* SETGT - SUBROUTINE TO SET THE FILE > THE SELECTED FIELD
     C*===============================================================
     C*
     C     SETGT         BEGSR
     C*
     C   21KEY1          SETGT     DBRCDF                             96
     C   22KEY1B         SETGT     DBRCDF2                            96
     C*
     C                   ENDSR                                                  SETGT
     C*
      *
      *
     C*===============================================================
     C* SETLL - SUBROUTINE TO SET THE FILE < THE SELECTED FIELD
     C*===============================================================
     C*
     C     SETLL         BEGSR
     C*
     C   21KEY1          SETLL     DBRCDF                             96
     C   22KEY1B         SETLL     DBRCDF2                            96
     C*
     C                   ENDSR                                                  SETLL
     C*
      *
      *
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
      *
      *
     C*===============================================================
     C* SLCTRD - SUBROUTINE TO MOVE FILEDS FROM SELECTED RECORDS
     C*===============================================================
     C*
     C     SLCTRD        BEGSR
     C*
     C                   MOVE      ##CO#         @@CO#
     C                   MOVE      SCFND         @@FND
     C*
     C     *IN31         IFEQ      *ON                                          YR SEARCH
     C                   MOVE      SCFND         ##FND
     C                   EXSR      OPT8
     C                   MOVE      ##YR          @@FYR
     C     ##KL          CABEQ     *ON                                    90
     C                   ENDIF                                                  *IN31=*ON
     C*
     C                   ENDSR
     C*
      *
      *
     C*===============================================================
     C* SUBSET - SUBROUTINE TO SUBSET THE LIST OF GROUPS
     C*===============================================================
     C*
     C     SUBSET        BEGSR
     C*
     C     ##XFND        IFEQ      'Y'
      ***** BEG ADD ******* 06/02/99 **********************************
     C     ##RCDS        ANDEQ     *OFF
      ***** END ADD ******* 06/02/99 **********************************
     C     ##FND         ANDNE     FNFND
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      ***** BEG ADD ******* 06/02/99 **********************************
      *
     C                   MOVE      'A'           SCAUTH
      *
     C     ##XFND        IFEQ      'Y'
     C     ##RCDS        ANDEQ     *ON
     C                   CALL      'SA008R'      RSTPL1                 99
      *
     C     ##RTCD        IFEQ      *ON
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  ##RTCD=*ON
      *
     C                   ENDIF                                                  ##XFND=Y
      *
      ***** END ADD ******* 06/02/99 **********************************
     C*
B01  C     SSPDSC        IFNE      *BLANKS
     C*
     C     LO:UP         XLATE     FNNAME        U1PDSC
     C     LO:UP         XLATE     SSPDSC        U2PDSC
     C*
     C                   CALL      'QCLSCAN'
     C                   PARM                    U1PDSC
     C                   PARM      30            STRLEN            3 0
     C                   PARM      1             START             3 0
     C                   PARM                    U2PDSC
     C                   PARM      30            PATLEN            3 0
     C                   PARM      '0'           TRANS             1
     C                   PARM      '1'           TRIM              1
     C                   PARM      ' '           WILD              1
     C                   PARM                    SCNRES            3 0
     C*
B02  C     SCNRES        IFEQ      0
     C                   MOVE      *OFF          MEETSS
E02  C                   ENDIF                                                  SCNRES=0
     C*
E01  C                   ENDIF                                                  SSPDSC<>*BLANKS
     C*
     C                   ENDSR                                                  SUBSET
      *
      *
      *
** Headers - HD
 Grp   Group Name                                   Eff Date     Exp Date
 Group Name                                  Grp    Eff Date     Exp Date
 Grp   Group Name                                  Short Name
 Group Name
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1033=Copy                   NNN                            01/24/08
1055=Display                YYY
1077=Auxiliary              YYN
1088=Policy years           YYN
1099=Regions                YYN
11010=Incr limits           YYN
11111=Chapters              YYN
11212=Agent rates           YYN
11313=Reserve cushion       YYN
11414=DARTS                 YYN
11515=Billing auxiliary     YYN
11616=Admin/DARTS           YYN
11717=Claim/DARTS           YYN
11818=Expense constant      YYN
11919=Cx/Rein State Defin   YYN
12020=Bank info             YYN
12121=Harbor Exposure       YYN
12222=State Comp Benefits   YYN
12323=Short rates           YYN
12424=Compensation payment  YYN
12525=CX Confirmation Ctrl  YYN
12626=Direct debit control  YYN
12727=Policy Export         YYN
12828=Group Contact Info    YYN
12929=EDI Submission info   YYN
13030=Auto Claim Letters    YYN
13131=Auto DARTS Control    YYN
13232=Auto CMS Letters      NNN
13636=Return Funds          YYN
14040=Carrier Information   YYN
14141=Audit tracking        YYN
14545=Premium Finance Comp  YYN
14646=MCO group codes       YYN
14747=State rate change     YYN
14848=Billing Options       YYN
15050=Stock discounts       YYN
15151=Internal Comm Rates   NNN
15252=Policy Page Endors    YYN
15353=Midterm Endorsements  YYN
15454=Stat Rpt/Rating       YYN
15555=Deductible discount   YYN
15656=Endor/Policy Frm Info YYN
15757=Endors/DARTS          YYN
15858=Adt trk/DARTS control YYN
15959=Loss Constant         YYN
16060=Installment Fees      YYN
16161=Auto Cash Dissolve    YYN
16262=Rate Set Codes        YYN
16363=Fee information       NNN
16464=Flexible Info         YYN
16565=Policy Page-3C        YYN                            12/31/10
17575=Template Associations NNN                            11/27/18
18080=Client control info   YNN
19090=New clm diary DARTS rmNNN
19191=HCN Program           NNN                            11/10/10
19292=Excluded States/WCPOLSYYN
19393=Commission Mod FactorsNNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
207F7=                      NNN
208F8=                      NNN
209F9=Change Info           YYY
211F11=Change view          YYY
212F12=Cancel               NNY
3
** PROGRAM NAME
129W0066R
147W0047R
163W0063R
164W0010R64
165W0010R65
190W0069R90
191W0191R
3
