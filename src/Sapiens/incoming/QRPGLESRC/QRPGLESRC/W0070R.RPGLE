     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_Qry dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      * ***************************************************************
      /TITLE W0070R - CLAIM MASTER MAINTENANCE
      * ***************************************************************
      *                                                               *
      *  DESCRIPTION:  THIS PROGRAM WILL DISPLAY A LIST OF ALL CLAIMS *
      *                BY NAME OR SS#.  EACH CLAIM CAN BE ACCESSED    *
      *                FOR UPDATE, DISPLAY OR SELECTION DEPENDING UPON*
      *                THE CALLING PROGRAM.                           *
      *                                                               *
      *  CALLED FROM:                                                 *
      *    MENU                                                       *
      *                                                               *
      *  CALLS TO:                                                    *
      *    W0070C01 - ADD A CLAIM                                     *
      *      Several others in OPTnn Subroutines                      *
      *                                                               *
      *  CREATED: 01/02/97  PROGRAMMER: C:47  PROJECT: WC577          *
      * ***************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/21/13 C:65  Project: SY266  #3089                          *
     H*                Fix the call to WTSEARCH to clear division     *
     H*                criteria if there is no claim master record.   *
     H*                                                               *
     H* 01181/13 C:33  Project: SY266  #2186                          *
     H*                Use WTSEARCH for all calls to W0070R.          *
     H*                                                               *
     H* 01/11/13 C:287 Project: SY266_QRY                             *
     H*                                                               *
     H* 12/26/11  C:062  SUPPORT: 76459                               *
     H*                  PREVENT RECURSIVE CALL OF W0070R50.          *
     H*                                                               *
     H* 07/29/11 C:191 SUPPORT: 77565 WA099                           *
     H*                USE JRST INSTEAD OF FROI AS IT COULD BE A1 ETC *
     H*                                                               *
     H* 06/01/11 C: 53 PROJECT: 25WPW                                 *
     H*                Add Option 95 - Wage Calc
     H*                                                               *
     H* 04/14/11 C:255 SEQ:76527 - Compiled only to avoid level check *
     H*                            Error.                             *
     H*                                                               *
     H* 04/06/11 C: 58 PROJECT: 114WYU                                *
     H*                Add WMSIL, highlight claim if it has comment.  *
     H*                                                               *
     H* 03/25/11 C:235 PROJECT: WC1983                                *
     H*                Add processing for the FROI install flag.      *
     H*                                                               *
     H* 07/19/10 C:246 SEQ:73582 - Show JR State in Claims Summary    *
     H*                    Subset in WW Claims and WW claims.         *
     H*                                                               *
     H* 06/22/10 C:190 PROJECT: 25WQD - Additional Change             *
     H*                Trigger edit message if user attempts          *
     H*                to create a reserve worksheet on record        *
     H*                only claim.                                    *
     H*                                                               *
     H* 04/06/10 C:247 Project: 25WPT - Litigation_Scrn_Tracking      *
     H*                        Attorney Fees (Option 94)              *
     H*                                                               *
     H* 03/24/10 C:232 Project: 139WNN                                *
     H*                Update programs to use the new EDI Control API *
     H*                WA076R instead of using WMIXP.                 *
     H*                                                               *
     H* 03/23/10 C:235 Project: 139WNF                                *
     H*                EDI Screen Design                              *
     H*                                                               *
     H* 03/17/10 C:246 Project: 133WIL                                *
     H*                Fix the issue with the date showing incorrect  *
     H*                year                                           *
     H*                                                               *
     H*                                                               *
     H* 03/29/10 C:190 Project: 25WQD                                 *
     H*                Add option 93 = Reserve Worksheet,             *
     H*                conditioned on CLTID = '025'                   *
     H*                                                               *
     H* 03/12/10 C:246 Project: 133WIL                                *
     H*                Fix the PRISS 14 for duplicate date issue.     *
     H*                                                               *
     H* 03/08/10 C:246 Project: 133WIL                                *
     H*                Fix the PRISS issue on Displaying JR state.    *
     H*                                                               *
     H* 02/16/10 C:246 Project: 133WIL                                *
     H*                Add Jurisdiction State on W/W/Claims Screen    *
      *
     H* 10/05/09 C:33  Project: WC1770                                *
     H*                Add Opt 24=Add'l Claimants                     *
     H*                                                               *
     H* 07/28/09 C:65  PROJECT: WC1819                                *
     H*                ADD OPTION FOR CMS LOG HISTORY BASED ON        *
     H*                SYSTEM FLAG SSYPF/SYFL12.                      *
      *
     H* 06/29/09 C:191 WC1806                                         *
     H*                EFT - UPDATE CALL PARAMS TO W0028R            *
     H*                                                               *
     H* 07/30/08 C:136 SUPPORT: WA071 64575                           *
     H*                REMOVE TIME DELAY PER CG                       *
     H*                                                               *
      * 05/20/08  C:53 Project: CI054                                  *
      *                Add PARMS to W4315R call                        *
     H*                                                               *
     H*  02/25/08 C:180 SUPPORT 62448                                 *
     H*                 Fix display of Controvert/Denied status field.
     H*                                                               *
     H*  01/14/08 C:190 Project: 133WET                               *
     H*                  Change the "PRINT NOI" option to print the   *
     H*                  LUBA FROI (LUBA-WC-1007) if client Id = 133  *
     H*                  and CMJRST = 'LA'.                           *
     H*                                                               *
     H*  10/30/07 C:136 SUPPORT: WA132 60829                          *
     H*                 IMPLEMENT TIME DELAY                          *
     H*                                                               *
     H*  10/30/07 C:191 Project: WC1438-114WOK                        *
     H*                 Enable F12=Cancel when called from claims     *
     H*                 summary screen                                *
     H*                                                               *
     H* 10/19/07 C:191 PROJECT: WC1584                                *
     H*                Add Option 32 = EDI Info                       *
     H*                                                               *
     H*  09/20/07 C:136 PROJECT: WC997                                *
     H*                 ADD PARMS FOR CALL TO W/W EFT                 *
     H*                                                               *
     H*  08/09/07 C:191 Project: WC1453                               *
     H*                 Add option 70=EFT Account Info, Option not    *
     H*                 allowed if the EFT control Flag is not a 'Y'  *
     H*                                                               *
     H*  05/07/07 C:191 Project: WC1438                               *
     H*                 Allow F12=Cancel in maintenance mode          *
     H*                 Add option 23=Claim Summary,if flag is active *
     H*                                                               *
     H*  05/25/07 C:191 PROJECT: WC1515                               *
     H*                 Check claims authority                        *
     H*                                                               *
     H* 02/05/07 C:132 PROJECT: 51WHC                                 *
     H*                ADD Option 90 = MMI Info if client 051, Back   *
     H*                base with Project 51MCK.                       *
     H*                                                               *
     H* 01/12/07 C:15 PROJECT: 71WHI  SEQ# 55417                      *
     H*               ADD 2ND LINE FOR F15 TO INCLUDE EMP#, DIV,      *
     H*               ADJ, AND BODY PART.                             *
     H*
     H*  08/18/06 C:53  PROJECT: 114WMF
     H*                 Back to base 114MFA, 114WBY, 114WDF, 114WIH
     H*                             12/26/02 5/21/01 2/07/02 4/12/05
     H*
     H*  05/18/06 C:132 Support: WC132/57310                          *
     H*                 Changed option 30 to be not available when in *
     H*                 display mode - in table at end, no other chgs *
     H*                 in source code.                               *
     H*                                                               *
     H*  05/10/06 C:58 PROJECT: WC1383  Add staff code F10 search
     H*                                                               *
     H*  05/04/06 C:15  Support: WC135/52600                          *
     H*                 Changed code to allow more than one user to   *
     H*                 access the restricted claims, using the dtaara*
     H*                 CLAIMS.                                       *
     H*                                                               *
     H*  04/12/06 C:58 PROJECT: WC1383
     H*                Add staff and LT Flag subsets. Add automatic
     H*                staff code subset if user parm UPFL4 = Y.
     H*
     H*  03/06/06 C:132 Support: WC071 51160
     H*                Change Code to load DSDTTM and use DSDATE
     H*                instead of *DATE anywhere in the program.
     H*                                                               *
     H*  01/31/06 C:167 ROJECT: WC1382                                *
     H*                 Highlight Claim if Denied/Controvert          *
     H*                                                               *
     H*  09/06/05 C:33  Seq#: WA135/48771                             *
     H*                 Restrict claims.  This should only be a temp  *
     H*                  fix until a more permanent solution can be   *
     H*                  developed for all clients.                   *
     H*                                                               *
     H*  01/03/05 C:62  PROJECT: 132DAW                               *
     H*                 Add option 31 to print NOI.                   *
     H*                                                               *
     H*  03/03/04 C:58  Support:                                      *
     H*                 Add claim status to the subset screen; if not *
     H*                 blank, edit data and use in claim selection.  *
     H*                                                               *
     H*  02/25/04 C:15  Support:                                      *
     H*                 Always seton *INLR when exiting pgm.  Caused  *
     H*                 problems in VDARTS by not setting on *INLR.   *
     H*                                                               *
     H*  11/07/03 C:53  Project: WC1163                               *
     H*                 Change call to W0100R to W0600R               *
     H*                                                               *
     H*  12/11/01 C:136 Support: 29637                                *
     H*                 Check system file for installation of EDI     *
     H*                 before displaying Option 18, EDI Info.        *
     H*                                                               *
     H*  05/31/00 C:53  Project: WC461                                *
     H*                 Add Daily batch lockout process               *
     H*                                                               *
     H*  04/18/01 C:62  Project: MC082 26153                          *
     H*                 SET *IN09 OFF BEFORE PROCESS TO DETERMINE IF  *
     H*                 IT SHOULD BE SET ON.                          *
     H*                                                               *
     H*  09/01/00 C:15  Project: WC577                                *
     H*                 Added subsets for 'accident date' and 'name   *
     H*                 contains'.                                    *
     H*                                                               *
     H*  07/19/00 C:15  Support: WC065/23557                          *
     H*                 Fixed edit to prevent clms from being deleted *
     H*                 if any pmt records were processed within the  *
     H*                 current month.                                *
     H*                                                               *
     H*  03/28/00 C:53  PROJECT: SUP# 22263                           *
     H*                 PREVENT RECURSIVE CALL ISSUE                  *
     H*                                                               *
     H*  02/14/00 C:62  PROJECT: MC151                                *
     H*                 DON'T CALL F7090R IF PREVIOUS EDITS ENCOUNTER.*
     H*                                                               *
     H*  08/16/99 C:33  PROJECT: WC943                                *
     H*                 CHANGE ALL 'OFD' DATA STRUCTURE MANIUPLATIONS *
     H*                 TO USE THE SUBROUTINE 'UPDOFA'                *
     H*                                                               *
     H*  06/07/99 C:118 SUPPORT:  WC055/17826                         *
     H*                 ADDED IN EXSR UPDOFA IN PGMSTR TO REMOVE      *
     H*                 OPTIONS 66=Deduct history, 82=Rein policy     *
     H*                 and 83=Partial filing if flags are not on.    *
     H*                 Added SSY01 and SWC01 to use to check flags.  *
     H*                                                               *
     H* 06/02/99 C:61  PROJECT 101AT                                  *
     H*              - Add code for Group / Employer restrictions.    *
     H*                                                               *
     H*  05/26/99 C:118 SEQ#: WC048/16031                             *
     H*                 CHANGE 'XXST' TO 'CMJRST' TO ALLOW STATE SPEC-*
     H*                 IFIC WINDOW MESSAGE TO APPEAR FOR ALABAMA.    *
     H*                                                               *
     H*  05/10/99 C:33 SEQ#: WC051/17998                              *
     H*                ALLOW FOR DIVISION RESTRICTION.                *
      *                                                               *
     H*  03/30/99 C:33 PROJECT: WC577                                 *
     H*          C:118 RESET VALID OPTION FLAG AFTER READS TO BE SURE *
     H*                CORRECT RRN'S ARE LOADED.                      *
     H*
     H*  01/11/99 C:31  Project: MC152
     H*                 V4: Bill Entry re-write
     H*                 1. Add EOMB# to W0070R50 PARM list
     H*
      * 10/12/98 C:94 PROJECT: SUPPORT                                *
      *               ADDED CONDITIONING BASED ON IF THEY HAVE WCMC   *
      *               INSTALLED BEFORE DELETE IS CALLED               *
      *                                                               *
      * 09/16/98 C:94 PROJECT: MC151                                  *
      *               ADD LINKED DELETE TO MANAGED CARE SIDE          *
      *                                                               *
      * 08/19/98 C:15 Support: WC071/13302                            *
      *               Added an extra field to the key list for the    *
      *               SS# sort to prevent rolling problems when more  *
      *               than 10 claims exist for a claimant.            *
      *                                                               *
      * 07/16/98 C:33 PROJECT: WC833                                  *
      *               ADD F3 PARM TO CALL OF W0080R & W8080R.         *
      *                                                               *
      * 07/13/98 C:15 Support: WC051/12451                            *
      *               Removed a parm from parm list for the state     *
      *               specific pgms.                                  *
      *                                                               *
      *  06/24/98  C:33  SEQ#: WC051/12105                            *
      *                  ADD GROUP TO CALL OF W0310R - REINSURANCE    *
      *                                                               *
      *  04/28/98  C:15  PROJECT: WC770                               *
      *                  Added a call to SRCCSC to valid the user's   *
      *                  security when trying to delete a claim.      *
      *                                                               *
      *  04/14/98  C:15  PROJECT: WC770                               *
      *                  Added parameters to call F0269C to prevent   *
      *                  error.                                       *
      *                                                               *
      *  03/25/98  C:85  PROJECT: WC770                               *
      *                  ADDED PARAMETERS TO CALL W0310R FOR REINS.   *
      *                                                               *
      *  03/10/98  C:33  PROJECT: WC577    WC060/10529                *
      *                  CLEAR ALL KEY FIELDS ON POSITION TO          *
      *                                                               *
      *  03/04/98  C:15  PROJECT: WC577                               *
      *                  Fixed edit on using Opt 4 if the group has   *
      *                  expired.                                     *
      *                                                               *
      *  02/27/98  C:35  PROJECT: WC577                               *
      *                  CHANGED FLAGS ON VARIOUS OPTIONS IN THE      *
      *                  TABLE AT THE BOTTOM OF THE PROGRAM SO THAT   *
      *                  ONLY 1=SELECT & 5=DISPLAY ARE AVAILABLE IN   *
      *                  SELECT MODE.                                 *
      *                                                               *
      *  02/02/98  C:33/47 PROJECT: WC577                             *
      *                  USE DIFFERENT LOGICALS FOR GROUP/EMP SUBSETS *
      *                                                               *
      *  01/23/98  C:85  PROJECT: WC577                               *
      *                  ADDED OPTION 54,                             *
      *                  RECOVERY REQUEST MAINTENANCE.                *
      *                                                               *
      *  01/20/98  C:33  PROJECT: WC577                               *
      *                  USE DIFFERENT LOGICALS FOR GROUP SUBSETS     *
      *                                                               *
      *  01/07/98 C:15 Project: WC577                                 *
      *               -Do not display option 81 if WCMC is not        *
      *                installed.                                     *
      *                                                               *
      *    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
      *    -------------------------------------------------------    *
      * ***************************************************************
      *                                                               *
      *  INDICATOR USAGE:                                             *
      *  51 - Highlight claim yellow if summary comments present.
      *  54 - Automatic subset - staff code
      *                                                               *
      *  71 - Subset accident date invalid
      *  72 - Subset status invalid
      *  74 - Used to display reverse image if Claim is Denied        *
      *       or Controvert                                           *
      *  76 - Subset staff code invalid
      *  77 - Subset lost time flag invalid
      *
      *  90 - Used as general purpose error indicator                 *
      *  90 - Used as general purpose error indicator                 *
      *  91 - Used to display CPF5203; roll up/down past last record  *
      *  92 - Used to condition SFLMSGID for more than one opt 1      *
      *  93 - Used to display CPD6A7C; invalid option selected        *
      *  94 - Used to display CPF5304; invalid function key pressed   *
      *  95 - Used to condition SFLEND; also used to indicate EOF and
      *       BOF on the file being read.
      *  99 - Used for misc purpose; error on calls, on CHAINs
      *                                                               *
      * ***************************************************************
      *
      *  MAINLINE (INTERNAL) SUBROUTINE REFERENCE SUMMARY
      *
      *  CHKSSM - Used to set indicator 11 for subset message
      *  ERRCK1 - Used to determine errors on the subfile selection
      *  ERRCK2 - Used to determine errors on the subset selection
      *  INZSST - Used to initialize the subset selection fields
      *  KEYINZ - Used to initialize key values when program starts
      *  KEYSET - Used to set key values when using position to
      *  KEYSFL - Used to set key values from subfile when paging
      *  MVDBSC - Used to move data base fields to screen fields
      *  OTHFKS - Used to process other function keys
      *  OTHOPT - Used to process other options
      *  PGMEND - Used for any end of program processing
      *  PGMSTR - Used for any start of program processing
      *  READ   - Used to read next record from file
      *  READP  - Used to read previous record from file
      *  SETGT  - Used to set greater than on file
      *  SETLL  - Used to set lower limits on file
      *  SF1UPD - Used to set subfile fields when item is deleted.
      *  SLCTRD - Used to move fields for selected records
      *  SUBSET - Used to determine if the record read should be placed
      *           in the subfile.
      *
      * ***************************************************************
      *
      *
     FW0070F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     usropn
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     FWMCMP     IF   E             DISK
     F                                     RENAME(PCM01:PFRCDF)
      *
      * ALPHA SORT
     FWMCMLI    IF   E           K DISK    USROPN
     F                                     INFDS(DBFDS)
     F                                     RENAME(PCM01:DBRCDF)
      *
      * SS# SORT
     FWMCML9    IF   E           K DISK    USROPN
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PCM01:DBRCDF2)
      *
      * GROUP/YEAR/CASE SORT
     FWMCML1    IF   E           K DISK    USROPN
     F                                     INFDS(DBFDS3)
     F                                     RENAME(PCM01:DBRCDF3)
      *
      * STATE CLAIM # SORT
     FWMCMLG    IF   E           K DISK    USROPN
     F                                     INFDS(DBFDS4)
     F                                     RENAME(PCM01:DBRCDF4)
      *
     F* ALPHA SORT - CO#/FND/LSN/FSN/MID/SS#/FYR/CASE
     FWMCMLL    IF   E           K DISK
     F                                     INFDS(DBFDB)
     F                                     RENAME(PCM01:DBRCDB)
     F* SS# SORT - CO#/FND/SS#/FYR
     FWMCML2    IF   E           K DISK
     F                                     INFDS(DBFDB2)
     F                                     RENAME(PCM01:DBRCDB2)
     F* STATE CLAIM # SORT - CO#/FND/ACL#
     FWMCMLM    IF   E           K DISK
     F                                     INFDS(DBFDB4)
     F                                     RENAME(PCM01:DBRCDB4)
     F* ALPHA SORT - CO#/FND/EMP#/DIV/LSN/FSN/MID/SS#/FYR/CASE
     FWMCMLN    IF   E           K DISK
     F                                     INFDS(DBFDC)
     F                                     RENAME(PCM01:DBRCDC)
     F* SS# SORT - CO#/FND/EMP#/DIV/SS#/FYR
     FWMCMLO    IF   E           K DISK
     F                                     INFDS(DBFDC2)
     F                                     RENAME(PCM01:DBRCDC2)
     F* CASE SORT - CO#/FND/EMP#/DIV/FYR/CASE
     FWMCMLP    IF   E           K DISK
     F                                     INFDS(DBFDC3)
     F                                     RENAME(PCM01:DBRCDC3)
     F* STATE CLAIM # SORT - CO#/FND/EMP#/DIV/ACL#
     FWMCMLR    IF   E           K DISK
     F                                     INFDS(DBFDC4)
     F                                     RENAME(PCM01:DBRCDC4)
     FWMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWDF2L     IF   E           K DISK
     FWMFNP     IF   E             DISK
     F                                     RENAME(PFN01:PFN02)
     FWMCXL     IF   E           K DISK
     FWMCHL     IF   E           K DISK
     FWDPAL3    IF   E           K DISK
     FSDCMLA    IF   E           K DISK
     F                                     RENAME(PCM01:PCM03)
     FWTABL     IF   E           K DISK
     F                                     RENAME(PPA01:TPA01)
     F                                     IGNORE(PSY01)
     F                                     IGNORE(PPC01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PBC01)
     F                                     IGNORE(PCS01)
     FWMEML     IF   E           K DISK
     F                                     INFDS(DBFDSE)
     FWMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PEM0P)
     FWMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FSMUPP     IF   E             DISK
     F                                     RENAME(PUP01:PUP03)
     FWMALP     IF   E             DISK
     FSSYLF     IF   E           K DISK
     FWMXRL     IF   E           K DISK
     FSTSTL1    IF   E           K DISK
     FWMSIL     IF   E           K DISK
      /COPY SCOPYWW01D
     D*
     D HD              S             80    DIM(16) CTDATA PERRCD(1)             COLUMN HEADINGS
     D OF              S              1    DIM(60) CTDATA PERRCD(1)             OPTIONS, FUNCTIONS
     D OFD             S             35    DIM(60) ALT(OF)
     D INF             S              1    DIM(76)                              F9 INFO LINE
     D NM1             S              1    DIM(80)
     D NM2             S              1    DIM(80)
     D H1              S              1    DIM(80)
     D GRP             S              3    DIM(999)                             GROUP RESTRICTIONS
     D AHD             S             80    DIM(16) CTDATA PERRCD(1)             COLUMN HEADINGS
     D*
     D DBFDSE          DS
     D  DBRRNE               397    400B 0
     D DBFDB           DS
     D  DBRRB                397    400B 0
     D DBFDB2          DS
     D  DBRR2B               397    400B 0
     D DBFDB4          DS
     D  DBRR4B               397    400B 0
     D DBFDC           DS
     D  DBRRC                397    400B 0
     D DBFDC2          DS
     D  DBRR2C               397    400B 0
     D DBFDC3          DS
     D  DBRR3C               397    400B 0
     D DBFDC4          DS
     D  DBRR4C               397    400B 0
     D*
     D OPTNM           C                   CONST('Claims')
     D DELETD          C                   CONST('*Deleted')
     D NOVALU          C                   CONST('* No Value Entered *')
     D ACDPGM          C                   CONST('W0070C01')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D CTVCON          C                   CONST(' Controvert')
     D DNYCON          C                   CONST(' Denied    ')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D GRPAPI          C                   CONST('WA016R')
     D QRYPGM          C                   CONST('W0070C1')
     D OVRDB1          C                   CONST('OVRDBF FILE(')
     D OVRDB2          C                   CONST(') SHARE(*YES) -
     D                                     SEQONLY(*NO)')
     D VIEWMX          C                   CONST(4)
     D INFOMX          C                   CONST(4)
     D XCADJ           C                   CONST('Adjuster')
     D XLUBA           C                   CONST('W0070RLU')
     D*
     D CLMS            DS
     D  FRST1                  1      3
     D  CRST1                  4     10
     D  URST1                 11     20
     D  FRST2                 21     23
     D  CRST2                 24     30
     D  URST2                 31     40
     D  FRST3                 41     43
     D  CRST3                 44     50
     D  URST3                 51     60
     D  FRST4                 61     63
     D  CRST4                 64     70
     D  URST4                 71     80
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCFYR                  7      9  0
     D  QCCASE                10     16  0
     D                 DS
     D  XXDT                   1      7  0
     D  XXCY                   1      3  0
     D  XXMM                   4      5  0
     D  XXDD                   6      7  0
     D  XXYM                   1      5  0
     D                 DS
     D  XXDT7                  1      7  0
     D  XXYY7                  1      3  0
     D  XXMM7                  4      5  0
     D  XXDD7                  6      7  0
     D  XXYM7                  1      5  0
     D                 DS
     D  GYC                    1     15
     D  XAFND                  1      3
     D  XAFYR                  5      7
     D  XACASE                 9     15
     D                 DS
     D  CMA                    1     90  2
     D                                     DIM(10)                              PMT AMOUNT
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
     D  CMR                    1     90  2
     D                                     DIM(10)                              RESERVE AMOUNT
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
     D                 DS                  INZ
     D  TP                     1     10
     D                                     DIM(10)
     D  XXTP01                 1      1
     D  XXTP02                 2      2
     D  XXTP03                 3      3
     D  XXTP04                 4      4
     D  XXTP05                 5      5
     D  XXTP06                 6      6
     D  XXTP07                 7      7
     D  XXTP08                 8      8
     D  XXTP09                 9      9
     D  XXTP10                10     10
     D*
     D                 DS
     D  XXCLM                  1     13
     D  XXFND                  1      3
     D  XXFYR                  4      6
     D  XXCASE                 7     13
     D                 DS
     D  SYFL12                 1     10
     D  XXFL12                 2      2
     D*
     D/copy WTSEARCHDS
     D*
     DWTSEARCH         pr                  extPgm('WTSEARCH')
     D ##Mode                         1    const
     D ##Call                         1    const
     D ##RRN                          9
     D ##Criteria                          likeds(dsSrchCrit)
     D*
     D clmRRN          s              9    inz(*zeros)
     D useWTSEARCH     s              1n
     C*
     C     *LIKE         DEFINE    CMLSN         KKLSN
     C     *LIKE         DEFINE    CMFSN         KKFSN
     C     *LIKE         DEFINE    CMMID         KKMID
     C     *LIKE         DEFINE    CMSS#         KKSS#
     C     *LIKE         DEFINE    CMFND         KKFND
     C     *LIKE         DEFINE    CMFYR         KKFYR
     C     *LIKE         DEFINE    CMCASE        KKCASE
     C     *LIKE         DEFINE    CMACL#        KKACL#
     C     *LIKE         DEFINE    CMACL#        XXACL#
     C     *LIKE         DEFINE    CMFYR         QQYR
     C     *LIKE         DEFINE    CHOCST        SVOCST
     C     *LIKE         DEFINE    CMJRST        XXJRST
     C     *LIKE         DEFINE    CMDIV         NNDIV
     C     *LIKE         DEFINE    @@CALL        SVCALL
     C     *LIKE         DEFINE    *IN09         SVIN09
     C     *LIKE         DEFINE    CMFND         #@FND
     C     *LIKE         DEFINE    CMEMP#        ##EMP
     C     *LIKE         DEFINE    SYKEY         XSKEY
     C     *LIKE         DEFINE    CMACDT        XXACDT
     C     *LIKE         DEFINE    CMLSN         U1PDSC
     C     *LIKE         DEFINE    CMLSN         U2PDSC
     C     *LIKE         DEFINE    STKEY         STKY
     C*
     C                   MOVE      *BLANKS       XXCNAM           20
     C                   TIME                    DSDTTM
     C*
     C*  KEY LIST FOR THE CLAIM FILES
     C*
     C     CMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C*  KEY LIST FOR THE CLAIM/DARTS CONTROL FILE
     C*
     C     CXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFND
     C*
     C*  KEY LIST FOR THE DARTS FILE
     C*
     C     C3KY          KLIST
     C                   KFLD                    QCKEY
     C*
     C*  KEY LIST FOR THE EMPLOYER MASTER
     C*
     C     EMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C     EMKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C*
     C*  KEY LIST FOR THE EMPLOYER MASTER FOR SUBSET
     C*
     C     EMKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSFND
     C                   KFLD                    SSEMP#
     C                   KFLD                    NNDIV
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFND
      *
      *  CLAIM/EDI CONTROL FILE
      *
     C     IXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXJRST
     C                   KFLD                    XX148             3
     C*
     C*  REPOSITION KEYS
     C*
     C     KEY           KLIST
     C                   KFLD                    ##CO#
     C*
     C     KEY1          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKLSN
     C                   KFLD                    KKFSN
     C                   KFLD                    KKMID
     C                   KFLD                    KKSS#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     KEY2          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSS#
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     KEY3          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     KEY4          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKACL#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     KEYB          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSFND
     C*
     C     KEY1B         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSFND
     C                   KFLD                    KKLSN
     C                   KFLD                    KKFSN
     C                   KFLD                    KKMID
     C                   KFLD                    KKSS#
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     KEY2B         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSFND
     C                   KFLD                    KKSS#
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     KEY3B         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     KEY4B         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSFND
     C                   KFLD                    KKACL#
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     KEYC          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSFND
     C                   KFLD                    SSEMP#
     C*
     C     KEY1C         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSFND
     C                   KFLD                    SSEMP#
     C                   KFLD                    KKLSN
     C                   KFLD                    KKFSN
     C                   KFLD                    KKMID
     C                   KFLD                    KKSS#
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     KEY2C         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSFND
     C                   KFLD                    SSEMP#
     C                   KFLD                    KKSS#
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     KEY3C         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSFND
     C                   KFLD                    SSEMP#
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     KEY4C         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSFND
     C                   KFLD                    SSEMP#
     C                   KFLD                    KKACL#
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C*  TABLE FILE
     C*
     C     TBKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXKEY            10
     C*
     C*  KEY LIST FOR THE USER PARM FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  KEY LIST FOR THE SYSTEM CONTROL FILES
     C*
     C     SYKY          KLIST
     C                   KFLD                    XSKEY
     C     XRKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    Q1USER
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
      *
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@SORT            2
     C                   PARM                    @@RRN2            9 0
     C                   PARM                    @@FYR             3 0
     C                   PARM                    @@PGM            10
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    ##MODE
     C                   PARM      SCDBRR        ##RRN
     C* EMPLOYER SEARCH
     C     ACDPL1        PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM                    ##RRN             9 0
     C                   PARM                    ##RRN2            9 0
     C                   PARM      'W'           ##QTE             1
     C                   PARM                    ##VIEW            1 0
     C                   PARM                    ##RRN3            9 0
     C*
     C     ACDPL2        PLIST
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    ##MODE
     C                   PARM                    XACO#             3
     C                   PARM                    XAFND             3
     C                   PARM                    XAFYR             3
     C*
     C     ACDPL3        PLIST
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    ##MODE
     C                   PARM                    ##RRN
     C                   PARM                    ##RRN2
     C*
     C     ACDPL4        PLIST
     C                   PARM      'W'           ##CALL            1
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FND             3 0
     C                   PARM      SCFYR         ##FYR             3 0
     C                   PARM      SCCASE        ##CASE            7 0
     C                   PARM      *BLANKS       ##SS#             9
     C                   PARM      *ZEROS        ##ACDT            8 0
     C                   PARM      *BLANKS       ##RTCD            1
     C*
     C     ACDPL5        PLIST
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    ##MODE
     C                   PARM      SCDBRR        ##RRN
     C                   PARM      Q1PGM         ##PGM            10
     C*
     C* Parm List to be used to call NOI Reprint program
     C*
     C     NOIPL         PLIST
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    ##MODE
     C                   PARM      SCDBRR        ##RRN
     C                   PARM                    ##FROI            2
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
     C                   PARM                    ##XDIV            1
     C*
     C*  Group expired API parameter list
     C*
     C     GRPPL         PLIST
     C                   PARM      FNFND         ##FND
     C                   PARM      FNNAME        ##NAME           60
     C                   PARM      FNEXDT        ##EXDT            7 0
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     UPRMPL        PLIST
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      3             ##VIEW            1 0
     C*
     C     RSTPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM      Q1USER        ##USR            10
     C                   PARM      'W'           ##APPL            1
     C                   PARM      *OFF          ##RCDS            1
      *
     C     RSTPL1        PLIST
     C                   PARM                    ##CO#
     C                   PARM      Q1USER        ##USR
     C                   PARM      'W'           ##APPL
     C                   PARM      *OFF          ##RTCD            1
     C     SCAUTH        PARM                    ##AUTH            1
     C                   PARM      CMFND         #@FND
     C                   PARM      CMEMP#        ##EMP
      *
     C     RSTPLF        PLIST
     C                   PARM                    ##CO#
     C                   PARM      Q1USER        ##USR
     C                   PARM      'W'           ##APPL
     C                   PARM      *OFF          ##RTCD
     C                   PARM                    ##AUTH
     C                   PARM      FNFND         #@FND
     C*
     C     QRYPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##GRP1          255
     C                   PARM                    ##GRP2          255
     C                   PARM                    ##GRP3          255
     C                   PARM                    ##GRP4          255
     C                   PARM                    ##GRP5          255
     C                   PARM                    ##GRP6          255
     C                   PARM                    ##GRP7          255
     C                   PARM                    ##GRP8          255
     C                   PARM                    ##GRP9          255
     C                   PARM                    ##GRPA          255
     C                   PARM                    ##GRPB          255
     C                   PARM                    ##GRPC           64
     C*
     C* Parameter list for claims security API
     C     CUSPL         PLIST
     C                   PARM                    #ACO#             3
     C                   PARM                    #AFND             3
     C                   PARM                    #AFYR             3
     C                   PARM                    #ACASE            7
     C                   PARM      Q1USER        ##USR
     C                   PARM                    ##RTCD
     C*
     C     IXAPI         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    XXJRST
     C                   PARM                    XX148             3
     C                   PARM                    CMFND
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
      *
     C     O94PL         PLIST
     C                   PARM                    @@CALL
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQSEQ#
     C                   PARM                    QQRTCD
     C*
     C/COPY SCOPYWW01C
      /Free
       //==============================================================
       // Determine if WTSEARCH should be called and use it
       //==============================================================
       Begsr *Inzsr;

       Select;
       When @@call = ' ';
          useWTSEARCH = *on;
       When @@call = '1';
          useWTSEARCH = *on;
       When @@call = '2';
          useWTSEARCH = *on;
       Endsl;

       Select;
       When useWTSEARCH;
          Exsr CallWTSEARCH;
          // WTSEARCH will bypass the rest of the program
       Other;
      /If Defined (WEB)
          Open W0070F;
      /Endif
       Endsl;

       Endsr;

       //==============================================================
       // Call WTSEARCH instead of using W/W
       //==============================================================
       Begsr CallWTSEARCH;

       if %parms >= 5;
          Select;
          When @@mode = 'Z';
             Clear EMfnd;
             Clear EMemp#;
             Clear EMdiv;
             Chain(n) @@rrn pem0p;
             dsClmGrp  = EMfnd;
             dsClmEmp# = EMemp#;
             dsClmDiv  = %editc(EMdiv:'X');
          Other;
             Clear CMfnd;
             Clear CMfyr;
             Clear CMcase;
             Clear CMemp#;
             Clear CMdiv;
             Chain(n) @@rrn pfrcdf;
             dsClmGrp  = CMfnd;
             dsClmYr   = CMfyr;
             dsClmCase = CMcase;
             dsClmEmp# = CMemp#;
             dsClmDiv  = %editc(CMdiv:'X');
             //*** begin add ***  06/21/13  *****************************************
             If not %found;
                dsClmDiv = '     ';
             Endif;
             //*** end add *****  06/21/13  *****************************************
          Endsl;
       Endif;

       if %parms >= 7;
          Clear FNfnd;
          Chain(n) @@rrn2 pfn02;
          dsClmGrp = FNfnd;
       Endif;

       if %parms >= 8;
          dsClmYr = @@Fyr;
       Endif;

       Callp WTSEARCH ('C':@@call:clmRRN:dsSrchCrit);
       @@rrn = %int(clmRRN);

       *inlr = *on;
       Return;

       Endsr;
      /End-free
      *
     C*===============================================================
     C* CHGNUM - SUBROUTINE TO CHANGE NUMBERS TO ALPHA WITHOUT ZEROS
     C*===============================================================
     C*
     C     CHGNUM        BEGSR
     C*
     C                   CLEAR                   NM1
     C*
     C     '0'           CHECK     XXNUM         XXZERO            3 0
     C     ' '           CHECKR    XXNUM         XXLEN             3 0
     C     XXZERO        SUB       1             XXBLNK            3 0
     C                   MOVEA     XXNUM         NM1
     C*
     C                   DO        XXBLNK        X                 3 0
     C                   MOVE      ' '           NM1(X)
     C                   ENDDO                                                  X->XXBLNK
     C*
     C                   CLEAR                   NM2
     C*
     C                   SELECT
     C*
     C     EDTMSK        WHENEQ    'XXX,XXX'
     C*
     C                   Z-ADD     XXLEN         W                 3 0
     C     XXLEN         DIV       3             Y                 3 0
     C                   ADD       Y             XXLEN
     C                   Z-ADD     XXLEN         Z                 3 0
     C*
     C                   DO        XXLEN         X
     C*
     C     NM1(W)        IFNE      *BLANK
     C*
     C     X             DIV       4             Y
     C                   MVR                     Y
     C*
     C     Y             IFEQ      *ZERO
     C                   MOVEA     ','           NM2(Z)
     C                   ELSE
     C                   MOVE      NM1(W)        NM2(Z)
     C                   SUB       1             W
     C                   ENDIF                                                  Y=*ZERO
     C*
     C                   ENDIF                                                  NM1,W<>*BLANK
     C*
     C                   SUB       1             Z
     C*
     C                   ENDDO                                                  X->XXLEN
     C**
     C     EDTMSK        WHENEQ    'X,XXX.XX'
     C*
     C                   Z-ADD     XXLEN         W
     C     XXLEN         DIV       3             Y
     C                   ADD       Y             XXLEN
     C                   ADD       1             XXLEN
     C                   Z-ADD     XXLEN         Z
     C*
     C                   DO        XXLEN         X
     C*
     C     X             IFEQ      1
     C*
     C                   MOVEL     *BLANK        NM2(Z)
     C*
     C                   ELSE
     C*
     C     NM1(W)        IFNE      *BLANK
     C*
     C     X             DIV       4             Y
     C                   MVR                     Y
     C*
     C     Y             IFEQ      *ZERO
     C*
     C     X             IFEQ      4
     C                   MOVEA     '.'           NM2(Z)
     C                   ELSE
     C                   MOVEA     ','           NM2(Z)
     C                   ENDIF                                                  Z=4
     C*
     C                   ELSE
     C*
     C                   MOVE      NM1(W)        NM2(Z)
     C                   SUB       1             W
     C*
     C                   ENDIF                                                  Y=*ZERO
     C*
     C                   ENDIF                                                  NM1,W<>*BLANK
     C*
     C                   ENDIF                                                  X=1
     C*
     C                   SUB       1             Z
     C*
     C                   ENDDO                                                  X->XXLEN
     C**
     C     EDTMSK        WHENEQ    'DATE'
     C                   MOVEL(P)  XXNUM         XX10A            10
     C     XX10A         IFNE      *ZERO
     C     XX10A         ANDNE     *BLANK
     C*
     C                   Z-ADD     1             W
     C*
     C                   DO        10            X
     C*
     C     X             IFEQ      3
     C     X             OREQ      6
     C                   MOVEL     '/'           NM2(X)
     C                   ELSE
     C                   MOVEL     NM1(W)        NM2(X)
     C                   ADD       1             W
     C                   ENDIF                                                  X=3/6
     C*
     C                   ENDDO                                                  X->10
     C                   ENDIF                                                  NM1 <> BLANK
     C*
     C***  BEGIN ADD  ***  05/10/99  *********************************
     C                   OTHER
     C                   MOVEA     NM1(1)        XXNUM
     C***  END   ADD  ***  05/10/99  *********************************
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  CHGNUM
     C*
      *
      *
     C*===============================================================
     C* CHKSSM - SUBROUTINE FOR SUBSET EDITS
     C*===============================================================
     C     CHKSSM        BEGSR
     C*
     C     SNGEMP        IFEQ      *OFF
     C     SSFND         ANDNE     *ZEROS
     C                   MOVE      *ON           *IN11
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 09/01/00 *************************************
     C     SSACDT        IFNE      *ZEROS
     C                   Z-ADD     SSACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         SSACDT
     C     SRERR         IFEQ      '1'
     C                   SETON                                          7190
     C                   ENDIF
     C*
     C                   Z-ADD     SSACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         XXACDT
     C*
     C                   ENDIF
     C*** END ADD ***** 09/01/00 *************************************
      ***** BEG ADD ******* 03/03/04 **********************************
     C     SSSTAT        IFNE      *BLANK
     C     SSSTAT        IFNE      'C'
     C     SSSTAT        ANDNE     'O'
     C                   SETON                                          7590
     C                   ENDIF                                                  STAT <> C,O
     C                   ENDIF                                                  STAT <> ' '
      ***** END ADD ******* 03/03/04 **********************************
     C*** BEGIN ADD *** 04/12/06 **************************************
     C     SSSTAF        IFNE      *BLANK
     C     SSSTAF        CHAIN     PUP02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                          7690
     C                   ENDIF                                                   IN99=ON
     C                   ENDIF                                                  STAF <> *BLN
     C     SSLTFL        IFNE      *BLANK
     C     SSLTFL        IFNE      'M'
     C     SSLTFL        ANDNE     'I'
     C     SSLTFL        ANDNE     'R'
     C                   SETON                                          7790
     C                   ENDIF                                                  LTFL<>M,I,R
     C                   ENDIF                                                  LTFL <> *BLN
     C*** END ADD ***** 04/12/06 **************************************
     C*
     C                   ENDSR                                                  CHKSSM
     C*===============================================================
     C* CVTDT - SUBROUTINE TO CONVERT DATE FROM CYMD TO MDCY
     C*===============================================================
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
      *
     C*** BEG ADD *** 02/16/10 ***************************
     C*===============================================================
     C* CDT133- SUBROUTINE TO CONVERT DATE FROM CYMD TO MDYY
     C*===============================================================
     C     CDT133        BEGSR
     C*
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      '*MDY'        SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   ENDSR                                                  CDT133
      *
     C*** END ADD *****  02/16/10  ************************************
     C*===============================================================
     C* EDTDLT - THIS SUBROUTINE DETERMINES WHETHER THE CLAIM IS
     C*  ELIGIBLE FOR DELETION.  IF NO PMTS EXIST OR THE TOTAL OF ALL
     C*  PMTS = 0, THE CLAIM CAN BE DELETED.  IF THE TOTAL OF ALL PMTS
     C*  DOES NOT EQUAL 0 OR REQUEST PMTS EXIST, THE CLAIM CANNOT BE
     C*  DELETED.   THEN DELETE CLAIM. IF THERE ARE REQUEST PAYMENTS,
     C*  DO NOT ALLOW DELETE.
     C*===============================================================
     C     EDTDLT        BEGSR
     C*
     C                   Z-ADD     0             XXPMT            11 2
     C                   MOVE      'N'           XXCPMT            1
     C                   Z-ADD     SCFND         KKFND
     C                   Z-ADD     SCFYR         KKFYR
     C                   Z-ADD     SCCASE        KKCASE
     C*
     C**         CMKY      CHAINPRP01                99
     C**         *IN99     IFEQ '0'
     C**                   SETON                     859005
     C**                   ELSE
     C*
     C     CMKY          CHAIN     PPA01                              99
     C     *IN99         DOWEQ     '0'
     C*
     C     PATYPE        IFEQ      'V'
     C     PATYPE        OREQ      'S'
     C     PATYPE        OREQ      'R'
     C                   SUB       PAAMT         XXPMT
     C                   ELSE
     C                   ADD       PAAMT         XXPMT
     C                   END
     C*
     C*  DETERMINE WHETHER THIS TRANSACTION IS IN THE CURRENT MONTH.
     C*
     C                   Z-ADD     PATRDT        XXDT7
     C     XXYM7         IFEQ      XXYM
     C                   MOVE      'Y'           XXCPMT
     C                   END
     C*
     C     CMKY          READE     PPA01                                  99
     C                   ENDDO
     C*
     C*  IF THE PMT TOTAL IS NOT 0 OR PMT EXIST IN THE CURRENT
     C*  MONTH, DON'T ALLOW THE CLAIM TO BE DELETED.
     C*
     C  N90XXPMT         IFNE      0
     C                   SETON                                        859005
     C                   END
     C  N90XXCPMT        IFEQ      'Y'
     C                   SETON                                        869005
     C                   END
     C*
     C**************       END
     C*
     C*  IF ACTIVE DARTS RMKS EXIST, DISPLAY A MESSAGE WINDOW AND
     C*  ALLOW THE USER TO OVERRIDE THE ERROR MESSAGE.
     C*
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      ##CO#         QCCO#
     C                   MOVE      SCFND         QCFND
     C                   MOVE      SCFYR         QCFYR
     C                   MOVE      SCCASE        QCCASE
     C*
     C     C3KY          CHAIN     PCM03                              99
     C  N90*IN99         IFEQ      '0'
     C                   MOVE      'N'           SCOVR
     C                   EXFMT     WDWMSG
     C*
     C     SCOVR         IFEQ      'N'
     C                   SETON                                        879005
     C                   END
     C*
     C                   END
     C*
     C                   ENDSR                                                  EDTDLT
      *
      *
      *
     C*===============================================================
     C* ERRCK1 - SUBROUTINE TO CHECK SCREEN SELECTIONS
     C*===============================================================
     C     ERRCK1        BEGSR
     C*
     C*** BEG ADD *** 08/18/06 **********************************
     C                   MOVE      *OFF          *IN36
     C*** END ADD *** 08/18/06 **********************************
      ***** BEG ADD ***** 04/12/05 ***********************************
     C                   MOVE      *OFF          *IN31
     C     XRKY          CHAIN     PXR01                              60
     C     *IN60         IFEQ      *OFF
     C     XRSTAT        ANDEQ     'A'
     C     SCSEL         ANDNE     ' '
     C                   MOVE      *ON           *IN31
     C                   ENDIF
      ***** END ADD ***** 04/12/05 ***********************************
     C*
     C                   MOVE      'Y'           XXVLDO            1
      ***** BEG ADD ******* 06/02/99 **********************************
      *
     C                   SELECT
      *
      ***** BEG ADD ***** 04/12/05 ***********************************
     C     *IN31         WHENEQ    *ON
     C                   SETON                                            90
      ***** END ADD ***** 04/12/05 ***********************************
     C     SCSEL         WHENEQ    '2'
     C     SCAUTH        ANDEQ     'I'
     C     ##RCDS        ANDEQ     *ON
     C                   SETON                                        88  90
      *
     C     SCSEL         WHENEQ    '4'
     C     SCAUTH        ANDEQ     'I'
     C                   SETON                                        89  90
      *
     C     SCAUTH        WHENEQ    'I'
      *
     C     SCSEL         IFEQ      '51'
     C     SCSEL         OREQ      '52'
     C     SCSEL         OREQ      '53'
     C     SCSEL         OREQ      '54'
     C     SCSEL         OREQ      '82'
     C     SCSEL         OREQ      '83'
     C     SCSEL         OREQ      '90'                                         ADD 08/18/06
     C                   SETON                                        84  90
     C                   ENDIF                                                  SCSEL=4/51/52/53/54
      *
     C                   OTHER
      *
      ***** END ADD ******* 06/02/99 **********************************
     C*
     C*  THE USER CAN ONLY USE OPTION 4 (DELETE) IF THEIR SECURITY
     C*  LEVEL IS ADEQUATE.
     C*
     C                   Z-ADD     SCFND         KKFND
     C     CXKY          CHAIN     PCX01                              99
     C*** BEGIN CHG *** 04/28/98 **************************************
B01  C********** UPLVL     IFLT CXSECL                     MAINTENANCE
     C     *IN99         IFEQ      '0'
     C*** END CHG ***** 04/28/98 **************************************
B02  C     SCSEL         IFEQ      '4'
     C*
     C*** BEGIN ADD *** 04/28/98 **************************************
     C*  The user's security type also needs to be validated.
     C*
     C                   MOVE      UPLVL         ##ULVL
     C                   MOVE      CXSECL        ##CLVL
     C                   CALL      'SRCCSC'
     C                   PARM      UPTYPE        ##UTYP            1
     C                   PARM                    ##ULVL            3
     C                   PARM      CXCDT         ##CTP1            1
     C                   PARM                    ##CLVL            3
     C                   PARM      '1'           ##EXEC            1
     C                   PARM      ' '           ##RES             1
     C                   PARM      CXCDT2        ##CTP2            1
     C                   PARM      CXCDT3        ##CTP3            1
     C                   PARM      CXCDT4        ##CTP4            1
     C                   PARM      CXCDT5        ##CTP5            1
     C                   PARM      ' '           ##CTP6            1
     C*
     C     ##RES         IFEQ      '1'
     C*** END ADD ***** 04/28/98 **************************************
     C                   SETON                                        899005
     C*** BEGIN ADD *** 04/28/98 **************************************
     C                   ENDIF
     C*** END ADD ***** 04/28/98 **************************************
     C*
E02  C                   ENDIF
E01  C                   ENDIF
      *
      *  IF GROUP IS EXPIRED, THEN DISPLAY INFO WINDOW
     C*** BEGIN ADD *** 04/18/01 **************************************
     C     SCSEL         IFEQ      '4'
     C                   SETOFF                                       09
     C                   ENDIF
     C*** END   ADD *** 04/18/01 **************************************
      *
     C     FNKY          CHAIN     PFN01                              60
     C  N90
     CANN09*IN60         IFEQ      *OFF
     C*
     C     FNEXDT        IFGT      *ZERO
     C*
     C     FNEXDT        IFLE      XXDT
     C                   CALL      GRPAPI        GRPPL
     C                   MOVE      '1'           QQCALL
     C                   MOVE      *ON           *IN09
     C                   ENDIF                                                  FNEXDT<=XXDT
     C*
     C                   ENDIF                                                  FNEXDT>*ZERO
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C*  IF THE GROUP IS EXPIRED, DON'T ALLOW DELETION.
     C*
B01  C     SCSEL         IFEQ      '4'
     C     *IN90         ANDNE     *ON
     C*
     C     FNEXDT        IFGT      *ZERO
     C     FNEXDT        ANDLE     XXDT
     C*** BEGIN DLT *** 03/04/98 ***********************************************
     C******************** MOVE *ON       *IN90
     C******************** WRITEMSGSFLC
     C******************** WRITESCR01
     C*** END DLT ***** 03/04/98 ***********************************************
     C*** BEGIN ADD *** 03/04/98 ***********************************************
     C                   SETON                                        7390
     C*** END ADD ***** 03/04/98 ***********************************************
E01  C                   ENDIF
     C*
E01  C                   ENDIF
     C*
     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C*  IF DELETING A CLAIM, CHECK FOR PMTS AND DIARIES
     C*
B01  C     SCSEL         IFEQ      '4'
     C     *IN90         ANDNE     *ON
     C     *IN09         ANDNE     *ON
     C                   EXSR      EDTDLT
     C*** BEGIN ADD *** 02/14/00 **************************************
     C     *IN90         IFEQ      *OFF
     C*** END   ADD *** 02/14/00 **************************************
     C*** BEG ADD *** 10/12/98 **************************************
     C     ##MCIF        IFEQ      'Y'
     C     ##WCIN        ANDEQ     'Y'
     C*** END ADD *** 10/12/98 **************************************
     C*** BEG ADD *** 09/16/98 **************************************
     C*
     C*   FIRST YOU NEED TO GET THE RRN OF THE CASE YOU ARE CHANGING
     C*
     C                   MOVE      'X'           ##FLAG            1
     C     ##FLAG        IFEQ      'X'
     C                   MOVE      ##CO#         XYCO#             3
     C                   MOVE      KKFND         XYFND             3
     C                   MOVE      KKFYR         XYFYR             3
     C                   MOVE      KKCASE        XYCASE            7
     C*
     C                   CALL      'FA201R'
     C                   PARM                    XYCO#
     C                   PARM                    XYFND
     C                   PARM                    XYFYR
     C                   PARM                    XYCASE
     C                   PARM      *ZEROS        XYRRN1            9 0
     C                   PARM      *BLANK        XYRTCD            1
     C*
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
     C*   NOW YOU CALL F7090R TO DO THE DELETE
     C*
     C                   CALL      'F7090R'
     C                   PARM      ' '           ##CALL            1
     C                   PARM                    ##KL              1
     C                   PARM      XYRRN1        ##RRN             9 0
     C                   PARM      *ZEROS        ##RRN1            9 0
     C                   PARM      *ZEROS        ##RRN2            9 0
     C                   PARM      *ZEROS        ##RRN3            9 0
     C                   PARM      'D'           ##TD              1
     C                   PARM      'X'           ##FLAG            1
     C                   PARM      'Y'           ##WCMS            1
     C*
     C     ##KL          IFEQ      *ON
     C                   SETON                                        90
     C                   ENDIF
     C*
     C                   ENDIF                                                  IF FLAG = Y
     C*** END ADD *** 09/16/98 **************************************
     C*** BEG ADD *** 10/12/98 **************************************
     C                   ENDIF                                                  IF MCIF = Y
     C*** END ADD *** 10/12/98 **************************************
     C*** BEGIN ADD *** 02/14/00 **************************************
     C                   ENDIF                                                  *IN90 = *OFF
     C*** END   ADD *** 02/14/00 **************************************
E01  C                   ENDIF
     C*
     C* CLAIM IS CLOSED MESSGAGE
     C*
     C  N90SCSEL         IFEQ      '53'
     C     SCSTAT        ANDEQ     'C'
     C                   SETON                                        72  90
     C                   ENDIF                                                  SCSEL=53
      ***** BED ADD ******* 06/02/99 **********************************
     C*** BEG   ADD ***06/22/10 ***************************************
     C*********            MOVE *OFF      *IN97
     C  N90SCSEL         IFEQ      '93'
     C     CMLTFL        ANDEQ     'R'
     C                   SETON                                        97  90
     C                   ENDIF                                                  SCSEL=93
     C*** END   ADD ***06/22/10 ***************************************
      *
     C                   ENDSL
      ***** END ADD ******* 06/02/99 **********************************
     C*** BEG ADD *** 08/18/06 **********************************
     C     SCSEL         IFEQ      '90'
     C*
     C     SCDBRR        CHAIN     PFRCDF                             99
     C                   MOVE(P)   CMFND         KKFND
     C     FNKY          CHAIN     PF201                              99
     C*
     C** LOAD RESERVE INFO
     C*
     C                   DO        10            I
     C                   MOVEL(P)  RT(I)         XXKEY
     C     TBKY          CHAIN     PRT01                              60
     C  N60              MOVEL     RTTYPE        TP(I)
     C   60              MOVE      *BLANK        TP(I)
     C                   ENDDO
     C*
     C                   Z-ADD     1             I
     C     'C'           LOOKUP    TP(I)                                  60
     C*
     C                   SELECT
     C     CMLTFL        WHENNE    'I'
     C                   SETON                                        36  90
     C*
     C     *IN60         WHENEQ    *OFF
     C                   SETON                                        75  90
     C*
     C     F2CLCK        WHENEQ    'N'
     C                   SETON                                        76  90
     C                   ENDSL
     C*
     C                   ENDIF                                                  SCSEL=90
     C*** END ADD *** 08/18/06 **********************************
     C*
     C                   ENDSR                                                  ERRCK1
      *
      *
      *
     C*===============================================================
     C* ERRCK2 - SUBROUTINE TO CHECK SUBSET SELECTIONS
     C*===============================================================
     C     ERRCK2        BEGSR
     C*
     C     SSFND         IFEQ      *ZEROS
     C                   MOVE      *ON           *IN80
     C                   ENDIF
     C*
     C     SSYEAR        IFEQ      *ZEROS
     C                   MOVE      *ON           *IN80
     C                   ENDIF
     C*
     C     SSEMP#        IFEQ      *ZEROS
     C     SSDIV         ANDNE     *BLANKS
     C                   MOVE      *ON           *IN80
     C                   ENDIF
     C*
     C     SSEMP#        IFEQ      *ZEROS
     C     SSCTG1        ANDNE     *BLANKS
     C                   MOVE      *ON           *IN80
     C                   ENDIF
     C*
      * PROCESS SEARCHES
      *
     C     *INKJ         IFEQ      *ON
B01  C                   SELECT
     C*
     C     HFLD          WHENEQ    'SSFND'
     C                   MOVE      ##CO#         XACO#
     C                   MOVE      *ZERO         XAFND
     C                   MOVE      *ZERO         XAFYR
     C                   MOVE      '2'           ##MODE
     C                   CALL      'W0010R'      ACDPL2                 99
     C                   MOVE      XAFND         SSFND
     C*
     C***  BEGIN ADD  ***  05/11/06  *********************************
     C*
     C*   STAFF CODE SEARCH - SUBSET
     C*
     C     HFLD          WHENEQ    'SSSTAF'
     C                   CALL      'S0005R'      UPRMPL
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PUP03                              99
     C*
     C                   MOVEL(P)  UPSTAF        SSSTAF
     C                   MOVE      *OFF          *IN64
     C                   MOVE      *ON           *IN55
     C                   ELSE
     C                   MOVE      *ON           *IN64
     C                   MOVE      *OFF          *IN55
     C                   END
     C*
     C*
     C***  END ADD ** ***  05/11/06  *********************************
     C*
     C     HFLD          WHENEQ    'SSEMP#'
     C***  BEGIN ADD  ***  05/10/99  *********************************
     C     *IN34         ANDEQ     *OFF
     C     HFLD          OREQ      'SSDIV'
     C     *IN35         ANDEQ     *OFF
     C***  END   ADD  ***  05/10/99  *********************************
     C                   MOVE      '2'           ##MODE
     C                   Z-ADD     *ZEROS        ##RRN3
     C                   CALL      'W0050R'      ACDPL1                 99
     C     ##RRN         CHAIN     PEM0P                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EMFND         SSFND
     C                   MOVE      EMEMP#        SSEMP#
     C***  BEGIN CHG  ***  05/10/99  *********************************
     C**********           MOVE EMDIV     SSDIV
     C*
     C     EMDIV         IFNE      *ZERO
     C                   MOVEL(P)  EMDIV         XXNUM
     C                   MOVEL     *BLANKS       EDTMSK
     C                   EXSR      CHGNUM
     C                   MOVEL     XXNUM         SSDIV
     C                   ELSE
     C                   MOVE      '0'           SSDIV
     C                   ENDIF                                                  EMDIV<>*ZERO
     C***  END   CHG  ***  05/10/99  *********************************
     C*
E01  C                   END
     C*
     C     HFLD          WHENEQ    'SSCTG1'
      *
     C     SSEMP#        IFNE      *ZERO
      *
      * MAKE SURE WE HAVE A GROUP
      *
     C     SSFND         IFEQ      *ZERO
     C                   MOVE      ##CO#         XACO#
     C                   MOVE      *ZERO         XAFND
     C                   MOVE      *ZERO         XAFYR
     C                   MOVE      '2'           ##MODE
     C                   CALL      'W0010R'      ACDPL2                 99
     C                   MOVE      XAFND         SSFND
E01  C                   END
     C*
     C                   MOVE      SSDIV         NNDIV
     C     EMKY1         CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     DBRRNE        ##RRN2
E01  C                   END
     C                   END                                                    SSEMP# <> ZERO
     C*
     C     SSEMP#        IFEQ      *ZERO
     C                   MOVE      '2'           ##MODE
     C                   Z-ADD     *ZEROS        ##RRN2
     C                   Z-ADD     *ZEROS        ##RRN3
     C                   CALL      'W0050R'      ACDPL1                 99
     C     ##RRN         CHAIN     PEM0P                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EMFND         SSFND
     C                   MOVE      EMEMP#        SSEMP#
     C***  BEGIN CHG  ***  05/10/99  *********************************
     C**********           MOVE EMDIV     SSDIV
     C*
     C     EMDIV         IFNE      *ZERO
     C                   MOVEL(P)  EMDIV         XXNUM
     C                   MOVEL     *BLANKS       EDTMSK
     C                   EXSR      CHGNUM
     C                   MOVEL     XXNUM         SSDIV
     C                   ELSE
     C                   MOVE      '0'           SSDIV
     C                   ENDIF                                                  EMDIV<>*ZERO
     C***  END   CHG  ***  05/10/99  *********************************
     C*
     C                   Z-ADD     ##RRN         ##RRN2
     C                   END
     C                   END
      *
     C     ##RRN         IFNE      *ZERO
     C     ##RRN2        ANDNE     *ZERO
     C                   MOVE      '2'           ##MODE
     C                   Z-ADD     *ZERO         ##RRN
     C                   CALL      'W0076R'      ACDPL3                 99
      *
     C     ##RRN         CHAIN     PAL01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      ALELID        SSCTG1
     C                   END
      *
     C                   END                                                    ##RRN <> ZERO
     C*
     C***  BEGIN ADD  ***  05/10/99  *********************************
     C                   OTHER
     C                   SETON                                            90
     C***  END   ADD  ***  05/10/99  *********************************
     C*
     C                   ENDSL
     C*
     C                   END                                                    *INKJ
     C*
     C                   ENDSR                                                  ERRCK2
      *
      *
      *
      ***** BEG ADD ******* 06/02/99 **********************************
      *===============================================================
      * GETGRP - SUBROUTINE TO GET GROUPS USER IS AUTHORIZED TO
      *===============================================================
      *
     C     GETGRP        BEGSR
      *
     C                   CLEAR                   GRP
      *
     C                   Z-ADD     1             X                 3 0
      *
     C     ##CO#         CHAIN     PFN01                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C                   CALL      'SA008R'      RSTPLF
      *
     C     ##RTCD        IFEQ      *OFF
     C                   MOVE      FNFND         GRP(X)
     C                   ADD       1             X
     C                   ENDIF                                                  ##RTCD=*OFF
      *
     C     ##CO#         READE     PFN01                                  60
      *
     C                   ENDDO                                                  *IN60=*OFF
      *
     C                   MOVEA(P)  GRP(1)        ##GRP1
     C                   MOVEA(P)  GRP(86)       ##GRP2
     C                   MOVEA(P)  GRP(171)      ##GRP3
     C                   MOVEA(P)  GRP(256)      ##GRP4
     C                   MOVEA(P)  GRP(341)      ##GRP5
     C                   MOVEA(P)  GRP(426)      ##GRP6
     C                   MOVEA(P)  GRP(511)      ##GRP7
     C                   MOVEA(P)  GRP(596)      ##GRP8
     C                   MOVEA(P)  GRP(681)      ##GRP9
     C                   MOVEA(P)  GRP(766)      ##GRPA
     C                   MOVEA(P)  GRP(851)      ##GRPB
     C                   MOVEA(P)  GRP(936)      ##GRPC
      *
     C                   ENDSR                                                  GETGRP
      ***** END ADD ******* 06/02/99 **********************************
      *
      *
      *
     C*===============================================================
     C* INZSST - SUBROUTINE TO INITIALIZE SUBSET SELECTIONS
     C*===============================================================
     C     INZSST        BEGSR
     C*
     C     SNGEMP        IFEQ      *OFF
     C*
     C*  GET GROUP INFO (IF CALLED FOR)
     C*
     C                   MOVE      *OFF          *IN33
     C*
     C     Q1PRMS        IFGE      7
     C     @@RRN2        ANDNE     *ZERO
     C*
     C     @@RRN2        CHAIN     WMFNP                              60
     C                   CLEAR                   @@RRN2
     C                   Z-ADD     FNFND         SSFND
     C                   MOVE      *ON           *IN33
     C*
     C                   ELSE
     C*
     C                   Z-ADD     0             SSFND
     C*
     C                   ENDIF                                                  Q1PRMS>=7/@@RRN2<>0
     C*
     C                   Z-ADD     0             SSEMP#
     C                   MOVE      *BLANKS       SSDIV
     C                   MOVE      *BLANKS       SSCTG1
     C*
     C                   ENDIF                                                  SNGEMP=*OFF
     C*
     C     Q1PRMS        IFGE      8
     C                   Z-ADD     @@FYR         SSYEAR
     C                   ELSE
     C                   Z-ADD     *ZERO         SSYEAR
     C                   ENDIF                                                  Q1PRMS>=8
     C*
     C     ##XFND        IFEQ      'Y'
     C     ##XEMP        OREQ      'Y'
      ***** BEG ADD ******* 06/02/99 **********************************
     C     ##RCDS        IFEQ      *OFF
      ***** END ADD ******* 06/02/99 **********************************
     C                   MOVEL     ##FND         SSFND
     C                   SETON                                        33
      ***** BEG ADD ******* 06/02/99 **********************************
     C                   ENDIF                                                  ##RCDS
      ***** END ADD ******* 06/02/99 **********************************
     C                   ENDIF
     C*
     C     ##XEMP        IFEQ      'Y'
      ***** BEG ADD ******* 06/02/99 **********************************
     C     ##RCDS        ANDEQ     *OFF
      ***** END ADD ******* 06/02/99 **********************************
     C*
     C                   MOVEL     UPEMP#        SSEMP#
     C                   SETON                                        34
     C*
     C***  BEGIN CHG  ***  05/10/99  *********************************
     C**********           MOVELUPDIV     SSDIV
     C     ##XDIV        IFEQ      'Y'
     C*
     C     UPDIV         IFNE      *ZERO
     C                   MOVEL(P)  UPDIV         XXNUM
     C                   MOVEL     *BLANKS       EDTMSK
     C                   EXSR      CHGNUM
     C                   MOVEL     XXNUM         SSDIV
     C                   ELSE
     C                   MOVE      '0'           SSDIV
     C                   ENDIF                                                  UPDIV<>*ZERO
     C*
     C                   SETON                                        35
     C*
     C                   ELSE
     C*
     C                   SETOFF                                       35
     C*
     C                   ENDIF                                                  ##XDIV=Y
     C***  END   CHG  ***  05/10/99  *********************************
     C*
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 09/01/00 *************************************
     C                   Z-ADD     0             SSACDT
     C                   MOVE      *BLANKS       SSNAME
     C*** END ADD ***** 09/01/00 *************************************
      ***** BEG ADD ******* 03/03/04 **********************************
     C                   MOVEL     *BLANKS       SSSTAT
      ***** END ADD ******* 03/03/04 **********************************
     C*** BEGIN ADD *** 04/12/06 **************************************
     C                   MOVEL     *BLANKS       SSLTFL
     C     *IN54         IFEQ      *ON
     C                   MOVEL(P)  UPSTAF        SSSTAF
     C                   ELSE                                                   x- IN54=ON
     C                   MOVEL     *BLANKS       SSSTAF
     C                   ENDIF                                                  e- IN54=ON
     C*** END ADD ***** 04/12/06 **************************************
     C*
     C                   ENDSR                                                  INZSST
      *
     C*===============================================================
     C* KEYINZ - SUBROUTINE TO INITIALIZE THE REPOSITION KEYS
     C*===============================================================
     C     KEYINZ        BEGSR
     C*
     C                   MOVE      *LOVAL        KKFSN
     C                   MOVE      *LOVAL        KKLSN
     C                   MOVE      *LOVAL        KKMID
     C                   MOVE      *LOVAL        KKSS#
     C                   MOVE      *LOVAL        KKFND
     C                   MOVE      *LOVAL        KKFYR
     C                   MOVE      *LOVAL        KKCASE
     C                   MOVE      *LOVAL        KKACL#
     C*
     C                   ENDSR                                                  KEYINZ
      *
      *
      *
     C*===============================================================
     C* KEYSET - SUBROUTINE TO SET THE KEYS FOR REPOSITIONING
     C*===============================================================
     C     KEYSET        BEGSR
     C*
     C***  BEGIN ADD  ***  03/10/98  *********************************
     C                   CLEAR                   KKLSN
     C                   CLEAR                   KKFSN
     C                   CLEAR                   KKMID
     C                   CLEAR                   KKSS#
     C                   CLEAR                   KKFND
     C                   CLEAR                   KKFYR
     C                   CLEAR                   KKCASE
     C***  END   ADD  ***  03/10/98  *********************************
     C*
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C                   MOVEL(P)  SCPST1        KKLSN
     C                   MOVEL(P)  SCPS1B        KKFSN
     C                   MOVEL(P)  SCPS1C        KKMID
     C                   MOVE      *LOVAL        KKSS#
     C                   MOVE      *LOVAL        KKFND
     C                   MOVE      *LOVAL        KKFYR
     C                   MOVE      *LOVAL        KKCASE
      *
     C     *IN22         WHENEQ    *ON
     C                   MOVE(P)   SCPST2        KKSS#
     C                   MOVE      *LOVAL        KKFYR
     C                   MOVE      *LOVAL        KKCASE
      *
     C     *IN23         WHENEQ    *ON
     C                   MOVE(P)   SCPST3        KKFND
     C                   MOVE(P)   SCPS3B        KKFYR
     C                   MOVE(P)   SCPS3C        KKCASE
      *
     C     *IN24         WHENEQ    *ON
     C                   MOVEL(P)  SCPST4        KKACL#
     C*** BEGIN ADD *** 08/19/98 **************************************
     C                   MOVE      *LOVAL        KKFYR
     C                   MOVE      *LOVAL        KKCASE
     C*** END ADD ***** 08/19/98 **************************************
     C                   ENDSL
     C*
     C                   ENDSR                                                  KEYSET
      *
      *
      *
     C*===============================================================
     C* KEYSFL - SUBROUTINE TO SET THE KEYS FOR PAGE UP AND PAGE DOWN
     C*===============================================================
     C     KEYSFL        BEGSR
     C*
     C                   MOVEL(P)  SCLSN         KKLSN
     C                   MOVEL(P)  SCFSN         KKFSN
     C                   MOVEL(P)  SCMID         KKMID
     C                   MOVE(P)   SCSS#         KKSS#
     C                   MOVE(P)   SCFND         KKFND
     C                   MOVE(P)   SCFYR         KKFYR
     C                   MOVE(P)   SCCASE        KKCASE
     C                   MOVEL(P)  SCACL#        KKACL#
     C*
     C                   ENDSR                                                  KEYSFL
      *
      *
      *
     C*===============================================================
     C* MVDBSC - SUBROUTINE TO LOAD SUBFILE FIELDS
     C*===============================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVE      @@CALL        QQCALL            1
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C*
     C***  BEGIN ADD  ***  01/20/98  *********************************
B01  C     XXVLDO        IFNE      'Y'
B01  C     SSFND         IFNE      *ZEROS
     C*
     C***  BEGIN ADD  ***  02/02/98  *********************************
B01  C     SSEMP#        IFNE      *ZEROS
     C*
     C   21              Z-ADD     DBRRC         SCDBRR
     C   22              Z-ADD     DBRR2C        SCDBRR
     C   23              Z-ADD     DBRR3C        SCDBRR
     C   24              Z-ADD     DBRR4C        SCDBRR
     C*
     C                   ELSE
     C***  END   ADD  ***  02/02/98  *********************************
     C*
     C   21              Z-ADD     DBRRB         SCDBRR
     C   22              Z-ADD     DBRR2B        SCDBRR
     C   24              Z-ADD     DBRR4B        SCDBRR
     C*
     C***  BEGIN ADD  ***  02/02/98  *********************************
E01  C                   ENDIF                                                  SSEMP#<>*ZEROS
     C***  END   ADD  ***  02/02/98  *********************************
E01  C                   ENDIF                                                  SSFND<>*ZEROS
     C***** BEGIN DEL **** 03/30/99 ***********************************
E01  C**********           ELSE
     C**********           MOVE 'N'       XXVLDO
     C***** END DEL ****** 03/30/99 ***********************************
E01  C                   ENDIF                                                  XXVLDO
     C***  END   ADD  ***  01/20/98  *********************************
     C*
     C                   MOVE      CMSS#         SCSS#
     C*
     C                   MOVEL(P)  CMLSN         SCLSN
     C                   MOVEL(P)  CMFSN         SCFSN
     C                   MOVEL(P)  CMMID         SCMID
      *
     C     CMLSN         CAT(P)    ',':0         XXCNAM
     C                   CAT       CMFSN:1       XXCNAM
     C                   CAT       CMMID:1       XXCNAM
      *
     C                   MOVEL(P)  CMACL#        SCACL#
     C     CMACL#        IFEQ      *BLANK
     C***  BEGIN CHG  ***  01/20/98  *********************************
     C**********           MOVELNOVALU    SCACL#    P
     C                   MOVEL(P)  NOVALU        XXACL#
     C                   ELSE
     C                   MOVEL(P)  CMACL#        XXACL#
     C***  END   CHG  ***  01/20/98  *********************************
     C                   ENDIF
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * F9=CHANGE INFO
     C                   Z-ADD     1             A                 3 0
     C                   MOVE      *BLANK        SCDATA
     C                   CLEAR                   INF
      ** **
      ** GROUP, YEAR, CASE, ACCIDENT DATE, LOST TIME STATUS, CLAIM STATUS,
      **   REOPEN DATE
      ** **
     C*** BEG ADD *** 02/16/10 ***************************
     C*** BEG CHG *** 07/19/10 ***************************
     C**         CLTID     IFNE '133'
     C*** END CHG *** 07/19/10 ***************************
     C*** END ADD *** 02/16/10 ***************************
     C**                   SELEC
     C**         INFO#     WHEQ 1
     C**                   MOVELCMFND     XXNUM     P
     C**                   EXSR CHGNUM
     C**                   MOVEANM1       INF,A
      *
     C**                   Z-ADD5         A
     C**                   MOVELCMFYR     XXNUM     P
     C**                   EXSR CHGNUM
     C**                   MOVEANM1       INF,A
     C**
     C**                   Z-ADD9         A
     C**                   MOVELCMCASE    XXNUM     P
     C**                   EXSR CHGNUM
     C**                   MOVEANM1       INF,A
      **
     C**                   Z-ADD17        A
     C**                   Z-ADDCMACDT    SRDTI
     C**                   EXSR CVTDT
     C**                   MOVELSRDTO     XXNUM  80 P
     C**                   MOVEL'DATE'    EDTMSK 10 P
     C**                   EXSR CHGNUM
     C**                   MOVEANM2       INF,A
      **
     C**                   Z-ADD29        A
     C**                   MOVEACMLTFL    INF,A
     C**                   Z-ADD32        A
     C**                   MOVEACMSTAT    INF,A
      **
     C**                   Z-ADD34        A
     C**
     C*  GET THE CLAIM REOPEN DATE, IF ONE EXISTS.
     C**
     C**                   Z-ADD0         XXRODT 100
     C**                   Z-ADDCMFND     KKFND
     C**                   Z-ADDCMFYR     KKFYR
     C**                   Z-ADDCMCASE    KKCASE
     C**         CMKY      SETGTPCH01
     C**         CMKY      REDPEPCH01                    99
     C**         *IN99     IFEQ *OFF
     C**         CHOCST    ANDEQ'O'
     C**                   Z-ADDCHEFDT    SRDTI
     C**                   EXSR CVTDT
     C**                   MOVELSRDTO     XXRODT
     C**                   ENDIF
     C**         CMKY      REDPEPCH01                    99
     C**         *IN99     IFEQ *ON
     C**                   Z-ADD0         XXRODT
     C**                   ENDIF
     C**
     C**                   MOVELXXRODT    XXNUM  80 P
     C**                   MOVEL'DATE'    EDTMSK 10 P
     C**                   EXSR CHGNUM
     C**                   MOVEANM2       INF,A
      ** **
      ** NURSE NAME & EXT#, ADUSTER NAME & EXT#
      ** **
B01  C**         INFO#     WHEQ 2
      **
      ** LOAD VARIABLE FOR ADJUSTER TITLE TO COLUMN HEADING ARRAY
      **
     C**                   MOVEASCHDL1    H1
     C**                   MOVELXCADJ     XX12A  12 P
     C**         CXADJ     IFNE *BLANK
     C**                   MOVELCXADJ     XX12A     P
     C**                   ENDIF
     C**                   MOVEAXX12A     H1,56
     C**                   MOVEAH1        SCHDL1
      **
     C**         CMNRSE    CHAINPUP02                99
     C**         *IN99     IFEQ *OFF
     C**                   MOVEAUPUSR     INF,A     P
      **
     C**                   Z-ADD13        A
     C**                   MOVELUPEXT#    XXNUM     P
     C**                   EXSR CHGNUM
     C**                   MOVEANM1       INF,A
E01  C**                   END
      **
     C**                   Z-ADD20        A
     C**         CMCLEX    CHAINPUP02                99
     C**         *IN99     IFEQ *OFF
     C**                   MOVEAUPUSR     INF,A     P
      **
     C**                   Z-ADD33        A
     C**                   MOVELUPEXT#    XXNUM     P
     C**                   EXSR CHGNUM
     C**                   MOVEANM1       INF,A
E01  C**                   END
      ** **
      ** EMPLOYER, DIV, EMP NAME, SUPERVISOR'S EXT#, ADJUSTER'S EXT#
      ** **
     C**         INFO#     WHEQ 3
     C**                   MOVELCMEMP#    XXNUM     P
     C**                   EXSR CHGNUM
     C**                   MOVEANM1       INF,A
      **
     C**                   Z-ADD11        A
     C**                   MOVELCMDIV     XXNUM     P
     C**                   EXSR CHGNUM
     C**                   MOVEANM1       INF,A
      **
     C**                   Z-ADD17        A
     C**         EMKY      CHAINPEM01                99
     C**         *IN99     IFEQ *OFF
     C**                   MOVELEMNAME    XX09A   9 P
     C**                   MOVEAXX09A     INF,A
E01  C**                   END
      **
     C**                   Z-ADD28        A
     C**         CMSUPR    CHAINPUP02                99
     C**         *IN99     IFEQ *OFF
     C**                   MOVEAUPUSR     INF,A
      **
     C**                   Z-ADD40        A
     C**                   MOVELUPEXT#    XXNUM     P
     C**                   EXSR CHGNUM
     C**                   MOVEANM1       INF,A
E01  C**                   END
      **
     C**                   Z-ADD38        A
     C**         CMCLEX    CHAINPUP02                99
     C**         *IN99     IFEQ *OFF
     C**                   MOVELUPEXT#    XXNUM     P
     C**                   EXSR CHGNUM
     C**                   MOVEANM1       INF,A
E01  C**                   END
      ** **
      ** ACCIDENT DESCRIPTION, BODY PART , TOTAL INCURRED
      ** **
B01  C**         INFO#     WHEQ 4
     C**                   MOVELCMACDS    XX15A     P
     C**                   MOVEAXX15A     INF,A
     C**                   Z-ADD18        A
     C**                   MOVELCMPART    XXKEY     P
     C**         TBKY      CHAINTPA01                60
     C**         *IN60     IFEQ *OFF
     C**                   MOVELPADESC    XX15A  15 P
     C**                   MOVEAXX15A     INF,A
     C**                   END
      **
     C**                   Z-ADD30        A
     C**                   Z-ADD0         XXTOTI 112
     C**                   Z-ADD0         XXTOTC 112
     C**                   Z-ADD*ZERO     TOTPMT 112
     C**                   Z-ADD*ZERO     TOTRES 112
     C**                   XFOOTCMA       TOTPMT
     C**                   XFOOTCMR       TOTRES
     C**                   ADD  TOTPMT    XXTOTI
     C**                   ADD  TOTRES    XXTOTI
     C**
     C**                   MOVELXXTOTI    XXNUM     P
     C**                   MOVEL'X,XXX.XX'EDTMSK    P
     C**                   EXSR CHGNUM
     C**                   MOVEANM2       INF,A
E01  C**                   ENDSL
     C*** BEG ADD *** 02/16/10 ***************************
     C**                   ENDIF
     C*** END ADD *****  02/16/10  ************************************
      ***
     C*** BEG ADD *** 02/16/10 ***************************
     C*** BEG CHG *** 07/19/10 ***************************
     C**         CLTID     IFEQ '133'
     C*** END CHG *****  07/19/10  ************************************
     C                   SELECT
     C     INFO#         WHENEQ    1
     C***  Begin add  ***  03/08/10  *********************************
     C     VIEW#         IFNE      3
     C***  END   ADD  ***  03/08/10  *********************************
     C***  Begin add  ***  03/12/10  *********************************
     C     VIEW#         IFEQ      1
     C                   MOVEL     *BLANKS       SCHDL1
     C                   MOVEL     AHD(1)        SCHDL1
     C                   ENDIF
     C***  END   ADD  ***  03/12/10  *********************************
     C                   MOVEL(P)  CMJRST        XXNUM
     C                   EXSR      CHGNUM
     C                   MOVEA     NM1           INF(A)
      *
     C                   Z-ADD     4             A
     C                   MOVEL(P)  CMFND         XXNUM
     C                   EXSR      CHGNUM
     C                   MOVEA     NM1           INF(A)
      *
     C                   Z-ADD     8             A
     C                   MOVEL(P)  CMFYR         XXNUM
     C                   EXSR      CHGNUM
     C                   MOVEA     NM1           INF(A)
      *
     C                   Z-ADD     12            A
     C                   MOVEL(P)  CMCASE        XXNUM
     C                   EXSR      CHGNUM
     C                   MOVEA     NM1           INF(A)
      *
     C                   Z-ADD     20            A
     C                   Z-ADD     CMACDT        SRDTI
     C                   EXSR      CDT133
     C                   MOVEL(P)  SRDTO         XXNUM            80
     C                   MOVEL(P)  'DATE'        EDTMSK           10
     C                   EXSR      CHGNUM
     C                   MOVEA     NM2           INF(A)
      *
     C                   Z-ADD     29            A
     C                   MOVEA     CMLTFL        INF(A)
     C                   Z-ADD     32            A
     C                   MOVEA     CMSTAT        INF(A)
      *
     C                   Z-ADD     34            A
     C*
     C*  GET THE CLAIM REOPEN DATE, IF ONE EXISTS.
     C*
     C***  Begin ADD  ***  03/12/10  *********************************
     C                   MOVEL(P)  *BLANKS       XXROD#           80
     C***  END   ADD  ***  03/12/10  *********************************
     C                   Z-ADD     0             XXRODT           10 0
     C                   Z-ADD     CMFND         KKFND
     C                   Z-ADD     CMFYR         KKFYR
     C                   Z-ADD     CMCASE        KKCASE
     C     CMKY          SETGT     PCH01
     C     CMKY          READPE    PCH01                                  99
     C     *IN99         IFEQ      *OFF
     C     CHOCST        ANDEQ     'O'
     C                   Z-ADD     CHEFDT        SRDTI
     C                   EXSR      CDT133
     C                   MOVEL     SRDTO         XXRODT
     C***  Begin add  ***  03/08/10  *********************************
     C                   MOVEL     SRDTO         XXROD#
     C***  END   ADD  ***  03/08/10  *********************************
     C                   ENDIF
     C     CMKY          READPE    PCH01                                  99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             XXRODT
     C***  Begin ADD  ***  03/12/10  *********************************
     C                   MOVEL     *BLANKS       XXROD#
     C***  END   ADD  ***  03/12/10  *********************************
     C                   ENDIF
     C*
     C                   MOVEL(P)  XXRODT        XXNUM            80
     C***  Begin add  ***  03/08/10  *********************************
     C                   MOVEL     *BLANKS       XXNUM
     C                   MOVEL     XXROD#        XXNUM
     C***  END   ADD  ***  03/08/10  *********************************
     C                   MOVEL(P)  'DATE'        EDTMSK           10
     C                   EXSR      CHGNUM
     C                   MOVEA     NM2           INF(A)
     C***  Begin add  ***  03/08/10  *********************************
     C                   ENDIF
     C***  END   ADD  ***  03/08/10  *********************************
     C***  Begin add  ***  03/08/10  *********************************
     C     VIEW#         IFEQ      3
     C                   MOVEL(P)  CMFND         XXNUM
     C                   EXSR      CHGNUM
     C                   MOVEA     NM1           INF(A)
      *
     C                   Z-ADD     5             A
     C                   MOVEL(P)  CMFYR         XXNUM
     C                   EXSR      CHGNUM
     C                   MOVEA     NM1           INF(A)
      *
     C                   Z-ADD     8             A
     C                   MOVEL(P)  CMCASE        XXNUM
     C                   EXSR      CHGNUM
     C                   MOVEA     NM1           INF(A)
      *
     C                   Z-ADD     15            A
     C                   MOVEL(P)  CMJRST        XXNUM
     C                   EXSR      CHGNUM
     C                   MOVEA     NM1           INF(A)
      *
     C                   Z-ADD     17            A
     C                   Z-ADD     CMACDT        SRDTI
     C                   EXSR      CDT133
     C                   MOVEL(P)  SRDTO         XXNUM            80
     C                   MOVEL(P)  'DATE'        EDTMSK           10
     C                   EXSR      CHGNUM
     C                   MOVEA     NM2           INF(A)
      *
     C                   Z-ADD     29            A
     C                   MOVEA     CMLTFL        INF(A)
     C                   Z-ADD     32            A
     C                   MOVEA     CMSTAT        INF(A)
      *
     C                   Z-ADD     34            A
     C*
     C*  GET THE CLAIM REOPEN DATE, IF ONE EXISTS.
     C*
     C***  Begin ADD  ***  03/17/10  *********************************
     C                   MOVEL(P)  *BLANKS       XXROD#           80
     C***  END   ADD  ***  03/17/10  *********************************
     C                   Z-ADD     0             XXRODT           10 0
     C                   Z-ADD     CMFND         KKFND
     C                   Z-ADD     CMFYR         KKFYR
     C                   Z-ADD     CMCASE        KKCASE
     C     CMKY          SETGT     PCH01
     C     CMKY          READPE    PCH01                                  99
     C     *IN99         IFEQ      *OFF
     C     CHOCST        ANDEQ     'O'
     C                   Z-ADD     CHEFDT        SRDTI
     C                   EXSR      CDT133
     C                   MOVEL     SRDTO         XXRODT
     C***  Begin add  ***  03/17/10  *********************************
     C                   MOVEL     SRDTO         XXROD#
     C***  END   ADD  ***  03/17/10  *********************************
     C                   ENDIF
     C     CMKY          READPE    PCH01                                  99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             XXRODT
     C***  Begin ADD  ***  03/17/10  *********************************
     C                   MOVEL     *BLANKS       XXROD#
     C***  END   ADD  ***  03/17/10  *********************************
     C                   ENDIF
     C*
     C                   MOVEL(P)  XXRODT        XXNUM            80
     C***  Begin add  ***  03/17/10  *********************************
     C                   MOVEL     *BLANKS       XXNUM
     C                   MOVEL     XXROD#        XXNUM
     C***  END   ADD  ***  03/17/10  *********************************
     C                   MOVEL(P)  'DATE'        EDTMSK           10
     C                   EXSR      CHGNUM
     C                   MOVEA     NM2           INF(A)
     C                   ENDIF
     C***  END   ADD  ***  03/08/10  *********************************
      ** **
      ** NURSE NAME & EXT#, ADUSTER NAME & EXT#
      ** **
B01  C     INFO#         WHENEQ    2
      *
      *  LOAD VARIABLE FOR ADJUSTER TITLE TO COLUMN HEADING ARRAY
      *BEG ADD 03/08/10 *************************************
     C                   MOVEL     *BLANKS       SCHDL1
     C                   SETON                                        67
     C                   SETON                                        63
     C                   SELECT
     C     VIEW#         WHENEQ    1
     C                   MOVEL     AHD(5)        SCHDL1
     C     VIEW#         WHENEQ    2
     C                   MOVEL     AHD(6)        SCHDL1
     C     VIEW#         WHENEQ    3
     C                   MOVEL     AHD(7)        SCHDL1
     C     VIEW#         WHENEQ    4
     C                   MOVEL     AHD(8)        SCHDL1
     C                   ENDSL
      *BEG ADD 03/08/10 *************************************
     C                   MOVEA     SCHDL1        H1
     C                   MOVEL(P)  XCADJ         XX12A            12
     C     CXADJ         IFNE      *BLANK
     C                   MOVEL(P)  CXADJ         XX12A
     C                   ENDIF
     C                   MOVEA     XX12A         H1(56)
     C                   MOVEA     H1            SCHDL1
      *
     C     CMNRSE        CHAIN     PUP02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEA(P)  UPUSR         INF(A)
      *
     C                   Z-ADD     13            A
     C                   MOVEL(P)  UPEXT#        XXNUM
     C                   EXSR      CHGNUM
     C                   MOVEA     NM1           INF(A)
E01  C                   END
      *
     C                   Z-ADD     20            A
     C     CMCLEX        CHAIN     PUP02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEA(P)  UPUSR         INF(A)
      *
     C                   Z-ADD     33            A
     C                   MOVEL(P)  UPEXT#        XXNUM
     C                   EXSR      CHGNUM
     C                   MOVEA     NM1           INF(A)
E01  C                   END
      ** **
      ** EMPLOYER, DIV, EMP NAME, SUPERVISOR'S EXT#, ADJUSTER'S EXT#
      ** **
     C     INFO#         WHENEQ    3
     C**** BEG ADD 03/08/10 *******************************
     C                   SETON                                        63
     C                   SETON                                        67
     C                   MOVEL     *BLANKS       SCHDL1
     C                   SELECT
     C     VIEW#         WHENEQ    1
     C                   MOVEL     AHD(9)        SCHDL1
     C     VIEW#         WHENEQ    2
     C                   MOVEL     AHD(10)       SCHDL1
     C     VIEW#         WHENEQ    3
     C                   MOVEL     AHD(11)       SCHDL1
     C     VIEW#         WHENEQ    4
     C                   MOVEL     AHD(12)       SCHDL1
     C                   ENDSL
      *BEG ADD 03/08/10 *************************************
     C**** END ADD 03/08/10 *******************************
     C                   MOVEL(P)  CMEMP#        XXNUM
     C                   EXSR      CHGNUM
     C                   MOVEA     NM1           INF(A)
      *
     C                   Z-ADD     11            A
     C                   MOVEL(P)  CMDIV         XXNUM
     C                   EXSR      CHGNUM
     C                   MOVEA     NM1           INF(A)
      *
     C                   Z-ADD     17            A
     C     EMKY          CHAIN     PEM01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  EMNAME        XX09A             9
     C                   MOVEA     XX09A         INF(A)
E01  C                   END
      *
     C                   Z-ADD     28            A
     C     CMSUPR        CHAIN     PUP02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEA     UPUSR         INF(A)
      *
     C                   Z-ADD     40            A
     C                   MOVEL(P)  UPEXT#        XXNUM
     C                   EXSR      CHGNUM
     C                   MOVEA     NM1           INF(A)
E01  C                   END
      *
     C**                   Z-ADD38        A
     C**         CMCLEX    CHAINPUP02                99
     C**         *IN99     IFEQ *OFF
     C**                   MOVELUPEXT#    XXNUM     P
     C**                   EXSR CHGNUM
     C**                   MOVEANM1       INF,A
E01  C**                   END
      ** **
      ** ACCIDENT DESCRIPTION, BODY PART , TOTAL INCURRED
      ** **
B01  C     INFO#         WHENEQ    4
      *BEG ADD 03/08/10 *************************************
     C                   MOVEL     *BLANKS       SCHDL1
     C                   SETON                                        67
     C                   SETON                                        63
     C                   SELECT
     C     VIEW#         WHENEQ    1
     C                   MOVEL     AHD(13)       SCHDL1
     C     VIEW#         WHENEQ    2
     C                   MOVEL     AHD(14)       SCHDL1
     C     VIEW#         WHENEQ    3
     C                   MOVEL     AHD(15)       SCHDL1
     C     VIEW#         WHENEQ    4
     C                   MOVEL     AHD(16)       SCHDL1
     C                   ENDSL
      *BEG ADD 03/08/10 *************************************
     C                   MOVEL(P)  CMACDS        XX15A
     C                   MOVEA     XX15A         INF(A)
     C                   Z-ADD     18            A
     C                   MOVEL(P)  CMPART        XXKEY
     C     TBKY          CHAIN     TPA01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  PADESC        XX15A            15
     C                   MOVEA     XX15A         INF(A)
     C                   END
      *
     C                   Z-ADD     30            A
     C                   Z-ADD     0             XXTOTI           11 2
     C                   Z-ADD     0             XXTOTC           11 2
     C                   Z-ADD     *ZERO         TOTPMT           11 2
     C                   Z-ADD     *ZERO         TOTRES           11 2
     C                   XFOOT     CMA           TOTPMT
     C                   XFOOT     CMR           TOTRES
     C                   ADD       TOTPMT        XXTOTI
     C                   ADD       TOTRES        XXTOTI
     C*
     C                   MOVEL(P)  XXTOTI        XXNUM
     C                   MOVEL(P)  'X,XXX.XX'    EDTMSK
     C                   EXSR      CHGNUM
     C                   MOVEA     NM2           INF(A)
E01  C                   ENDSL
     C***  Begin add  ***  02/16/10  *********************************
     C*** BEG CHG *****  07/19/10  ************************************
     C**                   ENDIF
     C*** END CHG *****  07/19/10  ************************************
     C*** END ADD *** 02/16/10 ***************************
     C                   MOVEA(P)  INF           XX44A            44
     C                   MOVE(P)   XX44A         SCDATA
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * F11=CHANGE VIEW
     C                   MOVE      *BLANK        XX30A            30
     C                   SETOFF                                         74
     C     CMDNY         IFEQ      'Y'
     C                   SETON                                          74
     C                   ENDIF
     C     CMCVFL        IFEQ      'Y'
     C                   SETON                                          74
     C                   ENDIF
     C***  End   add  ***  01/31/06  *********************************
     C***  BEG   ADD  ***  02/16/10  *********************************
     C***  BEG   CHG  ***  07/19/10  *********************************
     C**         CLTID     IFEQ '133'
     C***  END   CHG  ***  07/19/10  *********************************
     C***  BEG   ADD  ***  03/08/10  *********************************
     C     INFO#         IFEQ      1
     C***  End   add  ***  03/08/10  *********************************
     C                   SETOFF                                       67
     C                   SETON                                        63
     C***  BEG   ADD  ***  03/08/10  *********************************
     C                   ENDIF
     C***  End   add  ***  03/08/10  *********************************
     C***  BEG   CHG  ***  07/19/10  *********************************
     C**                   ENDIF
     C***  END   CHG  ***  07/19/10  *********************************
     C***  END   ADD  ***  02/16/10  *********************************
     C                   SELECT
B01  C     VIEW#         WHENEQ    1
     C                   MOVEL     XXCNAM        XX30A
     C                   MOVE      SCSS#         XX30A
     C                   MOVEL     XX30A         SCDATA
      *
     C     VIEW#         WHENEQ    2
     C***  Begin add  ***  02/16/10  *********************************
     C***  Begin CHG  ***  07/16/10  *********************************
     C**         CLTID     IFEQ '133'
     C****BEG ADD 03/08/10*******************************************
     C     INFO#         IFEQ      1
     C*** END ADD 03/08/10********************************************
     C                   MOVEL     *BLANKS       SCHDL1
     C                   MOVEL     AHD(3)        SCHDL1
     C****BEG ADD 03/08/10*******************************************
     C                   ENDIF
     C*** END ADD 03/08/10********************************************
     C***                  ENDIF
     C***  END   CHG  ***  07/16/10  *********************************
     C***  END   ADD  ***  02/16/10  *********************************
     C                   MOVEL     SCSS#         XX30A
     C                   MOVE      XXCNAM        XX30A
     C                   MOVEL     XX30A         SCDATA
      *
     C     VIEW#         WHENEQ    3
     C***  Begin add  ***  02/16/10  *********************************
     C***  Begin CHG  ***  07/16/10  *********************************
     C**         CLTID     IFEQ '133'
     C****BEG ADD 03/08/10*******************************************
     C     INFO#         IFEQ      1
     C*** END ADD 03/08/10********************************************
     C                   SETON                                        67
     C                   SETOFF                                       63
     C                   MOVEL     *BLANKS       SCHDL1
     C                   MOVEL     AHD(2)        SCHDL1
     C****BEG ADD 03/08/10*******************************************
     C                   ENDIF
     C*** END ADD 03/08/10********************************************
     C**                   ENDIF
     C***  END   CHG  ***  07/16/10  *********************************
     C***  END   ADD  ***  02/16/10  *********************************
     C                   MOVEL(P)  CMFND         XXNUM
     C                   EXSR      CHGNUM
     C                   MOVEA(P)  NM1           XAFND
      *
     C                   MOVEL(P)  CMFYR         XXNUM
     C                   EXSR      CHGNUM
     C                   MOVEA(P)  NM1           XAFYR
      *
     C                   MOVEL(P)  CMCASE        XXNUM
     C                   EXSR      CHGNUM
     C                   MOVEA(P)  NM1           XACASE
      *
     C                   MOVEL     GYC           XX30A
     C                   MOVE      SCSS#         XX30A
     C                   MOVEL     XX30A         SCDATA
     C***  Begin add  ***  02/16/10  *********************************
     C***  Begin CHG  ***  07/19/10  *********************************
     C**         CLTID     IFNE '133'
     C***  End   add  ***  02/16/10  *********************************
     C**         INFO#     IFEQ 1
     C**                   MOVELCMFND     XXNUM     P
     C**                   EXSR CHGNUM
     C**                   MOVEANM1       INF,A
      **
     C**                   Z-ADD5         A
     C**                   MOVELCMFYR     XXNUM     P
     C**                   EXSR CHGNUM
     C**                   MOVEANM1       INF,A
      *
     C**                   Z-ADD9         A
     C**                   MOVELCMCASE    XXNUM     P
     C**                   EXSR CHGNUM
     C**                   MOVEANM1       INF,A
      **
     C**                   Z-ADD18        A
     C**                   MOVEACMSS#     INF,A
      **
     C**                   Z-ADD29        A
     C**                   MOVE *BLANK    XX18A  18
     C**                   MOVELXXCNAM    XX18A
     C**                   MOVEAXX18A     INF,A
      **
     C**                   Z-ADD49        A
     C**                   Z-ADDCMACDT    SRDTI
     C**                   EXSR CVTDT
     C**                   MOVELSRDTO     XXNUM  80 P
     C**                   MOVEL'DATE'    EDTMSK 10 P
     C**                   EXSR CHGNUM
     C**                   MOVEANM2       INF,A
      **
     C**                   Z-ADD61        A
     C**                   MOVEACMLTFL    INF,A
      **
     C**                   Z-ADD64        A
     C**                   MOVEACMSTAT    INF,A
      **
     C**                   Z-ADD67        A
     C**
     C** GET THE CLAIM REOPEN DATE, IF ONE EXISTS.
     C**
     C**                   Z-ADD0         XXRODT
     C**                   Z-ADDCMFND     KKFND
     C**                   Z-ADDCMFYR     KKFYR
     C**                   Z-ADDCMCASE    KKCASE
     C**         CMKY      SETGTPCH01
     C**         CMKY      REDPEPCH01                    99
     C**         *IN99     IFEQ *OFF
     C**         CHOCST    ANDEQ'O'
     C**                   Z-ADDCHEFDT    SRDTI
     C**                   EXSR CVTDT
     C**                   MOVELSRDTO     XXRODT
     C**                   ENDIF
     C**         CMKY      REDPEPCH01                    99
     C**         *IN99     IFEQ *ON
     C**                   Z-ADD0         XXRODT
     C**                   ENDIF
     C**
     C**                   MOVELXXRODT    XXNUM  80 P
     C**                   MOVEL'DATE'    EDTMSK 10 P
     C**                   EXSR CHGNUM
     C**                   MOVEANM2       INF,A
     C**
     C**                   MOVE *BLANK    SCDATA
     C**                   MOVEAINF       SCDATA
E01  C**                   END
     C***  BEGIN ADD  ***  02/16/10  *********************************
     C**                   ENDIF
     C***  END   CHG  ***  02/16/10  *********************************
     C***  END   ADD  ***  02/16/10  *********************************
     C***  Begin add  ***  02/16/10  *********************************
     C***  Begin CHG  ***  07/19/10  *********************************
     C**         CLTID     IFEQ '133'
     C***  END   CHG  ***  07/19/10  *********************************
     C***  END   CHG  ***  03/08/10  *********************************
      *
     C     INFO#         IFEQ      1
     C***  Begin add  ***  03/08/10  *********************************
     C     VIEW#         ANDNE     3
     C***  END   ADD  ***  03/08/10  *********************************
     C                   MOVEL(P)  CMFND         XXNUM
     C                   EXSR      CHGNUM
     C                   MOVEA     NM1           INF(A)
      *
     C                   Z-ADD     5             A
     C                   MOVEL(P)  CMFYR         XXNUM
     C                   EXSR      CHGNUM
     C                   MOVEA     NM1           INF(A)
      *
     C                   Z-ADD     9             A
     C                   MOVEL(P)  CMCASE        XXNUM
     C                   EXSR      CHGNUM
     C                   MOVEA     NM1           INF(A)
      *
     C                   Z-ADD     18            A
     C                   MOVEA     CMSS#         INF(A)
      *
     C                   Z-ADD     29            A
     C                   MOVE      *BLANK        XX18A            18
     C                   MOVEL     XXCNAM        XX18A
     C                   MOVEA     XX18A         INF(A)
      *
     C                   Z-ADD     49            A
     C                   Z-ADD     CMACDT        SRDTI
     C                   EXSR      CDT133
     C                   MOVEL(P)  SRDTO         XXNUM            80
     C                   MOVEL(P)  'DATE'        EDTMSK           10
     C                   EXSR      CHGNUM
     C                   MOVEA     NM2           INF(A)
      *
     C                   Z-ADD     61            A
     C                   MOVEA     CMLTFL        INF(A)
      *
     C                   Z-ADD     64            A
     C                   MOVEA     CMSTAT        INF(A)
      *
     C                   Z-ADD     67            A
     C*
     C*  GET THE CLAIM REOPEN DATE, IF ONE EXISTS.
     C*
     C                   Z-ADD     0             XXRODT
     C                   Z-ADD     CMFND         KKFND
     C                   Z-ADD     CMFYR         KKFYR
     C                   Z-ADD     CMCASE        KKCASE
     C     CMKY          SETGT     PCH01
     C     CMKY          READPE    PCH01                                  99
     C     *IN99         IFEQ      *OFF
     C     CHOCST        ANDEQ     'O'
     C                   Z-ADD     CHEFDT        SRDTI
     C                   EXSR      CDT133
     C                   MOVEL     SRDTO         XXRODT
     C                   ENDIF
     C     CMKY          READPE    PCH01                                  99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             XXRODT
     C                   ENDIF
     C*
     C                   MOVEL(P)  XXRODT        XXNUM            80
     C                   MOVEL(P)  'DATE'        EDTMSK           10
     C                   EXSR      CHGNUM
     C                   MOVEA     NM2           INF(A)
     C*
     C                   MOVE      *BLANK        SCDATA
     C                   MOVEA     INF           SCDATA
E01  C                   END
      *******
     C***  BEGIN ADD  ***  03/08/10  *********************************
     C     INFO#         IFEQ      1
     C     VIEW#         ANDEQ     3
     C                   MOVEL(P)  CMFND         XXNUM
     C                   EXSR      CHGNUM
     C                   MOVEA     NM1           INF(A)
      *
     C                   Z-ADD     5             A
     C                   MOVEL(P)  CMFYR         XXNUM
     C                   EXSR      CHGNUM
     C                   MOVEA     NM1           INF(A)
      *
     C                   Z-ADD     8             A
     C                   MOVEL(P)  CMCASE        XXNUM
     C                   EXSR      CHGNUM
     C                   MOVEA     NM1           INF(A)
      *
     C                   Z-ADD     16            A
     C                   MOVEL(P)  CMJRST        XXNUM
     C                   EXSR      CHGNUM
     C                   MOVEA     NM1           INF(A)
      *
     C                   Z-ADD     19            A
     C                   MOVEA     CMSS#         INF(A)
      *
     C                   Z-ADD     29            A
     C                   MOVE      *BLANK        XX18A            18
     C                   MOVEL     XXCNAM        XX18A
     C                   MOVEA     XX18A         INF(A)
      *
     C                   Z-ADD     49            A
     C                   Z-ADD     CMACDT        SRDTI
     C                   EXSR      CDT133
     C                   MOVEL(P)  SRDTO         XXNUM            80
     C                   MOVEL(P)  'DATE'        EDTMSK           10
     C                   EXSR      CHGNUM
     C                   MOVEA     NM2           INF(A)
      *
     C                   Z-ADD     61            A
     C                   MOVEA     CMLTFL        INF(A)
      *
     C                   Z-ADD     64            A
     C                   MOVEA     CMSTAT        INF(A)
      *
     C                   Z-ADD     67            A
     C*
     C*  GET THE CLAIM REOPEN DATE, IF ONE EXISTS.
     C*
     C***  Begin ADD  ***  03/17/10  *********************************
     C                   MOVEL(P)  *BLANKS       XXROD#           80
     C***  END   ADD  ***  03/17/10  *********************************
     C                   Z-ADD     0             XXRODT
     C                   Z-ADD     CMFND         KKFND
     C                   Z-ADD     CMFYR         KKFYR
     C                   Z-ADD     CMCASE        KKCASE
     C     CMKY          SETGT     PCH01
     C     CMKY          READPE    PCH01                                  99
     C     *IN99         IFEQ      *OFF
     C     CHOCST        ANDEQ     'O'
     C                   Z-ADD     CHEFDT        SRDTI
     C                   EXSR      CDT133
     C                   MOVEL     SRDTO         XXRODT
     C***  Begin add  ***  03/17/10  *********************************
     C                   MOVEL     SRDTO         XXROD#
     C***  END   ADD  ***  03/17/10  *********************************
     C                   ENDIF
     C     CMKY          READPE    PCH01                                  99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             XXRODT
     C***  Begin ADD  ***  03/17/10  *********************************
     C                   MOVEL     *BLANKS       XXROD#
     C***  END   ADD  ***  03/17/10  *********************************
     C                   ENDIF
     C*
     C                   MOVEL(P)  XXRODT        XXNUM            80
     C***  Begin add  ***  03/17/10  *********************************
     C                   MOVEL     *BLANKS       XXNUM
     C                   MOVEL     XXROD#        XXNUM
     C***  END   ADD  ***  03/17/10  *********************************
     C                   MOVEL(P)  'DATE'        EDTMSK           10
     C                   EXSR      CHGNUM
     C                   MOVEA     NM2           INF(A)
     C*
     C                   MOVE      *BLANK        SCDATA
     C                   MOVEA     INF           SCDATA
E01  C                   ENDIF
     C***  BEGIN ADD  ***  02/16/10  *********************************
     C***  BEGIN CHG  ***  02/16/10  *********************************
     C**                   ENDIF
     C***  END   CHG  ***  02/16/10  *********************************
     C***  END   ADD  ***  02/16/10  *********************************
     C***  END   ADD  ***  03/08/10  *********************************
     C     VIEW#         WHENEQ    4
     C***  BEGIN CHG  ***  01/20/98  *********************************
     C**********           MOVELSCACL#    XX30A
     C***  Begin add  ***  02/16/10  *********************************
     C***  Begin add  ***  07/19/10  *********************************
     C**         CLTID     IFEQ '133'
     C***  END   CHG  ***  07/19/10  *********************************
     C***  Begin add  ***  03/08/10  *********************************
     C     INFO#         IFNE      2
     C***  END   ADD  ***  03/08/10  *********************************
     C                   MOVEL     *BLANKS       SCHDL1
     C***  Begin add  ***  03/08/10  *********************************
     C                   SELECT
     C     INFO#         WHENEQ    3
     C     VIEW#         ANDEQ     4
     C                   MOVEL     AHD(12)       SCHDL1
     C                   SETON                                        67
     C     INFO#         WHENEQ    4
     C     VIEW#         ANDEQ     4
     C                   MOVEL     AHD(16)       SCHDL1
     C                   SETON                                        67
     C                   OTHER
     C***  END   ADD  ***  03/08/10  *********************************
     C                   MOVEL     AHD(4)        SCHDL1
     C                   SETOFF                                       67
     C***  Begin add  ***  03/08/10  *********************************
     C                   ENDSL
     C***  END   ADD  ***  03/08/10  *********************************
     C                   SETON                                        63
     C***  Begin add  ***  03/08/10  *********************************
     C                   ENDIF
     C***  END   ADD  ***  02/16/10  *********************************
     C***  BEG   CHG  ***  07/19/10  *********************************
     C**                   ENDIF
     C***  END   CHG  ***  07/19/10  *********************************
     C***  END   ADD  ***  02/16/10  *********************************
     C                   MOVEL     XXACL#        XX30A
     C                   MOVEL     XX30A         SCDATA
E01  C                   ENDSL
     C***** Beg Add ***** 04/06/11 ***********************************
     C     CMKY          CHAIN     PSI01                              60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                          51
     C                   ELSE                                                   e >IN60 OFF
     C                   SETOFF                                         51
     C                   ENDIF                                                  e >IN60 OFF
     C***** End Add ***** 04/06/11 ***********************************
     C*
     C***  Begin add  ***  01/31/06  *********************************
      * If claim is controvert then display 'Controvert'
     C     CMCVFL        IFEQ      'Y'
     C                   MOVE      CTVCON        SCDATA
     C                   ENDIF
      * If claim is denied then display 'Denied' or
      * if claim is both controvert and denied
      * then display 'Denied'
     C     CMDNY         IFEQ      'Y'
     C                   MOVE      DNYCON        SCDATA
     C                   ENDIF
     C*
     C***  End   add  ***  01/31/06  *********************************
     C                   Z-ADD     CMFND         SCFND
     C                   Z-ADD     CMFYR         SCFYR
     C                   Z-ADD     CMCASE        SCCASE
     C                   Z-ADD     CMEMP#        SCEMP#
     C                   Z-ADD     CMDIV         SCDIV
     C                   Z-ADD     CMACDT        SRDTI
     C                   EXSR      CVTDT
     C                   MOVEL     SRDTO         SCACDT            8 0
     C   20              Z-ADD     @@RRN         SCDBRR
     C**                   MOVE CMLTFL    SCLTFL
     C                   MOVE      CMSTAT        SCSTAT
     C*
     C***  BEGIN ADD  ***  01/12/07  *********************************
     C                   MOVEL(P)  CMCLEX        SCADJ
     C                   MOVEL(P)  CMPART        XXKEY
     C*
     C                   CLEAR                   PADESC
     C*
     C     TBKY          CHAIN     TPA01                              60
     C*
     C  N60              MOVEL(P)  PADESC        SCPART
     C***  END   ADD  ***  01/12/07  *********************************
     C*
     C                   ENDSR                                                  MVDBSC
      *
      *
      *
     C*===============================================================
     C* OPT6 - SUBROUTINE FOR OPTION 6 (EMPLOYER INFO)
     C*===============================================================
     C     OPT6          BEGSR
     C*
     C  N09              MOVE      'C'           ##MODE
     C   09              MOVE      'D'           ##MODE
     C                   CALL      'W0070R06'    ACDPL                  99
     C*
     C                   ENDSR                                                  OPT6
      *
      *
      *
     C*===============================================================
     C* OPT7 - SUBROUTINE FOR OPTION 7 (CLAIMANT INFO)
     C*===============================================================
     C     OPT7          BEGSR
     C*
     C  N09              MOVE      'C'           ##MODE
     C   09              MOVE      'D'           ##MODE
     C                   CALL      'W0070R07'    ACDPL                  99
     C*
     C                   ENDSR                                                  OPT7
      *
      *
      *
     C*===============================================================
     C* OPT8 - SUBROUTINE FOR OPTION 8 (ACCIDENT INFO)
     C*===============================================================
     C     OPT8          BEGSR
     C*
     C  N09              MOVE      'C'           ##MODE
     C   09              MOVE      'D'           ##MODE
     C                   CALL      'W0070R08'    ACDPL                  99
     C*
     C                   ENDSR                                                  OPT8
      *
      *
      *
     C*===============================================================
     C* OPT9 - SUBROUTINE FOR OPTION 9 (MGT INFO)
     C*===============================================================
     C     OPT9          BEGSR
     C*
     C  N09              MOVE      'C'           ##MODE
     C   09              MOVE      'D'           ##MODE
     C                   CALL      'W0070R09'    ACDPL                  99
     C*
     C                   ENDSR                                                  OPT9
      *
      *
      *
     C*===============================================================
     C* OPT10 - SUBROUTINE FOR OPTION 10 (MEDICAL INFO)
     C*===============================================================
     C     OPT10         BEGSR
     C*
     C  N09              MOVE      'C'           ##MODE
     C   09              MOVE      'D'           ##MODE
     C                   CALL      'W0070R10'    ACDPL                  99
     C*
     C                   ENDSR                                                  OPT10
      *
      *
      *
     C*===============================================================
     C* OPT11 - SUBROUTINE FOR OPTION 11 (BENEFIT INFO)
     C*===============================================================
     C     OPT11         BEGSR
     C*
     C  N09              MOVE      'C'           ##MODE
     C   09              MOVE      'D'           ##MODE
     C                   CALL      'W0070R11'    ACDPL                  99
     C*
     C                   ENDSR                                                  OPT11
      *
      *
      *
     C*===============================================================
     C* OPT12 - SUBROUTINE FOR OPTION 12 (RSV/PMT INFO)
     C*===============================================================
     C     OPT12         BEGSR
     C*
     C*** BEG ADD *** 06/13/01 **************************************  *********
     C                   CALL      'WA044R'
     C                   PARM      *BLANKS       ##RTCD            1
     C     ##RTCD        IFNE      'F'
     C*** END ADD *** 06/13/01 **************************************  *********
     C*
     C  N09              MOVE      'C'           ##MODE
     C   09              MOVE      'D'           ##MODE
     C                   CALL      'W0070R12'    ACDPL                  99
     C*
     C*** BEG ADD *** 06/13/01 **************************************  *********
     C                   ENDIF
     C*** END ADD *** 06/13/01 **************************************  *********
     C*
     C                   ENDSR                                                  OPT12
      *
     C*** BEG ADD *** 03/23/10 **************************************  *********
     C*===============================================================
     C* OPT13 - SUSPENSION INFORMATION
     C*===============================================================
     C     OPT13         BEGSR
     C*
     C                   MOVE      QQFND         @FND              3 0
     C                   MOVE      QQFYR         @FYR              3 0
     C                   MOVE      QQCASE        @CASE             7 0
     C                   CALL      'W0070R1B'
     C                   PARM      *BLANKS       QQPRDS
     C                   PARM      ' '           QQCALL
     C                   PARM                    @FND
     C                   PARM                    @FYR
     C                   PARM                    @CASE
     C                   PARM      *BLANKS       QQRTCD
     C*
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   END
     C*
     C                   ENDSR                                                  OPT13
     C*** END ADD *** 03/23/10 **************************************  *********
      *
      *
     C*===============================================================
     C* OPT14 - SUBROUTINE FOR OPTION 14 (DARTS)
     C*===============================================================
     C     OPT14         BEGSR
     C*
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      ##CO#         QCCO#
     C                   MOVE      SCFND         QCFND
     C                   MOVE      SCFYR         QCFYR
     C                   MOVE      SCCASE        QCCASE
     C*
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM                    Q1PGM
     C*
     C                   ENDSR                                                  OPT14
      *
      *
      *
     C*===============================================================
     C* OPT15 - SUBROUTINE FOR OPTION 15 (STATE SPECIFIC)
     C*===============================================================
     C     OPT15         BEGSR
     C*
     C                   MOVE      SCFND         KKFND
     C                   MOVE      SCFYR         KKFYR
     C                   MOVE      SCCASE        KKCASE
     C     CMKY          CHAIN     DBRCDF3                            99
     C*
     C                   MOVE      ##CO#         QQCO#
     C                   MOVE      SCFND         QQFND
     C                   MOVE      SCFYR         QQFYR
     C                   MOVE      SCCASE        QQCASE
      *
     C*** BEGIN DLT *** 07/13/98 **************************************
     C**********           MOVE *ZERO     QQAZIP  90
     C********** CDKY      CHAINPCD01                99
     C********** *IN60     IFEQ *OFF
     C**********           Z-ADDCDZIP     QQAZIP
     C**********           END
     C*** END DLT ***** 07/13/98 **************************************
      *
     C                   MOVEL     CMJRST        XXJRST
     C                   MOVEL     '148'         XX148
      *
     C*** BEG CHG ****** 03/24/10 *************************************
     C***        IXKY      CHAINPIX01                60
     C***        *IN60     IFEQ *OFF
     C                   CALL      'WA076R'      IXAPI                  99
     C*
     C     @RTNCD        IFEQ      'Y'
     C*** END CHG ****** 03/24/10 *************************************
      *
     C     IXFROI        IFNE      *BLANK
     C     'W0070R'      CAT(P)    IXFROI:0      QQPGM             8
     C                   END
      *
     C                   ELSE
     C     'W0070R'      CAT(P)    CMJRST:0      QQPGM
     C                   END
      *
     C     2             SUBST(P)  QQPGM:7       XXST              2
      *
     C     XXST          IFNE      *BLANK
     C                   CALL      QQPGM                                99
     C                   PARM      @@CALL        QQCALL
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C*** BEGIN DLT *** 07/13/98 **************************************
     C******************** PARM           QQAZIP
     C*** END DLT ***** 07/13/98 **************************************
     C                   PARM      *BLANK        QQRTCD
     C                   END
      *
      * IF THERE IS NO STATE SPECIFIC SCREEN, DSP WINDOW MSG.
      *
     C                   MOVEL     '1'           QQCALL
     C***** BEGIN CHG **** 05/26/99 ***********************************
     C********** XXST      IFEQ *BLANK
     C     CMJRST        IFEQ      *BLANK
     C***** END CHG ****** 05/26/99 ***********************************
     C                   MOVEL     '2'           QQCALL
     C                   END
      *
     C     *IN99         IFEQ      *ON
     C     XXST          OREQ      *BLANK
     C                   CALL      'WA020R'                             99
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    ##MODE
     C                   PARM                    ##RRN
     C***** BEGIN CHG **** 05/26/99 ***********************************
     C**********           PARM XXST      QQINF1 40
     C                   PARM      CMJRST        QQINF1           40
     C***** END CHG ****** 05/26/99 ***********************************
     C                   PARM                    QQINF2           40
     C                   PARM                    QQNUM1           13 2
     C                   PARM                    QQNUM2           13 2
     C                   PARM                    QQDAT1            8
     C                   PARM                    QQDAT2            8
     C                   PARM                    QQCALL
     C                   PARM      *BLANK        QQRTCD
     C**                   MOVE *ON       AUTORF
     C                   CALL      'SRRMVMSG'                           99
     C                   END
     C*
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   END
     C*
     C                   ENDSR                                                  OPT15
      *
      *
      *
     C*===============================================================
     C* OPT16 - SUBROUTINE FOR OPTION 16 (LEGAL INFO)
     C*===============================================================
     C     OPT16         BEGSR
     C*
     C  N09              MOVE      'C'           ##MODE
     C   09              MOVE      'D'           ##MODE
     C                   CALL      'W0070R16'    ACDPL                  99
     C*
     C                   ENDSR                                                  OPT16
      *
      *
      *
     C*===============================================================
     C* OPT17 - SUBROUTINE FOR OPTION 17 (ADDITIONAL ADDRESS)
     C*===============================================================
     C     OPT17         BEGSR
     C*
     C                   CALL      'W0573R'                             99
     C                   PARM      @@CALL        ##CALL            1
     C                   PARM      ' '           ##PMTF            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      SCDBRR        ##RRN             9 0
     C*
     C                   ENDSR                                                  OPT17
      *
      *
      *
     C*===============================================================
     C* OPT18 - SUBROUTINE FOR OPTION 18 (EDI INFO)
     C*===============================================================
     C     OPT18         BEGSR
     C*
     C                   MOVE      ##CO#         QQCO#
     C                   MOVE      SCFND         QQFND
     C                   MOVE      SCFYR         QQFYR
     C                   MOVE      SCCASE        QQCASE
     C                   CALL      'W0070R5'                            99      MUST BE LO
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQRTCD            1
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   END
     C*
     C                   ENDSR                                                  OPT18
      *
      *
      *
     C*===============================================================
     C* OPT19 - SUBROUTINE FOR OPTION 19 (TREATING PHYSICIAN)
     C*===============================================================
     C     OPT19         BEGSR
     C*
     C                   MOVE      ##CO#         QQCO#
     C                   MOVE      SCFND         QQFND
     C                   MOVE      SCFYR         QQFYR
     C                   MOVE      SCCASE        QQCASE
     C                   Z-ADD     *ZERO         QQSEQ#            5 0
     C                   CALL      'W4330R'                             99
     C                   PARM                    QQCALL
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQSEQ#
     C                   PARM                    QQRTCD
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   END
     C*
     C                   ENDSR                                                  OPT19
      *
      *
      *
     C*===============================================================
     C* OPT21 - SUBROUTINE FOR OPTION 21 (DIARY ITEMS)
     C*===============================================================
     C     OPT21         BEGSR
     C*
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      ##CO#         QCCO#
     C                   MOVE      SCFND         QCFND
     C                   MOVE      SCFYR         QCFYR
     C                   MOVE      SCCASE        QCCASE
     C*
     C                   CALL      'SC015R2'                            99
     C                   PARM                    QCKEY
     C                   PARM                    QQRTCD
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   END
     C*
     C                   ENDSR                                                  OPT21
      *
      *
      *
     C*===============================================================
     C* OPT22 - SUBROUTINE FOR OPTION 22 (BENEFIT DIARY)
     C*===============================================================
     C     OPT22         BEGSR
     C*
     C                   MOVE      ##CO#         QQCO#             3
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCFYR         QQFYR
     C                   MOVE      SCCASE        QQCASE
     C                   MOVE      SCACDT        QQACDT
     C**
     C                   CALL      'W0190R'                             99
     C                   PARM                    QQPRDS
     C                   PARM      @@CALL        QQCALL
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQACDT            8
     C                   PARM      '1'           QQRTCD
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   END
     C*
     C                   ENDSR                                                  OPT22
      *
      *
     C*** BEG ADD ****** 05/07/07 **********************************************
     C*===============================================================
     C* OPT23 - Subroutine for Option 23 (Claim Summary)
     C*===============================================================
     C     OPT23         BEGSR
     C*
     C  N09              MOVE      'C'           ##MODE
     C   09              MOVE      'D'           ##MODE
     C                   CALL      'W0070RX1'    ACDPL5                 99
     C*
     C                   ENDSR                                                  OPT23
     C*** END ADD ****** 05/07/07 **********************************************
      *
     C*===============================================================
     C* OPT24 - SUBROUTINE FOR OPTION 24 (BODY PART IMPAIRMENT)
     C*===============================================================
     C     OPT24         BEGSR
     C*
     C***  Begin Chg  ***  10/05/09  **************************************
      **********
     C**********           MOVE ##CO#     QQCO#
     C**********           MOVE SCFND     QQFND
     C**********           MOVE SCFYR     QQFYR
     C**********           MOVE SCCASE    QQCASE
     C**********           CALL 'W0070R7'              99
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           QQCO#
     C**********           PARM           QQFND
     C**********           PARM           QQFYR
     C**********           PARM           QQCASE
     C**********           PARM           QQRTCD
     C********** QQRTCD    IFEQ 'A'
     C**********           MOVE *ON       *INKC
     C**********           END
     C                   CALL      'W0572R'                             99
     C                   PARM      @@CALL        ##CALL            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C     *INKC         PARM      *OFF          ##KC              1
     C     *INKL         PARM      *OFF          ##KL              1
     C***  End   Chg  ***  10/05/09  **************************************
     C*
     C                   ENDSR                                                  OPT24
      *
      *
      *
     C*===============================================================
     C* OPT25 - SUBROUTINE FOR OPTION 25 (BENEFIT ADJUSTMENTS)
     C*===============================================================
     C     OPT25         BEGSR
     C*
     C                   MOVE      ##CO#         QQCO#
     C                   MOVE      SCFND         QQFND
     C                   MOVE      SCFYR         QQFYR
     C                   MOVE      SCCASE        QQCASE
     C                   CALL      'W0070R9'                            99
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQRTCD
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   END
     C*
     C                   ENDSR                                                  OPT25
      *
      *
      *
     C*===============================================================
     C* OPT30 - SUBROUTINE FOR OPTION 30 (DEPT LETTERS)
     C*===============================================================
     C     OPT30         BEGSR
     C*
     C                   MOVE      '2'           QXCALL
     C                   MOVE      SCFND         QQFND
     C                   MOVE      SCFYR         QQFYR             3
     C                   MOVE      SCCASE        QQCASE            7
     C                   MOVE      'CS'          QQDEPT
      *
     C                   CALL      'W4110R'                             99
     C                   PARM                    QQPRDS           40
     C                   PARM                    QXCALL            1
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQDEPT            2
     C*
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   END
     C*
     C                   ENDSR                                                  OPT30
     C*** BEGIN ADD *** 01/03/05 **************************************
     C*===============================================================
     C* OPT31 - Subroutine for Option 31 (Reprint DWC-1)
     C*===============================================================
     C     OPT31         BEGSR
     C*
     C*  Retrieve the claim jurisdiction state
     C*
     C                   MOVE      SCFND         KKFND
     C                   MOVE      SCFYR         KKFYR
     C                   MOVE      SCCASE        KKCASE
     C     CMKY          CHAIN     DBRCDF3                            99
     C*
     C                   MOVEL     CMJRST        XXJRST
     C                   MOVEL     '148'         XX148
     C*
     C*** BEG CHG ****** 03/24/10 *************************************
     C***        IXKY      CHAINPIX01                60
     C***        *IN60     IFEQ *OFF
     C                   CALL      'WA076R'      IXAPI                  99
     C     @RTNCD        IFEQ      'Y'
     C*** END CHG ****** 03/24/10 *************************************
     C     IXFROI        ANDNE     *BLANKS
     C                   MOVEL     IXFROI        ##FROI
     C                   ELSE
     C                   MOVEL     CMJRST        ##FROI
     C                   END
     C*** BEG ADD ***** 03/25/11 **************************************
     C*** BEGIN CHG ****** 07/29/11 ************************************
     C************         MOVEL##FROI    STKY
     C                   MOVEL     CMJRST        STKY
     C*** END   CHG ****** 07/29/11 ************************************
     C*
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      *OFF
     C     ST1A6         ANDEQ     'Y'
     C*** END ADD ***** 03/25/11 **************************************
     C*
     C*** BEG ADD ***** 01/14/08 **************************************
     C     CLTID         IFEQ      '133'
     C     CMJRST        ANDEQ     'LA'
     C                   CALL      XLUBA         NOIPL                  99
     C                   ELSE
     C*** END ADD ***** 01/14/08 **************************************
     C     ##FROI        IFNE      *BLANKS
     C                   CALL      'W0070R31'    NOIPL                  99
     C                   ELSE
     C                   EXFMT     WDWNOI
     C                   ENDIF
     C*** BEG ADD ***** 01/14/08 **************************************
     C                   ENDIF                                                  CLTID = '133'
     C*** END ADD ***** 01/14/08 **************************************
     C*** BEG ADD ***** 03/25/11 **************************************
     C                   ELSE
      *
      * IF THERE IS NO FROI INSTALLED FOR STATE, DSP MESSAGE
      *
     C                   MOVEL     '3'           QQCALL
     C                   MOVEL     ##FROI        QQINF1
      *
     C     *IN60         IFEQ      *ON
     C     ST1A6         OREQ      ' '
     C                   CALL      'WA020R'                             99
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    ##MODE
     C                   PARM                    ##RRN
     C                   PARM                    QQINF1
     C                   PARM                    QQINF2
     C                   PARM                    QQNUM1
     C                   PARM                    QQNUM2
     C                   PARM                    QQDAT1
     C                   PARM                    QQDAT2
     C                   PARM                    QQCALL
     C                   PARM      *BLANK        QQRTCD
     C                   CALL      'SRRMVMSG'                           99
     C                   ENDIF
     C*
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   END
     C                   ENDIF
     C*
     C*** END ADD ***** 03/25/11 **************************************
     C*
     C                   ENDSR                                                  OPT31
     C*** END ADD ***** 01/03/05 **************************************
     C*** BEG ADD ****** 10/19/07 ************************************
     C*===============================================================
     C* OPT32 - SUBROUTINE FOR OPTION 32 (EDI Info)
     C*===============================================================
     C     OPT32         BEGSR
      *
     C  N09              MOVE      'C'           ##MODE
     C   09              MOVE      'D'           ##MODE
     C                   CALL      'W0070R32'    ACDPL                  99
      *
     C                   ENDSR                                                  OPT32
     C*** END ADD ****** 10/19/07 ************************************
     C*===============================================================
     C* OPT50 - SUBROUTINE FOR OPTION 50 (PAYMENT SEARCHES)
     C*===============================================================
     C     OPT50         BEGSR
      *
     C                   MOVE      ##CO#         XACO#             3
     C                   MOVE      SCFND         XAFND
     C                   MOVE      SCFYR         XAFYR
     C                   MOVE      SCCASE        XACASE            7
     C*** BEGIN ADD *** 12/26/11 **************************************
     C                   CALL      'SRCHKPGM'                           99
     C                   PARM      'W0070Q50'    ##PGM            10
     C                   PARM      *BLANKS       ##ERR             1
     C*
     C     ##ERR         IFEQ      *OFF
     C                   CALL      'SRCHKPGM'                           99
     C                   PARM      'W0070R50'    ##PGM            10
     C                   PARM      *BLANKS       ##ERR             1
     C*
     C     ##ERR         IFEQ      *ON
     C                   CALL      'FA711C'
     C                   PARM      *BLANKS       ##RTN             1
     C                   PARM      'W0070R50'    ##FNM            10
     C                   PARM      'W0070Q50'    ##TNM            10
     C*
     C                   CALL      'W0070Q50'                           99
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    XACO#
     C                   PARM                    XAFND
     C                   PARM                    XAFYR
     C                   PARM                    XACASE
     C                   PARM                    QQCALL
     C                   PARM      *BLANKS       XAEOMB            9
     C*
     C                   ELSE
     C*** END   ADD *** 12/26/11 **************************************
     C                   CALL      'W0070R50'                           99
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    XACO#
     C                   PARM                    XAFND
     C                   PARM                    XAFYR
     C                   PARM                    XACASE
     C                   PARM                    QQCALL
     C*** BEGIN ADD *** 01/11/99 **************************************
     C                   PARM      *BLANKS       XAEOMB            9
     C*** END ADD *** 01/11/99 ****************************************
     C*** BEGIN ADD *** 12/26/11 **************************************
     C                   ENDIF
     C                   ELSE
     C                   SETON                                        7890
     C                   ENDIF
     C*** END   ADD *** 12/26/11 **************************************
      *
     C                   ENDSR                                                  OPT50
      *
      *
      *
     C*===============================================================
     C* OPT51 - SUBROUTINE FOR OPTION 51 (Requested Payment Search)
     C*===============================================================
     C     OPT51         BEGSR
      *
     C                   MOVE      SCFND         QQFND
     C                   MOVE      SCFYR         QQFYR
     C                   MOVE      SCCASE        QQCASE
     C** ****              MOVE 'I'       QXCALL
     C                   MOVE      'A'           QXCALL
     C                   CALL      'W0080R'                             99
     C                   PARM                    QQPRDS
     C                   PARM                    QXCALL
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM                    QQFRDT            6
     C                   PARM                    QQTODT            6
     C                   PARM                    QQAMT             9
     C                   PARM                    QQEOB#            9
     C***  BEGIN ADD  ***  07/16/98  *********************************
     C                   PARM      *BLANKS       ##PNAM           40
     C                   PARM      *BLANKS       ##TRCD            2
     C     *INKC         PARM                    ##KC
     C***  END   ADD  ***  07/16/98  *********************************
      *
     C                   ENDSR                                                  OPT51
      *
      *
      *
     C*===============================================================
     C* OPT52 - SUBROUTINE FOR OPTION 52 (MANUAL PAYMENT)
     C*===============================================================
     C     OPT52         BEGSR
      *
     C                   MOVE      'A'           ##MODE
     C                   CALL      'W0120R'      ACDPL                  99
     C                   MOVE      *BLANK        ##MODE
      *
     C                   ENDSR                                                  OPT52
      *
      *
      *
     C*===============================================================
     C* OPT53 - SUBROUTINE FOR OPTION 53 (REOCCURRING COMP)
     C*===============================================================
     C*
     C     OPT53         BEGSR
     C*
     C                   CALL      'W8080R'                             99
     C                   PARM      *BLANKS       ##PRDS           40
     C                   PARM      *BLANK        ##CALL            1
     C                   PARM      SCFND         ##FND             3 0
     C                   PARM      SCFYR         ##FYR             3 0
     C                   PARM      SCCASE        ##CASE            7 0
     C***  BEGIN ADD  ***  07/16/98  *********************************
     C     *INKC         PARM                    ##KC
     C***  END   ADD  ***  07/16/98  *********************************
     C*
     C                   ENDSR                                                  OPT53
      *
      *
     C*
     C*===============================================================
     C* OPT54 - SUBROUTINE FOR OPTION 54 (RECOVERY REQUEST)
     C*===============================================================
     C     OPT54         BEGSR
     C*
     C*** BEG DEL *** 11/07/03 **************************************  *********
     C****BEGIN*ADD *** 01/23/98 ***********************************************
     C**N09*****           MOVE 'C'       ##MODE
     C***09*****           MOVE 'D'       ##MODE
     C**********           CALL 'W0100R'  ACDPL        99
     C****END***ADD *** 01/23/98 ***********************************************
     C*** END DEL *** 11/07/03 **************************************  *********
     C*** BEG ADD *** 11/07/03 **************************************  *********
     C*
     C*** THIS PROCESS WILL APPEAR TO LOOK LIKE A W/W FUNCTION
     C*
     C     XXOU54        DOUEQ     *ON
     C*
     C                   CALL      'W0600R'                             99
     C                   PARM      *BLANKS       ##PRDS           40
     C                   PARM      *BLANKS       ##CALL            1
     C                   PARM      SCFYR         ##YR              3 0
     C                   PARM      SCCASE        ##CASE            7 0
     C                   PARM      *ZEROS        ##REQ#            5 0
     C                   PARM                    ##RTCD            1
     C                   PARM      SCFND         ##FND             3 0
     C*
     C*** ##REQ# = SELECTION; ##RTCD = F6 ADD
     C*
     C     ##REQ#        IFNE      *ZEROS
     C     ##RTCD        OREQ      '6'
     C                   MOVE      'D'           ##MODE
     C                   CALL      'W0100R'                             99
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    ##MODE
     C                   PARM      SCDBRR        ##RRN
     C                   PARM                    ##REQ#
     C*
     C     *INKC         IFEQ      *ON
     C                   MOVE      *ON           XXOU54            1
     C                   ENDIF
     C*
     C                   ELSE
     C                   MOVE      *ON           XXOU54            1
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C*** BEG ADD *** 11/07/03 **************************************  *********
     C*
     C                   ENDSR                                                  OPT54
      *
      *
      *
     C*===============================================================
     C* OPT55 - Subroutine for Option 55 (Payment Request Search)
     C*===============================================================
     C     OPT55         BEGSR
      *
     C                   MOVE      ##CO#         QQCO#
     C                   MOVE      SCFND         QQFND
     C                   MOVE      SCFYR         QQFYR
     C                   MOVE      SCCASE        QQCASE
      *
     C                   CALL      'W0583R'                             99
     C     *INKC         PARM      *OFF          ##KC
     C                   PARM      *OFF          ##KL
     C                   PARM      *BLANK        QQCALL            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
      *
     C                   ENDSR                                                  OPT55
      *
      *
      *
     C*===============================================================
     C* OPT60 - SUBROUTINE FOR OPTION 60 (PAYMENT HISTORY)
     C*===============================================================
     C     OPT60         BEGSR
      *
     C                   MOVE      ##CO#         XACO#             3
     C                   MOVE      SCFND         XAFND
     C                   MOVE      SCFYR         XAFYR
     C                   MOVE      SCCASE        XACASE            7
     C                   CALL      'W0576R'                             99
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    QQCALL
     C                   PARM                    XACO#
     C                   PARM                    XAFND
     C                   PARM                    XAFYR
     C                   PARM                    XACASE
     C                   PARM      *BLANK        XAPYST            2
     C                   PARM      *ZEROS        XASFDT            8            Service "From" DT
     C                   PARM      *ZEROS        XASTDT            8            Service "To" DT
     C                   PARM      *BLANKS       XASPC            10            Payment Codes
     C                   PARM      *BLANKS       XASTT             5            Transaction Type
     C                   PARM      *BLANKS       XASPV1           10            Payment Value 1
     C                   PARM      *BLANKS       XASRC            10            Reserve Codes
     C                   PARM      *ZEROS        XASFEI            9            Federal ID #
     C                   PARM      *ZEROS        XASCHK            7            Check #
     C                   PARM      *ZEROS        XASEOB            9            EOMB #
     C                   PARM      *BLANKS       XASIRQ           10            Initial Requestor
     C                   PARM      *BLANK        XASGRP            1            Goup vs. Non-Group
     C*
     C                   ENDSR                                                  OPT60
      *
      *
      *
     C*===============================================================
     C* OPT61 - SUBROUTINE FOR OPTION 61 (RESERVE HISTORY)
     C*===============================================================
     C     OPT61         BEGSR
     C*
     C                   MOVE      SCFYR         QQYR
     C*
     C                   CALL      'W0574R1'                            99
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    ##CO#
     C                   PARM                    SCFND
     C                   PARM                    SCSS#
     C                   PARM                    QQYR
     C                   PARM                    SCCASE
     C                   PARM      *ZEROS        QQEFDT            8 0
     C                   PARM                    QQRTCD
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   END
     C*
     C                   ENDSR                                                  OPT61
      *
      *
      *
     C*===============================================================
     C* OPT62 - SUBROUTINE FOR OPTION 62 (CLAIM STATUS HISTORY)
     C*===============================================================
     C     OPT62         BEGSR
     C*
     C                   CALL      'W0575R'                             99
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    ##CO#
     C                   PARM                    SCFND
     C                   PARM                    SCEMP#
     C                   PARM                    SCDIV
     C                   PARM                    SCSS#
     C                   PARM                    SCFYR
     C                   PARM                    SCCASE
     C                   PARM      *ZEROS        QQEFDT
     C                   PARM                    QQRTCD
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   END
     C*
     C                   ENDSR                                                  OPT62
      *
      *
      *
     C*===============================================================
     C* OPT63 - SUBROUTINE FOR OPTION 63 (I/M/R HISTORY)
     C*===============================================================
     C     OPT63         BEGSR
     C*
     C                   CALL      'W4320R'                             99
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCFND
     C                   PARM                    SCFYR
     C                   PARM                    SCCASE
     C                   PARM                    QQRTCD
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   END
     C*
     C                   ENDSR                                                  OPT63
      *
      *
      *
     C*===============================================================
     C* OPT64 - SUBROUTINE FOR OPTION 64 (AWW HISTORY)
     C*===============================================================
     C     OPT64         BEGSR
     C*
     C                   MOVE      SCFND         QQFND
     C                   MOVE      SCFYR         QQFYR
     C                   MOVE      SCCASE        QQCASE
     C                   CALL      'W0070R8'                            99
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQRTCD
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   END
     C*
     C                   ENDSR                                                  OPT64
      *
      *
      *
     C*===============================================================
     C* OPT65 - SUBROUTINE FOR OPTION 65 (WORK HISTORY)
     C*===============================================================
     C     OPT65         BEGSR
     C*
     C*** BEG ADD *** 02/07/02 ****************************
     C     CLTID         IFEQ      '114'
     C*
     C*  GET THE RRN OF THE WCMC CASE
     C*
     C                   MOVE      ##CO#         XYCO#             3
     C                   MOVE      SCFND         XYFND             3
     C                   MOVE      SCFYR         XYFYR             3
     C                   MOVE      SCCASE        XYCASE            7
     C*
     C                   CALL      'FA201R'
     C                   PARM                    XYCO#
     C                   PARM                    XYFND
     C                   PARM                    XYFYR
     C                   PARM                    XYCASE
     C                   PARM      *ZEROS        XYRRN1            9 0
     C                   PARM      *BLANK        XYRTCD            1
     C*
     C     XYRTCD        IFNE      'F'
     C*
     C                   CALL      'F0176R'
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C                   PARM                    ##CALL
     C                   PARM      *ZEROS        ##RRN                          FAWHP
     C                   PARM      XYRRN1        ##RRN2                         FMCMP
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C*** END ADD *** 02/07/02 ****************************
     C*
     C                   CALL      'W4315R'                             99
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCFND
     C                   PARM                    SCFYR
     C                   PARM                    SCCASE
     C                   PARM                    QQRTCD
      *** BEG ADD *** 05/20/08 ****************************
     C                   PARM      *ZEROS        XXTADL            4 0
     C                   PARM      'Y'           XXUPDC            1            UPDATE CLAIM
      *** END ADD *** 05/20/08 ****************************
      *
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   END
     C*
     C*** BEG ADD *** 02/07/02 ****************************
     C                   ENDIF
     C*** END ADD *** 02/07/02 ****************************
     C                   ENDSR                                                  OPT65
      *
      *
      *
     C*===============================================================
     C* OPT66 - SUBROUTINE FOR OPTION 66 (DEDUCTIBLE HISTORY)
     C*===============================================================
     C     OPT66         BEGSR
     C*
     C                   MOVE      SCFND         QQFND
     C                   MOVE      SCEMP#        QQEMP#
     C                   MOVE      SCDIV         QQDIV             5
     C                   MOVE      SCFYR         QQFYR
     C                   MOVE      SCCASE        QQCASE
     C                   CALL      'W0585R'                             99
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQFND
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQRTCD
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   END
     C*
     C                   ENDSR                                                  OPT66
      *
      *
     C*** BEGIN ADD ***  07/28/09  ************************************
     C*===============================================================
     C* OPT67 - SUBROUTINE FOR OPTION 67 (CMS LOG HISTORY)
     C*===============================================================
     C     OPT67         BEGSR
     C*
     C                   MOVE      ' '           ##MODE
     C*
     C                   CALL      'W7485R'      ACDPL                  99
     C*
     C                   ENDSR                                                  OPT66
     C*** END ADD *****  07/28/09  ************************************
      *
      ***** BEG ADD ***** 08/09/07 ************************************
     C*===============================================================
     C* OPT70 - SUBROUTINE FOR OPTION 70 (EFT ACCOUNT INFO)
     C*===============================================================
     C     OPT70         BEGSR
     C*
     C                   MOVE      SCFND         XXFND
     C                   MOVE      SCFYR         XXFYR
     C                   MOVE      SCCASE        XXCASE
     C                   MOVE      XXCLM         ##CLM
     C*
     C                   CALL      'W0028R'                             99
     C                   PARM      @@CALL        ##CALL
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C*** BEGIN CHG ****** 06/29/09 ************************************
     C***************      PARM           #1RRN   90
     C                   PARM      SCDBRR        #1RRN             9 0
     C*** END   CHG ****** 06/29/09 ************************************
      *** BEG ADD ***** 09/20/07 *********************************
     C                   PARM                    ##CLM            13
     C                   PARM      *BLANK        ##FEIN           12
     C                   PARM      *ZERO         ##SEQ#            5 0
     C                   PARM      'C'           #1TYPE            1
      *** END ADD ***** 09/20/07 *********************************
     C*
     C                   ENDSR                                                  OPT70
      *
      *
      *
      ***** END ADD ***** 08/09/07 ************************************
     C*===============================================================
     C* OPT80 - SUBROUTINE FOR OPTION 80 (EMPLOYER INQUIRY)
     C*===============================================================
     C     OPT80         BEGSR
     C*
     C                   MOVE      'Z'           ##MODE
     C     EMKY2         CHAIN     PEM01                              60
     C                   Z-ADD     DBRRNE        ##RRN3
     C                   CALL      'W0050R'      ACDPL1                 99
     C*
     C                   ENDSR                                                  OPT80
      *
      *
      *
     C*===============================================================
     C* OPT81 - SUBROUTINE FOR OPTION 81 (WCMC INQUIRY)
     C*===============================================================
     C     OPT81         BEGSR
     C*
     C* N09                MOVE 'C'       ##MODE
     C*  09                MOVE 'D'       ##MODE
     C**                   CALL 'F0075R'  ACDPL        99
     C**
     C**                   MOVE SCSS#     QQSS#
     C**                   CALL 'F0075R'               99
     C**                   PARM *BLANK    QQPRDS 40
     C**                   PARM           QQCALL  1        1-Inq,2-Ext Call
     C**                   PARM           QQSS#   9
     C**                   PARM *ZERO     QQACD6  6
      *
      * IF WCMC IS INSTALLED AND LINKED WITH WCMS,
      * GET RRN OF FMCMP FROM CORRESPONDING WMCMP RECORD.
      *
     C     XXWCMC        IFEQ      'Y'
     C*
     C*
     C                   MOVE      ##CO#         QQCO#
     C                   MOVE      SCFND         QQWCF#
     C*
     C                   CALL      'FA700R'                             99
     C                   PARM      *BLANK        QQRTCD
     C                   PARM                    QQCO#
     C                   PARM                    QQWCF#            3
     C                   PARM      *ZEROS        QQSUB#            5
     C                   PARM      *ZEROS        QQFND
     C*
     C                   MOVE      QQSUB#        ##SUB#
     C                   MOVE      QQFND         ##FND
     C*
     C                   CALL      'F0269R'      ACDPL4                 99
     C*
     C                   ENDIF
     C*
     C                   ENDSR                                                  OPT81
      *
      *
      *
     C*===============================================================
     C* OPT82 - SUBROUTINE FOR OPTION 82 (REINSURANCE POLICY)
     C*===============================================================
     C     OPT82         BEGSR
     C*
     C**N09*****           MOVE 'C'       ##MODE
     C***09*****           MOVE 'D'       ##MODE
     C**********           CALL 'W0310R'  ACDPL        99
     C*
     C                   MOVE      SCEMP#        QQEMP#
     C                   MOVE      SCDIV         QQDIV
     C                   MOVE      SCACDT        QQACDT
     C***  BEGIN ADD  ***  06/24/98  *********************************
     C                   MOVE      SCFND         QQFND
     C***  END   ADD  ***  06/24/98  *********************************
     C*
     C                   CALL      'W0310R'
     C*** BEGIN ADD *** 03/25/98 **************************************
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C*** END   ADD *** 03/25/98 **************************************
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM      *BLANK        QQASFL            1
     C                   PARM                    QQACDT            8
     C***  BEGIN ADD  ***  06/24/98  *********************************
     C                   PARM                    QQFND             3
     C***  END   ADD  ***  06/24/98  *********************************
     C*
     C                   ENDSR                                                  OPT82
      *
      *
      *
     C*===============================================================
     C* OPT83 - SUBROUTINE FOR OPTION 83 (PARTIAL FILING)
     C*===============================================================
     C     OPT83         BEGSR
     C*
     C* N09                MOVE 'C'       ##MODE
     C*  09                MOVE 'D'       ##MODE
     C*                    CALL 'W0330R'  ACDPL        99
     C*
     C                   CALL      'W0330R'                             99
     C                   PARM      *BLANK        QQPRDS           40
     C                   PARM                    QQCALL            1
     C*
     C                   ENDSR                                                  OPT83
      *
      ***** BEG CHG ***** 02/05/07 ************************************
      *
     C*================================================================
     C* OPT9O - Option 90 (MMI Specification)
     C*================================================================
     C     OPT90         BEGSR
     C                   MOVE      *BLANK        ##KC
     C                   MOVE      *BLANK        ##KL
     C                   MOVE      ##CO#         QQCO#
     C                   MOVE      '00000'       QQSUB#
     C                   MOVE      SCFND         QQFND
     C                   MOVE      SCFYR         QQFYR
     C                   MOVE      SCCASE        QQCASE
     C                   MOVE      SCACDT        QQACDT
     C                   MOVE      SCSS#         QQSS#
     C                   CALL      'F0160R'
     C                   PARM                    ##KC              1
     C                   PARM                    ##KL              1
     C                   PARM                    @@CALL
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQACDT            8
     C                   PARM                    QQSS#             9
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM                    QQDATE            8
     C                   PARM                    QQRTN             1
     C                   ENDSR
      ***** END CHG ***** 02/05/07 ************************************
      *
     C*** BEG ADD *** 08/18/06 *****************************
     C*===============================================================
     C* OPT91 - SUBROUTINE FOR OPTION 91 (RECURRING COMP INFO)
     C*===============================================================
     C     OPT91         BEGSR
     C*
     C                   CALL      'W8082R'                             99
     C                   PARM      @@CALL        ##CALL
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C                   PARM      SCDBRR        ##RRN
     C*
     C                   ENDSR                                                  OPT90
     C*** END ADD *** 08/18/06 *****************************
     C*** BEGIN ADD ***08/18/06 ***************************************
     C*===============================================================
     C* OPT92 - Payment History Search
     C*===============================================================
     C     OPT92         BEGSR
     C*
     C                   MOVE      ##CO#         XACO#             3
     C                   MOVE      SCFND         XAFND
     C                   MOVE      SCFYR         XAFYR
     C                   MOVE      SCCASE        XACASE            7
     C                   CALL      'W0070R92'                           99
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    XACO#
     C                   PARM                    XAFND
     C                   PARM                    XAFYR
     C                   PARM                    XACASE
     C                   PARM                    QQCALL
     C                   PARM      *BLANKS       XAEOMB            9
     C*
     C                   ENDSR                                                  OPT92
     C*** END   ADD ***08/18/06 ***************************************
     C*** BEG   ADD ***03/29/10 ***************************************
     C*===============================================================
     C* OPT93 - SUBROUTINE FOR OPTION 93
     C*===============================================================
     C     OPT93         BEGSR
      *
     C  N09              MOVE      'C'           ##MODE
     C   09              MOVE      'D'           ##MODE
     C                   CALL      'W0590R'      ACDPL                  99
      *
     C                   ENDSR                                                  OPT93
     C*** END ADD ****** 03/29/10 ************************************
      *
     C*** BEG   ADD ***04/06/10 ***************************************
     C*===============================================================
     C* OPT94 - SUBROUTINE FOR OPTION 94 (ATTORNEY ORDERS)
     C*===============================================================
     C     OPT94         BEGSR
      *
     C     CLTID         IFEQ      '025'
     C  N09              MOVE      'C'           ##MODE
     C   09              MOVE      'D'           ##MODE
     C                   CALL      'W4343R'      O94PL                  99
     C                   ENDIF
      *
     C                   ENDSR                                                  OPT94
     C*** END ADD ****** 04/06/10 ************************************
     C*** BEG ADD *** 06/01/11 ***************************************
     C*===============================================================
     C* OPT95 - SUBROUTINE FOR OPTION 95 (WAGE CALC)
     C*===============================================================
     C     OPT95         BEGSR
      *
     C     CLTID         IFEQ      '025'
     C  N09              MOVE      ' '           ##MODE                           C
     C   09              MOVE      '1'           ##MODE                           D
     C                   CALL      'W0595R'      ACDPL                  99
     C                   ENDIF
      *
     C                   ENDSR                                                  OPT95
     C*** END ADD *** 06/01/11 ************************************
      *
     C*===============================================================
     C* OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
     C*===============================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKI         CASEQ     *ON           KIDO
     C                   CAS                     ENTRDO
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
      *
     C*===============================================================
     C* OTHOPT - SUBROUTINE FOR OTHER OPTIONS
     C*===============================================================
     C     OTHOPT        BEGSR
     C*
      ***** BEG ADD ******* 06/02/99 **********************************
     C                   MOVE      @@CALL        SVCALL
     C                   MOVE      *IN09         SVIN09
      *
     C     SCAUTH        IFEQ      'I'
     C     ##RCDS        ANDEQ     *ON
     C                   MOVE      *ON           *IN09
     C                   MOVE      '1'           @@CALL
     C                   MOVE      '1'           QQCALL
     C                   ENDIF                                                  SCAUTH=I
      *
      ***** END ADD ******* 06/02/99 **********************************
     C*
     C                   MOVE      ##CO#         QQCO#
     C                   MOVE      SCFND         QQFND
     C                   MOVE      SCFYR         QQFYR
     C                   MOVE      SCCASE        QQCASE
     C*
     C     SCSEL         CASEQ     '6'           OPT6
     C     SCSEL         CASEQ     '7'           OPT7
     C     SCSEL         CASEQ     '8'           OPT8
     C     SCSEL         CASEQ     '9'           OPT9
     C     SCSEL         CASEQ     '10'          OPT10
     C     SCSEL         CASEQ     '11'          OPT11
     C     SCSEL         CASEQ     '12'          OPT12
     C*** BEG ADD ****** 03/23/10 ********************************************
     C     SCSEL         CASEQ     '13'          OPT13
     C*** END ADD ****** 03/23/10 ********************************************
     C     SCSEL         CASEQ     '14'          OPT14
     C     SCSEL         CASEQ     '15'          OPT15
     C     SCSEL         CASEQ     '16'          OPT16
     C     SCSEL         CASEQ     '17'          OPT17
     C     SCSEL         CASEQ     '18'          OPT18
     C     SCSEL         CASEQ     '19'          OPT19
     C     SCSEL         CASEQ     '21'          OPT21
     C     SCSEL         CASEQ     '22'          OPT22
     C**         SCSEL     CASEQ'23'      OPT23
     C*** BEG ADD ****** 05/07/07 ********************************************
     C     SCSEL         CASEQ     '23'          OPT23
     C*** END ADD ****** 05/07/07 ********************************************
     C     SCSEL         CASEQ     '24'          OPT24
     C     SCSEL         CASEQ     '25'          OPT25
     C**         SCSEL     CASEQ'26'      OPT26
     C**         SCSEL     CASEQ'27'      OPT27
     C**         SCSEL     CASEQ'28'      OPT28
     C     SCSEL         CASEQ     '30'          OPT30
     C*** BEGIN ADD *** 01/03/05 **************************************
     C     SCSEL         CASEQ     '31'          OPT31
     C*** END   ADD *** 01/03/05 **************************************
     C*** BEG ADD ****** 10/19/07 ************************************
     C     SCSEL         CASEQ     '32'          OPT32
     C*** END ADD ****** 10/19/07 ************************************
     C     SCSEL         CASEQ     '50'          OPT50
     C     SCSEL         CASEQ     '51'          OPT51
     C     SCSEL         CASEQ     '52'          OPT52
     C     SCSEL         CASEQ     '53'          OPT53
     C     SCSEL         CASEQ     '54'          OPT54
     C     SCSEL         CASEQ     '55'          OPT55
     C     SCSEL         CASEQ     '60'          OPT60
     C     SCSEL         CASEQ     '61'          OPT61
     C     SCSEL         CASEQ     '62'          OPT62
     C     SCSEL         CASEQ     '63'          OPT63
     C     SCSEL         CASEQ     '64'          OPT64
     C     SCSEL         CASEQ     '65'          OPT65
     C     SCSEL         CASEQ     '66'          OPT66
     C*** BEGIN ADD ***  07/28/09  ************************************
     C     SCSEL         CASEQ     '67'          OPT67
     C*** END ADD *****  07/28/09  ************************************
      ***** BEG ADD ***** 08/09/07 ************************************
     C     SCSEL         CASEQ     '70'          OPT70
      ***** END ADD ***** 08/09/07 ************************************
     C     SCSEL         CASEQ     '80'          OPT80
     C     SCSEL         CASEQ     '81'          OPT81
     C     SCSEL         CASEQ     '82'          OPT82
     C     SCSEL         CASEQ     '83'          OPT83
      ***** BEG CHG ***** 02/05/07 ************************************
     C     SCSEL         CASEQ     '90'          OPT90
      ***** END CHG ***** 02/05/07 ************************************
     C*** BEGIN ADD ***08/18/06 ***************************************
     C     SCSEL         CASEQ     '91'          OPT91
     C     SCSEL         CASEQ     '92'          OPT92
     C*** END   ADD ***08/18/06 ***************************************
     C*** BEG ADD ****** 03/29/10 ************************************
     C     SCSEL         CASEQ     '93'          OPT93
     C*** END ADD ****** 03/29/10 ************************************
     C*** BEGIN ADD ***04/06/10 ***************************************
     C     SCSEL         CASEQ     '94'          OPT94
     C*** END   ADD ***04/06/10 ***************************************
     C*** BEG ADD *** 06/01/11 ***************************************
     C     SCSEL         CASEQ     '95'          OPT95
     C*** END ADD *** 06/01/11 ***************************************
     C                   END
     C*
      ***** BEG ADD ******* 06/02/99 **********************************
     C                   MOVE      SVCALL        @@CALL
     C                   MOVE      SVIN09        *IN09
      ***** END ADD ******* 06/02/99 **********************************
     C*
     C                   ENDSR                                                  OTHOPT
      *
      *
      *
     C*===============================================================
     C* PGMEND - SUBROUTINE TO END THE PROGRAM
     C*===============================================================
     C     PGMEND        BEGSR
     C*
     C                   MOVE      *ON           *INLR
      *
     C                   CLOSE     WMCMLI
     C                   CLOSE     WMCML9
     C                   CLOSE     WMCML1
     C                   CLOSE     WMCMLG
      /Free
      /If Defined (WEB)
       Close W0070F;
      /Endif
      /End-free
     C*
     C                   ENDSR                                                  PGMEND
      *
      *
      *
     C*===============================================================
     C* PGMSTR - SUBROUTINE TO START THE PROGRAM
     C*===============================================================
     C     PGMSTR        BEGSR
     C*
     C*** BEG ADD ***** 10/30/07 ******************************************
     C*** BEG DEL ***** 07/30/08 ******************************************
     C***********          CALL 'DLYJOB'
     C***********          PARM 5         XXTIME  20
     C*** END DEL ***** 07/30/08 ******************************************
     C*** END ADD ***** 10/30/07 ******************************************
     C***  Begin Add  ***  09/06/05  **************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C***  End   Add  ***  09/06/05  **************************************
      ***** BEG ADD ******* 06/02/99 **********************************
     C                   CALL      'SA008R'      RSTPL                  99
      ***** END ADD ******* 06/02/99 **********************************
      ***** BEG ADD ******* 02/16/10 **********************************
     C                   SETON                                        67
     C                   SETON                                        63
      ***** END ADD ******* 02/10/10 **********************************
     C*
     C***  BEGIN ADD  ***  01/12/07  *********************************
     C     CLTID         IFEQ      '071'
     C                   MOVE      *OFF          *IN12
     C                   ELSE
     C                   MOVE      *ON           *IN12
     C                   ENDIF
     C***  END   ADD  ***  01/12/07  *********************************
     C*
     C     UPKY          CHAIN     PUP01                              99
     C                   Z-ADD     UPFND         KKFND
     C*** BEGIN ADD *** 04/12/06 **************************************
     C     UPFL4         IFEQ      'Y'
     C                   MOVE      *ON           *IN54
     C                   ELSE                                                   x- FL4=Y
     C                   MOVE      *OFF          *IN54
     C                   END                                                    e- FL4=Y
     C*** END ADD ***** 04/12/06 **************************************
     C     CXKY          CHAIN     PCX01                              99
     C*
     C                   Z-ADD     10            SF1PAG            5 0
     C*
      ***** BEG CHG ***** 03/06/06 ************************************
     C**************       Z-ADD*DATE     SRDTI
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
      ***** END CHG ***** 03/06/06 ************************************
     C                   CALL      'SRCVTDAT'    DTCYMD                         MDY --> YMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    30    SEARCH
     C*
     C*  IF THE SORT VALUE IS BLANK, DEFAULT TO THE CONTROL FILE VALUE.
     C*
     C     @@SORT        IFEQ      *BLANKS
     C                   MOVE      CXCSDF        @@SORT
     C                   END
     C*
     C                   SELECT
     C     @@SORT        WHENEQ    'CA'                                         CLM ALPHA SORT
     C                   Z-ADD     1             VIEW#
     C***  Begin add  ***  02/16/10  *********************************
     C***  Begin CHG  ***  07/19/10  *********************************
     C***        CLTID     IFEQ '133'
     C***  END   CHG  ***  07/19/10  *********************************
     C                   MOVEL     AHD(1)        HD
     C                   MOVE      '1'           *IN63
     C***  BEG   CHG  ***  07/19/10  *********************************
     C**                   ENDIF
     C***  End   CHG  ***  07/19/10  *********************************
     C***  End   add  ***  02/16/10  *********************************
     C     @@SORT        WHENEQ    'SS'                                         SS# SORT
     C                   Z-ADD     2             VIEW#
     C     @@SORT        WHENEQ    'CN'                                         GRP/YR/CASE
     C                   Z-ADD     3             VIEW#
     C***  Begin add  ***  02/16/10  *********************************
     C***  Begin CHG  ***  07/19/10  *********************************
     C**         CLTID     IFEQ '133'
     C***  END   CHG  ***  07/19/10  *********************************
     C                   MOVEL     AHD(2)        HD
     C***  Begin CHG  ***  07/19/10  *********************************
     C**                   ENDIF
     C***  END   CHG  ***  07/19/10  *********************************
     C     @@SORT        WHENEQ    'SC'                                         STATE CLAIM #
     C                   Z-ADD     4             VIEW#
     C                   ENDSL
     C*
     C                   EXSR      GETHD#
     C                   MOVE      *OFF          SNGEMP            1
     C*** BEG ADD *** 08/18/06 ***************************************
     C     CLTID         IFNE      '114'
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      'NNN'         XXMIS
     C                   MOVE      '91'          XXNMBR
     C                   EXSR      UPDOFA
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      'NNN'         XXMIS
     C                   MOVE      '92'          XXNMBR
     C                   EXSR      UPDOFA
     C                   ENDIF
     C*** END ADD *** 08/18/06 ***************************************
     C*** BEG ADD *** 03/23/10 ***************************************
     C     CLTID         IFEQ      '139'
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      'NNN'         XXMIS
     C                   MOVE      '32'          XXNMBR
     C                   EXSR      UPDOFA
     C                   ENDIF
     C*** END ADD *** 03/23/10 ***************************************
     C*
     C     @@RRN         IFNE      *ZERO
      *
      * IF BEING CALLED FROM EMPLOYER MASTER,
      * DO NOT ALLOW OPT 80 (EMPLOYER SEARCH) OR F17 = SUBSET.
      *
     C     @@MODE        IFEQ      'Z'
     C*
     C                   MOVE      *ON           SNGEMP
     C***  BEGIN CHG  ***  08/16/99  *********************************
     C**********           MOVELCLRFKY    OFD,44           NO F17
     C                   MOVE      '2'           XXOPFN
     C                   MOVE      'NNN'         XXMIS
     C                   MOVE      '17'          XXNMBR
     C                   EXSR      UPDOFA
     C***  END   CHG  ***  08/16/99  *********************************
     C*
     C     @@RRN         CHAIN     PEM0P                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EMFND         SSFND
     C                   MOVE      EMEMP#        SSEMP#
     C***  BEGIN CHG  ***  05/10/99  *********************************
     C**********           MOVE EMDIV     SSDIV
     C*
     C     EMDIV         IFNE      *ZERO
     C                   MOVEL(P)  EMDIV         XXNUM
     C                   MOVEL     *BLANKS       EDTMSK
     C                   EXSR      CHGNUM
     C                   MOVEL     XXNUM         SSDIV
     C                   ELSE
     C                   MOVE      '0'           SSDIV
     C                   ENDIF                                                  EMDIV<>*ZERO
     C***  END   CHG  ***  05/10/99  *********************************
     C*
E01  C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   MOVE      'D'           @@MODE
     C***  BEGIN CHG  ***  08/16/99  *********************************
     C**********           MOVELCLRFKY    OFD,34           NO OPT 80
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      'NNN'         XXMIS
     C                   MOVE      '80'          XXNMBR
     C                   EXSR      UPDOFA
     C***  END   CHG  ***  08/16/99  *********************************
     C*
     C                   ELSE
     C*
     C     @@MODE        IFEQ      'M'
     C*
     C***  BEGIN CHG  ***  08/16/99  *********************************
     C**********           MOVELCLRFKY    OFD,35
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      'NNN'         XXMIS
     C                   MOVE      '81'          XXNMBR
     C                   EXSR      UPDOFA
     C***  END   CHG  ***  08/16/99  *********************************
     C*
     C                   ENDIF
     C*
     C*  IF THE @@RRN IS NOT ZERO, THEN DISPLAY THAT SINGLE RECORD.
     C*
     C                   MOVE      *ON           *IN20
     C                   MOVEA     '0000'        *IN(21)
     C                   MOVE      'Y'           FIRST             1
     C*
     C                   ENDIF                                                  @@MODE=Z
     C*
     C                   END
      *
      ***** BEG CHG ***** 02/05/07 ************************************
      *  If not Client 051, do not allow Option 90 to display
      *
     C     CLTID         IFNE      '051'
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      'NNN'         XXMIS
     C                   MOVE      '90'          XXNMBR
     C                   EXSR      UPDOFA
     C                   ENDIF                                                  CLTID <> 051
      ***** END CHG ***** 02/05/07 ************************************
     C*
      ***** BEG ADD ***** 03/29/10 ************************************
      *  If not Client 025, do not allow Option 93 to display
      *
     C     CLTID         IFNE      '025'
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      'NNN'         XXMIS
     C                   MOVE      '93'          XXNMBR
     C                   EXSR      UPDOFA
     C                   ENDIF                                                  CLTID <> 025
      ***** END ADD ***** 03/29/10 ************************************
     C*
     C*  IS WCMC INSTALLED AND INTERFACED WITH WCMS ?
     C*
     C                   CALL      'SA001R'
     C*** BEG CHG *** 10/12/98 ***************************************
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C**********
     C**********           PARM           QQWCIF  1
     C**********           PARM           QQFBIF  1        WCMC INSTALLED
     C**********           PARM           QQFL01  1        INTRFACD W/WCMS
     C**********
     C********** QQFBIF    IFEQ 'Y'
     C********** QQFL01    ANDEQ'Y'
     C     ##MCIF        IFEQ      'Y'
     C     ##WCIN        ANDEQ     'Y'
     C*** END CHG *** 10/12/98 ***************************************
     C                   MOVEL     'Y'           XXWCMC            1
     C                   END
     C*
     C*** BEG ADD *** 03/28/00 ***********************
     C                   CALL      'SRCHKPGM'                           99
     C                   PARM      'F0269R'      QQPGM
     C                   PARM      *BLANK        QQRTCD
     C*** END ADD *** 03/28/00 ***********************
     C*** BEGIN ADD *** 01/07/98 ***********************************************
     C*  If WCMC is not installed, do not display option 81.
     C*
     C     XXWCMC        IFNE      'Y'
     C     QQRTCD        OREQ      '1'                                          ADD 03/28/00
     C*
     C                   Z-ADD     1             X
     C     OF(X)         DOWEQ     '1'
     C*
     C                   MOVEL     OFD(X)        XXWK2A            2
     C     XXWK2A        IFEQ      '81'
     C                   MOVE      *BLANKS       OFD(X)
     C                   ENDIF
     C*
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   ENDIF
     C*** END ADD ***** 01/07/98 ***********************************************
     C***** BEGIN ADD **** 06/07/99 ***********************************
     C*
     C*  CHECK REINSURANCE FLAG, IF NOT Y, THEN DISABLE OPT. 82 & 83
     C*
     C                   MOVEL(P)  'SY'          XSKEY
     C     SYKY          CHAIN     SSY01                              60
     C     SYFL04        IFNE      'Y'
     C                   MOVE      '1'           XXOPFN            1
     C                   MOVE      'NNN'         XXMIS             3
     C                   MOVE      '82'          XXNMBR
     C                   EXSR      UPDOFA
     C                   MOVE      '83'          XXNMBR
     C                   EXSR      UPDOFA
     C                   ENDIF                                                  SYFL04=Y
     C*** BEGIN ADD ***  07/28/09  ************************************
     C     XXFL12        IFNE      'Y'
     C                   MOVE      '1'           XXOPFN            1
     C                   MOVE      'NNN'         XXMIS             3
     C                   MOVE      '67'          XXNMBR
     C                   EXSR      UPDOFA
     C                   ENDIF                                                  SYFL12 <> Y
     C*** END ADD *****  07/28/09  ************************************
     C*
     C*   CHECK DEDUCTIBLES FLAG, IF NOT Y, DISABLE OPT. 66
     C*
     C                   MOVEL(P)  'WC'          XSKEY
     C     SYKY          CHAIN     SWC01                              60
     C     WCFL06        IFNE      'Y'
     C                   MOVE      '1'           XXOPFN            1
     C                   MOVE      'NNN'         XXMIS             3
     C                   MOVE      '66'          XXNMBR            2
     C                   EXSR      UPDOFA
     C                   ENDIF                                                  WCFL06=Y
     C***** END   ADD **** 06/07/99 ***********************************
      *** BEG ADD **** 12/11/01 ************************************************
     C     WCFL09        IFNE      'Y'
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      'NNN'         XXMIS
     C                   MOVE      '18'          XXNMBR
     C                   EXSR      UPDOFA
      *** BEG ADD **** 10/19/07 ************************************************
     C*   CHECK FLAG, IF NOT Y, DISABLE OPT. 32
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      'NNN'         XXMIS
     C                   MOVE      '32'          XXNMBR
     C                   EXSR      UPDOFA
     C***** END   ADD **** 10/19/07 ***********************************
     C                   ENDIF                                                  WCFL09=Y
      *** END ADD **** 12/11/01 ************************************************
     C*** BEG ADD ****** 05/07/07 **********************************************
     C* Disable option 23, if claims summary not installed
     C* or if called from ww claims summary
     C     WCFL33        IFEQ      ' '
     C     Q1PRMS        ORGE      9
     C     @@PGM         ANDEQ     'W0070RX1'
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      'NNN'         XXMIS
     C                   MOVE      '23'          XXNMBR
     C                   EXSR      UPDOFA
     C*** BEG ADD ****** 10/30/07 **********************************************
     C* If called from claims summary, F12=Cancel should be allowed
     C                   MOVE      '2'           XXOPFN
     C                   MOVE      'YYY'         XXMIS
     C                   MOVE      '12'          XXNMBR
     C                   EXSR      UPDOFA
     C*** END ADD ****** 10/30/07 **********************************************
     C                   ENDIF                                                  Q1PRMS >= 9
     C*** END ADD ****** 05/07/07 **********************************************
     C*** BEG ADD ****** 08/09/07 **********************************************
     C*
     C* Disable option 70, if EFT Install Flag is not 'Y'
     C*
     C     WCFL35        IFNE      'Y'
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      'NNN'         XXMIS
     C                   MOVE      '70'          XXNMBR
     C                   EXSR      UPDOFA
     C                   ENDIF
     C*** END ADD ****** 08/09/07 **********************************************
      *** BEG ADD **** 04/06/10 ************************************************
     C*   IF CLIENT IS NOT 025, DISABLE OPT. 94
     C     CLTID         IFNE      '025'
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      'NNN'         XXMIS
     C                   MOVE      '94'          XXNMBR
     C                   EXSR      UPDOFA
      *** BEG ADD **** 06/01/11 ************************************************
     C** AND OPT 95
     C                   MOVE      '95'          XXNMBR
     C                   EXSR      UPDOFA
      *** END ADD **** 06/01/11 ************************************************
     C                   ENDIF
     C***** END   ADD **** 04/06/10 ***********************************
      ***** BEG ADD ******* 06/02/99 **********************************
      *
      *
     C     ##RCDS        IFEQ      *ON
     C     ##XFND        ANDEQ     'Y'
      *
     C                   EXSR      GETGRP
      *
      *  CALL CL TO CREATE AN OPEN QUERY FILE OVER THE FILES
      *
     C     OVRDB1        CAT(P)    'WMCMLI':0    XXOVRD           50
     C                   CAT       OVRDB2:0      XXOVRD
      *
     C                   CALL      'QCMDEXC'
     C                   PARM      XXOVRD        CMDSTR           50
     C                   PARM      50            LENGTH           15 5
      *
     C     OVRDB1        CAT(P)    'WMCML9':0    XXOVRD           50
     C                   CAT       OVRDB2:0      XXOVRD
      *
     C                   CALL      'QCMDEXC'
     C                   PARM      XXOVRD        CMDSTR           50
     C                   PARM      50            LENGTH           15 5
      *
     C     OVRDB1        CAT(P)    'WMCML1':0    XXOVRD           50
     C                   CAT       OVRDB2:0      XXOVRD
      *
     C                   CALL      'QCMDEXC'
     C                   PARM      XXOVRD        CMDSTR           50
     C                   PARM      50            LENGTH           15 5
     C*
     C     OVRDB1        CAT(P)    'WMCMLG':0    XXOVRD           50
     C                   CAT       OVRDB2:0      XXOVRD
      *
     C                   CALL      'QCMDEXC'
     C                   PARM      XXOVRD        CMDSTR           50
     C                   PARM      50            LENGTH           15 5
      *
     C                   CALL      QRYPGM        QRYPL                  99
      *
     C                   ENDIF                                                  ##RCDS=*ON
      *
     C                   OPEN      WMCMLI
     C                   OPEN      WMCML9
     C                   OPEN      WMCML1
     C                   OPEN      WMCMLG
      ***** END ADD ******* 06/02/99 **********************************
     C*
     C                   ENDSR                                                  PGMSTR
      *
      *
      *
     C*===============================================================
     C* READ - SUBROUTINE TO READ THE FILE
     C*===============================================================
     C     READ          BEGSR
     C*
     C                   MOVE      *OFF          XXVLDO
     C*
     C                   SELECT
     C     *IN20         WHENEQ    *ON
     C     FIRST         IFNE      'Y'
     C                   MOVE      *ON           *IN95
     C                   MOVE      'Y'           FIRST
     C                   ELSE
     C                   MOVE      'N'           FIRST
     C     @@RRN         CHAIN     PFRCDF                             95
     C                   END
     C**
     C     *IN21         WHENEQ    *ON
     C     SSFND         ANDEQ     *ZEROS
     C     KEY           READE     DBRCDF                                 95
     C**
     C     *IN22         WHENEQ    *ON
     C     SSFND         ANDEQ     *ZEROS
     C     KEY           READE     DBRCDF2                                95
     C**
     C     *IN23         WHENEQ    *ON
     C     SSFND         ANDEQ     *ZEROS
     C     KEY           READE     DBRCDF3                                95
     C**
     C     *IN24         WHENEQ    *ON
     C     SSFND         ANDEQ     *ZEROS
     C     KEY           READE     DBRCDF4                                95
     C**
     C     *IN21         WHENEQ    *ON
     C     SSFND         ANDNE     *ZEROS
     C     SSEMP#        ANDEQ     *ZEROS
     C     KEYB          READE     DBRCDB                                 95
     C                   eval      dbRRN  = dbRRb
     C**
     C     *IN22         WHENEQ    *ON
     C     SSFND         ANDNE     *ZEROS
     C     SSEMP#        ANDEQ     *ZEROS
     C     KEYB          READE     DBRCDB2                                95
     C                   eval      dbRRN2 = dbRR2b
     C**
     C     *IN23         WHENEQ    *ON
     C     SSFND         ANDNE     *ZEROS
     C     SSEMP#        ANDEQ     *ZEROS
     C     KEYB          READE     DBRCDF3                                95
     C**
     C     *IN24         WHENEQ    *ON
     C     SSFND         ANDNE     *ZEROS
     C     SSEMP#        ANDEQ     *ZEROS
     C     KEYB          READE     DBRCDB4                                95
     C                   eval      dbRRN4 = dbRR4b
     C**
     C     *IN21         WHENEQ    *ON
     C     SSFND         ANDNE     *ZEROS
     C     SSEMP#        ANDNE     *ZEROS
     C     KEYC          READE     DBRCDC                                 95
     C                   eval      dbRRN  = dbRRc
     C**
     C     *IN22         WHENEQ    *ON
     C     SSFND         ANDNE     *ZEROS
     C     SSEMP#        ANDNE     *ZEROS
     C     KEYC          READE     DBRCDC2                                95
     C                   eval      dbRRN2 = dbRR2c
     C**
     C     *IN23         WHENEQ    *ON
     C     SSFND         ANDNE     *ZEROS
     C     SSEMP#        ANDNE     *ZEROS
     C     KEYC          READE     DBRCDC3                                95
     C                   eval      dbRRN3 = dbRR3c
     C**
     C     *IN24         WHENEQ    *ON
     C     SSFND         ANDNE     *ZEROS
     C     SSEMP#        ANDNE     *ZEROS
     C     KEYC          READE     DBRCDC4                                95
     C                   eval      dbRRN4 = dbRR4c
     C**
     C                   ENDSL
     C*
     C                   ENDSR                                                  READ
      *
      *
      *
     C*===============================================================
     C* READP - SUBROUTINE TO READ THE FILE BACKWARDS
     C*===============================================================
     C     READP         BEGSR
     C*
     C                   MOVE      *OFF          XXVLDO
     C*
     C                   SELECT
     C     *IN20         WHENEQ    *ON
     C     FIRST         IFNE      'Y'
     C                   MOVE      *ON           *IN95
     C                   MOVE      'Y'           FIRST
     C                   ELSE
     C                   MOVE      'N'           FIRST
     C     @@RRN         CHAIN     PFRCDF                             95
     C                   END
     C**
     C     *IN21         WHENEQ    *ON
     C     SSFND         ANDEQ     *ZEROS
     C     KEY           READPE    DBRCDF                                 95
     C**
     C     *IN22         WHENEQ    *ON
     C     SSFND         ANDEQ     *ZEROS
     C     KEY           READPE    DBRCDF2                                95
     C**
     C     *IN23         WHENEQ    *ON
     C     SSFND         ANDEQ     *ZEROS
     C     KEY           READPE    DBRCDF3                                95
     C**
     C     *IN24         WHENEQ    *ON
     C     SSFND         ANDEQ     *ZEROS
     C     KEY           READPE    DBRCDF4                                95
     C**
     C     *IN21         WHENEQ    *ON
     C     SSFND         ANDNE     *ZEROS
     C     SSEMP#        ANDEQ     *ZEROS
     C     KEYB          READPE    DBRCDB                                 95
     C                   eval      dbRRN  = dbRRb
     C**
     C     *IN22         WHENEQ    *ON
     C     SSFND         ANDNE     *ZEROS
     C     SSEMP#        ANDEQ     *ZEROS
     C     KEYB          READPE    DBRCDB2                                95
     C                   eval      dbRRN2 = dbRR2b
     C**
     C     *IN23         WHENEQ    *ON
     C     SSFND         ANDNE     *ZEROS
     C     SSEMP#        ANDEQ     *ZEROS
     C     KEYB          READPE    DBRCDF3                                95
     C**
     C     *IN24         WHENEQ    *ON
     C     SSFND         ANDNE     *ZEROS
     C     SSEMP#        ANDEQ     *ZEROS
     C     KEYB          READPE    DBRCDB4                                95
     C                   eval      dbRRN4 = dbRR4b
     C**
     C     *IN21         WHENEQ    *ON
     C     SSFND         ANDNE     *ZEROS
     C     SSEMP#        ANDNE     *ZEROS
     C     KEYC          READPE    DBRCDC                                 95
     C                   eval      dbRRN  = dbRRc
     C**
     C     *IN22         WHENEQ    *ON
     C     SSFND         ANDNE     *ZEROS
     C     SSEMP#        ANDNE     *ZEROS
     C     KEYC          READPE    DBRCDC2                                95
     C                   eval      dbRRN2 = dbRR2c
     C**
     C     *IN23         WHENEQ    *ON
     C     SSFND         ANDNE     *ZEROS
     C     SSEMP#        ANDNE     *ZEROS
     C     KEYC          READPE    DBRCDC3                                95
     C                   eval      dbRRN3 = dbRR3c
     C**
     C     *IN24         WHENEQ    *ON
     C     SSFND         ANDNE     *ZEROS
     C     SSEMP#        ANDNE     *ZEROS
     C     KEYC          READPE    DBRCDC4                                95
     C                   eval      dbRRN4 = dbRR4c
     C**
     C                   ENDSL
     C*
     C                   ENDSR                                                  READP
      *
     C*===============================================================
     C* SETGT - SUBROUTINE TO SET THE FILE > THE SELECTED FIELD
     C*===============================================================
     C     SETGT         BEGSR
     C*
     C                   SELECT
     C     SSFND         WHENEQ    *ZEROS
     C   21KEY1          SETGT     DBRCDF                             96
     C   22KEY2          SETGT     DBRCDF2                            96
     C   23KEY3          SETGT     DBRCDF3                            96
     C   24KEY4          SETGT     DBRCDF4                            96
     C     SSFND         WHENNE    *ZEROS
     C     SSEMP#        ANDEQ     *ZEROS
     C   21KEY1B         SETGT     DBRCDB                             96
     C   22KEY2B         SETGT     DBRCDB2                            96
     C   23KEY3B         SETGT     DBRCDF3                            96
     C   24KEY4B         SETGT     DBRCDB4                            96
     C     SSFND         WHENNE    *ZEROS
     C     SSEMP#        ANDNE     *ZEROS
     C   21KEY1C         SETGT     DBRCDC                             96
     C   22KEY2C         SETGT     DBRCDC2                            96
     C   23KEY3C         SETGT     DBRCDC3                            96
     C   24KEY4C         SETGT     DBRCDC4                            96
     C                   ENDSL
     C*
     C                   ENDSR                                                  SETGT
      *
     C*===============================================================
     C* SETLL - SUBROUTINE TO SET THE FILE < THE SELECTED FIELD
     C*===============================================================
     C     SETLL         BEGSR
     C*
     C                   SELECT
     C     SSFND         WHENEQ    *ZEROS
     C   21KEY1          SETLL     DBRCDF                             96
     C   22KEY2          SETLL     DBRCDF2                            96
     C   23KEY3          SETLL     DBRCDF3                            96
     C   24KEY4          SETLL     DBRCDF4                            96
     C     SSFND         WHENNE    *ZEROS
     C     SSEMP#        ANDEQ     *ZEROS
     C   21KEY1B         SETLL     DBRCDB                             96
     C   22KEY2B         SETLL     DBRCDB2                            96
     C   23KEY3B         SETLL     DBRCDF3                            96
     C   24KEY4B         SETLL     DBRCDB4                            96
     C     SSFND         WHENNE    *ZEROS
     C     SSEMP#        ANDNE     *ZEROS
     C   21KEY1C         SETLL     DBRCDC                             96
     C   22KEY2C         SETLL     DBRCDC2                            96
     C   23KEY3C         SETLL     DBRCDC3                            96
     C   24KEY4C         SETLL     DBRCDC4                            96
     C                   ENDSL
     C*
     C                   ENDSR                                                  SETLL
      *
      *
      *
     C*===============================================================
     C* SF1UPD - SUBROUTINE TO PROCESS A DELETED RECORD
     C*===============================================================
     C     SF1UPD        BEGSR
     C*
     C     ##KL          IFEQ      *OFF
     C     ##KC          ANDEQ     *OFF
     C                   MOVEL(P)  DELETD        SCDATA
     C                   END
     C*
     C     ##KL          IFEQ      *ON
     C     ##KC          OREQ      *ON
     C     ##RRN         CHAIN     PFRCDF                             99
     C     *IN99         IFEQ      *OFF
     C                   EXSR      MVDBSC
     C                   Z-ADD     ##RRN         SCDBRR
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR                                                  SF1UPD
      *
      *
      *
     C*===============================================================
     C* SLCTRD - SUBROUTINE TO MOVE FIELDS FROM SELECTED RECORDS
     C*===============================================================
     C     SLCTRD        BEGSR
     C*
     C                   MOVE      SCDBRR        @@RRN
     C*
     C                   ENDSR                                                  SLCTRD
     C*
     C*** BEGIN ADD *** 09/01/00 *************************************
     C*===============================================================
     C* STRING - SUBROUTINE TO SEARCH STRINGS
     C*===============================================================
     C*
     C     STRING        BEGSR
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
     C     SCNRES        IFEQ      0
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SCNRES=0
     C*
     C                   ENDSR                                                  STRING
     C*
     C*** END ADD ***** 09/01/00 *************************************
     C*===============================================================
     C* SUBSET - SUBROUTINE TO SUBSET THE LIST OF CLAIMS
     C*===============================================================
     C     SUBSET        BEGSR
     C*
B01  C                   SELECT
     C*
     C     SSFND         WHENNE    0
     C     CMFND         ANDNE     SSFND
     C                   MOVE      *OFF          MEETSS
     C*
     C     SSYEAR        WHENNE    0
     C     CMFYR         ANDNE     SSYEAR
     C                   MOVE      *OFF          MEETSS
     C*
     C*** BEGIN ADD *** 09/01/00 *************************************
     C*  Check accident date against the subset
     C*
     C     SSACDT        WHENNE    0
     C     CMACDT        ANDNE     XXACDT
     C                   MOVE      *OFF          MEETSS
     C*** END ADD ***** 09/01/00 *************************************
     C*
     C     SSEMP#        WHENNE    0
     C     CMEMP#        ANDNE     SSEMP#
     C                   MOVE      *OFF          MEETSS
     C*
     C     SSDIV         WHENNE    *BLANKS
     C                   MOVE      SSDIV         NNDIV
     C     CMDIV         IFNE      NNDIV
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSCTG1        WHENNE    *BLANKS
     C     CMCTG1        ANDNE     SSCTG1
     C                   MOVE      *OFF          MEETSS
     C*
     C*** BEGIN ADD *** 04/12/06 **************************************
     C*    Lost time flag
     C     SSLTFL        WHENNE    *BLANKS
     C     CMLTFL        ANDNE     SSLTFL
     C                   MOVE      *OFF          MEETSS
     C*    User staff codes
     C     SSSTAF        WHENNE    *BLANKS
     C     CMCLEX        ANDNE     SSSTAF
     C     CMSUPR        ANDNE     SSSTAF
     C     CMAMAN        ANDNE     SSSTAF
     C     CMUMAN        ANDNE     SSSTAF
     C     CMNRSE        ANDNE     SSSTAF
     C     CMFEAD        ANDNE     SSSTAF
     C     CMSDFA        ANDNE     SSSTAF
     C     CMESUP        ANDNE     SSSTAF
     C********   CMCEC     ANDNESSSTAF
     C                   MOVE      *OFF          MEETSS
     C*** END ADD ***** 04/12/06 **************************************
     C*** BEGIN ADD *** 02/16/10 **************************************
     C*    Jurisdiction State
     C     SJRST         WHENNE    *BLANKS
     C     CMJRST        ANDNE     SJRST
     C                   MOVE      *OFF          MEETSS
     C*** END ADD ***** 02/10/10 **************************************
E01  C                   ENDSL
      *
      ***** BEG ADD ******* 06/02/99 **********************************
      *
      * Check user authorization to record
      *
     C                   MOVE      'A'           SCAUTH
      *
     C     ##XFND        IFEQ      'Y'
     C     ##XEMP        OREQ      'Y'
      *
     C     ##RCDS        IFEQ      *ON
     C*
     C                   CALL      'SA008R'      RSTPL1                 99
      *
     C     ##RTCD        IFEQ      *ON
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  ##RTCD=*ON
     C*
     C     ##RTCD        IFEQ      *OFF
     C     ##AUTH        ANDEQ     'I'
     C     @@CALL        ANDEQ     '2'
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  ##RTCD=*OFF
      *
     C                   ENDIF                                                  ##RCDS=*ON
      *
     C                   ENDIF                                                  ##XFND ##XEMP
      *
      ***** END ADD ******* 06/02/99 **********************************
      *
     C*** BEG ADD ****** 05/25/07 **********************************************
     C*** Call claims security API to check claim authority
     C                   MOVE      CMCO#         #ACO#
     C                   MOVE      CMFND         #AFND
     C                   MOVE      CMFYR         #AFYR
     C                   MOVE      CMCASE        #ACASE
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     ##RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                     RTCD = '2'
     C*** END ADD ****** 05/25/07 **********************************************
     C*
     C*** BEGIN ADD *** 09/01/00 *************************************
     C*  Check claimant name against the subset
     C*
     C     SSNAME        IFNE      *BLANKS
     C     MEETSS        ANDEQ     *ON
     C*
     C     LO:UP         XLATE(P)  CMLSN         U1PDSC
     C     LO:UP         XLATE(P)  SSNAME        U2PDSC
     C                   EXSR      STRING
     C*
     C     MEETSS        IFEQ      *OFF
     C                   MOVE      *ON           MEETSS
     C     LO:UP         XLATE(P)  CMFSN         U1PDSC
     C     LO:UP         XLATE(P)  SSNAME        U2PDSC
     C                   EXSR      STRING
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END ADD ***** 09/01/00 *************************************
      ***** BEG ADD ******* 03/03/04 **********************************
     C*  Check status against the subset
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     CMSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      ***** END ADD ******* 03/03/04 **********************************
     C*
     C***  Begin Add  ***  09/06/05  **************************************
     C                   SELECT
     C     CLTID         WHENEQ    '135'
     C     *DTAARA       DEFINE    CLAIMS        CLMS
     C     *LOCK         IN        *DTAARA
     C                   OUT       *DTAARA
     C                   MOVE      CMFND         ALPFND            3
     C                   MOVE      CMCASE        ALPCLM            7
     C     ALPCLM        IFEQ      CRST1
     C     ALPFND        ANDEQ     FRST1
     C*** Begin dlt *** 05/04/06 ******************************************
     C********** Q1USER    ANDNEURST1
     C*** End dlt ***** 05/04/06 ******************************************
     C     ALPCLM        OREQ      CRST2
     C     ALPFND        ANDEQ     FRST2
     C*** Begin dlt *** 05/04/06 ******************************************
     C********** Q1USER    ANDNEURST2
     C*** End dlt ***** 05/04/06 ******************************************
     C     ALPCLM        OREQ      CRST3
     C     ALPFND        ANDEQ     FRST3
     C*** Begin dlt *** 05/04/06 ******************************************
     C********** Q1USER    ANDNEURST3
     C*** End dlt ***** 05/04/06 ******************************************
     C     ALPCLM        OREQ      CRST4
     C     ALPFND        ANDEQ     FRST4
     C*** Begin dlt *** 05/04/06 ******************************************
     C********** Q1USER    ANDNEURST4
     C*** End dlt ***** 05/04/06 ******************************************
     C*** Begin add *** 05/04/06 ******************************************
     C                   DO
     C     ALPCLM        IFEQ      CRST1
     C     ALPFND        ANDEQ     FRST1
     C     Q1USER        ANDEQ     URST1
     C     ALPCLM        OREQ      CRST2
     C     ALPFND        ANDEQ     FRST2
     C     Q1USER        ANDEQ     URST2
     C     ALPCLM        OREQ      CRST3
     C     ALPFND        ANDEQ     FRST3
     C     Q1USER        ANDEQ     URST3
     C     ALPCLM        OREQ      CRST4
     C     ALPFND        ANDEQ     FRST4
     C     Q1USER        ANDEQ     URST4
     C                   LEAVE
     C                   ENDIF
     C*** End add ***** 05/04/06 ******************************************
     C                   MOVE      *OFF          MEETSS
     C*** Begin add *** 05/04/06 ******************************************
     C                   ENDDO
     C*** End add ***** 05/04/06 ******************************************
     C                   ENDIF
     C                   ENDSL
     C***  End   Add  ***  09/06/05  **************************************
     C*
     C                   ENDSR                                                  SUBSET
     C*===============================================================
      *  1         2         3         4         5         6         7         8
      *  01234567890123456789012345678901234567890123456789012345678901234567890
** HD - COLUMN HEADINGS
Opt Claimant Name        Soc Sec #  Grp  Yr  Case #  Dt of Inj LT St  Reopen Dt
Opt Soc Sec # Claimant Name         Grp  Yr  Case #  Dt of Inj LT St  Reopen Dt
Opt Grp Yr   Case    Soc Sec#   Claimant Name        Dt of Inj LT St  Reopen Dt
Opt State Claim #                   Grp  Yr  Case #  Dt of Inj LT St  Reopen Dt
Opt Claimant Name        Soc Sec #  Nurse       Ext#                Ext#
Opt Soc Sec # Claimant Name         Nurse       Ext#                Ext#
Opt Grp Yr   Case        Soc Sec #  Nurse       Ext#                Ext#
Opt State Claim #                   Nurse       Ext#                Ext#
Opt Claimant Name        Soc Sec #   Employer  Div  Name       Supervisor  Ext#
Opt Soc Sec # Claimant Name          Employer  Div  Name       Supervisor  Ext#
Opt Grp Yr   Case        Soc Sec #   Employer  Div  Name       Supervisor  Ext#
Opt State Claim #                    Employer  Div  Name       Supervisor  Ext#
Opt Claimant Name        Soc Sec #  Accident Desc    Body Part      Tot Incurred
Opt Soc Sec # Claimant Name         Accident Desc    Body Part      Tot Incurred
Opt Grp Yr   Case        Soc Sec #  Accident Desc    Body Part      Tot Incurred
Opt State Claim #                   Accident Desc    Body Part      Tot Incurred
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
1066=Employer Info          YYN
1077=Claimant Info          YYN
1088=Accident Info          YYN
1099=Mgt Info               YYN
11010=Medical Info          YYN
11111=Benefit Info          YYN
11212=Rsv/Pmt Summary       YYN
11313=Suspension InformationYYY
11414=DARTS                 YYN
11515=State Specific        YYN
11616=Legal Info            YYN
11717=Addl Address          YYN
11818=EDI                   YYN
11919=Treating Phys         YYN
12121=Diary Items           YYN
12222=Benefit Diary         YYN
12323=Claim Summary         YYN
12424=Add'l Claimants       YYN                            10/05/09
13030=Dept Letters          YNN
13131=Print FROI            YYN
13232=EDI Info              YYN
15050=Payment Searches      YYN
15151=Check Request         YNN
15252=Manual Check          YNN
15353=Reoccurring Comp      YNN
15454=Recovery Request      YNN
15555=Payment Rqst Search   YYN
16060=Pmt History           YYN
16161=Reserve History       YYN
16262=Clm Sts History       YYN
16363=I/M/R History         YYN
16464=AWW History           YYN
16565=Work History          YYN
16666=Deduct History        YYN
16767=CMS Log History       YYN
17070=EFT Account Info      YYN
18080=Emplr Inquiry         YYN
18181=WCMC Inquiry          YYN
18282=Rein Policy           YYN
18383=Partial Filing        YYN
19090=MMI Info              YYN
19191=Recurring Comp Info   YYN
19292=Payment History SearchYYN
19393=Rsv Worksheet         YYN                            03/29/10
19494=Attny Orders          YYN                            04/06/10
19595=Wage Calc             YYN                            06/01/11
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
209F9=Change info           YYN
211F11=Change view          YYY
212F12=Cancel               NYY
215F15=Fold/Truncate        YYY
217F17=Subset               YYY
3
** AHD -COLUMN HEADINGS
Opt Claimant Name        Soc Sec #  ST GRP  YR  CASE #  Dt Inj LT St  Reopen
Opt Grp  Yr  Case  ST Soc Sec#  Claimant Name        Dt Inj    LT St  Reopen
Opt Soc Sec # Claimant Name         ST Grp Yr  Case #   Dt Inj LT St  Reopen
Opt State Claim #                   ST Grp Yr  Case #   Dt Inj LT St  Reopen
Opt Claimant Name        Soc Sec #  Nurse       Ext#                Ext#
Opt Soc Sec # Claimant Name         Nurse       Ext#                Ext#
Opt Grp Yr   Case        Soc Sec #  Nurse       Ext#                Ext#
Opt State Claim #                   Nurse       Ext#                Ext#
Opt Claimant Name        Soc Sec #   Employer  Div  Name       Supervisor  Ext#
Opt Soc Sec # Claimant Name          Employer  Div  Name       Supervisor  Ext#
Opt Grp Yr   Case        Soc Sec #   Employer  Div  Name       Supervisor  Ext#
Opt State Claim #                    Employer  Div  Name       Supervisor  Ext#
Opt Claimant Name        Soc Sec #  Accident Desc    Body Part      Tot Incurred
Opt Soc Sec # Claimant Name         Accident Desc    Body Part      Tot Incurred
Opt Grp Yr   Case        Soc Sec #  Accident Desc    Body Part      Tot Incurred
Opt State Claim #                   Accident Desc    Body Part      Tot Incurred
