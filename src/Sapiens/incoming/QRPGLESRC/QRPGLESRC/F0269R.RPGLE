     Hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE F0269R - WORK WITH MEDICAL CASES - CASE SELECTION
     H*****************************************************************
     H* PROGRAM: F0269R
     H* DESCRIPTION: Work with Medical Cases: Case Selection
     H*****************************************************************
     H*
     H* ** NOTE **
     H*    ANY CHANGES TO OPTION ARRAY WILL NEED TO ADDRESS THE
     H*    SEQUENCE CONDITIONING IN THE PGMSTR SUBROUTINE
     H*      TEST BY TURNING EDI INSTALL FLAG OFF
     H*
     H*****************************************************************
     H*
     H*     * * * * *  IMPORTANT PROGRAMMERS NOTE!  * * * * *
     H*
     H*    THE DISPLAY FILE F0269F MUST BE COMPILED WITH PARAMETER:
     H*
     H*                       RSTDSP  = *YES
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**                                                             **
     H**      16      Subscriber # is restricted                     **
     H**      17      Group # is restricted                          **
     H**      18      Employer/division is restricted                **
     H**      19      Group expired or claim denied                  **
     H**                                                             **
     H*****************************************************************
     H** REVISIONS:
     h*                                                               *
     h*  12/18/24 C:342 Project : MC447 Phase 2                       *
     h*                 1.F10 Search disabled on the Group Field      *
     h*                 DSPF Changes:                                 *
     h*                 1.Changed Existing Group field into Typeahead *
     h*                                                               *
     H*  06/09/20  C:295 Project : SY420                              *
     H*                  Mask SSN                                     *
     h*                                                               *
     H*
     H*  08/06/13 C:062 Redmine: 3544 LSAT
     H*                 Initialize DBRRN-9.
     H*
     H*  11/13/12 C:287 Project: MC367
     H*                 Remove options/functions for GS access to Bill Entry
     H*
     H*  12/14/11 C:277 Project: 114MBB
     H*                 Add option 22 - Work with Payment Limit BucketsEntry
     H*
     H*  03/22/11 C:257 SUPPORT: 74402
     H*                 Retrofitted Project 114WIH from archived library ARC114
     H*  04/12/05 C:132 Project: 114WIH
     H*                 Block User Access to Claims
     H*
     H*   1/18/09 C:58  PROJECT: 139MGI / Display MCR0676 if search on
     H*     STATUS, MCR0781 if invalid status.
     H*   5/21/08 C:58  PROJECT: 139MGI / REMOVAL OF SUPPORT ADDITION
     H**  *** REMOVE requirement to enter subcriber added 11/08/07 ***
     H**                                                             **
     H* 12/27/07 C:58 PROJECT 139MGI Retrofit 12/04 V7R1 CORRECTIONS
     H*
     H*  11/08/07 C:47  PROJECT: 139MGI / SUPPORT
     H*                 On subset screen, a subscriber must be entered.
     H*
     H*  05/25/07 C:191 PROJECT: WC1515
     H*                 Check claims authority
     H*
     H*  04/20/07 C:58  PROJECT: 139MGI
     H*                 Add new subsets to W/W medical cases
     H*
     H* 10/06/06 C:181 PROJECT 114WMF
     H*                Back to Base - Medical Case Information(114MAC
     H*                               and 114MAD)
     H*
     H* 10/06/06 C:62  PROJECT: 114MAD
     H*                ADD ADDITIONAL VIEW FOR TREATMENT SITE.
     H*
     H*  09/12/06 C:62  PROJECT: 114MAC
     H*                 ADD PARM TO F0263R TO DETERMINE WORKFLOW OR NOT
     H*
     H*  02/26/02 C:62  Project: MC114 36179
     H*                 COMPILE ONLY
     H*
     H*  10/29/02 C:62  PROJECT: MC253
     H*                 ADD MULTI GROUP RESTRICTIONS LOGIC.
     H*
     H*  12/14/01 C:62  PROJECT: MC111 30098
     H*                 -REDEFINE INVOICED 'M' TYPE EOB TO ALSO BE IF
     H*                  EAEPFL = 'P'(BEEN THRU BATCH) IF AUTO CHECK
     H*
     H*  03/23/01 C:62  PROJECT: MCMNT
     H*                 -ADD A PARM TO CALL OF F0263R.
     H*                 -MAKE OPTION 82 UNAVAILABLE IN INQUIRY MODE.
     H*
     H*  11/30/00 C:62  PROJECT: MCMNT
     H*                 FROM OPT81, MAKE SURE WE RETURN TO THE WORK
     H*                 WITH SCREEN, NOT THE MENU.
     H*
     H*  05/19/00 C:62  PROJECT: MC172
     H*                 ADDED ADDITIONAL PARM TO CALL TO F3040R.
     H*
     H*  05/16/00 C:33 SEQ#: OP082/21740                              *
     H*                ALLOW FOR DIVISION RESTRICTION.                *
     H*
     H*  05/03/00 C:62  PROJECT: MCMNT
     H*                 -FIXED OPTION 12.
     H*
     H*  04/26/00 C:62  PROJECT: AR001
     H*                 -ADDED OPTION 21.
     H*
     H*  02/23/00 C:62  PROJECT: MCMNT
     H*                 -DEACTIVATE OPTIONS 18 AND 19.
     H*                 -CHANGE UPFL1 TO UPMRGR
     H*
     H*  02/09/00 C:31  Support: 21741
     H*                 Deleted claims in FMCMP
     H*                 1. Add CASE to end of logicals
     H*
     H*  12/17/99 C:62  PROJECT: Y2K
     H*                 CHANGED PARM LIST OF FA720R
     H*
     H*  11/09/99 C:53  PROJECT: MCMNT
     H*                 CORRECTED MODE "##CALL" ON CALL TO F0278R
     H*
     H*  10/18/99 C:53  PROJECT: MCMNT
     H*                 CORRECTED MODE "##CALL" ON CALL TO F0277R
     H*
     H*  09/27/99 C:62  PROJECT:MC212
     H*                 GETTING T/M TO WORK.
     H*
     H*  09/07/99 C:62  PROJECT:MC212
     H*                 GETTING T/M TO WORK.
     H*
     H*  08/31/99 C:53  PROJECT:MC204 SUPPORT # 19461
     H*                 CHANGES FOR PARM CALL TO W0070R TO CORRECT
     H*                 AN UPDATE AND OPTION ISSUE.
     H*
     H*  07/05/99 C:62  PROJECT:MC204
     H*                 -CHANGE CALL PARAMETER OF OPT81 TO WHATEVER
     H*                  THE CALL VALUE OF F0269R.
     H*
     H*  07/01/99 C:62  PROJECT:MC207
     H*                 -CHANGE THE HEADINGS FOR SUB#/GRP/YR/CASE#
     H*
     H*  06/22/99 C:53  KY010
     H*                 REPLACE W0270R5 WITH F0875R FOR WCMC EDI.
     H*
     H*  06/17/99 C:53  MC145
     H*                 REVAMP FUNC/OPT SELECTION W/NEW ROUTINE.
     H*                 FURTHER QUALIFY NON-PDS CLAIM CALLS.
     H*                 SET THE INITIAL VIEW BASED ON SYSTEM CONTROL
     H*                 FILE
     H*
     H*  05/10/99 C:53  MC145
     H*                 QUALIFIED CALL TO WCMC PGMS FOR LINK W/NON PDS
     H*                 QLAIM SYSTEM
     H*
     H*  04/22/99 C:31  Project: MC152
     H*                 V4: Bill Entry Re-Write
     H*                 1. Change logic the loads ACDPGM
     H*
     H*  04/13/99 C:53  Project: MC145
     H*                 STANDARDIZE T/M Activity
     H*
     H*  03/30/99 C:33 PROJECT: WC577                                 *
     H*          C:118 RESET VALID OPTION FLAG AFTER READS TO BE SURE *
     H*                CORRECT RRN'S ARE LOADED.                      *
     H*
     H*
     H*  03/17/99 C:56  Project: MC152
     H*                 Display Opt81=WCMS Claims when in serch mode
     H*  01/11/99 C:31  Project: MC178
     H*                 V4: Bill Entry re-write
     H*                 1. Change option processing based on group
     H*                    expired or claim denied.
     H*
     H*  01/10/99 C:53  PROJ: MC152
     H*                 ADD CALL TO PROVIDER BILLS IF INSTALLED
     H*
     H*  12/16/98 C:94  PROJ: MC152
     H*                 REMOVE ALL UPDATES TO THE FOLLOWING FILES:
     H*                 FAE1P, NAMBP AND REPLACE WITH CALL TO API
     H*                 AA001R WHICH WILL DO THE UPDATES
     H*                2. TURN *IN30 OFF IN KEYINZ
     H*
     H*  12/03/98 C:53  PROJ: MC098 14996
     H*                 REMOVE UPDATE TO CONTROL EOB # IN THE CONTROL
     H*                 FILE ON A DELETE ROUTINE
     H*
     H*  12/01/98 C:62  REQ/PROJ: MC098 15287
     H*                 USE EOINV# NOT EOBLFL TO CHECK IF INVOICED.
     H*
     H*  10/29/98 C:62  REQ/PROJ: MC098 14743
     H*                 1) IF SUB# IS PASSED IN THEN SUBSET ON IT.
     H*                 2) IF FND IS PASSED IN THEN SUBSET ON IT.
     H*
     H*  10/21/98 C:62 PROJECT: MC155
     H*                REFERENCE LOGICALS WITH EMP#/DIV WHEN SUBSET-
     H*                TING EMP/DIV.
     H*
     H* 10/06/98 C:94  PROJECT: 87MFX
     H*                CHANGED @@WCUP TO LOOK FOR A 'Y' INSTEAD OF
     H*                BLANK
     H*
     H*  09/21/98 C:94 PROJECT: 87MFX
     H*                UPDATE WORKER'S COMP FIELDS
     H*
     H*  09/15/98 C:94 PROJECT: MC151
     H*                ADDED SECURITY CHECK TO OPTION 82 AND 4
     H*
     H*  09/01/98 C:94 PROJECT: MC151
     H*                ADDED OPTION 82 - TRANSFER CASE AND OPTION 4
     H*                DELETE CASE
     H*
     H*  08/19/98 C:62 PROJECT: MC177
     H*                - VARIOUS CHANGES TO T/M.
     H*
     H*  08/11/98 C:62 PROJECT: MC155
     H*                1) USER PARMS WILL NOT UPDATE IF CHAIN FAILS.
     H*                2) WHEN USER COMES IN TO DISPLAY SINGLE RECORD
     H*                   MAKE SURE IT ASSIGNS A RRN TO SCDBRR.
     H*
     H*  07/20/98 C:62 PROJECT: MC177
     H*                CHECK SMUPL TO DETERMINE IF USER IS RESTRICTED
     H*                AND IF SO ONLY ALLOW THE USER TO SEE THE SUB-
     H*                SCRIBER, GROUP, EMPLOYER# AND/OR DIVISION THE
     H*                USER IS RESTRICTED TO.
     H*
     H*  06/23/98 C:61  PROJECT: 98MBD
     H*                 Chg call to F4110C.
     H*                 Pass zero RRN to access ww screen.
     H*
     H*  06/09/98 C:94  PROJECT: MC145
     H*                 ADD STATUS COLUMN TO ALL 4 DISPLAYS.
     H*
     H*  06/08/98 C:94  PROJECT: MC145
     H*                 ADD STATUS COLUMN TO DISPLAY.
     H*
     H*  04/09/98 C:47  PROJECT: 87MEA
     H*                 GIVE THIS PROGRAM THE ABILITY TO DISPLAY A
     H*                 SINGLE RECORD.
     H*
     H*  03/26/98 C:47  PROJECT: 87MCB
     H*                 CHANGED CALL FROM U0010R TO U0060R FOR PRE-CERT.
     H*
     H*  02/19/98 C:62  PROJECT: MC145
     H*                 1) DELETE OPT18 = EDI.
     H*                 2) CHANGE OPT19 TO OPT18 = EDI LOG.
     H*                 3) CHANGE OPT82 TO OPT19 = SEND SUBSQ 148.
     H*
     H*  02/04/98 C:62  PROJECT: MC145
     H*                 ** W0270R5 (OPT19) NEEDS MORE PARMS.
     H*
     H*  02/01/98 C:62  PROJECT: MC145
     H*                 1) IF SSFND IS SEARCHED OR ENTERED SSSUB# NE 0.
     H*                 2) RB CHECK APPLIED TO SUBSET FIELDS.
     H*                 3) USER PARM UPDATE UPON SELECTION.
     H*
     H*  01/28/98 C:33  PROJECT: MC145
     H*                -USE DIFFERENT LOGICALS FOR SUB OR FND SUBSET.
     H*                -ALLOW FOR DIFFERENT COMPANIES.
     H*
     H*  01/26/98 C:62  PROJECT: MC145
     H*                 1) NEW SEARCHES.
     H*                     SUBSCRIBERS - F0545R TO F0055R.
     H*                     GROUPS      - F0508R TO F0060R.
     H*                 2) FA703R - API USED TO CALL DARTS.
     H*                 3) 'MED SVCS INFO' TO 'CASE MGMT SVCS'
     H*                 4) FIXED SUBSET
     H*                 5) FIXED INQUIRY MODE FOR OPT9 AND 11
     H*                 6) LENGTHENED PROMPT FIELDS FOR LSN AND FSN
     H*                 7) 'ACC DATE' TO 'DATE OF INJURY'
     H*                 8) 'FYR' TO 'YR' OR 'YEAR'
     H*                 9) OPT11 HEADING TO 'CASE PROVIDERS'
     H*                10) REMOVED 'SELECTION' FROM HEADING + MENU
     H*                11) 'POSITION TO LAST' CHANGED FOR OTHER VIEWS.
     H*                12) ENABLED SECON FUNCTION LINE TO SEE F23.
     H*
     H*  12/18/97 C:53  PROJECT: MC145
     H*                 ADD 0 SUPPREESION/ MASK API
     H*
     H*  12/18/97 C:86  PROJECT: MC145
     H*                 IF SUBSETTING ON SUB#, TURN ON SEBSET MESSAGE.
     H*
     H*  10/09/97 C:31  Project: 87MCZ
     H*                 Additional 278 TX Plan Process
     H*                 1. Replace call to F0875R with call to W0270R5
     H*
     H* 10/02/97 C:59 PROJECT: 87MAZ
     H*               Add option 95 for prividers specialist
     H*
     H*
     H* 09/26/97 C:59 PROJECT: SUPPORT MC087/#8828
     H*               Calling incorrect program on darts
     H*               correct is F0270R NOT F0075R
     H* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     H*
     H* 03/04/97 C:56  87MAZ
     H*                CREATED
     H*
     H* 06/24/97 C:56  87MAZ
     H*             1. AMEND CALL TO DIAGNOSIS INFOMATION BECAUSE ADDED
     H*                PARMS
     H*             2. ADD OPTION 81 WCMS CLAIM MASTER MAINTENANCE
     H*             3. ADD OPTION 15 PRE-CERT
     H*
     H* 07/02/97 C:56  87MAZ
     H*             1. CHANGE OPT81 NAME TO CLAIMS MASTER
     H*             2. CAHNGE NAME PAST BY OPT81 TO CLAIMS MASTER MAINT.
     H*             3. REMOVE OPTION 20
     H*             4. ADD OPT18=EDI
     H*
     H* 07/07/97 C:56 PROJECT: 87MAZ
     H*             REMOVE OPTION 4=DELETE
     H*
     H* 07/16/97 C:32 PROJECT: 87MBN
     H*             1. ADD ENTRY PARMS FOR SUB/FND/FYR/CASE SELECTION
     H*             2. ADD WORK WITH EOMBS (OPTION 16)
     H*             3. ADD EDI LOG HISTORY (OPTION 19)
     H*
     H* 07/28/97 C:59 PROJECT: 87MBW
     H*             1. OPTION (94) CALL TO WORK WITH BWC TREATMENT PLAN
     H*
     H* 08/01/97 C:56 PROJECT: 87MAW
     H*             ADD ACCIDENT DATE TO ENTRY PARM LIST
     H*
     H*  08/21/97 C:31  Project: 87MAZ  CO# 31
     H*                 Add option 82 to Work with Medical Cases
     H*                 1. Add option 82.
     H*
     H*  08/25/97 C:31  Project: 87MAZ  CO# 48
     H*                 Remove case management fields
     H*                 (Compile only)
     H*
     H*  09/03/97 C:56  Project: 87MAZ CO# 31
     H*                 Make Opt82 operable
     H*
     H*  09/15/97 C:56  Project: MC087 #8086 support
     H*                 Send '1' for display mode for options in order
     H*                 to implement inquiry mode.
     H*****************************************************************
     FF0269F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     FFMCMP     IF   E             DISK
     F                                     RENAME(PCM01:PFRCDF)
     F*** BEGIN CHG *** 10/29/02 **************************************
     F**********FMCMLE  IF  E           K        DISK
     FFMCMLE    IF   E           K DISK    USROPN
     F*** END   CHG *** 10/29/02 **************************************
     F                                     INFDS(DBFDS)
     F                                     RENAME(PCM01:DBRCDF)
     F*** BEGIN CHG *** 10/29/02 **************************************
     F**********FMCMLB  IF  E           K        DISK
     FFMCMLB    IF   E           K DISK    USROPN
     F*** END   CHG *** 10/29/02 **************************************
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PCM01:DBRCDF2)
     F*** BEGIN CHG *** 10/29/02 **************************************
     F**********FMCML1  IF  E           K        DISK
     FFMCML1    IF   E           K DISK    USROPN
     F*** END   CHG *** 10/29/02 **************************************
     F                                     INFDS(DBFDS3)
     F                                     RENAME(PCM01:DBRCDF3)
     F*** BEGIN CHG *** 10/29/02 **************************************
     F**********FMCMLD  IF  E           K        DISK
     FFMCMLD    IF   E           K DISK    USROPN
     F*** END   CHG *** 10/29/02 **************************************
     F                                     INFDS(DBFDS4)
     F                                     RENAME(PCM01:DBRCDF4)
     F*** BEGIN ADD *** 10/06/06 **************************************
     FFMCML13   IF   E           K DISK
     F                                     INFDS(DBFDS5)
     F                                     RENAME(PCM01:DBRCDF5)
     F*** END   ADD *** 10/06/06 **************************************
     F*** BEGIN END *** 04/01/98 **************************************
     FFMCML3    IF   E           K DISK
     F                                     INFDS(DBFDSA)
     F                                     RENAME(PCM01:DBRCDFA)
     FFMCML5    IF   E           K DISK
     F                                     INFDS(DBFDSB)
     F                                     RENAME(PCM01:DBRCDFB)
     FFMCML6    IF   E           K DISK
     F                                     INFDS(DBFDSC)
     F                                     RENAME(PCM01:DBRCDFC)
     FFMCML7    IF   E           K DISK
     F                                     INFDS(DBFDSD)
     F                                     RENAME(PCM01:DBRCDFD)
     FFMCML8    IF   E           K DISK
     F                                     INFDS(DBFDSE)
     F                                     RENAME(PCM01:DBRCDFE)
     FFMCMLF    IF   E           K DISK
     F                                     INFDS(DBFDSF)
     F                                     RENAME(PCM01:DBRCDFF)
     F*** END   END *** 04/01/98 **************************************
     F*** BEGIN ADD *** 10/21/98 **************************************
     FFMCMLH    IF   E           K DISK
     F                                     INFDS(DBFDSK)
     F                                     RENAME(PCM01:DBRCDFK)
     FFMCMLI    IF   E           K DISK
     F                                     INFDS(DBFDSL)
     F                                     RENAME(PCM01:DBRCDFL)
     FFMCMLJ    IF   E           K DISK
     F                                     INFDS(DBFDSM)
     F                                     RENAME(PCM01:DBRCDFM)
     FFMCMLK    IF   E           K DISK
     F                                     INFDS(DBFDSN)
     F                                     RENAME(PCM01:DBRCDFN)
     F*** END   ADD *** 10/21/98 **************************************
     F*** BEGIN ADD *** 10/06/06 **************************************
     FFMCML12   IF   E           K DISK
     F                                     INFDS(DBFDSO)
     F                                     RENAME(PCM01:DBRCDFO)
     FFMCML11   IF   E           K DISK
     F                                     INFDS(DBFDSP)
     F                                     RENAME(PCM01:DBRCDFP)
     FFMCML10   IF   E           K DISK
     F                                     INFDS(DBFDSQ)
     F                                     RENAME(PCM01:DBRCDFQ)
     F*** END   ADD *** 10/06/06 **************************************
     F****FMCDL   IF  E           K        DISK
     FSMUPL     UF   E           K DISK
     FSMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FSMUPP     IF   E             DISK
     F                                     RENAME(PUP01:PUP03)
     F*
     FFMFNL1    IF   E           K DISK
     FFMSML     IF   E           K DISK
     F*** BEGIN CHG *** 10/06/06 **************************************
     F**********                                    KINFDS DBFDS5
     F                                     INFDS(DBFDS6)
     F*** END   CHG *** 10/06/06 **************************************
     FFMSMP     IF   E             DISK
     F                                     RENAME(PSM01:PSM02)
     FFMFNL     IF   E           K DISK
     F                                     RENAME(PFN01:PFN02)
     FFMFNP     IF   E             DISK
     F                                     RENAME(PFN01:PFN03)
     FFMEML     IF   E           K DISK
     FFMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PEM02)
     F*** BEGIN DEL *** 09/07/99 **************************************
     F*FAEOP   UF  E                    DISK                      A
     F**********                                    KINFDS DBFDSG
     F*** END   DEL *** 09/07/99 **************************************
     FFAEOL4    IF   E           K DISK
     F                                     INFDS(DBFDSH)
     F                                     RENAME(PEO01:PEO02)
     F*** BEGIN END *** 09/27/99 **************************************
     FFAEAL1    IF   E           K DISK
     F*** END   END *** 09/27/99 **************************************
     FFMC1L     UF   E           K DISK
     F*** BEGIN DEL *** 07/02/97 **************************************
     F*MCHL2**IF**E******     K        DISK
     F*** END DEL *** 07/02/97 ****************************************
     F*** BEGIN ADD *** 04/20/07 **************************************
     FFTGTP     IF   E             DISK
     FFTGTL     IF   E           K DISK
     F                                     RENAME(PGT01:PGT02)
     F*** END ADD ***** 04/20/07 **************************************
     F***** BEG ADD ***** 04/12/05 ***********************************
     FWMXRL     IF   E           K DISK    USROPN
     F***** END ADD ***** 04/12/05 ***********************************
      /COPY SCOPYWW01D
     D*
     D*** BEGIN CHG *** 10/06/06 **************************************
     D******************* HD      1   4 74               Column Headings
     D HD              S             74    DIM(5) CTDATA PERRCD(1)              Column Headings
     D*** END   CHG *** 10/06/06 **************************************
     D*** BEGIN CHG *** 12/14/11 **************************************
     D*** OF              S              1    DIM(31) CTDATA PERRCD(1)          Options, Function
     D*** OFD             S             35    DIM(31) ALT(OF)
     D OF              S              1    DIM(32) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(32) ALT(OF)
     D*** END   CHG *** 12/14/11 **************************************
     D*** BEGIN ADD *** 10/29/02 **************************************
     D GRP             S              3    DIM(999)                             GROUP RESTRICTIONS
     D*** END   ADD *** 10/29/02 **************************************
     D*
     D DATA            S             74    DIM(4)
     D DTA1            S              1    DIM(74)
     D DTA2            S              1    DIM(74)
     D DTA3            S              1    DIM(74)
     D DTA4            S              1    DIM(74)
     D*** BEGIN ADD *** 10/06/06 **************************************
     D DTA5            S              1    DIM(74)
     D*** END   ADD *** 10/06/06 **************************************
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*** BEGIN ADD *** 04/01/98 **************************************
     D*
     D DBFDSA          DS
     D  DBRRNA               397    400B 0
     D DBFDSB          DS
     D  DBRRNB               397    400B 0
     D DBFDSC          DS
     D  DBRRNC               397    400B 0
     D DBFDSD          DS
     D  DBRRND               397    400B 0
     D DBFDSE          DS
     D  DBRRNE               397    400B 0
     D DBFDSF          DS
     D  DBRRNF               397    400B 0
     D*** BEGIN DEL *** 09/07/99 **************************************
     D**********DBFDSG      DS
     D**********                          B 397 4000DBRRNG
     D*** END   DEL *** 09/07/99 **************************************
     D DBFDSH          DS
     D  DBRRNH               397    400B 0
     D*** END   ADD *** 04/01/98 **************************************
     D***  BEGIN ADD  ***  10/21/98  *********************************
     D DBFDSK          DS
     D  DBRRNK               397    400B 0
     D DBFDSL          DS
     D  DBRRNL               397    400B 0
     D DBFDSM          DS
     D  DBRRNM               397    400B 0
     D DBFDSN          DS
     D  DBRRNN               397    400B 0
     D***  END   ADD  ***  10/21/98  *********************************
     D***  BEGIN ADD  ***  10/06/06  *********************************
     D DBFDSO          DS
     D  DBRRNO               397    400B 0
     D DBFDSP          DS
     D  DBRRNP               397    400B 0
     D DBFDSQ          DS
     D  DBRRNQ               397    400B 0
     D***  END   ADD  ***  10/06/06  *********************************
     D*
     D***  BEGIN ADD  ***  01/28/98  *********************************
     D***  END   ADD  ***  01/28/98  *********************************
     D*** BEGIN ADD *** 02/01/98 **************************************
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D*** END   ADD *** 02/01/98 **************************************
     D*
     D*** BEG ADD *** 12/16/98 ***************************************
     D*  WRITE/UPDATE DATA STRUCTURE.
     D                 DS
     D  KKWRUP                 1      7
     D  XXFAEO                 1      1
     D  XXFAE1                 2      2
     D  XXFAHB                 3      3
     D  XXNAPB                 4      4
     D  XXNAMB                 5      5
     D  XXNATR                 6      6
     D  XXFAEA                 7      7
     D*** END ADD *** 12/16/98 ***************************************
     D*
     D*** BEGIN ADD *** 10/29/02 **************************************
     D QRYSLT          DS          5000
     D*** END   ADD *** 10/29/02 **************************************
     D*
     D* /COPY required constants
     D*
     D*** BEGIN CHG *** 01/26/98 **************************************
     D**********    'Medical Cases - Se-  C         OPTNM
     D**********    'lection'
     D OPTNM           C                   CONST('Medical Cases')
     D*** END   CHG *** 01/26/98 **************************************
     D*** BEGIN ADD *** 07/02/97 **************************************
     D NAME            C                   CONST('Claims Master Main-
     D                                     tenance')
     D NAME2           C                   CONST('   EDI/NOI Mainten-
     D                                     ance')
     D*** END ADD *** 07/02/97 ****************************************
     D ACDPG1          C                   CONST('F0260R')
     D ACDPG2          C                   CONST('F0268R')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D COMAPI          C                   CONST('FA013R')
     D USPAPI          C                   CONST('F0006R')
     D*** BEGIN ADD *** 10/29/02 **************************************
     D QRYPGM          C                   CONST('F0269C2')
     D*** END   ADD *** 10/29/02 **************************************
     D*** BEG ADD *** 12/18/97 ************************
     D CHGNUM          C                   CONST('SA020R')
     D*** END ADD *** 12/18/97 ************************
     D SF1PAG          C                   CONST(11)
     D INFOMX          C                   CONST(1)
     D*** BEGIN CHG *** 10/06/06 **************************************
     D************* 4                     C         VIEWMX
     D VIEWMX          C                   CONST(5)
     D*** END   CHG *** 10/06/06 **************************************
     D CLRFKY          C                   CONST('                    -
     D                                            NNN')
     D*** BEGIN ADD *** 10/29/02 **************************************
     D CO#             C                   CONST('(CMCO# *EQ')
     D CO#2            C                   CONST(') *AND (')
     D SUB#            C                   CONST('(CMSUB# *EQ')
     D FND             C                   CONST('*AND (CMFND *EQ -
     D                                     %VALUES(')
     D OVRDB1          C                   CONST('OVRDBF FILE(')
     D OVRDB2          C                   CONST(') SHARE(*YES)')
     D** **         'SEQONLY(*NO)'
     D*** END   ADD *** 10/29/02 **************************************
     D*
     D* Program specific constants
     D*
     D                 DS
     D  HLACDT                 1      8
     D  H1ACDT                 1      2
     D  H2ACDT                 3      4
     D  H3ACDT                 5      8

     I*
      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      /copy sPrSecure
      ***** End Add *** 06/09/20 *******************************************

     C     *LIKE         DEFINE    CMLSN         KKLSN
     C     *LIKE         DEFINE    CMFSN         KKFSN
     C     *LIKE         DEFINE    CMSS#         KKSS#
     C     *LIKE         DEFINE    CMACDT        KKACDT
     C     *LIKE         DEFINE    CMSUB#        KKSUB#
     C     *LIKE         DEFINE    CMFND         KKFND
     C     *LIKE         DEFINE    CMFYR         KKFYR
     C     *LIKE         DEFINE    CMCASE        KKCASE
     C     *LIKE         DEFINE    CMSCL#        KKSCL#
     C     *LIKE         DEFINE    CMSS#         XXSS#
     C     *LIKE         DEFINE    UPUSR         KKUSR
     C     *LIKE         DEFINE    CMSUB#        XXSUB#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    CMACDT        HDACDT
     C     *LIKE         DEFINE    EMEMP#        QQEMP#
     C     *LIKE         DEFINE    EMDIV         QQDIV
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    EOEOB#        HDEOB#
     C*** BEGIN DEL *** 07/02/97 **************************************
     C********** *LIKE     DEFN CHEFDT    XXEFDT
     C*** END DEL *** 07/02/97 ****************************************
     C*** BEGIN ADD *** 01/11/99 **************************************
     C     *LIKE         DEFINE    FNEXDT        XXDATE
     C*** END ADD *** 01/11/99 ****************************************
     C*** BEGIN ADD *** 10/06/06 **************************************
     C     *LIKE         DEFINE    CMTSIT        KKTSIT
     C*** END   ADD *** 10/06/06 **************************************
     C*** BEGIN ADD *** 10/29/02 **************************************
     C     *LIKE         DEFINE    UPFND         #2FND
     C     *LIKE         DEFINE    UPEMP#        #2EMP#
     C     *LIKE         DEFINE    UPSUB#        #2SUB#
     C     *LIKE         DEFINE    @@CALL        SVCALL
     C     *LIKE         DEFINE    *IN09         SVIN09
     C     *LIKE         DEFINE    UPMRSU        ##MRSU
     C     *LIKE         DEFINE    UPMRGR        ##MRGR
     C     *LIKE         DEFINE    UPMREM        ##MREM
     C*** END   ADD *** 10/29/02 **************************************
     C*** BEGIN ADD *** 04/20/07 **************************************
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C     *LIKE         DEFINE    GTKEY         KKTKEY
     C*** END ADD ***** 04/20/07 **************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM                    @@SUB#            5 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@FYR             3 0
     C                   PARM                    @@CASE            7 0
     C                   PARM                    @@SS#             9
     C                   PARM                    @@ACDT            8 0
     C                   PARM                    @@RTCD            1
     C*
     C     *like         define    @@RtCd        xxRtCd
     C                   clear                   xxRtCd
     C                   if        %parms >= 8
     C                   eval      xxRtCd = @@RtCd
     C                   endif
     C*
     C*** BEGIN ADD *** 04/20/07 **************************************
     C     SCHPL         PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM                    ##TBID
     C*** END ADD ***** 04/20/07 **************************************
     C*
     C     @@CALL        IFEQ      'W'
     C*
     C                   MOVEL     '1'           @@CALL
     C                   MOVEL     'W'           XXCALL            1
     C*
     C                   ENDIF
     C*
     C*** BEGIN DEL *** 10/13/97 ********************
     C*********  DATCHK    PLIST
     C*********            PARM           SRDTI   80
     C*********            PARM *BLANKS   SRFFMT  8
     C*********            PARM *BLANKS   SRTFMT  8
     C*********            PARM           SRDTO  10
     C*********            PARM           SRERR   1
     C*********
     C*********  DTMDYY    PLIST
     C*********            PARM           SRDTI
     C*********            PARM '*CYMD'   SRFFMT
     C*********            PARM *BLANKS   SRTFMT
     C*********            PARM           SRDTO
     C*********            PARM           SRERR
     C*** END   ADD *** 07/16/97 **************************************
     C*** END   DEL *** 10/13/97 ********************
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM                    ##SS#             9
     C                   PARM                    #@ACDT            8 0
     C*
     C     ACDPL2        PLIST
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      *BLANKS       ##SS#             9
     C                   PARM      *ZEROS        #@ACDT            8 0
     C**********           PARM           ##CO#   30
     C*
     C     DRTSPL        PLIST
     C                   PARM      *BLANKS       #@RTCD            1
     C                   PARM      ##WCIN        #@WCIN            1
     C                   PARM      ##WCDK        #@WDCK            1
     C                   PARM                    #@CO#             3
     C                   PARM                    #@SUB#            5
     C                   PARM                    #@FND             3
     C                   PARM                    #@FYR             3
     C                   PARM                    #@CASE            7
     C                   PARM      *BLANKS       #@KEY            40
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*
     C*** BEGIN ADD ** 08/21/97 ************************************************
     C*
     C     LOGFPL        PLIST
     C                   PARM                    QQCO#             3            COMPANY
     C                   PARM                    QQFNDX            3            FUND ID
     C                   PARM                    QQFYR             3            FUND YR
     C                   PARM                    QQCASE            7            CASE/CLAIM
     C                   PARM                    QQACTF            1            ACTION FLAG
     C                   PARM                    QQMGID            7            MSG ID
     C                   PARM                    QQRTCD            1
     C*** END ADD ** 08/21/97 **************************************************
     C*** BEG ADD *** 12/18/97 ************************
     C*
     C     CHGPL         PLIST
     C                   PARM                    QQNUM            80
     C                   PARM                    QQMASK           10
     C*** END ADD *** 12/18/97 ************************
     C*** BEGIN ADD *** 01/11/99 **************************************
     C     FA720P        PLIST
     C                   PARM                    @@MGIS            1
     C*** BEGIN CHG *** 12/17/99 **************************************
     C**********           PARM           CMCO#
     C**********           PARM           CMSUB#
     C**********           PARM           CMFND
     C                   PARM                    ##CO#
     C                   PARM                    SCSUB#
     C                   PARM                    SCFND
     C     *INKL         PARM      '0'           ##KL              1
     C*** END   CHG *** 12/17/99 **************************************
     C*** END ADD *** 01/11/99 ****************************************
     C*** BEGIN ADD *** 10/29/02 **************************************
     C*
     C     RSTPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM      Q1USER        ##USR            10
     C                   PARM      'M'           ##APPL            1
     C                   PARM      *OFF          ##RCDS            1
     C*
     C*
     C     RSTPLF        PLIST
     C                   PARM                    ##CO#
     C                   PARM      Q1USER        ##USR
     C                   PARM      'M'           ##APPL
     C                   PARM      *OFF          ##RTCD
     C                   PARM                    ##AUTH
     C                   PARM      FNFND         ##FND
     C                   PARM      *ZERO         ##EMP#
     C                   PARM      FNSUB#        ##SUB#
     C*
     C     RSTPLS        PLIST
     C                   PARM                    ##CO#
     C                   PARM      Q1USER        ##USR
     C                   PARM      'M'           ##APPL
     C                   PARM      *OFF          ##RTCD
     C                   PARM                    ##AUTH
     C                   PARM      *ZERO         ##FND
     C                   PARM      *ZERO         ##EMP#            9 0
     C                   PARM      SMSUB#        ##SUB#
     C*
     C     QRYPL         PLIST
     C                   PARM                    QRYSLT
     C*** END   ADD *** 10/29/02 **************************************
     C*** BEG ADD ****** 05/25/07 ********************************************
     C*** Parameter list for claims security API
     C     CUSPL         PLIST
     C                   PARM                    #@CO#
     C                   PARM                    #@FND
     C                   PARM                    #@FYR
     C                   PARM                    #@CASE
     C                   PARM      Q1USER        ##USR
     C                   PARM                    #@RTCD
     C*** END ADD ****** 05/25/07 ********************************************
     C*
     C     KEY1          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKLSN
     C                   KFLD                    KKFSN
     C                   KFLD                    KKSS#
     C                   KFLD                    KKACDT
     C*** BEGIN ADD *** 02/09/00 **************************************
     C                   KFLD                    KKCASE
     C*** END ADD *** 02/09/00 ****************************************
     C*
     C     KEY2          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSS#
     C                   KFLD                    KKACDT
     C*** BEGIN ADD *** 02/09/00 **************************************
     C                   KFLD                    KKCASE
     C*** END ADD *** 02/09/00 ****************************************
     C*
     C     KEY3          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     KEY4          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSCL#
     C***  BEG ADD  ***  01/13/99  *********************************
     C                   KFLD                    KKSS#
     C***  END ADD  ***  01/13/99  *********************************
     C***  BEGIN ADD  ***  10/06/06  *********************************
     C     KEYA          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTSIT
     C                   KFLD                    KKLSN
     C                   KFLD                    KKFSN
     C                   KFLD                    KKSS#
     C                   KFLD                    KKACDT
     C***  END   ADD  ***  10/06/06  *********************************
     C*
     C***  BEGIN ADD  ***  01/28/98  *********************************
     C     KEY5          KLIST
     C                   KFLD                    ##CO#
     C***  BEGIN ADD  ***  04/01/98  *********************************
     C*
     C* KEYS USED WHEN SSSUB# NE ZERO
     C*
     C     KEY6          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSSUB#
     C                   KFLD                    KKLSN
     C                   KFLD                    KKFSN
     C                   KFLD                    KKSS#
     C                   KFLD                    KKACDT
     C*** BEGIN ADD *** 02/09/00 **************************************
     C                   KFLD                    KKCASE
     C*** END ADD *** 02/09/00 ****************************************
     C*
     C     KEY7          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSSUB#
     C                   KFLD                    KKSS#
     C                   KFLD                    KKACDT
     C*** BEGIN ADD *** 02/09/00 **************************************
     C                   KFLD                    KKCASE
     C*** END ADD *** 02/09/00 ****************************************
     C*
     C     KEY8          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     KEY9          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSSUB#
     C                   KFLD                    KKSCL#
     C***  BEGIN ADD  ***  10/06/06  *********************************
     C     KEYB          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSSUB#
     C                   KFLD                    KKTSIT
     C                   KFLD                    KKLSN
     C                   KFLD                    KKFSN
     C                   KFLD                    KKSS#
     C                   KFLD                    KKACDT
     C***  END   ADD  ***  10/06/06  *********************************
     C*
     C     KEY10         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSSUB#
     C*
     C* KEYS USED WHEN SSSUB# AND SSFND NE ZERO.
     C*
     C     KEY11         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSSUB#
     C                   KFLD                    SSFND
     C                   KFLD                    KKLSN
     C                   KFLD                    KKFSN
     C                   KFLD                    KKSS#
     C                   KFLD                    KKACDT
     C*** BEGIN ADD *** 02/09/00 **************************************
     C                   KFLD                    KKCASE
     C*** END ADD *** 02/09/00 ****************************************
     C*
     C     KEY12         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSSUB#
     C                   KFLD                    SSFND
     C                   KFLD                    KKSS#
     C                   KFLD                    KKACDT
     C*** BEGIN ADD *** 02/09/00 **************************************
     C                   KFLD                    KKCASE
     C*** END ADD *** 02/09/00 ****************************************
     C*
     C     KEY13         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSSUB#
     C                   KFLD                    SSFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     KEY14         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSSUB#
     C                   KFLD                    SSFND
     C                   KFLD                    KKSCL#
     C***  BEGIN ADD  ***  10/06/06  *********************************
     C     KEYC          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSSUB#
     C                   KFLD                    SSFND
     C                   KFLD                    KKTSIT
     C                   KFLD                    KKLSN
     C                   KFLD                    KKFSN
     C                   KFLD                    KKSS#
     C                   KFLD                    KKACDT
     C***  END   ADD  ***  10/06/06  *********************************
     C*
     C     KEY15         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSSUB#
     C                   KFLD                    SSFND
     C***  END   ADD  ***  04/01/98  *********************************
     C***  BEGIN ADD  ***  10/21/98  *********************************
     C*
     C* KEYS USED WHEN SSSUB# AND SSFND NE ZERO AND SSEMP# NE ZERO
     C*
     C     KEY16         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSSUB#
     C                   KFLD                    SSFND
     C                   KFLD                    SSEMP#
     C                   KFLD                    SSDIV
     C                   KFLD                    KKLSN
     C                   KFLD                    KKFSN
     C                   KFLD                    KKSS#
     C                   KFLD                    KKACDT
     C*** BEGIN ADD *** 02/09/00 **************************************
     C                   KFLD                    KKCASE
     C*** END ADD *** 02/09/00 ****************************************
     C*
     C     KEY17         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSSUB#
     C                   KFLD                    SSFND
     C                   KFLD                    SSEMP#
     C                   KFLD                    SSDIV
     C                   KFLD                    KKSS#
     C                   KFLD                    KKACDT
     C*** BEGIN ADD *** 02/09/00 **************************************
     C                   KFLD                    KKCASE
     C*** END ADD *** 02/09/00 ****************************************
     C*
     C     KEY18         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSSUB#
     C                   KFLD                    SSFND
     C                   KFLD                    SSEMP#
     C                   KFLD                    SSDIV
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     KEY19         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSSUB#
     C                   KFLD                    SSFND
     C                   KFLD                    SSEMP#
     C                   KFLD                    SSDIV
     C                   KFLD                    KKSCL#
     C***  BEGIN ADD  ***  10/06/06  *********************************
     C     KEYD          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSSUB#
     C                   KFLD                    SSFND
     C                   KFLD                    SSEMP#
     C                   KFLD                    SSDIV
     C                   KFLD                    KKTSIT
     C                   KFLD                    KKLSN
     C                   KFLD                    KKFSN
     C                   KFLD                    KKSS#
     C                   KFLD                    KKACDT
     C***  END   ADD  ***  10/06/06  *********************************
     C*
     C     KEY20         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSSUB#
     C                   KFLD                    SSFND
     C                   KFLD                    SSEMP#
     C                   KFLD                    SSDIV
     C***  END   ADD  ***  10/21/98  *********************************
     C*
     C***  BEGIN ADD  ***  05/16/00  *********************************
     C     KEY20A        KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSSUB#
     C                   KFLD                    SSFND
     C                   KFLD                    SSEMP#
     C***  END   ADD  ***  05/16/00  *********************************
     C*
     C*
     C*** KEY LIST FOR CLAIM DEMOGRAPHICS
     C*
     C***        CDKEY     KLIST
     C***                  KFLD           XXSS#
     C*
     C*** KEY LIST FOR USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    KKUSR
     C*
     C*** KEY LIST FOR FUND/GRP MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C*
     C*** KEY LIST FOR FUND/GRP  MASTER FILE
     C*
     C     FNKEY1        KLIST
     C                   KFLD                    ##CO#
     C*** BEGIN CHG *** 01/26/98 **************************************
     C**********           KFLD           ##SUB#
     C**********           KFLD           SSFND
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*** END   CHG *** 01/26/98 **************************************
     C*
     C     FNKEY2        KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*** BEGIN ADD *** 10/29/02 **************************************
     C     FNKY3         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXSUB#
     C*** END   ADD *** 10/29/02 **************************************
     C*
     C*** KEY LIST FOR SUBSCRIBER MASTER FILE
     C*
     C     SMKEY         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSSUB#
     C*
     C     SMKEY1        KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXSUB#
     C*
     C*** KEY LIST FOR EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    ##CO#
     C*** BEGIN CHG *** 10/29/02 **************************************
     C**********           KFLD           ##SUB#
     C**********           KFLD           ##FND
     C                   KFLD                    SSSUB#
     C                   KFLD                    SSFND
     C*** END   CHG *** 10/29/02 **************************************
     C                   KFLD                    SSEMP#
     C                   KFLD                    SSDIV
     C*
     C     EMKEY1        KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     CMKEY         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*** BEGIN DEL *** 07/02/97 **************************************
     C**********
     C*** KEY LIST FOR CLAIM STATUS HISTORY FILE
     C**********
     C********** CHKEY     KLIST
     C**********           KFLD           ##CO#
     C**********           KFLD           XXSUB#
     C**********           KFLD           XXFND
     C**********           KFLD           XXFYR
     C**********           KFLD           XXCASE
     C**********           KFLD           XXEFDT
     C**********
     C********** CHKEY1    KLIST
     C**********           KFLD           ##CO#
     C**********           KFLD           XXSUB#
     C**********           KFLD           XXFND
     C**********           KFLD           XXFYR
     C**********           KFLD           XXCASE
     C*** END DEL *** 07/02/97 ****************************************
     C*** BEGIN ADD *** 09/27/99 **************************************
     C     EAKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C                   KFLD                    EORCN
     C*** END   ADD *** 09/27/99 **************************************
     C*
     C     EOKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     EOKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*** BEGIN ADD *** 10/29/02 **************************************
      ***** BEG ADD ***** 04/12/05 ***********************************
     C     XRKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    Q1USER
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
      ***** END ADD ***** 04/12/05 ***********************************
     C*** BEGIN ADD *** 04/20/07 **************************************
     C*** Key list for WCMC generic table file
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKTKEY
     C*
     C*** END ADD ***** 04/20/07 **************************************
     C*================================================================
     C* INITIALIZE
     C*================================================================
     C*
     C                   MOVEL(P)  Q1USER        SCUSER
     C*
     C*** END   ADD *** 10/29/02 **************************************
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* ASSRRN - ASSIGN VALUE TO SCREEN RRN.
     C*================================================================
     C     ASSRRN        BEGSR
     C*
     C                   SELECT
      *
      *** BEGIN ADD *** 04/09/98 **************************************
     C     *IN20         WHENEQ    *ON
     C                   Z-ADD     DBRRN3        SCDBRR
      *** END   ADD *** 04/09/98 **************************************
     C*** BEGIN ADD *** 10/21/98 **************************************
     C*
     C     SSEMP#        WHENNE    *ZEROS
     C*
     C   21              Z-ADD     DBRRNK        SCDBRR
     C   22              Z-ADD     DBRRNL        SCDBRR
     C   23              Z-ADD     DBRRNM        SCDBRR
     C   24              Z-ADD     DBRRNN        SCDBRR
     C*** BEGIN ADD *** 10/06/06 **************************************
     C   25              Z-ADD     DBRRNQ        SCDBRR
     C*** END   ADD *** 10/06/06 **************************************
     C*** END   ADD *** 10/21/98 **************************************
     C*
     C     SSFND         WHENNE    *ZEROS
     C*
     C   21              Z-ADD     DBRRND        SCDBRR
     C   22              Z-ADD     DBRRNE        SCDBRR
     C   23              Z-ADD     DBRRN3        SCDBRR
     C   24              Z-ADD     DBRRNF        SCDBRR
     C*** BEGIN ADD *** 10/06/06 **************************************
     C   25              Z-ADD     DBRRNP        SCDBRR
     C*** END   ADD *** 10/06/06 **************************************
     C*
     C     SSSUB#        WHENNE    *ZEROS
     C*
     C   21              Z-ADD     DBRRNA        SCDBRR
     C   22              Z-ADD     DBRRNB        SCDBRR
     C   23              Z-ADD     DBRRN3        SCDBRR
     C   24              Z-ADD     DBRRNC        SCDBRR
     C*** BEGIN ADD *** 10/06/06 **************************************
     C   25              Z-ADD     DBRRNO        SCDBRR
     C*** END   ADD *** 10/06/06 **************************************
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C     SSFND         IFNE      *ZEROS
     C********** BEGIN ADD ******************* 12/18/97 ***************
     C     SSSUB#        ORNE      *ZEROS
     C********** END   ADD ******************* 12/18/97 ***************
     C     SSEMP#        ORNE      *ZEROS
     C     SSDIV         ORNE      *ZEROS
     C*** BEGIN ADD *** 04/20/07 **************************************
     C     SSSTAF        ORNE      *BLANK
     C     SSLTFL        ORNE      *BLANK
     C     SSSTAT        ORNE      *BLANK
     C*** END ADD ***** 04/20/07 **************************************
     C                   MOVE      *ON           *IN11
     C                   ENDIF
     C                   ENDSR
     C*** BEGIN DEL *** 09/07/99 **************************************
     C*================================================================
     C* DELEOB - Deletes record
     C*================================================================
     C********** DELEOB    BEGSR
     C*
     C**********           DELETPEO01
     C*
     C**********           ENDSR
     C*** END   DEL *** 09/07/99 **************************************
     C*** BEG DEL *** 12/16/98 ***************************************
     C***********
     C***** NOTE *****
     C*  THESE SUBROUTINES ARE NOW FOUND IN API AA001R.
     C***********
     C*================================================================
     C**DELEB1*-*Deletes record from EOMB auxiliary file FAE1P.
     C*================================================================
     C*================================================================
     C**DELMBT*-*Deletes record from Medical Bill Trans. File NAMBP.
     C*================================================================
     C*** END DEL *** 12/16/98 ***************************************
     C*================================================================
     C* DELUPD - Deletes records from FAEOP, FAE1P and NAMBP.
     C*          Updates FMC1P to reset EOB# to original value.
     C*================================================================
     C     DELUPD        BEGSR
     C*
     C* Deletes record from EOMB file (FAEOP).
     C*
     C*** BEGIN DEL *** 09/07/99 **************************************
     C**********           EXSR DELEOB
     C*** END   DEL *** 09/07/99 **************************************
     C*** BEG ADD *** 12/16/98 ***************************************
     C*** BEGIN ADD *** 09/07/99 **************************************
     C*
     C                   MOVEL     *BLANKS       KKWRUP
     C                   Z-ADD     *ZEROS        @@RCN
     C*
     C     ##PRVB        IFEQ      'Y'
     C*
     C                   MOVE      'D'           XXFAEO
     C*** END   ADD *** 09/07/99 **************************************
     C                   MOVE      'D'           XXFAEA
     C*** BEGIN ADD *** 09/07/99 **************************************
     C                   ELSE
     C*
     C                   MOVE      'D'           XXFAEO
     C*** END   ADD *** 09/07/99 **************************************
     C                   MOVE      'D'           XXFAE1
     C                   MOVE      'D'           XXNAMB
     C*** BEGIN ADD *** 09/07/99 **************************************
     C                   ENDIF
     C*** END   ADD *** 09/07/99 **************************************
     C*
     C                   CALL      'AA001R'
     C                   PARM      SCDBRR        @@RRN             9 0          RRN OF CASE
     C                   PARM                    @@EOB#            9 0          EOB #
     C                   PARM                    @@RCN             3 0          RECON #
     C                   PARM      Q1PGM         @@PGM            10            NAME OF PGM CALLING
     C                   PARM                    @@BAMT            9 2          BRS AMOUNT
     C                   PARM                    @@NAMT            9 2          NWS AMOUNT
     C                   PARM                    @@BHRF            1            BRS INV H/R FLAG
     C                   PARM                    @@BBLF            1            BRS INV PROC FLAG
     C                   PARM                    @@BINV            9 0          BRS INVOICE #
     C                   PARM                    @@BCAM            9 2          BRS CALC AMOUNT
     C                   PARM                    @@NHRF            1            NWS INV H/R FLAG
     C                   PARM                    @@NBLF            1            NWS INV PROC FLAG
     C                   PARM                    @@NINV            9 0          NWS INVOICE #
     C                   PARM                    @@SYS             1            TYPE OF SERVICE
     C                   PARM      KKWRUP        @@WRUP            7            WRITE/UPDATE FLAG
     C                   PARM                    ##TMBA            9 2          TIME/MAT BEFORE
     C                   PARM                    ##TMAA            9 2          TIME/MAT AFTER
     C*** END ADD *** 12/16/98 ***************************************
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
      ***** BEG ADD ***** 04/12/05 ***********************************
     C                   MOVE      *OFF          *IN34
     C     ##MCIF        IFEQ      'Y'
     C     XXCLT#        ANDEQ     '114'
     C*
     C     XRKY          CHAIN     PXR01                              60
     C     *IN60         IFEQ      *OFF
     C     XRSTAT        ANDEQ     'A'
     C     SCSEL         ANDNE     ' '
     C                   MOVE      *ON           *IN34
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C                   ENDIF                                                  ##MCIF = 'Y'
      ***** END ADD ***** 04/12/05 ***********************************
     C*** BEGIN ADD *** 04/22/99 **************************************
     C*          Count number of lines with selection code
     C*          ------------------------------------------------------
     C                   ADD       1             XXTS1             7 0
     C*** END ADD *** 04/22/99 ****************************************
     C*
     C***  BEGIN ADD  ***  01/28/98  *********************************
     C                   MOVE      'Y'           VLDOPT            1
     C***  END   ADD  ***  01/28/98  *********************************
     C*** BEGIN ADD *** 01/11/99 **************************************
     C*
     C*          If group expired or claim is denied: set on 19 so
     C*          that selected options will be display only.
     C*          ------------------------------------------------------
     C                   SETOFF                                       19
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C                   Z-ADD     SCSUB#        KKSUB#
     C                   Z-ADD     SCFND         KKFND
     C     FNKEY1        CHAIN     PFN02                              99
     C     *IN99         IFEQ      *ON
     C     FNEXDT        ORLE      XXDATE
     C     FNEXDT        ANDNE     0
     C                   SETON                                        19
     C                   MOVE      '2'           @@MGIS            1
     C                   ENDIF                                                  *IN99 = On
     C*
     C*          If claim is denied: check users security level and
     C*          type to determine if options will be display only.
     C*          ------------------------------------------------------
     C     SCDNY         IFEQ      'Y'
     C     *IN19         ANDEQ     *OFF
     C     UPKEY         CHAIN(N)  PUP01                              99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             UPLVL
     C                   MOVE      *BLANK        UPTYPE
     C                   ENDIF                                                  *IN99 = On
     C*
     C                   CALL      'FA715R'
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM      ##CO#         @@CO#             3 0
     C                   PARM      SCSUB#        @@SUB#            5 0
     C                   PARM      SCFND         @@FND             3 0
     C                   PARM                    @@PVMC            1 0          Init Med
     C                   PARM                    @@PVPR            1 0          Init Prov
     C                   PARM                    @@WCUP            1            Allw Upd WCMS Clms
     C                   PARM                    @@CLDL            3 0          Clm Dlt Lvl
     C                   PARM                    @@CLDT            1            Clm Dlt Typ
     C                   PARM                    @@CLTL            3 0          Clm Xfer Lvl
     C                   PARM                    @@CLTT            1            Clm Xfer Typ
     C                   PARM                    @@SDCL            3 0          Settle Dt Lvl
     C                   PARM                    @@SDCT            1            Settle Dt Typ
     C                   PARM                    @@BSCL            3 0          BWC Sts Lvl
     C                   PARM                    @@BSCT            1            BWC Sts Typ
     C                   PARM                    @@DCDL            3 0          Del Diag Cd Lvl
     C                   PARM                    @@DCDT            1            Del Diag Cd Typ
     C                   PARM                    @@DCCL            3 0          Chg Diag St Lvl
     C                   PARM                    @@DCCT            1            Chg Diag St Typ
     C                   PARM                    @@C#CL            3 0          Clt Case Lvl
     C                   PARM                    @@C#CT            1            Clt Case Typ
     C                   PARM                    @@FIMR            5            RMCD: From IMR
     C                   PARM                    @@TIMR            5            RMCD: To IMR
     C                   PARM                    @@FRN             5            RMCD: From RN
     C                   PARM                    @@TRN             5            RMCD: To RN
     C                   PARM                    @@CS#L            3 0          St Clm#  Lvl
     C                   PARM                    @@CS#T            1            St Clm#  Typ
     C                   PARM                    @@DNSL            3 0          Ovr DNYD Lvl
     C                   PARM                    @@DNST            1            Ovr DNYD Typ
     C                   PARM                    @@NERC            5            RMCD: New Emplr
     C                   PARM                    @@MEUS           10            New Employer
     C                   PARM                    @@NWPA            5            RMCD: ADD PC
     C                   PARM                    @@CPAA            5            RMCD: 'A' OR 'N'
     C                   PARM                    @@CPAD            5            RMCD: 'D'
     C                   PARM                    @@PLCL            3 0          LIAB CHG LVL
     C                   PARM                    @@PLCT            1            LIAB CHG TYP
     C*
     C                   CALL      'SA006R'
     C                   PARM      UPLVL         @@ULVL            3 0          USER LEVEL
     C                   PARM      UPTYPE        @@UTYP            1            USER TYPE
     C                   PARM      @@DNSL        @@FLVL            3 0          DNYD LEVEL
     C                   PARM      @@DNST        @@FTYP            1            DNYD TYPE
     C                   PARM                    ##RTCD            1            RETURN CODE
     C*
     C     ##RTCD        IFEQ      'F'
     C                   MOVE      '3'           @@MGIS            1
     C                   SETON                                        19
     C                   ENDIF                                                  RTCD = 'F'
     C                   ENDIF                                                  DNY = 'Y'
     C*
     C*          If IN19 and opt 2 (change): switch to opt 5 (display)
     C*          ------------------------------------------------------
     C     *IN19         IFEQ      *ON
     C     SCSEL         ANDEQ     '2'
     C                   CALL      'FA720R'      FA720P
     C*** BEGIN ADD *** 12/17/99 **************************************
     C     *INKL         IFEQ      *ON
     C*
     C                   SETON                                        90
     C*
     C                   ELSE
     C*** END   ADD *** 12/17/99 **************************************
     C                   MOVEL(P)  '5'           SCSEL
     C*** BEGIN ADD *** 12/17/99 **************************************
     C                   ENDIF
     C*** END   ADD *** 12/17/99 **************************************
     C                   ENDIF                                                  *IN19 = On
     C*** END ADD *** 01/11/99 ****************************************
     C*** BEGIN ADD *** 10/29/02 **************************************
     C                   SELECT
     C*
     C*  Check for user authorization to OPT 2 = change.
     C*
     C     SCSEL         WHENEQ    '2'
     C     SCAUTH        ANDEQ     'I'
     C     ##RCDS        ANDEQ     *ON
     C     *IN55         ANDEQ     *ON
     C                   SETON                                        75  90
     C*
     C     SCAUTH        WHENEQ    'I'
     C     *IN55         ANDEQ     *ON
     C*
     C     SCSEL         IFEQ      '4'
     C     SCSEL         OREQ      '20'
     C     SCSEL         OREQ      '82'
     C                   SETON                                        76  90
     C                   ENDIF
     C*
     C     SCSEL         IFEQ      '5'
     C                   MOVEL     ACDPG1        ACDPGM
     C                   ENDIF
     C*
     C                   OTHER
     C*** END   ADD *** 10/29/02 **************************************
     C*
     C                   SELECT
     C*
     C     SCSEL         WHENEQ    '2'
     C     SCSEL         OREQ      '5'
     C*
     C                   MOVEL(P)  ACDPG1        ACDPGM           10
     C*
     C*** BEG ADD *** 09/01/98 **************************************
     C*
     C     SCSEL         WHENEQ    '4'
     C                   MOVEL(P)  '99'          SCSEL
     C*
     C*** END ADD *** 09/01/98 **************************************
     C                   ENDSL
     C*** BEGIN ADD *** 10/29/02 **************************************
     C                   ENDSL
     C*** END   ADD *** 10/29/02 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C* PROCESS SUBSET SEARCHES
     C*
     C                   EXSR      OTHFKS
     C*
     C     *IN90         CABEQ     *ON           CK2END
     C*
     C     SSSUB#        IFNE      *ZEROS
     C     SMKEY         CHAIN     PSM01                              99
     C     *IN99         IFEQ      '1'
     C                   SETON                                        9071
     C     *IN90         CABEQ     '1'           CK2END
     C                   ENDIF
     C                   ENDIF
     C*
     C*** BEGIN DLT *** 05/21/08 **************************************
     C*** BEGIN ADD *** 11/08/07 **************************************
     C********** SSSUB# ** IFEQ *ZEROS
     C******************** SETON                     9072
     C********** *IN90 *** CABEQ'1'       CK2END
     C******************** ENDIF
     C*** END   ADD *** 11/08/07 **************************************
     C*** END DLT ***** 05/21/08 **************************************
     C*
     C     SSFND         IFNE      *ZEROS
     C*
     C*** BEGIN ADD *** 01/26/98 **************************************
     C                   MOVE      SSFND         KKFND
     C*
     C  N90SSSUB#        IFEQ      *ZEROS
     C*
     C                   SETON                                        9072
     C*
     C                   ELSE
     C*
     C                   MOVE      SSSUB#        KKSUB#
     C*
     C*** END   ADD *** 01/26/98 **************************************
     C*
     C     FNKEY1        CHAIN     PFN02                              99
     C     *IN99         IFEQ      '1'
     C                   SETON                                        9073
     C     *IN90         CABEQ     '1'           CK2END
     C                   ENDIF
     C*** BEGIN ADD *** 01/26/98 **************************************
     C                   ENDIF                                                  SSFND EQ 0
     C*** END   ADD *** 01/26/98 **************************************
     C                   ENDIF
     C*
     C  N90SSEMP#        IFNE      *ZEROS
     C     EMKEY         CHAIN     PEM01                              99
     C     *IN99         IFEQ      '1'
     C                   SETON                                        9074
     C     *IN90         CABEQ     '1'           CK2END
     C                   ENDIF
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 04/20/07 **************************************
     C*** BEGIN CHG *** 12/27/07 **************************************
     C******************** MOVEA'000' *** *IN,40 **********************
     C******************** MOVEA'000' *** *IN,45 **********************
     C*
     C                   MOVEA     '000'         *IN(43)
     C                   MOVEA     '000'         *IN(75)
     C*** END CHG ***** 12/27/07 **************************************
     C                   MOVEL     *BLANK        SCLDSC
     C                   MOVEL     *BLANK        SCS3DS
     C*
     C*    Verify staff code
     C     SSSTAF        IFNE      *BLANK
     C*
     C*** BEGIN CHG *** 12/27/07 **************************************
     C********** SSSTAF ** CHAINPUP02 ************** 46 ***************
     C********** *IN46 *** CABEQ'1' ***** CK2END ******* 90 ***********
     C*
     C     SSSTAF        CHAIN     PUP02                              75
     C     *IN75         IFEQ      '1'
     C                   MOVEL(P)  *BLANK        SCS3DS
     C                   SETON                                        4390
     C                   GOTO      CK2END
     C                   ENDIF                                                   e- IN75=ON
     C*
     C                   MOVEL(P)  UPUSR         SCS3DS
     C     SCS3DS        CAT       UPUSNM:1      SCS3DS
     C                   MOVEA     '010'         *IN(43)                           44 NEXT
     C*** END CHG ***** 12/27/07 **************************************
     C*
     C                   ENDIF                                                   e- STAF<> ' '
     C*
     C*    Verify lost time flag
     C     SSLTFL        IFNE      *BLANK
     C*
     C                   MOVEL     'LTFL'        KKTBID
     C                   MOVEL     SSLTFL        KKTKEY
     C*** BEGIN CHG *** 12/27/07 **************************************
     C********** GTKY **** CHAINPGT02 ************** 47 ***************
     C********** *IN47 *** CABEQ'1' ***** CK2END ******* 90 ***********
     C*
     C     GTKY          CHAIN     PGT02                              76
     C     *IN76         IFEQ      '1'
     C                   MOVEL(P)  *BLANK        SCLDSC
     C                   SETON                                        4490
     C                   GOTO      CK2END
     C                   ENDIF                                                   e- IN76=ON
     C                   MOVEA     '001'         *IN(43)                           45 NEXT
     C*** END CHG ***** 12/27/07 **************************************
     C                   MOVEL(P)  GTDESC        SCLDSC
     C*
     C                   ENDIF                                                  e- LTFL<> ' '
     C*
     C*    Verify status
     C     SSSTAT        IFNE      *BLANK
     C     SSSTAT        ANDNE     'O'
     C     SSSTAT        ANDNE     'C'
     C     SSSTAT        ANDNE     'R'
     C*** BEGIN CHG *** 12/27/07 **************************************
     C******************** SETON ******************* 4590 *************
     C                   SETON                                        774590
     C*** END CHG ***** 12/27/07 **************************************
     C                   GOTO      CK2END
     C*
     C                   ENDIF                                                  e- STAT<> ' '
     C*** END ADD ***** 04/20/07 **************************************
     C*
     C     CK2END        ENDSR
     C*** BEGIN ADD *** 10/29/02 **************************************
     C*===============================================================
     C* GETGRP - SUBROUTINE TO GET GROUPS USER IS AUTHORIZED TO
     C*===============================================================
     C*
     C     GETGRP        BEGSR
     C*
     C                   Z-ADD     1             X                 3 0
     C                   MOVE      *OFF          GRPERR            1
     C*
     C     FNKY3         CHAIN     PFN02                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C                   CALL      'SA008R'      RSTPLF
     C*
     C     ##RTCD        IFEQ      *OFF
     C                   MOVE      FNFND         GRP(X)
     C                   ADD       1             X
     C                   ELSE
     C                   MOVE      *ON           GRPERR
     C                   ENDIF                                                  ##RTCD=*OFF
     C*
     C     FNKY3         READE     PFN02                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C     GRPERR        IFEQ      *ON
     C*
     C                   CAT       FND:1         QRYSLT
     C*
     C                   DO        X             Z                 3 0
     C                   CAT       GRP(Z):1      QRYSLT
     C                   ENDDO                                                  Y->X
     C*
     C                   CAT       '))':0        QRYSLT
     C*
     C                   ENDIF                                                  GRPERR=*OFF
     C*
     C                   ENDSR                                                  GETGRP
     C*===============================================================
     C* GETSUB - SUBROUTINE TO GET SUBSCRIBERS USER IS AUTHORIZED TO
     C*===============================================================
     C     GETSUB        BEGSR
     C*
     C     ##CO#         CHAIN     PSM01                              60
     C*
     C                   MOVE      *ON           FIRST             1
     C                   MOVE      ##CO#         XACO#             3
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C                   CALL      'SA008R'      RSTPLS
     C*
     C     ##RTCD        IFEQ      *OFF
     C*
     C                   Z-ADD     SMSUB#        ##SUB#
     C                   Z-ADD     SMSUB#        XXSUB#
     C                   MOVE      SMSUB#        XASUB#            5
     C*
     C     FIRST         IFEQ      *ON
     C                   MOVE      *OFF          FIRST
     C     CO#           CAT(P)    XACO#:1       QRYSLT
     C                   CAT       CO#2:0        QRYSLT
     C                   CAT       SUB#:0        QRYSLT
     C                   CAT       XASUB#:1      QRYSLT
     C                   CAT       ')':0         QRYSLT
     C                   ELSE
     C                   CAT       '*OR':1       QRYSLT
     C                   CAT       SUB#:1        QRYSLT
     C                   CAT       XASUB#:1      QRYSLT
     C                   CAT       ')':0         QRYSLT
     C                   ENDIF                                                  FIRST=*ON
     C*
     C                   CLEAR                   GRP
     C*
     C     ##MRGR        IFEQ      'Y'
     C                   EXSR      GETGRP
     C                   ENDIF                                                  ##MRGR=Y
     C*
     C                   ENDIF                                                  ##RTCD=*OFF
     C*
     C     ##CO#         READE     PSM01                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C                   CAT       ')':0         QRYSLT
     C*
     C*  CALL CL TO CREATE AN OPEN QUERY FILE OVER THE FILES
     C*
     C     OVRDB1        CAT(P)    'FMCMLE':0    XXOVRD           50
     C                   CAT       OVRDB2:0      XXOVRD
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM      XXOVRD        CMDSTR           50
     C                   PARM      44            LENGTH           15 5
     C*
     C     OVRDB1        CAT(P)    'FMCMLB':0    XXOVRD           50
     C                   CAT       OVRDB2:0      XXOVRD
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM      XXOVRD        CMDSTR           50
     C                   PARM      44            LENGTH           15 5
     C*
     C     OVRDB1        CAT(P)    'FMCML1':0    XXOVRD           50
     C                   CAT       OVRDB2:0      XXOVRD
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM      XXOVRD        CMDSTR           50
     C                   PARM      44            LENGTH           15 5
     C*
     C     OVRDB1        CAT(P)    'FMCMLD':0    XXOVRD           50
     C                   CAT       OVRDB2:0      XXOVRD
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM      XXOVRD        CMDSTR           50
     C                   PARM      44            LENGTH           15 5
     C*
     C                   CALL      QRYPGM        QRYPL                  99
     C*
     C                   ENDSR                                                  GETSUB
     C*** END   ADD *** 10/29/02 **************************************
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*** BEGIN ADD *** 07/20/98 **************************************
     C*
     C* Initialize the restriction indicators.
     C*
     C                   SETOFF                                       161718
     C*** END   ADD *** 07/20/98 **************************************
     C***  BEGIN ADD  ***  05/16/00  *********************************
     C                   SETOFF                                       91
     C***  END   ADD  ***  05/16/00  *********************************
     C*
     C*** INITIALIZE SUB# AND GRP#
     C*
     C                   MOVEL     SCUSER        KKUSR
     C     UPKEY         CHAIN(N)  PUP01                              99
     C     *IN99         IFEQ      '0'
     C*** BEGIN ADD *** 07/20/98 **************************************
     C*
     C* Check to see if Subscriber # is restricted.
     C*
     C*** BEGIN ADD *** 10/29/02 **************************************
     C********** UPMRSU    IFEQ 'Y'
     C********** UPSUB#    ANDNE*ZEROS
     C     UPMRSU        IFEQ      'Y'
     C     ##RCDS        ANDEQ     *OFF
     C     UPSUB#        ANDNE     *ZEROS
     C     *IN55         OREQ      *OFF
     C     UPMRSU        ANDEQ     'Y'
     C     UPSUB#        ANDNE     *ZEROS
     C*** END   ADD *** 10/29/02 **************************************
     C*
     C                   Z-ADD     UPSUB#        SSSUB#
     C                   SETON                                        16
     C*
     C*** BEGIN ADD *** 10/29/98 **************************************
     C                   ELSE
     C*
     C     @@SUB#        IFNE      *ZEROS
     C*
     C                   Z-ADD     @@SUB#        SSSUB#
     C                   SETON                                        16
     C*
     C                   ENDIF
     C*** END   ADD *** 10/29/98 **************************************
     C*
     C                   ENDIF                                                  UPMRSU 'Y'
     C*
     C* Check to see if Group # is restricted.
     C*
     C*** BEGIN ADD *** 10/29/02 **************************************
     C********** UPMRGR    IFEQ 'Y'
     C********** UPSUB#    ANDNE*ZEROS
     C********** UPFND     ANDNE*ZEROS
     C     UPMRGR        IFEQ      'Y'
     C     ##RCDS        ANDEQ     *OFF
     C     UPSUB#        ANDNE     *ZEROS
     C     UPFND         ANDNE     *ZEROS
     C     *IN55         OREQ      *OFF
     C     UPMRGR        ANDEQ     'Y'
     C     UPSUB#        ANDNE     *ZEROS
     C     UPFND         ANDNE     *ZEROS
     C*** END   ADD *** 10/29/02 **************************************
     C*
     C                   Z-ADD     UPSUB#        SSSUB#
     C                   Z-ADD     UPFND         SSFND
     C                   SETON                                        1617
     C*** BEGIN ADD *** 10/29/98 **************************************
     C                   ELSE
     C*
     C     @@FND         IFNE      *ZEROS
     C*
     C                   Z-ADD     @@FND         SSFND
     C                   SETON                                        17
     C*
     C                   ENDIF
     C*** END   ADD *** 10/29/98 **************************************
     C*
     C                   ENDIF                                                  UPMRGR 'Y'
     C*
     C* Check to see if Employer #/Division are restricted.
     C*
     C*** BEGIN ADD *** 10/29/02 **************************************
     C********** UPMREM    IFEQ 'Y'
     C********** UPSUB#    ANDNE*ZEROS
     C********** UPFND     ANDNE*ZEROS
     C********** UPEMP#    ANDNE*ZEROS
     C     UPMREM        IFEQ      'Y'
     C     ##RCDS        ANDEQ     *OFF
     C     UPSUB#        ANDNE     *ZEROS
     C     UPFND         ANDNE     *ZEROS
     C     UPEMP#        ANDNE     *ZEROS
     C     *IN55         OREQ      *OFF
     C     UPMREM        ANDEQ     'Y'
     C     UPSUB#        ANDNE     *ZEROS
     C     UPFND         ANDNE     *ZEROS
     C     UPEMP#        ANDNE     *ZEROS
     C*** END   ADD *** 10/29/02 **************************************
     C*
     C                   Z-ADD     UPSUB#        SSSUB#
     C                   Z-ADD     UPFND         SSFND
     C                   Z-ADD     UPEMP#        SSEMP#
     C                   Z-ADD     UPDIV         SSDIV
     C                   SETON                                        161718
     C*
     C***  BEGIN ADD  ***  05/16/00  *********************************
     C     UPFL5         IFNE      'Y'
     C                   CLEAR                   SSDIV
     C                   SETON                                        91
     C                   ENDIF                                                  UPFL5<>Y
     C***  END   ADD  ***  05/16/00  *********************************
     C*
     C                   ENDIF                                                  UPMREM 'Y'
     C*
     C* Check to see if UPSUB# EQ *zeros, if it is, chain FMFNP
     C* to try to get the WCMC Subscriber # and Group # that is
     C* linked to the WCMS Group #.
     C*
     C* Check to see if user is restricted to a WCMS group.
     C*
     C     UPSUB#        IFEQ      *ZEROS
     C*
     C* If there is a WCMC Sub/Grp that is linked to the WCMS Grp,
     C* then load those values into the subset and protect those
     C* fields.
     C*
     C     FNKEY         CHAIN     PFN01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     UPXFND        IFEQ      'Y'
     C*
     C                   Z-ADD     FNSUB#        SSSUB#
     C                   Z-ADD     FNFND         SSFND
     C                   SETON                                        1617
     C*
     C                   ENDIF                                                  UPXFND 'Y'
     C*
     C     UPXEMP        IFEQ      'Y'
     C*
     C                   Z-ADD     FNSUB#        SSSUB#
     C                   Z-ADD     FNFND         SSFND
     C                   Z-ADD     UPEMP#        SSEMP#
     C                   Z-ADD     UPDIV         SSDIV
     C                   SETON                                        161718
     C*
     C***  BEGIN ADD  ***  05/16/00  *********************************
     C                   SETON                                        91
     C*
     C     UPFL5         IFNE      'Y'
     C                   CLEAR                   SSDIV
     C                   SETOFF                                       91
     C                   ENDIF                                                  UPFL5<>Y
     C***  END   ADD  ***  05/16/00  *********************************
     C*
     C                   ENDIF                                                  UPXEMP 'Y'
     C*
     C                   ENDIF                                                  *IN99 FNKEY
     C*
     C                   ENDIF                                                  UPSUB# *ZERO
     C*
     C                   ENDIF                                                  *IN99 UPKEY
     C*** END   ADD *** 07/20/98 **************************************
     C*** BEGIN DEL *** 07/20/98 **************************************
     C*
     C********** UPFL1     IFEQ 'N'
     C********** FNKEY     CHAINPFN01                99
     C********** *IN99     IFEQ '0'
     C**********           Z-ADDFNSUB#    SSSUB#
     C**********           Z-ADDFNFND     SSFND
     C**********           SETON                     30
     C**********           ELSE
     C**********           SETOF                     30
     C**********           ENDIF
     C**********           ENDIF                           FN CHAIN
     C*
     C*** INITIALIZE EMP# AND DIV
     C*
     C********** UPFL2     IFEQ 'Y'
     C********** UPEMP#    ANDNE*ZEROS
     C**********           MOVELUPEMP#    SSEMP#
     C**********           MOVELUPDIV     SSDIV
     C**********           SETON                     31
     C**********           ELSE
     C**********           SETOF                     31
     C**********           ENDIF
     C*
     C**********           ENDIF                           UP CHAIN
     C*** END   DEL *** 07/20/98 **************************************
     C*
      *** BEG ADD *** 04/09/98 ****************************************
     C*
     C*  IF YOU PASS IN A CASE,
     C*    THEN DISPLAY THAT SINGLE RECORD.
     C*
     C     @@SUB#        IFNE      *ZERO
     C     @@FND         ANDNE     *ZERO
     C     @@FYR         ANDNE     *ZERO
     C     @@CASE        ANDNE     *ZERO
     C                   Z-ADD     @@SUB#        SSSUB#
     C                   Z-ADD     @@FND         SSFND
     C                   END
     C*
      *** END ADD *** 04/09/98 ****************************************
     C*** BEG ADD *** 12/27/07 ****************************************
     C                   MOVEA     '000'         *IN(43)
     C*** END ADD *** 12/27/07 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*** BEGIN ADD *** 10/29/02 **************************************
     C*
     C*** GET SYSTEM INFO
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C                   PARM      *BLANK        ##RHAB            1            REHAB INST
     C                   PARM      *BLANK        ##MSS             1            MS SUPPORT
     C                   PARM      *BLANK        ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANK        ##PRVB            1            PROVIDER BILLS
     C                   PARM      *BLANK        ##NPWC            1            NON-PDS WC  LS
     C                   PARM      *BLANK        ##OS              1            OUTOME/SAVINGS
     C                   PARM      *BLANK        ##ARLS            1            Auto Release Flag
     C                   PARM      *BLANK        ##CCOD            1            Correct Coding Flag
     C                   PARM      *BLANK        ##CRVW            1            Code Review Flag
     C                   PARM      *BLANK        ##PROM            1            ProMatch Flag
     C                   PARM      *BLANK        ##MCRV            1            Medicare CR Flag
     C                   PARM      *BLANK        ##FREQ            1            Frequency Flag
     C                   PARM      *BLANK        ##CRAC            1            CR Auto Complete
     C                   PARM      *BLANK        ##PREO            1            CR Auto Complete
     C                   PARM      *BLANK        ##PARN            1            Par/Non-Par/Both
     C                   PARM      *BLANK        ##MGRI            1            Multi Group Restr
     C*
     C                   SETOFF                                       55
     C     ##MGRI        IFEQ      'Y'
     C                   SETON                                        55
     C                   ENDIF
     C*** END   ADD *** 10/29/02 **************************************
     C*** BEG ADD *** 12/16/98 **************************************
     C                   SETOFF                                       30
     C*** END ADD *** 12/16/98 **************************************
     C                   MOVE(P)   *LOVAL        KKLSN
     C                   MOVE(P)   *LOVAL        KKFSN
     C                   MOVE(P)   *LOVAL        KKSS#
     C                   MOVE(P)   *LOVAL        KKACDT
     C                   MOVE(P)   *LOVAL        KKSUB#
     C                   MOVE(P)   *LOVAL        KKFND
     C                   MOVE(P)   *LOVAL        KKFYR
     C                   MOVE(P)   *LOVAL        KKCASE
     C                   MOVE(P)   *LOVAL        KKSCL#
     C*** BEGIN ADD *** 10/06/06 **************************************
     C                   MOVE(P)   *LOVAL        KKTSIT
     C*** END   ADD *** 10/06/06 **************************************
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C   21              MOVEL(P)  SCPST1        KKLSN
     C   21              MOVEL(P)  SCPSTA        KKFSN
     C   22              MOVEL(P)  SCPST2        KKSS#
     C                   Z-ADD     SCPSTB        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKACDT
     C   23              MOVE(P)   SCPST3        KKSUB#
     C   23              MOVE(P)   SCPSTC        KKFND
     C   23              MOVE(P)   SCPSTD        KKFYR
     C   23              MOVE(P)   SCPSTE        KKCASE
     C   24              MOVEL(P)  SCPST4        KKSCL#
     C*** BEGIN ADD *** 10/06/06 **************************************
     C   25              MOVEL(P)  SCPST5        KKTSIT
     C   25              MOVEL(P)  *BLANKS       KKLSN
     C   25              MOVEL(P)  *BLANKS       KKFSN
     C   25              MOVEL(P)  *BLANKS       KKSS#
     C   25              Z-ADD     *ZEROS        KKACDT
     C*** END   ADD *** 10/06/06 **************************************
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*** BEG ADD *** 09/15/98 ***************************************
     C                   SETOFF                                       3031
     C*** END ADD *** 09/15/98 ***************************************
     C                   MOVEL(P)  SCLSN         KKLSN
     C                   MOVEL(P)  SCFSN         KKFSN
     C                   MOVEL(P)  SCSS#         KKSS#
     C                   MOVE(P)   SCACDT        KKACDT
     C                   MOVEL(P)  SCSUB#        KKSUB#
     C                   MOVE(P)   SCFND         KKFND
     C                   MOVE(P)   SCFYR         KKFYR
     C                   MOVE(P)   SCCASE        KKCASE
     C                   MOVEL(P)  SCSCL#        KKSCL#
     C*** BEGIN ADD *** 10/06/06 **************************************
     C                   MOVEL(P)  SCTSIT        KKTSIT
     C*** END   ADD *** 10/06/06 **************************************
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=SEARCH.
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SETOFF                                       808182
     C*
     C*** BEGIN ADD *** 12/27/07 **************************************
     C                   SETOFF                                       434445
     C                   SETOFF                                       757677
     C*** END ADD ***** 12/27/07 **************************************
     C*
     C*** SUBSCRIBER SEARCH
     C*
     C     HFLD          IFEQ      'SSSUB#'
     C*** BEGIN CHG *** 07/20/98 **************************************
     C********** *IN30     ANDEQ'1'
     C     *IN16         ANDEQ     *ON
     C*** END   CHG *** 07/20/98 **************************************
     C                   SETON                                        90
     C                   ENDIF
     C*
     C     HFLD          IFEQ      'SSSUB#'
     C*** BEGIN CHG *** 07/20/98 **************************************
     C********** *IN30     ANDEQ'0'
     C     *IN16         ANDEQ     *OFF
     C*** END   CHG *** 07/20/98 **************************************
     C                   CALL      'F0055R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C     *INKC         IFEQ      *ON
     C                   MOVE      '1'           OUTFLG
     C*                    SETON                     90
     C                   ENDIF
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        9080
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PSM02                              99
     C                   MOVE      SMSUB#        SSSUB#
     C                   SETON                                        9081
     C                   ENDIF
     C*
     C                   ENDIF                                                  SSSUB# IFEQ
     C*
     C     HFLD          IFEQ      'SSFND'
     C*** BEGIN CHG *** 07/20/98 **************************************
     C********** *IN30     ANDEQ'1'
     C     *IN17         ANDEQ     *ON
     C*** END   CHG *** 07/20/98 **************************************
     C                   SETON                                        90
     C                   ENDIF
     C*
     C     HFLD          IFEQ      'SSFND'
     C*** BEGIN CHG *** 07/20/98 **************************************
     C********** *IN30     ANDEQ'0'
     C     *IN17         ANDEQ     *OFF
     C*** END   CHG *** 07/20/98 **************************************
     C*
     C     SSSUB#        IFEQ      *ZEROS
     C                   SETON                                        9072
     C                   ELSE
     C******** Beg Chg ***** 12/18/24 ***************************
     C                   SETON                                        90
     C****               MOVE      SSSUB#        XXSUB#
     C****
     C**** SMKEY1        CHAIN     PSM01                              99
     C****
     C****               CALL      'F0060R'
     C****               PARM      '2'           ##CALL            1
     C**** *INKC         PARM      '0'           ##KC              1
     C**** *INKL         PARM      '0'           ##KL              1
     C*** BEGIN CHG *** 10/06/06 **************************************
     C******************** PARM DBRRN5    ##RRN   90
     C****               PARM      DBRRN6        ##RRN             9 0
     C*** END   CHG *** 10/06/06 **************************************
     C****
     C**** *INKC         IFEQ      *ON
     C****               MOVE      '1'           OUTFLG
     C****               ENDIF
     C****
     C**** *INKL         IFEQ      *ON
     C**** ##RRN         OREQ      *ZEROS
     C****               SETON                                        9081
     C****               ELSE
     C****
     C**** ##RRN         CHAIN     PFN03                              99
     C****
     C****               MOVE      FNFND         SSFND
     C****               MOVE      FNSUB#        SSSUB#
     C****               SETON                                        9082
     C****               ENDIF
     C****
     C******** End Chg ***** 12/18/24 ***************************
     C                   ENDIF                                                  SSSUB# EQ 0
     C*
     C                   ENDIF                                                  SSFND IFEQ
     C*
     C*** EMPLOYER SEARCH
     C*
     C     HFLD          IFEQ      'SSEMP#'
     C*** BEGIN CHG *** 07/20/98 **************************************
     C********** *IN31     ANDEQ'1'
     C     HFLD          OREQ      'SSDIV'
     C*
     C     *IN18         IFEQ      *ON
     C*
     C                   SETON                                        90
     C*
     C                   ELSE
     C*** END   CHG *** 07/20/98 **************************************
     C*
     C*** BEGIN DEL *** 07/20/98 **************************************
     C********** HFLD      IFEQ 'SSEMP#'
     C********** *IN31     ANDEQ'0'
     C*** END   DEL *** 07/20/98 **************************************
     C*
     C                   CALL      'F0140R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      *ZEROS        ##RRN2            9 0
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        9082
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PEM02                              99
     C*
     C                   MOVE      EMEMP#        SSEMP#
     C                   MOVE      EMDIV         SSDIV
     C                   SETON                                        90
     C*** BEGIN ADD *** 04/20/07 **************************************
     C*** BEGIN CHG *** 12/27/07 **************************************
     C******************** SETON ******************* 40 ***************
     C                   SETON                                        4390       STAF NEXT
     C*** END CHG ***** 12/27/07 **************************************
     C*** END ADD ***** 04/20/07 **************************************
     C*
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 07/20/98 **************************************
     C                   ENDIF                                                  *IN18 *OFF
     C*** END   ADD *** 07/20/98 **************************************
     C*
     C                   ENDIF                                                  SSEMP# IFEQ
     C*
     C*** BEGIN ADD *** 04/20/07 **************************************
     C* SCREEN 3 - STAFF SEARCH.
     C*
     C     HFLD          IFEQ      'SSSTAF'
     C*
     C                   CALL      'S0005R'
     C                   PARM      '2'           ##CALL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      3             ##VIEW            1 0
     C*
     C     ##RRN         IFEQ      *ZEROS
     C*** BEGIN CHG *** 12/27/07 **************************************
     C******************** SETON ********************* 46 **err  Not found
     C******************** SETON ******************* 9040 ***** PC same    ???
     C                   SETON                                        4390           PC same
     C*** END CHG ***** 12/27/07 **************************************
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PUP03                              99
     C*
     C                   MOVEL(P)  UPUSR         SCS3DS
     C     SCS3DS        CAT       UPUSNM:1      SCS3DS
     C*
     C                   MOVEL     UPSTAF        SSSTAF
     C*** BEGIN CHG *** 12/27/07 **************************************
     C******************** SETON******************** 9041 ***** PC next
     C                   SETOFF                                       43
     C                   SETON                                        4490           PC next
     C*** END CHG ***** 12/27/07 **************************************
     C                   ENDIF                                                   RR# <> 0
     C                   ENDIF                                                   HFLD = STAF
     C*
     C* SCREEN 3 SUBSET - LOST TIME STATUS SEARCH.
     C     HFLD          IFEQ      'SSLTFL'
     C*
     C                   MOVEL     'LTFL'        ##TBID
     C                   CALL      'F2380R'      SCHPL
     C*
     C*** BEGIN CHG *** 12/27/07 **************************************
     C********** *INKL     IFEQ *ON ***********************************
     C********** ##RRN *** OREQ *ZEROS ********************************
     C* ***************************************************************
     C******************** SETON ******************* 904741 ***********
     C*
     C     ##RRN         IFEQ      *ZEROS
     C                   SETOFF                                       43
     C                   SETON                                        4490           STAY PUT
     C*** END CHG ***** 12/27/07 **************************************
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGT01                              99
     C*
     C*** BEGIN CHG *** 12/27/07 **************************************
     C******************** MOVELGTDESC    SCLDSC **********************
     C                   MOVEL(P)  GTDESC        SCLDSC
     C                   MOVEL     GTKEY         SSLTFL
     C******************** SETON ******************* 9042 ******* PC next
     C                   SETOFF                                       4344
     C                   SETON                                        4590             PC next
     C*** END CHG ***** 12/27/07 **************************************
     C*** BEGIN CHG *** 12/27/07 **************************************
     C*** END CHG ***** 12/27/07 **************************************
     C                   ENDIF
     C*
     C                   ENDIF                                                   HFLD = LTFL
     C*** END ADD ***** 04/20/07 **************************************
     C*
     C*** BEGIN CHG ***  1/23/09 **************************************
     C*** BEGIN ADD *** 11/18/08 **************************************
     C*  No status search
     C     HFLD          IFEQ      'SSSTAT'
     C                   SETON                                        90
     C                   SETON                                        78
     C                   ENDIF
     C*** END ADD ***** 11/18/08 **************************************
     C*** END CHG *****  1/23/09 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C**         *IN21     IFEQ *ON
     C**                   Z-ADDCMSUB#    XXSUB#
     C**                   Z-ADDCMFND     XXFND
     C**                   Z-ADDCMFYR     XXFYR
     C**                   Z-ADDCMCASE    XXCASE
     C**         CMKEY     CHAINDBRCDF3              99
     C**                   Z-ADDDBRRN3    SCDBRR
     C**                   ENDIF
     C*
     C*****                MOVELCMSS#     XXSS#
     C*****      CDKEY     CHAINPCD01                99
     C*
     C***  BEGIN ADD  ***  01/28/98  *********************************
     C*
     C*** BEGIN DEL *** 04/22/99 **************************************
     C********             MOVELACDPG2    ACDPGM    P
     C*** END DEL *** 04/22/99 ****************************************
     C*** BEGIN ADD *** 04/22/99 **************************************
     C*
     C*          When number of lines selected matches the number of
     C*          lines refreshed: set ACDPGM back to the ADD program.
     C*          ------------------------------------------------------
     C                   ADD       1             XXTS2             7 0
     C     XXTS1         IFEQ      XXTS2
     C     XXTS1         OREQ      0
     C                   Z-ADD     0             XXTS1
     C                   Z-ADD     0             XXTS2
     C                   MOVEL(P)  ACDPG2        ACDPGM
     C                   ENDIF
     C*** END ADD *** 04/22/99 ****************************************
     C*
     C     VLDOPT        IFEQ      'N'
     C*
     C                   EXSR      ASSRRN
     C***** BEGIN DEL **** 03/30/99 ***********************************
     C**********           ELSE
     C**********
     C**********           MOVE 'N'       VLDOPT
     C***** END DEL ****** 03/30/99 ***********************************
     C                   ENDIF                                                  VLDOPT<>Y
     C***  END   ADD  ***  01/28/98  *********************************
     C*
     C                   MOVEL     CMLSN         SCLSN
     C                   MOVEL     CMFSN         SCFSN
     C*
     C                   MOVEL(P)  CMLSN         ##LAST           56
     C                   MOVEL(P)  CMFSN         ##FRST           15
     C                   MOVE      '1'           ##TERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    ##LAST
     C                   PARM                    ##FRST
     C                   PARM                    ##TERM
     C                   MOVEL     ##LAST        SCNAME
     C*
     C*****                MOVELCDSS#     SCSS#
      ***** Beg Chg *** 06/09/20 *******************************************
      /Free
         scSS# = MaskData('SSN':'CMSS#':CMSS#);
      /End-free
     C*****              MOVEL     CMSS#         SCSS#
      ***** End Chg *** 06/09/20 *******************************************
     C                   MOVEL     CMSUB#        SCSUB#
     C                   MOVEL     SCSUB#        ALSUB#            5
     C*** BEG ADD *** 12/18/97 ************************
     C                   MOVEL(P)  ALSUB#        QQNUM
     C                   MOVEL(P)  *BLANKS       QQMASK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     QQNUM         ALSUB#
     C*** END ADD *** 12/18/97 ************************
     C                   MOVEL     CMFND         SCFND
     C                   MOVEL     SCFND         ALFND             3
     C*** BEG ADD *** 12/18/97 ************************
     C                   MOVEL(P)  ALFND         QQNUM
     C                   MOVEL(P)  *BLANKS       QQMASK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     QQNUM         ALFND
     C*** END ADD *** 12/18/97 ************************
     C                   MOVE      CMFYR         SCFYR
     C                   MOVE      SCFYR         ALFYR             3
     C*** BEG ADD *** 12/18/97 ************************
     C                   MOVEL(P)  ALFYR         QQNUM
     C                   MOVEL(P)  *BLANKS       QQMASK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     QQNUM         ALFYR
     C*** END ADD *** 12/18/97 ************************
     C                   MOVEL     CMCASE        SCCASE
     C                   MOVEL     SCCASE        ALCASE            7
     C*** BEG ADD *** 12/18/97 ************************
     C                   MOVEL(P)  ALCASE        QQNUM
     C                   MOVEL(P)  *BLANKS       QQMASK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     QQNUM         ALCASE
     C*** END ADD *** 12/18/97 ************************
     C                   Z-ADD     CMACDT        SCACDT
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXACDT            8 0
     C*** BEG ADD *** 12/18/97 ************************
     C                   MOVEL(P)  XXACDT        QQNUM
     C                   MOVEL(P)  'DATE'        QQMASK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     QQNUM         ALACDT           10
     C*** END ADD *** 12/18/97 ************************
      *
     C                   MOVEL     CMSCL#        SCSCL#
     C*** BEGIN ADD *** 10/06/06 **************************************
     C                   MOVEL(P)  CMTSIT        SCTSIT
     C*** END   ADD *** 10/06/06 **************************************
     C*
     C*** BEG ADD *** 06/08/98 ************************
     C                   MOVEL     CMSTAT        SCSTAT
     C*** END ADD *** 06/08/98 ************************
     C*** BEGIN ADD *** 01/11/99 **************************************
     C                   MOVEL     CMDNY         SCDNY
     C*** END ADD *** 01/11/99 ****************************************
     C                   SELECT
     C     HD#           WHENEQ    1
     C                   Z-ADD     1             Y                 2 0
     C                   MOVEA     SCNAME        DTA1(Y)
     C***BEG CHG *** 06/08/98 *************************
     C*********            ADD  31        Y
     C                   ADD       27            Y
     C***END CHG *** 06/08/98 *************************
     C                   MOVEA     SCSS#         DTA1(Y)
     C                   ADD       10            Y
     C                   MOVEA     ALSUB#        DTA1(Y)
     C                   ADD       6             Y
     C                   MOVEA     ALFND         DTA1(Y)
     C                   ADD       4             Y
     C                   MOVEA     ALFYR         DTA1(Y)
     C                   ADD       4             Y
     C                   MOVEA     ALCASE        DTA1(Y)
     C                   ADD       8             Y
     C                   MOVEA     ALACDT        DTA1(Y)
     C*** BEG ADD *** 06/08/98 ***********************
     C                   ADD       12            Y
     C                   MOVEA     SCSTAT        DTA1(Y)
     C*** END ADD *** 06/08/98 ***********************
     C                   MOVEA(P)  DTA1          SCDATA
     C*
     C     HD#           WHENEQ    2
     C                   Z-ADD     1             Y
     C                   MOVEA     SCSS#         DTA2(Y)
     C                   ADD       11            Y
     C                   MOVEA     ALACDT        DTA2(Y)
     C                   ADD       11            Y
     C                   MOVEA     SCNAME        DTA2(Y)
     C*** BEG CHG *** 06/09/98 **********************
     C*                    ADD  31        Y
     C                   ADD       27            Y
     C*** END CHG *** 06/09/98 **********************
     C                   MOVEA     ALSUB#        DTA2(Y)
     C                   ADD       6             Y
     C                   MOVEA     ALFND         DTA2(Y)
     C                   ADD       4             Y
     C                   MOVEA     ALFYR         DTA2(Y)
     C                   ADD       4             Y
     C                   MOVEA     ALCASE        DTA2(Y)
     C*** BEG ADD *** 06/09/98 ***********************
     C                   ADD       9             Y
     C                   MOVEA     SCSTAT        DTA2(Y)
     C*** END ADD *** 06/09/98 ***********************
     C                   MOVEA(P)  DTA2          SCDATA
     C*
     C     HD#           WHENEQ    3
     C                   Z-ADD     1             Y
     C                   MOVEA     ALSUB#        DTA3(Y)
     C                   ADD       6             Y
     C                   MOVEA     ALFND         DTA3(Y)
     C                   ADD       4             Y
     C                   MOVEA     ALFYR         DTA3(Y)
     C                   ADD       4             Y
     C                   MOVEA     ALCASE        DTA3(Y)
     C                   ADD       9             Y
     C                   MOVEA     SCNAME        DTA3(Y)
     C*** BEG CHG *** 06/09/98 **********************
     C*                    ADD  31        Y
     C                   ADD       27            Y
     C*** END CHG *** 06/09/98 **********************
     C                   MOVEA     SCSS#         DTA3(Y)
     C                   ADD       10            Y
     C                   MOVEA     ALACDT        DTA3(Y)
     C*** BEG ADD *** 06/09/98 **********************
     C                   ADD       12            Y
     C                   MOVEA     SCSTAT        DTA3(Y)
     C*** END ADD *** 06/09/98 **********************
     C                   MOVEA(P)  DTA3          SCDATA
     C*
     C     HD#           WHENEQ    4
     C                   Z-ADD     1             Y
     C                   MOVEA     SCSCL#        DTA4(Y)
     C*** BEG CHG *** 06/09/98 *********************
     C***                  ADD  26        Y
     C                   ADD       23            Y
     C                   MOVEA     SCNAME        DTA4(Y)
     C***                  ADD  28        Y
     C                   ADD       27            Y
     C*** END CHG *** 06/09/98 *********************
     C                   MOVEA     SCSS#         DTA4(Y)
     C                   ADD       10            Y
     C                   MOVEA     ALACDT        DTA4(Y)
     C*** BEG ADD *** 06/09/98 *********************
     C                   ADD       12            Y
     C                   MOVEA     SCSTAT        DTA4(Y)
     C*** END ADD *** 06/09/98 *********************
     C                   MOVEA(P)  DTA4          SCDATA
     C*** BEGIN ADD *** 10/06/06 **************************************
     C     HD#           WHENEQ    5
     C                   Z-ADD     1             Y
     C                   MOVEA     SCTSIT        DTA5(Y)
     C                   ADD       23            Y
     C                   MOVEA     SCNAME        DTA5(Y)
     C                   ADD       27            Y
     C                   MOVEA     SCSS#         DTA5(Y)
     C                   ADD       10            Y
     C                   MOVEA     ALACDT        DTA5(Y)
     C                   ADD       12            Y
     C                   MOVEA     SCSTAT        DTA5(Y)
     C                   MOVEA(P)  DTA5          SCDATA
     C*** END   ADD *** 10/06/06 **************************************
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* MVEOBF - Move information to FAEOP fields.
     C*================================================================
     C     MVEOBF        BEGSR
     C*
     C     ##CO#         CHAIN     PC101                              99
     C*
     C                   Z-ADD     C1EOB#        HDEOB#
     C*
     C     C1EOB#        IFEQ      *HIVAL
     C*
     C                   Z-ADD     1             C1EOB#
     C*
     C                   ELSE
     C*
     C                   ADD       1             C1EOB#
     C*
     C                   ENDIF
     C*
     C                   EXSR      UPDC1
     C*
     C                   Z-ADD     C1EOB#        EOCHK#
     C                   Z-ADD     C1EOB#        EOEOB#
     C                   Z-ADD     C1EOB#        SCEOB#
     C                   Z-ADD     ##CO#         EOCO#
     C                   Z-ADD     CMSUB#        EOSUB#
     C                   Z-ADD     CMFND         EOFND
     C                   Z-ADD     CMFYR         EOFYR
     C                   Z-ADD     CMCASE        EOCASE
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EOTRDT
     C*
     C                   MOVE      *ZEROS        EOFEIN
     C                   Z-ADD     *ZEROS        EOMOD
     C                   Z-ADD     *ZEROS        EOAMT
     C                   Z-ADD     *ZEROS        EOFRDT
     C                   Z-ADD     *ZEROS        EOTODT
     C                   MOVE      *BLANKS       EOTRCD
     C                   MOVE      *BLANKS       EOPNAM
     C                   MOVE      *BLANKS       EOADR1
     C                   MOVE      *BLANKS       EOADR2
     C                   MOVE      *BLANKS       EOCTY
     C                   MOVE      *BLANKS       EOST
     C                   Z-ADD     *ZEROS        EOZIP
     C                   MOVE      *BLANKS       EOEOB1
     C                   MOVE      *BLANKS       EOEOB2
     C                   MOVE      *BLANKS       EOEOB3
     C                   MOVE      'M'           EOSYS
     C                   MOVE      'X'           EOFLG1
     C                   MOVE      'R'           EOFLG2
     C                   MOVE      *BLANKS       EOFLG3
     C                   MOVE      'Y'           EOFLG4
     C*** BEGIN CHG *** 08/19/98 **************************************
     C**********           MOVE *BLANKS   EOFLG5
     C*** BEGIN CHG *** 12/01/98 **************************************
     C**********           MOVE 'Y'       EOFLG5
     C                   MOVEL     *BLANKS       EOFLG5
     C*** END   CHG *** 12/01/98 **************************************
     C*** END   CHG *** 08/19/98 **************************************
     C                   MOVE      *BLANKS       EOFLG6
     C                   MOVE      *BLANKS       EOFLG7
     C                   MOVE      *BLANKS       EOFLG8
     C                   MOVE      *BLANKS       EOFLG9
     C                   Z-ADD     *ZEROS        EOAMTC
     C*** BEGIN CHG *** 08/19/98 **************************************
     C**********           MOVE *BLANKS   EOSTAT
     C                   MOVE      'P'           EOSTAT
     C*** END   CHG *** 08/19/98 **************************************
     C                   Z-ADD     *ZEROS        EOWCCK
     C                   MOVE      *BLANKS       EODIA1
     C                   MOVE      *BLANKS       EODIA2
     C                   MOVE      *BLANKS       EODIA3
     C                   MOVE      *BLANKS       EODIA4
     C                   MOVE      *BLANKS       EOEMPL
     C                   Z-ADD     *ZEROS        EODFC
     C                   MOVE      *BLANKS       EOFLIC
     C                   MOVE      'N'           EOTAPE
     C                   Z-ADD     1             EO#BLL
     C                   MOVE      *BLANKS       EOCLSF
     C                   MOVE      *BLANKS       EOAST
     C                   MOVE      *BLANKS       EOAUDT
     C                   MOVE      *BLANKS       EOBTFL
     C                   MOVE      'H'           EOHRFL
     C                   MOVE      *BLANKS       EOBLFL
     C                   MOVE      *BLANKS       EONLFL
     C                   MOVE      *BLANKS       EODIFL
     C                   Z-ADD     *ZEROS        EOINV#
     C                   MOVE      *BLANKS       EORMDS
     C                   MOVE      *BLANKS       EORMK1
     C                   MOVE      *BLANKS       EORMK2
     C                   MOVE      *BLANKS       EORMK3
     C                   MOVE      *BLANKS       EOCKCN
     C                   Z-ADD     *ZEROS        EOOTHR
     C                   Z-ADD     *ZEROS        EONDIS
     C                   MOVE      *BLANKS       EOSB01
     C                   MOVE      *BLANKS       EOSB02
     C                   MOVE      *BLANKS       EOSB03
     C                   MOVE      *BLANKS       EOSB04
     C                   MOVE      *BLANKS       EOSB05
     C                   MOVE      *BLANKS       EOSB06
     C                   MOVE      *BLANKS       EOSB07
     C                   MOVE      *BLANKS       EOSB08
     C                   MOVE      *BLANKS       EOSB09
     C                   MOVE      *BLANKS       EOSB10
     C                   Z-ADD     *ZEROS        EOBAMT
     C                   MOVE      *BLANKS       EOZIPA
     C                   MOVE      *BLANKS       EOCERT
     C                   Z-ADD     *ZEROS        EOEXT
     C*** BEG ADD *** 12/16/98 **************************************
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EOCRDT
     C****END ADD *** 12/16/98 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* OPT7 - OPTION 7 (Work with Medical Case)
     C*================================================================
     C     OPT7          BEGSR
     C                   MOVE      *BLANKS       ##KC
     C                   MOVE      *BLANKS       ##KL
     C*** BEGIN ADD 09/15/97 ******************************************
     C     @@CALL        IFEQ      '1'
     C                   MOVE      'L'           ##MODE
     C                   ELSE
     C                   MOVE      'M'           ##MODE
     C                   ENDIF
     C*** END ADD *** 09/15/97 ****************************************
     C*
     C*** BEGIN DEL *** 09/15/97 **************************************
     C**********           MOVE 'C'       ##MODE
     C*** END DEL *** 09/15/97 ****************************************
     C*
     C*** BEGIN ADD *** 01/11/99 **************************************
     C     *IN19         IFEQ      *ON
     C                   CALL      'FA720R'      FA720P
     C                   MOVE      'D'           ##MODE
     C                   ENDIF                                                  *IN19 = On
     C*** END ADD *** 01/11/99 ****************************************
     C                   CALL      'F0260R'
     C                   PARM                    ##KC
     C                   PARM                    ##KL
     C                   PARM                    ##MODE
     C                   PARM      SCDBRR        ##RRN
     C                   PARM      *BLANKS       ##SS#
     C                   PARM      *ZEROS        ##ACDT            8 0
     C*
     C                   ENDSR
     C*================================================================
     C* OPT8 - OPTION 8 (Diagnosis Information)
     C*================================================================
     C     OPT8          BEGSR
     C*
     C     @@CALL        IFEQ      '1'
     C                   MOVE      'L'           ##CALL
     C                   ELSE
     C                   MOVE      'M'           ##CALL
     C                   ENDIF
     C*** BEGIN ADD *** 01/11/99 **************************************
     C     *IN19         IFEQ      *ON
     C                   CALL      'FA720R'      FA720P
     C                   MOVE      'L'           ##CALL
     C                   ENDIF                                                  *IN19 = On
     C*** END ADD *** 01/11/99 ****************************************
     C*
     C                   CALL      'F0275R'
     C*** BEGIN ADD *** 06/24/97 **************************************
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C*** END ADD *** 06/24/97 ****************************************
     C                   PARM                    ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      SCDBRR        ##RRN2            9 0
     C*
     C                   ENDSR
     C*================================================================
     C* OPT9 - Option 9 (Medical Services Information)
     C*================================================================
     C     OPT9          BEGSR
     C*
     C     @@CALL        IFEQ      '1'
     C                   MOVE      'L'           ##CALL
     C                   ELSE
     C                   MOVE      'M'           ##CALL
     C                   ENDIF
     C*** BEGIN ADD *** 01/11/99 **************************************
     C     *IN19         IFEQ      *ON
     C                   CALL      'FA720R'      FA720P
     C                   MOVE      'L'           ##CALL
     C                   ENDIF                                                  *IN19 = On
     C*** END ADD *** 01/11/99 ****************************************
     C*
     C                   CALL      'F0276R'
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C                   PARM                    ##CALL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      SCDBRR        ##RRN2
     C*
     C                   ENDSR
     C*================================================================
     C* OPT10 - Option 10 (Case History)
     C*================================================================
     C     OPT10         BEGSR
     C*
     C                   CALL      'F0175R'
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C*** BEGIN ADD *** 01/11/99 **************************************
     C********             PARM @@CALL    ##CALL
     C                   PARM      '1'           ##CALL
     C*** END ADD *** 01/11/99 ****************************************
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      SCDBRR        ##RRN2
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 10/02/97 **************************************
     C*================================================================
     C* OPT11 - Option 11 Work with Providers/Specialist
     C*================================================================
     C     OPT11         BEGSR
     C*** BEG ADD *** 10/18/99 **************************************
     C     @@CALL        IFEQ      ' '
     C                   MOVE      ' '           ##CALL
     C                   ELSE
     C                   MOVE      '1'           ##CALL
     C                   ENDIF
     C*** BEG ADD *** 10/18/99 **************************************
     C*** BEGIN ADD *** 01/11/99 **************************************
     C     *IN19         IFEQ      *ON
     C                   CALL      'FA720R'      FA720P
     C                   MOVE      '1'           ##CALL
     C                   ENDIF                                                  *IN19 = On
     C*** END ADD *** 01/11/99 ****************************************
     C*
     C                   CALL      'F0277R'
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C*** BEGIN CHG *** 01/11/99 **************************************
     C********             PARM @@CALL    ##CALL
     C                   PARM                    ##CALL
     C*** END CHG *** 01/11/99 ****************************************
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      SCDBRR        ##RRN2
     C*
     C                   ENDSR
     C*** END ADD *** 10/02/97 ****************************************
     C*================================================================
     C* OPT12 - Option 12 (Work History)
     C*================================================================
     C     OPT12         BEGSR
     C*
     C     @@CALL        IFEQ      '1'
     C*
     C                   MOVEL     'D'           ##MODE
     C*
     C                   ELSE
     C*
     C                   MOVE      ##CO#         #1CO#
     C                   MOVE      SCSUB#        #1SUB#
     C                   MOVE      SCFND         #1FND
     C*
     C                   CALL      'FA701R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    #1CO#
     C                   PARM                    #1SUB#
     C                   PARM                    #1FND
     C                   PARM      *BLANKS       ##OUTS            1
     C*** BEGIN ADD *** 05/03/00 **************************************
     C     ##OUTS        IFNE      'Y'
     C*** END   ADD *** 05/03/00 **************************************
     C*
     C*** BEGIN DEL *** 01/11/99 **************************************
     C********   ##OUTS    IFEQ 'Y'
     C********
     C********             MOVEL'C'       ##MODE
     C********
     C********             ELSE
     C********
     C********             MOVEL'D'       ##MODE
     C********
     C********             ENDIF
     C*** END DEL *** 01/11/99 ****************************************
     C*** BEG ADD *** 09/21/98 **************************************
     C*
     C*** BEG CHG *** 10/06/98 ***************************************
     C                   MOVE      *BLANK        @1WCUP
     C**********           MOVE 'F'       @1WCUP
     C*** END CHG *** 10/06/98 ***************************************
     C*
     C*
     C     ##WCIF        IFEQ      'Y'
     C     ##WCIN        ANDEQ     'Y'
     C                   CALL      'FA715R'
     C                   PARM                    @1RTCD            1
     C                   PARM      ##CO#         @1CO#             3 0
     C                   PARM      KKSUB#        @1SUB#            5 0
     C                   PARM      KKFND         @1FND             3 0
     C                   PARM                    @1PVMC            1 0          Init Med
     C                   PARM                    @1PVPR            1 0          Init Prov
     C                   PARM                    @1WCUP            1            Allw Upd WCMS Clms
     C                   PARM                    @1CLDL            3 0          Clm Dlt Lvl
     C                   PARM                    @1CLDT            1            Clm Dlt Typ
     C                   PARM                    @1CLTL            3 0          Clm Xfer Lvl
     C                   PARM                    @1CLTT            1            Clm Xfer Typ
     C                   PARM                    @1SDCL            3 0          Settle Dt Lvl
     C                   PARM                    @1SDCT            1            Settle Dt Typ
     C                   PARM                    @1BSCL            3 0          BWC Sts Lvl
     C                   PARM                    @1BSCT            1            BWC Sts Typ
     C                   PARM                    @1DCDL            3 0          Del Diag Cd Lvl
     C                   PARM                    @1DCDT            1            Del Diag Cd Typ
     C                   PARM                    @1DCCL            3 0          Chg Diag St Lvl
     C                   PARM                    @1DCCT            1            Chg Diag St Typ
     C                   PARM                    @1C#CL            3 0          Clt Case Lvl
     C                   PARM                    @1C#CT            1            Clt Case Typ
     C                   PARM                    @1FIMR            5            RMCD: From IMR
     C                   PARM                    @1TIMR            5            RMCD: To IMR
     C                   PARM                    @1FRN             5            RMCD: From RN
     C                   PARM                    @1TRN             5            RMCD: To RN
     C*
     C*** BEG CHG *** 10/06/98 ***************************************
     C*** BEG ADD *** 10/06/06 ***************************************
     C*TEST  FOR CLIENT ID# 114
     C     XXCLT#        IFEQ      '114'
     C                   MOVEL     'C'           ##MODE
     C                   ELSE
     C*** END ADD *** 10/06/06 ***************************************
     C     @1WCUP        IFEQ      'Y'
     C********** @1WCUP    IFEQ ' '
     C*** END CHG *** 10/06/98 ***************************************
     C                   MOVEL     'C'           ##MODE
     C*** BEGIN ADD *** 05/03/00 **************************************
     C                   ELSE
     C*
     C                   MOVEL     'D'           ##MODE
     C*** END   ADD *** 05/03/00 **************************************
     C                   ENDIF                                                  @@WCUP = Y
     C*** BEG ADD *** 10/06/06 ***************************************
     C                   ENDIF                                                  XXCLT#=114
     C*** END ADD *** 10/06/06 ***************************************
     C*
     C                   ENDIF                                                  ##WCIF ##WCIN
     C*
     C*** END ADD *** 09/21/98 **************************************
     C*** BEGIN ADD *** 05/03/00 **************************************
     C*
     C                   ELSE
     C*
     C                   MOVEL     'C'           ##MODE
     C*
     C                   ENDIF
     C*** END   ADD *** 05/03/00 **************************************
     C*
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 01/11/99 **************************************
     C     *IN19         IFEQ      *ON
     C                   CALL      'FA720R'      FA720P
     C                   MOVE      'D'           ##MODE
     C                   ENDIF                                                  *IN19 = On
     C*** END ADD *** 01/11/99 ****************************************
     C                   CALL      'F0263R'
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C                   PARM                    ##MODE
     C                   PARM      SCDBRR        ##RRN
     C*** BEGIN ADD *** 10/06/06 **************************************
     C                   PARM      'N'           ##CONT            1
     C*** END   ADD *** 10/06/06 **************************************
     C*** BEGIN ADD *** 03/23/01 **************************************
     C                   PARM      @@CALL        ##CALL
     C*** END   ADD *** 03/23/01 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* OPT13 - Option 13 Treatment Plan
     C*================================================================
     C     OPT13         BEGSR
     C*
     C*** BEGIN ADD *** 01/11/99 **************************************
     C     *IN19         IFEQ      *ON
     C                   CALL      'FA720R'      FA720P
     C                   MOVE      '1'           ##MODE
     C*** BEG ADD *** 11/09/99 **************************************
     C                   ELSE
     C                   MOVE      @@CALL        ##MODE
     C*** END ADD *** 11/09/99 **************************************
     C                   ENDIF                                                  *IN19 = On
     C*** END ADD *** 01/11/99 ****************************************
     C                   CALL      'F0278R'
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C*** BEGIN ADD *** 01/11/99 **************************************
     C********             PARM @@CALL    ##MODE
     C                   PARM                    ##MODE
     C*** END ADD *** 01/11/99 ****************************************
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      SCDBRR        ##RRN2
     C*
     C                   ENDSR
     C*================================================================
     C* OPT14 - Option 14 (DARTS)
     C*================================================================
     C     OPT14         BEGSR
     C*
     C                   MOVE      ##CO#         #@CO#
     C                   MOVE      SCSUB#        #@SUB#
     C                   MOVE      SCFND         #@FND
     C                   MOVE      SCFYR         #@FYR
     C                   MOVE      SCCASE        #@CASE
     C*
     C                   CALL      'FA703R'      DRTSPL
     C*
     C                   ENDSR
     C*================================================================
     C* OPT15 - Option 10 (Pre-Cert)
     C*================================================================
     C     OPT15         BEGSR
      *** BEG DEL *** 03/26/98 ****************************************
     C***                  MOVELOPTNM     ##PRDS
     C***                  MOVE *BLANKS   ##CALL
     C***                  MOVE *BLANKS   ##CERT
     C***                  MOVE *BLANKS   ##EXT
     C***
     C***                  CALL 'U0010R'
     C***                  PARM           ##PRDS
     C***                  PARM           ##CALL
     C***                  PARM           ##CERT 15
     C***                  PARM           ##EXT   3
      *** END DEL *** 03/26/98 ****************************************
     C*
      *** BEG ADD *** 03/26/98 ****************************************
     C*  NEW PRE-CERT
     C*** BEGIN ADD *** 01/11/99 **************************************
     C     *IN19         IFEQ      *ON
     C                   CALL      'FA720R'      FA720P
     C                   MOVE      '1'           ##CALL
     C*** BEGIN ADD *** 02/08/99 **************************************
     C                   ELSE
     C*
     C                   MOVEL     @@CALL        ##CALL
     C*** END   ADD *** 02/08/99 **************************************
     C                   ENDIF                                                  *IN19 = On
     C*** END ADD *** 01/11/99 ****************************************
     C                   CALL      'U0060R'
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C*** BEGIN ADD *** 01/11/99 **************************************
     C********             PARM @@CALL    ##CALL
     C                   PARM                    ##CALL
     C*** END ADD *** 01/11/99 ****************************************
     C                   PARM      SCDBRR        ##RRN
     C*
     C                   ENDSR
      *** END ADD *** 03/26/98 ****************************************
     C*
     C*** BEGIN ADD *** 07/26/97 **************************************
     C*================================================================
     C* OPT16 - Option 16 (EOMB(s))
     C*================================================================
     C     OPT16         BEGSR
     C                   MOVE      *BLANKS       ##CALL
     C*** BEGIN ADD *** 09/15/97 **************************************
     C     @@CALL        IFEQ      '1'
     C                   MOVE      '1'           ##CALL
     C                   ENDIF
     C*** END ADD *** 09/15/97 ****************************************
     C                   MOVE      *BLANKS       ##RTN
     C*** BEGIN ADD *** 01/11/99 **************************************
     C     *IN19         IFEQ      *ON
     C                   CALL      'FA720R'      FA720P
     C                   MOVE      '1'           ##CALL
     C                   ENDIF                                                  *IN19 = On
     C*** END ADD *** 01/11/99 ****************************************
     C*
     C     SCDBRR        CHAIN     PFRCDF                             99
     C     *IN99         IFEQ      '0'
     C                   Z-ADD     *ZEROS        ##EOB#                         ADD 01/10/99
     C                   MOVE      *BLANKS       ##CERT                         ADD 01/10/99
     C                   MOVE      CMFND         ##FND
     C                   MOVE      CMFYR         ##FYR
     C                   MOVE      CMSUB#        ##SUB#
     C                   MOVE      CMCASE        ##CAS#
     C                   MOVE      CMSS#         ##SS#             9
     C                   MOVE      CMACDT        #1ACDT            6
     C                   MOVE      *ZEROS        ##EOB#            9 0
     C*
     C     ##PRVB        IFNE      'Y'                                          ADD 01/10/99
     C*
     C                   CALL      'F0349R'
     C                   PARM                    ##CALL            1
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##FYR             3 0
     C                   PARM                    ##CAS#            7 0
     C                   PARM                    ##RTN             1
     C*
     C*** BEG ADD *** 01/10/99 **************************************
     C                   ELSE                                                   ##PRVB
     C                   CALL      'F0350R'
     C                   PARM                    ##CALL
     C                   PARM                    ##SUB#
     C                   PARM                    ##FND
     C                   PARM                    ##FYR
     C                   PARM                    ##CAS#
     C                   PARM                    ##EOB#            9 0
     C                   PARM                    ##CERT           15
     C*
     C                   ENDIF                                                  ##PRVB
     C*** END ADD *** 01/10/99 **************************************
     C*
     C                   ENDIF                                                  *IN99 IFEQ
     C*
     C                   ENDSR
     C*** END ADD *** 07/26/97 ****************************************
     C*** BEGIN ADD *** 07/02/97 **************************************
     C*** BEGIN ADD *** 07/26/97 **************************************
     C*================================================================
     C* OPT17 - Option 17 Work with Time/Materials Activity
     C*================================================================
     C     OPT17         BEGSR
     C*
     C     @@CALL        IFEQ      ' '
     C                   MOVEL     ' '           ##CALL
     C                   ELSE
     C*
     C                   MOVEL     '1'           ##CALL
     C                   ENDIF
     C*
     C                   MOVE      SCSUB#        KKSUB#
     C                   MOVE      SCFND         KKFND
     C                   MOVE      SCFYR         KKFYR
     C                   MOVE(P)   SCCASE        KKCASE
     C                   MOVE      *ZEROS        SCEOB#
     C*
     C     EOKY          CHAIN     PEO02                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*** BEGIN ADD *** 09/27/99 **************************************
     C     ##PRVB        IFEQ      'Y'
     C*
     C     EAKY          CHAIN     PEA01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     EABINV        IFEQ      *ZEROS
     C     EASYS         ANDEQ     'M'
     C*** BEGIN ADD *** 12/14/01 **************************************
     C     ##ACIS        ANDNE     'Y'
     C     EABINV        OREQ      *ZEROS
     C     EASYS         ANDEQ     'M'
     C     EAEPFL        ANDNE     'P'
     C     ##ACIS        ANDEQ     'Y'
     C*** END   ADD *** 12/14/01 **************************************
     C*
     C                   Z-ADD     EAEOB#        SCEOB#
     C                   LEAVE
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*** END   ADD *** 09/27/99 **************************************
     C*
     C*** BEGIN CHG *** 12/01/98 **************************************
     C********** EOBLFL    IFNE 'P'
     C     EOINV#        IFEQ      *ZEROS
     C*** END   CHG *** 12/01/98 **************************************
     C     EOSYS         ANDEQ     'M'
     C*
     C                   Z-ADD     EOEOB#        SCEOB#
     C                   LEAVE
     C*
     C                   ENDIF
     C*** BEGIN ADD *** 09/27/99 **************************************
     C                   ENDIF
     C*** END   ADD *** 09/27/99 **************************************
     C*
     C     EOKY          READE     PEO02                                  99
     C*
     C                   ENDDO
     C*
     C     ##WCIF        IFEQ      'Y'
     C*
     C* API to access WARPP to be able to determine if there are any
     C* check requests on this particular EOMB, before allowing T/M.
     C*
     C                   ENDIF
     C*
     C     SCEOB#        IFNE      *ZEROS
     C*
     C                   CALL      'F3040R'
     C                   PARM                    ##CALL
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      DBRRNH        ##RRN2
     C*** BEGIN ADD *** 08/19/98 **************************************
     C                   PARM      *ZEROS        ##RRN3            9 0
     C                   PARM      *BLANKS       ##CERT           15
     C                   PARM      *ZEROS        ##EXT             3 0
     C*** END   ADD *** 08/19/98 **************************************
     C*** BEGIN ADD *** 05/19/00 **************************************
     C                   PARM      *BLANKS       ##EOB             1
     C                   PARM      *ZEROS        ##RCN             3 0
     C*** END   ADD *** 05/19/00 **************************************
     C*** BEGIN ADD *** 09/07/99 **************************************
     C     ##CALL        IFEQ      'F'
     C*
     C                   EXSR      DELUPD
     C*
     C                   ENDIF
     C*** END   ADD *** 09/07/99 **************************************
     C*
     C                   ELSE
     C*
     C                   EXSR      WRITER
     C*** BEGIN ADD *** 09/07/99 **************************************
     C*
     C                   MOVE      SCSUB#        KKSUB#
     C                   MOVE      SCFND         KKFND
     C                   MOVE      SCFYR         KKFYR
     C                   MOVE(P)   SCCASE        KKCASE
     C                   MOVE      *ZEROS        SCEOB#
     C*
     C     EOKY          CHAIN     PEO02                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*** BEGIN ADD *** 09/27/99 **************************************
     C     ##PRVB        IFEQ      'Y'
     C*
     C     EAKY          CHAIN     PEA01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     EABINV        IFEQ      *ZEROS
     C     EASYS         ANDEQ     'M'
     C*** BEGIN ADD *** 12/14/01 **************************************
     C     ##ACIS        ANDNE     'Y'
     C     EABINV        OREQ      *ZEROS
     C     EASYS         ANDEQ     'M'
     C     EAEPFL        ANDNE     'P'
     C     ##ACIS        ANDEQ     'Y'
     C*** END   ADD *** 12/14/01 **************************************
     C*
     C                   Z-ADD     EAEOB#        SCEOB#
     C                   LEAVE
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*** END   ADD *** 09/27/99 **************************************
     C*
     C     EOINV#        IFEQ      *ZEROS
     C     EOSYS         ANDEQ     'M'
     C*
     C                   Z-ADD     EOEOB#        SCEOB#
     C                   LEAVE
     C*
     C                   ENDIF
     C*** BEGIN ADD *** 09/27/99 **************************************
     C                   ENDIF
     C*** END   ADD *** 09/27/99 **************************************
     C*
     C     EOKY          READE     PEO02                                  99
     C*
     C                   ENDDO
     C*** END   ADD *** 09/07/99 **************************************
     C*
     C*** BEGIN DEL *** 08/19/98 **************************************
     C**********           MOVE ##CO#     #1CO#
     C**********           MOVE SCSUB#    #1SUB#
     C**********           MOVE SCFND     #1FND
     C**********           MOVE SCFYR     #1FYR
     C**********           MOVE SCCASE    #1CASE
     C**********           MOVE SCCASE    #1EOB#
     C*
     C**********           CALL 'N6181R'
     C**********           PARM           #1CO#   3
     C**********           PARM           #1SUB#  5
     C**********           PARM           #1FND   3
     C**********           PARM           #1FYR   3
     C**********           PARM           #1CASE  7
     C**********           PARM           #1EOB#  9
     C*** END   DEL *** 08/19/98 **************************************
     C*
     C                   CALL      'F3040R'
     C                   PARM                    ##CALL
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C                   PARM      *ZEROS        ##RRN
     C*** BEGIN CHG *** 08/19/98 **************************************
     C**********           PARM DBRRNG    ##RRN2
     C                   PARM      DBRRNH        ##RRN2
     C*** END   CHG *** 08/19/98 **************************************
     C*** BEGIN ADD *** 08/19/98 **************************************
     C                   PARM      *ZEROS        ##RRN3            9 0
     C                   PARM      *BLANKS       ##CERT           15
     C                   PARM      *ZEROS        ##EXT             3 0
     C*** END   ADD *** 08/19/98 **************************************
     C*** BEGIN ADD *** 05/19/00 **************************************
     C                   PARM      *BLANKS       ##EOB             1
     C                   PARM      *ZEROS        ##RCN             3 0
     C*** END   ADD *** 05/19/00 **************************************
     C*
     C     ##CALL        IFEQ      'F'
     C*
     C                   EXSR      DELUPD
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* OPT18 - Option 18 (EDI LOG HISTORY)
     C*================================================================
     C     OPT18         BEGSR
     C                   MOVE      *BLANKS       ##CALL
     C*** BEGIN ADD *** 09/15/97 **************************************
     C     @@CALL        IFEQ      '1'
     C                   MOVE      '1'           ##CALL
     C                   ENDIF
     C*** END ADD *** 09/15/97 ****************************************
     C*** BEGIN ADD *** 01/11/99 **************************************
     C     *IN19         IFEQ      *ON
     C                   CALL      'FA720R'      FA720P
     C                   MOVE      '1'           ##CALL
     C                   ENDIF                                                  *IN19 = On
     C*** END ADD *** 01/11/99 ****************************************
     C*
     C     SCDBRR        CHAIN     PFRCDF                             99
     C     *IN99         IFEQ      '0'
     C*** BEGIN DEL *** 10/09/97 ***********************************************
     C*** BEG ADD *** 06/22/99 **************************************  *********
     C                   MOVE      CMFND         ##FND
     C                   MOVE      CMFYR         ##FYR
     C                   MOVE      CMSUB#        ##SUB#
     C                   MOVE      CMCASE        ##CAS#
     C                   MOVE      CMSS#         ##SS#
     C                   MOVE      CMACDT        ##ACDT
     C                   MOVE      *ZEROS        ##EOB#
     C                   CALL      'F0875R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##CALL            1
     C                   PARM                    ##MODE            1
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##FYR             3 0
     C                   PARM                    ##CAS#            7 0
     C                   PARM                    ##SS#             9
     C                   PARM                    ##ACDT            8 0
     C                   PARM                    ##EOB#            9 0
     C                   PARM      '837'         ##TSET            3
     C*** END ADD *** 06/22/99 **************************************  *********
     C*** END DEL *** 10/09/97 *************************************************
     C*** BEGIN ADD *** 10/09/97 ***********************************************
     C***                  MOVELOPTNM     QQPRDS
     C***                  MOVE *BLANK    QQCALL
     C***                  MOVE CMCASE    QQCASE
     C***                  MOVE CMFYR     QQFYR
     C*** BEGIN ADD *** 02/04/98 **************************************
     C***                  MOVE CMFND     QQFND
     C***                  MOVE CMCO#     QQCO#
     C*** END   ADD *** 02/04/98 **************************************
     C***                  MOVE *BLANK    QQRTCD
     C***                  CALL 'W0270R5'
     C***                  PARM           QQPRDS 40
     C***                  PARM           QQCALL  1
     C***                  PARM           QQCASE  7
     C***                  PARM           QQFYR   3
     C*** BEGIN ADD *** 02/04/98 **************************************
     C***                  PARM           QQFND   3
     C***                  PARM           QQCO#   3
     C*** END   ADD *** 02/04/98 **************************************
     C***                  PARM           QQRTCD  1
     C*** END ADD *** 10/09/97 *************************************************
     C*
     C                   ENDIF                                                  *IN99 IFEQ
     C*
     C                   ENDSR
     C*** END ADD *** 07/26/97 ****************************************
     C*
     C*** BEGIN ADD ** 08/21/97 ************************************************
     C*================================================================
     C* OPT19 - Option 19 (Call EDI Log)
     C*================================================================
     C     OPT19         BEGSR
     C*** BEGIN ADD *** 01/11/99 **************************************
     C                   DO
     C     *IN19         IFEQ      *ON
     C                   SETON                                        9089
     C                   LEAVE
     C                   ENDIF                                                  *IN19 = On
     C*** END ADD *** 01/11/99 ****************************************
     C                   MOVE      ##CO#         QQCO#                          COMPANY
     C*** BEGIN DEL *** 09/03/97 **************************************
     C**********           MOVE ##FND     QQFNDX           FUND ID
     C**********           MOVE ##YR      QQFYR            FUND YR
     C**********           MOVE ##CASE    QQCASE           CASE/CLAIM
     C*** END DEL *** 09/03/97 ****************************************
     C     SCDBRR        CHAIN     PFRCDF                             99
     C     *IN99         IFEQ      '0'
     C                   MOVE      CMFND         QQFNDX
     C                   MOVE      CMFYR         QQFYR
     C                   MOVE      CMCASE        QQCASE
     C                   ENDIF
     C*** BEGIN ADD *** 09/03/97 **************************************
     C*** END ADD *** 09/03/97 ****************************************
     C                   MOVE      *BLANKS       QQACTF                         ACTION FLAG
     C                   MOVE      *BLANKS       QQMGID                         MSG ID
     C                   MOVE      *BLANKS       QQRTCD                         RETURN FLAG
     C                   CALL      'W0070R6'     LOGFPL
     C*** BEGIN ADD *** 01/11/99 **************************************
     C                   ENDDO                                                  DO Loop
     C*** END ADD *** 01/11/99 ****************************************
     C                   ENDSR
     C*** END ADD ** 08/21/97 **************************************************
     C*** BEGIN DEL *** 07/02/97 **************************************
     C*================================================================
     C* OPT20 - Option 20 (Change Status History)
     C*================================================================
     C********** OPT20     BEGSR
     C**********           MOVE *BLANKS   ##KC
     C**********           MOVE *BLANKS   ##KL
     C**********           MOVE *BLANKS   ##OCST
     C**********           MOVE *BLANKS   ##SUB#
     C**********           MOVE *BLANKS   ##SBNM
     C**********           MOVE *BLANKS   ##FUND
     C**********           MOVE *BLANKS   ##FNYR
     C**********           MOVE *BLANKS   ##CASE
     C**********           MOVE *BLANKS   ##EMP#
     C**********           MOVE *BLANKS   ##DIV
     C**********           MOVE *BLANKS   ##EMNM
     C**********           MOVE *BLANKS   ##SS#
     C**********           MOVE *BLANKS   ##ACDT
     C**********           MOVE *BLANKS   ##CLNM
     C**********           MOVE *BLANKS   ##SCL#
     C*
     C**********           MOVE 'A'       ##MODE
     C**********           Z-ADDSCDBRR    ##RRN
     C********** SCDBRR    CHAINPFRCDF               99
     C********** *IN99     IFEQ '0'
     C**********           MOVELCMSUB#    ##SUB
     C**********           MOVELCMFND     ##FUND
     C**********           MOVE CMFYR     ##FNYR
     C**********           MOVELCMCASE    ##CASE
     C**********           MOVELCMEMP#    ##EMP#
     C**********           MOVELCMDIV     ##DIV
     C**********           MOVELCMSS#     ##SS#
     C********** CMACDT    MULT 100.0001  HDACDT
     C**********           MOVELHDACDT    ##ACDT
     C**********           MOVELCMSCL#    ##SCL#
     C**********           MOVELSCNAME    ##CLNM
     C*
     C**********           MOVELCMSUB#    XXSUB#
     C********** SMKEY1    CHAINPSM01                99
     C********** *IN99     IFEQ '0'
     C**********           MOVELSMNAME    ##SBNM
     C**********           ENDIF
     C*
     C**********           MOVELCMFND     XXFND
     C**********           MOVELCMEMP#    XXEMP#
     C**********           MOVELCMDIV     XXDIV
     C********** EMKEY1    CHAINPEM01                99
     C********** *IN99     IFEQ '0'
     C**********           MOVELEMNAME    ##EMNM
     C**********           ENDIF
     C*
     C**********           MOVE CMFYR     XXFYR
     C**********           MOVELCMCASE    XXCASE
     C**********           MOVE *HIVAL    XXEFDT
     C********** CHKEY     SETGTPCH01
     C********** CHKEY1    REDPEPCH01                    99
     C********** *IN99     IFEQ '0'
     C*
     C**********           SELEC
     C********** CHOCST    WHEQ 'O'
     C**********           MOVE 'C'       ##OCST
     C********** CHOCST    WHEQ 'C'
     C**********           MOVE 'R'       ##OCST
     C********** CHOCST    WHEQ 'R'
     C**********           MOVE 'C'       ##OCST
     C**********           ENDSL
     C**********           ENDIF
     C*
     C**********           CALL 'F0175R01'
     C**********           PARM           ##KC
     C**********           PARM           ##KL
     C**********           PARM           ##MODE
     C**********           PARM           ##OCST  1
     C**********           PARM           ##RRN
     C**********           PARM           ##CO#
     C**********           PARM           ##SUB   5
     C**********           PARM           ##SBNM 40
     C**********           PARM           ##FUND  3
     C**********           PARM           ##FNYR  2
     C**********           PARM           ##CASE  7
     C**********           PARM           ##EMP#  5
     C**********           PARM           ##DIV   3
     C**********           PARM           ##EMNM 40
     C**********           PARM           ##SS#   9
     C**********           PARM           ##ACDT  6
     C**********           PARM           ##CLNM 40
     C**********           PARM           ##SCL# 25
     C*
     C**********           ENDIF                           *IN99 IFEQ
     C*
     C**********           ENDSR
     C*** END DEL *** 07/02/97 ****************************************
     C*================================================================
     C* OPT20 - SUBROUTINE FOR OPTION 20
     C*================================================================
     C*
     C* OPTION 20=CHANGE STATUS
     C*
     C     OPT20         BEGSR
     C*
     C                   MOVE      'S'           ##MODE
     C                   CALL      ACDPG1        ACDPL2                 99
     C                   ENDSR
     C*
     C*** BEGIN ADD *** 04/26/00 **************************************
     C*================================================================
     C* OPT21 - A/R INQUIRY
     C*================================================================
     C     OPT21         BEGSR
     C*
     C                   CALL      'N0420R'
     C                   PARM      @@CALL        ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0          NMIMP
     C                   PARM      SCDBRR        ##RRN2            9 0          FMCMP
     C*
     C     *INKC         IFEQ      *ON
     C     *INKL         OREQ      *ON
     C*
     C                   SETOFF                                       KCKL
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END   ADD *** 04/26/00 **************************************
     C*** BEGIN ADD *** 12/14/11 **************************************
     C*================================================================
     C* OPT22 - Work With Payment Limit Buckets
     C*================================================================
     C     OPT22         BEGSR
     C*
     C                   CALL      'F2382R'
     C                   PARM      @@CALL        ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM                    SCSUB#
     C                   PARM                    SCFND
     C                   PARM                    SCFYR
     C                   PARM                    SCCASE
     C*
     C     *INKC         IFEQ      *ON
     C     *INKL         OREQ      *ON
     C*
     C                   SETOFF                                       KCKL
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END   ADD *** 12/14/11 **************************************
     C*** BEGIN ADD *** 06/24/97 **************************************
     C*
     C*================================================================
     C* OPT30 - Option 30 (Department Letters)
     C*================================================================
     C     OPT30         BEGSR
     C*
     C                   CALL      'F4110C'
     C                   PARM      @@CALL        ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
      ***** BEG ADD ******* 06/23/98 **********************************
     C                   PARM      *ZERO         ##RRN1            9 0
      ***** END ADD ******* 06/23/98 **********************************
     C                   PARM      SCDBRR        ##RRN             9 0
      ***** BEG ADD ******* 06/23/98 **********************************
     C                   PARM      'C'           ##TYPE            1
      ***** END ADD ******* 06/23/98 **********************************
      ***** BEG DEL ******* 06/23/98 **********************************
     C**********           PARM 'M'       ##LVL   1
     C**********           PARM *BLANKS   ##KEY  40
      ***** END DEL ******* 06/23/98 **********************************
     C*
     C                   ENDSR
     C*================================================================
     C* OPT81 - Option 81 (WCMS Claim Master Maintenance)
     C*================================================================
     C     OPT81         BEGSR
     C*
     C*** CHANGE USER PARMS FUND IN SMUPP TO MATCH SELECTED CASE'S FUND
     C*
     C                   MOVE      UPFND         HLFND             3
     C     SCDBRR        CHAIN     PFRCDF                             99
     C*
     C*** CHECK TO SEE IF USER IS ALLOWED TO CHANGE USER PARMS
     C*
     C     UPFND         IFNE      CMFND
     C*** BEGIN CHG *** 02/23/00 **************************************
     C********** UPFL1     ANDNE'Y'
     C     UPMRGR        ANDEQ     'Y'
     C*** END   CHG *** 02/23/00 **************************************
     C                   SETON                                        9096
     C                   GOTO      END81
     C                   ENDIF
     C*
     C                   MOVE      CMFND         XXFND
     C                   MOVE      CMSUB#        XXSUB#
     C     FNKEY2        CHAIN     PFN02                              99
     C                   MOVE      FNWCF#        ##WCF#            3
     C                   MOVE      *BLANKS       ##RTCD
     C                   CALL      'F0006R1'
     C                   PARM                    ##CO#
     C                   PARM                    ##WCF#
     C                   PARM                    ##RTCD
     C*
     C*** CALL WCMS CLIAMS MASTER MAINTENANCE
     C*
     C     ##WCIN        IFEQ      'Y'                                          WC INTERFACE
     C     ##NPWC        ANDNE     'Y'                                          ADD 06/17/99
     C*
     C                   MOVE      ##CO#         #1CO#
     C                   MOVE      SCSUB#        #1SUB#
     C                   MOVE      SCFND         #1FND
     C*
     C                   CALL      'FA701R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM      *BLANKS       ##OUTS            1
     C*
     C     ##RTCD        IFEQ      ' '
     C     ##OUTS        ANDNE     'Y'                                          NOT AN OUT ACCT
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       ##RTCD
     C                   PARM                    #1CO#
     C                   PARM      *BLANKS       #1WCF#            3
     C                   PARM                    #1SUB#
     C                   PARM                    #1FND
     C*
     C     ##RTCD        IFEQ      ' '
     C*
     C                   MOVE      SCFYR         #1FYR
     C                   MOVE      SCCASE        #1CASE
     C*
     C                   CALL      'WA004R'                             99      CHG 05/10/99
     C                   PARM                    #1CO#             3
     C                   PARM      #1WCF#        #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C*** BEG ADD *** 08/31/99 **************************************
     C     @@CALL        IFEQ      '1'
     C     @@CALL        OREQ      '2'
     C                   MOVE      '1'           ##CALL
     C                   MOVE      'D'           ##MODE
     C                   ELSE
     C                   MOVE      @@CALL        ##CALL
     C                   MOVE      'C'           ##MODE
     C                   ENDIF
     C*** END ADD *** 08/31/99 **************************************
     C                   CALL      'W0070R'                             99      CHG 05/10/99
     C*** BEGIN CHG *** 07/05/99 **************************************
     C*** BEG CHG *** 08/31/99 **************************************
     C******************   PARM '1'       ##CALL  1
     C                   PARM                    ##CALL            1
     C*** END CHG *** 08/31/99 **************************************
     C*** END   CHG *** 07/05/99 **************************************
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C*** BEG CHG *** 08/31/99 **************************************
     C******************** PARM 'M'       ##MODE  1
     C                   PARM                    ##MODE            1
     C*** END CHG *** 08/31/99 **************************************
     C                   PARM                    ##RRN             9 0
     C                   PARM      *BLANKS       ##SORT            2
     C*** BEGIN ADD *** 11/30/00 **************************************
     C     *INKC         IFEQ      *ON
     C     *INKL         OREQ      *ON
     C                   SETOFF                                       KCKL
     C                   ENDIF
     C*** END   ADD *** 11/30/00 **************************************
     C*
     C                   ELSE
     C                   SETON                                        8690
     C                   ENDIF
     C*
     C                   ELSE
     C                   SETON                                        8690
     C                   ENDIF
     C*
     C                   ELSE
     C                   SETON                                        8590
     C                   ENDIF
     C*
     C                   ELSE
     C                   ENDIF
     C*** BEGIN ADD *** 07/02/97 **************************************
     C*
     C*** CHANGE USER PARM FUND IN SMUPP BACK TO PREVIOUS VALUE
     C*
     C                   MOVE      *BLANKS       ##RTCD
     C                   CALL      'F0006R1'
     C                   PARM                    ##CO#
     C                   PARM                    HLFND
     C                   PARM                    ##RTCD
     C*** END ADD *** 07/02/97 ****************************************
     C     END81         ENDSR
     C*
     C*** END ADD *** 06/24/97 ****************************************
     C*** BEG ADD *** 09/01/98 ****************************************
     C*================================================================
     C* OPT82 - Option 82 (Transfer case)
     C*================================================================
     C     OPT82         BEGSR
     C*
     C*** BEG ADD *** 09/15/98 **************************************
     C*
     C                   SETOFF                                       3031
     C     UPKEY         CHAIN(N)  PUP01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   CALL      'FA715R'
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM      ##CO#         @@CO#             3 0
     C                   PARM      SCSUB#        @@SUB#            5 0
     C                   PARM      SCFND         @@FND             3 0
     C                   PARM                    @@PVMC            1 0          Init Med
     C                   PARM                    @@PVPR            1 0          Init Prov
     C                   PARM                    @@WCUP            1            Allw Upd WCMS Clms
     C                   PARM                    @@CLDL            3 0          Clm Dlt Lvl
     C                   PARM                    @@CLDT            1            Clm Dlt Typ
     C                   PARM                    @@CLTL            3 0          Clm Xfer Lvl
     C                   PARM                    @@CLTT            1            Clm Xfer Typ
     C                   PARM                    @@SDCL            3 0          Settle Dt Lvl
     C                   PARM                    @@SDCT            1            Settle Dt Typ
     C                   PARM                    @@BSCL            3 0          BWC Sts Lvl
     C                   PARM                    @@BSCT            1            BWC Sts Typ
     C                   PARM                    @@DCDL            3 0          Del Diag Cd Lvl
     C                   PARM                    @@DCDT            1            Del Diag Cd Typ
     C                   PARM                    @@DCCL            3 0          Chg Diag St Lvl
     C                   PARM                    @@DCCT            1            Chg Diag St Typ
     C                   PARM                    @@C#CL            3 0          Clt Case Lvl
     C                   PARM                    @@C#CT            1            Clt Case Typ
     C                   PARM                    @@FIMR            5            RMCD: From IMR
     C                   PARM                    @@TIMR            5            RMCD: To IMR
     C                   PARM                    @@FRN             5            RMCD: From RN
     C                   PARM                    @@TRN             5            RMCD: To RN
     C*
     C                   CALL      'SA006R'
     C                   PARM      UPLVL         @@ULVL            3 0          USER LEVEL
     C                   PARM      UPTYPE        @@UTYP            1            USER TYPE
     C                   PARM      @@CLTL        @@FLVL            3 0          FILE LEVEL
     C                   PARM      @@CLTT        @@FTYP            1            FILE TYPE
     C                   PARM                    ##RTCD            1            RETURN CODE
     C*
     C     ##RTCD        IFEQ      'F'
     C                   SETON                                        30
     C                   ELSE
     C*** END ADD *** 09/15/98 **************************************
     C*
     C     ##WCIF        IFEQ      'Y'
     C     ##WCIN        ANDEQ     'Y'
     C                   MOVE      ##CO#         #1CO#
     C                   MOVE      SCSUB#        #1SUB#
     C                   MOVE      SCFND         #1FND
     C*
     C                   CALL      'FA701R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM      *BLANKS       ##OUTS            1
     C*
     C     ##OUTS        IFEQ      'N'
     C                   SETON                                        9031
     C                   ELSE
     C*
     C                   CALL      'F7091R'
     C                   PARM                    ##KC              1
     C                   PARM                    ##KL              1
     C                   PARM      'C'           ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      'T'           ##TD              1
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   CALL      'F7091R'
     C                   PARM                    ##KC              1
     C                   PARM                    ##KL              1
     C                   PARM      'C'           ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      'T'           ##TD              1
     C*
     C                   ENDIF
     C*
     C*** BEG ADD *** 09/15/98 ****************************************
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END ADD *** 09/15/98 ****************************************
     C*
     C                   ENDSR
     C*** END ADD *** 09/01/98 ****************************************
     C*================================================================
     C* OPT90 - Option 90 (Case Management Summary)
     C*================================================================
     C*****
     C*****      OPT90     BEGSR
     C*****                MOVE *BLANK    ##KC
     C*****                MOVE *BLANK    ##KL
     C****BEGIN ADD *** 09/15/97 **************************************
     C*****      @@CALL    IFEQ '1'
     C*****                MOVE 'D'       ##MODE
     C*****                ELSE
     C*****                MOVE 'C'       ##MODE
     C*****                ENDIF
     C****END ADD *** 09/15/97 ****************************************
     C*****
     C****BEGIN DEL *** 09/15/97 **************************************
     C**********           MOVE 'C'       ##MODE
     C****END DEL *** 09/15/97 ****************************************
     C*****                Z-ADDSCDBRR    ##RRN
     C*****                CALL 'F0271R'
     C*****                PARM           ##KC    1
     C*****                PARM           ##KL    1
     C*****                PARM           ##MODE  1
     C*****                PARM           ##RRN
     C*****                ENDSR
     C*================================================================
     C**OPT91 - Option 91 (Tx Reports)
     C*================================================================
     C*****      OPT91     BEGSR
     C*****                MOVE *BLANKS   ##KC
     C*****                MOVE *BLANKS   ##KC
     C*****                MOVE *BLANK    ##CALL
     C****BEGIN ADD *** 09/15/97 **************************************
     C*****      @@CALL    IFEQ '1'
     C*****                MOVE '1'       ##CALL
     C*****                ENDIF
     C****END ADD *** 09/15/97 ****************************************
     C*****                Z-ADDSCDBRR    ##RRN
     C*****                CALL 'F0171R'
     C*****                PARM           ##KC
     C*****                PARM           ##KL
     C*****                PARM           ##CALL
     C*****                PARM           ##RRN
     C*****                ENDSR
     C*================================================================
     C**OPT92  - Option 92 (Physicians of Record)
     C*================================================================
     C*****      OPT92     BEGSR
     C*****                MOVE *BLANK    ##CALL
     C****BEGIN ADD *** 09/15/97 **************************************
     C*****      @@CALL    IFEQ '1'
     C*****                MOVE '1'       ##CALL
     C*****                ENDIF
     C****END ADD *** 09/15/97 ****************************************
     C*****                Z-ADDSCDBRR    ##RRN
     C*****                CALL 'F0172R'
     C*****                PARM           ##CALL
     C*****                PARM           ##RRN
     C*****                ENDSR
     C*================================================================
     C**OPT93 - Option 93 (BWC FROI Information)
     C*================================================================
     C*****      OPT93     BEGSR
     C*****                MOVE *BLANK    ##KC
     C*****                MOVE *BLANK    ##KL
     C****BEGIN ADD *** 09/15/97 **************************************
     C*****      @@CALL    IFEQ '1'
     C*****                MOVE 'D'       ##MODE
     C*****                ELSE
     C*****                MOVE 'C'       ##MODE
     C*****                ENDIF
     C****END ADD *** 09/15/97 ****************************************
     C*****
     C****BEGIN DEL *** 09/15/97 **************************************
     C**********           MOVE 'C'       ##MODE
     C****END DEL *** 09/15/97 ****************************************
     C*****                Z-ADDSCDBRR    ##RRN
     C*****                CALL 'F0274R'
     C*****                PARM           ##KC
     C*****                PARM           ##KL
     C*****                PARM           ##MODE
     C*****                PARM           ##RRN
     C*****                ENDSR
     C*** BEG ADD *** 09/01/98 **************************************
     C*
     C*================================================================
     C* OPT99 - Option 99 (Delete case)
     C*================================================================
     C     OPT99         BEGSR
     C*** BEG ADD *** 09/15/98 **************************************
     C*
     C                   SETOFF                                       3031
     C     UPKEY         CHAIN(N)  PUP01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   CALL      'FA715R'
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM      ##CO#         @@CO#             3 0
     C                   PARM      SCSUB#        @@SUB#            5 0
     C                   PARM      SCFND         @@FND             3 0
     C                   PARM                    @@PVMC            1 0          Init Med
     C                   PARM                    @@PVPR            1 0          Init Prov
     C                   PARM                    @@WCUP            1            Allw Upd WCMS Clms
     C                   PARM                    @@CLDL            3 0          Clm Dlt Lvl
     C                   PARM                    @@CLDT            1            Clm Dlt Typ
     C                   PARM                    @@CLTL            3 0          Clm Xfer Lvl
     C                   PARM                    @@CLTT            1            Clm Xfer Typ
     C                   PARM                    @@SDCL            3 0          Settle Dt Lvl
     C                   PARM                    @@SDCT            1            Settle Dt Typ
     C                   PARM                    @@BSCL            3 0          BWC Sts Lvl
     C                   PARM                    @@BSCT            1            BWC Sts Typ
     C                   PARM                    @@DCDL            3 0          Del Diag Cd Lvl
     C                   PARM                    @@DCDT            1            Del Diag Cd Typ
     C                   PARM                    @@DCCL            3 0          Chg Diag St Lvl
     C                   PARM                    @@DCCT            1            Chg Diag St Typ
     C                   PARM                    @@C#CL            3 0          Clt Case Lvl
     C                   PARM                    @@C#CT            1            Clt Case Typ
     C                   PARM                    @@FIMR            5            RMCD: From IMR
     C                   PARM                    @@TIMR            5            RMCD: To IMR
     C                   PARM                    @@FRN             5            RMCD: From RN
     C                   PARM                    @@TRN             5            RMCD: To RN
     C*
     C                   CALL      'SA006R'
     C                   PARM      UPLVL         @@ULVL            3 0          USER LEVEL
     C                   PARM      UPTYPE        @@UTYP            1            USER TYPE
     C                   PARM      @@CLDL        @@FLVL            3 0          FILE LEVEL
     C                   PARM      @@CLDT        @@FTYP            1            FILE TYPE
     C                   PARM                    ##RTCD            1            RETURN CODE
     C*
     C     ##RTCD        IFEQ      'F'
     C                   SETON                                        30
     C                   ELSE
     C*** END ADD *** 09/15/98 **************************************
     C*
     C     ##WCIF        IFEQ      'Y'
     C     ##WCIN        ANDEQ     'Y'
     C                   MOVE      ##CO#         #1CO#
     C                   MOVE      SCSUB#        #1SUB#
     C                   MOVE      SCFND         #1FND
     C*
     C                   CALL      'FA701R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM      *BLANKS       ##OUTS            1
     C*
     C     ##OUTS        IFEQ      'N'
     C                   SETON                                        9031
     C                   ELSE
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
     C                   CALL      'F7090R'
     C                   PARM      ' '           ##CALL            1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      SCDBRR        ##RRN4            9 0
     C                   PARM      *ZEROS        ##RRN5            9 0
     C                   PARM      *ZEROS        ##RRN6            9 0
     C                   PARM      *ZEROS        ##RRN7            9 0
     C                   PARM      'D'           ##TD              1
     C                   PARM      'X'           ##FLAG            1
     C                   PARM      'N'           ##WCMS            1
     C*
     C                   ENDIF
     C*
     C                   ELSE
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
     C                   CALL      'F7090R'
     C                   PARM      ' '           ##CALL            1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      SCDBRR        ##RRN4            9 0
     C                   PARM      *ZEROS        ##RRN5            9 0
     C                   PARM      *ZEROS        ##RRN6            9 0
     C                   PARM      *ZEROS        ##RRN7            9 0
     C                   PARM      'D'           ##TD              1
     C                   PARM      'X'           ##FLAG            1
     C                   PARM      'N'           ##WCMS            1
     C*
     C*** BEG ADD *** 09/15/98 ****************************************
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END ADD *** 09/15/98 ****************************************
     C*
     C                   ENDSR
     C*** END ADD *** 09/01/98 ****************************************
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C                   SETOFF                                       3031
     C     *INKJ         CASEQ     *ON           KJDO
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*** BEGIN ADD *** 10/29/02 **************************************
     C     *IN55         IFEQ      *ON
     C*
     C                   MOVE      @@CALL        SVCALL
     C                   MOVE      *IN09         SVIN09
     C*
     C     SCAUTH        IFEQ      'I'
     C                   MOVE      *ON           *IN09
     C                   MOVE      '1'           @@CALL
     C                   ENDIF                                                  SCAUTH=I
     C                   ENDIF
     C*** END   ADD *** 10/29/02 **************************************
      ***** END ADD ******* 03/17/99 **********************************
     C*
     C*** BEGIN ADD *** 02/01/98 **************************************
     C*** BEGIN CHG *** 01/11/99 **************************************
     C     SCSEL         IFEQ      '7'
     C********   SCSEL     IFEQ '8'
     C     SCSEL         OREQ      '8'
     C*** END CHG *** 01/11/99 ****************************************
     C     SCSEL         OREQ      '9'
     C     SCSEL         OREQ      '10'
     C     SCSEL         OREQ      '11'
     C     SCSEL         OREQ      '12'
     C     SCSEL         OREQ      '13'
     C     SCSEL         OREQ      '14'
     C     SCSEL         OREQ      '15'
     C     SCSEL         OREQ      '16'
     C     SCSEL         OREQ      '17'
     C     SCSEL         OREQ      '18'
     C     SCSEL         OREQ      '19'
     C     SCSEL         OREQ      '20'
     C     SCSEL         OREQ      '30'
     C     SCSEL         OREQ      '81'
     C*** BEG ADD *** 09/01/98 **************************************
     C     SCSEL         OREQ      '82'
     C     SCSEL         OREQ      '99'
     C*** END ADD *** 09/01/98 **************************************
     C*
     C                   EXSR      UPDTUP
     C*
     C                   ENDIF
     C*** END   ADD *** 02/01/98 **************************************
     C********** SCSEL     CASEQ'6'       OPT6              RSRVD PRINT
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
     C*** BEGIN ADD *** 04/26/00 **************************************
     C     SCSEL         CASEQ     '21'          OPT21
     C*** BEGIN ADD *** 12/14/11 **************************************
     C     SCSEL         CASEQ     '22'          OPT22
     C*** END   ADD *** 12/14/11 **************************************
     C*** END   ADD *** 04/26/00 **************************************
     C     SCSEL         CASEQ     '30'          OPT30
     C     SCSEL         CASEQ     '81'          OPT81
     C*** BEG ADD *** 09/01/98 ***************************************
     C     SCSEL         CASEQ     '82'          OPT82
     C     SCSEL         CASEQ     '99'          OPT99
     C*** END ADD *** 09/01/98 ***************************************
     C*
     C                   ENDCS
     C*
     C*                    MOVEL'Y'       VLDOPT
     C*** BEGIN ADD *** 10/29/02 **************************************
     C     *IN55         IFEQ      *ON
     C                   MOVE      SVCALL        @@CALL
     C                   MOVE      SVIN09        *IN09
     C                   ENDIF
     C*** END   ADD *** 10/29/02 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*** BEGIN ADD *** 10/29/02 **************************************
     C                   CLOSE     FMCMLE
     C                   CLOSE     FMCMLB
     C                   CLOSE     FMCML1
     C                   CLOSE     FMCMLD
     C*** END   ADD *** 10/29/02 **************************************
     F***** BEG ADD ***** 04/12/05 ***********************************
     C     ##MCIF        IFEQ      'Y'
     C     XXCLT#        ANDEQ     '114'
     C                   CLOSE     WMXRL
     C                   ENDIF
     F***** END ADD ***** 04/12/05 ***********************************
     C                   MOVE      '1'           *INLR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*** BEGIN ADD *** 10/29/02 **************************************
     C*
     C                   MOVEL     SCUSER        KKUSR
     C     UPKEY         CHAIN(N)  PUP01                              99
     C*
     C* Check for active user authorization
     C*
     C     *IN55         IFEQ      *ON
     C                   CALL      'SA008R'      RSTPL                  99
     C                   MOVE      UPMRSU        ##MRSU
     C                   MOVE      UPMRGR        ##MRGR
     C                   MOVE      UPMREM        ##MREM
     C*
     C     ##RCDS        IFEQ      *ON
     C     ##MRSU        ANDEQ     'Y'
     C                   EXSR      GETSUB
     C                   ENDIF                                                  ##RCDS=*ON
     C                   ENDIF
     C*
     C                   OPEN      FMCMLE
     C                   OPEN      FMCMLB
     C                   OPEN      FMCML1
     C                   OPEN      FMCMLD
     C*** END   ADD *** 10/29/02 **************************************
     C*
     C**  GET SYSTEM INFO
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C*** BEG ADD *** 01/10/99 **************************************
     C                   PARM                    ##RHAB            1            REHAB INST
     C                   PARM                    ##MSS             1            MS SUPPORT
     C                   PARM                    ##FSCH            1            FEE SCHEDULE
     C                   PARM                    ##PRVB            1            PROVIDER BILL
     C                   PARM                    ##NPWC            1            NON-PDS WC  LS
     C*** END ADD *** 01/10/99 **************************************
     C*** BEGIN ADD *** 12/14/01 **************************************
     C*
     C                   CALL      'SA009R'
     C                   PARM      *ZEROS        ##EOBD            3 0          EOMB DELETE SECURITY
     C                   PARM      *BLANKS       ##EOBI            1            EOMB INTERACIVE PROC
     C                   PARM      *BLANKS       ##ARIN            1            AUDIT/RECON INSTALLE
     C                   PARM      *BLANKS       ##ACIS            1            AUTO CHECK ISSUANCE
     C                   PARM      *BLANKS       ##PAY0            1            PRINT WCMC EOB PAY>0
     C                   PARM      *BLANKS       ##SDTS            1            ALLOW FUTURE SERV DT
     C                   PARM      *BLANKS       ##MCIV            1            MED CASES INIT VIEW
     C                   PARM      *BLANKS       ##PBIV            1            PROV BILLS ININT VIE
     C                   PARM      *BLANKS       ##AINV            1            AUTO INVOICE FLAG
     C                   PARM      *BLANKS       ##WHPG            1            Work History Usage
     C                   PARM      *BLANKS       ##PRIV            1            PROVIDER INIT VIEW
     C                   PARM      *BLANKS       ##TDUP            1            ADJUDICATION DUP CRI
     C                   PARM      *BLANKS       ##NWSL            1            PROV NTWK SELECTION
     C*** END   ADD *** 12/14/01 **************************************
     C*** BEGIN ADD *** 10/06/06 **************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
     C*
     C     XXCLT#        IFEQ      '114'
     C                   SETON                                        33
     C                   ELSE
     C                   SETOFF                                       33
     C                   ENDIF                                                  XXCLT#=114
     C*
     C*** END   ADD *** 10/06/06 **************************************
     F***** BEG ADD ***** 04/12/05 ***********************************
     C     ##MCIF        IFEQ      'Y'
     C     XXCLT#        ANDEQ     '114'
     C                   OPEN      WMXRL
     C                   ENDIF
     F***** END ADD ***** 04/12/05 ***********************************
     C*** BEGIN ADD *** 04/26/00 **************************************
     C*
     C** IS A/R INSTALLED ?
     C*
     C     ##AR          IFNE      'Y'
     C     ##PRVB        ORNE      'Y'
     C*
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      'NNN'         XXMIS
     C                   MOVE      '21'          XXNMBR
     C                   EXSR      UPDOFA
     C*
     C                   ENDIF                                                              '1'
     C*** END   ADD *** 04/26/00 **************************************
     C*
     C**  IS EDI INSTALLED ?
     C*
     C     ##EDI         IFNE      'Y'
     C*
     C**********           MOVELCLRFKY    OFD,16           DEL 06/17/99
     C**********           MOVELCLRFKY    OFD,17           DEL 06/17/99
     C*** BEG ADD *** 06/17/99 *******************************
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      'NNN'         XXMIS
     C                   MOVE      '18'          XXNMBR
     C                   EXSR      UPDOFA
     C                   MOVE      '19'          XXNMBR
     C                   EXSR      UPDOFA
     C*** END ADD *** 06/17/99 *******************************
     C*
     C                   ENDIF                                                              '1'
     C*
     C**  IS UR INSTALLED ?
     C*
     C     ##UR          IFNE      'Y'
     C*
     C**********           MOVELCLRFKY    OFD,13           DEL 06/17/99
     C*** BEG ADD *** 06/17/99 *******************************
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      'NNN'         XXMIS
     C                   MOVE      '15'          XXNMBR
     C                   EXSR      UPDOFA
     C*** END ADD *** 06/17/99 *******************************
     C*
     C                   ENDIF                                                              '1'
     C*
     C**  IS MED CASE MGMT INSTALLED ?
     C*
     C     ##MED         IFNE      'Y'
     C*
     C**********           MOVELCLRFKY    OFD,6            DEL 06/17/99
     C**********           MOVELCLRFKY    OFD,7            DEL 06/17/99
     C**********           MOVELCLRFKY    OFD,9            DEL 06/17/99
     C**********           MOVELCLRFKY    OFD,10           DEL 06/17/99
     C**********           MOVELCLRFKY    OFD,11           DEL 06/17/99
     C**********           MOVELCLRFKY    OFD,15           DEL 06/17/99
     C*** BEG ADD *** 06/17/99 *******************************
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      'NNN'         XXMIS
     C                   MOVE      '08'          XXNMBR
     C                   EXSR      UPDOFA
     C                   MOVE      '09'          XXNMBR
     C                   EXSR      UPDOFA
     C                   MOVE      '11'          XXNMBR
     C                   EXSR      UPDOFA
     C                   MOVE      '12'          XXNMBR
     C                   EXSR      UPDOFA
     C                   MOVE      '13'          XXNMBR
     C                   EXSR      UPDOFA
     C                   MOVE      '17'          XXNMBR
     C                   EXSR      UPDOFA
     C*** BEG ADD *** 06/17/99 *******************************
     C*
     C                   ENDIF                                                              '1'
     C*** BEG ADD *** 12/14/11 ****************************************
     C* Show option 22 when client is CSI
     C     XXCLT#        IFEQ      '114'
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '22'          XXNMBR
     C                   MOVE      'YYY'         XXMIS
     C                   EXSR      UPDOFA
     C                   ENDIF
     C*** END ADD *** 12/14/11 ****************************************
     C*
     C**  CALLED FROM W0070R.
     C*
     C     XXCALL        IFEQ      'W'
     C*
     C*** BEG ADD *** 06/17/99 *******************************
     C     ##WCIF        ORNE      'Y'
     C     ##NPWC        OREQ      'Y'
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      'NNN'         XXMIS
     C                   MOVE      '81'          XXNMBR
     C                   EXSR      UPDOFA
     C*** END ADD *** 06/17/99 *******************************
     C**********           MOVELCLRFKY    OFD,20           DEL 06/17/99
     C*
     C                   ENDIF                                                              '1'
     C*
     C*** BEGIN ADD *** 09/15/97 **************************************
     C     @@CALL        IFEQ      '1'
     C                   MOVE      '1'           ##MODE
     C                   ENDIF                                                  @@CALL IFEQ '1'
     C*** END ADD *** 09/15/97 ****************************************
      *** BEG ADD *** 04/09/98 ****************************************
     C*
     C*  IF YOU PASS IN A CASE,
     C*    THEN DISPLAY THAT SINGLE RECORD.
     C*
     C     @@SUB#        IFNE      *ZERO
     C     @@FND         ANDNE     *ZERO
     C     @@FYR         ANDNE     *ZERO
     C     @@CASE        ANDNE     *ZERO
     C                   MOVE      *ON           *IN20
     C                   MOVEA     '0000'        *IN(21)
     C                   MOVE      'Y'           FIRST             1
     C                   Z-ADD     @@SUB#        KKSUB#
     C                   Z-ADD     @@FND         KKFND
     C                   Z-ADD     @@FYR         KKFYR
     C                   Z-ADD     @@CASE        KKCASE
     C                   END
     C*
      *** END ADD *** 04/09/98 ****************************************
     C*
     C                   MOVEL(P)  ACDPG2        ACDPGM
     C***** BEGIN DEL **** 03/30/99 ***********************************
     C*** BEGIN ADD *** 08/11/98 **************************************
     C**********           MOVEL'N'       VLDOPT
     C*** END   ADD *** 08/11/98 **************************************
     C***** END DEL ****** 03/30/99 ***********************************
     C*** BEG ADD *** 06/17/99 ****************************************
     C*
     C** GET INITIAL VIEW FROM CONTROL FILE SFBPF (THEIR ARE 4 POSSIBLE VIEWS)
     C*
     C                   CALL      'SA009R'
     C                   PARM      *ZEROS        ##EOBD            3 0          EOMB DELETE SECURITY
     C                   PARM      ' '           ##EOBI            1            EOMB INTERACIVE PROC
     C                   PARM      ' '           ##ARIN            1            AUDIT/RECON INSTALLE
     C                   PARM      ' '           ##ACIS            1            AUTO CHECK ISSUANCE
     C                   PARM      ' '           ##PAY0            1            PRINT WCMC EOB PAY>0
     C                   PARM      ' '           ##SDTS            1            ALLOW FUTURE SERV DT
     C                   PARM      ' '           ##MCIV            1            MED CASES INIT VIEW
     C                   PARM      ' '           ##PBIV            1            PROV BILLS ININT VIE
     C     ##MCIV        IFEQ      '1'
     C     ##MCIV        OREQ      '2'
     C     ##MCIV        OREQ      '3'
     C     ##MCIV        OREQ      '4'
     C                   MOVE      ##MCIV        VIEW#                          copybook field
     C                   ELSE
     C                   Z-ADD     1             VIEW#
     C                   ENDIF
     C*** END ADD *** 06/17/99 ****************************************
    C*** Beg Add *** 11/13/12 ***************************************
      /Free
      /If Not Defined(WEB)
       //Disable option 14
           XXOpfn = '1';
           XXMIS = 'NNN';
           XXNmbr = '14';
           Exsr UPDOFA;
       //Disable option 81
           XXOpfn = '1';
           XXMIS = 'NNN';
           XXNmbr = '81';
           Exsr UPDOFA;
       //Disable Function 17
           XXOpfn = '2';
           XXMIS = 'NNN';
           XXNmbr = '17';
           Exsr UPDOFA;
      /EndIf
      /End-Free
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C*
     C                   MOVE      'N'           VLDOPT
     C*
     C*** Begin Add *** 08/06/13 *******************************************
     C*
     C                   eval      DBRRN  = *zero
     C                   eval      DBRRN2 = *zero
     C                   eval      DBRRN3 = *zero
     C                   eval      DBRRN4 = *zero
     C                   eval      DBRRN5 = *zero
     C                   eval      DBRRN6 = *zero
     C                   eval      DBRRN7 = *zero
     C                   eval      DBRRN8 = *zero
     C                   eval      DBRRN9 = *zero
     C*
     C*** End   Add *** 08/06/13 *******************************************
     C*
     C                   SELECT
     C*
     C     *IN20         WHENEQ    *ON                                          DSP SINGLE CASE
     C     FIRST         IFNE      'Y'
     C                   MOVE      *ON           *IN95
     C                   MOVE      'Y'           FIRST
     C                   ELSE
     C                   MOVE      'N'           FIRST
     C     KEY3          CHAIN     DBRCDF3                            95
     C     DBRRN3        CHAIN     PFRCDF                             95
     C                   z-add     dbrrn3        dbrrn
     C                   END
     C*
     C     SSEMP#        WHENNE    *ZEROS
     C     *IN18         ANDEQ     *ON                                          EMP RESTRICT
     C     UPFL5         ANDNE     'Y'
     C*
     C   21KEY20A        READE     DBRCDFK                                95
     C   22KEY20A        READE     DBRCDFL                                95
     C   23KEY20A        READE     DBRCDFM                                95
     C   24KEY20A        READE     DBRCDFN                                95
     C   25KEY20A        READE     DBRCDFQ                                95
      /Free
       dbRRN  = dbRRNk;
       dbRRN2 = dbRRNl;
       dbRRN3 = dbRRNm;
       dbRRN4 = dbRRNn;
       dbRRN5 = dbRRNq;
      /End-free
     C*
     C     SSEMP#        WHENNE    *ZEROS
     C*
     C   21KEY20         READE     DBRCDFK                                95
     C   22KEY20         READE     DBRCDFL                                95
     C   23KEY20         READE     DBRCDFM                                95
     C   24KEY20         READE     DBRCDFN                                95
     C   25KEY20         READE     DBRCDFQ                                95
      /Free
       dbRRN  = dbRRNk;
       dbRRN2 = dbRRNl;
       dbRRN3 = dbRRNm;
       dbRRN4 = dbRRNn;
       dbRRN5 = dbRRNq;
      /End-free
     C*
     C     SSFND         WHENNE    *ZEROS
     C*
     C   21KEY15         READE     DBRCDFD                                95
     C   22KEY15         READE     DBRCDFE                                95
     C   23KEY15         READE     DBRCDF3                                95
     C   24KEY15         READE     DBRCDFF                                95
     C   25KEY15         READE     DBRCDFP                                95
      /Free
       dbRRN  = dbRRNd;
       dbRRN2 = dbRRNe;
       dbRRN3 = dbRRN3;
       dbRRN4 = dbRRNf;
       dbRRN5 = dbRRNp;
      /End-free
     C*
     C     SSSUB#        WHENNE    *ZEROS
     C*
     C   21KEY10         READE     DBRCDFA                                95
     C   22KEY10         READE     DBRCDFB                                95
     C   23KEY10         READE     DBRCDF3                                95
     C   24KEY10         READE     DBRCDFC                                95
     C   25KEY10         READE     DBRCDFO                                95
      /Free
       dbRRN  = dbRRNa;
       dbRRN2 = dbRRNb;
       dbRRN3 = dbRRN3;
       dbRRN4 = dbRRNc;
       dbRRN5 = dbRRNo;
      /End-free
     C*
     C                   OTHER
     C*
     C   21KEY5          READE     DBRCDF                                 95
     C   22KEY5          READE     DBRCDF2                                95
     C   23KEY5          READE     DBRCDF3                                95
     C   24KEY5          READE     DBRCDF4                                95
     C   25KEY5          READE     DBRCDF5                                95
     C*
     C                   ENDSL
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C*
     C                   MOVE      'N'           VLDOPT
     C*
     C*** Begin Add *** 08/06/13 *******************************************
     C*
     C                   eval      DBRRN  = *zero
     C                   eval      DBRRN2 = *zero
     C                   eval      DBRRN3 = *zero
     C                   eval      DBRRN4 = *zero
     C                   eval      DBRRN5 = *zero
     C                   eval      DBRRN6 = *zero
     C                   eval      DBRRN7 = *zero
     C                   eval      DBRRN8 = *zero
     C                   eval      DBRRN9 = *zero
     C*
     C*** End   Add *** 08/06/13 *******************************************
     C*
     C                   SELECT
     C*
     C     *IN20         WHENEQ    *ON                                          DSP SINGLE CASE
     C     FIRST         IFNE      'Y'
     C                   MOVE      *ON           *IN95
     C                   MOVE      'Y'           FIRST
     C                   ELSE
     C                   MOVE      'N'           FIRST
     C     KEY3          CHAIN     DBRCDF3                            95
     C     DBRRN3        CHAIN     PFRCDF                             95
     C                   z-add     dbrrn3        dbrrn
     C                   END
     C*
     C     SSEMP#        WHENNE    *ZEROS
     C     *IN18         OREQ      *ON                                          EMP RESTRICT
     C     UPFL5         ANDEQ     'Y'                                          DIV RESTRICT
     C*
     C   21KEY20         READPE    DBRCDFK                                95
     C   22KEY20         READPE    DBRCDFL                                95
     C   23KEY20         READPE    DBRCDFM                                95
     C   24KEY20         READPE    DBRCDFN                                95
     C   25KEY20         READPE    DBRCDFQ                                95
      /Free
       dbRRN  = dbRRNk;
       dbRRN2 = dbRRNl;
       dbRRN3 = dbRRNm;
       dbRRN4 = dbRRNn;
       dbRRN5 = dbRRNq;
      /End-free
     C*
     C     SSEMP#        WHENNE    *ZEROS
     C     *IN18         ANDEQ     *ON                                          EMP RESTRICT
     C     UPFL5         ANDNE     'Y'
     C*
     C   21KEY20A        READPE    DBRCDFK                                95
     C   22KEY20A        READPE    DBRCDFL                                95
     C   23KEY20A        READPE    DBRCDFM                                95
     C   24KEY20A        READPE    DBRCDFN                                95
     C   25KEY20A        READPE    DBRCDFQ                                95
      /Free
       dbRRN  = dbRRNk;
       dbRRN2 = dbRRNl;
       dbRRN3 = dbRRNm;
       dbRRN4 = dbRRNn;
       dbRRN5 = dbRRNq;
      /End-free
     C*
     C     SSFND         WHENNE    *ZEROS
     C*
     C   21KEY15         READPE    DBRCDFD                                95
     C   22KEY15         READPE    DBRCDFE                                95
     C   23KEY15         READPE    DBRCDF3                                95
     C   24KEY15         READPE    DBRCDFF                                95
     C   25KEY15         READPE    DBRCDFP                                95
      /Free
       dbRRN  = dbRRNd;
       dbRRN2 = dbRRNe;
       dbRRN3 = dbRRN3;
       dbRRN4 = dbRRNf;
       dbRRN5 = dbRRNp;
      /End-free
     C*
     C     SSSUB#        WHENNE    *ZEROS
     C*
     C   21KEY10         READPE    DBRCDFA                                95
     C   22KEY10         READPE    DBRCDFB                                95
     C   23KEY10         READPE    DBRCDF3                                95
     C   24KEY10         READPE    DBRCDFC                                95
     C   25KEY10         READPE    DBRCDFO                                95
      /Free
       dbRRN  = dbRRNa;
       dbRRN2 = dbRRNb;
       dbRRN3 = dbRRN3;
       dbRRN4 = dbRRNc;
       dbRRN5 = dbRRNo;
      /End-free
     C*
     C                   OTHER
     C*
     C   21KEY5          READPE    DBRCDF                                 95
     C   22KEY5          READPE    DBRCDF2                                95
     C   23KEY5          READPE    DBRCDF3                                95
     C   24KEY5          READPE    DBRCDF4                                95
     C   25KEY5          READPE    DBRCDF5                                95
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C*
     C     *IN20         IFNE      *ON                                          DSP SINGLE CASE
     C*
     C                   SELECT
     C*
     C     SSEMP#        WHENNE    *ZEROS
     C*
     C   21KEY16         SETGT     DBRCDFK                            96
     C   22KEY17         SETGT     DBRCDFL                            96
     C   23KEY18         SETGT     DBRCDFM                            96
     C   24KEY19         SETGT     DBRCDFN                            96
     C   25KEYD          SETGT     DBRCDFQ                            96
     C*
     C     SSFND         WHENNE    *ZEROS
     C*
     C   21KEY11         SETGT     DBRCDFD                            96
     C   22KEY12         SETGT     DBRCDFE                            96
     C   23KEY13         SETGT     DBRCDF3                            96
     C   24KEY14         SETGT     DBRCDFF                            96
     C   25KEYC          SETGT     DBRCDFP                            96
     C*
     C     SSSUB#        WHENNE    *ZEROS
     C*
     C   21KEY6          SETGT     DBRCDFA                            96
     C   22KEY7          SETGT     DBRCDFB                            96
     C   23KEY8          SETGT     DBRCDF3                            96
     C   24KEY9          SETGT     DBRCDFC                            96
     C   25KEYB          SETGT     DBRCDFO                            96
     C*
     C*
     C                   OTHER
     C*
     C   21KEY1          SETGT     DBRCDF                             96
     C   22KEY2          SETGT     DBRCDF2                            96
     C   23KEY3          SETGT     DBRCDF3                            96
     C   24KEY4          SETGT     DBRCDF4                            96
     C   25KEYA          SETGT     DBRCDF5                            96
     C*
     C                   ENDSL
     C                   END
     C*
     C                   ENDSR
     C*================================================================
     C*
     C     SETLL         BEGSR
     C*
     C     *IN20         IFNE      *ON                                          DSP SINGLE CASE
     C*
     C                   SELECT
     C*
     C     SSEMP#        WHENNE    *ZEROS
     C*
     C   21KEY16         SETLL     DBRCDFK                            96
     C   22KEY17         SETLL     DBRCDFL                            96
     C   23KEY18         SETLL     DBRCDFM                            96
     C   24KEY19         SETLL     DBRCDFN                            96
     C   25KEYD          SETLL     DBRCDFQ                            96
     C*
     C     SSFND         WHENNE    *ZEROS
     C*
     C   21KEY11         SETLL     DBRCDFD                            96
     C   22KEY12         SETLL     DBRCDFE                            96
     C   23KEY13         SETLL     DBRCDF3                            96
     C   24KEY14         SETLL     DBRCDFF                            96
     C   25KEYC          SETLL     DBRCDFP                            96
     C*
     C     SSSUB#        WHENNE    *ZEROS
     C*
     C   21KEY6          SETLL     DBRCDFA                            96
     C   22KEY7          SETLL     DBRCDFB                            96
     C   23KEY8          SETLL     DBRCDF3                            96
     C   24KEY9          SETLL     DBRCDFC                            96
     C   25KEYB          SETLL     DBRCDFO                            96
     C*
     C*
     C                   OTHER
     C*
     C   21KEY1          SETLL     DBRCDF                             96
     C   22KEY2          SETLL     DBRCDF2                            96
     C   23KEY3          SETLL     DBRCDF3                            96
     C   24KEY4          SETLL     DBRCDF4                            96
     C   25KEYA          SETLL     DBRCDF5                            96
     C*
     C                   ENDSL
     C*
     C                   END
     C*
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C*** BEGIN ADD *** 07/16/97 **************************************
     C     @@CALL        IFEQ      '2'
     C                   MOVE      SCSUB#        @@SUB#
     C                   MOVE      SCFND         @@FND
     C                   MOVE      SCFYR         @@FYR
     C                   MOVE      SCCASE        @@CASE
     C                   MOVEL     SCSS#         @@SS#
     C*** BEGIN ADD *** 08/01/97 **************************************
     C                   Z-ADD     SCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         @@ACDT
     C*** END ADD *** 08/01/97 ****************************************
     C                   if        %parms >= 8
     C                   MOVEL     '*'           @@RTCD
     C                   endif
     C                   ENDIF
     C*** END   ADD *** 07/16/97 **************************************
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C     SSSUB#        IFNE      *ZEROS
     C     SSSUB#        ANDNE     CMSUB#
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSFND         IFNE      *ZEROS
     C     SSFND         ANDNE     CMFND
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSEMP#        IFNE      *ZEROS
     C     SSEMP#        ANDNE     CMEMP#
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSDIV         IFNE      *ZEROS
     C     SSDIV         ANDNE     CMDIV
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*** BEGIN ADD *** 10/29/02 **************************************
     C     *IN55         IFEQ      *ON
     C*
     C                   MOVE      'A'           SCAUTH
     C*
     C* Check Subscriber Restriction
     C*
     C     ##MRSU        IFEQ      'Y'
     C     ##MRGR        OREQ      'Y'
     C     ##MREM        OREQ      'Y'
     C*
     C     ##RCDS        IFEQ      *ON
     C                   CALL      'SA008R'                             99
     C                   PARM                    ##CO#
     C                   PARM      Q1USER        ##USR            10
     C                   PARM      'M'           ##APPL            1
     C                   PARM      *OFF          ##RTCD            1
     C     SCAUTH        PARM      *BLANK        ##AUTH            1
     C                   PARM      *ZERO         #2FND
     C                   PARM      *ZERO         #2EMP#
     C                   PARM      CMSUB#        #2SUB#
     C*
     C     ##RTCD        IFEQ      *ON
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  ##RTCD=*ON
     C                   ENDIF                                                  ##RCDS
     C                   ENDIF                                                  MRSU
     C*
     C*  Check Group restriction
     C*
     C     ##MRSU        IFEQ      'Y'
     C     ##MRGR        OREQ      'Y'
     C     ##MREM        OREQ      'Y'
     C*
     C     ##RCDS        IFEQ      *ON
     C                   CALL      'SA008R'                             99
     C                   PARM                    ##CO#
     C                   PARM      Q1USER        ##USR            10
     C                   PARM      'M'           ##APPL            1
     C                   PARM      *OFF          ##RTCD            1
     C     SCAUTH        PARM      *BLANK        ##AUTH            1
     C                   PARM      CMFND         #2FND
     C                   PARM      *ZERO         #2EMP#
     C                   PARM      CMSUB#        #2SUB#
     C*
     C     ##RTCD        IFEQ      *ON
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  ##RTCD=*ON
     C                   ENDIF                                                  ##RCDS
     C                   ENDIF                                                  MRGR
     C*
     C*  Check for Employer Restriction
     C*
     C     ##MRSU        IFEQ      'Y'
     C     ##MRGR        OREQ      'Y'
     C     ##MREM        OREQ      'Y'
      *
     C     ##RCDS        IFEQ      *ON
     C                   CALL      'SA008R'                             99
     C                   PARM                    ##CO#
     C                   PARM      Q1USER        ##USR            10
     C                   PARM      'M'           ##APPL            1
     C                   PARM      *OFF          ##RTCD            1
     C     SCAUTH        PARM      *BLANK        ##AUTH            1
     C                   PARM      CMFND         #2FND
     C                   PARM      CMEMP#        #2EMP#
     C                   PARM      CMSUB#        #2SUB#
     C*
     C     ##RTCD        IFEQ      *ON
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  ##RTCD=*ON
     C                   ENDIF                                                  ##RCDS
     C                   ENDIF                                                  MREM
     C                   ENDIF
     C*** END   ADD *** 10/29/02 **************************************
     C*
     C*** BEGIN ADD *** 04/20/07 **************************************
     C*    Case nurse staff code
     C     SSSTAF        IFNE      *BLANK
     C     SSSTAF        ANDNE     CMCMNU
     C     SSSTAF        ANDNE     CMCMSU
     C     SSSTAF        ANDNE     CMCLSU
     C     SSSTAF        ANDNE     CMCLAD
     C     SSSTAF        ANDNE     CMCLFA
     C     SSSTAF        ANDNE     CMSTF3
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  e- STAF<>CMNU
     C*
     C*    Lost time flag
     C     SSLTFL        IFNE      *BLANK
     C     SSLTFL        ANDNE     CMLTFL
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  e- STAF<>CMNU
     C*
     C*    Status
     C     SSSTAT        IFNE      *BLANK
     C     SSSTAT        ANDNE     CMSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  e- STAF<>CMNU
     C*** END ADD ***** 04/20/07 **************************************
     C*** BEG ADD ****** 05/25/07 **********************************************
     C*** Call API to convert SUB#/GRP# to WC FND#
     C                   MOVE      CMCO#         #1CO#
     C                   MOVE      CMSUB#        #1SUB#
     C                   MOVE      CMFND         #1FND
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       ##RTCD
     C                   PARM                    #1CO#
     C                   PARM      *BLANKS       #1WCF#
     C                   PARM                    #1SUB#
     C                   PARM                    #1FND
     C*
     C*** Call claims security API to check claim authority
     C                   MOVE      CMCO#         #@CO#
     C                   MOVE      #1WCF#        #@FND
     C                   MOVE      CMFYR         #@FYR
     C                   MOVE      CMCASE        #@CASE
     C                   MOVE      *BLANKS       #@RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     #@RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                     RTCD = '2'
     C*** END ADD ****** 05/25/07 **********************************************
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 02/01/98 **************************************
     C*================================================================
     C* UPDTUP - Update User parm file upon selection (2, 5).
     C*================================================================
     C     UPDTUP        BEGSR
     C*
     C     SCDBRR        CHAIN(N)  PFRCDF                             99
     C*** BEGIN ADD *** 08/11/98 **************************************
     C     *IN99         IFEQ      *OFF
     C*** END   ADD *** 08/11/98 **************************************
     C*
     C                   MOVEL     Q1USER        KKUSR
     C*
     C     UPKEY         CHAIN     PUP01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVE      CMCO#         UPCO#
     C                   MOVE      CMSUB#        UPSUB#
     C                   MOVE      CMFND         UPFND
     C                   MOVE      CMFYR         UPFYR
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1PGM         UPPGM
     C                   MOVEL     DSTIME        UPTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         UPDT
     C*
     C                   UPDATE    PUP01
     C*
     C                   ENDIF
     C*** BEGIN ADD *** 08/11/98 **************************************
     C                   ENDIF
     C*** END   ADD *** 08/11/98 **************************************
     C*
     C                   ENDSR
     C*** END   ADD *** 02/01/98 **************************************
     C*================================================================
     C* UPDC1  - Updates Company auxiliary file.
     C*================================================================
     C     UPDC1         BEGSR
     C*
     C                   MOVEL     Q1USER        C1IUSR
     C                   MOVEL     Q1USER        C1USER
     C                   MOVEL     Q1PGM         C1PGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVE      DSTIME        C1TIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C1IDT
     C*
     C                   MOVEL     SRDTO         C1DT
     C*
     C                   UPDATE    PC101
     C*
     C                   ENDSR
     C*** BEGIN DEL *** 09/07/99 **************************************
     C*================================================================
     C* WRITEE - Writes record to FAEOP.
     C*================================================================
     C********** WRITEE    BEGSR
     C*
     C**********           CLEARPEO01
     C**********           EXSR MVEOBF
     C*
     C**********           MOVELQ1USER    EOIUSR
     C**********           MOVELQ1USER    EOUSER
     C**********           MOVELQ1PGM     EOPGM
     C*
     C**********           TIME           DSDTTM
     C*
     C**********           MOVE DSTIME    EOTIME
     C*
     C**********           Z-ADDDSDATE    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     EOIDT
     C*
     C**********           MOVELSRDTO     EODT
     C*
     C**********           WRITEPEO01
     C***                  FEOD FAEOP
     C*
     C**********           ENDSR
     C*** END   DEL *** 09/07/99 **************************************
     C*================================================================
     C* WRITER - Writes record to FAEOP, FAE1P AND NAMBP.
     C*================================================================
     C     WRITER        BEGSR
     C*
     C     SCDBRR        CHAIN     PFRCDF                             99
     C*** BEGIN ADD *** 09/07/99 **************************************
     C*
     C     ##CO#         CHAIN     PC101                              99
     C*
     C                   Z-ADD     C1EOB#        HDEOB#
     C*
     C     C1EOB#        IFEQ      *HIVAL
     C*
     C                   Z-ADD     1             C1EOB#
     C*
     C                   ELSE
     C*
     C                   ADD       1             C1EOB#
     C*
     C                   ENDIF
     C*
     C                   EXSR      UPDC1
     C*** END   ADD *** 09/07/99 **************************************
     C*
     C* Write record to the EOMB file (FAEOP).
     C*
     C*** BEGIN DEL *** 09/07/99 **************************************
     C**********           EXSR WRITEE
     C*** END   DEL *** 09/07/99 **************************************
     C*** BEGIN ADD *** 09/07/99 **************************************
     C                   MOVE      *BLANKS       KKWRUP
     C                   Z-ADD     *ZEROS        @@RCN
     C                   MOVEL     'M'           @@SYS
     C*
     C     ##PRVB        IFEQ      'Y'
     C*** END   ADD *** 09/07/99 **************************************
     C*
     C* Write record to the EOMB auxiliary file (FAE1P).
     C*
     C*** BEG CHG *** 12/16/98 ***************************************
     C*** BEGIN DEL *** 09/07/99 **************************************
     C**********           MOVE *BLANKS   KKWRUP
     C**********           MOVE 'W'       XXFAE1
     C*** END   DEL *** 09/07/99 **************************************
     C*** BEGIN ADD *** 09/07/99 **************************************
     C                   MOVE      'W'           XXFAEO
     C                   MOVE      'W'           XXFAEA
     C*** END   ADD *** 09/07/99 **************************************
     C*
     C                   CALL      'AA001R'
     C                   PARM      SCDBRR        @@RRN             9 0          RRN OF CASE
     C                   PARM      C1EOB#        @@EOB#            9 0          EOB #
     C                   PARM                    @@RCN             3 0          RECON #
     C                   PARM      Q1PGM         @@PGM            10            NAME OF PGM CALLING
     C                   PARM                    @@BAMT            9 2          BRS AMOUNT
     C                   PARM                    @@NAMT            9 2          NWS AMOUNT
     C*** BEGIN CHG *** 09/17/99 **************************************
     C**********           PARM           @@BHRF  1        BRS INV H/R FLAG
     C                   PARM      'H'           @@BHRF            1            BRS INV H/R FLAG
     C*** END   CHG *** 09/17/99 **************************************
     C                   PARM                    @@BBLF            1            BRS INV PROC FLAG
     C                   PARM                    @@BINV            9 0          BRS INVOICE #
     C                   PARM                    @@BCAM            9 2          BRS CALC AMOUNT
     C                   PARM                    @@NHRF            1            NWS INV H/R FLAG
     C                   PARM                    @@NBLF            1            NWS INV PROC FLAG
     C                   PARM                    @@NINV            9 0          NWS INVOICE #
     C*** BEGIN CHG *** 09/07/99 **************************************
     C**********           PARM EOSYS     @@SYS   1        TYPE OF SERVICE
     C                   PARM                    @@SYS             1            TYPE OF SERVICE
     C*** END   CHG *** 09/07/99 **************************************
     C                   PARM      KKWRUP        @@WRUP            7            WRITE/UPDATE FLAG
     C                   PARM                    ##TMBA            9 2          TIME/MAT BEFORE
     C                   PARM                    ##TMAA            9 2          TIME/MAT AFTER
     C**********           EXSR WRITE1
     C*** BEGIN ADD *** 09/07/99 **************************************
     C                   ELSE
     C*** END   ADD *** 09/07/99 **************************************
     C*
     C* Write record to the Medical Mgmt Bill Transfer file (NAMBP).
     C*
     C*
     C*** BEGIN DEL *** 09/07/99 **************************************
     C**********           MOVE *BLANKS   KKWRUP
     C*** END   DEL *** 09/07/99 **************************************
     C*** BEGIN ADD *** 09/07/99 **************************************
     C                   MOVE      'W'           XXFAEO
     C                   MOVE      'W'           XXFAE1
     C*** END   ADD *** 09/07/99 **************************************
     C                   MOVE      'W'           XXNAMB
     C*
     C                   CALL      'AA001R'
     C                   PARM      SCDBRR        @@RRN             9 0          RRN OF CASE
     C                   PARM      C1EOB#        @@EOB#            9 0          EOB #
     C                   PARM                    @@RCN             3 0          RECON #
     C                   PARM      Q1PGM         @@PGM            10            NAME OF PGM CALLING
     C                   PARM                    @@BAMT            9 2          BRS AMOUNT
     C                   PARM                    @@NAMT            9 2          NWS AMOUNT
     C                   PARM      'H'           @@BHRF            1            BRS INV H/R FLAG
     C                   PARM                    @@BBLF            1            BRS INV PROC FLAG
     C                   PARM                    @@BINV            9 0          BRS INVOICE #
     C                   PARM                    @@BCAM            9 2          BRS CALC AMOUNT
     C                   PARM                    @@NHRF            1            NWS INV H/R FLAG
     C                   PARM                    @@NBLF            1            NWS INV PROC FLAG
     C                   PARM                    @@NINV            9 0          NWS INVOICE #
     C*** BEGIN CHG *** 09/07/99 **************************************
     C**********           PARM EOSYS     @@SYS   1        TYPE OF SERVICE
     C                   PARM                    @@SYS             1            TYPE OF SERVICE
     C*** END   CHG *** 09/07/99 **************************************
     C                   PARM      KKWRUP        @@WRUP            7            WRITE/UPDATE FLAG
     C                   PARM                    ##TMBA            9 2          TIME/MAT BEFORE
     C                   PARM                    ##TMAA            9 2          TIME/MAT AFTER
     C**********           EXSR WRITEM
     C*** END CHG *** 12/16/98 ***************************************
     C*** BEGIN ADD *** 09/07/99 **************************************
     C                   ENDIF
     C*** END   ADD *** 09/07/99 **************************************
     C*
     C                   ENDSR
** Column headings
Claimant Name              Soc Sec #  Sub# Grp  Yr   Case#  Dt of Inj Sts
Soc Sec #   Dt of Inj Claimant Name               Sub# Grp  Yr   Case# Sts
 Sub# Grp  Yr   Case#  Claimant Name              Soc Sec #  Dt of Inj Sts
State Claim #          Claimant Name              Soc Sec #  Dt of Inj Sts
Curr Tx Site           Claimant Name              Soc Sec #  Dt of Inj Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
1077=Case Master Info       YYN
1088=Diagnosis Info         YYN
1099=Case Mgmt Svcs         YYN
11010=Case History          YYN
11111=Case Providers        YYN
11212=Work Information      YYN
11313=Treatment Plan        YYN
11414=DARTS                 YYY
11515=Pre-Cert              YYN
11616=EOMB(s)               YYN
11717=T/M Activity          YYN
11818=EDI Log Info          NNN
11919=Send Subsq 148        NNN
12020=Change Status         YNN
12121=A/R Inquiry           YYN
12222=Payment Limit Buckets NNN
13030=Dept Letters          YNN
18181=WCMS Claims           YYY
18282=Transfer case         YNN
203F3=Exit                  YYY
205F5=Refresh               YNN
206F6=Add                   YNN
211F11=Change View          YYY
212F12=Cancel               NNY
213F13=User Parms           NNN
215F15=Fold/Truncate        NNN
217F17=Subset               YYY
3
