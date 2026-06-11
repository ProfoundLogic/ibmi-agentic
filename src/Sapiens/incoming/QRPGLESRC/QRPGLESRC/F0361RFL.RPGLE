     Hcopyright('(c) 2025 Sapiens, Inc. All rights reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H*  Program: F0361RFL                                            *
     H*  Description: Hospital (UB) Header Information - FLORIDA      *
     H*  Created: 01/05/00  C:56 Project: MC007                       *
     H*****************************************************************
     H*  REVISIONS                                                    *
     H*                                                               *
     H*  07/18/25  C:342 Project MC451                                *
     H*                  NCCI Med Data Call Enhancements              *
     H*                                                               *
     H*  06/27/19 C:079 Project: 152MAP                               *
     H*                 Relax edits if Pbills lite is installed       *
     H*
     H*  04/30/19 C:062 Project: 152WAB
     H*                 Apply Optacomp Mods.
     H*                                                               *
     H*  11/21/17  C:079 114/51197                                    *
     H*                  Set off 94 for Princ Diag Code Editin        *
     H*                                                               *
     H*  01/15/16  C:079 114/39337                                    *
     H*                  Only process Lic # PPO if client 114         *
     H*                                                               *
     H*  10/01/15  C:079 PHW37605                                     *
     H*                  Check ICD-10 Indicator based on date and     *
     H*                  only in add mod                              *
     H*                                                               *
     H*  09/17/15 C:079 Support: 099/37167                            *
     H*                 Add type to ICD-10 Key list for editing       *
     H*                                                               *
     H*  06/03/15 C:079 PROJECT: MC329 Phase III                      *
     H*                 Validate specific indicator for ICD10 codes   *
     H*
     H*  01/08/15 C:296 PROJECT: Fl079                                *
     H*                 Add new field for "County code" on screen and *
     H*                 handel in program.                            *
     H*
     H*  09/08/14 C:280 PROJECT: 147MDC                               *
     H*                 Add new field dispecific and validate         *
     H*
     H*  08/05/13 C:277 PROJECT: MC329 Phase II                       *
     H*                 Add search functionality and Validate ICD10   *
     H*                                                               *
     H*  05/23/13 C:277 PROJECT: MC329                                *
     H*                 Activate the ICD10 fields if ICD10 is active  *
     H*                                                               *
     H*  04/30/13 C:284 Project: 147MAS                               *
     H*                 Call new Api FA782R.                          *
     H*                                                               *
     H*  04/17/13 C:284 Project: 114MID                               *
     H*                 Include rendering information for CSI.        *
     H*                 Changed the copybook to handle 5 screens.     *
     H*                 Rename existing SCR04 to SCR05                *
     H*                                                               *
     H*  11/19/12 C:287 Project: MC367
     H*                 Disable function code F11 for GS access to Bill Entry
     H*                                                               *
     H*   04/05/11 C:257 Support: 71931                               *
     H*                  Assigning RRN to @@RRN after WRITEDBRCDF to  *
     H*                  resolve duplicate records in file FAEHP.     *
     H*                                                               *
     H*   07/23/10 C:227 Support: 73593                               *
     H*                  Validate admission hour for Revision E.      *
     H*                                                               *
     H*   07/12/10 C:079 Support: 73533                               *
     H*                  Do not validate license prefix in this pgm   *
     H*                                                               *
     H*   06/28/10 C:227 PROJECT: FL083                               *
     H*                  PRISS# 41 ADMISSION HR NOT PICKING RIGHT VAL.*
     H*                                                               *
     H*   06/21/10 C:227 PROJECT: FL083                               *
     H*                  Deleted the condition for SCLIC#             *
     H*                                                               *
     H*   04/26/10 C:079 PROJECT: FL083                               *
     H*                  Changed where in42 is set for SCFALC         *
     H*                                                               *
     H*   03/11/10 C:227 PROJECT: FL083                               *
     H*              220 Added Service date in parameter of API.      *
     H*                                                               *
     H*   06/25/09 C:227 SUPPORT 68961                                *
     H*                  Compilation only.                            *
     H*                                                               *
     H*   10/04/08 C:180 SUPPORT 65159                                *
     H*                  Fourth position of FL Phy lic can be blank   *
     H*                                                               *
     H*   09/08/08 C:198 Support: 64286                               *
     H*                  Rejected inactive key values in GTTBID =     *
     H*                  'FLBT'                                       *
     H*                                                               *
     H*   09/05/08 C:198 Support: 64157                               *
     H*                  Operating Physician License# is not working  *
     H*                  in Add mode.                                 *
     H*                                                               *
     H*  08/21/08 C:180 PROJECT:MC294 PRISS 22
     H*                         Do not underline NPI# fields when
     H*                         in display mode. - DSPF changes only.
     H*
     H*  07/15/08 C:180 PROJECT:MC294 Phase II
     H*                         Add SCR02.
     H*
     H*
     H*   03/20/08 C:198                                              *
     H*                  Project: FS297                               *
     H*                  MDS to reject selected prefixes for specialt-*
     H*                  -ies not associate with WC - Scope change    *
     H*                                                               *
     H* 12/10/07 C:198 Project: FL064                                 *
     H*                Expanded license# field to 13 characters.      *
     H*
     H*  11/01/07 C:079 PROJECT: FL066
     H*                 For outpatient surgery require sched/unsched
     H*
     H*  08/29/07  C:180  SUPP 59201
     H*                   Validate SCLIC# based on following formats:
     H*
     H*                   AANNNNNNNNNNN
     H*                   (OR)
     H*                   AAANNNNNNNNNN
     H*                   (OR)
     H*                   AAAANNNNNNNNN
     H*
     H*                   Left justify and space fill to end of field
     H*                   Key alpha prefix and numeric digits of
     H*                   license number
     H*                   Must be valid value
     H*                   DO NOT zero pad numeric portion
     H*                                                               *
     H*  07/24/07 C:079 PROJECT:FL061                                 *
     H*                 USE EDITST TO CHECK FOR EDI REQUIRED          *
     H*
     H*  05/04/07 C:188 PROJECT: 48MWU
     H*                 No changes made, verified functionality
     H*                 of projects 48MFD, 48MWO, and 48MWK.
     H*                                                               *
     H*  06/13/07 C:182   Project: FL070                              *
     H*                   1.Add New Fld                               *
     H*                         'FAC ZIP CODE '                       *
     H*                                                               *
     H*  04/18/07 C:188 PROJECT:FL061                                 *
     H*                 SET EDITST TO ADJ OR POLICY STATE BASED ON    *
     H*                 EOCPST                                        *
     H*
     H*  03/21/07  C:182  Project: FS297
     H*                   Set EDI flag = N if license# is invali
     H*                                                               *
     H*  03/14/07 C:182   Project: FL067                              *
     H*                   1.Add New Fld                               *
     H*                         'Other proc (3)'                      *
     H*                         'Other proc (4)'                      *
     H*                         'Other proc (5)'                      *
     H*                         'Other proc (6)'                      *
     H*                         'Other diag (5)'                      *
     H*                         'Other diag (6)'                      *
     H*                         'Other diag (6)'                      *
     H*                         'Other diag (8)'                      *
     H*                         'Ext. cause (1)'                      *
     H*                         'Ext. cause (2)'                      *
     H*                         'Ext. cause (3)'                      *
     H*                         'Sched/Unsched '                      *
     H*                   2.Renamed original 'SCR03'to 'WDW02'        *
     H*                   3.Add New Screen 'SCR03'                    *
     H*                                                               *
     H*
     H*  12/22/06 C:182 PROJECT: FS294
     H*                 ADDED SEARCH OPTION TO FIELD SCTAPE
     H*
     H*  02/27/06 C:163 SUPPORT: MC010 49588
     H*                 PRINCIPAL PROCEDURE CODE NO LONGER REQUIRED
     H*
     H*  11/11/05  C:163  Project: FL052
     H*                   FL EDI Changes 2005
     H*
     H*  11/08/05 C:163 PROJECT: 48MWK
     H*                 FL EDI CORRECTION LICENSE #
     H*
     H*  03/25/05 C:079 PROJECT: FL050
     H*                 1. CHANGE EDIT ON LICENSE NUMBER
     H*
     H*  04/15/04 C:079 PROJECT: FL046
     H*                 1. CHANGE EDIT ON ELIGIBLE FOR DATA SUBMISSION
     H*                 2. ADD TYPE OF ADMISSION FIELD
     H*
     H*  12/12/03 C:079 PROJECT: FL048
     H*                 -REQUIRE TYPE OF SERVICE
     H*
     H*  08/19/03 C:062 PROJECT: FL045
     H*                 -ONLY ALLOW OPER TYPE FOR PROCEDURE CODES 9
     H*                 -ONLY SEARCH PROCEDURE CODE FILE.
     H*
     H*  04/28/03 C:079 PROJECT: FL043
     H*                 EXPAND LIC# FIELD TO 13A
     H*
     H*  03/12/03 C:079 SUPPORT: MC044 36319
     H*                 CHECK THE STATUS OF DIAG CODES
     H*
     H*  04/30/02 C:079 PROJECT: FL041
     H*                 Lic# err window only display if SCFLG7<>Y
     H*
     H*  04/21/02 C:079 PROJECT: GA015
     H*                 Use state id for Proc Code Method lookup
     H*
     H*  02/15/02 C:079 SUPPORT: MC044 31188
     H*                 If specialty code is blank on SCR01 then it
     H*                 s/b blank on SCR02
     H*
     H*  12/09/01 C:079 PROJECT: FL040
     H*                 Allow 4th digit of Lic# to be alpha
     H*
     H*  10/04/01 C:079 PROJECT: FSMNT
     H*                 Use MOVEL not MOVE for Payment Plan Desciption
     H*
     H*  07/06/01 C:079 PROJECT: FSMNT #196
     H*                 ADD EDIT TO SPECIALTY CODE
     H*
     H*  05/30/01 C:062 PROJECT: FS229
     H*                 POPULATE EOMB# SCREEN FIELD
     H*
     H*  05/09/01  C:079  SUPPORT: MC044 27425
     H*                   CHANGE EDIT ON SCLIC# TO ALLOW 3RD POSITION
     H*                   TO BE ALPHA OR NUMERIC
     H*
     H*  03/30/01  C:062  PROJECT: FL036
     H*                   ADDED LOGIC FOR ERROR WINDOW FOR LICENSE #.
     H*
     H*  01/02/01  C:062  Proj: MCMNT  BASE ONLY
     H*                   WHEN ENTERING SCR01 POSITION TO PAT ACCT #
     H*
     H*  12/05/00  C:079  Proj: MCMNT
     H*                   If SCTAPE=N don't edit on SCLIC#
     H*
     H*  11/22/00  C:062  Proj: FS212
     H*                   COMPILE ONLY.
     H*
     H*  11/08/00 C:62 PROJECT:FS146
     H*                CHANGE 'BLTY' TO 'ZZBT' AND 'FLBT'.
     H*
     H*  11/06/00  C:062  Support FS215
     H*                   SWAP OUT F2310R WITH F2013R.
     H*
     H*  10/13/00  C:062  Support MC238
     H*                   ADD LOGIC TO DEAL WITH NON-SEARCHABLE FIELDS.
     H*
     H*  09/20/00  C:117  PROJECT:FL031
     H*                   CHANGEING THE EDITTING FOR SCLIC#;
     H*                   ADDING SEARCH TO SAME FIELD.
     H*
     H*  08/30/00  C:062  PROJ: MC071 24169
     H*                   ERROR ON TYPE OF BILL FIELD.
     H*
     H*  08/29/00  C:062  PROJ: MCMNT
     H*                   ADD DISCHARGE DATE.
     H*
     H*  08/14/00  C:079  SUPPORT:MC048/#23950
     H*                   1.Change Indicator on SCSCPF,SCSCPT TO 53
     H*                     Instead of 80 which was conflicting
     H*
     H*  06/16/00  C:079  SUPPORT:MC033/#23078
     H*                   1.Change edit on discharge hour (SCDCHR) and
     H*                     admit hour (SCADHR) to allow 00-23 and 99
     H*                   2.Default admit hour to 99 if left blank
     H*                     because if not it will display 00.
     H*
     H*  06/05/00  C:117  PROJECT:MCMNT
     H*                   REVISED SCR02; MOVED DATA SUBM FLAG TO SCR01
     H*
     H*  04/11/00  C:117  PROJECT:FS211
     H*                   ADDITIONAL EDITS TO PBILLS PGMS FOR DATA
     H*                   SUBMISSION (Added CARERR & EDIERR subrs)
     H*
     H*  04/11/00  C:079  PROJ: FS211
     H*                   Allow future dates for Statement covers from
     H*                   and to based on FBAL04 pos 8
     H*
     H*  03/08/00  C:062  PROJ: MCMNT
     H*                   PASS "D" LOS TYPE TO F2010R FOR DIAGNOSIS SCH
     H*                   SO THAT F2010R WILL SUBSET ON "D" LOS TYPES.
     H*                   PASS "O" LOS TYPE TO F2010R FOR ICD9 SEARCH
     H*                   SO THAT F2010R WILL SUBSET ON "O" LOS TYPES.
     H*                                                               *
     H*  02/21/00  C:117  PROJ: FS208
     H*                   ADDED LOGIC TO PROTECT SPECIALTY IF ON A
     H*                   RECON; ALSO SAVES SPECIALTY.
     H*
     H*  02/05/00 C:79  Support: MC048/#21649                         *
     H*                 Remove Date Carrier Received field from SCR01 *
     H*                                                               *
     H*  11/12/99 C:79  MC152                                         *
     H*                 Delete/Change efficiency isssue.              *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS - SCR01
     H*
     H*  08-    ERROR
     H*  09-    INQUIRY MODE
     H*  10-13  ERROR
     H*  16-    DISPLAY NETWORK (ALL SCREENS)
     H*  17-18  CURSOR POSITIONING
     H*  19-20  ERROR WINDOW
     H*  21-23  ERROR
     H*  24-    DATA SUBMISSION PROTECT IF SENT
     H*  25-27  ERROR
     H*  29-    RECON (ALL SCREENS)
     H*  30-    ELIGIBLE FOR DATA SUBMISSION (ALL SCREENS)
     H*  31-38  CURSOR POSITIONING
     H*  39-    CURSOR POSITIONING
     H*  40-    ADD MODE
     H*  41-    CURSOR POSITIONING
     H*  46-    ERROR
     H*  48-    ERROR
     H*  49-    INVALID SEARCH FIELD
     H*  50-55  ERROR
     H*  58-59  ERROR
     H*  60-    GENERAL CHAIN
     H*  61-63  INVALID SEARCH FIELD
     H*  66     ERROR
     H*  70-89  ERROR
     H*  90-    COPYBOOK ERROR
     H*  91-92  ERROR
     H*  98-99  GENERAL CHAIN
     H*
     H*  INDICATORS - SCR02
     H*
     H*  09-    INQUIRY MODE
     H*  16-    DISPLAY NETWORK (ALL SCREENS)
     H*  28-    CURSOR POSITIONING
     H*  29-    RECON (ALL SCREENS)
     H*  30-    ELIGIBLE FOR DATA SUBMISSION (ALL SCREENS)
     H*  31-    CURSOR POSITIONING
     H*  39-    CURSOR POSITIONING
     H*  40-    ADD MODE
     H*  41-    ERROR
     H*  43-46  CURSOR POSITIONING
     H*  60-    GENERAL CHAIN
     H*  64-65  ERROR
     H*  66-79  ERROR
     H*  90-    COPYBOOK ERROR
     H*  98-99  GENERAL CHAIN
     H*****************************************************************
     H*
     FF0361FFL  CF   E             WORKSTN
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F*
     F*  Physical file definition (for maintaining record)
     FFAEUP     UF A E             DISK
     F                                     RENAME(PEU01:DBRCDF)
     F*
     F*  EOMB Transaction File
     FFAEOP     UF   E             DISK
     F*
     F*  WCMC Control File
     FFMCXL     IF   E           K DISK
     F*
     F*  Diagnosis Code Physical File
     FFMDIL     IF   E           K DISK
     F*
     F*  State Code Table File
     FSTSTL     IF   E           K DISK
     F*
     F*  Generic Table File
     FFTABL     IF   E           K DISK
     F                                     IGNORE(PRC01)
     F*
     F*  Bill Entry Control File
     FFMBEL     IF   E           K DISK
     F*
     F*  Provider Master File
     FFMPDL     IF   E           K DISK
     F*
     F*  EOMB Detail EOB/Notes Transaction File
     FFAEEL     IF   E           K DISK
     FFAEUL     UF   E           K DISK
     F                                     INFDS(DBFDSA)
     FFTGTP     IF   E             DISK
     FFTGTL     IF   E           K DISK
     F                                     RENAME(PGT01:PGT02)
     F*  PROCEDURE CODE MASTER FILE
     FFMPRL     IF   E           K DISK
     F*  RESONABLE AND CUSTOMARY FILE
     FFMRCL1    IF   E           K DISK
     F                                     RENAME(PRC01:PRC02)
     FFAESL5    UF   E           K DISK
     FFMPLLFL   IF   E           K DISK
     F*** Beg Add *** 08/05/13 ***************************************
     Ffmicd10l1 if   e           k disk
     F*** End Add *** 08/05/13 ***************************************
     F*** Bed Add *** 01/08/15 ***************************************
     fftccp     if   e             disk
     f                                     RENAME(PCC01:PCC02)
     fftctyl2fl if   e           k disk
     F*** end Add *** 01/08/15 ***************************************
     d*** Beg Chg *** 04/17/13 ****************************************
      *******/COPY SCOPYMT01D
      /COPY SCOPYMT05D
     d*** End Chg *** 04/17/13 ****************************************
     d*** Beg Add *** 06/27/19 ***************************************
      /copy sprmodule
     d isPbillsLite    S               n
     d*** End Add *** 06/27/19 ***************************************
     D @A              S              1    DIM(26) CTDATA PERRCD(26)
     D @N              S              1    DIM(10) CTDATA PERRCD(10)
     D LL              S              1    DIM(11)
     D RVC             S              2    DIM(8) CTDATA PERRCD(8)
     D*
     D*
     D DBFDSA          DS
     D  DBRRNA               397    400B 0
     D*
     D* LICENSE NUMBER DATA STRUCTURE
     D*
     D                 DS
     D  SCLIC#                 1     13
     D  SCLIC1                 1      1
     D  SCLIC2                 2      2
     D  SCLIC3                 3      3
     D  SCLIC4                 4      4
     D  SCLICA                 3     13
     D  SCLICB                 4     13
     D  SCLICC                 5     13
     D*
     D*OPERATING PHYSICIANS LICENSE NUMBER DATA STRUCTURE
     D*
     D                 DS
     D  SCOLIC                 1     13
     D  SCOLC1                 1      1
     D  SCOLC2                 2      2
     D  SCOLC3                 3      3
     D  SCOLC4                 4      4
     D  SCOLCA                 3     13
     D  SCOLCB                 4     13
     D  SCOLCC                 5     13
     D*¢¢                                     5   9 SCOLC5
     D*¢¢                                     4   6 SCOLC6
     D*¢¢                                     3   9 SCOLC7
     D*
     D* ATTENDING PHY LIC NUMBER DATA STRUCTURE
     D*
     D                 DS
     D  SCFALC                 1     15
     D  SCLC1                  1      1
     D  SCLC2                  2      2
     D  SCLC3                  3      3
     D  SCLC4                  4      4
     D  SCLCA                  3     13
     D  SCLCB                  4     13
     D  SCLCC                  5     13
     D*
     D* BILL TYPE DATA STRUCTURE
     D*
     D                 DS
     D  SCBLTY                 1      3
     D  SCBLT1                 1      1
     D  SCBLT2                 2      2
     D  SCBLT3                 3      3
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Hospital (UB) Header-
     D                                      Information')
     D COMAPI          C                   CONST('FA007R')
     D*** Beg Del *** 04/17/13 ****************************************
     D******* #OFSCR          C                   CONST(3)
     D*** End Del *** 04/17/13 ****************************************
     D ZZLIC#          C                   CONST('ZZ99999999999')

     D***Beg Add *** 05/23/13 ***************************************
     D SA015R          pr                  extpgm('SA015R')
     D @@rtcd                         1
     D*
     D xxicd10         s              1
     D @@rtcd          s              1
     D***End Add *** 05/23/13 ***************************************
     D*** Beg Add *** 08/05/13 ***************************************
     D RtvICD10Code    pr                  extpgm('F2554R')
     D incall                         1
     D inKC                           1
     D inKL                           1
     D outICD10                       7
     D inLOST                         1
     D*
     D incall          s              1
     D inKC            s              1
     D inKL            s              1
     D outICD10        s              7
     D inLOST          s              1
     D kkicd10         s                   like(iccode)
     D*** End Add *** 08/05/13 ***************************************
     D*** Beg Add *** 01/08/15 ***************************************
     DGetCountyCd      pr                  EXTPGM('F2360R')
     D incall                         1
     D inKC                           1
     D inKL                           1
     D ##rrn                          9  0
     D ##st                           2
     D*
     DvarCntyCd        s              2A
     DdecCtyMod        s                   like(EOCtyMod)
     D*** End Add *** 01/08/15 ***************************************
     D*** Beg Add *** 07/18/25 ***************************************
     D invalidPos      s              2  0
     D*** End Add *** 07/18/25 ***************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          for FAEUP
     C                   PARM                    @@RRN2            9 0          for FAEOP
     C                   PARM                    @@NWCD            5
     C                   PARM                    @@CHRG            9            TOTAL CHARGE
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*  Key List for WCMC Control File
     C*
     C     CXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##SUB#
     C                   KFLD                    ##FND
     C*
     C*  Key List for Diagnosis Code File
     C*
     C     DIKY          KLIST
     C                   KFLD                    KKICD9
     C                   KFLD                    KKLOST
     C*
     C** Key List for State Code Table File
     C*
     C     STKY          KLIST
     C                   KFLD                    ##KEY
     C*
     C** Key List for Specialty Code File
     C*
     C     SPKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##KEY
     C*
     C** Key List for Fee Schedule Control File
     C*
     C*
     C** Key List for Bill Entry Control FIle
     C*
     C     BEKY          KLIST
     C                   KFLD                    KKAST
     C                   KFLD                    KKSYS
     C                   KFLD                    KKBTYP
     C                   KFLD                    KKSTYP
     C** Key List for Provider Master File
     C*
     C     PDKY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C* KEY LIST FOR Header EOB/Notes File
     C*
     C     EEKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C* KEY LIST FOR EOMB PHYS/HCFA HEADER FILE
     C*
     C     EUKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C* Key list for Generic Table File
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*
     C     GTKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID
     C*
     C* KEY LIST FOR PROCEDURE CODE MASTER FILE
     C*
     C     PRKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKCPT4
     C                   KFLD                    KKEFDT
     C*
     C     PRKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKCPT4
     C*
     C* KEY LIST FOR REASONABLE AND CUSTOMARY FILE
     C*
     C     RCKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKCPT4
     C*
     C*  KEY LIST FOR EOMB CHANGE STATUS TRANSACTION FILE
     C*
     C     ESKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKBLST
     C                   KFLD                    KKRCN
     C*
     C     ESKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKBLST
     C*
     C*
     C* FL Provider Lic#
     C*
     C     PLKY          KLIST
     C                   KFLD                    KKLIC#           13
     C*** Beg Add *** 08/05/13 ***************************************
     C*** KEY LIST FOR DIAGNOSIS CODE MASTER FILE
     C     fmicd10l1key  klist
     C                   kfld                    kkicd10
     C*** End Add *** 08/05/13 ***************************************
     C*** Beg Add *** 09/17/15 ***************************************
     C                   kfld                    kkLost
     C*** End Add *** 09/17/15 ***************************************
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    CXSUB#        ##SUB#
     C     *LIKE         DEFINE    DIICD9        ##ICD9
     C     *LIKE         DEFINE    DILOST        ##LOST
     C     *LIKE         DEFINE    DIICD9        KKICD9
     C     *LIKE         DEFINE    DILOST        KKLOST
     C     *LIKE         DEFINE    STKEY         ##KEY
     C     *LIKE         DEFINE    EUCO#         KKCO#
     C     *LIKE         DEFINE    EUSUB#        KKSUB#
     C     *LIKE         DEFINE    EUFND         KKFND
     C     *LIKE         DEFINE    EUFYR         KKFYR
     C     *LIKE         DEFINE    EUCASE        KKCASE
     C     *LIKE         DEFINE    BEAST         KKAST
     C     *LIKE         DEFINE    BESYS         KKSYS
     C     *LIKE         DEFINE    BEBTYP        KKBTYP
     C     *LIKE         DEFINE    BESTYP        KKSTYP
     C     *LIKE         DEFINE    PDFEIN        KKFEIN
     C     *LIKE         DEFINE    PDMOD         KKMOD
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C     *LIKE         DEFINE    EUICN         HLICN
     C     *LIKE         DEFINE    EUDCN         HLDCN
     C     *LIKE         DEFINE    EUSCPF        HLSCPF
     C     *LIKE         DEFINE    EUSCPT        HLSCPT
     C     *LIKE         DEFINE    EUADDT        HLADDT
     C     *LIKE         DEFINE    EUADDT        HLSYDT
     C     *LIKE         DEFINE    PRCPT4        KKCPT4
     C     *LIKE         DEFINE    PREFDT        KKEFDT
     C     *LIKE         DEFINE    SCPCM         HLPCM
     C     *LIKE         DEFINE    SCPDSC        HLDESC
     C     *LIKE         DEFINE    ESBLST        KKBLST
     C     *LIKE         DEFINE    EORCN         KKRCN
     C     *LIKE         DEFINE    EOCRDT        HLCRDT
     C     *LIKE         DEFINE    EOCRDT        HLCSDT
     C     *LIKE         DEFINE    EOCRDT        HLRRDT
     C     *LIKE         DEFINE    EOCRDT        HLRSDT
     C     *LIKE         DEFINE    EUDCDT        HLDCDT
     C     *LIKE         DEFINE    EOAST         EDITST
     C     *LIKE         DEFINE    EOFRDT        @@SDTE
     C*
     C*** Beg Chg *** 04/17/13 ****************************************
     C*******/COPY SCOPYMT01C
     C/COPY SCOPYMT05C
     C*** End Chg *** 04/17/13 ****************************************
     C*
     C*================================================================
     C* DATSBR - Date check subrountine
     C*================================================================
     C     DATSBR        BEGSR
     C* THIS SUBROUTINE IS CALLED AFTER EVERY SEARCH TO PROPERLY
     C* FORMAT ALL DATES ON SCREEN
     C*
     C                   Z-ADD     SCSCPF        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCSCPF
     C*
     C                   Z-ADD     SCSCPT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCSCPT
     C*
     C                   Z-ADD     SCADDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCADDT
     C                   Z-ADD     SCDCDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDCDT
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*
     C*================================================================
     C* DSPSC2- Set indicators for displaying fields on screen 2
     C*================================================================
     C     DSPSC2        BEGSR
     C*
     C* Determine whethor to display the Facility License Number
     C* and if it should be in high intensity
     C*
     C     XXSCR2        IFEQ      *ON
     C     XXREVI        ANDGE     'E'
     C*
     C     @@MODE        OREQ      'D'
     C     XXREVI        ANDGE     'E'
     C*
     C                   SETON                                        48
     C*
     C                   ELSE
     C                   SETOFF                                       48
     C*
     C                   ENDIF
     C*
     C                   MOVEL     SCBLTY        XXBLTY            3
     C     2             SUBST     XXBLTY:1      XBLTY             2
     C*
     C     XXSCR2        IFEQ      *ON
     C     XXREVI        ANDGE     'E'
     C*
     C     XBLTY         IFEQ      '83'                                         AMBULATORY
     C     XBLTY         OREQ      '21'                                         NURSING HOME
     C     XBLTY         OREQ      '22'                                         NURSING HOME
     C     XBLTY         OREQ      '23'                                         NURSING HOME
     C     XBLTY         OREQ      '28'                                         NURSING HOME
     C     XBLTY         OREQ      '32'                                         HOME HEALTH
     C     XBLTY         OREQ      '33'                                         HOME HEALTH
     C     XBLTY         OREQ      '34'                                         HOME HEALTH
     C*
     C                   SETON                                        97
     C*
     C                   ELSE
     C                   SETOFF                                       97
     C*
     C                   ENDIF                                                  XXBLTY=83
     C*
     C                   ENDIF                                                  XXSCR2 = ON
     C*
     C                   ENDSR
     C*================================================================
     C* CARERR - Date check subrountine
     C*================================================================
     C     CARERR        BEGSR
     C*
     C     CARTAG        TAG
     C*
     C     EORCN         IFEQ      0
     C                   SETON                                        19
     C                   SETOFF                                       20
     C                   Z-ADD     EOCRDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCRDT
     C                   Z-ADD     EOCSDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCSDT
     C*** Beg Chg *** 04/17/13 *************************************************
     C*******            EXFMT     SCR04
     C                   EXFMT     SCR05
     C*** End Chg *** 04/17/13 *************************************************
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C* DATE CARR RCVD CANNOT BE BLANK
     C*
     C     SCCRDT        IFEQ      *ZEROS
     C                   SETON                                        9010
     C                   GOTO      CARTAG
     C                   ENDIF
     C*
     C* DATE RECIEVED MUST BE VALID
     C*
     C     SCCRDT        IFNE      *ZEROS
     C                   Z-ADD     SCCRDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9021
     C                   GOTO      CARTAG
     C                   ELSE
     C                   MOVEL     SRDTO         SCCRDT
     C                   Z-ADD     SCCRDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLCRDT
     C                   ENDIF                                                  SRERR = '1'
     C*
     C                   ENDIF                                                  SCRRDT NE *ZEROS
     C*
     C* Received date cannot be less than DOI
     C*
     C     *IN90         IFEQ      *OFF
     C     HLCRDT        ANDNE     *ZEROS
     C     HLCRDT        ANDLT     EOACDT
     C                   SETON                                        9022
     C                   GOTO      CARTAG
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C* Received date cannot be less than EOMB TO DATE
     C*
     C     *IN90         IFEQ      *OFF
     C     HLCRDT        ANDNE     *ZEROS
     C     HLCRDT        ANDLT     EOTODT
     C     ##SDTS        ANDNE     'Y'
     C                   SETON                                        9023
     C                   GOTO      CARTAG
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C* Received date cannot be greater than system date
     C*
     C     *IN90         IFEQ      *OFF
     C     HLCRDT        ANDNE     *ZEROS
     C     HLCRDT        ANDGT     HLSYDT
     C                   SETON                                        9025
     C                   GOTO      CARTAG
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C*  IF ADJ STATE IS KY, SENT DT CANNOT BE BLANK
     C*
     C     EDITST        IFEQ      'KY'
     C     SCCSDT        ANDEQ     *ZEROS
     C                   SETON                                        9008
     C                   GOTO      CARTAG
     C                   ENDIF
     C*
     C* Received date cannot be greater than sent date
     C*
     C     *IN90         IFEQ      *OFF
     C     SCCRDT        IFNE      *ZEROS
     C     SCCSDT        ANDNE     *ZEROS
     C*
     C                   Z-ADD     SCCSDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLCSDT
     C*
     C     HLCSDT        IFGT      HLCRDT
     C                   SETON                                        9026
     C                   GOTO      CARTAG
     C                   ENDIF                                                  HLCRDT GT HLRSDT
     C*
     C                   ENDIF                                                  SCCRDT NE *ZEROS
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCCSDT        ANDNE     *ZEROS
     C                   Z-ADD     SCCSDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9027
     C                   GOTO      CARTAG
     C                   ELSE
     C                   MOVEL     SRDTO         SCCSDT
     C*
     C                   Z-ADD     SCCSDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLCSDT
     C                   ENDIF                                                  SRERR = '1'
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C* Sent date cannot be less than DOI
     C*
     C     *IN90         IFEQ      *OFF
     C     HLCSDT        ANDNE     *ZEROS
     C     HLCSDT        ANDLT     EOACDT
     C                   SETON                                        9013
     C                   GOTO      CARTAG
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C* Sent date cannot be less than EOMB TO DATE
     C*
     C     *IN90         IFEQ      *OFF
     C     HLCSDT        ANDNE     *ZEROS
     C     HLCSDT        ANDLT     EOTODT
     C     ##SDTS        ANDNE     'Y'
     C                   SETON                                        9012
     C                   GOTO      CARTAG
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C* Sent date cannot be greater than system date
     C*
     C     *IN90         IFEQ      *OFF
     C     HLCSDT        ANDNE     *ZEROS
     C     HLCSDT        ANDGT     HLSYDT
     C                   SETON                                        9011
     C                   GOTO      CARTAG
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C     @@RRN2        CHAIN     PEO01                              99
     C                   MOVE      SCCRDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EOCRDT
     C                   MOVE      EOCSDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EOCSDT
     C*** Beg Add *** 07/18/25 ***************************************
     C                   MOVEL     SCFZIP        EOFLIC
     C                   MOVE      SCZIP         EOFLIC
     C*** End Add *** 07/18/25 ***************************************
     C*
     C                   UPDATE    PEO01
     C*
     C                   ELSE                                                   *INKL=OFF
     C*
     C                   SETON                                        90
     C                   GOTO      CAREND
     C*
     C                   ENDIF                                                  *INKL=*OFF
     C*
     C                   ELSE                                                   RCN=0
     C*
     C                   SETON                                        20
     C                   SETOFF                                       19
     C                   Z-ADD     ESRRDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRRDT
     C                   Z-ADD     ESRSDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRSDT
     C*** Beg Chg *** 04/17/13 ****************************************
     C*******            EXFMT     SCR04
     C                   EXFMT     SCR05
     C*** End Chg *** 04/17/13 ****************************************
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C*  CARR RCVD DT CANNOT BE BLANK
     C*
     C     SCRRDT        IFEQ      *ZEROS
     C                   SETON                                        9010
     C                   GOTO      CARTAG
     C                   ENDIF
     C*
     C* DATE RECIEVED MUST BE VALID
     C*
     C     SCRRDT        IFNE      *ZEROS
     C                   Z-ADD     SCRRDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9021
     C                   GOTO      CARTAG
     C                   ELSE
     C                   MOVEL     SRDTO         SCRRDT
     C                   Z-ADD     SCRRDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLRRDT
     C                   ENDIF                                                  SRERR = '1'
     C*
     C                   ENDIF                                                  SCRRDT NE *ZEROS
     C*
     C* Received date cannot be less than DOI
     C*
     C     *IN90         IFEQ      *OFF
     C     HLRRDT        ANDNE     *ZEROS
     C     HLRRDT        ANDLT     EOACDT
     C                   SETON                                        9022
     C                   GOTO      CARTAG
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C* Received date cannot be less than EOMB TO DATE
     C*
     C     *IN90         IFEQ      *OFF
     C     HLRRDT        ANDNE     *ZEROS
     C     HLRRDT        ANDLT     EOTODT
     C     ##SDTS        ANDNE     'Y'
     C                   SETON                                        9023
     C                   GOTO      CARTAG
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C* Received date cannot be greater than system date
     C*
     C     *IN90         IFEQ      *OFF
     C     HLRRDT        ANDNE     *ZEROS
     C     HLRRDT        ANDGT     HLSYDT
     C                   SETON                                        9025
     C                   GOTO      CARTAG
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C*  IF ADJ STATE IS KY, SENT DT CANNOT BE BLANK
     C*
     C     EDITST        IFEQ      'KY'
     C     SCCSDT        ANDEQ     *ZEROS
     C                   SETON                                        9008
     C                   GOTO      CARTAG
     C                   ENDIF
     C*
     C* Received date cannot be greater than sent date
     C*
     C     *IN90         IFEQ      *OFF
     C     SCRRDT        IFNE      *ZEROS
     C     SCRSDT        ANDNE     *ZEROS
     C*
     C                   Z-ADD     SCRSDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLRSDT
     C*
     C     HLRSDT        IFGT      HLRRDT
     C                   SETON                                        9026
     C                   GOTO      CARTAG
     C                   ENDIF                                                  HLCRDT GT HLRSDT
     C*
     C                   ENDIF                                                  SCCRDT NE *ZEROS
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCRSDT        ANDNE     *ZEROS
     C                   Z-ADD     SCRSDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9027
     C                   GOTO      CARTAG
     C                   ELSE
     C                   MOVEL     SRDTO         SCCSDT
     C*
     C                   Z-ADD     SCRSDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLRSDT
     C                   ENDIF                                                  SRERR = '1'
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C* Sent date cannot be less than DOI
     C*
     C     *IN90         IFEQ      *OFF
     C     HLRSDT        ANDNE     *ZEROS
     C     HLRSDT        ANDLT     EOACDT
     C                   SETON                                        9013
     C                   GOTO      CARTAG
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C* Sent date cannot be less than EOMB TO DATE
     C*
     C     *IN90         IFEQ      *OFF
     C     HLRSDT        ANDNE     *ZEROS
     C     HLRSDT        ANDLT     EOTODT
     C     ##SDTS        ANDNE     'Y'
     C                   SETON                                        9012
     C                   GOTO      CARTAG
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C* Sent date cannot be greater than system date
     C*
     C     *IN90         IFEQ      *OFF
     C     HLRSDT        ANDNE     *ZEROS
     C     HLRSDT        ANDGT     HLSYDT
     C                   SETON                                        9011
     C                   GOTO      CARTAG
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C     ESKY1         SETGT     PES01
     C     ESKY2         READPE    PES01                                  60
     C                   MOVE      SCRRDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ESRRDT
     C                   MOVE      SCRSDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ESRSDT
     C*
     C                   UPDATE    PES01
     C*
     C                   ELSE                                                   *INKL=OFF
     C*
     C                   SETON                                        90
     C                   GOTO      CAREND
     C*
     C                   ENDIF                                                  *INKL=*OFF
     C*
     C                   ENDIF                                                  RCN=0
     C*
     C     CAREND        ENDSR
     C*================================================================
     C*  EDIERR - EDI ERROR SR FOR FUTURE DATES
     C*================================================================
     C     EDIERR        BEGSR
     C*
     C     EDITAG        TAG
     C*
     C                   MOVEL     EUEDI         SCTAPE
     C                   EXFMT     SCR03
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C     SCTAPE        IFEQ      *BLANKS
     C                   SETON                                        9017
     C                   GOTO      EDITAG
     C                   ENDIF                                                  SCEDI IFEQ BLANKS
     C*
     C     SCTAPE        IFNE      *BLANKS
     C     SCTAPE        ANDNE     'Y'
     C     SCTAPE        ANDNE     'N'
     C     *IN24         ANDEQ     *OFF
     C                   SETON                                        9018
     C                   GOTO      EDITAG
     C                   ENDIF                                                  SCEDI IFNE BLANKS
     C*
     C     *IN90         IFEQ      *OFF
     C     EUKY          CHAIN     PEU01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     SCTAPE        EUEDI
     C                   UPDATE    PEU01
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  *IN90=*OFF
     C                   ENDIF                                                  *INKL=*OFF
     C*
     C     EDIEND        ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C                   MOVEL     *OFF          UPDATE
    C*** Beg Add *** 06/27/19 ****************************************
     C                   if        not isPbillsLite
    C*** End Add *** 06/27/19 ****************************************
     C*
     C** SPECIALTY CODE MUST BE VALID IF ENTERED AND NOT A RECON
     C*
     C     SCSPCD        IFNE      *BLANKS
     C     *IN29         ANDEQ     *OFF
     C*
     C                   MOVEL(P)  SCSPCD        ##KEY
     C     SPKY          CHAIN     PSP01                              99
     C*
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9070
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C                   ENDIF
      /Free
       // Beg Add ***** 01/08/15 ********************************
       // County code required for HO and HS type of service.

          indCcder1 = *off;
          If (SCicn = 'HO' or SCicn = 'HS') and scCntyCd = *blanks;
             indCcder1 = *on;
             *in90 = *on;
             LeaveSr;
          Endif;

       // End Add ***** 01/08/15 ********************************
      /End-Free
     C******ZIP CODE MUST BE ENTERED & CONTAIN ONLY NUMERIC VALUE
     C*
     C     SCFZIP        IFEQ      *ZEROS
     C     *IN29         ANDEQ     *OFF
     C*
     C                   SETON                                        9025
     C                   GOTO      CK1END
     C*
     C                   ENDIF
       //*** Beg add ***** 07/18/25 ***********************************
       If isModEnabled('NCCI Med Data Call');
          If scfzip = *zeros and EOrcn = 0;
             *in25 = *on;
             *in90 = *on;
             leavesr;
          Endif;
       Endif;
       //*** End add ***** 07/18/25 ************************************
     C*
     C*
     C* LOAD SPECIALTY CODE DESCRIPTION
     C*
     C                   MOVEL     SCSPCD        ##KEY
     C     SPKY          CHAIN     PSP01                              99
     C  N99              MOVEL     SPDESC        SCSCDS
     C   99              CLEAR                   SCSCDS
     C*
     C* SET FLAG TO INDICATE ERROR CHECK HAS BEEN DONE ON SCREEN ONE
     C* SO NEXT TIME 1VDBSC IS CALLED WE KNOW WE ARE ON SCR02
     C*
     C                   MOVE      *ON           XXSCR2
     C*
     C*
     C* PRINCIPAL DIAGNOSIS CODE IS REQUIRED
     C*
     C     SCDIA1        IFEQ      *BLANKS
     C                   SETON                                        9078
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCDIA1 = *BLANKS
     C*
     C** VALIDATE PRINCIPAL DIAGNOSIS CODE IF ENTERED
     C*
     C                   MOVE      'D'           KKLOST
     C     SCDIA1        IFNE      *BLANKS
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxicd10 = '1' and scICdInd ='Y';
          kkICd10 = scDIa1;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If not %found(fmicd10l1)
                or ICstat = 'I';
                *in90 = *on;
                *in71 = *on;
                leavesr;
             Endif;
       // Beg Add ***** 06/03/15 *******************************************
              *in94 = *off;
           if scdia1 <> *blanks
              and ICvld = '0';
              *in90 = *on;
              *in94 = *on;
              leavesr;
           endif;
       // End Add ***** 06/03/15 *******************************************
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   SCDIA1        KKICD9
     C                   MOVEL(P)  SCDIA1        KKICD9
     C*** End Chg *** 05/23/13 ***************************************
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        9071
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   ENDIF                                                  SCDIA1 NE BLANKS
     C*
     C*** Beg Add *** 09/08/14 ****************************************
     C*
      /Free
              *in90 = *off;
              *in94 = *off;

           if scdia1 <> *blanks
       // Beg Add ***** 06/03/15 *******************************************
              and scICdInd <> 'Y'
       // End Add ***** 06/03/15 *******************************************
              and dispecific = '0';
              *in90 = *on;
              *in94 = *on;
              leavesr;
           endif;

      /End-Free
     C*
     C*** End Add *** 09/08/14 ****************************************
     C*
     C*
     C* TYPE OF BILL CANNOT BE BLANK
     C*
     C     SCBLTY        IFEQ      *BLANKS
     C                   SETON                                        9072
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCBLTY = *BLANKS
     C*
     C* VALIDATE TYPE OF BILL
     C*
     C                   MOVEL(P)  BTTBID        KKTBID
     C                   MOVEL(P)  SCBLTY        KKKEY
     C     GTKY          SETLL     PGT02
     C     GTKY          READE     PGT02                                  99
     C                   MOVE      'Y'           ##ERR             1
     C     *IN99         DOWEQ     *OFF
     C     GTSTAT        IFEQ      'A'
     C                   MOVE      'N'           ##ERR
     C                   LEAVE
     C                   ENDIF                                                  GTSTAT = 'A'
     C     GTKY          READE     PGT02                                  99
     C                   ENDDO                                                  *IN99 = *OFF
     C     ##ERR         IFEQ      'Y'
     C                   SETON                                        9073
     C                   GOTO      CK1END
     C                   ENDIF                                                  ##FLG = 'Y'
     C     SCREVI        IFLT      'E'
     C                   MOVEL     SCBLTY        ##BT              3
     C     2             SUBST     ##BT          XXBT              2
     C     XXBT          LOOKUP    RVC                                    60
     C     *IN60         IFEQ      '1'
     C                   SETON                                        906731
     C                   ENDIF
     C                   ENDIF
     C*
     C* STATEMENT COVERS FROM DATE IS REQUIRED
     C*
     C     SCSCPF        IFEQ      *ZEROS
     C                   SETON                                        9074
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCSCPF = *ZEROS
     C*
     C* VALIDATE THE FROM STATEMENT COVERS DATE
     C*
     C                   Z-ADD     SCSCPF        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9076
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     SRDTO         SCSCPF
     C                   ENDIF
     C*
     C* STATEMENT COVERS TO DATE IS REQUIRED
     C*
     C     SCSCPT        IFEQ      *ZEROS
     C                   SETON                                        9075
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCSCPT = *ZEROS
     C*
     C* VALIDATE THE STATEMENT COVERS TO DATE
     C*
     C                   Z-ADD     SCSCPT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9077
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     SRDTO         SCSCPT
     C                   ENDIF
     C*
     C* ADDMISSION DATE IS REQUIRED
     C*
     C     SCADDT        IFEQ      *ZEROS
     C                   SETON                                        9048
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCADDT = *ZEROS
     C*
     C* VALIDATE ADMISSION DATE IF ENTERED
     C*
     C     SCADDT        IFNE      *ZEROS
     C                   Z-ADD     SCADDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9054
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     SRDTO         SCADDT
     C                   ENDIF
     C                   ENDIF                                                  SCADDT NE *ZEROS
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLSYDT
     C*
     C                   Z-ADD     SCSCPF        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLSCPF
     C*
     C                   Z-ADD     SCSCPT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLSCPT
     C*
     C                   Z-ADD     SCADDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLADDT
     C*
     C* STATEMENT COVERS TO AND FROM DATES CANNOT BE LESS THAN DOI
     C*
     C     HLSCPF        IFNE      *ZEROS
     C     HLSCPF        ANDLT     EOACDT
     C                   SETON                                        9085
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLSCPF NE *ZEROS
     C*
     C     HLSCPT        IFNE      *ZEROS
     C     HLSCPT        ANDLT     EOACDT
     C                   SETON                                        9086
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLSCPT NE *ZEROS
     C*
     C* STATEMENT COVERS TO AND FROM DATES CANNOT BE GREATER THAN SYSTEM DATE
     C* UNLESS SYSTEM FLAG ALLOWS                                      M DATE
     C     ##SDTS        IFNE      'Y'
     C*
     C     HLSCPF        IFNE      *ZEROS
     C     HLSCPF        ANDGT     HLSYDT
     C                   SETON                                        9087
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLSCPF NE *ZEROS
     C*
     C     HLSCPT        IFNE      *ZEROS
     C     HLSCPT        ANDGT     HLSYDT
     C                   SETON                                        9088
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLSCPF NE *ZEROS
     C*
     C* ADMISSION DATE CANNOT BE GREATER THAN SYSTEM DATE
     C*
     C     HLADDT        IFNE      *ZEROS
     C     HLADDT        ANDGT     HLSYDT
     C                   SETON                                        9050
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLADDT NE *ZEROS
     C*  Give an error window if the date is a future date and the
     C*  data submission flag is 'Y'.
     C*
     C*
     C                   ENDIF                                                  ##SDTS IFNE
     C*
     C* STATEMENT COVERS TO DATE MUST BE < STATEMENT COVERS TO DATE
     C*
     C     HLSCPF        IFGT      HLSCPT
     C                   SETON                                        9053
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C* DATE CARRIER RECEIVED CANNOT BE < DATE STATEMENT COVERS TO
     C*
     C* This is all conditioned on weather or not the EDI flag is on
     C*
     C     SCTAPE        IFNE      'N'
     C     SCTAPE        ANDNE     ' '
     C     EORCN         IFEQ      0
     C     HLSCPT        ANDGT     EOCRDT
     C                   EXSR      CARERR
     C     ##SDTS        IFNE      'Y'
     C                   SETON                                        90
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF                                                  EORCN=0
     C*
     C*     (If recon > 0)  FAESP
     C*
     C     EORCN         IFGT      0
     C                   MOVE      'R'           KKBLST
     C     ESKY1         SETGT     PES01
     C     ESKY2         READPE    PES01                                  60
     C     *IN60         IFEQ      *OFF
     C     HLSCPT        ANDGT     ESRRDT
     C                   EXSR      CARERR
     C     ##SDTS        IFNE      'Y'
     C                   SETON                                        90
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  EORCN>0
     C                   ENDIF                                                  EUEDI=Y
     C*
     C**ADDMISSION DATE IS REQUIRED
     C**VALIDATE ADMISSION DATE IF ENTERED
     C*
     C* ADMISSION DATE CANNOT BE LESS THAN ACCIDENT DATE
     C*
     C     HLADDT        IFNE      *ZEROS
     C     HLADDT        ANDLT     EOACDT
     C                   SETON                                        9089
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLADDT NE *ZEROS
     C*
     C**ADMISSION DATE CANNOT BE GREATER THAN SYSTEM DATE
     C*
     C* ADMISSION DATE CANNOT BE GREATER THAN STATEMENT COVERS FROM DATE
     C*
     C     HLADDT        IFNE      *ZEROS
     C     HLADDT        ANDGT     HLSCPF
     C                   SETON                                        9055
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLADDT NE *ZEROS
     C*
     C* IF SCREVI >= E AND FIRST DIGIT IN SCBLTY EQUALS 1 OR 2 AND
     C* SECOND DIGIT EQUALS 1, 2, OR 8, THEN THIS FIELD CANNOT BE BLANK
     C*
     C                   MOVE      ' '           XFLG              1
     C*
     C     SCREVI        IFGE      'E'
     C                   MOVEL     SCBLTY        XXBLTY            3
     C     1             SUBST     XXBLTY        XBLT              1
     C     1             SUBST     XXBLTY:2      YBLT              1
     C     XBLT          IFEQ      '1'
     C     XBLT          OREQ      '2'
     C     YBLT          IFEQ      '1'
     C     YBLT          OREQ      '2'
     C     YBLT          OREQ      '8'
     C*
     C                   MOVE      '1'           XFLG
     C*
     C                   ENDIF                                                    SECOND BIT
     C                   ENDIF                                                    FIRST BIT
     C*
     C                   ENDIF                                                    SCADHR EDIT
     C*
     C*
     C* ADMIT HOUR AND DISCHARGE HOUR MUST BE 00-23 OR 99
     C* IF ENTERED.
     C*
     C                   SETOFF                                       565797
     C     XFLG          IFEQ      '1'
     C     SCREVI        ANDGE     'E'
     C     SCADHR        ANDEQ     *BLANKS
     C                   SETON                                        905797
     C                   SETOFF                                       56
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C     XFLG          IFEQ      ' '
     C     SCREVI        ANDGE     'E'
     C     SCADHR        ANDNE     *BLANKS
     C                   SETON                                        9097
     C                   SETOFF                                       5657
     C                   GOTO      CK1END
     C                   ENDIF
     C     ' '           SCAN      SCADHR                                 98
     C     *IN98         IFEQ      *ON
     C     XFLG          ANDEQ     '1'
     C     SCADHR        ORLT      '00'
     C     XFLG          ANDEQ     '1'
     C     SCADHR        ORGT      '23'
     C     XFLG          ANDEQ     '1'
     C                   SETON                                        9051
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCADHR LT 1
     C*
     C* TYPE OF ADMISSION IS REQUIRED.
     C*
     C     SCTAPE        IFNE      'Y'
     C     SCTAPE        ANDNE     'N'
     C     SCTAPE        ANDNE     ' '
     C*
     C     SCADTY        IFEQ      *BLANKS
     C                   SETON                                        9091
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCADTY EQ BLANK
     C*
     C* VALIDATE TYPE OF ADMISSION
     C*
     C                   MOVEL(P)  'FLTA'        KKTBID
     C                   MOVEL(P)  SCADTY        KKKEY
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *ON
     C     GTSTAT        OREQ      'I'
     C                   SETON                                        9092
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C*
     C                   ENDIF
     C*
     C* VALIDATE DISCHARGE DATE IF ENTERED
     C*
     C     SCDCDT        IFNE      *ZEROS
     C                   Z-ADD     SCDCDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9058
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     SRDTO         SCDCDT
     C                   ENDIF
     C                   ENDIF                                                  SCDCDT NE *ZEROS
     C*
     C* DISCHARGE DATE CANNOT BE LESS THAN ADMISSION DATE
     C*
     C*
     C                   Z-ADD     SCDCDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLDCDT
     C*
     C     HLDCDT        IFNE      *ZEROS
     C     HLDCDT        ANDLT     HLADDT
     C                   SETON                                        9059
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLADDT NE *ZEROS
     C*
     C* DISCHARG HOUR IS REQUIRED
     C*
     C     XFLG          IFEQ      '1'
     C     SCREVI        ANDGE     'E'
     C     SCDCHR        ANDEQ     *BLANKS
     C                   SETON                                        9056
     C                   SETOFF                                       5797
     C                   GOTO      CK1END
     C                   ENDIF
     C     XFLG          IFEQ      ' '
     C     SCREVI        ANDGE     'E'
     C     SCDCHR        ANDNE     *BLANKS
     C                   SETON                                        9057
     C                   SETOFF                                       5697
     C                   GOTO      CK1END
     C                   ENDIF
     C     SCDCHR        IFEQ      *BLANKS
     C     XFLG          ANDEQ     '1'
     C                   SETON                                        9066
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCDCHR = *ZEROS
     C*
     C     ' '           SCAN      SCDCHR                                 98
     C     *IN98         IFEQ      *ON
     C     XFLG          ANDEQ     '1'
     C     SCDCHR        ORLT      '00'
     C     XFLG          ANDEQ     '1'
     C     SCDCHR        ORGT      '23'
     C     SCDCHR        ANDNE     '99'
     C     XFLG          ANDEQ     '1'
     C                   SETON                                        9052
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCDCHR LT 00
     C**DATE*CARRIER*RECEIVED IS REQUIRED
     C**VALIDATE*THE*DATE*CARRIER RECIEVED
     C**DATE*CARRIER*RECEIVED CANNOT BE LESS THAN DOI
     C**DATE*CARRIER*RECEIVED CANNOT BE LESS THAN STATEMENT COVERS TO
     C*
     C* VALIDATE BOTH TYPES OF SERVICE
     C*
     C     SCICN         IFNE      *BLANKS
     C*
     C                   MOVE      'SVTY'        KKTBID
     C                   MOVEL(P)  SCICN         KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9083
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C*
     C                   ENDIF                                                  SCICN NE BLANKS
     C*
     C     SCDCN         IFNE      *BLANKS
     C                   MOVE      'SVTZ'        KKTBID
     C                   MOVEL(P)  SCDCN         KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9084
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCDCN NE *BLANKS
     C*
     C* TYPE OF SERVICE IS REQUIRED
     C*
     C     SCICN         IFEQ      *BLANKS
     C                   SETON                                        9082
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCICN = BLAN
     C*
     C* THE FOLLOWING CORRESPONDENCE SHOULD EXIST BETWEEN
     C*          TYPE OF BILL AND TYPE OF SERVICE
     C*          --------------------------------
     C*          111 - 115    -->   IR, IP, IM, IS
     C*          131 - 135    -->   HS, HO, AS, AO
     C*
     C     SCICN         IFNE      *BLANKS
     C*
     C     SCBLTY        IFGE      '111'
     C     SCBLTY        ANDLE     '115'
     C     SCICN         ANDNE     'IR'
     C     SCICN         ANDNE     'IP'
     C     SCICN         ANDNE     'IM'
     C     SCICN         ANDNE     'IS'
     C                   SETON                                        9079
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCBLTY GE 111
     C*
     C     SCBLTY        IFGE      '131'
     C     SCBLTY        ANDLE     '135'
     C     SCICN         ANDNE     'HS'
     C     SCICN         ANDNE     'HO'
     C     SCICN         ANDNE     'AS'
     C     SCICN         ANDNE     'AO'
     C                   SETON                                        9046
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCBLTY GE 131
     C*
     C                   ENDIF                                                  SCICN NE BLANKS
     C*
     C** ELIGIBLE FOR DATA SUBMISSION FIELD IS REQUIRED IF IN30 IS ON
     C*          (DETERMINED IN 1GMSTR SUBROUINTE)
     C*
     C   30SCTAPE        IFEQ      *BLANKS
     C                   SETON                                        9080
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCTAPEIFEQ BLANKS
     C*
     C     *IN24         IFEQ      *OFF
     C*
     C                   MOVEL(P)  'FLES'        KKTBID
     C                   MOVEL(P)  SCTAPE        KKKEY
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *ON
     C     GTSTAT        OREQ      'I'
     C*
     C                   SETON                                        9081
     C                   GOTO      CK1END
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*
     C*  VALIDATE THE REVISION INDICATOR
     C*
     C     SCREVI        IFEQ      *BLANK
     C*
     C*
     C                   SETON                                        905657
     C                   SETON                                        97
     C*
     C                   GOTO      CK1END
     C                   ELSE
     C*
     C                   MOVEL(P)  'REVI'        KKTBID
     C                   MOVEL(P)  SCREVI        KKKEY
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *ON
     C     GTSTAT        OREQ      'I'
     C*
     C                   SETON                                        905657
     C                   SETOFF                                       97
     C                   GOTO      CK1END
     C*
     C                   ENDIF
     C                   ENDIF                                                  REVI
     C*
     C     @@MODE        IFEQ      'A'
     C                   MOVE      SCREVI        XXREVI            1
     C                   ELSE
     C                   MOVE      EOREVI        XXREVI            1
     C                   ENDIF
    C*** Beg Add *** 06/27/19 ****************************************
     C                   Endif
    C*** End Add *** 06/27/19 ****************************************
     C*
     C                   EXSR      DSPSC2
     C*
     C     CK1END        ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
    C*** Beg Add *** 06/27/19 ****************************************
     C                   if        not isPbillsLite
    C*** End Add *** 06/27/19 ****************************************
     C*
     C* FLORIDA LICENSE NUMBER IS REQUIRED IF DS FLAG =Y
     C*
     C*
     C     XXREVI        IFGE      'E'
     C                   MOVEL     SCBLTY        XXBLTY            3
     C     2             SUBST     XXBLTY:1      XBLTY             2
     C*
     C     XBLTY         IFEQ      '83'                                         AMBULATORY
     C     XBLTY         OREQ      '21'                                         NURSING HOME
     C     XBLTY         OREQ      '22'                                         NURSING HOME
     C     XBLTY         OREQ      '23'                                         NURSING HOME
     C     XBLTY         OREQ      '28'                                         NURSING HOME
     C     XBLTY         OREQ      '32'                                         HOME HEALTH
     C     XBLTY         OREQ      '33'                                         HOME HEALTH
     C     XBLTY         OREQ      '34'                                         HOME HEALTH
     C*
     C     SCFALC        IFEQ      *BLANKS
     C                   SETON                                        949596     FIELD REQUIRED
     C                   SETON                                        90         FIELD REQUIRED
     C                   GOTO      CK2END
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCTAPE        IFNE      'N'
     C     SCTAPE        ANDNE     ' '
     C*
     C     SCLC1         IFEQ      'Z'
     C     SCLC2         ANDEQ     'Z'
     C     SCFALC        ANDNE     ZZLIC#
     C                   SETON                                        9092
     C                   SETOFF                                       9193
     C                   GOTO      CK2END
     C                   ENDIF
     C*
     C*  Check to see if the Provider DB installed
     C*
     C                   MOVEL     *BLANKS       ##KEY
     C                   MOVEL     'FL'          ##KEY
     C     STKY          CHAIN     PST01                              99
     C     *IN99         IFEQ      *OFF
     C     STBS01        ANDEQ     'Y'
     C*
     C* edit against the PHY Lic# database
     C*
     C                   MOVEL     SCFALC        KKLIC#
     C     PLKY          CHAIN     PPL01                              99
     C     *IN99         IFEQ      *ON
     C     SCFLG7        IFNE      'Y'
     C     *IN90         DOUEQ     *OFF
     C                   SETOFF                                       90
     C                   MOVEL     *BLANKS       SCFLG7
     C                   EXFMT     WDW01
     C     *INKL         IFEQ      *OFF
     C     SCFLG7        IFNE      'Y'
     C     SCFLG7        ANDNE     'N'
     C                   SETON                                        4190
     C                   ENDIF
     C                   ENDIF
     C                   ENDDO
     C     *INKL         IFEQ      *ON
     C                   MOVEL     *BLANKS       SCFLG7
     C                   SETON                                        90
     C                   SETOFF                                       KL
     C                   ELSE
     C     SCFLG7        IFEQ      'N'
     C                   SETON                                        90
     C                   MOVEL     *BLANKS       SCFLG7
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     'Y'           SCFLG7
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     *BLANKS       SCFLG7
     C                   ENDIF                                                  *IN99
     C                   MOVEL     *ON           UPDATE            1
     C*
     C                   ELSE                                                   STBS01=Y
     C*
     C     SCFALC        IFNE      *BLANKS
     C     SCTAPE        ANDNE     'N'
     C     SCTAPE        ANDNE     ' '
     C                   MOVE      *ZEROS        LCNT              5 0
     C*
     C* FIRST TWO CHARACTERS OF LICENSE NUMBER MUST BE ALPHA
     C*
     C     SCLC1         LOOKUP    @A                                     60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        909193
     C                   SETOFF                                       92
     C                   GOTO      CK2END
     C                   ENDIF
     C*
     C                   ADD       1             LCNT
      *
     C     SCLC2         LOOKUP    @A                                     60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        909193
     C                   SETOFF                                       92
     C                   GOTO      CK2END
     C                   ENDIF
     C*
     C                   ADD       1             LCNT              5 0
      *
     C* 3rd position can be alpha or numeric
      *
     C     SCLC3         LOOKUP    @A                                     60
     C     *IN60         IFEQ      *ON
     C                   ADD       1             LCNT              5 0
     C                   ELSE
     C                   MOVE      *IN40         SV40              1
     C     SCLC3         LOOKUP    @N                                     40
     C     *IN40         IFEQ      *OFF
     C                   SETON                                        909193
     C                   SETOFF                                       92
     C                   GOTO      CK2END
     C                   ENDIF
     C                   MOVE      SV40          *IN40
     C                   ENDIF
      *
     C*
     C* 4TH position can be alpha or numeric
     C*
     C     SCLC4         IFGT      *BLANKS
      *
     C     SCLC4         LOOKUP    @A                                     60
     C     *IN60         IFEQ      *ON
      *
     C     LCNT          IFEQ      3
     C                   ADD       1             LCNT              5 0
     C                   ELSE
     C                   SETON                                        909193
     C                   SETOFF                                       92
     C                   GOTO      CK2END
     C                   ENDIF
      *
     C                   ELSE
     C                   MOVE      *IN40         SV40              1
     C     SCLC4         LOOKUP    @N                                     40
     C     *IN40         IFEQ      *OFF
     C                   SETON                                        909193
     C                   SETOFF                                       92
     C                   GOTO      CK2END
     C                   ENDIF
     C                   MOVE      SV40          *IN40
     C                   ENDIF
     C                   ENDIF
      *
     C                   CLEAR                   LL
      *
     C                   SELECT
     C     LCNT          WHENEQ    2
     C                   MOVEA     SCLCA         LL
     C     LCNT          WHENEQ    3
     C                   MOVEA     SCLCB         LL
     C     LCNT          WHENEQ    4
     C                   MOVEA     SCLCC         LL
     C                   ENDSL
      *
     C     13            SUB       LCNT          MAX               3 0
     C                   Z-ADD     MAX           IX                3 0
      * Find 1st non-blank
     C                   DO        MAX
     C     LL(IX)        IFNE      *BLANKS
     C                   LEAVE
     C                   ENDIF
     C                   SUB       1             IX
     C                   ENDDO
      *
      * Check numeric part
      *
     C     LL(1)         IFGT      *ZERO
      *
     C                   DO        IX            IY                3 0
     C     LL(IY)        LOOKUP    @N                                     60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        909192
     C                   SETOFF                                       93
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
      *
     C                   ELSE
     C                   SETON                                        909192
     C                   SETOFF                                       93
     C                   ENDIF
      *
     C     *IN90         IFEQ      *ON
     C                   GOTO      CK2END
     C                   ENDIF
     C*
     C                   ENDIF                                                  SCLIC# <> *BLANKS
     C*
     C                   ENDIF                                                  SCLIC# <> *BLANKS
     C*
     C                   ENDIF                                                  SCTAPE= *BLANKS
     C*
     C     SCLIC#        IFEQ      *BLANKS
     C                   SETON                                        9068
     C                   GOTO      CK2END
     C                   ELSE
     C     SCLIC1        IFEQ      'Z'
     C     SCLIC2        ANDEQ     'Z'
     C     SCLIC#        ANDNE     ZZLIC#
     C                   SETON                                        9089
     C                   GOTO      CK2END
     C                   ENDIF
     C                   ENDIF
     C*
     C*  Check to see if the Provider DB installed
     C*
     C                   MOVEL     *BLANKS       ##KEY
     C                   MOVEL     'FL'          ##KEY
     C     STKY          CHAIN     PST01                              99
     C     *IN99         IFEQ      *OFF
     C     STBS01        ANDEQ     'Y'
     C*
     C* edit against the Prov Lic# database
     C*
     C                   MOVEL     SCLIC#        KKLIC#
     C     PLKY          CHAIN     PPL01                              99
     C     *IN99         IFEQ      *ON
     C     SCFLG7        IFNE      'Y'
     C     *IN90         DOUEQ     *OFF
     C                   SETOFF                                       90
     C                   MOVEL     *BLANKS       SCFLG7
     C                   EXFMT     WDW01
     C     *INKL         IFEQ      *OFF
     C     SCFLG7        IFNE      'Y'
     C     SCFLG7        ANDNE     'N'
     C                   SETON                                        4190
     C                   ENDIF
     C                   ENDIF
     C                   ENDDO
     C     *INKL         IFEQ      *ON
     C                   MOVEL     *BLANKS       SCFLG7
     C                   SETON                                        90
     C                   SETOFF                                       KL
     C                   ELSE
     C     SCFLG7        IFEQ      'N'
     C                   SETON                                        90
     C                   MOVEL     *BLANKS       SCFLG7
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     'Y'           SCFLG7
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     *BLANKS       SCFLG7
     C                   ENDIF                                                  *IN99
     C                   MOVEL     *ON           UPDATE            1
     C*
     C                   ELSE                                                   STBS01=Y
     C*
     C     SCLIC#        IFNE      *BLANKS
     C     SCTAPE        ANDNE     'N'
     C     SCTAPE        ANDNE     ' '
     C*
     C* FIRST TWO CHARACTERS OF LICENSE NUMBER MUST BE ALPHA
     C*
     C* 3rd position can be alpha or numeric
     C* 4rd position can be alpha or numeric
     C*
     C**  The last 7 digits MUST BE numeric
     C*
     C*
     C*
     C                   MOVE      *ZEROS        LCNT              5 0
     C*
     C* FIRST TWO CHARACTERS OF LICENSE NUMBER MUST BE ALPHA
     C*
     C     SCLIC1        LOOKUP    @A                                     60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        9069
     C                   GOTO      CK2END
     C                   ENDIF
     C*
     C                   ADD       1             LCNT
      *
     C     SCLIC2        LOOKUP    @A                                     60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        9069
     C                   GOTO      CK2END
     C                   ENDIF
     C*
     C                   ADD       1             LCNT              5 0
      *
     C* 3rd position can be alpha or numeric
      *
     C     SCLIC3        LOOKUP    @A                                     60
     C     *IN60         IFEQ      *ON
     C                   ADD       1             LCNT              5 0
     C                   ELSE
     C                   MOVE      *IN40         SV40              1
     C     SCLIC3        LOOKUP    @N                                     40
     C     *IN40         IFEQ      *OFF
     C                   SETON                                        9069
     C                   GOTO      CK2END
     C                   ENDIF
     C                   MOVE      SV40          *IN40
     C                   ENDIF
      *
     C*
     C* 4TH position can be alpha or numeric
     C*
     C     SCLIC4        IFGT      *BLANKS
      *
     C     SCLIC4        LOOKUP    @A                                     60
     C     *IN60         IFEQ      *ON
      *
     C     LCNT          IFEQ      3
     C                   ADD       1             LCNT              5 0
     C                   ELSE
     C                   SETON                                        9069
     C                   GOTO      CK2END
     C                   ENDIF
      *
     C                   ELSE
     C                   MOVE      *IN40         SV40              1
     C     SCLIC4        LOOKUP    @N                                     40
     C     *IN40         IFEQ      *OFF
     C                   SETON                                        9069
     C                   GOTO      CK2END
     C                   ENDIF
     C                   MOVE      SV40          *IN40
     C                   ENDIF
     C                   ENDIF
      *
     C                   CLEAR                   LL
      *
     C                   SELECT
     C     LCNT          WHENEQ    2
     C                   MOVEA     SCLICA        LL
     C     LCNT          WHENEQ    3
     C                   MOVEA     SCLICB        LL
     C     LCNT          WHENEQ    4
     C                   MOVEA     SCLICC        LL
     C                   ENDSL
      *
     C     13            SUB       LCNT          MAX               3 0
     C                   Z-ADD     MAX           IX                3 0
      * Find 1st non-blank
     C                   DO        MAX
     C     LL(IX)        IFNE      *BLANKS
     C                   LEAVE
     C                   ENDIF
     C                   SUB       1             IX
     C                   ENDDO
      * Check numeric part
      *
      *
     C     LL(1)         IFGT      *ZERO
      *
     C                   DO        IX            IY                3 0
     C     LL(IY)        LOOKUP    @N                                     60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        9070
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
      *
     C                   ELSE
     C                   SETON                                        9070
     C                   ENDIF
      *
     C     *IN90         IFEQ      *ON
     C                   GOTO      CK2END
     C                   ENDIF
     C*
     C                   ENDIF                                                  SCLIC# <> *BLANKS
     C*
     C                   ENDIF                                                  SCLIC# <> *BLANKS
     C*
     C*
     C*
     C*  PAYMENT PLAN IS REQUIRED
     C*
     C     SCTAPE        IFNE      'Y'
     C     SCTAPE        ANDNE     'N'
     C     SCPPLN        ANDEQ     *BLANKS
     C                   SETON                                        9066
     C                   GOTO      CK2END
     C                   ENDIF
     C*
     C*  VALIDATE PAYMENT PLAN
     C*
     C  N90SCPPLN        IFNE      *BLANKS
     C     SCTAPE        IFEQ      'Y'
     C                   MOVEL(P)  'FLPP'        KKTBID
     C                   ELSE
     C                   MOVEL(P)  'FLPA'        KKTBID
     C                   ENDIF
     C                   MOVEL(P)  SCPPLN        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9067
     C                   GOTO      CK2END
     C                   ELSE
     C                   MOVEL     GTDESC        SCPPLD
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C*
     C* PROCDURE CODE METHOD IS REQUIRED
     C*
     C     SCPCM         IFEQ      *BLANKS
     C                   SETON                                        9071
     C                   GOTO      CK2END
     C                   ENDIF                                                  SCPCM = *BLANKS
     C*
     C* VALIDATE PROCEDURE CODE METHOD
     C*
     C     EDITST        CAT(P)    'PM':0        PMTBID            4
     C                   MOVEL(P)  PMTBID        KKTBID
     C     GTKY2         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'ZZPM'        KKTBID
     C                   ENDIF
     C*
     C     GTKY2         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVEL     'PCMT'        KKTBID
     C                   ENDIF
     C*
     C                   MOVEL(P)  SCPCM         KKKEY
     C*
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9072
     C                   ENDIF
     C*
     C* IF PROC CODE METHOD =0 PRINCIPAL PROCEDURE CODE MUST BE BLANK
     C*
     C     SCPCM         IFEQ      '0'
     C     SCPRC1        ANDNE     *BLANKS
     C                   SETON                                        9064
     C                   GOTO      CK2END
     C                   ENDIF                                                  SCPCM = *BLANKS
     C*
     C* IF PROC CODE METHOD =0 SECONDARY PROCEDURE CODE MUST BE BLANK
     C*
     C     SCPCM         IFEQ      '0'
     C     SCPRC2        ANDNE     *BLANKS
     C                   SETON                                        9065
     C                   GOTO      CK2END
     C                   ENDIF                                                  SCPCM = *BLANKS
     C*
     C* IF PROC CODE METHOD =0 SECONDARY PROCEDURE CODE MUST BE BLANK
     C*
     C     SCPCM         IFEQ      '0'
     C     SCPRC3        ANDNE     *BLANKS
     C                   SETON                                        9014
     C                   GOTO      CK2END
     C                   ENDIF                                                  SCPCM = *BLANKS
     C*
     C* IF PROC CODE METHOD =0 SECONDARY PROCEDURE CODE MUST BE BLANK
     C*
     C     SCPCM         IFEQ      '0'
     C     SCPRC4        ANDNE     *BLANKS
     C                   SETON                                        9020
     C                   GOTO      CK2END
     C                   ENDIF                                                  SCPCM = *BLANKS
     C*
     C* IF PROC CODE METHOD =0 SECONDARY PROCEDURE CODE MUST BE BLANK
     C*
     C     SCPCM         IFEQ      '0'
     C     SCPRC5        ANDNE     *BLANKS
     C                   SETON                                        9025
     C                   GOTO      CK2END
     C                   ENDIF                                                  SCPCM = *BLANKS
     C*
     C* IF PROC CODE METHOD =0 SECONDARY PROCEDURE CODE MUST BE BLANK
     C*
     C     SCPCM         IFEQ      '0'
     C     SCPRC6        ANDNE     *BLANKS
     C                   SETON                                        9036
     C                   GOTO      CK2END
     C                   ENDIF                                                  SCPCM = *BLANKS
     C*
     C* VALIDATE PROCEDURE CODES BASE ON PROCEDURE CODE METHODS ENTERED
     C* IF PROC CODE METHOD NE 0 THEN PRINC PROCEDURE CODE REQUIREC
     C     SCPCM         IFNE      '0'
     C*
     C     SCPCM         IFEQ      '4'
     C     SCPRC1        ANDNE     *BLANKS
     C                   MOVEL     SCPRC1        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9073
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCPCM=4
     C*
     C     SCPCM         IFEQ      '5'
     C     SCPRC1        ANDNE     *BLANKS
     C                   MOVEL     SCPRC1        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  SCPRC1        KKCPT4
     C     RCKY          CHAIN     PRC02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9073
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCPCM=5
     C                   ENDIF
     C*
     C                   MOVEL     ' '           SCLOS1
     C     SCPCM         IFEQ      '9'
     C     SCPRC1        ANDNE     *BLANKS
     C                   MOVEL     'O'           SCLOS1
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxicd10 = '1' and scICdInd ='Y';
          kkICd10 = scPRc1;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'O';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If not %found(fmicd10l1)
                or ICstat = 'I';
                *in90 = *on;
                *in73 = *on;
                leavesr;
             Endif;
       // Beg Add ***** 06/03/15 *******************************************
              erPrc1 = *off;
           if scPRc1 <> *blanks
              and ICvld = '0';
              *in90  = *on;
              erPrc1 = *on;
              leavesr;
           endif;
       // End Add ***** 06/03/15 *******************************************
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   MOVEL(P)  SCPRC1        KKICD9
     C                   MOVE      SCLOS1        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9073
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C*
     C*
     C*** Beg Add *** 09/08/14 ****************************************
     C*
      /Free

              *in90 = *off;
              erprc1 = *off;

           if scprc1 <> *blanks
       // Beg Add ***** 06/03/15 *******************************************
              and scICdInd <> 'Y'
       // End Add ***** 06/03/15 *******************************************
              and dispecific = '0';
              *in90 = *on;
              erprc1 = *on;
              leavesr;
           endif;

      /End-Free
     C*
     C*** End Add *** 09/08/14 ****************************************
     C*
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   ENDIF                                                  SCPCM =9
     C*
     C* IF PROC CODE METHOD NE 0 THEN SECONDARY CODE IS OPTIONAL
     C     SCPRC2        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '4'
     C                   MOVEL     SCPRC2        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9074
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCPRC2 NE *BLANKS
     C*
     C     SCPRC2        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '5'
     C                   MOVEL     SCPRC2        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  SCPRC2        KKCPT4
     C     RCKY          CHAIN     PRC02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9074
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF
     C                   ENDIF                                                  SCPRC2 NE *BLANKS
     C*
     C                   MOVEL     ' '           SCLOS2
     C     SCPRC2        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '9'
     C                   MOVEL     'O'           SCLOS2
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxicd10 = '1' and scICdInd ='Y';
          kkICd10 = scPRc2;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'O';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If not %found(fmicd10l1)
                or ICstat = 'I';
                *in90 = *on;
                *in74 = *on;
                leavesr;
             Endif;
       // Beg Add ***** 06/03/15 *******************************************
              erPrc2 = *off;
           if scPRc2 <> *blanks
              and ICvld = '0';
              *in90  = *on;
              erPrc2 = *on;
              leavesr;
           endif;
       // End Add ***** 06/03/15 *******************************************
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   MOVEL(P)  SCPRC2        KKICD9
     C                   MOVE      SCLOS2        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9074
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*
     C*** Beg Add *** 09/08/14 ****************************************
     C*
      /Free

              *in90 = *off;
              erprc2 = *off;

           if scprc2 <> *blanks
       // Beg Add ***** 06/03/15 *******************************************
              and scICdInd <> 'Y'
       // End Add ***** 06/03/15 *******************************************
              and dispecific = '0';
              *in90 = *on;
              erprc2 = *on;
              leavesr;
           endif;

      /End-Free
     C*
     C*** End Add *** 09/08/14 ****************************************
     C*
     C*** End Add *** 08/05/13 ****************************************
     C                   ENDIF                                                  SCPRC2 NE *BLANKS
     C*
     C*
     C* SCPRC3
     C*
     C     SCPRC3        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '4'
     C                   MOVEL     SCPRC3        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9012
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCPRC3 NE *BLANKS
     C*
     C     SCPRC3        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '5'
     C                   MOVEL     SCPRC3        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  SCPRC3        KKCPT4
     C     RCKY          CHAIN     PRC02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9012
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF
     C                   ENDIF                                                  SCPRC3 NE *BLANKS
     C*
     C                   MOVEL     ' '           SCLOS3
     C*
     C     SCPRC3        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '9'
     C                   MOVEL     'O'           SCLOS3
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxicd10 = '1' and scICdInd ='Y';
          kkICd10 = scPRc3;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'O';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If not %found(fmicd10l1)
                or ICstat = 'I';
                *in90 = *on;
                *in12 = *on;
                leavesr;
             Endif;
       // Beg Add ***** 06/03/15 *******************************************
              erPrc3 = *off;
           if scPRc3 <> *blanks
              and ICvld = '0';
              *in90  = *on;
              erPrc3 = *on;
              leavesr;
           endif;
       // End Add ***** 06/03/15 *******************************************
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   MOVEL(P)  SCPRC3        KKICD9
     C                   MOVE      SCLOS3        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9012
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*
     C*** Beg Add *** 09/08/14 ****************************************
     C*
      /Free

              *in90 = *off;
              erprc3 = *off;

           if scprc3 <> *blanks
       // Beg Add ***** 06/03/15 *******************************************
              and scICdInd <> 'Y'
       // End Add ***** 06/03/15 *******************************************
              and dispecific = '0';
              *in90 = *on;
              erprc3 = *on;
              leavesr;
           endif;

      /End-Free
     C*
     C*** End Add *** 09/08/14 ****************************************
     C*
     C*** End Add *** 08/05/13 ****************************************
     C                   ENDIF                                                  SCPRC3 NE *BLANKS
     C*
     C* SCPRC4
     C*
     C     SCPRC4        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '4'
     C                   MOVEL     SCPRC4        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9018
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCPRC4 NE *BLANKS
     C*
     C     SCPRC4        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '5'
     C                   MOVEL     SCPRC4        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  SCPRC4        KKCPT4
     C     RCKY          CHAIN     PRC02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9018
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF
     C                   ENDIF                                                  SCPRC4 NE *BLANKS
     C*
     C                   MOVEL     ' '           SCLOS4
     C*
     C     SCPRC4        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '9'
     C                   MOVEL     'O'           SCLOS4
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxicd10 = '1' and scICdInd ='Y';
          kkICd10 = scPRc4;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'O';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If not %found(fmicd10l1)
                or ICstat = 'I';
                *in90 = *on;
                *in18 = *on;
                leavesr;
             Endif;
       // Beg Add ***** 06/03/15 *******************************************
              erprc4 = *off;
           if scPRc4 <> *blanks
              and ICvld = '0';
              *in90  = *on;
              erPrc4 = *on;
              leavesr;
           endif;
       // End Add ***** 06/03/15 *******************************************
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   MOVEL(P)  SCPRC4        KKICD9
     C                   MOVE      SCLOS4        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9018
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*
     C*** Beg Add *** 09/08/14 ****************************************
     C*
      /Free

              *in90 = *off;
              erprc4 = *off;

           if scprc4 <> *blanks
       // Beg Add ***** 06/03/15 *******************************************
              and scICdInd <> 'Y'
       // End Add ***** 06/03/15 *******************************************
              and dispecific = '0';
              *in90 = *on;
              erprc4 = *on;
              leavesr;
           endif;

      /End-Free
     C*
     C*** End Add *** 09/08/14 ****************************************
     C*
     C*** End Add *** 08/05/13 ****************************************
     C                   ENDIF                                                  SCPRC4 NE *BLANKS
     C*
     C* SCPRC5
     C*
     C     SCPRC5        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '4'
     C                   MOVEL     SCPRC5        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9023
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCPRC5 NE *BLANKS
     C*
     C     SCPRC5        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '5'
     C                   MOVEL     SCPRC5        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  SCPRC5        KKCPT4
     C     RCKY          CHAIN     PRC02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9023
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF
     C                   ENDIF                                                  SCPRC5 NE *BLANKS
     C*
     C                   MOVEL     ' '           SCLOS5
     C*
     C     SCPRC5        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '9'
     C                   MOVEL     'O'           SCLOS5
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxicd10 = '1' and scICdInd ='Y';
          kkICd10 = scPRc5;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'O';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If not %found(fmicd10l1)
                or ICstat = 'I';
                *in90 = *on;
                *in23 = *on;
                leavesr;
             Endif;
       // Beg Add ***** 06/03/15 *******************************************
              erprc5 = *off;
           if scPRc5 <> *blanks
              and ICvld = '0';
              *in90  = *on;
              erPrc5 = *on;
              leavesr;
           endif;
       // End Add ***** 06/03/15 *******************************************
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   MOVEL(P)  SCPRC5        KKICD9
     C                   MOVE      SCLOS5        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9023
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*
     C*** Beg Add *** 09/08/14 ****************************************
     C*
      /Free

              *in90 = *off;
              erprc5 = *off;

           if scprc5 <> *blanks
       // Beg Add ***** 06/03/15 *******************************************
              and scICdInd <> 'Y'
       // End Add ***** 06/03/15 *******************************************
              and dispecific = '0';
              *in90 = *on;
              erprc5 = *on;
              leavesr;
           endif;

      /End-Free
     C*
     C*** End Add *** 09/08/14 ****************************************
     C*
     C*** End Add *** 08/05/13 ****************************************
     C*
     C                   ENDIF                                                  SCPRC5 NE *BLANKS
     C*
     C* SCPRC6
     C*
     C     SCPRC6        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '4'
     C                   MOVEL     SCPRC6        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9034
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCPRC6 NE *BLANKS
     C*
     C     SCPRC6        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '5'
     C                   MOVEL     SCPRC6        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  SCPRC6        KKCPT4
     C     RCKY          CHAIN     PRC02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9034
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF
     C                   ENDIF                                                  SCPRC6 NE *BLANKS
     C*
     C                   MOVEL     ' '           SCLOS6
     C*
     C     SCPRC6        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '9'
     C                   MOVEL     'O'           SCLOS6
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxicd10 = '1' and scICdInd ='Y';
          kkICd10 = scPRc6;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'O';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If not %found(fmicd10l1)
                or ICstat = 'I';
                *in90 = *on;
                *in34 = *on;
                leavesr;
             Endif;
       // Beg Add ***** 06/03/15 *******************************************
              erprc6 = *off;
           if scPRc6 <> *blanks
              and ICvld = '0';
              *in90  = *on;
              erPrc6 = *on;
              leavesr;
           endif;
       // End Add ***** 06/03/15 *******************************************
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   MOVEL(P)  SCPRC6        KKICD9
     C                   MOVE      SCLOS6        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9034
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*
     C*** Beg Add *** 09/08/14 ****************************************
     C*
      /Free

              *in90 = *off;
              erprc6 = *off;

           if scprc6 <> *blanks
       // Beg Add ***** 06/03/15 *******************************************
              and scICdInd <> 'Y'
       // End Add ***** 06/03/15 *******************************************
              and dispecific = '0';
              *in90 = *on;
              erprc6 = *on;
              leavesr;
           endif;

      /End-Free
     C*
     C*** End Add *** 09/08/14 ****************************************
     C*
     C*** End Add *** 08/05/13 ****************************************
     C*
     C*** End Add *** 08/05/13 ****************************************
     C                   ENDIF                                                  SCPRC6 NE *BLANKS
     C*
     C                   ENDIF                                                  SCPCM NE 0
     C*
     C* VALIDATE THE OTHER DIAGNOSIS CODES
     C*
     C     SCDIA2        IFNE      *BLANKS
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxicd10 = '1' and scICdInd ='Y';
          kkICd10 = scDIa2;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If not %found(fmicd10l1)
                or ICstat = 'I';
                *in90 = *on;
                *in77 = *on;
                leavesr;
             Endif;
       // Beg Add ***** 06/03/15 *******************************************
              erdia2 = *off;
           if scDIa2 <> *blanks
              and ICvld = '0';
              *in90  = *on;
              erDia2 = *on;
              leavesr;
           endif;
       // End Add ***** 06/03/15 *******************************************
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   MOVEL     SCDIA2        KKICD9
     C                   MOVE      'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        9077
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*
     C*** Beg Add *** 09/08/14 ****************************************
     C*
      /Free

              *in90 = *off;
              erdia2 = *off;

           if scdia2 <> *blanks
       // Beg Add ***** 06/03/15 *******************************************
              and scICdInd <> 'Y'
       // End Add ***** 06/03/15 *******************************************
              and dispecific = '0';
              *in90 = *on;
              erdia2 = *on;
              leavesr;
           endif;

      /End-Free
     C*
     C*** End Add *** 09/08/14 ****************************************
     C*
     C*** End Add *** 08/05/13 ****************************************
     C                   ENDIF                                                  SCDIA1 NE *BLANKS
     C*
     C     SCDIA3        IFNE      *BLANKS
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxicd10 = '1' and scICdInd ='Y';
          kkICd10 = scDIa3;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If not %found(fmicd10l1)
                or ICstat = 'I';
                *in90 = *on;
                *in78 = *on;
                leavesr;
             Endif;
       // Beg Add ***** 06/03/15 *******************************************
              erdia3 = *off;
           if scDIa3 <> *blanks
              and ICvld = '0';
              *in90  = *on;
              erDia3 = *on;
              leavesr;
           endif;
       // End Add ***** 06/03/15 *******************************************
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   MOVEL     SCDIA3        KKICD9
     C                   MOVE      'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        9078
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
      /Free
       Endif;
      /End-Free
     C*
     C*** Beg Add *** 09/08/14 ****************************************
     C*
      /Free

              *in90 = *off;
              erdia3 = *off;

           if scdia3 <> *blanks
       // Beg Add ***** 06/03/15 *******************************************
              and scICdInd <> 'Y'
       // End Add ***** 06/03/15 *******************************************
              and dispecific = '0';
              *in90 = *on;
              erdia3 = *on;
              leavesr;
           endif;

      /End-Free
     C*
     C*** End Add *** 09/08/14 ****************************************
     C                   ENDIF                                                  SCDIA1 NE *BLANKS
     C*
     C     SCDIA4        IFNE      *BLANKS
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxicd10 = '1' and scICdInd ='Y';
          kkICd10 = scDIa4;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If not %found(fmicd10l1)
                or ICstat = 'I';
                *in90 = *on;
                *in79 = *on;
                leavesr;
             Endif;
       // Beg Add ***** 06/03/15 *******************************************
              erdia4 = *off;
           if scDIa4 <> *blanks
              and ICvld = '0';
              *in90  = *on;
              erDia4 = *on;
              leavesr;
           endif;
       // End Add ***** 06/03/15 *******************************************
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   MOVEL     SCDIA4        KKICD9
     C                   MOVE      'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        9079
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*
     C*** Beg Add *** 09/08/14 ****************************************
     C*
      /Free

              *in90 = *off;
              erdia4 = *off;

           if scdia4 <> *blanks
       // Beg Add ***** 06/03/15 *******************************************
              and scICdInd <> 'Y'
       // End Add ***** 06/03/15 *******************************************
              and dispecific = '0';
              *in90 = *on;
              erdia4 = *on;
              leavesr;
           endif;


      /End-Free
     C*
     C*** End Add *** 09/08/14 ****************************************
     C*
     C*** End Add *** 08/05/13 ****************************************
     C                   ENDIF                                                  SCDIA1 NE *BLANKS
    C*** Beg Add *** 06/27/19 ****************************************
     C                   Endif
    C*** End Add *** 06/27/19 ****************************************
     C     @@MODE        IFNE      'D'
     C                   SETON                                        23
     C                   ENDIF
     C*
     C     CK2END        ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C* VALIDATE THE OTHER DIAGNOSIS CODES
     C*================================================================
     C     ERRCK3        BEGSR
     C*
    C*** Beg Add *** 06/27/19 ****************************************
     C                   if        not isPbillsLite
    C*** End Add *** 06/27/19 ****************************************
     C*
     C     SCDIA5        IFNE      *BLANKS
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxicd10 = '1' and scICdInd ='Y';
          kkICd10 = scDIa5;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If not %found(fmicd10l1)
                or ICstat = 'I';
                *in90 = *on;
                *in80 = *on;
                leavesr;
             Endif;
       // Beg Add ***** 06/03/15 *******************************************
              erdia5 = *off;
           if scDIa5 <> *blanks
              and ICvld = '0';
              *in90  = *on;
              erDia5 = *on;
              leavesr;
           endif;
       // End Add ***** 06/03/15 *******************************************
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   MOVEL     SCDIA5        KKICD9
     C                   MOVE      'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        9080
     C                   GOTO      CK3END
     C                   ENDIF                                                  *IN99 = *ON
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*
     C*** Beg Add *** 09/08/14 ****************************************
     C*
      /Free

              *in90 = *off;
              erdia5 = *off;

           if scdia5 <> *blanks
       // Beg Add ***** 06/03/15 *******************************************
              and scICdInd <> 'Y'
       // End Add ***** 06/03/15 *******************************************
              and dispecific = '0';
              *in90 = *on;
              erdia5 = *on;
              leavesr;
           endif;


      /End-Free
     C*
     C*** End Add *** 09/08/14 ****************************************
     C*
     C*** End Add *** 08/05/13 ****************************************
     C                   ENDIF                                                  SCDIA5 NE *BLANKS
     C*
     C     SCDIA6        IFNE      *BLANKS
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxicd10 = '1' and scICdInd ='Y';
          kkICd10 = scDIa6;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If not %found(fmicd10l1)
                or ICstat = 'I';
                *in90 = *on;
                *in81 = *on;
                leavesr;
             Endif;
       // Beg Add ***** 06/03/15 *******************************************
              erdia6 = *off;
           if scDIa6 <> *blanks
              and ICvld = '0';
              *in90  = *on;
              erDia6 = *on;
              leavesr;
           endif;
       // End Add ***** 06/03/15 *******************************************
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   MOVEL     SCDIA6        KKICD9
     C                   MOVE      'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        9081
     C                   GOTO      CK3END
     C                   ENDIF                                                  *IN99 = *ON
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*
     C*** Beg Add *** 09/08/14 ****************************************
     C*
      /Free

              *in90 = *off;
              erdia6 = *off;

           if scdia6 <> *blanks
       // Beg Add ***** 06/03/15 *******************************************
              and scICdInd <> 'Y'
       // End Add ***** 06/03/15 *******************************************
              and dispecific = '0';
              *in90 = *on;
              erdia6 = *on;
              leavesr;
           endif;


      /End-Free
     C*
     C*** End Add *** 09/08/14 ****************************************
     C*
     C*** End Add *** 08/05/13 ****************************************
     C                   ENDIF                                                  SCDIA6 NE *BLANKS
     C*
     C     SCDIA7        IFNE      *BLANKS
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxicd10 = '1' and scICdInd ='Y';
          kkICd10 = scDIa7;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If not %found(fmicd10l1)
                or ICstat = 'I';
                *in90 = *on;
                *in82 = *on;
                leavesr;
             Endif;
       // Beg Add ***** 06/03/15 *******************************************
              erdia7 = *off;
           if scDIa7 <> *blanks
              and ICvld = '0';
              *in90  = *on;
              erDia7 = *on;
              leavesr;
           endif;
       // End Add ***** 06/03/15 *******************************************
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   MOVEL     SCDIA7        KKICD9
     C                   MOVE      'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        9082
     C                   GOTO      CK3END
     C                   ENDIF                                                  *IN99 = *ON
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*
     C*** Beg Add *** 09/08/14 ****************************************
     C*
      /Free

              *in90 = *off;
              erdia7 = *off;

           if scdia7 <> *blanks
       // Beg Add ***** 06/03/15 *******************************************
              and scICdInd <> 'Y'
       // End Add ***** 06/03/15 *******************************************
              and dispecific = '0';
              *in90 = *on;
              erdia7 = *on;
              leavesr;
           endif;

      /End-Free
     C*
     C*** End Add *** 09/08/14 ****************************************
     C*
     C*** End Add *** 08/05/13 ****************************************
     C                   ENDIF                                                  SCDIA7 NE *BLANKS
     C*
     C     SCDIA8        IFNE      *BLANKS
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxicd10 = '1' and scICdInd ='Y';
          kkICd10 = scDIa8;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If not %found(fmicd10l1)
                or ICstat = 'I';
                *in90 = *on;
                *in83 = *on;
                leavesr;
             Endif;
       // Beg Add ***** 06/03/15 *******************************************
              erdia8 = *off;
           if scDIa8 <> *blanks
              and ICvld = '0';
              *in90  = *on;
              erDia8 = *on;
              leavesr;
           endif;
       // End Add ***** 06/03/15 *******************************************
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   MOVEL     SCDIA8        KKICD9
     C                   MOVE      'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        9083
     C                   GOTO      CK3END
     C                   ENDIF                                                  *IN99 = *ON
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*
     C*** Beg Add *** 09/08/14 ****************************************
     C*
      /Free

              *in90 = *off;
              erdia8 = *off;

           if scdia8 <> *blanks
       // Beg Add ***** 06/03/15 *******************************************
              and scICdInd <> 'Y'
       // End Add ***** 06/03/15 *******************************************
              and dispecific = '0';
              *in90 = *on;
              erdia8 = *on;
              leavesr;
           endif;

      /End-Free
     C*
     C*** End Add *** 09/08/14 ****************************************
     C*
     C*** End Add *** 08/05/13 ****************************************
     C                   ENDIF                                                  SCDIA8 NE *BLANKS
     C     SCCAU1        IFNE      *BLANKS
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxicd10 = '1' and scICdInd ='Y';
          kkICd10 = scCAu1;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If not %found(fmicd10l1)
                or ICstat = 'I';
                *in90 = *on;
                *in84 = *on;
                leavesr;
             Endif;
       // Beg Add ***** 06/03/15 *******************************************
              ercau1 = *off;
           if scCAu1 <> *blanks
              and ICvld = '0';
              *in90  = *on;
              erCau1 = *on;
              leavesr;
           endif;
       // End Add ***** 06/03/15 *******************************************
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   MOVEL     SCCAU1        KKICD9
     C                   MOVE      'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        9084
     C                   GOTO      CK3END
     C                   ENDIF                                                  *IN99 = *ON
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*
     C*** Beg Add *** 09/08/14 ****************************************
     C*
      /Free

              *in90 = *off;
              ercau1 = *off;

           if sccau1 <> *blanks
       // Beg Add ***** 06/03/15 *******************************************
              and scICdInd <> 'Y'
       // End Add ***** 06/03/15 *******************************************
              and dispecific = '0';
              *in90 = *on;
              ercau1 = *on;
              leavesr;
           endif;

      /End-Free
     C*
     C*** End Add *** 09/08/14 ****************************************
     C*
     C*** End Add *** 08/05/13 ****************************************
     C                   ENDIF                                                  SCCAU1 NE *BLANKS
     C     SCCAU2        IFNE      *BLANKS
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxicd10 = '1' and scICdInd ='Y';
          kkICd10 = scCAu2;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If not %found(fmicd10l1)
                or ICstat = 'I';
                *in90 = *on;
                *in85 = *on;
                leavesr;
             Endif;
       // Beg Add ***** 06/03/15 *******************************************
              ercau2 = *off;
           if scCAu2 <> *blanks
              and ICvld = '0';
              *in90  = *on;
              erCau2 = *on;
              leavesr;
           endif;
       // End Add ***** 06/03/15 *******************************************
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   MOVEL     SCCAU2        KKICD9
     C                   MOVE      'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        9085
     C                   GOTO      CK3END
     C                   ENDIF                                                  *IN99 = *ON
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*
     C*** Beg Add *** 09/08/14 ****************************************
     C*
      /Free

              *in90 = *off;
              ercau2 = *off;

           if sccau2 <> *blanks
       // Beg Add ***** 06/03/15 *******************************************
              and scICdInd <> 'Y'
       // End Add ***** 06/03/15 *******************************************
              and dispecific = '0';
              *in90 = *on;
              ercau2 = *on;
              leavesr;
           endif;

      /End-Free
     C*
     C*** End Add *** 09/08/14 ****************************************
     C*
     C*** End Add *** 08/05/13 ****************************************
     C                   ENDIF                                                  SCCAU2 NE *BLANKS
     C*
     C     SCCAU3        IFNE      *BLANKS
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxicd10 = '1' and scICdInd ='Y';
          kkICd10 = scCAu3;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If not %found(fmicd10l1)
                or ICstat = 'I';
                *in90 = *on;
                *in86 = *on;
                leavesr;
             Endif;
       // Beg Add ***** 06/03/15 *******************************************
              ercau3 = *off;
           if scCAu3 <> *blanks
              and ICvld = '0';
              *in90  = *on;
              erCau3 = *on;
              leavesr;
           endif;
       // End Add ***** 06/03/15 *******************************************
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   MOVEL     SCCAU3        KKICD9
     C                   MOVE      'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        9086
     C                   GOTO      CK3END
     C                   ENDIF                                                  *IN99 = *ON
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*
     C*** Beg Add *** 09/08/14 ****************************************
     C*
      /Free

              *in90 = *off;
              ercau3 = *off;

           if sccau3 <> *blanks
       // Beg Add ***** 06/03/15 *******************************************
              and scICdInd <> 'Y'
       // End Add ***** 06/03/15 *******************************************
              and dispecific = '0';
              *in90 = *on;
              ercau3 = *on;
              leavesr;
           endif;

      /End-Free
     C*
     C*** End Add *** 09/08/14 ****************************************
     C*
     C*** End Add *** 08/05/13 ****************************************
     C                   ENDIF                                                  SCCAU3 NE *BLANKS
     C*                                                                LANKS
     C*
     C* IF OUTPATIENT SURGERY THEN REQUIRE SCHEDULED/UNSCHEDULED INDIC
     C*
     C     SCICN         IFEQ      'HS'
     C     SCICN         OREQ      'AS'
     C*
     C     SCSCHD        IFEQ      *BLANK
     C                   SETON                                        9088
     C                   GOTO      CK3END
     C                   ENDIF                                                  SCSCHD= BLANK
     C                   ENDIF                                                  SCICN = HS
     C     SCSCHD        IFNE      *BLANK
     C                   MOVEL(P)  SCSCHD        KKKEY
     C                   MOVEL(P)  'FLSC'        KKTBID
     C     GTKY          CHAIN     PGT02                              99
     C   99              SETON                                        9087
     C   99              GOTO      CK3END
     C                   ENDIF                                                  SCSCHD<>BLANK
     C*
     C* Validate Operating Phy. Lic#
     C*
     C     SCTAPE        IFNE      'N'
     C     SCTAPE        ANDNE     *BLANKS
     C*
     C     SCOLIC        IFNE      *BLANKS
     C* 4TH position can be alpha or numeric
     C*   The last 7 digits MUST BE numeric
     C*
     C     SCOLC1        IFEQ      'Z'
     C     SCOLC2        ANDEQ     'Z'
     C     SCOLIC        ANDNE     ZZLIC#
     C                   SETON                                        9089
     C                   GOTO      CK3END
     C                   ENDIF
     C                   MOVE      *ZEROS        LCNT              5 0
     C*
     C* FIRST TWO CHARACTERS OF LICENSE NUMBER MUST BE ALPHA
     C*
     C     SCOLC1        LOOKUP    @A                                     60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        9005
     C                   GOTO      CK3END
     C                   ENDIF
     C*
     C                   ADD       1             LCNT
      *
     C     SCOLC2        LOOKUP    @A                                     60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        9005
     C                   GOTO      CK3END
     C                   ENDIF
     C*
     C                   ADD       1             LCNT              5 0
      *
     C* 3rd position can be alpha or numeric
      *
     C     SCOLC3        LOOKUP    @A                                     60
     C     *IN60         IFEQ      *ON
     C                   ADD       1             LCNT              5 0
     C                   ELSE
     C                   MOVE      *IN40         SV40              1
     C     SCOLC3        LOOKUP    @N                                     40
     C     *IN40         IFEQ      *OFF
     C                   SETON                                        9094
     C                   GOTO      CK3END
     C                   ENDIF
     C                   MOVE      SV40          *IN40
     C                   ENDIF
      *
     C*
     C* 4TH position can be alpha or numeric
     C*
     C     SCOLC4        LOOKUP    @A                                     60
     C     *IN60         IFEQ      *ON
      *
     C     LCNT          IFEQ      3
     C                   ADD       1             LCNT              5 0
     C                   ELSE
     C                   SETON                                        9096
     C                   GOTO      CK3END
     C                   ENDIF
      *
     C                   ELSE
     C                   MOVE      *IN40         SV40              1
     C     SCOLC4        LOOKUP    @N                                     40
     C     *IN40         IFEQ      *OFF
     C                   SETON                                        9096
     C                   GOTO      CK3END
     C                   ENDIF
     C                   MOVE      SV40          *IN40
     C                   ENDIF
      *
     C                   CLEAR                   LL
      *
     C                   SELECT
     C     LCNT          WHENEQ    2
     C                   MOVEA     SCOLCA        LL
     C     LCNT          WHENEQ    3
     C                   MOVEA     SCOLCB        LL
     C     LCNT          WHENEQ    4
     C                   MOVEA     SCOLCC        LL
     C                   ENDSL
      *
     C     13            SUB       LCNT          MAX               3 0
     C                   Z-ADD     MAX           IX                3 0
      * Find 1st non-blank
     C                   DO        MAX
     C     LL(IX)        IFNE      *BLANKS
     C                   LEAVE
     C                   ENDIF
     C                   SUB       1             IX
     C                   ENDDO
      * Check numeric part
      *
      *
     C     LL(1)         IFGT      *ZERO
      *
     C                   DO        IX            IY                3 0
     C     LL(IY)        LOOKUP    @N                                     60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        9093
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
      *
     C                   ELSE
     C                   SETON                                        9093
     C                   ENDIF
      *
     C     *IN90         IFEQ      *ON
     C                   GOTO      CK3END
     C                   ENDIF
     C*
     C                   ELSE                                                   SCLIC# = *Blanks
     C*
     C     SCICN         IFEQ      'IS'
     C     SCICN         OREQ      'HS'
     C     SCICN         OREQ      'AS'
     C*
     C                   SETON                                        9004
     C                   GOTO      CK3END
     C*
     C                   ENDIF                                                  EUICN
     C*
     C                   ENDIF                                                  SCLIC# <> *BLANKS
     C*
     C                   ENDIF                                                  SCTAPE <> 'N'/' '
     C*
       //*** Beg add ***** 07/18/25 *************************************
       If isModEnabled('NCCI Med Data Call');
          // %check return non zero if non numeric value including blank
          invalidPos = %check('0123456789': %trim(scnpi1));
          if invalidPos <> 0 or scnpi1 = *blanks;
             mssnpi = 'MC50007';
             ersnpi = *on;
             *in90 = *on;
             leavesr;
          Endif;
       Endif;
       //*** End add ***** 07/18/25 ************************************
    C*** Beg Add *** 06/27/19 ****************************************
     C                   Endif
    C*** End Add *** 06/27/19 ****************************************
     C*
     C     CK3END        ENDSR
     C*** Beg Add *** 04/17/13 ****************************************
     C*
     C*================================================================
     C* ERRCK4 - Error checking for srceen 4 (SCR04)
     C*================================================================
     C     ERRCK4        BEGSR
     C*
     C     CK4END        ENDSR
     C*================================================================
     C* ERRCK5 - Error checking for srceen 5 (SCR05)
     C*================================================================
     C     ERRCK5        BEGSR
     C*
     C     CK5END        ENDSR
     C*** End Add *** 04/17/13 ****************************************
     C*
     C*================================================================
     C* KJDO - F10=SEARCH.
     C*================================================================
     C     KJDO          BEGSR
     C*
     C*** INITIALIZE POSITION CURSOR INDICATORS
     C*
     C*
     C*
     C                   CLEAR                   HLDIA
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCSPCD'
     C                   MOVEL     SCSPCD        HLSPCD            3
     C                   CLEAR                   ##KEY1
     C                   CALL      'F2335R'
     C                   PARM      '2'           ##CALL
     C                   PARM                    ##KEY1           10
     C                   PARM      ' '           ##RTCD
     C*
     C     ##RTCD        IFNE      '*'
     C                   MOVE      HLSPCD        SCSPCD
     C                   SETON                                        9036      pos cursor
     C                   ELSE
     C                   MOVEL     ##KEY1        SCSPCD
     C                   MOVEL     SCSPCD        ##KEY
     C     SPKY          CHAIN     PSP01                              99
     C  N99              MOVEL     SPDESC        SCSCDS
     C                   SETON                                        9023      pos cursor
     C                   ENDIF
     C*
      /Free
       //***** Bed Add ***** 01/08/15 *******************************
       When hfld = 'SCCNTYCD';
         incall = '2';
         inKC = '0';
         inKL = '0';
         ##rrn = *zeros;
         ##st = 'FL';
         GetCountyCd (incall:inKc:inKl:##rrn:##st);
           if ##rrn <> 0;
             Chain ##rrn PCC02;
               if %found(ftccp);
                 scCntyCd = %subst(cckey:3:3);
                 *in90 = *on;
                 *in37 = *on;
               else;
                 *in90 = *on;
                 indCntyCd = *on;
               endif;
           endif;
       //***** End Add ***** 01/08/15 *******************************
      /End-Free
     C*
     C** PRINCIPAL DIAGNOSIS CODE SEARCH
     C*
     C     HFLD          WHENEQ    'SCDIA1'
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   SCDIA1        HLDIA             5
     C                   MOVEL(P)  SCDIA1        HLDIA             7
     C*** End Chg *** 05/23/13 ***************************************
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxicd10 = '1' and scICdInd ='Y';
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scDIa1 = HLDIA;
             *in90 = *on;
             *in34 = *on;
          Else;
             scDIa1 = outICD10;
             *in90 = *on;
             *in31 = *on;
          Endif;

          If scDia1 <> *blanks;
             kkICd10 = scDIa1;
       //*** Beg Add *** 09/17/15 ****************************************
             kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   scD1ds = ICdsc1;
                Endif;
          Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   CLEAR                   ##ICD9
     C                   eval      *in94 = *off
     C                   CALL      'F2010R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##ICD9
     C                   PARM      'D'           ##LOST
     C*
     C     ##KL          IFEQ      '1'
     C     ##ICD9        OREQ      *BLANKS
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   HLDIA         SCDIA1
     C                   MOVEL(P)  HLDIA         SCDIA1
     C*** End Chg *** 05/23/13 ***************************************
     C                   SETON                                        9034      pos cursor
     C                   ELSE
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   ##ICD9        SCDIA1
     C                   MOVEL(P)  ##ICD9        SCDIA1
     C*** End Chg *** 05/23/13 ***************************************
     C                   SETON                                        9031      pos cursor
     C                   ENDIF
     C*
     C     SCDIA1        IFNE      *BLANKS
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   SCDIA1        KKICD9
     C                   MOVEL(P)  SCDIA1        KKICD9
     C*** End Chg *** 05/23/13 ***************************************
     C                   MOVE      ##LOST        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCD1DS
     C                   ENDIF
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C*
     C* TYPE OF BILL SEARCH
     C*
     C     HFLD          WHENEQ    'SCBLTY'
     C                   Z-ADD     0             ##RRN
     C                   MOVE      SCBLTY        HLBLTY            4
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM      BTTBID        ##TBID
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVE      HLBLTY        SCBLTY
     C                   SETON                                        9031
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCBLTY
     C                   MOVEL     GTDESC        SCBTDS
     C                   SETON                                        9033
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C* TYPE OF ADMISSION
     C*
     C     HFLD          WHENEQ    'SCADTY'
     C                   MOVEL(P)  'FLTA'        ##TBID
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM                    ##TBID
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C     GTSTAT        ANDNE     'I'
     C                   MOVEL     GTKEY         SCADTY
     C                   MOVEL     GTDESC        SCATDS
     C                   SETON                                        9041
     C*
     C                   ELSE
     C                   SETON                                        9039
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C                   SETON                                        9039
     C*
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C* TYPE OF SERVICE SEARCHES
     C*
     C     HFLD          WHENEQ    'SCICN'
     C                   Z-ADD     0             ##RRN
     C                   MOVE      SCICN         HLICN
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM      'SVTY'        ##TBID
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVE      HLICN         SCICN
     C                   SETON                                        9032
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCICN
     C                   MOVEL     GTDESC        SCICDS
     C                   SETON                                        9035
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C     HFLD          WHENEQ    'SCDCN'
     C                   Z-ADD     0             ##RRN
     C                   MOVE      SCDCN         HLDCN
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM      'SVTZ'        ##TBID
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVE      HLDCN         SCDCN
     C                   SETON                                        9035
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCDCN
     C                   MOVEL     GTDESC        SCDCDS
     C                   SETON                                        905697
     C                   SETOFF                                       57
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C* PROCEDURE CODE METHOD SEARCH
     C*
     C     HFLD          WHENEQ    'SCPCM'
     C                   MOVEL     SCPCM         HLPCM
     C                   MOVEL     SCPDSC        HLDESC
     C*
     C     EDITST        CAT(P)    'PM':0        PMTBID            4
     C                   MOVEL(P)  PMTBID        KKTBID
     C     GTKY2         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'ZZPM'        KKTBID
     C                   ENDIF
     C*
     C     GTKY2         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'PCMT'        KKTBID
     C                   ENDIF
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      '0'           ##KC
     C                   PARM      '0'           ##KL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      KKTBID        ##TBID
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVEL     HLPCM         SCPCM
     C                   MOVEL     HLDESC        SCPDSC
     C                   SETON                                        9046
     C                   ENDIF
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C                   MOVEL(P)  GTKEY         SCPCM
     C                   MOVEL(P)  GTDESC        SCPDSC
     C                   SETON                                        9039      pos cursor
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C* PROCEDURE CODE SEARCHES
     C*
     C     HFLD          WHENEQ    'SCPRC1'
     C     HFLD          OREQ      'SCLOS1'
     C*
     C* PROCEDURE CODE MUST BE ENTERED TO DO THIS SEARCH
     C*
     C     SCPCM         IFEQ      *BLANKS
     C                   SETON                                        9050
     C                   ENDIF
     C*
     C     SCPCM         IFEQ      '0'
     C                   SETON                                        9032
     C                   ENDIF                                                  SCPCM = '0'
     C*
     C*
      *
      *  DID NOT MAKE A SELECTION
      *
     C*
     C     SCPCM         IFEQ      '4'
     C     SCPCM         OREQ      '5'
     C                   MOVEL     SCPRC1        HLPRC1
     C                   MOVEL     SCP1DS        HLP1DS
     C                   MOVE      SCAST         ##ST              2
     C                   MOVE      *BLANKS       ##CPT4           15
     C                   MOVE      *BLANKS       ##DESC           40
     C                   CALL      'F2305R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##ST
     C                   PARM                    ##CPT4
     C                   PARM                    ##DESC
     C*
     C     ##KL          IFEQ      '1'
     C     ##CPT4        OREQ      *BLANKS
     C                   MOVEL(P)  HLPRC1        SCPRC1
     C                   MOVEL(P)  HLP1DS        SCP1DS
     C                   SETON                                        9039
     C                   ELSE
     C                   MOVEL(P)  ##CPT4        SCPRC1
     C                   MOVEL(P)  ##DESC        SCP1DS
     C                   MOVE      *BLANKS       SCLOS1
     C                   SETON                                        9040
     C                   ENDIF                                                  ##KL = '1'
     C                   ENDIF                                                  SCPCM = '4'
     C*
     C     SCPCM         IFEQ      '9'
     C                   MOVEL     SCPRC1        HLPRC1            7
     C                   MOVEL     SCLOS1        HLLOS1            1
     C                   MOVEL     SCP1DS        HLP1DS           40
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxicd10 = '1' and scICdInd ='Y';
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'O';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scPRc1 = HLPRC1;
             scLOs1 = HLLOS1;
             scP1ds = HLP1DS;
             *in90 = *on;
             *in39 = *on;
          Else;
             scPRc1 = outICD10;
             *in90 = *on;
             *in40 = *on;
          Endif;

          If scPRc1 <> *blanks;
             kkICd10 = scPRc1;
       //*** Beg Add *** 09/17/15 ****************************************
             KKLost = 'O';
       //*** End Add *** 09/17/15 ****************************************
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   scP1ds = ICdsc1;
                Endif;
          Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   MOVE      *BLANKS       ##ICD9
     C                   CALL      'F2010R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##ICD9
     C                   PARM      'O'           ##LOST
     C*
     C     ##ICD9        IFEQ      *BLANKS
     C                   MOVEL(P)  HLPRC1        SCPRC1
     C                   MOVEL     HLLOS1        SCLOS1
     C                   MOVEL(P)  HLP1DS        SCP1DS
     C                   SETON                                        9039
     C                   ELSE
     C                   MOVEL(P)  ##ICD9        SCPRC1
     C                   MOVE      ##LOST        SCLOS1
     C                   MOVEL(P)  SCPRC1        KKICD9
     C                   MOVE      SCLOS1        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP1DS
     C                   ENDIF                                                  *INPP = *OFF
     C                   SETON                                        9040
     C                   ENDIF                                                  ##ICD9 = *BLANKS
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   ENDIF                                                  SCPCM = '9'
     C*
     C     HFLD          WHENEQ    'SCPRC2'
     C     HFLD          OREQ      'SCLOS2'
     C*
     C* PROCEDURE CODE MUST BE ENTERED TO DO THIS SEARCH
     C*
     C     SCPCM         IFEQ      *BLANKS
     C                   SETON                                        9053
     C                   ENDIF
     C*
     C     SCPCM         IFEQ      '0'
     C                   SETON                                        9033
     C                   ENDIF                                                  SCPCM = '0'
     C*
     C*
      *
      *  DID NOT MAKE A SELECTION
      *
     C*
     C     SCPCM         IFEQ      '4'
     C     SCPCM         OREQ      '5'
     C                   MOVEL     SCPRC2        HLPRC2
     C                   MOVEL     SCP2DS        HLP2DS
     C                   MOVE      SCAST         ##ST              2
     C                   MOVE      *BLANKS       ##CPT4           15
     C                   MOVE      *BLANKS       ##DESC           40
     C                   CALL      'F2305R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##ST
     C                   PARM                    ##CPT4
     C                   PARM                    ##DESC
     C*
     C     ##KL          IFEQ      '1'
     C     ##CPT4        OREQ      *BLANKS
     C                   MOVEL(P)  HLPRC2        SCPRC2
     C                   MOVEL(P)  HLP2DS        SCP2DS
     C                   SETON                                        9040
     C                   ELSE
     C                   MOVEL(P)  ##CPT4        SCPRC2
     C                   MOVEL(P)  ##DESC        SCP2DS
     C                   MOVE      *BLANKS       SCLOS2
     C                   SETON                                        9011
     C                   ENDIF                                                  ##KL = '1'
     C                   ENDIF                                                  SCPCM = '4'
     C*
     C     SCPCM         IFEQ      '9'
     C                   MOVEL     SCPRC2        HLPRC2            7
     C                   MOVE      SCLOS2        HLLOS2            1
     C                   MOVEL     SCP2DS        HLP2DS           40
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxicd10 = '1' and scICdInd ='Y';
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'O';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scPRc2 = HLPRC2;
             scLOs2 = HLLOS2;
             scP2ds = HLP2DS;
             *in90 = *on;
             *in40 = *on;
          Else;
             scPRc2 = outICD10;
             *in90 = *on;
             *in11 = *on;
          Endif;

          If scPRc2 <> *blanks;
             kkICd10 = scPRc2;
       //*** Beg Add *** 09/17/15 ****************************************
             kkLost = 'O';
       //*** End Add *** 09/17/15 ****************************************
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   scP2ds = ICdsc1;
                Endif;
          Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   MOVE      *BLANKS       ##ICD9
     C                   CALL      'F2010R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##ICD9
     C                   PARM      'O'           ##LOST
     C*
     C     ##ICD9        IFEQ      *BLANKS
     C     ##KL          OREQ      '1'
     C                   MOVEL(P)  HLPRC2        SCPRC2
     C                   MOVEL     HLLOS2        SCLOS2
     C                   MOVEL(P)  HLP2DS        SCP2DS
     C                   SETON                                        9040
     C                   ELSE
     C                   MOVEL(P)  ##ICD9        SCPRC2
     C                   MOVE      ##LOST        SCLOS2
     C                   MOVEL(P)  SCPRC2        KKICD9
     C                   MOVE      SCLOS2        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP2DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   SETON                                        9011
     C                   ENDIF                                                  ##ICD9 = *BLANKS
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   ENDIF                                                  SCPCM = '9'
     C*
     C*
     C     HFLD          WHENEQ    'SCPRC3'
     C     HFLD          OREQ      'SCLOS3'
     C*
     C* PROCEDURE CODE MUST BE ENTERED TO DO THIS SEARCH
     C*
     C     SCPCM         IFEQ      *BLANKS
     C                   SETON                                        9013
     C                   ENDIF
     C*
     C     SCPCM         IFEQ      '0'
     C                   SETON                                        9015
     C                   ENDIF                                                  SCPCM = '0'
     C*
     C     SCPCM         IFEQ      '4'
     C     SCPCM         OREQ      '5'
     C                   MOVEL     SCPRC3        HLPRC3
     C                   MOVEL     SCP3DS        HLP3DS
     C                   MOVE      SCAST         ##ST              2
     C                   MOVE      *BLANKS       ##CPT4           15
     C                   MOVE      *BLANKS       ##DESC           40
     C                   CALL      'F2305R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##ST
     C                   PARM                    ##CPT4
     C                   PARM                    ##DESC
     C*
     C     ##KL          IFEQ      '1'
     C     ##CPT4        OREQ      *BLANKS
     C                   MOVEL(P)  HLPRC3        SCPRC3
     C                   MOVEL(P)  HLP3DS        SCP3DS
     C                   SETON                                        9011
     C                   ELSE
     C                   MOVEL(P)  ##CPT4        SCPRC3
     C                   MOVEL(P)  ##DESC        SCP3DS
     C                   MOVE      *BLANKS       SCLOS3
     C                   SETON                                        9017
     C                   ENDIF                                                  ##KL = '1'
     C                   ENDIF                                                  SCPCM = '4'
     C*
     C     SCPCM         IFEQ      '9'
     C                   MOVEL     SCPRC3        HLPRC3            7
     C                   MOVE      SCLOS3        HLLOS3            1
     C                   MOVEL     SCP3DS        HLP3DS           40
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxicd10 = '1' and scICdInd ='Y';
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'O';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scPRc3 = HLPRC3;
             scLOs3 = HLLOS3;
             scP3ds = HLP3DS;
             *in90 = *on;
             *in11 = *on;
          Else;
             scPRc3 = outICD10;
             *in90 = *on;
             *in17 = *on;
          Endif;

          If scPRc3 <> *blanks;
             kkICd10 = scPRc3;
       //*** Beg Add *** 09/17/15 ****************************************
             kkLost = 'O';
       //*** End Add *** 09/17/15 ****************************************
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   scP3ds = ICdsc1;
                Endif;
          Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   MOVE      *BLANKS       ##ICD9
     C                   CALL      'F2010R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##ICD9
     C                   PARM      'O'           ##LOST
     C*
     C     ##ICD9        IFEQ      *BLANKS
     C     ##KL          OREQ      '1'
     C                   MOVEL(P)  HLPRC3        SCPRC3
     C                   MOVEL     HLLOS3        SCLOS3
     C                   MOVEL(P)  HLP3DS        SCP3DS
     C                   SETON                                        9011
     C                   ELSE
     C                   MOVEL(P)  ##ICD9        SCPRC3
     C                   MOVE      ##LOST        SCLOS3
     C                   MOVEL(P)  SCPRC3        KKICD9
     C                   MOVE      SCLOS3        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP3DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   SETON                                        9017
     C                   ENDIF                                                  ##ICD9 = *BLANKS
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   ENDIF                                                  SCPCM = '9'
     C*
     C     HFLD          WHENEQ    'SCPRC4'
     C     HFLD          OREQ      'SCLOS4'
     C*
     C* PROCEDURE CODE MUST BE ENTERED TO DO THIS SEARCH
     C*
     C     SCPCM         IFEQ      *BLANKS
     C                   SETON                                        9019
     C                   ENDIF
     C*
     C     SCPCM         IFEQ      '0'
     C                   SETON                                        9021
     C                   ENDIF                                                  SCPCM = '0'
     C*
     C     SCPCM         IFEQ      '4'
     C     SCPCM         OREQ      '5'
     C                   MOVEL     SCPRC4        HLPRC4
     C                   MOVEL     SCP4DS        HLP4DS
     C                   MOVE      SCAST         ##ST
     C                   MOVE      *BLANKS       ##CPT4
     C                   MOVE      *BLANKS       ##DESC
     C                   CALL      'F2305R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##ST
     C                   PARM                    ##CPT4
     C                   PARM                    ##DESC
     C*
     C     ##KL          IFEQ      '1'
     C     ##CPT4        OREQ      *BLANKS
     C                   MOVEL(P)  HLPRC4        SCPRC4
     C                   MOVEL(P)  HLP4DS        SCP4DS
     C                   SETON                                        9017
     C                   ELSE
     C                   MOVEL(P)  ##CPT4        SCPRC4
     C                   MOVEL(P)  ##DESC        SCP4DS
     C                   MOVE      *BLANKS       SCLOS4
     C                   SETON                                        9022
     C                   ENDIF                                                  ##KL = '1'
     C                   ENDIF                                                  SCPCM = '4'
     C*
     C     SCPCM         IFEQ      '9'
     C                   MOVEL     SCPRC4        HLPRC4            7
     C                   MOVE      SCLOS4        HLLOS4            1
     C                   MOVEL     SCP4DS        HLP4DS           40
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxicd10 = '1' and scICdInd ='Y';
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'O';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scPRc4 = HLPRC4;
             scLOs4 = HLLOS4;
             scP4ds = HLP4DS;
             *in90 = *on;
             *in17 = *on;
          Else;
             scPRc4 = outICD10;
             *in90 = *on;
             *in22 = *on;
          Endif;

          If scPRc4 <> *blanks;
             kkICd10 = scPRc4;
       //*** Beg Add *** 09/17/15 ****************************************
             kkLost = 'O';
       //*** End Add *** 09/17/15 ****************************************
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   scP4ds = ICdsc1;
                Endif;
          Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   MOVE      *BLANKS       ##ICD9
     C                   CALL      'F2010R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##ICD9
     C                   PARM      'O'           ##LOST
     C*
     C     ##ICD9        IFEQ      *BLANKS
     C     ##KL          OREQ      '1'
     C                   MOVEL(P)  HLPRC4        SCPRC4
     C                   MOVEL     HLLOS4        SCLOS4
     C                   MOVEL(P)  HLP4DS        SCP4DS
     C                   SETON                                        9017
     C                   ELSE
     C                   MOVEL(P)  ##ICD9        SCPRC4
     C                   MOVE      ##LOST        SCLOS4
     C                   MOVEL(P)  SCPRC4        KKICD9
     C                   MOVE      SCLOS4        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP4DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   SETON                                        9022
     C                   ENDIF                                                  ##ICD9 = *BLANKS
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   ENDIF                                                  SCPCM = '9'
     C*
     C     HFLD          WHENEQ    'SCPRC5'
     C     HFLD          OREQ      'SCLOS5'
     C*
     C* PROCEDURE CODE MUST BE ENTERED TO DO THIS SEARCH
     C*
     C     SCPCM         IFEQ      *BLANKS
     C                   SETON                                        9024
     C                   ENDIF
     C*
     C     SCPCM         IFEQ      '0'
     C                   SETON                                        9026
     C                   ENDIF                                                  SCPCM = '0'
     C*
     C     SCPCM         IFEQ      '4'
     C     SCPCM         OREQ      '5'
     C                   MOVEL     SCPRC5        HLPRC5
     C                   MOVEL     SCP5DS        HLP5DS
     C                   MOVE      SCAST         ##ST
     C                   MOVE      *BLANKS       ##CPT4
     C                   MOVE      *BLANKS       ##DESC
     C                   CALL      'F2305R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##ST
     C                   PARM                    ##CPT4
     C                   PARM                    ##DESC
     C*
     C     ##KL          IFEQ      '1'
     C     ##CPT4        OREQ      *BLANKS
     C                   MOVEL(P)  HLPRC5        SCPRC5
     C                   MOVEL(P)  HLP5DS        SCP5DS
     C                   SETON                                        9022
     C                   ELSE
     C                   MOVEL(P)  ##CPT4        SCPRC5
     C                   MOVEL(P)  ##DESC        SCP5DS
     C                   MOVE      *BLANKS       SCLOS5
     C                   SETON                                        9027
     C                   ENDIF                                                  ##KL = '1'
     C                   ENDIF                                                  SCPCM = '4'
     C*
     C     SCPCM         IFEQ      '9'
     C                   MOVEL     SCPRC5        HLPRC5            7
     C                   MOVE      SCLOS5        HLLOS5            1
     C                   MOVEL     SCP5DS        HLP5DS           40
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxicd10 = '1' and scICdInd ='Y';
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'O';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scPRc5 = HLPRC5;
             scLOs5 = HLLOS5;
             scP5ds = HLP5DS;
             *in90 = *on;
             *in22 = *on;
          Else;
             scPRc5 = outICD10;
             *in90 = *on;
             *in27 = *on;
          Endif;

          If scPRc5 <> *blanks;
             kkICd10 = scPRc5;
       //*** Beg Add *** 09/17/15 ****************************************
             kkLost = 'O';
       //*** End Add *** 09/17/15 ****************************************
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   scP5ds = ICdsc1;
                Endif;
          Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   MOVE      *BLANKS       ##ICD9
     C                   CALL      'F2010R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##ICD9
     C                   PARM      'O'           ##LOST
     C*
     C     ##ICD9        IFEQ      *BLANKS
     C     ##KL          OREQ      '1'
     C                   MOVEL(P)  HLPRC5        SCPRC5
     C                   MOVEL     HLLOS5        SCLOS5
     C                   MOVEL(P)  HLP5DS        SCP5DS
     C                   SETON                                        9022
     C                   ELSE
     C                   MOVEL(P)  ##ICD9        SCPRC5
     C                   MOVE      ##LOST        SCLOS5
     C                   MOVEL(P)  SCPRC5        KKICD9
     C                   MOVE      SCLOS5        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP5DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   SETON                                        9027
     C                   ENDIF                                                  ##ICD9 = *BLANKS
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   ENDIF                                                  SCPCM = '9'
     C*
     C     HFLD          WHENEQ    'SCPRC6'
     C     HFLD          OREQ      'SCLOS6'
     C*
     C* PROCEDURE CODE MUST BE ENTERED TO DO THIS SEARCH
     C*
     C     SCPCM         IFEQ      *BLANKS
     C                   SETON                                        9035
     C                   ENDIF
     C*
     C     SCPCM         IFEQ      '0'
     C                   SETON                                        9037
     C                   ENDIF                                                  SCPCM = '0'
     C*
     C     SCPCM         IFEQ      '4'
     C     SCPCM         OREQ      '5'
     C                   MOVEL     SCPRC6        HLPRC6
     C                   MOVEL     SCP6DS        HLP6DS
     C                   MOVE      SCAST         ##ST
     C                   MOVE      *BLANKS       ##CPT4
     C                   MOVE      *BLANKS       ##DESC
     C                   CALL      'F2305R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##ST
     C                   PARM                    ##CPT4
     C                   PARM                    ##DESC
     C*
     C     ##KL          IFEQ      '1'
     C     ##CPT4        OREQ      *BLANKS
     C                   MOVEL(P)  HLPRC6        SCPRC6
     C                   MOVEL(P)  HLP6DS        SCP6DS
     C                   SETON                                        9027
     C                   ELSE
     C                   MOVEL(P)  ##CPT4        SCPRC6
     C                   MOVEL(P)  ##DESC        SCP6DS
     C                   MOVE      *BLANKS       SCLOS6
     C                   SETON                                        9043
     C                   ENDIF                                                  ##KL = '1'
     C                   ENDIF                                                  SCPCM = '4'
     C*
     C     SCPCM         IFEQ      '9'
     C                   MOVEL     SCPRC6        HLPRC6            7
     C                   MOVE      SCLOS6        HLLOS6            1
     C                   MOVEL     SCP6DS        HLP6DS           40
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxicd10 = '1' and scICdInd ='Y';
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'O';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scPRc6 = HLPRC6;
             scLOs6 = HLLOS6;
             scP6ds = HLP6DS;
             *in90 = *on;
             *in27 = *on;
          Else;
             scPRc6 = outICD10;
             *in90 = *on;
             *in43 = *on;
          Endif;

          If scPRc6 <> *blanks;
             kkICd10 = scPRc6;
       //*** Beg Add *** 09/17/15 ****************************************
             kkLost = 'O';
       //*** End Add *** 09/17/15 ****************************************
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   scP6ds = ICdsc1;
                Endif;
          Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   MOVE      *BLANKS       ##ICD9
     C                   CALL      'F2010R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##ICD9
     C                   PARM      'O'           ##LOST
     C*
     C     ##ICD9        IFEQ      *BLANKS
     C     ##KL          OREQ      '1'
     C                   MOVEL(P)  HLPRC6        SCPRC6
     C                   MOVEL     HLLOS6        SCLOS6
     C                   MOVEL(P)  HLP6DS        SCP6DS
     C                   SETON                                        9027
     C                   ELSE
     C                   MOVEL(P)  ##ICD9        SCPRC6
     C                   MOVE      ##LOST        SCLOS6
     C                   MOVEL(P)  SCPRC6        KKICD9
     C                   MOVE      SCLOS6        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP6DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   SETON                                        9043
     C                   ENDIF                                                  ##ICD9 = *BLANKS
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   ENDIF                                                  SCPCM = '9'
     C*
     C*
     C*
     C* OTHER DIAGNOSIS CODES SEARCHES
     C*
     C     HFLD          WHENEQ    'SCDIA2'
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   SCDIA2        HLDIA             5
     C                   MOVEL(P)  SCDIA2        HLDIA             7
     C*** End Chg *** 05/23/13 ***************************************
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxicd10 = '1' and scICdInd ='Y';
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scDIa2 = HLDIA;
             *in90 = *on;
             *in43 = *on;
          Else;
             scDIa2 = outICD10;
             *in90 = *on;
             *in44 = *on;
          Endif;

          If scDia2 <> *blanks;
             kkICd10 = scDIa2;
       //*** Beg Add *** 09/17/15 ****************************************
             kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   scD2ds = ICdsc1;
                Endif;
          Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   CLEAR                   ##ICD9
     C                   CALL      'F2010R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##ICD9
     C                   PARM      'D'           ##LOST
     C*
     C     ##KL          IFEQ      '1'
     C     ##ICD9        OREQ      *BLANKS
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   HLDIA         SCDIA2
     C                   MOVEL(P)  HLDIA         SCDIA2
     C*** End Chg *** 05/23/13 ***************************************
     C                   SETON                                        9043      pos cursor
     C                   ELSE
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   ##ICD9        SCDIA2
     C                   MOVEL(P)  ##ICD9        SCDIA2
     C*** End Chg *** 05/23/13 ***************************************
     C                   SETON                                        9044      pos cursor
     C                   ENDIF
     C*
     C     SCDIA2        IFNE      *BLANKS
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   SCDIA2        KKICD9
     C                   MOVEL(P)  SCDIA2        KKICD9
     C*** End Chg *** 05/23/13 ***************************************
     C                   MOVE      ##LOST        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCD2DS
     C                   ENDIF
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C*
     C     HFLD          WHENEQ    'SCDIA3'
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   SCDIA3        HLDIA             5
     C                   MOVEL(P)  SCDIA3        HLDIA             7
     C*** End Chg *** 05/23/13 ***************************************
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxicd10 = '1' and scICdInd ='Y';
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scDIa3 = HLDIA;
             *in90 = *on;
             *in44 = *on;
          Else;
             scDIa3 = outICD10;
             *in90 = *on;
             *in45 = *on;
          Endif;

          If scDia3 <> *blanks;
             kkICd10 = scDIa3;
       //*** Beg Add *** 09/17/15 ****************************************
             kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   scD3ds = ICdsc1;
                Endif;
          Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   CLEAR                   ##ICD9
     C                   CALL      'F2010R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##ICD9
     C                   PARM      'D'           ##LOST
     C*
     C     ##KL          IFEQ      '1'
     C     ##ICD9        OREQ      *BLANKS
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   HLDIA         SCDIA3
     C                   MOVEL(P)  HLDIA         SCDIA3
     C*** End Chg *** 05/23/13 ***************************************
     C                   SETON                                        9044      pos cursor
     C                   ELSE
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   ##ICD9        SCDIA3
     C                   MOVEL(P)  ##ICD9        SCDIA3
     C*** End Chg *** 05/23/13 ***************************************
     C                   SETON                                        9045      pos cursor
     C                   ENDIF
     C*
     C     SCDIA3        IFNE      *BLANKS
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   SCDIA3        KKICD9
     C                   MOVEL(P)  SCDIA3        KKICD9
     C*** End Chg *** 05/23/13 ***************************************
     C                   MOVE      ##LOST        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCD3DS
     C                   ENDIF
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C*
     C     HFLD          WHENEQ    'SCDIA4'
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   SCDIA4        HLDIA             5
     C                   MOVEL(P)  SCDIA4        HLDIA             7
     C*** End Chg *** 05/23/13 ***************************************
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxicd10 = '1' and scICdInd ='Y';
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scDIa4 = HLDIA;
             *in90 = *on;
             *in45 = *on;
          Else;
             scDIa4 = outICD10;
             *in90 = *on;
             *in45 = *on;
          Endif;

          If scDia4 <> *blanks;
             kkICd10 = scDIa4;
       //*** Beg Add *** 09/17/15 ****************************************
             kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   scD4ds = ICdsc1;
                Endif;
          Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   CLEAR                   ##ICD9
     C                   CALL      'F2010R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##ICD9
     C                   PARM      'D'           ##LOST
     C*
     C     ##KL          IFEQ      '1'
     C     ##ICD9        OREQ      *BLANKS
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   HLDIA         SCDIA4
     C                   MOVEL(P)  HLDIA         SCDIA4
     C*** End Chg *** 05/23/13 ***************************************
     C                   SETON                                        9045      pos cursor
     C                   ELSE
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   ##ICD9        SCDIA4
     C                   MOVEL(P)  ##ICD9        SCDIA4
     C*** End Chg *** 05/23/13 ***************************************
     C                   SETON                                        9045      pos cursor
     C                   ENDIF
     C*
     C     SCDIA4        IFNE      *BLANKS
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   SCDIA4        KKICD9
     C                   MOVEL(P)  SCDIA4        KKICD9
     C*** End Chg *** 05/23/13 ***************************************
     C                   MOVE      ##LOST        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCD4DS
     C                   ENDIF
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C*
     C*
     C     HFLD          WHENEQ    'SCDIA5'
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   SCDIA5        HLDIA             5
     C                   MOVEL(P)  SCDIA5        HLDIA             7
     C*** End Chg *** 05/23/13 ***************************************
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxicd10 = '1' and scICdInd ='Y';
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scDIa5 = HLDIA;
             *in90 = *on;
             *in37 = *on;
          Else;
             scDIa5 = outICD10;
             *in90 = *on;
             *in95 = *on;
          Endif;

          If scDia5 <> *blanks;
             kkICd10 = scDIa5;
       //*** Beg Add *** 09/17/15 ****************************************
             kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   scD5ds = ICdsc1;
                Endif;
          Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   CLEAR                   ##ICD9
     C                   CALL      'F2010R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##ICD9
     C                   PARM      'D'           ##LOST
     C*
     C     ##KL          IFEQ      '1'
     C     ##ICD9        OREQ      *BLANKS
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   HLDIA         SCDIA5
     C                   MOVEL(P)  HLDIA         SCDIA5
     C*** End Chg *** 05/23/13 ***************************************
     C*                    SETON                     9031  pos cursor
     C                   SETON                                        9037      pos cursor
     C                   ELSE
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   ##ICD9        SCDIA5
     C                   MOVEL(P)  ##ICD9        SCDIA5
     C*** End Chg *** 05/23/13 ***************************************
     C*                    SETON                     9033  pos cursor
     C                   SETON                                        9095      pos cursor
     C                   ENDIF
     C*
     C     SCDIA5        IFNE      *BLANKS
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   SCDIA5        KKICD9
     C                   MOVEL(P)  SCDIA5        KKICD9
     C*** End Chg *** 05/23/13 ***************************************
     C                   MOVE      ##LOST        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCD5DS
     C                   ENDIF
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C*
     C     HFLD          WHENEQ    'SCDIA6'
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   SCDIA6        HLDIA             5
     C                   MOVEL(P)  SCDIA6        HLDIA             7
     C*** End Chg *** 05/23/13 ***************************************
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxicd10 = '1' and scICdInd ='Y';
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scDIa6 = HLDIA;
             *in90 = *on;
             *in95 = *on;
          Else;
             scDIa6 = outICD10;
             *in90 = *on;
             *in35 = *on;
          Endif;

          If scDia6 <> *blanks;
             kkICd10 = scDIa6;
       //*** Beg Add *** 09/17/15 ****************************************
             kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   scD6ds = ICdsc1;
                Endif;
          Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   CLEAR                   ##ICD9
     C                   CALL      'F2010R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##ICD9
     C                   PARM      'D'           ##LOST
     C*
     C     ##KL          IFEQ      '1'
     C     ##ICD9        OREQ      *BLANKS
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   HLDIA         SCDIA6
     C                   MOVEL(P)  HLDIA         SCDIA6
     C*** End Chg *** 05/23/13 ***************************************
     C*                    SETON                     9033  pos cursor
     C                   SETON                                        9095      pos cursor
     C                   ELSE
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   ##ICD9        SCDIA6
     C                   MOVEL(P)  ##ICD9        SCDIA6
     C*** End Chg *** 05/23/13 ***************************************
     C                   SETON                                        9035      pos cursor
     C                   ENDIF
     C*
     C     SCDIA6        IFNE      *BLANKS
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   SCDIA6        KKICD9
     C                   MOVEL(P)  SCDIA6        KKICD9
     C*** End Chg *** 05/23/13 ***************************************
     C                   MOVE      ##LOST        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCD6DS
     C                   ENDIF
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C*
     C     HFLD          WHENEQ    'SCDIA7'
     C*** Beg Chg *** 05/23/13 ***************************************
     C*********          MOVE(P)   SCDIA7        HLDIA             5
     C                   MOVEL(P)  SCDIA7        HLDIA             7
     C*** End Chg *** 05/23/13 ***************************************
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxicd10 = '1' and scICdInd ='Y';
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scDIa7 = HLDIA;
             *in90 = *on;
             *in35 = *on;
          Else;
             scDIa7 = outICD10;
             *in90 = *on;
             *in38 = *on;
          Endif;

          If scDia7 <> *blanks;
             kkICd10 = scDIa7;
       //*** Beg Add *** 09/17/15 ****************************************
             kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   scD7ds = ICdsc1;
                Endif;
          Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   CLEAR                   ##ICD9
     C                   CALL      'F2010R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##ICD9
     C                   PARM      'D'           ##LOST
     C*
     C     ##KL          IFEQ      '1'
     C     ##ICD9        OREQ      *BLANKS
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   HLDIA         SCDIA7
     C                   MOVEL(P)  HLDIA         SCDIA7
     C*** End Chg *** 05/23/13 ***************************************
     C                   SETON                                        9035      pos cursor
     C                   ELSE
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   ##ICD9        SCDIA7
     C                   MOVEL(P)  ##ICD9        SCDIA7
     C*** End Chg *** 05/23/13 ***************************************
     C*                    SETON                     9037  pos cursor
     C                   SETON                                        9038      pos cursor
     C                   ENDIF
     C*
     C     SCDIA7        IFNE      *BLANKS
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   SCDIA7        KKICD9
     C                   MOVEL(P)  SCDIA7        KKICD9
     C*** End Chg *** 05/23/13 ***************************************
     C                   MOVE      ##LOST        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCD7DS
     C                   ENDIF
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C*
     C     HFLD          WHENEQ    'SCDIA8'
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   SCDIA8        HLDIA             5
     C                   MOVEL(P)  SCDIA8        HLDIA             7
     C*** End Chg *** 05/23/13 ***************************************
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxicd10 = '1' and scICdInd ='Y';
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scDIa8 = HLDIA;
             *in90 = *on;
             *in38 = *on;
          Else;
             scDIa8 = outICD10;
             *in90 = *on;
             *in31 = *on;
          Endif;

          If scDia8 <> *blanks;
             kkICD10 = scDIa8;
       //*** Beg Add *** 09/17/15 ****************************************
             kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   scD8ds = ICdsc1;
                Endif;
          Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   CLEAR                   ##ICD9
     C                   CALL      'F2010R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##ICD9
     C                   PARM      'D'           ##LOST
     C*
     C     ##KL          IFEQ      '1'
     C     ##ICD9        OREQ      *BLANKS
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   HLDIA         SCDIA8
     C                   MOVEL(P)  HLDIA         SCDIA8
     C*** End Chg *** 05/23/13 ***************************************
     C                   SETON                                        9038      pos cursor
     C                   ELSE
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   ##ICD9        SCDIA8
     C                   MOVEL(P)  ##ICD9        SCDIA8
     C*** End Chg *** 05/23/13 ***************************************
     C*                    SETON                     9037  pos cursor
     C                   SETON                                        9031      pos cursor
     C                   ENDIF
     C*
     C     SCDIA8        IFNE      *BLANKS
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   SCDIA8        KKICD9
     C                   MOVEL(P)  SCDIA8        KKICD9
     C*** End Chg *** 05/23/13 ***************************************
     C                   MOVE      ##LOST        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCD8DS
     C                   ENDIF
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C*
     C     HFLD          WHENEQ    'SCCAU1'
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   SCCAU1        HLDIA             5
     C                   MOVEL(P)  SCCAU1        HLDIA             7
     C*** End Chg *** 05/23/13 ***************************************
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxicd10 = '1' and scICdInd ='Y';
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scCAu1 = HLDIA;
             *in90 = *on;
             *in31 = *on;
          Else;
             scCAu1 = outICD10;
             *in90 = *on;
             *in39 = *on;
          Endif;

          If scCAu1 <> *blanks;
             kkICD10 = scCAu1;
       //*** Beg Add *** 09/17/15 ****************************************
             kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   scC1ds = ICdsc1;
                Endif;
          Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   CLEAR                   ##ICD9
     C                   CALL      'F2010R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##ICD9
     C                   PARM      'D'           ##LOST
     C*
     C     ##KL          IFEQ      '1'
     C     ##ICD9        OREQ      *BLANKS
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   HLDIA         SCCAU1
     C                   MOVEL(P)  HLDIA         SCCAU1
     C*** End Chg *** 05/23/13 ***************************************
     C*                    SETON                     9037  pos cursor
     C                   SETON                                        9031      pos cursor
     C                   ELSE
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   ##ICD9        SCCAU1
     C                   MOVEL(P)  ##ICD9        SCCAU1
     C*** End Chg *** 05/23/13 ***************************************
     C                   SETON                                        9039      pos cursor
     C                   ENDIF
     C*
     C     SCCAU1        IFNE      *BLANKS
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   SCCAU1        KKICD9
     C                   MOVEL(P)  SCCAU1        KKICD9
     C*** End Chg *** 05/23/13 ***************************************
     C                   MOVE      ##LOST        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCC1DS
     C                   ENDIF
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C*
     C     HFLD          WHENEQ    'SCCAU2'
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   SCCAU2        HLDIA             5
     C                   MOVEL(P)  SCCAU2        HLDIA             7
     C*** End Chg *** 05/23/13 ***************************************
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxicd10 = '1' and scICdInd ='Y';
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scCAu2 = HLDIA;
             *in90 = *on;
             *in39 = *on;
          Else;
             scCAu2 = outICD10;
             *in90 = *on;
             *in40 = *on;
          Endif;

          If scCAu2 <> *blanks;
             kkICD10 = scCAu2;
       //*** Beg Add *** 09/17/15 ****************************************
             kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   scC2ds = ICdsc1;
                Endif;
          Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   CLEAR                   ##ICD9
     C                   CALL      'F2010R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##ICD9
     C                   PARM      'D'           ##LOST
     C*
     C     ##KL          IFEQ      '1'
     C     ##ICD9        OREQ      *BLANKS
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   HLDIA         SCCAU2
     C                   MOVEL(P)  HLDIA         SCCAU2
     C*** End Chg *** 05/23/13 ***************************************
     C                   SETON                                        9039      pos cursor
     C                   ELSE
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   ##ICD9        SCCAU2
     C                   MOVEL(P)  ##ICD9        SCCAU2
     C*** End Chg *** 05/23/13 ***************************************
     C                   SETON                                        9040      pos cursor
     C                   ENDIF
     C*
     C     SCCAU2        IFNE      *BLANKS
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   SCCAU2        KKICD9
     C                   MOVEL(P)  SCCAU2        KKICD9
     C*** End Chg *** 05/23/13 ***************************************
     C                   MOVE      ##LOST        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCC2DS
     C                   ENDIF
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C*
     C     HFLD          WHENEQ    'SCCAU3'
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   SCCAU3        HLDIA             5
     C                   MOVEL(P)  SCCAU3        HLDIA             7
     C*** End Chg *** 05/23/13 ***************************************
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxicd10 = '1' and scICdInd ='Y';
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scCAu3 = HLDIA;
             *in90 = *on;
             *in39 = *on;
          Else;
             scCAu3 = outICD10;
             *in90 = *on;
             *in40 = *on;
          Endif;

          If scCAu3 <> *blanks;
             kkICD10 = scCAu3;
       //*** Beg Add *** 09/17/15 ****************************************
             kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   scC3DS = ICdsc1;
                Endif;
          Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   CLEAR                   ##ICD9
     C                   CALL      'F2010R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##ICD9
     C                   PARM      'D'           ##LOST
     C*
     C     ##KL          IFEQ      '1'
     C     ##ICD9        OREQ      *BLANKS
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   HLDIA         SCCAU3
     C                   MOVEL(P)  HLDIA         SCCAU3
     C*** End Chg *** 05/23/13 ***************************************
     C                   SETON                                        9040      pos cursor
     C                   ELSE
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   ##ICD9        SCCAU3
     C                   MOVEL(P)  ##ICD9        SCCAU3
     C*** End Chg *** 05/23/13 ***************************************
     C                   SETON                                        9041      pos cursor
     C                   ENDIF
     C*
     C     SCCAU3        IFNE      *BLANKS
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   SCCAU3        KKICD9
     C                   MOVEL(P)  SCCAU3        KKICD9
     C*** End Chg *** 05/23/13 ***************************************
     C                   MOVE      ##LOST        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCC3DS
     C                   ENDIF
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C*
     C     HFLD          WHENEQ    'SCSCHD'
     C*
     C                   MOVEL(P)  'FLSC'        KKTBID
     C*
     C     GTKY2         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      0             ##RRN
     C                   PARM                    KKTBID
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SCSCHD
     C                   MOVEL     GTDESC        SCSHDS
     C*
     C                   SETON                                        90  42
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE                                                   ##RRN<>0
     C                   SETON                                        9041
     C*
     C                   ENDIF                                                  ##RRN<>0
     C*
     C                   ENDIF                                                  99 = OFF
     C*
     C*
     C*
     C*
     C* PAYMENT PLAN SEARCH
     C*
     C     HFLD          WHENEQ    'SCPPLN'
     C     SCTAPE        IFEQ      'Y'
     C                   MOVEL(P)  'FLPP'        ##TBID
     C                   ELSE
     C                   MOVEL(P)  'FLPA'        ##TBID
     C                   ENDIF
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM                    ##TBID
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        9031
     C                   ELSE
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTKEY         SCPPLN
     C                   MOVEL(P)  GTDESC        SCPPLD
     C                   SETON                                        9046
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##KC = '1'
     C     HFLD          WHENEQ    'SCLIC#'
     C*
     C                   MOVEL     *BLANKS       ##KEY
     C                   MOVEL     'FL'          ##KEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      *OFF
     C     STBS01        ANDEQ     'Y'
     C                   MOVEL(P)  SCLIC#        HLLIC#           13
     C                   CALL      'F2395R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##LIC#           13
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C                   SETON                                        9028
     C                   SETOFF                                       38
     C                   MOVEL(P)  HLLIC#        SCLIC#
     C                   ELSE
     C     ##LIC#        IFEQ      *BLANKS
     C                   MOVEL(P)  HLLIC#        SCLIC#
     C                   SETON                                        9028
     C                   ELSE
     C                   MOVEL(P)  ##LIC#        SCLIC#
     C                   SETON                                        9031
     C                   SETOFF                                       28
     C                   ENDIF                                                  ##LIC#=*BLAN
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE                                                   *IN60=*OFF
     C                   SETON                                        9028
     C                   ENDIF                                                  *IN60=*OFF
     C     HFLD          WHENEQ    'SCTAPE'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM      0             ##RRN             9 0
     C                   PARM      'FLES'        ##TBID
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SCTAPE
     C*                    SETON                     6162
     C                   SETON                                        9038
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C*
     C*                    SETOF                     63
     C                   SETON                                        9038
     C*
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
    C*
     C     HFLD          WHENEQ    'SCREVI'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM      0             ##RRN             9 0
     C                   PARM      'REVI'        ##TBID
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SCREVI
     C*                    MOVELGTDESC    SCREVD    P
     C*
     C                   SETON                                        9038
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C*
     C                   SETON                                        905697
     C                   SETOFF                                       57
     C*
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C                   OTHER
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCACCT'
     C                   SETOFF                                       61
     C                   SETON                                        626349
     C     HFLD          WHENEQ    'SCSCPF'
     C                   SETOFF                                       626349
     C                   SETON                                        61
     C     HFLD          WHENEQ    'SCSCPT'
     C                   SETOFF                                       616349
     C                   SETON                                        62
     C     HFLD          WHENEQ    'SCADDT'
     C                   SETOFF                                       626149
     C                   SETON                                        63
     C     HFLD          WHENEQ    'SCADHR'
     C                   SETOFF                                       626361
     C                   SETON                                        49
     C     HFLD          WHENEQ    'SCDCDT'
     C                   SETOFF                                       6349
     C                   SETON                                        6162
     C     HFLD          WHENEQ    'SCDCHR'
     C                   SETOFF                                       6149
     C                   SETON                                        6263
     C     HFLD          WHENEQ    'SCFZIP'
     C                   SETON                                        27
     C*** Beg Add *** 07/18/25 ***************************************
     C     HFLD          WHENEQ    'SCNPI1'
     C                   EVAL      MSSNPI = 'MCR0676'
     C                   EVAL      ERSNPI = *ON
     C*** End Add *** 07/18/25 ***************************************
     C                   ENDSL
     C*
     C                   SETON                                        90
     C                   ENDSL
     C*
     C                   EXSR      DATSBR
     C*
     C                   ENDSR
     C*================================================================
     C* KKDO - F11=EOB/Notes
     C*================================================================
     C     KKDO          BEGSR
     C                   Z-ADD     0             ##RRN
     C                   Z-ADD     @@RRN2        ##RRN2            9 0
     C                   CALL      'F0355R'
     C                   PARM      ' '           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM                    ##RRN2
     C                   SETON                                        90
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   CLEAR                   SCCHRG
     C                   CLEAR                   SCMRA
     C                   CLEAR                   SCNTWK
     C                   CLEAR                   SCOTHR
     C                   CLEAR                   SCOTH2
     C                   CLEAR                   SCPAID
     C                   CLEAR                   SCFLG7
     C                   CLEAR                   SCSCDS
     C                   CLEAR                   SCSPCD
     C                   CLEAR                   SCREVI
     C                   CLEAR                   SCFALC
     C*** Beg Add *** 04/17/13 ****************************************
     C                   CLEAR                   SCRFEI
     C                   CLEAR                   SCRLSN
     C                   CLEAR                   SCRFSN
     C                   CLEAR                   SCRAD1
     C                   CLEAR                   SCRAD2
     C                   CLEAR                   SCRCTY
     C                   CLEAR                   SCRST
     C                   CLEAR                   SCRZIP
     C*** End Add *** 04/17/13 ****************************************
     C                   MOVE      *BLANKS       SCANPI
     C                   MOVE      *BLANKS       SCONPI
     C                   MOVE      *BLANKS       SCNPI1
     C                   MOVE      *BLANKS       SCNPI2
     C*
     C*** INITIALIZE POSITION CURSOR INDICATORS
     C*
     C                   SETOFF                                       313233
     C                   SETOFF                                       343536
     C                   SETOFF                                       373839
     C                   SETOFF                                       404142
     C                   SETOFF                                       434445
     C                   SETOFF                                       465053
     C                   SETOFF                                       545556
     C                   SETOFF                                       5758
     C                   SETOFF                                       5152
     C                   SETOFF                                       575859
     C                   SETOFF                                       50
     C                   SETOFF                                       3941
     C                   EXSR      DSPSC2
     C     @@MODE        IFNE      'D'
     C                   SETON                                        23
     C                   ENDIF
     C*
     C* If in ADD mode and @@RRN is not zero, then user has used F12
     C* from the detail program.  This require a chain to the
     C* Header Info file in order to access the Header information.
     C*
     C     @@MODE        IFEQ      'A'
     C     @@RRN         ANDNE     *ZEROS
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C                   ENDIF                                                  @@MODE = 'A'
     C*
     C** GET PROVIDER SPECIALTY CODE (SERVICING PROVIDER)
     C*
     C                   MOVE      EOFEIN        KKFEIN
     C                   MOVE      EOMOD         KKMOD
     C*  IF THERE IS A SPECIALTY CODE IN THE FAEOP FILE, USE IT;
     C*    OTHERWISE, USE THE ON IN FMPDP
     C     EOSPCD        IFNE      *BLANKS
     C                   MOVEL     EOSPCD        SCSPCD
     C     PDKY          CHAIN     PPD01                              99
     C  N99              MOVEL(P)  PDLIC#        SCLIC#
     C  N99              MOVEL(P)  PDLIC#        SCFLIC
     C                   ELSE
     C     PDKY          CHAIN     PPD01                              99
     C  N99              MOVEL     PDSPCD        SCSPCD
     C  N99              MOVEL(P)  PDLIC#        SCLIC#
     C  N99              MOVEL(P)  PDLIC#        SCFLIC
     C  N99              MOVEL(P)  PDZIP         SCFZIP
     C*** Beg Add *** 07/18/25 ***************************************
     C  N99              MOVE      PDZIP         SCZIP
     C*** End Add *** 07/18/25 ***************************************
     C                   ENDIF
     C*
     C*
     C* If on SCR02 then get specialty code from EOSPCD
     C*
     C     XXSCR2        IFEQ      *ON
     C     @@MODE        OREQ      'D'
     C                   MOVEL     EOSPCD        SCSPCD
     C                   ENDIF
     C*
     C     EUPPLN        IFNE      *BLANKS
     C                   MOVEL     EUPPLN        SCPPLN
     C                   ENDIF
     C*
     C     SCPPLN        IFNE      *BLANKS
     C                   MOVEL(P)  'FLPA'        KKTBID
     C                   MOVEL(P)  SCPPLN        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C  N99              MOVEL     GTDESC        SCPPLD
     C                   ENDIF
     C*
     C                   MOVEL     SCSPCD        ##KEY
     C     SPKY          CHAIN     PSP01                              99
     C  N99              MOVEL     SPDESC        SCSCDS
     C   99              MOVE      *BLANKS       SCSCDS
     C*
     C                   MOVEL     EOAST         SCAST
     C                   MOVE(P)   EOZIPA        SCZIPA
     C*
     C* DETERMINE IF ANY EOB/NOTES EXISTS FOR THIS BILL
     C*
     C                   Z-ADD     EOSUB#        KKSUB#
     C                   Z-ADD     EOFND         KKFND
     C                   Z-ADD     EOFYR         KKFYR
     C                   Z-ADD     EOCASE        KKCASE
     C                   Z-ADD     EOEOB#        KKEOB#
     C     EEKY          CHAIN     PEE01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        68
     C                   ELSE
     C                   SETOFF                                       68
     C                   ENDIF                                                  *IN99 IFEQ *OFF
     C*
     C                   Z-ADD     EOCHRG        SCCHRG
     C*
     C     @@MODE        IFNE      'A'
     C     @@MODE        OREQ      'A'
     C     @@RRN         ANDNE     *ZEROS
     C*
     C                   MOVEL     EUACCT        SCACCT
     C*
     C                   MOVEL     EUDIA1        SCDIA1
     C                   MOVEL     SCDIA1        KKICD9
     C                   MOVE      'D'           KKLOST
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       If xxICD10 = '1' and EUicdInd ='Y';
          KKICD10 = EUICD1001;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
               scD1ds = ICdsc1;
             Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCD1DS
     C                   ENDIF                                                  *IN99 = *OFF
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C*
     C                   MOVEL     EUBLTY        SCBLTY
     C                   MOVEL(P)  BTTBID        KKTBID
     C                   MOVEL(P)  SCBLTY        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCBTDS
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   Z-ADD     EUSCPF        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCSCPF
     C*
     C                   Z-ADD     EUSCPT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCSCPT
     C*
     C                   Z-ADD     EUADDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCADDT
     C                   Z-ADD     EUDCDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDCDT
     C*
     C*
     C*
     C* The admit hour defaults to '99'=unknown if in add mode other
     C* wise pull from the database
     C*          @@MODE    IFNE 'A'
     C                   MOVE      EUADHR        SCADHR
     C*                    ENDIF                           @@MODE = 'A'
     C*
     C                   MOVE      EUDCHR        SCDCHR
     C                   MOVE      'N'           ##ER1             1
     C                   MOVEL     SCBLTY        ##BLTY            3
     C     1             SUBST     ##BLTY        #BT1              1
     C     1             SUBST     ##BLTY:2      #BT2              1
     C     #BT1          IFEQ      '1'
     C     #BT1          OREQ      '2'
     C     #BT2          IFEQ      '1'
     C     #BT2          OREQ      '2'
     C     #BT2          OREQ      '8'
     C                   MOVE      'Y'           ##ER1
     C                   ENDIF
     C                   ENDIF
     C     ##ER1         IFEQ      'N'
     C                   MOVE      *BLANKS       SCADHR
     C                   MOVE      *BLANKS       SCDCHR
     C                   ENDIF
     C                   MOVE      EUADTY        SCADTY
     C*
     C                   MOVEL     EUICN         SCICN
     C                   MOVEL     'SVTY'        KKTBID
     C                   MOVEL(P)  EUICN         KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCICDS
     C*
     C     SCICN         IFEQ      'IS'
     C     SCICN         OREQ      'HS'
     C     SCICN         OREQ      'AS'
     C                   MOVE      X'26'         ATR
     C                   ENDIF                                                  SCICN = IS,HS,AS
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   MOVEL     EUDCN         SCDCN
     C                   MOVEL     'SVTZ'        KKTBID
     C                   MOVEL(P)  EUDCN         KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCDCDS
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   MOVEL     EUPCM         SCPCM
     C                   MOVE      'PCMT'        KKTBID
     C                   MOVEL(P)  SCPCM         KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCPDSC
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   MOVEL     EUPRC1        SCPRC1
     C                   MOVE      EUPRC1        SCLOS1
     C     SCPCM         IFEQ      '4'
     C     SCPRC1        ANDNE     *BLANKS
     C                   MOVEL(P)  SCPRC1        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        SCP1DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  SCPCM = '4'
     C*
     C     SCPCM         IFEQ      '5'
     C     SCPRC1        ANDNE     *BLANKS
     C                   MOVEL(P)  SCPRC1        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        SCP1DS
     C                   ELSE
     C                   MOVEL(P)  SCPRC1        KKCPT4
     C     RCKY          CHAIN     PRC02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  RCDSC1        SCP1DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF
     C                   ENDIF                                                  SCPCM = 5
     C*
     C*** Beg Chg *** 08/05/13 ***************************************
     C*****SCPCM         IFEQ      '9'
     C*****SCPRC1        ANDNE     *BLANKS
      /Free
         If SCpcm = '9' and (Scprc1 <> ' ' or EUPRC1001 <> ' ' );
      /End-Free
     C*** End Chg *** 08/05/13 ***************************************
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       If xxICD10 = '1' and EUicdInd ='Y';
          KKICD10 = EUPRC1001;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'O';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
               scP1ds = ICdsc1;
             Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C                   MOVEL(P)  SCPRC1        KKICD9
     C                   MOVE      SCLOS1        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP1DS
     C                   ENDIF                                                  *IN99 = *OFF
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C                   ENDIF                                                  SCPCM = '9'
     C*
     C                   MOVEL     EUPRC2        SCPRC2
     C                   MOVE      EUPRC2        SCLOS2
     C     SCPCM         IFEQ      '4'
     C     SCPRC2        ANDNE     *BLANKS
     C                   MOVEL     SCPRC2        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        SCP2DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  SCPCM = '4'
     C*
     C     SCPCM         IFEQ      '5'
     C     SCPRC2        ANDNE     *BLANKS
     C                   MOVEL(P)  SCPRC2        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        SCP2DS
     C                   ELSE
     C                   MOVEL(P)  SCPRC2        KKCPT4
     C     RCKY          CHAIN     PRC02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  RCDSC1        SCP2DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF
     C                   ENDIF                                                  SCPCM = 5
     C*
     C*** Beg Chg *** 08/05/13 ***************************************
     C*****SCPCM         IFEQ      '9'
     C*****SCPRC2        ANDNE     *BLANKS
      /Free
         If SCpcm = '9' and (Scprc2 <> ' ' or EUPRC1002 <> ' ' );
      /End-Free
     C*** End Chg *** 08/05/13 ***************************************
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       If xxICD10 = '1' and EUicdInd ='Y';
          KKICD10 = EUPRC1002;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'O';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
               scP2ds = ICdsc1;
             Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C                   MOVEL(P)  SCPRC2        KKICD9
     C                   MOVE      SCLOS2        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP2DS
     C                   ENDIF                                                  *IN99 = *OFF
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C                   ENDIF                                                  SCPCM = '9'
     C*
     C*
     C                   MOVEL     EUPRC3        SCPRC3
     C                   MOVE      EUPRC3        SCLOS3
     C     SCPCM         IFEQ      '4'
     C     SCPRC3        ANDNE     *BLANKS
     C                   MOVEL     SCPRC3        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        SCP3DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  SCPCM = '4'
     C*
     C     SCPCM         IFEQ      '5'
     C     SCPRC3        ANDNE     *BLANKS
     C                   MOVEL(P)  SCPRC3        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        SCP3DS
     C                   ELSE
     C                   MOVEL(P)  SCPRC3        KKCPT4
     C     RCKY          CHAIN     PRC02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  RCDSC1        SCP3DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF
     C                   ENDIF                                                  SCPCM = 5
     C*
     C*** Beg Chg *** 08/05/13 ***************************************
     C*****SCPCM         IFEQ      '9'
     C*****SCPRC3        ANDNE     *BLANKS
      /Free
         If SCpcm = '9' and (Scprc3 <> ' ' or EUPRC1003 <> ' ' );
      /End-Free
     C*** End Chg *** 08/05/13 ***************************************
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       If xxICD10 = '1' and EUicdInd ='Y';
          KKICD10 = EUPRC1003;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'O';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
               scP3ds = ICdsc1;
             Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C                   MOVEL(P)  SCPRC3        KKICD9
     C                   MOVE      SCLOS3        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP3DS
     C                   ENDIF                                                  *IN99 = *OFF
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C                   ENDIF                                                  SCPCM = '9'
     C*
     C                   MOVEL     EUPRC4        SCPRC4
     C                   MOVE      EUPRC4        SCLOS4
     C     SCPCM         IFEQ      '4'
     C     SCPRC4        ANDNE     *BLANKS
     C                   MOVEL     SCPRC4        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        SCP4DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  SCPCM = '4'
     C*
     C     SCPCM         IFEQ      '5'
     C     SCPRC4        ANDNE     *BLANKS
     C                   MOVEL(P)  SCPRC4        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        SCP4DS
     C                   ELSE
     C                   MOVEL(P)  SCPRC4        KKCPT4
     C     RCKY          CHAIN     PRC02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  RCDSC1        SCP4DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF
     C                   ENDIF                                                  SCPCM = 5
     C*
     C*** Beg Chg *** 08/05/13 ***************************************
     C*****SCPCM         IFEQ      '9'
     C*****SCPRC4        ANDNE     *BLANKS
      /Free
         If SCpcm = '9' and (Scprc4 <> ' ' or EUPRC1004 <> ' ' );
      /End-Free
     C*** End Chg *** 08/05/13 ***************************************
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       If xxICD10 = '1' and EUicdInd ='Y';
          KKICD10 = EUPRC1004;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'O';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
               scP4ds = ICdsc1;
             Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C                   MOVEL(P)  SCPRC4        KKICD9
     C                   MOVE      SCLOS4        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP4DS
     C                   ENDIF                                                  *IN99 = *OFF
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C                   ENDIF                                                  SCPCM = '9'
     C*
     C                   MOVEL     EUPRC5        SCPRC5
     C                   MOVE      EUPRC5        SCLOS5
     C     SCPCM         IFEQ      '4'
     C     SCPRC5        ANDNE     *BLANKS
     C                   MOVEL     SCPRC5        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        SCP5DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  SCPCM = '4'
     C*
     C     SCPCM         IFEQ      '5'
     C     SCPRC5        ANDNE     *BLANKS
     C                   MOVEL(P)  SCPRC5        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        SCP5DS
     C                   ELSE
     C                   MOVEL(P)  SCPRC5        KKCPT4
     C     RCKY          CHAIN     PRC02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  RCDSC1        SCP5DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF
     C                   ENDIF                                                  SCPCM = 5
     C*
     C*** Beg Chg *** 08/05/13 ***************************************
     C*****SCPCM         IFEQ      '9'
     C*****SCPRC5        ANDNE     *BLANKS
      /Free
         If SCpcm = '9' and (Scprc5 <> ' ' or EUPRC1005 <> ' ' );
      /End-Free
     C*** End Chg *** 08/05/13 ***************************************
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       If xxICD10 = '1' and EUicdInd ='Y';
          KKICD10 = EUPRC1005;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'O';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
               scP5ds = ICdsc1;
             Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C                   MOVEL(P)  SCPRC5        KKICD9
     C                   MOVE      SCLOS5        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP5DS
     C                   ENDIF                                                  *IN99 = *OFF
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C                   ENDIF                                                  SCPCM = '9'
     C*
     C                   MOVEL     EUPRC6        SCPRC6
     C                   MOVE      EUPRC6        SCLOS6
     C     SCPCM         IFEQ      '4'
     C     SCPRC6        ANDNE     *BLANKS
     C                   MOVEL     SCPRC6        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        SCP6DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  SCPCM = '4'
     C*
     C     SCPCM         IFEQ      '5'
     C     SCPRC6        ANDNE     *BLANKS
     C                   MOVEL(P)  SCPRC6        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        SCP6DS
     C                   ELSE
     C                   MOVEL(P)  SCPRC6        KKCPT4
     C     RCKY          CHAIN     PRC02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  RCDSC1        SCP6DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF
     C                   ENDIF                                                  SCPCM = 5
     C*
     C*** Beg Chg *** 08/05/13 ***************************************
     C*****SCPCM         IFEQ      '9'
     C*****SCPRC6        ANDNE     *BLANKS
      /Free
         If SCpcm = '9' and (Scprc6 <> ' ' or EUPRC1006 <> ' ' );
      /End-Free
     C*** End Chg *** 08/05/13 ***************************************
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       If xxICD10 = '1' and EUicdInd ='Y';
          KKICD10 = EUPRC1006;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'O';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
               scP6ds = ICdsc1;
             Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C                   MOVEL(P)  SCPRC6        KKICD9
     C                   MOVE      SCLOS6        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP6DS
     C                   ENDIF                                                  *IN99 = *OFF
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C                   ENDIF                                                  SCPCM = '9'
     C*
     C*
     C                   MOVEL     EUDIA2        SCDIA2
     C*** Beg Chg *** 08/05/13 ***************************************
     C*****SCDIA2        ANDNE     *BLANKS
      /Free
         If SCDIA2 <> ' ' or EUICD1002 <> ' ' ;
      /End-Free
     C*** End Chg *** 08/05/13 ***************************************
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       If xxICD10 = '1' and EUicdInd ='Y';
          KKICD10 = EUICD1002;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
               scD2ds = ICdsc1;
             Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C                   MOVEL(P)  SCDIA2        KKICD9
     C                   MOVE      'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCD2DS
     C                   ENDIF                                                  *IN99 = *OFF
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C                   ENDIF                                                  SCDIA2 = *BLANKS
     C*
     C                   MOVEL     EUDIA3        SCDIA3
     C*** Beg Chg *** 08/05/13 ***************************************
     C*****SCDIA3        ANDNE     *BLANKS
      /Free
         If SCDIA3 <> ' ' or EUICD1003 <> ' ' ;
      /End-Free
     C*** End Chg *** 08/05/13 ***************************************
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       If xxICD10 = '1' and EUicdInd ='Y';
          KKICD10 = EUICD1003;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
               scD3ds = ICdsc1;
             Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C                   MOVEL(P)  SCDIA3        KKICD9
     C                   MOVE      'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCD3DS
     C                   ENDIF                                                  *IN99 = *OFF
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C                   ENDIF                                                  SCDIA3 = *BLANKS
     C*
     C                   MOVEL     EUDIA4        SCDIA4
     C*** Beg Chg *** 08/05/13 ***************************************
     C*****SCDIA4        ANDNE     *BLANKS
      /Free
         If SCDIA4 <> ' ' or EUICD1004 <> ' ' ;
      /End-Free
     C*** End Chg *** 08/05/13 ***************************************
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       If xxICD10 = '1' and EUicdInd ='Y';
          KKICD10 = EUICD1004;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
               scD4ds = ICdsc1;
             Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C                   MOVEL(P)  SCDIA4        KKICD9
     C                   MOVE      'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCD4DS
     C                   ENDIF                                                  *IN99 = *OFF
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C                   ENDIF                                                  SCDIA4 = *BLANKS
     C*
     C*
     C*
     C                   MOVEL     EUDIA5        SCDIA5
     C*** Beg Chg *** 08/05/13 ***************************************
     C*****SCDIA5        ANDNE     *BLANKS
      /Free
         If SCDIA5 <> ' ' or EUICD1005 <> ' ' ;
      /End-Free
     C*** End Chg *** 08/05/13 ***************************************
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       If xxICD10 = '1' and EUicdInd ='Y';
          KKICD10 = EUICD1005;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
               scD5ds = ICdsc1;
             Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C                   MOVEL(P)  SCDIA5        KKICD9
     C                   MOVE      'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCD5DS
     C                   ENDIF                                                  *IN99 = *OFF
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C                   ENDIF                                                  SCDIA5 = *BLANKS
     C*
     C                   MOVEL     EUDIA6        SCDIA6
     C*** Beg Chg *** 08/05/13 ***************************************
     C*****SCDIA6        ANDNE     *BLANKS
      /Free
         If SCDIA6 <> ' ' or EUICD1006 <> ' ' ;
      /End-Free
     C*** End Chg *** 08/05/13 ***************************************
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       If xxICD10 = '1' and EUicdInd ='Y';
          KKICD10 = EUICD1006;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
               scD6ds = ICdsc1;
             Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C                   MOVEL(P)  SCDIA6        KKICD9
     C                   MOVE      'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCD6DS
     C                   ENDIF                                                  *IN99 = *OFF
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C                   ENDIF                                                  SCDIA6 = *BLANKS
     C*
     C                   MOVEL     EUDIA7        SCDIA7
     C*** Beg Chg *** 08/05/13 ***************************************
     C*****SCDIA7        ANDNE     *BLANKS
      /Free
         If SCDIA7 <> ' ' or EUICD1007 <> ' ' ;
      /End-Free
     C*** End Chg *** 08/05/13 ***************************************
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       If xxICD10 = '1' and EUicdind ='Y';
          KKICD10 = EUICD1007;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
               scD7ds = ICdsc1;
             Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C                   MOVEL(P)  SCDIA7        KKICD9
     C                   MOVE      'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCD7DS
     C                   ENDIF                                                  *IN99 = *OFF
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C                   ENDIF                                                  SCDIA7 = *BLANKS
     C*
     C                   MOVEL     EUDIA8        SCDIA8
     C*** Beg Chg *** 08/05/13 ***************************************
     C*****SCDIA8        ANDNE     *BLANKS
      /Free
         If SCDIA8 <> ' ' or EUICD1008 <> ' ' ;
      /End-Free
     C*** End Chg *** 08/05/13 ***************************************
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       If xxICD10 = '1' and EUicdInd ='Y';
          KKICD10 = EUICD1008;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
               scD8ds = ICdsc1;
             Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C                   MOVEL(P)  SCDIA8        KKICD9
     C                   MOVE      'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCD8DS
     C                   ENDIF                                                  *IN99 = *OFF
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C                   ENDIF                                                  SCDIA8 = *BLANKS
     C*
     C                   MOVEL     EUCAU1        SCCAU1
     C*** Beg Chg *** 08/05/13 ***************************************
     C*****SCCAU1        ANDNE     *BLANKS
      /Free
         If SCCAU1 <> ' ' or EUCAU1001 <> ' ' ;
      /End-Free
     C*** End Chg *** 08/05/13 ***************************************
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       If xxICD10 = '1' and EUicdInd ='Y';
          KKICD10 = EUCAU1001;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
               scC1ds = ICdsc1;
             Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C                   MOVEL(P)  SCCAU1        KKICD9
     C                   MOVE      'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCC1DS
     C                   ENDIF                                                  *IN99 = *OFF
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C                   ENDIF                                                  SCCAU1 = *BLANKS
     C*
     C                   MOVEL     EUCAU2        SCCAU2
     C*** Beg Chg *** 08/05/13 ***************************************
     C*****SCCAU2        ANDNE     *BLANKS
      /Free
         If SCCAU2 <> ' ' or EUCAU1002 <> ' ' ;
      /End-Free
     C*** End Chg *** 08/05/13 ***************************************
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       If xxICD10 = '1' and EUicdInd ='Y';
          KKICD10 = EUCAU1002;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
               scC2ds = ICdsc1;
             Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C                   MOVEL(P)  SCCAU2        KKICD9
     C                   MOVE      'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCC2DS
     C                   ENDIF                                                  *IN99 = *OFF
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C                   ENDIF                                                  SCCAU2 = *BLANKS
     C*
     C                   MOVEL     EUCAU3        SCCAU3
     C*** Beg Chg *** 08/05/13 ***************************************
     C*****SCCAU3        ANDNE     *BLANKS
      /Free
         If SCCAU3 <> ' ' or EUCAU1003 <> ' ' ;
      /End-Free
     C*** End Chg *** 08/05/13 ***************************************
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       If xxICD10 = '1' and EuicdInd ='Y';
          KKICD10 = EUCAU1003;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
               scC3ds = ICdsc1;
             Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C                   MOVEL(P)  SCCAU3        KKICD9
     C                   MOVE      'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCC3DS
     C                   ENDIF                                                  *IN99 = *OFF
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C                   ENDIF                                                  SCCAU3 = *BLANKS
     C*
     C                   MOVE      EUSCHD        SCSCHD
     C                   MOVE      EUOLIC        SCOLIC
     C                   MOVEL(P)  'FLSC'        KKTBID
     C                   MOVEL(P)  SCSCHD        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTDESC        SCSHDS
     C                   ENDIF
     C*
     C*
     C*
     C                   MOVE      EUEDI         SCTAPE
     C                   MOVEL(P)  'FLES'        KKTBID
     C                   MOVEL(P)  SCTAPE        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTDESC        SCDSDS
     C                   ELSE
     C                   CLEAR                   SCDSDS
     C                   ENDIF
     C                   MOVEL(P)  EULIC#        SCLIC#
     C     EUFLIC        IFNE      *BLANKS
     C                   MOVEL(P)  EUFLIC        SCLIC#
     C                   ENDIF
     C*
     C*
     C                   Z-ADD     EOMRA         SCMRA
     C                   Z-ADD     EONDIS        SCNTWK
     C                   Z-ADD     EOOTHR        SCOTHR
     C                   Z-ADD     EOOTH2        SCOTH2
     C                   Z-ADD     EOPAID        SCPAID
     C                   MOVEL(P)  EOFLIC        SCFZIP
     C*** Beg Add *** 07/18/25 ***************************************
     C                   MOVE      EOFLIC        SCZIP
     C*** End Add *** 07/18/25 ***************************************
     C*
     C                   MOVE      EUANPI        SCANPI
     C                   MOVE      EUONPI        SCONPI
     C                   MOVE      EUNPI1        SCNPI1
     C                   MOVE      EUNPI2        SCNPI2
     C                   ENDIF                                                  @@MODE NE 'A'
     C*
     C                   MOVEL     EOREVI        SCREVI
     C                   MOVEL     EUFALC        SCFALC
     C*
     C     @@MODE        IFEQ      'A'
     C     SCREVI        ANDEQ     *BLANKS
     C                   MOVEL     BEREVI        SCREVI
     C                   ENDIF                                                  @@MODE=A
     C*** Beg Add *** 04/17/13 ****************************************
     C     CLTID         IFEQ      '114'
     C                   MOVE      EURFEI        SCRFEI
     C                   MOVE      EURLSN        SCRLSN
     C                   MOVE      EURFSN        SCRFSN
     C                   MOVE      EURAD1        SCRAD1
     C                   MOVE      EURAD2        SCRAD2
     C                   MOVE      EURCTY        SCRCTY
     C                   MOVE      EURST         SCRST
     C                   MOVE      EURZIP        SCRZIP
     C                   ENDIF
     C*** End Add *** 04/17/13 ****************************************
     C*** Beg Add *** 05/23/13 ****************************************
      /Free
          //Beg Chg *** 08/05/13 ****************************************
          //scICdind = 'N';
          //If xxicd10 = *on;
          If xxicd10 = *on and EUicdind = 'Y';
          //End Chg *** 08/05/13 ****************************************
             scDIa1  = EUICD1001;
             scDIa2  = EUICD1002;
             scDIa3  = EUICD1003;
             scDIa4  = EUICD1004;
             scDIa5  = EUICD1005;
             scDIa6  = EUICD1006;
             scDIa7  = EUICD1007;
             scDIa8  = EUICD1008;
             scCAu1  = EUCAU1001;
             scCAu2  = EUCAU1002;
             scCAu3  = EUCAU1003;
             scICdind= EUICDind ;
          //Beg Chg *** 08/05/13 ****************************************
             If EUPCM = '9';
                scPRc1  = EUPRC1001;
                scPRc2  = EUPRC1002;
                scPRc3  = EUPRC1003;
                scPRc4  = EUPRC1004;
                scPRc5  = EUPRC1005;
                scPRc6  = EUPRC1006;
             Else;
                scPRc1  = EUPRC1;
                scPRc2  = EUPRC2;
                scPRc3  = EUPRC3;
                scPRc4  = EUPRC4;
                scPRc5  = EUPRC5;
                scPRc6  = EUPRC6;
             EndIf;
          //End Chg *** 08/05/13 ****************************************
          // Beg Add *** 08/05/13 ****************************************
          Elseif EUICDind ='N';
             scICdind = EUICDind;
          // End Add *** 08/05/13 ****************************************
          Endif;
      /End-Free
     C*** End Add *** 05/23/13 ****************************************
      /Free
       // Beg Add *** 01/08/15 ****************************************
          scCntyCd = EUcnty;
       // End Add *** 01/08/15 ****************************************
      /End-Free
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C                   MOVE      *OFF          LICCHG            1
     C     SCLIC#        IFNE      EUFLIC
     C     SCOLIC        ORNE      EUOLIC
     C                   MOVE      *ON           LICCHG
     C                   ENDIF
     C*
     C                   Z-ADD     EOCO#         EUCO#
     C                   Z-ADD     EOSUB#        EUSUB#
     C                   Z-ADD     EOFND         EUFND
     C                   Z-ADD     EOFYR         EUFYR
     C                   Z-ADD     EOCASE        EUCASE
     C                   MOVE      EOSS#         EUSS#
     C                   Z-ADD     EOACDT        EUACDT
     C*
     C                   MOVEL(P)  SCACCT        EUACCT
     C                   MOVEL(P)  SCDIA1        EUDIA1
     C                   MOVEL(P)  SCBLTY        EUBLTY
     C*
     C                   Z-ADD     SCSCPF        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EUSCPF
     C*
     C                   Z-ADD     SCSCPT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EUSCPT
     C*
     C                   Z-ADD     SCADDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EUADDT
     C                   Z-ADD     SCDCDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EUDCDT
     C*
     C*
     C                   MOVE      SCADHR        EUADHR
     C*
     C* Default the admit hour to '99'=unknown if left blank otherwise
     C* move the screen field in
     C     SCREVI        IFGE      'E'
     C                   MOVE      SCADHR        EUADHR
     C                   ELSE
     C     SCADHR        IFEQ      *BLANKS
     C                   MOVE      '99'          EUADHR
     C                   ELSE
     C                   MOVE      SCADHR        EUADHR
     C                   ENDIF                                                  SCADHR IFEQ
     C                   ENDIF                                                  SCREVI>=E
     C*
     C                   MOVE      SCDCHR        EUDCHR
     C                   MOVE      SCADTY        EUADTY
     C                   MOVEL     SCICN         EUICN
     C                   MOVEL     SCDCN         EUDCN
     C                   MOVEL     SCPCM         EUPCM
     C     SCPCM         IFNE      '9'
     C                   MOVE      *BLANKS       SCLOS1
     C                   MOVE      *BLANKS       SCLOS2
     C                   ENDIF
     C     SCLIC1        CAT       SCLIC2        XXLIC2            2
     C     XXLIC2        CAT       SCLIC3        XXLIC3            3
     C*
     C                   MOVE      SCTAPE        EUEDI
     C                   MOVEL(P)  SCLIC#        EULIC#
     C                   MOVEL(P)  SCLIC#        EUFLIC
     C                   MOVEL(P)  SCPRC1        EUPRC1
     C                   MOVE      SCLOS1        EUPRC1
     C                   MOVEL(P)  SCPRC2        EUPRC2
     C                   MOVE      SCLOS2        EUPRC2
     C                   MOVEL(P)  SCDIA2        EUDIA2
     C                   MOVEL(P)  SCDIA3        EUDIA3
     C                   MOVEL(P)  SCDIA4        EUDIA4
     C                   MOVEL(P)  SCPRC3        EUPRC3
     C                   MOVE      SCLOS3        EUPRC3
     C                   MOVEL(P)  SCPRC4        EUPRC4
     C                   MOVE      SCLOS4        EUPRC4
     C                   MOVEL(P)  SCPRC5        EUPRC5
     C                   MOVE      SCLOS5        EUPRC5
     C                   MOVEL(P)  SCPRC6        EUPRC6
     C                   MOVE      SCLOS6        EUPRC6
     C                   MOVEL(P)  SCDIA5        EUDIA5
     C                   MOVEL(P)  SCDIA6        EUDIA6
     C                   MOVEL(P)  SCDIA7        EUDIA7
     C                   MOVEL(P)  SCDIA8        EUDIA8
     C                   MOVEL(P)  SCCAU1        EUCAU1
     C                   MOVEL(P)  SCCAU2        EUCAU2
     C                   MOVEL(P)  SCCAU3        EUCAU3
     C                   MOVEL(P)  SCSCHD        EUSCHD
     C     SCOLC1        CAT       SCOLC2        XXOLC2            2
     C     XXOLC2        CAT       SCOLC3        XXOLC3            3
     C*
     C                   MOVEL(P)  SCOLIC        EUOLIC
     C                   MOVE      SCTAPE        EUEDI
     C                   MOVEL     SCPPLN        EUPPLN
     C                   MOVE      SCSPCD        EOSPCD
     C                   MOVEL(P)  SCFZIP        EOFLIC
     C*** Beg Add *** 07/18/25 ***************************************
     C                   MOVE      SCZIP         EOFLIC
     C*** End Add *** 07/18/25 ***************************************
     C                   MOVE      SCREVI        EOREVI
     C                   MOVE      SCFALC        EUFALC
     C*
     C                   MOVE      SCANPI        EUANPI
     C                   MOVE      SCONPI        EUONPI
     C                   MOVE      SCNPI1        EUNPI1
     C                   MOVE      SCNPI2        EUNPI2
     C*** Beg Add *** 04/17/13 ****************************************
     C     CLTID         IFEQ      '114'
     C                   MOVE      SCRFEI        EURFEI
     C                   MOVE      SCRLSN        EURLSN
     C                   MOVE      SCRFSN        EURFSN
     C                   MOVE      SCRAD1        EURAD1
     C                   MOVE      SCRAD2        EURAD2
     C                   MOVE      SCRCTY        EURCTY
     C                   MOVE      SCRST         EURST
     C                   MOVE      SCRZIP        EURZIP
     C                   ENDIF
     C*** End Add *** 04/17/13 ****************************************
     C*** Beg Add *** 05/23/13 ****************************************
      /Free
          //Beg Chg *** 08/05/13 ****************************************
          EUICDind  = scICdind;
          If xxICD10 = '1' and scICdInd ='Y';
          //End Chg *** 08/05/13 ****************************************
             EUICD1001 = scDIa1;
             EUICD1002 = scDIa2;
             EUICD1003 = scDIa3;
             EUICD1004 = scDIa4;
             EUICD1005 = scDIa5;
             EUICD1006 = scDIa6;
             EUICD1007 = scDIa7;
             EUICD1008 = scDIa8;
             EUCAU1001 = scCAu1;
             EUCAU1002 = scCAu2;
             EUCAU1003 = scCAu3;
          //Beg Chg *** 08/05/13 ****************************************
               If SCPCM = '9';
                 EUPRC1001 = scPRc1;
                 EUPRC1002 = scPRc2;
                 EUPRC1003 = scPRc3;
                 EUPRC1004 = scPRc4;
                 EUPRC1005 = scPRc5;
                 EUPRC1006 = scPRc6;
                 EUPRC1 = *Blanks;
                 EUPRC2 = *Blanks;
                 EUPRC3 = *Blanks;
                 EUPRC4 = *Blanks;
                 EUPRC5 = *Blanks;
                 EUPRC6 = *Blanks;
               Else;
                 EUPRC1001 = *Blanks;
                 EUPRC1002 = *Blanks;
                 EUPRC1003 = *Blanks;
                 EUPRC1004 = *Blanks;
                 EUPRC1005 = *Blanks;
                 EUPRC1006 = *Blanks;
                 EUPRC1 = scPRc1;
                 EUPRC2 = scPRc2;
                 EUPRC3 = scPRc3;
                 EUPRC4 = scPRc4;
                 EUPRC5 = scPRc5;
                 EUPRC6 = scPRc6;
               EndIf;

             // If ICD10 is Active then no need of ICD9 fields
             EUDIa1 = *Blanks;
             EUDIa2 = *Blanks;
             EUDIa3 = *Blanks;
             EUDIa4 = *Blanks;
             EUDIa5 = *Blanks;
             EUDIa6 = *Blanks;
             EUDIa7 = *Blanks;
             EUDIa8 = *Blanks;
             EUCAu1 = *Blanks;
             EUCAu2 = *Blanks;
             EUCAu3 = *Blanks;
          Else;
             // If ICD9 is Active then no need of ICD10 fields
             EUICD1001 = *Blanks;
             EUICD1002 = *Blanks;
             EUICD1003 = *Blanks;
             EUICD1004 = *Blanks;
             EUICD1005 = *Blanks;
             EUICD1006 = *Blanks;
             EUICD1007 = *Blanks;
             EUICD1008 = *Blanks;
             EUPRC1001 = *Blanks;
             EUPRC1002 = *Blanks;
             EUPRC1003 = *Blanks;
             EUPRC1004 = *Blanks;
             EUPRC1005 = *Blanks;
             EUPRC1006 = *Blanks;
             EUCAU1001 = *Blanks;
             EUCAU1002 = *Blanks;
             EUCAU1003 = *Blanks;
          //End Chg *** 08/05/13 ****************************************
          Endif;
      /End-Free
     C*** End Add *** 05/23/13 ****************************************
      /Free
       // Beg Add *** 01/08/15 ****************************************
          EUcnty = scCntyCd;
          kkEfDt = EUAdDt;
          varCntyCd = scCntyCd;
          setgt (varCntyCd:kkefdt) pct01;
          readpe varCntyCd pct01;
            If not %eof (ftctyl2fl);
              DecCtyMod = CTYgMod;
            Else;
              DecCtyMod = *Zeros;
            Endif;
       // End Add *** 01/08/15 ****************************************
      /End-Free
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C* SET OF INDICATORS FOR ERROR CHECKING. THESE ARE NOT PART OF
     C* COPYBOOK ERROR INDICATOR SET.
     C*
     C                   SETOFF                                       5152
     C                   SETOFF                                       575859
     C                   SETOFF                                       50
       //*** Beg add ***** 07/18/25 ************************************
       mssnpi = *blanks;
       ersnpi = *off;
       *in27  = *off;
       //*** End add ***** 07/18/25 ************************************
       //*** Beg Add *** 11/21/17 ****************************************
      /Free
                         *in94 = *Off;
      /End-Free
       //*** End Add *** 11/21/17 ****************************************
     C*** Beg Add *** 07/03/15 *******************************************
     C                   eval      erDia2 = *off
     C                   eval      erDia3 = *off
     C                   eval      erDia4 = *off
     C                   eval      erDia5 = *off
     C                   eval      erDia6 = *off
     C                   eval      erDia7 = *off
     C                   eval      erDia8 = *off
     C                   eval      erCau1 = *off
     C                   eval      erCau2 = *off
     C                   eval      erCau3 = *off
     C*** End Add *** 07/03/15 *******************************************
     C*
     C*** INITIALIZE POSITION CURSOR INDICATORS
     C*
     C                   SETOFF                                       313233
     C                   SETOFF                                       343536
     C                   SETOFF                                       373839
     C                   SETOFF                                       404142
     C                   SETOFF                                       434445
     C                   SETOFF                                       465053
     C                   SETOFF                                       545556
     C                   SETOFF                                       5758
     C*
     C     *INKJ         CASEQ     *ON           KJDO                           F10=search
     C                   ENDCS
     C*** Beg Add *** MC367 *** 11/19/12 ******************************
      /Free
      /If Defined(WEB)
         If *INKK;
           ExSr KKDO;  //F11=EOB/Notes
         EndIf;
      /EndIf
      /End-Free
     C*** End Add *** MC367 *** 11/19/12 ******************************
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C*  Get relative record number for header record and put in
     C*  parm @@RRN.  This will allow the ability to access header
     C*  data if user F12=Cancel from detail screen on an add.
     C*
     C     @@MODE        IFEQ      'A'
     C     EUKY          CHAIN(N)  PEU01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DBRRNA        @@RRN
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  @@MODE = 'A'
     C*
     C* The following call to the WRITER subroutine is necessary
     C* because if program bombs out in add mode before the user
     C* takes action to write the header record, 'change' mode
     C* will be looking to update a record that does not exist.
     C* Thus, if the @@RRN (FAEUP) is blanks in change mode, we must
     C* call the write subroutine to add the header record.
     C*       (WILL BE EXECUTED IF USER HITS ENTER)
     C*
     C     @@MODE        IFEQ      'C'
     C     *INKC         ANDEQ     *OFF
     C     @@RRN         ANDEQ     *ZEROS
     C                   EXSR      WRITER
     C                   ENDIF                                                  @@MODE = 'C'
     C*
     C*
     C*   Call Cancel/Pend program if user hits F3=EXIT
     C*   If in Add mode, enable user to cancel bill (##CANC = Y)
     C*   Otherwise do not allow cancel capability (##CANC = N)
     C*
     C     *INKC         IFEQ      *ON
     C     @@MODE        ANDNE     'D'
     C*
     C     @@MODE        IFEQ      'A'
     C                   MOVE      'Y'           ##CANC                         able to cancel
     C                   ELSE
     C                   MOVE      ' '           ##CANC                         unable to cancel
     C                   ENDIF
     C*
     C* If in add mode and F3=Exit is hit, call writer subroutine
     C* to write the header record
     C*
     C     @@MODE        IFEQ      'A'
     C     *INKC         ANDEQ     *ON
     C                   EXSR      WRITER
     C                   ENDIF                                                  @@MODE = 'A'
     C*
     C* If in 'change' mode and F3=Exit is hit, call update subroutine
     C* to update the header record with the changed info.
     C* This will work on both SCR01 and SCR02.
     C*
     C     @@MODE        IFEQ      'C'
     C     *INKC         ANDEQ     *ON
     C     @@RRN         ANDNE     *ZEROS
     C                   EXSR      UPDATR
     C                   ENDIF                                                  @@MODE = 'C'
     C*
     C* The following call to the WRITER subroutine is necessary
     C* because if program bombs out in add mode before the user
     C* takes action to write the header record, 'change' mode
     C* will be looking to update a record that does not exist.
     C* Thus, if the @@RRN (FAEUP) is blanks in change mode, we must
     C* call the write subroutine to add the header record.
     C*       (WILL BE EXECUTED IF USER HITS F3=EXIT)
     C*
     C     @@MODE        IFEQ      'C'
     C     @@RRN         ANDEQ     *ZEROS
     C                   EXSR      WRITER
     C                   ENDIF                                                  @@MODE = 'C'
     C*
     C                   Z-ADD     @@RRN2        ##RRN
     C                   CALL      'F0392R'
     C     *INKL         PARM      ' '           ##KL
     C                   PARM      ' '           ##MODE            1
     C                   PARM                    ##RRN             9 0
     C                   PARM                    ##CANC            1
     C                   PARM                    @@CHRG
     C                   PARM      ' '           ##RTCD            1
     C*
     C                   ENDIF                                                  *INKC = *ON
     C*
     C* CALL 'ADJUDICATION CALL' PROGRAM IF IN UNPEND MODE
     C* THIS IS DONE BECAUSE OTHER BILLS MAY HAVE BEEN PROCOSSED
     C* FOR THIS CASE SINCE THIS BILL HAS BEEN ADJUDICATED
     C*
     C*** Beg Add *** 04/30/13 ****************************************
     C*** Beg Add *** 01/15/16 ****************************************
     C*** Begin Chg *** 04/30/19 **************************************
     C**********         if        cltid = '114'
     C                   if        cltid = '114' Or cltid = '152'
     C*** End   Chg *** 04/30/19 **************************************
     C*** End Add *** 01/15/16 ****************************************
     C                   CALL      'FA782R'
     C                   PARM                    @@mode
     C                   PARM                    @@rrn2
     C                   PARM                    sclic#
     C*** Beg Add *** 01/15/16 ****************************************
     C                   Endif
     C*** End Add *** 01/15/16 ****************************************
     C*** End Add *** 04/30/13 ****************************************
     C*
     C                   SETON                                        LR
     C                   MOVE      '1'           OUTFLG
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C* The admit hour defaults to '99'=unknown if in add mode
     C     @@MODE        IFEQ      'A'
     C     SCTAPE        ANDEQ     'Y'
     C     @@MODE        OREQ      'A'
     C     SCTAPE        ANDEQ     'N'
     C                   MOVE      '99'          SCADHR
     C                   ENDIF                                                  @@MODE = 'A'
     C*
     C** Get standard screen headings
     C*
     C     @@RRN2        CHAIN(N)  PEO01                              99
     C*
     C*
     C*  SET EDITST TO POLICY OR ADJ STATE BASED ON CPST FLAG
     C*
     C     EOCPST        IFEQ      'Y'
     C                   MOVEL(P)  EOPOST        EDITST
     C                   ELSE
     C                   MOVEL(P)  EOAST         EDITST
     C                   ENDIF
     C*
     C*
     C                   Z-ADD     EOEOB#        SCEOB#
     C     EOAST         CAT(P)    'BT':0        BTTBID            4
     C                   MOVEL(P)  BTTBID        KKTBID
     C     GTKY2         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'ZZBT'        KKTBID
     C     GTKY2         CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'BLTY'        BTTBID
     C                   ELSE
     C                   MOVEL(P)  'ZZBT'        BTTBID
     C                   ENDIF
     C                   ENDIF
     C                   MOVE      EOCO#         #1CO#
     C                   MOVE      EOSUB#        #1SUB#
     C                   MOVE      EOFND         #1FND
     C                   MOVE      EOFYR         #1FYR
     C                   MOVE      EOCASE        #1CASE
     C                   MOVE      EOBTYP        @@BTYP            1
     C                   MOVE      EOFRDT        @@SDTE
     C                   CLEAR                   ##RTCD
     C                   CLEAR                   #1L1
     C                   CLEAR                   #1L2
     C*
     C                   CALL      'FA702R'
     C                   PARM                    ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM                    #1L1             78
     C                   PARM                    #1L2             78
     C*
     C                   MOVEL     #1L1          SCHD1
     C                   MOVEL     #1L2          SCHD2
     C*
     C                   CLEAR                   ##RTCD
     C                   CLEAR                   #1L1
     C                   CLEAR                   #1L2
     C                   CALL      'FA707R'
     C                   PARM                    ##RTCD
     C                   PARM                    @@RRN2
     C                   PARM                    #1L1
     C                   MOVEL     #1L1          SCHD3
     C*
     C* CALL API FOR SYSTEM FLAGS
     C*
     C                   CALL      'SA001R'
     C                   PARM      ' '           ##WCIF            1
     C                   PARM      ' '           ##MCIF            1
     C                   PARM      ' '           ##WCIN            1
     C                   PARM      ' '           ##AR              1
     C                   PARM      ' '           ##NTWK            1
     C                   PARM      ' '           ##UR              1
     C                   PARM      ' '           ##MED             1
     C                   PARM      ' '           ##EDI             1
     C                   PARM      ' '           ##WCDK            1
     C                   PARM      ' '           ##RHAB            1
     C*
     C* SET INDICATOR TO DISPLAY NEWORK REDUCTIONS FIELD IF
     C* NETWORK IS INSTALLED
     C*
     C     ##NTWK        IFEQ      'Y'
     C                   SETON                                        47
     C                   ELSE
     C                   SETOFF                                       47
     C                   ENDIF                                                  ##NTWK = 'Y'
     C*
     C** Get 'Reductions' headings
     C*
      /Free
       clear scHMRA_dot;
       clear scHOT1_dot;
      /If Not Defined(WEB)
       scHMRA_dot = '. . . . . .';
       scHOT1_dot = '. . . . . .';
      /Endif
      /End-free
     C                   Z-ADD     EOSUB#        ##SUB#
     C     CXKY          CHAIN     PCX01                              99
     C                   MOVEL     CXHMRA        SCHMRA
     C                   MOVEL     CXHNET        SCHNET
     C                   MOVEL     CXHOT1        SCHOT1
     C                   MOVEL     CXHOT2        SCHOT2
     C*
     C** DETERMINE IF ELIGIBLE FOR DATA SUBMISSION SHOULD BE DISPLAYED
     C** AND REQUIRED
     C*
     C                   MOVEL     EDITST        ##KEY
     C     STKY          CHAIN     PST01                              99
     C     *IN99         IFEQ      *OFF
     C     ST1A4         ANDEQ     'Y'
     C                   SETON                                        30
     C                   MOVE      EDITST        KKAST
     C                   MOVE      EOSYS         KKSYS
     C                   MOVE      EOBTYP        KKBTYP
     C                   MOVEL     EOSTYP        KKSTYP
     C*
     C     BEKY          CHAIN     PBE01                              98
     C     EOTAPE        IFEQ      *BLANKS
     C  N98              MOVE      BEEDI         SCTAPE
     C                   ELSE
     C                   MOVE      EOTAPE        SCTAPE
     C                   ENDIF
     C  N98              MOVE      BEPPLN        SCPPLN
     C*
     C                   ELSE
     C*
     C                   SETOFF                                       30
     C                   ENDIF                                                  *IN99 IFEQ *OFF
     C*
     C** DETERMINE IF NETWORK TITLE AND FIELD SHOULD BE DISPLAYED
     C*
     C** SET VARIOUS FIELDS
     C*
     C                   MOVE      'D'           KKLOST
     C                   Z-ADD     EOCO#         KKCO#
     C                   Z-ADD     EOSUB#        KKSUB#
     C                   Z-ADD     EOFND         KKFND
     C                   Z-ADD     EOFYR         KKFYR
     C                   Z-ADD     EOCASE        KKCASE
     C*
     C* DISPLAY PROVIDER NETWORK IF NOT BLANKS
     C*
     C     EONTWK        IFNE      *BLANKS
     C                   MOVEL(P)  EONTWK        SCNWCD
     C                   SETON                                        16
     C                   ELSE
     C                   SETOFF                                       16
     C                   ENDIF                                                  EONTWK
     C*
     C* SET INITIAL CURSOR POSITON TO DATE FIRST CONSULTED
     C                   SETON                                        37
     C*
     C*
     C* PROTECT SPECIALTY IF EORCN>0 AND EOBTYP<>'P'
     C*
     C     EORCN         IFGT      *ZERO
     C                   SETON                                        29
     C                   ELSE
     C*
     C                   SETOFF                                       29
     C                   ENDIF                                                  EORCN>0
     C*
     C* INITIALIZE SCREEN FLAG FOR SPECIALTY CODE POPULATION
     C*
     C                   MOVE      *OFF          XXSCR2            1
      *
      * Call API for additional system control information
     C*
     C                   CALL      'SA009R'
 1   C                   PARM      *ZEROS        ##EOBD            3 0          EOMB DELETE SECURITY
 2   C                   PARM      ' '           ##EOBI            1            EOMB INTERACIVE PROC
 3   C                   PARM      ' '           ##ARIN            1            AUDIT/RECON INSTALLE
 4   C                   PARM      ' '           ##ACIS            1            AUTO CHECK ISSUANCE
 5   C                   PARM      ' '           ##PAY0            1            PRINT WCMC EOB PAY>0
 6   C                   PARM      ' '           ##SDTS            1            ALLOW FUTURE SERV DT
 7   C                   PARM      ' '           ##MCIV            1            MED CASES INIT VIEW
 8   C                   PARM      ' '           ##PBIV            1            PROV BILLS ININT VIE
 9   C                   PARM      ' '           ##AINV            1            AUTO INVOICE FLAG
10   C                   PARM      ' '           ##WHPG            1            Work History Usage
11   C                   PARM      ' '           ##PRIV            1            PROVIDER INIT VIEW
12   C                   PARM      ' '           ##TDUP            1            ADJUDICATION DUP CRI
13   C                   PARM      ' '           ##NWSL            1            ADJUDICATION DUP CRI
     C                   Z-ADD     EORCN         KKRCN
     C*
     C* PROTECT ELIG FOR DATA SUBMISSION IF SENT TO THE STATE ALREADY
     C* IT CAN BE CHANGE IN THE EDI CORRECTION SCREENS
     C*
     C     EOTAPE        IFNE      ' '
     C     EOTAPE        ANDNE     'Y'
     C     EOTAPE        ANDNE     'N'
     C     EOTAPE        ANDNE     'E'
     C                   SETON                                        24
     C                   ELSE
     C                   SETOFF                                       24
     C                   ENDIF
     C*
     C                   MOVEL(P)  'FLES'        KKTBID
     C                   MOVEL(P)  SCTAPE        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *OFF
     C     GTSTAT        ANDNE     'I'
     C                   MOVEL(P)  GTDESC        SCDSDS
     C                   ELSE
     C                   CLEAR                   SCDSDS
     C                   ENDIF
     C     @@MODE        IFEQ      'D'
     C                   MOVE      EOREVI        XXREVI            1
     C                   ENDIF
     C*** Beg Add *** 04/17/13 ****************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C     CLTID         IFEQ      '114'
     C                   MOVE      4             #OFSCR            1 0
     C                   ELSE
     C                   MOVE      3             #OFSCR
     C                   ENDIF
     C*
     C*** End Add *** 04/17/13 ****************************************
     C*** Beg Add *** 05/23/13 ****************************************
      /Free

         // Call System API to check wheather the ICD10 is active or not

         @@rtcd = *blanks;
         callp SA015R(@@rtcd);

         Select;
         // ICD10 is inactive
         When @@rtcd = '1';
         xxicd10 = '0';
         dspICdind = '1';

         // ICD10 is active
         When @@rtcd = '2';
         xxicd10 = '1';
         dspICdind = '0';
         // Beg Add *** 08/05/13 ****************************************
         // Beg Add *** 10/01/15 ****************************************
         If @@mode = 'A';
         // End Add *** 10/01/15 ****************************************
           scICdInd ='Y';
         // Beg Add *** 10/01/15 ****************************************
         Endif;
         // End Add *** 10/01/15 ****************************************
         // End Add *** 08/05/13 ****************************************
         Endsl;

         If @@MODE = 'D';
            DspicdInd = '1';
         Endif;

      /End-Free
     C*** End Add *** 05/23/13 ****************************************
       //*** Beg Add *** 06/27/19 *************************************
      /Free
         isPbillsLite = isModEnabled('Pbills Lite');
      /End-Free
       //*** End Add *** 06/27/19 *************************************
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1USER        EUUSER
     C                   MOVEL     Q1PGM         EUPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        EUTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EUDT
     C*
     C                   UPDATE    DBRCDF
     C                   ENDIF
     C*
     C** UPDATE EOMB TRANSACTION FILE
     C*
     C     @@RRN2        CHAIN     PEO01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  SCSPCD        EOSPCD
     C                   MOVEL(P)  SCDIA1        EODIA1
     C                   MOVE      SCTAPE        EOTAPE
     C     UPDATE        IFEQ      *ON
     C                   MOVEL(P)  SCFLG7        EOFLG7
     C                   MOVEL     *OFF          UPDATE
     C                   ENDIF
     C                   MOVEL(P)  SCFZIP        EOFLIC
     C*** Beg Add *** 07/18/25 ***************************************
     C                   MOVE      SCZIP         EOFLIC
     C*** End Add *** 07/18/25 ***************************************
     C                   MOVE      SCREVI        EOREVI
     C*** Beg Add *** 01/18/15 ******************************************
     C                   Eval      EOCtyMod = DecCtyMod
     C*** End Add *** 01/18/15 ******************************************
     C                   MOVEL     Q1USER        EOUSER
     C                   MOVEL     Q1PGM         EOPGM
     C                   Z-ADD     DSTIME        EOTIME
     C                   MOVEL     SRDTO         EODT
     C                   UPDATE    PEO01
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C* If in ADD mode and @@RRN (FAEUP) is not zeros, then the user
     C* has used F12 from detail screen.  Must delete previous written
     C* Header record and write the new one.  This prevents having two
     C* Header records for the bill.
     C*
     C     @@RRN         IFNE      *ZEROS
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C                   DELETE    DBRCDF
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  @@RRN NE *ZEROS
     C*
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   Z-ADD     EOEOB#        EUEOB#
     C                   MOVEL     Q1USER        EUUSER
     C                   MOVEL     Q1USER        EUIUSR
     C                   MOVEL     Q1PGM         EUPGM
     C                   MOVEL     Q1PGM         EUIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        EUTIME
     C                   Z-ADD     DSTIME        EUITIM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EUDT
     C                   MOVEL     SRDTO         EUIDT
     C*
     C                   WRITE     DBRCDF
     C     EUKY          CHAIN     PEU01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DBRRNA        @@RRN
     C                   ENDIF                                                  *IN99 = *OFF
     C     @@RRN2        CHAIN     PEO01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  SCSPCD        EOSPCD
     C                   MOVEL(P)  SCDIA1        EODIA1
     C                   MOVE      SCTAPE        EOTAPE
     C                   MOVEL(P)  SCFZIP        EOFLIC
     C*** Beg Add *** 07/18/25 ***************************************
     C                   MOVE      SCZIP         EOFLIC
     C*** End Add *** 07/18/25 ***************************************
     C                   MOVE      SCREVI        EOREVI
     C*** Beg Add *** 01/18/15 ******************************************
     C                   Eval      EOCtyMod = DecCtyMod
     C*** End Add *** 01/18/15 ******************************************
     C     UPDATE        IFEQ      *ON
     C                   MOVEL(P)  SCFLG7        EOFLG7
     C                   MOVEL     *OFF          UPDATE
     C                   ENDIF
     C                   MOVEL     Q1USER        EOUSER
     C                   MOVEL     Q1PGM         EOPGM
     C                   Z-ADD     DSTIME        EOTIME
     C                   MOVEL     SRDTO         EODT
     C                   UPDATE    PEO01
     C                   ENDIF
     C*
     C                   FEOD      FAEUP
     C*
     C                   MOVE      '1'           OUTFLG
     C                   ENDSR
**
ABCDEFGHIJKLMNOPQRSTUVWXYZ
**
0123456789
**
8321222328323334
