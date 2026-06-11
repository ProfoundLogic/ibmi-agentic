     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H BNDDIR('STBNDDIR')
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE F0353R - Physician (HFCA) Detail Information
     H*****************************************************************
     H*
     H* F0353R - Physician (HFCA) Detail Information
     H*
     H* May 1998  C:56   Project: MC152
     H*****************************************************************
     H*
     H** CALLED FROM:
     H*    F0350R99 - PBILLS CONTROLLING PROGRAM
     H*    F0390R   - WORK WITH PBILLS DETAIL
     H*    F0350R   - WORK WITH PBILLS
     H*  CALLS TO:
     H*    N0037R   - SELECT NETWORK CONTRACTS
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  07/18/25  C:342 Project MC451
     H*                  NCCI Med Data Call Enhancements
     H*
     H*  08/06/25 C:333 Project: 114MIA
     H*                 PA Discount Algorithm
     H*
     H*  07/15/24 C:342 Project: FS688
     H*                 2024 USH&L Fee Schedule (recompile)
     H*
     H*  07/18/23 C:342 Project: NY060
     H*                 NY Compliance: CARC/RARC Update
     H*
     H*  07/02/20 C:079 Project : FS583
     H*                 Add Proc Code edit for End Date in USH&L file
     H*
     H*  03/31/20 C:079 Support : 99/87616
     H*                 Default 1 in Proc code for Code Info WDW window
     H*
     H*  10/15/19 C:212 Project : FS563
     H*                 For ASC load the RUEFDT from type A instead of P
     H*
     H*  01/31/19 C:079 Project : WV018
     H*                 For Code Info use correct date in the key list
     H*
     H*  11/05/18 C:079 Project : FS547
     H*                 Use service date in key for editing HL codes
     H*
     H*  01/09/18 C:079 Project : FS538
     H*                 Use service date in key for getting Lab code RRN
     H*
     H*  06/06/17 C:079 Support : 102/47618
     H*                 Do not reset the code type CArcFl on a recon
     H*
     H*  08/24/16 C:307 SUPPORT : 43022
     H*                 Code changes to avoid the messages.
     H*
     H*  04/04/16 C:212 and C:079
     H*                 Project: OK022                                      ag
     H*                 multiple changes related to medicare fee schedule flag
     H*
     H*  01/18/16  C:079  Support: 146/38952
     H*                   Corrected the logic for multiple units when adj
     H*                   code is override
     H*
     H*  11/02/15  C:309  SUPPORT: 37884 (EOR missing information)
     H*                   Adding NDC code on screen and related program
     H*                   changes
     H*
     H*  10/07/15  C:079  Project: FS373
     H*                   Set MedUnt for USH&L
     H*
     H*  09/15/15  C:079  Support: MC090 37261
     H*                   Move ICD-10 description over two spaces
     H*
     H*  08/14/15  C:309  Project : CT011 - 2015 CT FS
     H*                   Add a condition to exclude codes
     H*                   beginning with 'L' or 'G'.
     H*
     H*  05/19/15  C:079  Support: MC129 34743
     H*                   On a recon do not update FDDHP
     H*
     H*  08/25/14  C:079  Support: MC129 86582
     H*                   Corrected the logic for multiple units
     H*
     H*  08/14/14  C:274  Support: 86496
     H*                   For KY, modify so to set MEDUNT flag for HCPCS codes
     H*
     H*  07/11/14  C:280  Support: 86063
     H*                   Change KKICD9 to movel                       s.
     H*
     H*  02/04/14  C:290  PRISS: 17 Project SY336
     H*                   Corrected the logic for populating ICD10 codes.
     H*                   Includes Display file changes also.
     H*
     H*  11/19/13  C:296  Support: 84206
     H*                   Change option on physician detail screen
     H*                   for state = 'SC'
     H*
     H*  06/07/13  C:277  Support:MC099 81106
     H*                   Don't show Medicare fields for State 'SC'
     H*
     H*  05/30/13  C:284  Support: 82005
     H*                   Populate effective date before reading
     H*                   FTRTL2.
     H*
     H*  12/05/12  C:079  Support:MC114 81221
     H*                   Do not clear the ClaimCheck Done flag             n.
     H*
     H*  11/14/12 C:287 Project: MC367
     H*                 Disabled functions 14 and 15 for GS. These were done in
     H*                 display file source.
     H*
     H*  10/18/12  C:284  Project:114MHU
     H*                   Add condition to by pass edits on certain condition.
     H*
     H*  09/21/12  C:079  Support:MC099 80806
     H*                   Dont split anes codes into multiple lines for
     H*                   Medicare Codes in WRITER
     H*
     H*  07/26/12  C:079  Project: TN016
     H*                   Modify so to set MEDUNT flag for HCPCS codes
     H*
     H*  07/26/12  C:277  Project: TN016
     H*                   Modify so that we dont use Medicare Files for
     H*                   TN.
     H*
     H*  06/21/12  C:277  Project:114MFI
     H*                   Disable F9 when Adjudication state = HL
     H*
     H*  09/21/11  C:079  Support:MC099 77825
     H*                   Dont split anes codes into multiple lines for
     H*                   Medicare Codes (correction)
     H*
     H*  08/19/11  C:079  Project:114MYS
     H*                   Dont split anes codes into multiple lines for
     H*                   Medicare Codes.
     H*
     H*  08/16/11  C:274  Project:CT007
     H*                1. Modify F09=Search window for CT to search
     H*                   the Medicare DME file and it will bring up
     H*                   Work With Medicare DME Rates program.
     H*                2. Modify Edit for CT Medicare DME to look
     H*                   at the Medicare DME File.
     H*
     H*  06/22/11  C:198  Project:114MYS
     H*                   Added logic to bverify if payment limit is reached
     H*
     H*  06/20/11  C:079  Support:MC146 77192
     H*                1. Modify so that we dont use Medicare Files for
     H*                   SC.
     H*
     H*  04/12/11  C:190  Project:KY019
     H*                1. Modify F9=Code info so that ambulance codes
     H*                   will display the Medicare Ambulance
     H*                   Maintenance Screen.
     H*                2. Modify F10=Search window for KY to search
     H*                   the ambulance file and it will bring up
     H*                   Work With Medicare Ambulance Rates program.
     H*                3. Modify Edit for KY ambulance codes to look
     H*                   at the Medicare Ambulance File.
     H*
     H*  03/25/11  C:257  Support:76181
     H*                   For Place of Service 24,state requiredis TX
     H*
     H*  01/10/11  C:227  Support:75345  MC114
     H*                   Place of Service/Type of Service for NJ state
     H*
     H*  12/09/10  C:227  Project:25MRV
     H*                   Added call for ADL functionality for client 25
     H*
     H*  11/22/10  C:198  Project WV009 Change
     H*                   Fixed the issue  of F9=Code info when proc
     H*                   code is not found in R & C file.
     H*
     H*  10/01/10  C:198  Project WV009
     H*              079  More options/edits on F9=Code info/F2/F10
     H*                   if state is based on medicare FS.
     H*
     H*  08/12/10  C:198  Project 114MWS
     H*                   More options on F9=Code info if bill is
     H*                   Medicare based PPO bill.
     H*
     H*  10/26/09  C:227  Support 70331
     H*                   ASC facility billing issue
     H*
     H*  07/22/09  C:227  Project AR038
     H*                   Changes in KIDO for AR state
     H*
     H*  06/30/09  C:227  Project TX063
     H*                   Check for FMASP for ASC specialty code
     H*
     H*  06/04/09  C:198  Support 68719
     H*                   Read FAESL5 with no lock when it is not updating
     H*
     H*  05/18/09  C:220  Project:MC334
     H*                   Add Rendering NPI on F1=Repeat
     H*
     H*  05/12/09 C:227 Project:TX061
     H*                 Remove TWCC-ADL functionality
     H*
     H*  04/01/09  C:227  Support: 67397
     H*                   Body Area not required for Temporary Codes.  URE
     H*
     H*  02/28/09  C:079  PROJECT: MC288
     H*                   APPLY MC288 CHANGES FROM F0353RFL            URE
     H*
     H*  11/14/08  C:212  PROJECT: TX043
     H*                   ADD PROCESSIG FOR NEW WINDOW ADDED FOR PROCEDURE
     H*                   CODE SELECTION (F9)
     H*
     H*  09/15/08 C:198 Support: 64232                                     *
     H*                 Corrected SG modifier Processing.                  *
     H*
     F*  02/08/08  C:189  PROJECT: MC319
     F*                   DO NOT ALLOW INACTIVE CODES FOR PLACE OF SERVICE
     F*
     F*  05/23/07  C:062  PROJECT: MC295
     F*                   ADD RENDERING PROVIDER NPI#
     H*
     H*  04/17/07  C:062  PROJECT: GO144 57408
     H*                   KEEP CURSOR POSITION WHEN F24 IS USED.
     H*
     H*  02/27/07  C:062  PROJECT: 114MMZ
     H*                   CALL MEDICARE RATE INFO.
     H*
     H*  07/22/06  C:079  PROJECT: NY012
     H*                   DISPLAY THE CORRECT REGION
     H*
     H*  01/26/06  C:079  SUPPORT: 51135
     H*                   USE HLFDAT TO COMPARE SERVICE DATE
     H*
     H*  06/20/05  C:079  SUPPORT: 47606
     H*                   1. PASS SCCHRG AND SCPAID TO F0356R01
     H*
     H*  03/10/05  C:079  SUPPORT: 46036
     H*                   1. F1 REPEAT WILL POPULATE BODY AREA
     H*
     H*  12/19/04  C:079  SUPPORT: 45094
     H*                   1. SET REG 4 FOR TUPP PROVIDERS
     H*                   2. ADD DEFAULT REGION FOR NY
     H*
     H*  09/28/04  C:079  PROJ: TX026
     H*                   ADD DISPLAY OF ASC RATES FOR F9=CODE INFO
     H*                   REQUIRE COUNTY FOR TEXAS ASC BILLS
     H*
     H*  08/10/04  C:079  SUPPORT: 43504
     H*                   ADD WINDOW FOR RECODING TO USE EOBR CODE S11
     H*
     H*  04/15/04  C:079  PROJECT FL046
     H*                   FLORIDA: EDIT EOBR CODES
     H*
     H*  04/26/04  C:062  PROJECT FS279
     H*                   POPULATE BILL CODE DESCRIPTION
     H*
     H*  02/06/04  C:079  SUPPORT MC025 41199
     H*                   USE CORRECT DATE FORMAT FOR TEXAS BILLED CODE
     H*
     H*  12/12/03  C:079  PROJECT FL048
     H*                   DISPLAY REGION/LOCALITY FOR FLORIDA
     H*
     H*  09/19/03  C:062  PROJECT TX021
     H*                   -ADD F16=ADL INFO
     H*                   -ADD F24=MORE KEYS
     H*
     H*  07/10/03  C:079  PROJECT TX020
     H*                   1. DEFAULT DISPLAY OF LOCALITY BASED ON STATE
     H*                   2. LIFT VALID CODE EDIT AFTER 8/1/03
     H*                   2. LIFT EDIT ON SURGERY AFTER 8/1/03
     H*
     H*  04/25/03  C:079  PROJECT FS255
     H*                   CHANGES TO F1=REPEAT
     H*
     H*  04/03/03  C:079  SUPPORT MC044 36611
     H*                   SCPOS CHANGED TO 3.0N
     H*
     H*  02/13/03  C:079  PROJECT 114MCG
     H*                   ADD EDIT ON HCPCS CODES TO REQUIRE MODIFIER
     H*
     H*  01/07/03  C:062  PROJECT FS244
     H*                   USE EOFLIC TO GET THE GEO ZIP
     H*
     H*  12/20/02  C:079  PROJECT: NW075
     H*                   CALL N0037R TO DETERMINE MANUAL FEE SCHEDULE
     H*
     H*  11/11/02  C:079  PROJECT 114MDC
     H*                   IF REDUCTIONS ARE ZERO BILL IS GOING THROUGH
     H*                   CODEREVIEW THEN ZERO OUT CHARGES ON
     H*
     H*  09/19/02  C:079  PROJECT NY006
     H*                   1. ADD DISPLAY OF REGION/LOCALITY SCRGN#
     H*
     H*  08/21/02  C:062  PROJECT 114MDC
     H*                   IF THERE IS A CHANGE OR ADD, THEN SET
     H*                   EOCRDO TO BLANK. EVEN DELETE
     H*
     H*  08/07/02  C:062  PROJECT FS244
     H*                   ZIP AREA IS NOW BEING ASSIGNED AT THE LINE
     H*                   ITEM LEVEL.
     H*
     H*  07/16/02  C:062  SUPPORT:MC025 33431
     H*                   KEEP KKDSEQ VALUE.
     H*
     H*  07/05/02  C:062  SUPPORT:FS246
     H*                   ALLOW DATE WINDOW TO POP UP EVEN IF NOT
     H*                   AN EDI BILL.
     H*
     H*  06/03/02  C:062  SUPPORT:MC114 32705
     H*                   POPULATE SV1A2 WHEN USING FMRCP FOR CORRECT
     H*                   SPLITTING OUT OF UNITS INTO MULTIPLE LINES
     H*
     H*  04/08/02  C:062  PROJECT:FS236
     H*                   POPULATE CAPRTY WITH EOBTYP.
     H*
     H*  04/05/02  C:079  PROJECT:114MAO
     H*                   DELETE MESSAGES FROM CORRECT CODING.
     H*
     H*  01/07/02  C:062  PROJECT:FS235
     H*                   DELETE ONLY EOB CODES IF CURRENT RECON LEVEL.
     H*
     H*  12/20/01  C:079  SUPPORT:30445
     H*                   IF AST=UC THEN DROP DOWN TO FMPRPUC AFTER
     H*                   FAILING ON FMRCP
     H*
     H*  12/19/01  C:079  PROJECT:95MAX
     H*                   CHANGE SCUNTS TO 5.2 TO ALLOW DECIMAL ENTRY
     H*                   COMPILE ONLY
     H*
     H*  11/08/01  C:079  PROJECT:MC249
     H*                   On 2=change if units > 1 and PR1A2 is blank
     H*                   split into multiple line items
     H*
     H*  10/31/01  C:079  SUPPORT:MC055 29571
     H*                   On an override don't force and EOMB code if
     H*                   percent >= 100.
     H*
     H*  09/19/01  C:079  PROJECT:MC246
     H*                   DHUNTS CANNOT = 0 SO. IF IT IS THEN SET = 1
     H*
     H*  08/31/01  C:079  SUPPORT:MC114 28202
     H*                   DON'T BREAK INTO MULTIPLE UNITS IF PR1A2
     H*                   = 'U'.
     H*                                                               *
     H*  08/30/01  C:079  PROJECT: WV003                              *
     H*                   REQUIRE LT OR RT MODIFIERS FOR CERTAIN      *
     H*                   VISION CODES FOR WV.                        *
     H*
     H*  08/27/01  C:062  PROJECT:TX015
     H*                   -REMOVE 7/17/01 REVISION
     H*                   -ADD EDIT/SEARCH ON BILLED PROCEDURE CODE
     H*
     H*  07/17/01  C:079  SUPPORT: MC025 25456
     H*                   Pop up error window for R&C codes if tape sub
     H*                   flag=Y.
     H*
     H*  05/31/01  C:062  PROJECT: MC114 27653
     H*                   DON'T EDIT FIELDS THAT ARE PROTECTED.
     H*
     H*  05/24/01  C:062  PROJECT: FS209
     H*                   LOAD CACODE WITH DHPRCD
     H*
     H*  05/02/01  C:079  SUPPORT: MC114 27315
     H*                   1. Change conflicting indicator use on SCBLCD
     H*                      which was causing invalid code on SCPRCD.
     H*                   2. Make FTABL, FMPRL, and FTPSL user
     H*                      controlled files and close them on return
     H*                      in case adj state changes on next entry.
     H*
     H*  04/18/01  C:079 Support: MC048 27094
     H*                  Use SCBLCD in the key when verifying in the
     H*                  R&C file
     H*
     H*  04/18/01  C:079  PROJECT 90MCS
     H*                   For LA surgery codes allow body areas:
     H*                   'O','A','B1','B2'
     H*
     H*  03/26/01  C:062  PROJECT FSMNT
     H*                   CHANGE PROCESSING OF EOB NOTES/CODES
     H*
     H*  02/15/01  C:062  PROJECT MCMNT   BASE ONLY
     H*                   COMPILE ONLY
     H*
     H*  01/16/01  C:062  Support MC048 25852
     H*                   WHEN AST IS "UC", ONLY CHECK AGAINST R&C.
     H*
     H*  01/15/01  C:079  Support MC048 25778
     H*                   When anes flag = H (Hourly) don't break item
     H*                   into multiple units.
     H*
     H*  12/08/00  C:79   PROJECT:PA008
     H*                   Condition chain to FMRCP based on state
     H*                   control file (STSTP) flag STBS02.
     H*
     H*  11/03/00  C:079  Support MC081 24815
     H*                   Initialize SCUNTS to 1 on an add
     H*
     H*  10/13/00  C:062  Support MC238
     H*                   ADD LOGIC TO DEAL WITH NON-SEARCHABLE FIELDS.
     H*
     H*  10/13/00  C:79   Project: MCMNT
     H*                   Correct conditions for CARERR window to be
     H*                   displayed. Use GT not GE.
     H*
     H*  09/06/00  C:79   Project: LA015
     H*                   For surgery codes body area S1-S5 is required
     H*
     H*  08/30/00  C:79   Support: MC081/#23976
     H*                   Add subroutine for NC body area edit.
     H*
     H*  08/15/00  C:62   PROJECT: MC215
     H*                   REPLACE SFBPF REFERENCES WITH APIS.
     H*
     H*  08/08/00  C:079  Proj: OK010
     H*                   Retain units field for Rental codes
     H*
     H*  08/02/00  C:117  Proj: FL033
     H*                   Added error window for user defined CPT codes
     H*
     H*  04/28/00  C:079  Support:MC010/#22721
     H*                   Modify Florida edit for Anesthesia with mod
     H*                   47 to include A6 body area.
     H*
     H*  04/11/00  C:117  PROJECT:FS211
     H*                   ADDITIONAL EDITS TO PBILLS PGMS FOR DATA
     H*                   SUBMISSION (Added CARERR & EDIERR subrs)
     H*
     H*  04/04/00 C:117 PROJECT MCMNT
     H*                 Changed function F15 to "Display Lines" rather
     H*                 than "Previous Lines".
     H*
     H*  04/02/00 C:117 PROJECT: MCMNT
     H*                 DISPLAY "EOB CODE NEEDED" MSG WHEN F12=CANCEL
     H*                 FROM F0356R01 (EOB CODE/NOTES)
     H*
     H*  03/07/00 C:79  Project: FSMNT
     H*                 Remove screen billed code on repeat processing
     H*
     H*  02/25/00 C:117 SUPPORT: MC044/21153
     H*                 Added Adj. code to appear on F1=Repeat
     H*
     H*  02/24/00 C:79  PROJ: FSMNT
     H*                 Remove clearing of red/paid fields. This is
     H*                 done in FA713R.
     H*
     H*  02/18/00 C:79  PROJ: FSMNT
     H*                 Put editing back on billed procedure code.
     H*
     H*  02/04/00 C:117 PROJ: FS144
     H*                 CAITIM NOT BEING POPULATED
     H*
     H*  01/11/00 C:62  Support: NW012
     H*                 -REMOVE LOGIC OF MANUAL FEE SCHEDULE.
     H*                 -DON'T CROSS-FOOT IF CSCD = '300', THIS IS FOR
     H*                  THE MANUAL FEE SCHEDULE OVERRIDE CALCULATION.
     H*                 -DON'T DELETE MESSAGE IF 'NW00001'.
     H*
     H*  01/10/00 C:56  Support: MC044 20222
     H*                 Add 'Entered Date' to write of FDCAP when
     H*                 multiple units are entered.
     H*
     H*  01/04/00 C:79  Project: MS008
     H*                 1. Keep units for mileage code, ie ambulance
     H*
     H*  12/03/99 C:79  Project: 48MDA
     H*                 1. Add state specific edits for AL, LA, NE
     H*                    MS, NM, NY, OH.
     H*                 2. Move TX state spec editing to subroutine
     H*
     H*  11/23/99 C:79  Support: CLT044/20789
     H*                 Add state specific edit for Florida phys
     H*                 therapy codes prior to 9/1/97.
     H*
     H*  11/12/99 C:56  MC152
     H*                 Delete/Change efficiency isssue.
     H*
     H*  09/30/99 C:56  Support: MC152
     H*                 Add screen billed code to repeat processing
     H*
     H*  09/16/99 C:56  Support: MC072 #19641
     H*             1.  Correct writing of reductions values and paid
     H*                 amount when multiple units are entered for an
     H*                 override adjudication code.
     H*
     H*             2.  Add write of eob code for subsuquent line items
     H*                 when multiple units.
     H*
     H*  08/27/99 C:56  MC152
     H*                 When Adjudication Code is override type and
     H*                 override percentage is zero, delete any
     H*                 adjudication/network eob codes or messages.
     H*
     H*  08/26/99 C:79  Support: MC025
     H*                 Add condition on procedure code modifier edit
     H*                 to give error if it's inactive
     H*
     H*  08/18/99 C:56  Project: MC152
     H*                 Do not validate Billed Code per Richie Moore.
     H*
     H*  08/04/99 C:79  Support#:18956
     H*                 Initialize CAPMOD to blanks if no modifier in
     H*                 FDDHP
     H*
     H*  07/23/99 C:31  Project: NW010
     H*                 WC Summary File
     H*                 1. Add Manual fee schedule message and code
     H*
     H* 06/15/99 C:56  Project: MC152
     H*                Correct problem of not being able to operate
     H*                function keys after taking F1=Repeat.
     H*                                                               *
     H* 06/08/99 C:79  Project: OK008                                 *
     H*                Add condition for calculating anesthesia units *
     H*                when this is Oklahoma bill anes by surgeon     *
     H*                                                               *
     H* 05/19/99 C:79  Project: FL019                                 *
     H*                Write payment plan EHPPLN to DHPPLN for tape   *
     H*                submission.                                    *
     H*                                                               *
     H* 05/18/99 C:56  Project: MC152                                 *
     H*                Changed FDCALB to FDCALF                       *
     H*                                                               *
     H* 05/13/99 C:56  Support: MC025 #18021                          *
     H*                1. Capture remainder from dividing charged     *
     H*                   amount by units                             *
     H*                                                               *
     H* 05/05/99 C:56  Support: MC025 #17939                          *
     H*                Add deletion of FDDAP records when line item   *
     H*                is deleted.                                    *
     H*                                                               *
     H* 04/19/99 C:56  Support: MC025  #17566                         *
     H*                If adjudication code is not override type,     *
     H*                clear PAID/MRA/OTH/OTH2/NTWK reductions fields.*
     H*                This will prevent extraneous information from  *
     H*                being written in these fields.                 *
     H*                                                               *
     H* 04/06/99 C:79  Project: FLMNT                                 *
     H*                Write R&C flag to FDDHP (DHRCFL)               *
     H*                                                               *
     H*
     H* 04/01/99 C:56  Project: MC152
     H*                Allow zero charge amount for an override bill
     H*                type per request from TASB.
     H*
     H* 03/30/99 C:56  Project: MC152                                 *
     H*                Add call to EOB Codes/Notes program when an    *
     H*                override type adjudication code is entered.    *
     H*                If any reductions is entered or calculated,    *
     H*                require entry of EOB code.                     *
     H*                                                               *
     H* 03/23/99 C:79  Support #: 17240                               *
     H*                Move SCUNTS into DHUNTS for anesthesia codes   *
     H*
     H* 02/23/99 C:56  Project: MC152                                 *
     H*                Add deletion of EOB Codes/Notes and Error      *
     H*                Messages to subroutine DLTPRE.                 *
     H*                                                               *
     H* 01/18/99 C:79 PROJ:25MKO
     H*               1.  Only allow override adjudication codes for
     H*                   Vendor Fee bills
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      29      Add mode                                       **
     H**      12      County code blank/invalid                      **
     H*****************************************************************
     H*
     FF0353F    CF   E             WORKSTN
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F*
     F*  Physical file definition (for maintaining record)
     FFDDHP     UF A E             DISK
     F                                     RENAME(PDH01:DBRCDF)
     F*
     F*  EOMB Transaction File
     FFAEOP     UF   E             DISK
     F*
     F*  Type of Service File
     FFTTSP     IF   E             DISK
     F*
     F*  Fee Schedule Detail File
     FFDCALF    IF   E           K DISK
     F*
     F*  Fee Schedule Detail File
     FFDCAP     UF A E             DISK
     F                                     RENAME(PCA01:PCA02)
     FFDCAL12   IF   E           K DISK
     F                                     RENAME(PCA01:PCA03)
     FFDCAL10   IF   E           K DISK
     F                                     RENAME(PCA01:PCA04)
     F*
     F*  EOMB Detail EOB/Notes Transaction File
     FFDDEL     UF   E           K DISK
     FFDDEL3    IF   E           K DISK
     F                                     RENAME(PDE01:PDE03)
     F*
     F*  Fee Schedule Claims Status Code File
     FFTCSP     IF   E             DISK
     F                                     RENAME(PCS01:PCS02)
     F*
     F*  Place of Service Table File
     FFTPSL     IF   E           K DISK    USROPN
     F*
     F*  Procedure Code File
     FFMPRL     IF   E           K DISK    USROPN
     F                                     INFDS(DBFPR)
    F***** Beg Add *** 11/02/15 **************************************
     F*  WCMC NDC Drug Code File
     FFMRXP     IF   E             DISK
     F                                     RENAME(PRX01:PRX02)
     FFMRXL     IF   E           K DISK
     F*
    F***** End Add *** 11/02/15 **************************************
     F*  Provider Bills Control File
     FFMBEL     IF   E           K DISK
     F*
     F*  WCMC Control File
     FFMCXL     IF   E           K DISK
     F*
     F*  Bill Status Table File
     FFTBSL     IF   E           K DISK
     FFTBSP     IF   E             DISK
     F                                     RENAME(PBS01:PBS02)
     F*
     F*  EOMB Detail Amount Transaction File
     FFDDAL2    UF   E           K DISK
     F*
     F*
     F*  EOMB Phys/HCFA Header File
     FFAEHL     UF   E           K DISK
     F*
     F*  Diagnosis Code Physical File
     FFMDIL     IF   E           K DISK
     F*
     F*  Logical over all Tabel Files
     FFTABL     IF   E           K DISK    USROPN
     F                                     RENAME(PTS01:PTS02)
     F                                     IGNORE(PAC01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PSP01)
     F                                     IGNORE(PAT01)
     F                                     IGNORE(PFT01)
     F                                     IGNORE(PPS01)
     FFTCCP     IF   E             DISK
     F                                     RENAME(PCC01:PCC02)
     FFAAGLTX   IF   E           K DISK
     F                                     INFDS(DBFDD)
     F*
     F* Pre-Cert Master File
     FUMPCP     IF   E             DISK
     FUMPCL3    IF   E           K DISK
     F                                     RENAME(PPC01:PPC02)
     F*
     F* WCMC Claim Master File
     FFMCML1    IF   E           K DISK
     F                                     INFDS(DBFDB)
     FFDDHL     IF   E           K DISK
     F                                     INFDS(DBFDC)
     F*
     F* State FS Rules/Pgm Cross Reference File
     FFDRUL1    IF   E           K DISK
     F* REASONABLE AND CUSTORMARY FILE
     F*
     FFMRCL     IF   E           K DISK
     F                                     INFDS(DBFRC)
     FFDMGL2    UF   E           K DISK
     FFDDEP     O    E             DISK
     F                                     RENAME(PDE01:PDE02)
     FFAESL5    UF   E           K DISK
     F*  State Code Table File
     FSTSTL     IF   E           K DISK
     FFMVCLTX   IF   E           K DISK
     FFMP1L     UF   E           K DISK
     FFMGZL     IF   E           K DISK
     F*
     F*  Error log file
     FFMZRL1    IF   E           K DISK    USROPN
     FFMMCL1    IF   E           K DISK    INFDS(DBFMC)
     F                                     USROPN
     F*
     FFMASL1    IF   E           K DISK
     F                                     INFDS(DBFAS)
     FFTRVL1    IF   E           K DISK
     F                                     INFDS(DBRVU)
     FFTLBL1    IF   E           K DISK
     F                                     INFDS(DBLAB)
     FFMAML2    IF   E           K DISK
     FFMDFL1    IF   E           K DISK
     F                                     INFDS(DBDRG)
     FFMDEL2    IF   E           K DISK
     F                                     RENAME(PDE01:PDM02)
     FFTLBL2    IF   E           K DISK
     F                                     RENAME(PLB01:PLB02)
     FFTRVL3    IF   E           K DISK
     F                                     RENAME(PRV01:PRV03)
     FFMDEL1    IF   E           K DISK
     F                                     INFDS(DBDME)
     F                                     RENAME(PDE01:PDM01)
     FFMAML1    IF   E           K DISK
     F                                     INFDS(DBAMB)
     F                                     RENAME(PAM01:PAM11)
     FFTRTL2    IF   E           K DISK
     FFMFNL     IF   E           K DISK
     F*** Beg Add *** 06/21/12 ****************************************
     FFmhlp     if   e             disk
     F                                     rename(phl01:phl02) Prefix(h2:2)
     F*
     FFmhll1    if   e           k disk    rename(phl01:phl03) Prefix(h3:2)
     FFtaap     if   e             disk
     F*** Beg Chg *** 11/05/18 ****************************************
     F*Ftaal1    if   e           k disk    rename(paa01:paa02) Prefix(a2:2)
     FFtaal2    if   e           k disk    rename(paa01:paa02) Prefix(a2:2)
     F*** End Chg *** 11/05/18 ****************************************
     F*** End Add *** 06/21/12 ****************************************
      *** Beg Add *** 02/04/14 ****************************************
     Ffmicd10l1 if   e           k disk
      *** End Add *** 02/04/14 ****************************************
     F*** Beg Add *** 11/05/18 ****************************************
     FFmhll2    if   e           k disk    rename(phl01:phl04) Prefix(H4:2)
     F*** End Add *** 11/05/18 ****************************************
     F*
      /COPY SCOPYMT01D
     F*
     D*
     D QCA             S             56    DIM(8) CTDATA PERRCD(1)
     D EOBR1           S              3    DIM(13) CTDATA PERRCD(1) ASCEND
     D EOBR2           S              3    DIM(4) CTDATA PERRCD(1) ASCEND
     D MOD             S              2    DIM(4)
     D*
     D DBFDB           DS
     D  DBRRNB               397    400B 0
     D DBFPR           DS
     D  PRRRN                397    400B 0
     D DBFRC           DS
     D  RCRRN                397    400B 0
     D DBFDC           DS
     D  DBRRNC               397    400B 0
     D DBFDD           DS
     D  DBRRND               397    400B 0
     D DBFMC           DS
     D  DBRRMC               397    400B 0
     D DBFAS           DS
     D  ASRRN                397    400B 0
     D DBRVU           DS
     D  RVRRN                397    400B 0
     D DBLAB           DS
     D  LBRRN                397    400B 0
     D DBAMB           DS
     D  AMRRN                397    400B 0
     D DBDRG           DS
     D  DRRRN                397    400B 0
     D DBDME           DS
     D  DMRRN                397    400B 0
     D*
     D                 DS
     D  TMTOS                  1      5
     D  HLKEY                  1      2
     D  HDKEY                  3      5
     D                 DS
     D  QQCMD                  1     56
     D  QQST                  40     41
     D                 DS
     D  QQCMD2                 1     56
     D  QQST2                 38     39
     D                 DS
     D  P1CNTY                 1     15
     D  YYCNTY                 6      9
     D*
      *** Beg Add *** 02/04/14 ****************************************
     D kkicd10         s                   like(iccode)
      *** End Add *** 02/04/14 ****************************************
     D*** Beg Add *** 01/18/16 ****************************************
     D isOvrd          s              1A
      *** Beg Add *** 08/24/16 ****************************************
     D inFMMCP         s              1
      *** End Add *** 08/24/16 ****************************************
     D svPaid          s                   like(CApaid)
     D svMRA           s                   like(CAmra)
     D svNdis          s                   like(CAndis)
     D svOthr          s                   like(CAothr)
     D svOth2          s                   like(CAoth2)
     D*** End Add *** 01/18/16 ****************************************
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Physician (HCFA) Det-
     D                                     ail Information')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)

     D*** Beg Add *** 06/21/12 ****************************************
     DRtvPrcCd         pr                  extpgm('F2386R')
     D inCall                         1
     D inKC                           1
     D inKl                           1
     D inRrn                          9  0
     D*
     D inCall          s              1
     D inKC            s              1
     D inKl            s              1
     D inRrn           s              9  0
     D*** End Add *** 06/21/12 ****************************************
     D*** Beg Add *** 10/18/12 ****************************************
     DEdtcde           s              1    Inz('Y')
     D*** End Add *** 10/18/12 ****************************************
     d*** Beg Add *** 08/06/25 ****************************************
     d isMedicare      s              1n
     d strMsg          s            100a
     d/copy sPrGenPop
     d*** End Add *** 08/06/25 ****************************************
     d*** Beg Add *** 07/18/25 ***************************************
     d invalidPos      s              2  0
      /copy sPrModule
     d*** End Add *** 07/18/25 ***************************************
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          FDDHP
     C                   PARM                    @@RRN2            9 0          FAEOP
     C                   PARM                    @@RRN3            9 0          FDCAP
     C                   PARM                    @@NWCD            5
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C* KEY LIST FOR WCMC DETAIL FILE
     C*
     C     CAKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKDSEQ
     C*
     C     CAKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     CAKY3         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKEOB#
     C*
     C     CAKY4         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKLIN#
     C*
     C* KEY LIST FOR EOB/Notes File
     C*
     C     DEKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*
     C     DEKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C                   KFLD                    KKDSEQ
     C*
     C*  KEY LIST FOR PLACE OF SERVICE FILE
     C*
     C     PSKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKPOS
     C*
     C*  KEY LIST FOR TABLE FILE
     C*
     C     TBKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKKEY
     C*
     C*  KEY LIST FOR PROCEDURE CODE FILE
     C*
     C     PRKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKPRCD
     C                   KFLD                    KKFDAT
     C*
     C     PRKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKPRCD
     C*
    C***** Beg Add *** 11/02/15 **************************************
     C*
     C     RXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKNDC
     C*
    C***** End Add *** 11/02/15 **************************************
     C*  Key List for WCMC Control File
     C*
     C     CXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C* Key List for Bill Status Tabel File
     C*
     C     BSKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTYPE
     C                   KFLD                    KKCODE
     C*
     C* Key List for EOMB Detail Amount Transaction File
     C*
     C     DAKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*
     C*
     C* KEY LIST FOR EOMB PHYS/HCFA HEADER FILE
     C*
     C     EHKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C*  Key List for Diagnosis Code File
     C*
     C     DIKY          KLIST
     C                   KFLD                    KKICD9
     C                   KFLD                    KKLOST
     C*
     C* KEY LIST FOR PRE-CERT MASTER FILE
     C*
     C     PCKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C* KEY LIST FOR WCMC CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C* KEY LIST FOR EOMB PHYS/HCFA BILL DETAIL FILE
     C*
     C     DHKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*
     C* Key list for State FS Rules/Pgm Cross Reference File
     C*
     C     RUKY1         KLIST
     C                   KFLD                    KKST
     C                   KFLD                    KKATYP
     C                   KFLD                    KKEFDT
     C*
     C     RUKY2         KLIST
     C                   KFLD                    KKST
     C                   KFLD                    KKATYP
     C*
     C* Key List for Provider Bills Control File (FMBEP)
     C*
     C     BEKY          KLIST
     C                   KFLD                    EOAST
     C                   KFLD                    EOSYS
     C                   KFLD                    EOBTYP
     C                   KFLD                    EOSTYP
     C*
     C* key list for geo zip file
     C*
     C     GZKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFRZP
     C                   KFLD                    HLFDAT
     C*
     C     GZKY2         KLIST
     C                   KFLD                    ##CO#
     C*
     C* key list for provider auxiliary file
     C*
     C     P1KY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C*  FS BASE CONTROL FILE
     C*
     C* key list for reasonable and customary file
     C*
     C     RCKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKZIPA
     C                   KFLD                    KKCPT4
     C                   KFLD                    KKEFDT
     C*
     C     RCKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKZIPA
     C                   KFLD                    KKCPT4
     C*
     C*
     C* KEY LIST FOR PROVIDER BILL ERROR MESSAGE FILE
     C*
     C     MGKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*
     C*          Provider Master File
     C*          ------------------------------------------------------
     C*
     C*          Provider Network Contract File
     C*          ------------------------------------------------------
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
     C** Key List for State Code Table File
     C*
     C     STKY          KLIST
     C                   KFLD                    @@KEY
     C*
     C*  KEY LIST FOR VALID STATE CODE FILE
     C*
     C     VCKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKPRCD
     C                   KFLD                    KKFDAT
     C*
     C     VCKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKPRCD
     C     ZRKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKEFDT
     C     MCKY1         KLIST
     C                   KFLD                    KKPRCD
     C                   KFLD                    KKEFDT
     C     MCKY2         KLIST
     C                   KFLD                    KKPRCD
     C     CCKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY
     C*
     C     AGKY          KLIST
     C                   KFLD                    KKGRP
     C                   KFLD                    KKRGN
     C                   KFLD                    KKEFDT
     C*
     C     AGKY1         KLIST
     C                   KFLD                    KKGRP
     C                   KFLD                    KKRGN
     C*
     C     ASKY          KLIST
     C                   KFLD                    ##HCPC            5
     C                   KFLD                    ##EFDT            7 0
     C*
     C     ASKY1         KLIST
     C                   KFLD                    ##HCPC
     C     RVKY1         KLIST
     C                   KFLD                    ##HCPC
     C                   KFLD                    ##MOD1            2
     C                   KFLD                    ##EFDT
     C*
     C     RVKY2         KLIST
     C                   KFLD                    ##HCPC
     C                   KFLD                    ##MOD1
     C*
     C     RVKY3         KLIST
     C                   KFLD                    ##HCPC
     C                   KFLD                    ##MOD1            2
     C                   KFLD                    ##EFDT
     C*** Beg Del *** 01/31/19 ****************************************
     C***************    KFLD                    ##MREL
     C*** End Del *** 01/31/19 ****************************************
     C*
     C     RTKY1         KLIST
     C                   KFLD                    ###ST
     C                   KFLD                    ###TYP
     C                   KFLD                    ##EFDT
     C*
     C     RTKY2         KLIST
     C                   KFLD                    ###ST
     C                   KFLD                    ###TYP
     C     DMKY1         KLIST
     C                   KFLD                    ##HCPC
     C                   KFLD                    XXMOD1            2
     C                   KFLD                    XXMOD2            2
     C                   KFLD                    ##EFDT
     C*
     C     DMKY2         KLIST
     C                   KFLD                    ##HCPC
     C                   KFLD                    XXMOD1            2
     C                   KFLD                    XXMOD2            2
     C*
     C     AMKY1         KLIST
     C                   KFLD                    KKST
     C                   KFLD                    KKLOC
     C                   KFLD                    ##HCPC
     C                   KFLD                    ##EFDT
     C*
     C     AMKY2         KLIST
     C                   KFLD                    KKST
     C                   KFLD                    KKLOC
     C                   KFLD                    ##HCPC
     C*
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
      *** Beg Add *** 02/04/14 ****************************************
     C*   KEY LIST FOR DIAGNOSIS CODE MASTER FILE
     C     fmicd10l1key  klist
     C                   kfld                    kkicd10
      *** End Add *** 02/04/14 ****************************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    DHTOS         HLTOS
     C     *LIKE         DEFINE    DHPRCD        HLCPT4
     C     *LIKE         DEFINE    DHMOD1        HLMOD
     C     *LIKE         DEFINE    CALIN#        XXLIN#
     C     *LIKE         DEFINE    CASUB#        KKSUB#
     C     *LIKE         DEFINE    CAFND         KKFND
     C     *LIKE         DEFINE    CAFYR         KKFYR
     C     *LIKE         DEFINE    CACASE        KKCASE
     C     *LIKE         DEFINE    CAFEIN        KKFEIN
     C     *LIKE         DEFINE    CAMOD         KKMOD
     C     *LIKE         DEFINE    CAEOB#        KKEOB#
     C     *LIKE         DEFINE    CADSEQ        KKDSEQ
     C     *LIKE         DEFINE    CSKEY         KKKEY
     C     *LIKE         DEFINE    EOACDT        HLACDT
     C     *LIKE         DEFINE    CAFDAT        HLFDAT
     C     *LIKE         DEFINE    PSPOS         KKPOS
     C     *LIKE         DEFINE    PRCPT4        KKPRCD
     C     *LIKE         DEFINE    PREFDT        KKFDAT
     C     *LIKE         DEFINE    BSTYPE        KKTYPE
     C     *LIKE         DEFINE    BSCODE        KKCODE
     C     *LIKE         DEFINE    DIICD9        KKICD9
     C     *LIKE         DEFINE    DILOST        KKLOST
     C     *LIKE         DEFINE    ZRFZCD        KKFZCD
     C     *LIKE         DEFINE    CADCS#        XXDCS#
     C     *LIKE         DEFINE    CADCS#        KKDCS#
     C     *LIKE         DEFINE    EOFRDT        HKFDAT
     C     *LIKE         DEFINE    EOCHRG        TLCHRG
     C     *LIKE         DEFINE    CALIN#        KKLIN#
     C     *LIKE         DEFINE    CAOT1C        HLOT1C
     C     *LIKE         DEFINE    CAOT2C        HLOT2C
     C     *LIKE         DEFINE    CARCN         KKRCN
     C     *LIKE         DEFINE    CACHRG        SMCHG
     C     *LIKE         DEFINE    CAMRA         SMMRA
     C     *LIKE         DEFINE    CANDIS        SMNDIS
     C     *LIKE         DEFINE    CAOTHR        SMOTHR
     C     *LIKE         DEFINE    CAOTH2        SMOTH2
     C     *LIKE         DEFINE    CAPAID        SMPAID
     C     *LIKE         DEFINE    CACHRG        DFCHRG
     C     *LIKE         DEFINE    CAMRA         DFMRA
     C     *LIKE         DEFINE    CANDIS        DFNDIS
     C     *LIKE         DEFINE    CAOTHR        DFOTHR
     C     *LIKE         DEFINE    CAOTH2        DFOTH2
     C     *LIKE         DEFINE    CAPAID        DFPAID
     C     *LIKE         DEFINE    CALIN#        ZZLIN#
     C     *LIKE         DEFINE    CADSEQ        ZZDSEQ
     C     *LIKE         DEFINE    DHUNTS        XXUNTS
     C     *LIKE         DEFINE    RUST          KKST
     C     *LIKE         DEFINE    RUATYP        KKATYP
     C     *LIKE         DEFINE    RUEFDT        KKEFDT
     C***** Beg Add *** 10/15/19 ***********************************************
     C     *LIKE         DEFINE    RUEFDT        RUAneEfDt
     C***** End Add *** 10/15/19 ***********************************************
     C     *LIKE         DEFINE    EOCRDT        HLSYDT
     C     *LIKE         DEFINE    CAPAID        XXTOTL
     C     *LIKE         DEFINE    RCZIPA        KKZIPA
     C     *LIKE         DEFINE    RCCPT4        KKCPT4
     C*** Beg Chg *** 07/18/23 **************************************
     C***  *LIKE         DEFINE    DEEOB         XXEOB
     C     *LIKE         DEFINE    DEEOBCD       XXEOB
     C*** End Chg *** 07/18/23 **************************************
     C     *LIKE         DEFINE    CALIN#        ##LIN#
     C     *LIKE         DEFINE    CACHRG        ##CHRG
     C     *LIKE         DEFINE    CAPAID        ##PAID
     C     *LIKE         DEFINE    STKEY         @@KEY
     C     *LIKE         DEFINE    ESBLST        KKBLST
     C     *LIKE         DEFINE    EOCO#         KKCO#
     C     *LIKE         DEFINE    AGRGN         KKRGN
     C     *LIKE         DEFINE    AGGRP         KKGRP
     C     *LIKE         DEFINE    EOCRDT        HLCRDT
     C     *LIKE         DEFINE    EOCRDT        HLCSDT
     C     *LIKE         DEFINE    EOCRDT        HLRRDT
     C     *LIKE         DEFINE    EOCRDT        HLRSDT
     C     *LIKE         DEFINE    PR1A2         SV1A2
     C     *LIKE         DEFINE    CACHRG        SVCHRG
     C     *LIKE         DEFINE    CALIN#        SVLIN#
     C     *LIKE         DEFINE    CADSEQ        SVDSEQ
     C     *LIKE         DEFINE    CADSEQ        SVRCN
     C     *LIKE         DEFINE    DENOT1        XXNOT1
     C     *LIKE         DEFINE    DENOT2        XXNOT2
     C**** Beg Add *** 07/18/23 **************************************
     C     *LIKE         DEFINE    DENOT3        XXNOT3
     C     *LIKE         DEFINE    DENOT4        XXNOT4
     C**** Beg Add *** 07/18/23 **************************************
     C     *LIKE         DEFINE    CADSEQ        HDDSEQ
     C     *LIKE         DEFINE    GZFRZP        KKFRZP
     C     *LIKE         DEFINE    GZEFDT        HLEFDT
     C     *LIKE         DEFINE    ZREFDT        Z1EFDT
     C     *LIKE         DEFINE    CASRC         XXSRC
     C     *LIKE         DEFINE    ZRRGN#        KKLOC
     C     *LIKE         DEFINE    STMFS         XXMFS
     C     *LIKE         DEFINE    RTMREL        ##MREL
     C     *LIKE         DEFINE    RTEFDT        SVEFDT
     C     *LIKE         DEFINE    RTST          ###ST
     C     *LIKE         DEFINE    RTTYPE        ###TYP
     C     *LIKE         DEFINE    PR1A2         MEDUNT
    C***** Beg Add *** 11/02/15 **************************************
     C     *LIKE         DEFINE    DHNDC         KKNDC
    C***** End Add *** 11/02/15 **************************************
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DATSBR - Date check subrountine
     C*================================================================
     C     DATSBR        BEGSR
     C*
     C* THIS SUBROUTINE IS CALLED AFTER EVERY SEARCH TO PROPERLY
     C* FORMAT ALL DATES ON SCREEN
     C*
     C                   Z-ADD     SCFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFDAT
     C*
     C                   ENDSR
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C*
     C* DELETE THE FDCAP RECORD
     C*
     C     @@RRN3        CHAIN     PCA02                              99
     C     *IN99         IFEQ      *OFF
     C                   DELETE    PCA02
     C                   ENDIF
     C*
     C*
     C* DELETE EOB CODES/NOTES FOR THE LINE ITEM
     C*
     C     DEKY          CHAIN     PDE01                              99
     C     *IN99         DOWEQ     *OFF
     C                   DELETE    PDE01
     C     DEKY          READE     PDE01                                  99
     C                   ENDDO                                                  *IN99 = *OFF
     C*
     C*
     C* DELETE THE DETAIL TRANSACTION RECORDS FOR LINE ITEM
     C*
     C     DAKY          CHAIN     PDA01                              99
     C     *IN99         DOWEQ     *OFF
     C                   DELETE    PDA01
     C     DAKY          READE     PDA01                                  99
     C                   ENDDO
     C*
     C* DELETE ERROR MESSAGES FOR THE LINE ITEM
     C*
     C     MGKY          CHAIN     PMG01                              99
     C     *IN99         DOWEQ     *OFF
     C                   DELETE    PMG01
     C     MGKY          READE     PMG01                                  99
     C                   ENDDO                                                  *IN99 = *OFF
     C                   EXSR      UFAEOP
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
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
     C                   EXFMT     SCR03
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C* DATE CARR RCVD CONNOT BE BLANK
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
     C     EHEDI         IFEQ      'Y'
     C     EHEDI         OREQ      'E'
     C                   SETON                                        9023
     C                   GOTO      CARTAG
     C                   ENDIF
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
     C     EOAST         IFEQ      'KY'
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
     C     EHEDI         IFEQ      'Y'
     C     EHEDI         OREQ      'E'
     C                   SETON                                        9012
     C                   GOTO      CARTAG
     C                   ENDIF
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
     C                   Z-ADD     SCCSDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EOCSDT
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
     C                   EXFMT     SCR03
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
     C     EHEDI         IFEQ      'Y'
     C     EHEDI         OREQ      'E'
     C                   SETON                                        9023
     C                   GOTO      CARTAG
     C                   ENDIF
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
     C     EOAST         IFEQ      'KY'
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
     C     EHEDI         IFEQ      'Y'
     C     EHEDI         OREQ      'E'
     C                   SETON                                        9012
     C                   GOTO      CARTAG
     C                   ENDIF
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
     C     CAREND        TAG
     C     ##CLT#        IFEQ      '025'
     C                   SETON                                        10
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C*  EDIERR - EDI ERROR SR FOR FUTURE DATES
     C*================================================================
     C     EDIERR        BEGSR
     C*
     C     EDITAG        TAG
     C*
     C                   MOVEL     EHEDI         SCEDI
     C                   EXFMT     SCR02
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C     SCEDI         IFEQ      *BLANKS
     C                   SETON                                        9017
     C                   GOTO      EDITAG
     C                   ENDIF                                                  SCEDI IFEQ BLANKS
     C*
     C     SCEDI         IFNE      *BLANKS
     C     SCEDI         ANDNE     'Y'
     C     SCEDI         ANDNE     'N'
     C                   SETON                                        9018
     C                   GOTO      EDITAG
     C                   ENDIF                                                  SCEDI IFNE BLANKS
     C*
     C     *IN90         IFEQ      *OFF
     C     EHKY          CHAIN     PEH01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     SCEDI         EHEDI
     C                   UPDATE    PEH01
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  *IN90=*OFF
     C                   ENDIF                                                  *INKL=*OFF
     C*
     C     EDIEND        ENDSR
     C*================================================================
     C*  EDICPT - EDI ERROR SR FOR USER DEFINED CPT CODES
     C*================================================================
     C     EDICPT        BEGSR
     C*
     C     CPTTAG        TAG
     C*
     C                   MOVEL     EHEDI         SCEDI
     C                   EXFMT     SCR04
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C     SCEDI         IFEQ      *BLANKS
     C                   SETON                                        9017
     C                   GOTO      CPTTAG
     C                   ENDIF                                                  SCEDI IFEQ BLANKS
     C*
     C     SCEDI         IFNE      *BLANKS
     C     SCEDI         ANDNE     'Y'
     C     SCEDI         ANDNE     'N'
     C                   SETON                                        9018
     C                   GOTO      CPTTAG
     C                   ENDIF                                                  SCEDI IFNE BLANKS
     C*
     C     *IN90         IFEQ      *OFF
     C     EHKY          CHAIN     PEH01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     SCEDI         EHEDI
     C                   UPDATE    PEH01
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  *IN90=*OFF
     C                   ENDIF                                                  *INKL=*OFF
     C*
     C     CPTEND        ENDSR
     C*================================================================
     C     EDITAL        BEGSR
     C*================================================================
     C*          ALABAMA STATE SPECIFIC EDITS
     C*
     C     1             SUBST     SCPRCD:5      XXPRCD            1
     C                   SELECT
     C*
     C* IF THIS IS A SURGERY CODE, BODY PART S1-S5 IS REQUIRED
     C     SCPRCD        WHENGE    '10000'
     C     SCPRCD        ANDLE     '69999'
     C     SCAREA        ANDNE     'S1'
     C     SCAREA        ANDNE     'S2'
     C     SCAREA        ANDNE     'S3'
     C     SCAREA        ANDNE     'S4'
     C     SCAREA        ANDNE     'S5'
     C*
     C                   SETON                                        9067
     C*
     C*** IF THIS IS A ANES CODE, BODY PART A1-A4 REQUIRED
     C     SCPRCD        WHENGE    '00100'
     C     SCPRCD        ANDLE     '01999'
     C     XXPRCD        ANDNE     'T'
     C     SCAREA        ANDNE     'A1'
     C     SCAREA        ANDNE     'A2'
     C     SCAREA        ANDNE     'A3'
     C     SCAREA        ANDNE     'A4'
     C*
     C                   SETON                                        9067
     C*
     C     SCPRCD        WHENEQ    '97752'
     C     SCAREA        ANDNE     '01'
     C     SCAREA        ANDNE     '02'
     C     SCAREA        ANDNE     '03'
     C     SCAREA        ANDNE     '04'
     C     SCAREA        ANDNE     '05'
     C     SCAREA        ANDNE     '06'
     C     SCAREA        ANDNE     '7A'
     C     SCAREA        ANDNE     '7B'
     C     SCAREA        ANDNE     '7C'
     C     SCAREA        ANDNE     '7D'
     C     SCAREA        ANDNE     '7E'
     C     SCAREA        ANDNE     '7F'
     C     SCAREA        ANDNE     '7G'
     C*
     C                   SETON                                        9067
     C*
     C                   ENDSL                                                  SCPRCD GE 10000
     C*
     C                   ENDSR
     C*================================================================
     C*  EDITEB - EDIT FLORIDA EOB CODES
     C*================================================================
     C     EDITEB        BEGSR
     C*
     C                   MOVE      *BLANK        FNDEB1            1
     C                   MOVE      *BLANK        FNDEB2            1
     C*
     C                   SELECT
     C*
     C     SCPAID        WHENEQ    0
     C*
     C                   MOVE      'N'           FNDEB1
     C*
     C     DEKY          CHAIN(N)  PDE01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*** Beg Chg *** 07/18/23 **************************************
     C****               MOVEL(P)  DEEOB         KKKEY
     C                   MOVEL(P)  DEEOBCD       KKKEY
     C*** End Chg *** 07/18/23 **************************************
     C     TBKY          CHAIN     PEB01                              99
     C     *IN99         IFEQ      *OFF
     C     DESTAT        ANDEQ     'A'
     C     DEESRC        ANDNE     'A'
     C*
     C     EBXREF        LOOKUP    EOBR1                                  98
     C*
     C     *IN98         IFEQ      *ON
     C                   MOVE      'Y'           FNDEB1
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     DEKY          READE(N)  PDE01                                  99
     C*
     C                   ENDDO
     C*
     C     SCPAID        WHENNE    0
     C*
     C                   MOVE      'N'           FNDEB2
     C*
     C     DEKY          CHAIN(N)  PDE01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*** Beg Chg *** 07/18/23 **************************************
     C*****              MOVEL(P)  DEEOB         KKKEY
     C                   MOVEL(P)  DEEOBCD       KKKEY
     C*** End Chg *** 07/18/23 **************************************
     C     TBKY          CHAIN     PEB01                              99
     C     *IN99         IFEQ      *OFF
     C     DESTAT        ANDEQ     'A'
     C*
     C     EBXREF        LOOKUP    EOBR2                                  98
     C*
     C     *IN98         IFEQ      *ON
     C                   MOVE      'Y'           FNDEB2
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     DEKY          READE(N)  PDE01                                  99
     C*
     C                   ENDDO
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C     EDITFL        BEGSR
     C*================================================================
     C*          FLORIDA STATE SPECIFIC EDITS
     C*
     C* BODY AREA IS REQUIRED IF PHYS THERAPY CODE AND DATE < 9/1/97.
     C*
     C                   SELECT
     C*
     C     SCPRCD        WHENGE    '97010'
     C     SCPRCD        ANDLE     '97752'
     C     HLFDAT        ANDLT     0970901
     C     SCAREA        ANDNE     '2 '
     C     SCAREA        ANDNE     '3 '
     C     SCAREA        ANDNE     '4 '
     C     SCAREA        ANDNE     '5 '
     C     SCAREA        ANDNE     '6 '
     C*
     C                   SETON                                        9067
     C*
     C*** If this is an Anesthesia Code with mod -47, Body Part A1-A6 required
     C     SCMOD1        WHENEQ    '47'
     C     SCMOD2        OREQ      '47'
     C     SCMOD3        OREQ      '47'
     C     SCMOD4        OREQ      '47'
     C*
     C     SCAREA        IFNE      'A1'
     C     SCAREA        ANDNE     'A2'
     C     SCAREA        ANDNE     'A3'
     C     SCAREA        ANDNE     'A4'
     C     SCAREA        ANDNE     'A5'
     C     SCAREA        ANDNE     'A6'
     C*
     C                   SETON                                        9067
     C*
     C                   ENDIF
     C*
     C                   ENDSL                                                  SCPRCD GE 97010
     C*
     C                   ENDSR
     C*================================================================
     C     EDITLA        BEGSR
     C*================================================================
     C*          LOUISIANNA STATE SPECIFIC EDITS
     C*
     C                   SELECT
     C*
     C* IF THIS IS A THERMOGRAPHY CODE, BODY PART T1-T7 REQUIRED
     C     SCPRCD        WHENGE    '93760'
     C     SCPRCD        ANDLE     '93762'
     C     SCAREA        ANDNE     'T1'
     C     SCAREA        ANDNE     'T2'
     C     SCAREA        ANDNE     'T3'
     C     SCAREA        ANDNE     'T4'
     C     SCAREA        ANDNE     'T5'
     C     SCAREA        ANDNE     'T6'
     C     SCAREA        ANDNE     'T7'
     C*
     C                   SETON                                        9067
     C*
     C*** IF THIS IS A PHYS THERAPY CODE, BODY PART P1-P6 REQUIRED
     C     SCPRCD        WHENGE    'OT010'
     C     SCPRCD        ANDLE     'OT999'
     C     SCAREA        ANDNE     'P1'
     C     SCAREA        ANDNE     'P2'
     C     SCAREA        ANDNE     'P3'
     C     SCAREA        ANDNE     'P4'
     C     SCAREA        ANDNE     'P5'
     C     SCAREA        ANDNE     'P6'
     C*
     C                   SETON                                        9067
     C*
     C     SCPRCD        WHENGE    'PT010'
     C     SCPRCD        ANDLE     'PT999'
     C     SCAREA        ANDNE     'P1'
     C     SCAREA        ANDNE     'P2'
     C     SCAREA        ANDNE     'P3'
     C     SCAREA        ANDNE     'P4'
     C     SCAREA        ANDNE     'P5'
     C     SCAREA        ANDNE     'P6'
     C*
     C                   SETON                                        9067
     C*
     C     SCPRCD        WHENGE    '97010'
     C     SCPRCD        ANDLE     '97799'
     C     SCAREA        ANDNE     'P1'
     C     SCAREA        ANDNE     'P2'
     C     SCAREA        ANDNE     'P3'
     C     SCAREA        ANDNE     'P4'
     C     SCAREA        ANDNE     'P5'
     C     SCAREA        ANDNE     'P6'
     C*
     C                   SETON                                        9067
     C*
     C*** IF THIS IS A SURGERY CODE, BODY PART S1-S5 IS REQUIRED
     C     SCPRCD        WHENGE    '10000'
     C     SCPRCD        ANDLE     '69999'
     C     SCAREA        ANDNE     'S1'
     C     SCAREA        ANDNE     'S2'
     C     SCAREA        ANDNE     'S3'
     C     SCAREA        ANDNE     'S4'
     C     SCAREA        ANDNE     'S5'
     C     SCAREA        ANDNE     'A '
     C     SCAREA        ANDNE     'B1'
     C     SCAREA        ANDNE     'B2'
     C     SCAREA        ANDNE     'O '
     C*
     C                   SETON                                        9067
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C     EDITNE        BEGSR
     C*================================================================
     C*          NEBRASKA STATE SPECIFIC EDITS
     C*
     C                   SELECT
     C*
     C* IF THIS IS A SURGERY CODE, BODY PART S1-S5 REQUIRED
     C     SCPRCD        WHENGE    '10000'
     C     SCPRCD        ANDLE     '69999'
     C     SCAREA        ANDNE     'S1'
     C     SCAREA        ANDNE     'S2'
     C     SCAREA        ANDNE     'S3'
     C     SCAREA        ANDNE     'S4'
     C     SCAREA        ANDNE     'S5'
     C*
     C                   SETON                                        9067
     C*
     C                   ENDSL                                                  SCPRCD GE 10000
     C*
     C                   ENDSR
     C*================================================================
     C     EDITMS        BEGSR
     C*================================================================
     C*          MISSISSIPPI STATE SPECIFIC EDITS
     C*
     C                   SELECT
     C*
     C* IF THIS IS A SURGERY CODE, BODY PART A, O, B1, OR B2 REQUIRED
     C     SCPRCD        WHENGE    '10000'
     C     SCPRCD        ANDLE     '69999'
     C     SCAREA        ANDNE     'A '
     C     SCAREA        ANDNE     'B1'
     C     SCAREA        ANDNE     'B2'
     C     SCAREA        ANDNE     'O '
     C*
     C                   SETON                                        9067
     C*
     C*** IF THIS IS A PHYS THERAPY CODE, BODY PART P1-P6 REQUIRED
     C     SCPRCD        WHENGE    '97010'
     C     SCPRCD        ANDLE     '97799'
     C     SCPRCD        ORGE      '95900'
     C     SCPRCD        ANDLE     '95904'
     C     SCPRCD        OREQ      '95935'
     C*
     C     SCAREA        IFNE      'P1'
     C     SCAREA        ANDNE     'P2'
     C     SCAREA        ANDNE     'P3'
     C     SCAREA        ANDNE     'P4'
     C     SCAREA        ANDNE     'P5'
     C     SCAREA        ANDNE     'P6'
     C*
     C                   SETON                                        9067
     C*
     C                   ENDIF                                                  SCAREA NE P1
     C*
     C                   ENDSL                                                  SCPRCD GE 10000
     C*
     C                   ENDSR
     C*================================================================
     C     EDITNC        BEGSR
     C*================================================================
     C*          NORTH CAROLINA STATE SPECIFIC EDITS
     C*
     C                   SELECT
     C*
     C* IF THIS IS A SURGERY CODE, BODY PARTS S1-S5 ARE REQUIRED
     C     SCPRCD        WHENGE    '10000'
     C     SCPRCD        ANDLE     '69999'
     C     SCAREA        ANDNE     'S1'
     C     SCAREA        ANDNE     'S2'
     C     SCAREA        ANDNE     'S3'
     C     SCAREA        ANDNE     'S4'
     C     SCAREA        ANDNE     'S5'
     C*
     C                   SETON                                        9067
     C*
     C                   ENDSL                                                  SCPRCD GE 10000
     C*
     C                   ENDSR
     C*================================================================
     C     EDITNM        BEGSR
     C*================================================================
     C*          NEW MEXICO STATE SPECIFIC EDITS
     C*
     C                   SELECT
     C*
     C* IF THIS IS A SURGERY CODE, BODY PART M, S OR B REQUIRED
     C     SCPRCD        WHENGE    '10000'
     C     SCPRCD        ANDLE     '69999'
     C     SCAREA        ANDNE     'M '
     C     SCAREA        ANDNE     'B '
     C     SCAREA        ANDNE     'S '
     C*
     C                   SETON                                        9067
     C*
     C                   ENDSL                                                  SCPRCD GE 10000
     C*
     C                   ENDSR
     C*================================================================
     C     EDITNY        BEGSR
     C*================================================================
     C*          NEW YORK STATE SPECIFIC EDITS
     C*
     C                   SELECT
     C*
     C* IF THIS IS A RADIOLOGY CODE, BODY PART C OR R REQUIRED
     C     SCPRCD        WHENGE    '70010'
     C     SCPRCD        ANDLE     '79999'
     C     SCAREA        ANDNE     'C '
     C     SCAREA        ANDNE     'R '
     C*
     C                   SETON                                        9067
     C*
     C                   ENDSL                                                  SCPRCD GE 10000
     C*
     C                   ENDSR
     C*================================================================
     C     EDITOH        BEGSR
     C*================================================================
     C*          OHIO STATE SPECIFIC EDITS
     C*
     C                   SELECT
     C*
     C* IF THIS IS CODE 97010C-97999, BODY PART 1-6, 7A-7F IS REQUIRE  D
     C     SCPRCD        WHENGE    '97010'
     C     SCPRCD        ANDLE     '97999'
     C     SCAREA        ANDNE     '01'
     C     SCAREA        ANDNE     '02'
     C     SCAREA        ANDNE     '03'
     C     SCAREA        ANDNE     '04'
     C     SCAREA        ANDNE     '05'
     C     SCAREA        ANDNE     '06'
     C     SCAREA        ANDNE     '07'
     C     SCAREA        ANDNE     '08'
     C     SCAREA        ANDNE     '09'
     C     SCAREA        ANDNE     '10'
     C*
     C                   SETON                                        9067
     C*
     C                   ENDSL                                                  SCPRCD GE 10000
     C*
     C                   ENDSR
     C*================================================================
     C     EDITPA        BEGSR
     C*================================================================
     C*          PENNSYLVANIA STATE SPECIFIC EDITS
     C*
     C* VALIDATE BILLED PROCEDURE CODES
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C     SCPRCD        IFGE      'A0001'
     C     SCPRCD        ANDLE     'C9999'
     C     SCPRCD        ORGE      'E0001'
     C     SCPRCD        ANDLE     'Z9999'
     C*
     C     SCMOD1        IFEQ      *BLANKS
     C     SCMOD2        ANDEQ     *BLANKS
     C     SCMOD3        ANDEQ     *BLANKS
     C     SCMOD4        ANDEQ     *BLANKS
     C*
     C                   SETON                                        9004
     C*
     C                   ENDIF                                                  SCMOD1 IFE BLAN
     C*
     C                   ENDIF                                                  SCCPT4 IFGE
     C*
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C                   ENDSR
     C*================================================================
     C     EDITTX        BEGSR
     C*================================================================
     C*          TEXAS STATE SPECIFIC EDITS
     C*
     C**N99***** XXRCFL    IFEQ 'Y'
     C*
     C* VALIDATE BILLED PROCEDURE CODES
     C*
     C     *IN90         IFEQ      *OFF
     C     HLFDAT        ANDLT     1030801
     C*
     C                   MOVEL(P)  SCBLCD        KKPRCD
     C*
     C     KKPRCD        IFEQ      *BLANKS
     C                   MOVEL(P)  SCPRCD        KKPRCD
     C                   ENDIF
     C*
     C                   Z-ADD     SCFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKFDAT
     C*
     C     VCKY1         SETGT     PVC01
     C     VCKY2         READPE    PVC01                                  99
     C*
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9002
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C* BODY AREA IS REQUIRED IF SURGERY CODE
     C*
     C                   SELECT
     C*
     C     SCPRCD        WHENGE    '10040'
     C     SCPRCD        ANDLE     '69979'
     C     SCAREA        ANDNE     'O '
     C     SCAREA        ANDNE     'O1'
     C     SCAREA        ANDNE     'O2'
     C     HLFDAT        ANDLT     1030801
     C*
     C                   SETON                                        9067
     C*
     C     SCPRCD        WHENEQ    '97752'
     C     SCAREA        ANDNE     '01'
     C     SCAREA        ANDNE     '02'
     C     SCAREA        ANDNE     '03'
     C     SCAREA        ANDNE     '04'
     C     SCAREA        ANDNE     '05'
     C     SCAREA        ANDNE     '06'
     C     SCAREA        ANDNE     '7A'
     C     SCAREA        ANDNE     '7B'
     C     SCAREA        ANDNE     '7C'
     C     SCAREA        ANDNE     '7D'
     C     SCAREA        ANDNE     '7E'
     C     SCAREA        ANDNE     '7F'
     C     SCAREA        ANDNE     '7G'
     C*
     C                   SETON                                        9067
     C*
     C                   ENDSL                                                  SCPRCD GE 10040
     C                   ENDIF
     C*
     C     HLFDAT        IFGE      1040901
     C     SCPRCD        ANDGE     '10000'
     C     SCPRCD        ANDLE     '69999'
     C     SCMOD1        IFEQ      'SG'
     C     SCMOD2        OREQ      'SG'
     C     SCMOD3        OREQ      'SG'
     C     SCMOD4        OREQ      'SG'
     C     SCPOS         OREQ      24
     C     SCTOS         ANDEQ     'F  '
     C     SCCNTY        IFEQ      *BLANKS
     C                   SETON                                        9012
     C                   ENDIF
     C                   MOVEL     'TX'          XXCNTY            5
     C                   MOVE      SCCNTY        XXCNTY
     C                   MOVEL(P)  XXCNTY        KKKEY
     C     CCKY          CHAIN     PCC01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9012
     C                   ELSE
     C     P1KY          CHAIN     PP101                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  SCCNTY        YYCNTY
     C                   UPDATE    PP101
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C     EDITWV        BEGSR
     C*================================================================
     C*          WEST VIRGINIA STATE SPECIFIC EDITS
     C*
     C     SCFDAT        IFGE      1010501
     C*
     C     SCPRCD        IFEQ      'V5030'
     C     SCPRCD        OREQ      'V5040'
     C     SCPRCD        OREQ      'V5050'
     C     SCPRCD        OREQ      'V5055'
     C     SCPRCD        OREQ      'V5060'
     C     SCPRCD        OREQ      'V5065'
     C     SCPRCD        OREQ      'V5080'
     C     SCPRCD        OREQ      'V5170'
     C     SCPRCD        OREQ      'V5260'
     C     SCPRCD        OREQ      'V5265'
     C     SCPRCD        OREQ      'V5270'
     C     SCPRCD        OREQ      'V5336'
     C*
     C     SCMOD1        IFNE      'RT'
     C     SCMOD1        ANDNE     'LT'
     C*
     C     SCMOD2        ANDNE     'RT'
     C     SCMOD2        ANDNE     'LT'
     C*
     C     SCMOD3        ANDNE     'RT'
     C     SCMOD3        ANDNE     'LT'
     C*
     C     SCMOD4        ANDNE     'RT'
     C     SCMOD4        ANDNE     'LT'
     C*
     C                   SETON                                        9021
     C*
     C                   ENDIF                                                  SCMOD1 IFNE
     C*
     C                   ENDIF                                                  SCPRCD IFEQ
     C*
     C                   ENDIF                                                  SCFDAT IFGE
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLSYDT
     C*
     C                   MOVE      *BLANK        MEDUNT
     C*
     C* Validate Claim Status (Adjudication) code
     C*
     C     SCCSCD        IFEQ      *BLANKS
     C     *IN46         ANDEQ     *OFF
     C                   SETON                                        9071
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCCSCD IFEQ BLANKS
     C*
     C                   MOVEL(P)  SCCSCD        KKKEY
     C     TBKY          CHAIN     PCS01                              99
     C     *IN99         IFEQ      *ON
     C     *IN46         ANDEQ     *OFF
     C                   SETON                                        9072
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C     EOBTYP        IFEQ      'V'
     C     CSOVRD        ANDNE     'Y'
     C     *IN46         ANDEQ     *OFF
     C*
     C                   SETON                                        9096
     C                   GOTO      CK1END
     C*
     C                   ENDIF                                                  EOBTYP=V    ON
     C*
     C* Validate Date of Service
     C*
     C     SCFDAT        IFEQ      *ZEROS
     C     *IN46         ANDEQ     *OFF
     C                   SETON                                        9073
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCFDAT IFEQ ZEROS
     C*
     C                   Z-ADD     SCFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C     *IN46         ANDEQ     *OFF
     C                   SETON                                        9074
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     SRDTO         SCFDAT
     C                   ENDIF
     C*
     C* DATE OF SERVICE MUST BE LESS THAN ACCIDENT DATE
     C*
     C                   Z-ADD     SCFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLFDAT
     C*
     C     HLFDAT        IFLT      EOACDT
     C     *IN46         ANDEQ     *OFF
     C                   SETON                                        9075
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLFDAT IFLT HLACDT
     C*
     C* BASE THE EDIT OF PREVENTING FUTURE SERVICE DATES ON CONTROL
     C* FLAG SFBPF/FBAL04 POSTION 8 (BLANK = EDIT, Y = ALLOW FUTURE DATES)
     C*
     C     ##SDTS        IFNE      'Y'
     C*
     C     HLFDAT        IFGT      HLSYDT
     C     *IN46         ANDEQ     *OFF
     C                   SETON                                        9028
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLFDAT GT HLSYDT
     C*  Give an error window if the date is a future date and the
     C*  data submission flag is 'Y'.
     C*
     C                   ELSE                                                   XXFSDF='Y'
     C     EHEDI         IFEQ      'Y'
     C     HLFDAT        ANDGT     HLSYDT
     C     *IN46         ANDEQ     *OFF
     C     EHEDI         OREQ      'E'
     C     HLFDAT        ANDGT     HLSYDT
     C     *IN46         ANDEQ     *OFF
     C                   EXSR      EDIERR
     C                   SETON                                        90
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXFSDF = *BLANKS
     C*
     C* Date of Service must be less than Carr Rcvd Dt
     C*     (If recon = 0)  FAEOP
     C*
     C* This is all conditioned on weather or not the EDI flag is on
     C*
     C     EORCN         IFEQ      0
     C     HLFDAT        ANDGT     EOCRDT
     C     *IN46         ANDEQ     *OFF
     C                   EXSR      CARERR
     C     EHEDI         IFEQ      'Y'
     C     EHEDI         OREQ      'E'
     C                   SETON                                        90
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF                                                  EORCN=0
     C*
     C*     (If recon > 0)  FAESP
     C*
     C     EORCN         IFGT      0
     C                   MOVEL     'R'           KKBLST
     C     ESKY1         SETGT     PES01
     C     ESKY2         READPE(N) PES01                                  60
     C     *IN60         IFEQ      *OFF
     C     HLFDAT        IFGT      ESRRDT
     C     *IN46         ANDEQ     *OFF
     C                   EXSR      CARERR
     C     EHEDI         IFEQ      'Y'
     C     EHEDI         OREQ      'E'
     C                   SETON                                        90
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF                                                  HLFDAT ANDGT
     C                   ELSE                                                   *IN60
     C     EORCN         SUB       1             KKRCN
     C     ESKY1         SETGT     PES01
     C     ESKY2         READPE(N) PES01                                  60
     C     *IN60         IFEQ      *OFF
     C     HLFDAT        ANDGT     ESRRDT
     C     *IN46         ANDEQ     *OFF
     C                   EXSR      CARERR
     C     EHEDI         IFEQ      'Y'
     C     EHEDI         OREQ      'E'
     C                   SETON                                        90
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  EORCN>0
     C                   MOVEL     *BLANKS       SCZIPA
     C                   MOVEL     *BLANKS       SCZIPB
     C                   MOVEL     *ZEROS        HLEFDT
     C*
     C* Let's get the zip area forn this line.
     C*
     C                   MOVEL(P)  EOFLIC        KKFRZP
     C     KKFRZP        IFEQ      *BLANKS
     C     KKFRZP        OREQ      '000'
     C     P1KY          CHAIN(N)  PP101                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  P1ZIP         KKFRZP
     C                   ENDIF
     C                   ENDIF
     C*
     C     KKFRZP        IFNE      *BLANKS
     C     KKFRZP        ANDNE     '000'
     C*
     C     GZKY1         SETGT     PGZ01
     C     GZKY2         READPE    PGZ01                                  99
     C     *IN99         DOWEQ     *OFF
     C     GZSTAT        IFNE      'I'
     C     KKFRZP        ANDLE     GZTOZP
     C     HLEFDT        IFEQ      *ZEROS
     C     GZEFDT        ORGT      HLEFDT
     C     GZEFDT        ANDLE     HLFDAT
     C                   MOVEL     GZZIPA        SCZIPA
     C                   MOVEL     GZZIPA        SCZIPB
     C                   Z-ADD     GZEFDT        HLEFDT
     C                   ENDIF
     C                   ENDIF
     C     GZKY2         READPE    PGZ01                                  99
     C                   ENDDO
     C                   ENDIF
     C*
     C* VALIDATE PLACE OF SERVICE
     C*
     C     SCPOS         IFEQ      *ZEROS
     C     *IN46         ANDEQ     *OFF
     C                   SETON                                        9076
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCPOS IFEQ BLANKS
     C*
     C                   MOVE(P)   SCPOS         KKPOS
     C     PSKY          CHAIN     PPS01                              99
     C     *IN99         IFEQ      *ON
     C     *IN46         ANDEQ     *OFF
     C                   SETON                                        9077
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C*
     C     *IN99         IFEQ      *OFF
     C     *IN46         ANDEQ     *OFF
     C     PSSTAT        ANDNE     'A'
     C                   SETON                                        9025
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C*
     C* CHARGES ARE REQUIRED
     C*
     C*
     C* IF ADJUDICATION CODE IS OVERRIDE TYPE, ALLOW ZERO CHARGE AMOUNT
     C*
     C     CSOVRD        IFNE      'Y'
     C     SCCHRG        IFEQ      *ZEROS
     C                   SETON                                        9089
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCCHRG IFEQ ZEROS
     C                   ENDIF                                                  CSOBRD NE Y
     C*
     C* PROCEDURE CODE IS REQUIRED
     C*
     C                   MOVE      ' '           XXRCFL            1
     C*
     C     SCPRCD        IFEQ      *BLANKS
    C***** Beg Chg *** 11/02/15 **************************************
     C******IN46         ANDEQ     *OFF
     C     SCNDC         ANDEQ     *Blanks
    C***** End Chg *** 11/02/15 **************************************
     C                   SETON                                        9083
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCPRCD IFEQ BLANKS
     C*
     C                   MOVEL(P)  SCPRCD        KKPRCD
     C                   Z-ADD     SCFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         KKFDAT
     C*** Beg Add *** 10/18/12 *************************************************
      /Free
       If ##clt# = '114' and sccscd = '2'
        and (eoast  = 'P1' or eoast = 'P2')
        and hlfdat < 1100101;
           Edtcde = 'N';
       Else;
           Edtcde = 'Y';
       Endif;
      /End-Free
     C*** End Add *** 10/18/12 *************************************************
     C*
     C* VERIFY THAT THERE IS A FEE SCHEDULE ON THE SYSTEM FOR THE
     C* SERVICE DATE
     C*
     C                   MOVEL     SCAST         KKST
     C                   MOVE      'P'           KKATYP
     C                   Z-ADD     KKFDAT        KKEFDT
     C*** Beg Add *** 10/18/12 *************************************************
      /Free
       If Edtcde = 'Y';
      /End-Free
     C*** End Add *** 10/18/12 *************************************************
     C***** Beg Add *** 10/15/19 ***********************************************
      /Free
       // First get the fffective date for ASC (to be used later). Then
       // chain to FDRUP and keep using RUEFDT for edits further down
       RUAneEfDt = *loval;
       SetGT (kkst:'A':kkefdt) pru01;
       ReadPE (kkst:'A') pru01;
       If not %eof(fdrul1);
          RUAneEfDt = RUefdt;
       EndIf;
      /End-Free
     C***** End Add *** 10/15/19 ***********************************************
     C     RUKY1         SETGT     PRU01
     C     RUKY2         READPE    PRU01                                  99
     C     *IN99         IFEQ      *ON
     C     *IN46         ANDEQ     *OFF
     C                   SETON                                        9030
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C*
     C* VALIDATE CPT4 CODE
     C*
     C*** Beg Add *** 06/21/12 ****************************************
      /Free
         //If State is 'HL and specialty code <> ASC
         If Eoast = 'HL';
         //*** Beg Add *** 10/07/15 **************************************
         //If Anes code or HCPCS set medunt to Y
           If (DHprCd >= '00100' and DHprCd <= '01999')
              or (DHprCd >= 'A0001' and DHprCd <= 'Z9999');
             MedUnt = 'Y';
           Endif;
         //*** Beg Add *** 10/07/15 **************************************
           If Eospcd <> 'ASC';

         //*** Beg Chg *** 11/05/18 **************************************
         //** Chain scprcd phl03;
         //**    If not %Found(Fmhll1);
              SetGt (scprcd:RUefdt) phl04;
              ReadPe scprcd phl04;
                 If %Eof(Fmhll2) or H4efdt < RUefDt;
         //*** End Chg *** 11/05/18 **************************************
                   *in84 = *on;
                   *in90 = *on;
                 Else;

                   If H3Stat = 'I';
                     *in57 = *on;
                     *in90 = *on;
                   Endif;

         //*** Beg Chg *** 11/05/18 **************************************
         //******  scCdds = %subst(H3desc:1:25);
                   scCdds = %subst(H4desc:1:25);
         //*** End Chg *** 11/05/18 **************************************
                   xxrcfl = 'C';
                 Endif;
         //*** Beg Add *** 07/02/20 **************************************
                 SetGt (scprcd:kkEfDt) phl04;
                 Readpe scprcd phl04;
                 If %Found(fmhll2);
                    If H4enDt < kkEfDt and H4enDt <> *zeros;
                      *in84 = *on;
                      *in90 = *on;
                    Endif;
                 Endif;
         //*** End Add *** 07/02/20 **************************************
         // Billed Code
              If scblcd <> *blanks;
               Chain scblcd phl03;
                  If not %Found(Fmhll1);
                    *in16 = *on;
                    *in90 = *on;
                  Else;

                    If H3Stat = 'I';
                      *in15 = *on;
                      *in90 = *on;
                    Endif;
                  Endif;
              Else;
              scblcd = scprcd;
              Endif;
           Endif;

         //If State is 'HL and specialty code = ASC
           If Eospcd = 'ASC';

      //***** Beg Chg *** 10/15/19 *********************************************
         //*** Beg Chg *** 11/05/18 **************************************
         //***Chain scprcd phl03;
         //***   If not %Found(Fmhll1);
      //***** SetGt (scprcd:RUefdt) paa02;
      //***** ReadPe scprcd paa02;
      //*****    If %Eof(Ftaal2) or a2efdt < RUefDt;
         //*** End Chg *** 11/05/18 **************************************

              SetGt (scprcd:RUAneEfDt) paa02;
              ReadPe scprcd paa02;
                 If %Eof(Ftaal2) or a2efdt < RUAneEfDt;
      //***** End Chg *** 10/15/19 *********************************************
                   *in84 = *on;
                   *in90 = *on;
                 Else;

                   If h3Stat = 'I';
                     *in57 = *on;
                     *in90 = *on;
                   Endif;

         //*** Beg Chg *** 11/05/18 **************************************
         //*****   scCdds = %subst(H3desc:1:25);
                   SetGt (scprcd:RUefdt) phl04;
                   ReadPe scprcd phl04;
                   If Not %Eof(Fmhll2) and H4code = scPrCd;
                      scCdds = %subst(H4desc:1:25);
                   Endif;
         //*** End Chg *** 11/05/18 **************************************
                   xxrcfl = 'C';
                 Endif;
         // Billed Code
              If scblcd <> *blanks;
         //*** Beg Chg *** 11/05/18 **************************************
         //*** Chain scblcd phl03;
         //***    If not %Found(Fmhll1);
               Chain scblcd paa02;
                  If not %Found(Ftaal2);
         //*** End Chg *** 11/05/18 **************************************
                    *in16 = *on;
                    *in90 = *on;
                  Else;

                    If H3Stat = 'I';
                      *in15 = *on;
                      *in90 = *on;
                    Endif;
                  Endif;
              Endif;
           Endif;
         Else;

      /End-Free
     C*** End Add *** 06/21/12 ****************************************
     C     EOSPCD        IFEQ      'ASC'
     C     EOAST         ANDEQ     'TX'
     C                   MOVEL(P)  SCPRCD        ##HCPC
     C                   Z-ADD     KKFDAT        ##EFDT
     C     ASKY          SETGT     PAS01
     C     ASKY1         READPE    PAS01                                  99
     C     *IN99         IFEQ      *ON
     C                   GOTO      CHKTAG
     C                   ELSE
     C                   MOVEL(P)  ASDESC        SCCDDS
     C                   MOVE      'C'           XXRCFL
     C                   ENDIF
     C                   ELSE
     C     EOAST         IFEQ      'KY'
     C*** Beg Add *** 08/14/14 *********************************************
     C                   if        scprcd >= 'A0001' and scprcd <= 'Z9999'
     C                   eval      medunt = 'Y'
     C                   endif
     C*** End Add *** 08/14/14 *********************************************
     C*
     C                   MOVEL(P)  SCPRCD        ##HCPC
     C                   Z-ADD     KKFDAT        ##EFDT
     C     ASKY          SETGT     PAM01
     C     ASKY1         READPE    PAM01                                  99
     C     *IN99         IFEQ      *ON
     C                   GOTO      CHKTAG
     C                   ELSE
     C*
     C****** Beg Add **** 11/02/15 ************************************
     C     SCPRCD        IFNE      *BLANKS
     C     SCNDC         ANDEQ     *BLANKS
     C****** End Add **** 11/02/15 ************************************
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  98
     C****** Beg Add **** 12/02/15 ************************************
     C                   ENDIF
     C****** End Add **** 12/02/15 ************************************
     C                   MOVEL(P)  AMDESC        SCCDDS
     C                   MOVEL     'A'           XXSRC
     C                   MOVE      'C'           XXRCFL
     C                   ENDIF
     C                   ELSE
     C     CHKTAG        TAG
     C*** Check in files FMAMP, FMDFP, FMDEP, FTLBP, FTRVP only if bill is
     C*** medicare PPO processing (EODSTY = 'DR'/'DS')
     C*** Also set CASRC accordingly
     C     EODSTY        IFEQ      'DR'
     C     EODSTY        OREQ      'DS'
     C*** chain on various files
     C                   MOVEL(P)  SCPRCD        ##HCPC
     C     ##MREL        IFEQ      '1'
     C                   MOVE      SVEFDT        ##EFDT
     C                   ELSE
     C                   Z-ADD     KKFDAT        ##EFDT
     C                   ENDIF                                                          ##MREL = '1'
     C*
     C     ASKY          SETGT     PAM01
     C     ASKY1         READPE    PAM01                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C     AMSTAT        IFEQ      'I'
     C     ASKY1         READPE    PAM01                                  99
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C     *IN99         IFEQ      *ON
     C*** CHAIN IN NEXT FILE - FMDFP
     C     ASKY          SETGT     PDF01
     C     ASKY1         READPE    PDF01                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C     DFSTAT        IFEQ      'I'
     C     ASKY1         READPE    PDF01                                  99
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C     *IN99         IFEQ      *ON
     C*** CHAIN IN NEXT FILE - FMDEP
     C     ASKY          SETGT     PDM02
     C     ASKY1         READPE    PDM02                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C     DESTAT        IFEQ      'I'
     C     ASKY1         READPE    PDM02                                  99
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C     *IN99         IFEQ      *ON
     C*** CHAIN IN NEXT FILE - FTLBP
     C     ASKY          SETGT     PLB02
     C     ASKY1         READPE    PLB02                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C     LBSTAT        IFEQ      'I'
     C     ASKY1         READPE    PLB02                                  99
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C     *IN99         IFEQ      *ON
     C*** CHAIN IN NEXT FILE - FTRVP
     C     ASKY          SETGT     PRV03
     C     ASKY1         READPE    PRV03                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C     RVSTAT        IFEQ      'I'
     C     ASKY1         READPE    PRV03                                  99
     C                   ELSE
     C     SCPRCD        IFGE      '00100'
     C     SCPRCD        ANDLE     '01999'
     C                   MOVE      'Y'           MEDUNT
     C                   ENDIF
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9084
     C                   ELSE
     C                   MOVEL     'R'           XXSRC
     C                   MOVEL(P)  RVDESC        SCCDDS
     C                   MOVEL     'Y'           XXRCFL
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ELSE
     C                   MOVEL     'L'           XXSRC
     C                   MOVEL(P)  LBDESC        SCCDDS
     C                   MOVEL     'Y'           XXRCFL
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ELSE
     C                   MOVEL     'D'           XXSRC
     C                   MOVEL(P)  DEDESC        SCCDDS
     C                   MOVEL     'Y'           XXRCFL
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ELSE
     C                   MOVEL     'F'           XXSRC
     C                   MOVEL(P)  DFDESC        SCCDDS
     C                   MOVEL     'Y'           XXRCFL
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ELSE
     C                   MOVEL     'A'           XXSRC
     C                   MOVEL(P)  AMDESC        SCCDDS
     C                   MOVEL     'Y'           XXRCFL
     C                   ENDIF                                                  *IN99 = *OFF
       //*** Beg Add *** 08/06/25 ******************************************
       If STfsTy = 'S';
          Setgt prky ppr01;
          Readpe prky1 ppr01;
          If Not %Eof(FMPRL);
             scCdDs = PRdsc1;
             xxRcFl = 'C';
          Else;
             *in90 = *on;
             *in82 = *on;
             LeaveSr;
          Endif;
       Endif;
       //*** End Add *** 08/06/25 ******************************************
     C*
     C                   ELSE                                                   EODSTY = DR/DS
     C     XXMFS         IFEQ      'Y'
     C     EOAST         ANDNE     'SC'
     C***** Beg Add *** 04/04/16 *******************************************************************
     C     XXMFS         OREQ      'C'
     C     EOAST         ANDNE     'SC'
     C***** End Add *** 04/04/16 *******************************************************************
     C*
     C     ##MREL        IFEQ      '1'
     C                   MOVE      SVEFDT        ##EFDT
     C                   ELSE
     C                   Z-ADD     KKFDAT        ##EFDT
     C                   ENDIF                                                          ##MREL = '1'
     C*
     C                   MOVEL(P)  SCPRCD        ##HCPC
     C*
     C                   SETON                                        99
     C*
     C     EOSPCD        IFEQ      'ASC'
     C*** CHAIN IN NEXT FILE - FMASP
     C     ASKY          SETGT     PAS01
     C     ASKY1         READPE    PAS01                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C     ASSTAT        IFEQ      'I'
     C     ASKY1         READPE    PAS01                                  99
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     'S'           XXSRC
     C                   MOVEL(P)  ASDESC        SCCDDS
     C                   MOVEL     'M'           XXRCFL
     C                   ENDIF
     C*
     C                   ENDIF                                                  EOSPCD = 'ASC'
     C*
     C     *IN99         IFEQ      *ON
     C     EOSPCD        ANDNE     'ASC'
     C*
     C     ASKY          SETGT     PAM01
     C     ASKY1         READPE    PAM01                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C     AMSTAT        IFEQ      'I'
     C     ASKY1         READPE    PAM01                                  99
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     'A'           XXSRC
     C                   MOVEL(P)  AMDESC        SCCDDS
     C                   MOVEL     'M'           XXRCFL
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN99 = *ON C'
     C*
     C     *IN99         IFEQ      *ON
     C     EOSPCD        ANDNE     'ASC'
     C*** Beg Add *** 04/04/16 ***************************************
     C     xxMFS         ANDNE     'C'
     C*** End Add *** 04/04/16 ***************************************
     C*** CHAIN IN NEXT FILE - FMDFP
     C     ASKY          SETGT     PDF01
     C     ASKY1         READPE    PDF01                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C     DFSTAT        IFEQ      'I'
     C     ASKY1         READPE    PDF01                                  99
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     'F'           XXSRC
     C                   MOVEL(P)  DFDESC        SCCDDS
     C                   MOVEL     'M'           XXRCFL
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN99 = *ON C'
     C*
     C     *IN99         IFEQ      *ON
     C     EOSPCD        ANDNE     'ASC'
     C*** CHAIN IN NEXT FILE - FMDEP
     C     ASKY          SETGT     PDM02
     C     ASKY1         READPE    PDM02                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C     DESTAT        IFEQ      'I'
     C     ASKY1         READPE    PDM02                                  99
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     'D'           XXSRC
     C                   MOVEL(P)  DEDESC        SCCDDS
     C                   MOVEL     'M'           XXRCFL
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN99 = *ON C'
     C*
     C     *IN99         IFEQ      *ON
     C     EOSPCD        ANDNE     'ASC'
     C*** Beg Add *** 04/04/16 ***************************************
     C     xxMFS         ANDNE     'C'
     C*** End Add *** 04/04/16 ***************************************
     C*** CHAIN IN NEXT FILE - FTLBP
     C     ASKY          SETGT     PLB02
     C     ASKY1         READPE    PLB02                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C     LBSTAT        IFEQ      'I'
     C     ASKY1         READPE    PLB02                                  99
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     'L'           XXSRC
     C                   MOVEL(P)  LBDESC        SCCDDS
     C                   MOVEL     'M'           XXRCFL
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN99 = *ON C'
     C*
     C     *IN99         IFEQ      *ON
     C     EOSPCD        ANDNE     'ASC'
     C*** Beg Add *** 04/04/16 ***************************************
     C     xxMFS         ANDNE     'C'
     C*** End Add *** 04/04/16 ***************************************
     C*** CHAIN IN NEXT FILE - FTRVP
     C     ASKY          SETGT     PRV03
     C     ASKY1         READPE    PRV03                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C     RVSTAT        IFEQ      'I'
     C     ASKY1         READPE    PRV03                                  99
     C                   ELSE
     C     SCPRCD        IFGE      '00100'
     C     SCPRCD        ANDLE     '01999'
     C                   MOVE      'Y'           MEDUNT
     C                   ENDIF
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     'R'           XXSRC
     C                   MOVEL(P)  RVDESC        SCCDDS
     C                   MOVEL     'M'           XXRCFL
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN99 = *ON C'
     C***** Beg Add *** 04/04/16 *******************************************************************
     C     *IN99         IFEQ      *ON
     C     EOSPCD        ANDNE     'ASC'
     C     xxMFS         ANDEQ     'C'
     C     EOAST         ANDNE     'SC'
     C*** CHAIN IN NEXT FILE - FMPRPOK
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        SCCDDS
     C                   MOVEL     'C'           XXRCFL
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN99 = *ON C'
     C***** End Add *** 04/04/16 *******************************************************************
     C*
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9084
     C                   ENDIF
     C*
     C*** Beg Add *** 07/26/12 *********************************************
     C                   if        scprcd >= 'A0001' and scprcd <= 'Z9999'
     C                   eval      medunt = 'Y'
     C                   endif
     C*** End Add *** 07/26/12 *********************************************
     C*
     C                   ELSE                                                   XXMFS = Y
     C****** Beg Add **** 11/02/15 ************************************
     C     SCPRCD        IFNE      *BLANKS
     C     SCNDC         ANDEQ     *BLANKS
     C****** End Add **** 11/02/15 ************************************
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  99
     C****** Beg Add **** 11/02/15 ************************************
     C                   ELSE
     C                   EVAL      PREFDT = KKFDAT
     C                   ENDIF
     C****** End Add **** 11/02/15 ************************************
     C*
     C     *IN99         IFEQ      *OFF
     C     SCAST         ANDNE     'UC'
     C     *IN46         ANDEQ     *OFF
     C*
     C                   MOVE      PR1A2         SV1A2
     C*
     C* GO TO REASONABLE AND CUTORMARY FILE IF CPT4 EFFECTIVE DATE IS
     C* LESS THAN FEE SCHEDULE EFFECTIVE DATE OR CODE IS INACTIVE
     C*
     C     PREFDT        IFLT      RUEFDT
     C                   SETON                                        99
     C                   ENDIF                                                  PREFDT < RUEFDT
     C*
     C  N99PRSTAT        IFEQ      'I'
     C                   SETON                                        99
     C                   ENDIF                                                  *IN99  = 'I'
     C*
     C  N99PR1AG         IFEQ      'Y'
     C     EHEDI         ANDEQ     'Y'
     C     EHEDI         OREQ      'E'
     C     PR1AG         ANDEQ     'Y'
     C                   EXSR      EDICPT
     C                   SETON                                        90
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99  = 'I'
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        SCCDDS
     C                   MOVE      'C'           XXRCFL
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C     *IN99         IFEQ      *ON
     C     *IN46         ANDEQ     *OFF
     C     SCAST         OREQ      'UC'
     C     *IN46         ANDEQ     *OFF
     C*
     C* If State flag STBS02='N' then don't check R&C file
     C     ##RC          IFEQ      'N'
     C                   SETON                                        9084
     C                   GOTO      CK1END
     C*
     C                   ELSE
     C*
     C                   MOVEL     SCZIPA        KKZIPA
     C                   MOVEL(P)  SCPRCD        KKCPT4
     C                   MOVEL     KKFDAT        KKEFDT
     C****** Beg Add **** 11/02/15 ************************************
     C     SCPRCD        IFNE      *BLANKS
     C     SCNDC         ANDEQ     *BLANKS
     C****** End Add **** 11/02/15 ************************************
     C     RCKY1         SETGT     PRC01
     C     RCKY2         READPE    PRC01                                  99
     C****** Beg Add **** 11/02/15 ************************************
     C                   ENDIF
     C****** End Add **** 11/02/15 ************************************
     C*
     C     *IN99         IFEQ      *ON
     C*
     C* VALIDATE CPT4 CODE AGAINST UC
     C*
     C     SCAST         IFEQ      'UC'
     C****** Beg Add **** 11/02/15 ************************************
     C     SCPRCD        IFNE      *BLANKS
     C     SCNDC         ANDEQ     *BLANKS
     C****** End Add **** 11/02/15 ************************************
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  98
     C****** Beg Add **** 11/02/15 ************************************
     C                   ENDIF
     C****** End Add **** 11/02/15 ************************************
     C*
     C     *IN98         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        SCCDDS
     C                   MOVE      'C'           XXRCFL
     C                   MOVE      PR1A2         SV1A2
     C*
     C                   ELSE
     C                   SETON                                        9084
     C                   GOTO      CK1END
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C                   SETON                                        9084
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C*
     C                   ENDIF                                                  SCAST=UC
     C*
     C     *IN99         IFEQ      *OFF
     C     RCSTAT        ANDEQ     'I'
     C*
     C* VALIDATE CPT4 CODE AGAINST UC
     C*
     C     SCAST         IFEQ      'UC'
     C****** Beg Add **** 11/02/15 ************************************
     C     SCPRCD        IFNE      *BLANKS
     C     SCNDC         ANDEQ     *BLANKS
     C****** End Add **** 11/02/15 ************************************
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  98
     C****** Beg Add **** 11/02/15 ************************************
     C                   ENDIF
     C****** End Add **** 11/02/15 ************************************
     C*
     C     *IN98         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        SCCDDS
     C                   MOVE      'C'           XXRCFL
     C                   MOVE      PR1A2         SV1A2
     C*
     C                   ELSE
     C                   SETON                                        9057
     C                   GOTO      CK1END
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C                   SETON                                        9057
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C*
     C                   ENDIF                                                  SCAST=UC
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  RCDSC1        SCCDDS
     C                   MOVE      'Y'           XXRCFL
     C                   MOVE      RC1A2         SV1A2
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ENDIF                                                  ##RC IFEQ ' '
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  EODSTY = DR/DS
     C                   ENDIF                                                  EOAST  = 'KY'
     C                   ENDIF
     C* VERIFY THAT THE PROCECURE CODE IS EFFECTIVE FOR THE FEE SCHEDULE
     C*
     C     XXRCFL        IFEQ      'C'
     C     PREFDT        IFLT      RUEFDT
     C     *IN46         ANDEQ     *OFF
     C                   SETON                                        9042
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C     SCFDAT        IFGE      07152011
     C     EOAST         IFEQ      'CT'
     C     SCPRCD        ANDGE     'A0001'
     C     SCPRCD        ANDLE     'Z9999'
     C***** Beg Add *** 08/14/15 *************************************
      /Free
           If %Subst(SCPRCD:1:1) <> 'L' And
              %Subst(SCPRCD:1:1) <> 'G' ;
      /End-Free
     C***** End Add *** 08/14/15 *************************************
     C**    -CHAIN IN NEXT FILE - FMDEP-
     C                   MOVEL(P)  SCPRCD        ##HCPC
     C                   Z-ADD     SCFDAT        ##EFDT
     C     ASKY          SETGT     PDM02
     C     ASKY1         READPE    PDM02                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     'D'           XXSRC
     C                   ENDIF
     C*
     C***** Beg Add *** 08/14/15 *************************************
      /Free
           EndIf;
      /End-Free
     C***** End Add *** 08/14/15 *************************************
     C                   ENDIF                                                  CT & HCPCS A0001-Z99
     C                   ENDIF                                                  SCFDAT IFGE 7/15/11
     C*
     C* VALIDATE CPT4 BILLED CODE WITHOUT USING EFFECTIVE DATE
     C*
     C     SCBLCD        IFNE      *BLANKS
     C*** Check in files FMAMP, FMDFP, FMDEP, FTLBP, FTRVP only if bill is
     C*** medicare PPO processing (EODSTY = 'DR'/'DS')
     C*** Also set CASRC accordingly
     C     EODSTY        IFEQ      'DR'
     C     EODSTY        OREQ      'DS'
     C     XXMFS         OREQ      'Y'
     C*** Beg Add *** 04/04/16 ******************************************
     C     XXMFS         OREQ      'C'
     C     CARCFL        ANDNE     'C'
     C     CARCFL        ANDNE     'Y'
     C*** End Add *** 04/04/16 ******************************************
     C*** chain on various files
     C                   MOVEL(P)  SCBLCD        ##HCPC
     C     ##MREL        IFEQ      '1'
     C                   MOVE      SVEFDT        ##EFDT
     C                   ELSE
     C                   Z-ADD     KKFDAT        ##EFDT
     C                   ENDIF                                                          ##MREL = '1'
     C*
     C     ASKY          SETGT     PAM01
     C     ASKY1         READPE    PAM01                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C     AMSTAT        IFEQ      'I'
     C     ASKY1         READPE    PAM01                                  99
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C     *IN99         IFEQ      *ON
     C*** CHAIN IN NEXT FILE - FMDFP
     C     ASKY          SETGT     PDF01
     C     ASKY1         READPE    PDF01                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C     DFSTAT        IFEQ      'I'
     C     ASKY1         READPE    PDF01                                  99
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C     *IN99         IFEQ      *ON
     C*** CHAIN IN NEXT FILE - FMDEP
     C     ASKY          SETGT     PDM02
     C     ASKY1         READPE    PDM02                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C     DESTAT        IFEQ      'I'
     C     ASKY1         READPE    PDM02                                  99
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C     *IN99         IFEQ      *ON
     C*** CHAIN IN NEXT FILE - FTLBP
     C     ASKY          SETGT     PLB02
     C     ASKY1         READPE    PLB02                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C     LBSTAT        IFEQ      'I'
     C     ASKY1         READPE    PLB02                                  99
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C     *IN99         IFEQ      *ON
     C*** CHAIN IN NEXT FILE - FTRVP
     C     ASKY          SETGT     PRV03
     C     ASKY1         READPE    PRV03                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C     RVSTAT        IFEQ      'I'
     C     ASKY1         READPE    PRV03                                  99
     C                   ELSE
     C     SCPRCD        IFGE      '00100'
     C     SCPRCD        ANDLE     '01999'
     C                   MOVE      'Y'           MEDUNT
     C                   ENDIF
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     *IN99         IFEQ      *ON
     C*** CHAIN IN NEXT FILE - FMASP
     C     EOSPCD        IFEQ      'ASC'
     C     ASKY          SETGT     PAS01
     C     ASKY1         READPE    PAS01                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C     ASSTAT        IFEQ      'I'
     C     ASKY1         READPE    PAS01                                  99
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C                   ENDIF                                                  EOSPCD = 'ASC'
     C*
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9016
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE                                                   EODSTY = DR/DS
     C*
     C                   MOVEL(P)  SCBLCD        KKPRCD
     C*
     C****** Beg Add **** 11/02/15 ************************************
     C     SCPRCD        IFNE      *BLANKS
     C     SCNDC         ANDEQ     *BLANKS
     C****** End Add **** 11/02/15 ************************************
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  99
     C****** Beg Add **** 11/02/15 ************************************
     C                   ENDIF
     C****** End Add **** 11/02/15 ************************************
     C*
     C  N99PRSTAT        IFEQ      'I'
     C                   SETON                                        99
     C                   ELSE
     C                   MOVEL     PRDSC1        XXBPRD           25
     C                   ENDIF                                                  *IN99  = 'I'
     C*
     C     *IN99         IFEQ      *ON
     C* If State flag STBS02='N' then don't check R&C file
     C     ##RC          IFEQ      'N'
     C                   SETON                                        9016
     C                   GOTO      CK1END
     C*
     C                   ELSE
     C*
     C                   MOVEL     SCZIPA        KKZIPA
     C                   MOVEL(P)  SCBLCD        KKCPT4
     C                   MOVEL     KKFDAT        KKEFDT
     C*
     C     RCKY2         CHAIN     PRC01                              99
     C*
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9016
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C*
     C     *IN99         IFEQ      *OFF
     C     RCSTAT        ANDEQ     'I'
     C                   SETON                                        9015
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C*
     C                   ENDIF                                                  ##RC = 'N'
     C*
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C*
     C                   ENDIF                                                  EODSTY = DR/DS
     C                   ENDIF                                                  SCBLCD IFEQ BLANKS
     C*** Beg Add *** 06/21/12 ****************************************
      /Free
         Endif;
      /End-Free
     C*** End Add *** 06/21/12 ****************************************
     C*** Beg Add *** 10/18/12 ****************************************
      /Free
       Endif;
      /End-free
     C*** End Add *** 10/18/12 ****************************************
     C**VERIFY*THAT THERE IS A FEE SCHEDULE ON THE SYSTEM FOR THE
     C**SERVICE*DATE
     C**VALIDATE*CPT4 CODE
     C**GO*TO*REASONABLE AND CUTORMARY FILE IF CPT4 EFFECTIVE DATE IS
     C**LESS*THAN FEE SCHEDULE EFFECTIVE DATE
     C**N99******PRSTAT    IFEQ 'I'
     C**VERIFY*THAT THE PROCECURE CODE IS EFFECTIVE FOR THE FEE SCHEDULE
     C*
     C* VALIDATE THE PROCEDURE CODE MODIFIERS IF ENTETED (1-4)
     C*
     C     SCMOD1        IFNE      *BLANKS
     C     *IN46         ANDEQ     *OFF
     C                   MOVEL(P)  SCMOD1        KKKEY
     C     TBKY          CHAIN     PPM01                              99
     C     *IN99         IFEQ      *ON
     C     PMSTAT        OREQ      'I'
     C                   SETON                                        9085
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C                   ENDIF                                                  SCMOD1 IFNE *BLANKS
     C*
     C     SCMOD2        IFNE      *BLANKS
     C     *IN46         ANDEQ     *OFF
     C                   MOVEL(P)  SCMOD2        KKKEY
     C     TBKY          CHAIN     PPM01                              99
     C     *IN99         IFEQ      *ON
     C     PMSTAT        OREQ      'I'
     C                   SETON                                        9086
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C                   ENDIF                                                  SCMOD2 IFNE *BLANKS
     C*
     C     SCMOD3        IFNE      *BLANKS
     C     *IN46         ANDEQ     *OFF
     C                   MOVEL(P)  SCMOD3        KKKEY
     C     TBKY          CHAIN     PPM01                              99
     C     *IN99         IFEQ      *ON
     C     PMSTAT        OREQ      'I'
     C                   SETON                                        9087
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C                   ENDIF                                                  SCMOD3 IFNE *BLANKS
     C*
     C     SCMOD4        IFNE      *BLANKS
     C     *IN46         ANDEQ     *OFF
     C                   MOVEL(P)  SCMOD4        KKKEY
     C     TBKY          CHAIN     PPM01                              99
     C     *IN99         IFEQ      *ON
     C     PMSTAT        OREQ      'I'
     C                   SETON                                        9088
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C                   ENDIF                                                  SCMOD4 IFNE *BLANKS
     C*
     C* ONE DIAGNOSIS CODE MUST BE MARKED WITH AN 'X'
     C*
     C     SCDXR1        IFEQ      *BLANKS
     C     SCDXR2        ANDEQ     *BLANKS
     C     SCDXR3        ANDEQ     *BLANKS
     C     SCDXR4        ANDEQ     *BLANKS
     C     *IN46         ANDEQ     *OFF
     C                   SETON                                        9066
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C* VALIDATE DIAGNOSIS CODE CROSS REFERENCE FUND
     C*
     C     SCDXR1        IFNE      'X'
     C     SCDXR1        ANDNE     *BLANKS
     C     *IN46         ANDEQ     *OFF
     C                   SETON                                        9079
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C* VALIDATE DIAGNOSIS CODE CROSS REFERENCE FUND
     C*
     C     SCDXR2        IFNE      'X'
     C     SCDXR2        ANDNE     *BLANKS
     C     *IN46         ANDEQ     *OFF
     C                   SETON                                        9080
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C     SCDXR3        IFNE      'X'
     C     SCDXR3        ANDNE     *BLANKS
     C     *IN46         ANDEQ     *OFF
     C                   SETON                                        9081
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C     SCDXR4        IFNE      'X'
     C     SCDXR4        ANDNE     *BLANKS
     C     *IN46         ANDEQ     *OFF
     C                   SETON                                        9082
     C                   GOTO      CK1END
     C                   ENDIF
     C*
    C***** Beg Add *** 11/02/15 **************************************
     C*VALIDATE NATIONAL DRUG CODE (NDC)
     C*
     C     SCNDC         IFEQ      *BLANKS
     C                   MOVEL     *BLANKS       SCNDCD
     C                   ENDIF
     C*
     C     SCPRCD        IFEQ      '96370'
     C     SCNDC         ANDEQ     *BLANKS
     C                   Z-ADD     @@RRN2        ##RRN2
     C                   Z-ADD     @@RRN3        ##RRN3
     C*
     C     @@MODE        IFEQ      'A'
     C                   Z-ADD     SCSEQ#        ##DSEQ            5 0
     C                   Z-ADD     0             ##RRN3
     C                   Z-ADD     SCSEQ#        KKDSEQ
     C                   ELSE
     C                   Z-ADD     0             ##DSEQ
     C                   ENDIF                                                  @@MODE = 'A'
     C*
     C                   MOVE      'N'           FNDEB4            1
     C*
     C     DEKY          CHAIN(N)  PDE01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*** Beg Chg *** 07/18/23 **************************************
     C****               MOVEL(P)  DEEOB         KKKEY
     C                   MOVEL(P)  DEEOBCD       KKKEY
     C*** End Chg *** 07/18/23 **************************************
     C     TBKY          CHAIN     PEB01                              99
     C     *IN99         IFEQ      *OFF
     C     DESTAT        ANDEQ     'A'
     C*
     C     EBXREF        ANDEQ     'D62'
     C*
     C                   MOVE      'Y'           FNDEB4
     C                   LEAVE
     C*
     C                   ENDIF
     C*
     C     DEKY          READE(N)  PDE01                                  99
     C*
     C                   ENDDO
     C*
     C     FNDEB4        IFEQ      'N'
     C*
     C                   EXFMT     EB4WDW
     C*
     C     *INKL         IFEQ      *ON
     C                   SETON                                        9006
     C                   GOTO      CK1END
     C                   ELSE                                                   *INKL=*ON
     C*
     C                   CALL      'F0356R01'
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      'A'           ##MODE
     C                   PARM      *ZEROS        ##RRN
     C                   PARM                    ##RRN2
     C                   PARM                    ##RRN3
     C                   PARM                    ##DSEQ
     C*
     C                   SETON                                        90
     C                   GOTO      CK1END
     C                   ENDIF                                                  *INLK=*ON
     C                   ENDIF                                                  FNDEB4    IFEQ 'N'
     C                   ENDIF                                                  SCPRCD IFEQ 96370
     C*
     C     SCNDC         IFNE      *BLANKS
     C     *IN46         ANDEQ     *OFF
     C*
     C                   MOVEL(P)  SCNDC         KKNDC
     C     RXKY          CHAIN     PRX01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9001
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C*
     C     RXSTAT        IFEQ      'I'
     C                   SETON                                        9001
     C                   GOTO      CK1END
     C                   ENDIF                                                  RXSTAT = 'I'
     C                   ENDIF                                                  SCNDC IFNE BLANKS
     C*
    C***** End Add *** 11/02/15 **************************************
     C* VALIDATE TYPE OF SERVICE CODE
     C*
     C     SCPOS         IFEQ      24
     C     SCAST         ANDEQ     'TX'
     C     SCTOS         IFEQ      *BLANKS
     C                   SETON                                        909492
     C                   SETOFF                                       9193
     C                   ENDIF                                                  SCTOS = ' '
     C                   ENDIF                                                  SCPOS = ' 24'
     C     SCTOS         IFNE      *BLANKS
     C     *IN46         ANDEQ     *OFF
     C                   MOVEL(P)  EOAST         HLKEY             2
     C                   MOVEL(P)  SCTOS         HDKEY             3
     C*                    MOVE HDKEY     HLKEY
     C                   MOVEL(P)  TMTOS         KKKEY
     C     TBKY          CHAIN     PTS02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9078
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ ON
     C                   ENDIF                                                  SCTOS IFNE BLANKS
     C*
     C* TYPE CODE #1 REQUIRED IF CORRESPONDING AMOUNT IS ENTERED
     C*
     C     SCOTHR        IFNE      *ZEROS
     C     SCOT1C        ANDEQ     *BLANKS
     C                   SETON                                        9061
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCOTHER IFN *ZEROS
     C*
     C* VALIDATE TYPE CODE #1
     C*
     C     SCOT1C        IFNE      *BLANKS
     C                   MOVE      'O'           KKTYPE
     C                   MOVEL(P)  SCOT1C        KKCODE
     C     BSKY          CHAIN     PBS01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9063
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C                   ENDIF                                                  SCOT1C IFNE *BLANKS
     C*
     C* TYPE CODE #2 REQUIRED IF CORRESPONDING AMOUNT IS ENTERED
     C*
     C     SCOTH2        IFNE      *ZEROS
     C     SCOT2C        ANDEQ     *BLANKS
     C                   SETON                                        9062
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCOTHER IFN *ZEROS
     C*
     C* VALIDATE TYPE CODE #2
     C*
     C     SCOT2C        IFNE      *BLANKS
     C                   MOVE      'O'           KKTYPE
     C                   MOVEL(P)  SCOT2C        KKCODE
     C     BSKY          CHAIN     PBS01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9064
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C                   ENDIF                                                  SCOT2C IFNE *BLANKS
       //*** Beg add ***** 07/18/25 *************************************
       If isModEnabled('NCCI Med Data Call');
          // %check return non zero if non numeric value including blank
          invalidPos = %check('0123456789': %trim(scnpir));
          If invalidPos <> 0 or scnpir = *blanks;
             msnpir = 'MC50007';
             ernpir = *on;
             *in90 = *on;
             leavesr;
          Endif;
       Endif;
       //*** End add ***** 07/18/25 ************************************
     C**BODY*AREA IS REQUIRED IF SURGERY CODE
     C*
     C* VALIDATE BODY PART CODE IF ENTERED
     C*
     C     SCAREA        IFNE      *BLANKS
     C     *IN46         ANDEQ     *OFF
     C                   MOVEL(P)  SCAREA        KKKEY
     C     TBKY          CHAIN     PPA01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9040
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C                   ENDIF                                                  SCAREA IFNE *BLANKS
     C* Now perform all state specific edits before cross footing
     C                   SELECT
     C*
     C*
     C     SCAST         WHENEQ    'AL'
     C     *IN46         ANDEQ     *OFF
     C*
     C                   EXSR      EDITAL
     C   90              GOTO      CK1END
     C*
     C     SCAST         WHENEQ    'FL'
     C     *IN46         ANDEQ     *OFF
     C*
     C                   EXSR      EDITFL
     C   90              GOTO      CK1END
     C*
     C     SCAST         WHENEQ    'LA'
     C     *IN46         ANDEQ     *OFF
     C*
     C                   EXSR      EDITLA
     C   90              GOTO      CK1END
     C*
     C     SCAST         WHENEQ    'NE'
     C     *IN46         ANDEQ     *OFF
     C*
     C                   EXSR      EDITNE
     C   90              GOTO      CK1END
     C*
     C     SCAST         WHENEQ    'NC'
     C     *IN46         ANDEQ     *OFF
     C*
     C                   EXSR      EDITNC
     C   90              GOTO      CK1END
     C*
     C     SCAST         WHENEQ    'NM'
     C     *IN46         ANDEQ     *OFF
     C*
     C                   EXSR      EDITNM
     C   90              GOTO      CK1END
     C*
     C     SCAST         WHENEQ    'NY'
     C     *IN46         ANDEQ     *OFF
     C*
     C                   EXSR      EDITNY
     C   90              GOTO      CK1END
     C*
     C     SCAST         WHENEQ    'OH'
     C     *IN46         ANDEQ     *OFF
     C*
     C                   EXSR      EDITOH
     C   90              GOTO      CK1END
     C*
     C     SCAST         WHENEQ    'TX'
     C     *IN46         ANDEQ     *OFF
     C*
     C                   EXSR      EDITTX
     C   90              GOTO      CK1END
     C*
     C     SCAST         WHENEQ    'WV'
     C     *IN46         ANDEQ     *OFF
     C*
     C                   EXSR      EDITWV
     C   90              GOTO      CK1END
     C*
     C     SCAST         WHENEQ    'PA'
     C     *IN46         ANDEQ     *OFF
     C                   EXSR      EDITPA
     C   90              GOTO      CK1END
     C*
     C                   ENDSL                                                              *BLANKS
     C*
     C* CROSS-FOOT IF ADJUDICATION CODE IS OVERRIDE TYPE AND HAS
     C* NO OVERRIDE PRECENTAGE
     C*
     C                   MOVEL(P)  SCCSCD        KKKEY
     C     TBKY          CHAIN     PCS01                              99
     C*
     C* IF THE SUM OF THE REDUCTIONS AND PAID IS GREATER THAN THE
     C* CHARGED AMOUNT, THEN GIVE AN ERROR
     C*
     C     *IN99         IFEQ      *OFF
     C     CSOVRD        ANDEQ     'Y'
     C     CSOVRP        ANDEQ     *ZEROS
     C     SCCHRG        ANDNE     *ZEROS
     C     SCMRA         ADD       SCOTHR        XXTOTL
     C                   ADD       SCOTH2        XXTOTL
     C   47              ADD       SCNTWK        XXTOTL
     C                   ADD       SCPAID        XXTOTL
     C     XXTOTL        IFGT      SCCHRG
     C                   SETON                                        9058
     C                   GOTO      CK1END
     C                   ENDIF                                                  XXTOTL GT SCCHRG
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C* SCENARIO #1
     C*
     C* MOVE CHARGE AMOUNT INTO MRA AMOUNT IF ALL REDUCTIONS ARE
     C* *ZERO AND PAID AMOUNT IS ZERO
     C*
     C     *IN99         IFEQ      *OFF
     C     CSOVRD        ANDEQ     'Y'
     C     CSOVRP        ANDEQ     *ZEROS
     C     SCCHRG        ANDNE     *ZEROS
     C     SCMRA         ANDEQ     *ZEROS
     C     SCOTHR        ANDEQ     *ZEROS
     C     SCOTH2        ANDEQ     *ZEROS
     C     SCNTWK        ANDEQ     *ZEROS
     C     SCPAID        ANDEQ     *ZEROS
     C     SCCSCD        ANDNE     '300'
     C                   Z-ADD     SCCHRG        SCMRA
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C* SCENARIO #2
     C*
     C* IF CHARGE AMOUNT IS NOT ZERO, PAID AMOUNT IS NOT ZERO
     C* AND ALL REDUCTIONS AMOUNTS ARE ZERO, THEN PUT DIFFERENCE
     C* BETWEEN CHARGE AMOUNT AND PAID AMOUNT IN MRA REDUCTIONS
     C*
     C     *IN99         IFEQ      *OFF
     C     CSOVRD        ANDEQ     'Y'
     C     CSOVRP        ANDEQ     *ZEROS
     C     SCCHRG        ANDNE     *ZEROS
     C     SCPAID        ANDNE     *ZEROS
     C     SCMRA         ANDEQ     *ZEROS
     C     SCOTHR        ANDEQ     *ZEROS
     C     SCOTH2        ANDEQ     *ZEROS
     C     SCNTWK        ANDEQ     *ZEROS
     C     SCCHRG        SUB       SCPAID        SCMRA
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C* THE FOLLOWING MATH WILL CALCULATE THE PAID AMOUNT
     C* BASED ON THE CHARGED AMOUNT AND REDUCTION AMOUNTS ENTERED
     C* THIS WILL NOT CAUSE PROBLEMS WITH SCENARIOS #1 AND #2
     C*
     C     SCCHRG        IFNE      *ZEROS
     C     SCCSCD        ANDNE     '300'
     C     SCCHRG        SUB       SCMRA         SCPAID
     C                   SUB       SCOTHR        SCPAID
     C                   SUB       SCOTH2        SCPAID
     C                   SUB       SCNTWK        SCPAID
     C                   ENDIF                                                  SCCHRG NE *ZEROS
     C     SCCHRG        IFNE      *ZEROS
     C     SCCSCD        ANDNE     '300'
     C     EOCRNE        ANDEQ     'Y'
     C     CSOVRD        ANDNE     'Y'
     C     SCMRA         ANDEQ     *ZEROS
     C     SCOTHR        ANDEQ     *ZEROS
     C     SCOTH2        ANDEQ     *ZEROS
     C     SCNTWK        ANDEQ     *ZEROS
     C                   Z-ADD     0             SCPAID
     C                   ENDIF                                                  SCCHRG NE *ZEROS
     C*
     C* SPECIAL CASE OF MFS = 'Y' FOR '300
     C*
     C     SCCHRG        IFNE      *ZEROS
     C     SCCSCD        ANDEQ     '300'
     C*
     C     SCMRA         IFNE      *ZEROS
     C     SCOTHR        ORNE      *ZEROS
     C     SCOTH2        ORNE      *ZEROS
     C     SCNTWK        ORNE      *ZEROS
     C*
     C     SCCHRG        SUB       SCMRA         SCPAID
     C                   SUB       SCOTHR        SCPAID
     C                   SUB       SCOTH2        SCPAID
     C                   SUB       SCNTWK        SCPAID
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C* IF OVERIDE TYPE AND CHARGE IS ZEROS AND PAID NE ZEROS, THEN
     C* PUT THE DIFFERENCE IN SCMRA
     C*
     C     SCCHRG        IFEQ      *ZEROS
     C     CSOVRD        ANDEQ     'Y'
     C     SCPAID        ANDNE     *ZEROS
     C     SCCHRG        SUB       SCPAID        SCMRA
     C                   Z-ADD     0             SCOTHR
     C                   Z-ADD     0             SCOTH2
     C                   Z-ADD     0             SCNTWK
     C                   ENDIF                                                  SCCHRG NE *ZEROS
     C*
     C     SCCHRG        IFEQ      *ZEROS
     C     CSOVRD        ANDEQ     'Y'
     C     SCPAID        ANDEQ     *ZEROS
     C     SCCHRG        SUB       SCMRA         SCPAID
     C                   SUB       SCOTHR        SCPAID
     C                   SUB       SCOTH2        SCPAID
     C                   SUB       SCNTWK        SCPAID
     C                   ENDIF                                                  SCCHRG = *ZEROS
     C*
     C* IF ADJUDICATION CODE IS AN OVERRIDE TYPE AND THE OVERRIDE
     C* PRECENTAGE IS GREATER THAN ZEROS, CLEAR ALL REDUCTIONS AMOUNT
     C* FIELDS AND PAID AMOUNT FIELD.  THE ADJUDICATION PROGRAM
     C* REQUIRES THIS AMOUNTS TO BE ZEROS IN ORDER TO CALCULATE
     C* THE PERCENTAGE
     C*
     C     *IN99         IFEQ      *OFF
     C     CSOVRD        ANDEQ     'Y'
     C     CSOVRP        ANDGT     *ZEROS
     C                   Z-ADD     0             SCMRA
     C                   Z-ADD     0             SCOTHR
     C                   Z-ADD     0             SCOTH2
     C                   Z-ADD     0             SCNTWK
     C                   Z-ADD     0             SCPAID
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C* GIVE ERROR IF THE ADJUDICATION CODE IS AN OVERRIDE TYPE,
     C* OVERRIDE PERCENT IS ZERO, A REDUCTION IS APPLIED AND
     C* NO EOB CODE EXISTS FOR THE LINE ITEM
     C*
     C* 03/30/99 -  CALL EOB CODES/NOTES PROGRAM NOW INSTEAD OF GIVING
     C*             AN ERROR
     C*
     C                   MOVE      *BLANKS       XXEOB
     C     *IN99         IFEQ      *OFF
     C     CSOVRD        ANDEQ     'Y'
     C*
     C     CSOVRP        ANDLT     100
     C*
     C     SCAST         IFEQ      'FL'
     C     SCPAID        ANDLT     SCCHRG
     C*
     C     @@MODE        IFEQ      'A'
     C                   Z-ADD     SCSEQ#        ##DSEQ            5 0
     C                   Z-ADD     0             ##RRN3
     C                   Z-ADD     SCSEQ#        KKDSEQ
     C                   ELSE
     C                   Z-ADD     0             ##DSEQ
     C                   ENDIF                                                  @@MODE = 'A'
     C*
     C                   EXSR      EDITEB
     C*
     C                   ENDIF
     C*
     C*
     C*
     C                   MOVEL(P)  '*YES'        GO                4
     C                   Z-ADD     EORCN         KKRCN
     C                   Z-ADD     SCSEQ#        KKDSEQ
     C     DEKY2         CHAIN     PDE03                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C     DEESRC        IFEQ      'U'
     C*** Beg Add *** 01/18/16 ****************************************
     C     DESTAT        ANDEQ     'A'
     C*** End Add *** 01/18/16 ****************************************
     C                   MOVEL     *BLANKS       GO
     C                   LEAVE
     C                   ENDIF
     C*
     C     DEKY2         READE     PDE03                                  99
     C                   ENDDO
     C*
     C     GO            IFEQ      '*YES'
     C*
     C     FNDEB1        OREQ      'N'
     C     FNDEB2        OREQ      'N'
     C*
     C                   Z-ADD     @@RRN2        ##RRN2
     C                   Z-ADD     @@RRN3        ##RRN3
     C*
     C* IF IN ADD MODE, NEED TO SEND IN SCREEN DSEQ NUMBER TO BE
     C* WRITTEN IN EOB CODES/NOTES FILE.  THIS IS DONE BECAUSE THIS
     C* DETAIL CHARGE IS NOT WRITTEN YET. ALSE SEND ZERO FOR THE
     C* RELATIVE RECORD NUMBER OF FDCAP (##RRN3).
     C*
     C     @@MODE        IFEQ      'A'
     C                   Z-ADD     SCSEQ#        ##DSEQ            5 0
     C                   Z-ADD     0             ##RRN3
     C                   Z-ADD     SCSEQ#        KKDSEQ
     C                   ELSE
     C                   Z-ADD     0             ##DSEQ
     C                   ENDIF                                                  @@MODE = 'A'
     C*
     C*** REMOVE ANY EOB CODES AND MESSAGES PRODUCED BY ADJUDICATION
     C*   OR NETWORK PROCESSING
     C*
     C     DEKY          CHAIN     PDE01                              99
     C     *IN99         DOWEQ     *OFF
     C     DEESRC        IFEQ      'A'
     C*** Beg Chg *** 07/18/23 **************************************
     C**** DEEOB         ANDNE     *BLANKS
     C     DEEOBCD       ANDNE     *BLANKS
     C*** End Chg *** 07/18/23 **************************************
     C     DEESRC        OREQ      'N'
     C*** Beg Chg *** 07/18/23 **************************************
     C**** DEEOB         ANDNE     *BLANKS
     C     DEEOBCD       ANDNE     *BLANKS
     C*** End Chg *** 07/18/23 **************************************
     C     DERCN         IFEQ      EORCN
     C                   DELETE    PDE01
     C                   ELSE
     C                   EXCEPT    RPDE01
     C                   ENDIF
     C                   ELSE
     C                   EXCEPT    RPDE01
     C                   ENDIF                                                  DEESRC = 'A'
     C     DEKY          READE     PDE01                                  99
     C                   ENDDO
     C*
     C     MGKY          CHAIN     PMG01                              99
     C     *IN99         DOWEQ     *OFF
     C     MGSRC         IFEQ      'S'
     C     MGSRC         OREQ      'N'
     C     MGSRC         OREQ      'C'
     C     MGMSG#        IFNE      'NW00001'
     C                   DELETE    PMG01
     C                   ELSE
     C*
     C                   UPDATE    PMG01
     C*
     C                   ENDIF
     C                   ELSE
     C                   EXCEPT    RPMG01
     C                   ENDIF                                                  MGSRC = 'S'
     C     MGKY          READE     PMG01                                  99
     C                   ENDDO
     C*
     C     FNDEB1        IFEQ      'N'
     C                   EXFMT     EB1WDW
     C                   ENDIF
     C     FNDEB2        IFEQ      'N'
     C                   EXFMT     EB2WDW
     C                   ENDIF
     C*
     C                   CALL      'F0356R01'
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      'A'           ##MODE
     C                   PARM      *ZEROS        ##RRN
     C                   PARM                    ##RRN2
     C                   PARM                    ##RRN3
     C                   PARM                    ##DSEQ
     C                   PARM      SCCHRG        ##CHRG
     C                   PARM      SCPAID        ##PAID
     C*
     C     ##KL          IFEQ      '1'
     C                   SETON                                        9014
     C                   SETOFF                                       69
     C                   ELSE
     C                   SETOFF                                       14
     C                   ENDIF
     C     SCMRA         IFNE      *ZEROS
     C     SCNTWK        ORNE      *ZEROS
     C     SCOTHR        ORNE      *ZEROS
     C     SCOTH2        ORNE      *ZEROS
     C*
     C*
     C*
     C*
     C                   Z-ADD     EORCN         KKRCN
     C                   Z-ADD     SCSEQ#        KKDSEQ
     C                   SETON                                        90
     C     DEKY2         CHAIN     PDE03                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C     DEESRC        IFEQ      'U'
     C*** Beg Add *** 01/18/16 ****************************************
     C     DESTAT        ANDEQ     'A'
     C*** End Add *** 01/18/16 ****************************************
     C*** Beg Chg *** 07/18/23 **************************************
     C****               MOVEL(P)  DEEOB         XXEOB
     C                   MOVEL(P)  DEEOBCD       XXEOB
     C*** End Chg *** 07/18/23 **************************************
     C                   MOVEL(P)  DENOT1        XXNOT1
     C                   MOVEL(P)  DENOT2        XXNOT2
     C                   SETOFF                                       90
     C                   LEAVE
     C                   ENDIF
     C*
     C     DEKY2         READE     PDE03                                  99
     C                   ENDDO
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C*
     C*
     C                   ENDIF                                                  GO = *YES
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C**CLEAR*PAID AMOUNT AND REDUCTIONS FIELDS IF THE ADJUDICATION
     C**CODE*IS NOT OVERRIDE TYPE.  ADJUDICATION PROGRAMS AND NETWORK
     C**PROGRAMS WILL DETERMINE THE NECESSARY REDUCTIONS.
     C*
     C     SCAST         IFEQ      'FL'
     C     SCPRCD        ANDNE     SCBLCD
     C     SCBLCD        ANDNE     *BLANKS
     C*
     C                   Z-ADD     @@RRN2        ##RRN2
     C                   Z-ADD     @@RRN3        ##RRN3
     C*
     C     @@MODE        IFEQ      'A'
     C                   Z-ADD     SCSEQ#        ##DSEQ            5 0
     C                   Z-ADD     0             ##RRN3
     C                   Z-ADD     SCSEQ#        KKDSEQ
     C                   ELSE
     C                   Z-ADD     0             ##DSEQ
     C                   ENDIF                                                  @@MODE = 'A'
     C*
     C                   MOVE      'N'           FNDEB3            1
     C*
     C     DEKY          CHAIN(N)  PDE01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*** Beg Chg *** 07/18/23 **************************************
     C*****              MOVEL(P)  DEEOB         KKKEY
     C                   MOVEL(P)  DEEOBCD       KKKEY
     C*** End Chg *** 07/18/23 **************************************
     C     TBKY          CHAIN     PEB01                              99
     C     *IN99         IFEQ      *OFF
     C     DESTAT        ANDEQ     'A'
     C     EBXREF        ANDEQ     'S11'
     C*
     C                   MOVE      'Y'           FNDEB3
     C                   LEAVE
     C*
     C                   ENDIF
     C*
     C     DEKY          READE(N)  PDE01                                  99
     C*
     C                   ENDDO
     C*
     C     FNDEB3        IFEQ      'N'
     C*
     C                   EXFMT     EB3WDW
     C*
     C                   CALL      'F0356R01'
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      'A'           ##MODE
     C                   PARM      *ZEROS        ##RRN
     C                   PARM                    ##RRN2
     C                   PARM                    ##RRN3
     C                   PARM                    ##DSEQ
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* IF IN CHANGE MODE, THEN SET 'OUTFLG' TO END PROGRAM
     C*
     C     CALEND        TAG
     C*
     C     @@MODE        IFEQ      'C'
     C                   MOVE      '1'           OUTFLG
     C                   ENDIF
     C*
     C     CK1END        ENDSR                                                  SRERR IFEQ 1
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*================================================================
     C* KADO - F01=REPEAT
     C*================================================================
     C     KADO          BEGSR
     C*
     C     SCLIN#        IFGT      0
     C     SCLIN#        SUB       1             KKLIN#
     C     CAKY4         CHAIN     PCA04                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     CADSEQ        KKDSEQ
     C     DHKY          CHAIN     PDH01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C* POSITION CURSOR TO DATE OF SERVICE
     C*
     C                   SETON                                        41
     C                   MOVEL(P)  DHNPIR        SCNPIR
     C*
     C* LOAD REPEAT VALUES TO SCREEN
     C*
     C                   Z-ADD     CAFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCFDAT
     C*
     C                   Z-ADD     CACHRG        SCCHRG
     C                   MOVEL     CACSCD        SCCSCD
     C                   MOVEL(P)  SCCSCD        KKKEY
     C     TBKY          CHAIN     PCS01                              99
     C  N99              MOVEL(P)  CSDESC        SCCSDS
     C*
     C*
     C                   MOVE      DHPOS         SCPOS
     C                   MOVE(P)   SCPOS         KKPOS
     C     PSKY          CHAIN     PPS01                              99
     C  N99              MOVEL(P)  PSDESC        SCPODS
     C*
     C                   MOVEL     DHPRCD        SCPRCD
     C                   MOVEL     DHAREA        SCAREA
     C**N99*************** MOVELTSDESC    SCTODS    P
     C*
     C                   MOVE      DHDXR1        SCDXR1
     C                   MOVE      DHDXR2        SCDXR2
     C                   MOVE      DHDXR3        SCDXR3
     C                   MOVE      DHDXR4        SCDXR4
     C                   Z-ADD     1             SCUNTS
     C*
     C**N99*************** MOVELPADESC    SCARDS    P
     C                   SETON                                        90
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  SCLIN# GT 0
     C                   ENDSR
     C*================================================================
     C* KBDO - F2=PROCESS
     C*================================================================
     C     KBDO          BEGSR
     C*
     C* IF USER HITS F2 IN ADD MODE ON LIN#1, THEN CALL SUBROUTINES
     C* ERRCK1 AND WRITER BEFOR ADJUDICATING
     C*
     C     @@MODE        IFEQ      'A'
     C     SCLIN#        ANDEQ     1
     C                   SETOFF                                       KB
B02  C     *IN90         DOUEQ     *OFF
     C                   MOVEA     '00000'       *IN(71)
     C                   MOVEA     '00000'       *IN(76)
     C                   MOVEA     '00000'       *IN(81)
     C                   MOVEA     '00000'       *IN(86)
B03  C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C     *INKE         CASEQ     *ON           KEDO
     C                   CAS                     OTHFKS
     C                   ENDCS
B04  C     *IN90         IFEQ      *OFF
     C     @@MODE        ANDNE     'D'
     C                   EXSR      ERRCK1
E04  C                   ENDIF
B04  C     *IN90         IFEQ      *OFF
B05  C     #OFSCR        IFEQ      1
     C                   EXSR      FINISH
X05  C                   ELSE
     C     @@MODE        CASEQ     'C'           UPDATR
     C     @@MODE        CASEQ     'D'           DSPLYR
     C                   CAS                     OTHERR
     C                   ENDCS
     C                   EXSR      XSCR02
E05  C                   ENDIF
E04  C                   ENDIF
E03  C                   ENDIF
     C   90              EXFMT     SCR01
E02  C                   ENDDO
     C                   ENDIF
     C*
     C*  CALL FA713R WHICH WILL CALL APPROPRIATE ADJUDICATION PROGRAMS
     C*
     C**WRITE/UPDATE EOMB DETAIL AMOUNT TRANSACTION FILE
     C**COMPARE*THE DETAIL CHARGE AMOUNTS WITH SUM OF DETAIL AMOUNTS
     C**IF*THERE IS A DIFFERENCE, THEN WRITE OR UPDATE DETAIL AMONT REC
     C**UPDATE*IF THERE IS A FDDAP FOR THE CURRENT RECON
     C**ELSE*WRITE NEW FDDAP RECORD TO REFLECT CHANGES
     C*
     C                   SETON                                        LR
     C*
     C                   RETURN
     C*
     C     KBEND         ENDSR
     C*
     C*================================================================
     C* KGDO - F7=HEADER INFO
     C*================================================================
     C     KGDO          BEGSR
     C     BEKY          CHAIN     PBE01                              99
     C     *IN99         IFEQ      *OFF
     C     BEHESM        ANDNE     *BLANK
     C                   Z-ADD     @@RRN2        ##RRN
     C                   MOVEL(P)  BEHESM        PGMNAM           10
     C                   CALL      PGMNAM
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRN
     C                   ENDIF                                                  *IN99 = Off
     C                   SETON                                        90
     C     KGEND         ENDSR
     C*
     C*================================================================
     C* KIDO - F9=CODE INFO
     C*================================================================
     C     KIDO          BEGSR
     C*
     C*
     C*** Beg Add *** 04/04/16 ****************************************
      /Free
       If STmfs = 'C';
          *in26 = *on;
       Else;
          *in26 = *off;
       EndIf;
      /End-Free
     C*** End Add *** 04/04/16 ****************************************
     C** CAPTURE THE CURRENT VALUES OF *IN17 AND *IN18
     C                   MOVE      *IN17         IN17X             1
     C                   MOVE      *IN18         IN18X             1
     C*
     C                   CLEAR                   WDW01
     C     EOAST         IFEQ      'KY'
     C                   MOVE      X'20'         AT3
     C                   MOVE      X'24'         AT4                            UNDER HI
     C                   MOVE      X'A7'         AT5
     C                   ELSE
     C                   MOVE      X'A7'         AT3
     C                   MOVE      X'A7'         AT4
     C                   MOVE      X'20'         AT5
     C                   ENDIF
     C*** choose a  indicator to reflect PPO processing  fields on WDW01
     C** set it on when bill is medicare PPO processing bill.
     C                   CLEAR                   WDW02
       //*** Beg Add 08/06/25 **********************************************
       If EOdsTy = 'DR' or EOdsTy = 'DS';
          strMsg = 'Medicare or State Procedure Code Information';
          isMedicare = PopTwoBtn(strmsg:'Select Information':
           'Medicare':'State');
       Endif;
       //*** End Add 08/06/25 **********************************************
       //*** Beg Del 08/06/25 **********************************************
     C***  EODSTY        IFEQ      'DR'
     C***  EODSTY        OREQ      'DS'
     C***  XXMFS         OREQ      'Y'
     C***  EOAST         ANDNE     'SC'
       //*** End Del 08/06/25 **********************************************
     C*** Beg Add *** 07/26/12 *****************************************
       //*** Beg Del 08/06/25 **********************************************
     C***  EOAST         OREQ      'TN'
       //*** End Del 08/06/25 **********************************************
      /Free
       //*** Beg Add 08/06/25 **********************************************
       If ((EOdsTy = 'DR' or EOdsTy = 'DS') and isMedicare) or
        (xxMfs = 'Y' and EOast <> 'SC') or EOast = 'TN';
       //*** End Add 08/06/25 **********************************************
           If eoast = 'TN';
              *in54 = *on;
           Else;
              *in54 = *off;
           Endif;
      /End-free
     C*** End Add *** 07/26/12 *****************************************
     C                   EXSR      PRCWDW
     C                   ELSE
     C     SCAST         IFEQ      'AR'
     C                   SETON                                        57
     C                   ELSE
     C                   SETOFF                                       57
     C                   ENDIF
     C*
     C*** BEG ADD *** 06/07/13 ****************************************
      /Free
         If EOast = 'SC';
            *in26 = *on;
         Else;
            *in26 = *off;
         Endif;
      /End-Free
     C*** END ADD *** 06/07/13 ****************************************
     C     EXFWDW        TAG
     C*** Beg Add *** 03/31/20 *********************************************
     C                   Eval      scPrCf = '1'
     C*** End Add *** 03/31/20 *********************************************
     C                   EXFMT     WDW01
     C*
     C                   MOVE      *OFF          *IN57
     C                   MOVE      *OFF          *IN06
     C                   MOVE      *OFF          *IN17
     C                   MOVE      *OFF          *IN18
     C                   MOVE      '0'           FLAG1             1
     C*
     C** VERIFY IF VALID OPTION HAS BEEN ENTERED
     C*
     C     SCPRCF        IFNE      '1'
     C     SCPRCF        ANDNE     *BLANKS
     C                   MOVE      '1'           *IN17
     C                   MOVE      '1'           *IN18
     C                   MOVE      '0'           *IN06
     C                   GOTO      EXFWDW
     C                   ENDIF
     C*
     C     SCASRF        IFNE      '1'
     C     SCASRF        ANDNE     *BLANKS
     C                   MOVE      '1'           *IN17
     C                   MOVE      '0'           *IN18
     C                   MOVE      '0'           *IN06
     C                   GOTO      EXFWDW
     C                   ENDIF
     C*
     C     SCASIN        IFNE      '1'
     C     SCASIN        ANDNE     *BLANKS
     C                   MOVE      '0'           *IN17
     C                   MOVE      '1'           *IN18
     C                   MOVE      '0'           *IN06
     C                   GOTO      EXFWDW
     C                   ENDIF
     C*
     C     SCRVU         IFNE      '1'
     C     SCRVU         ANDNE     *BLANKS
     C                   MOVE      '0'           *IN17
     C                   MOVE      '1'           *IN18
     C                   MOVE      '0'           *IN06
     C                   GOTO      EXFWDW
     C                   ENDIF
     C*
     C     SCLAB         IFNE      '1'
     C     SCLAB         ANDNE     *BLANKS
     C                   MOVE      '0'           *IN17
     C                   MOVE      '1'           *IN18
     C                   MOVE      '0'           *IN06
     C                   GOTO      EXFWDW
     C                   ENDIF
     C*
     C     SCAMB         IFNE      '1'
     C     SCAMB         ANDNE     *BLANKS
     C                   MOVE      '0'           *IN17
     C                   MOVE      '1'           *IN18
     C                   MOVE      '0'           *IN06
     C                   GOTO      EXFWDW
     C                   ENDIF
     C     SCDME         IFNE      '1'
     C     SCDME         ANDNE     *BLANKS
     C                   MOVE      '0'           *IN17
     C                   MOVE      '1'           *IN18
     C                   MOVE      '0'           *IN06
     C                   GOTO      EXFWDW
     C                   ENDIF
     C*
     C** VALIDATE IF ONLY ONE OPTION HAS BEEN SELECTED
     C*
     C     SCPRCF        IFNE      *BLANKS
     C                   MOVE      '1'           FLAG1
     C                   ENDIF
     C*
     C     SCASRF        IFNE      *BLANKS
     C     FLAG1         IFEQ      '1'
     C                   MOVE      '1'           *IN17
     C                   MOVE      '0'           *IN18
     C                   MOVE      '1'           *IN06
     C                   GOTO      EXFWDW
     C                   ELSE
     C                   MOVE      '1'           FLAG1
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCASIN        IFNE      *BLANKS
     C     FLAG1         IFEQ      '1'
     C                   MOVE      '0'           *IN17
     C                   MOVE      '1'           *IN18
     C                   MOVE      '1'           *IN06
     C                   GOTO      EXFWDW
     C                   ELSE
     C                   MOVE      '1'           FLAG1
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCRVU         IFNE      *BLANKS
     C     FLAG1         IFEQ      '1'
     C                   MOVE      '0'           *IN17
     C                   MOVE      '1'           *IN18
     C                   MOVE      '1'           *IN06
     C                   GOTO      EXFWDW
     C                   ELSE
     C                   MOVE      '1'           FLAG1
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCLAB         IFNE      *BLANKS
     C     FLAG1         IFEQ      '1'
     C                   MOVE      '0'           *IN17
     C                   MOVE      '1'           *IN18
     C                   MOVE      '1'           *IN06
     C                   GOTO      EXFWDW
     C                   ELSE
     C                   MOVE      '1'           FLAG1
     C                   ENDIF
     C                   ENDIF
     C     SCAMB         IFNE      *BLANKS
     C     FLAG1         IFEQ      '1'
     C                   MOVE      '0'           *IN17
     C                   MOVE      '1'           *IN18
     C                   MOVE      '1'           *IN06
     C                   GOTO      EXFWDW
     C                   ELSE
     C                   MOVE      '1'           FLAG1
     C                   ENDIF
     C                   ENDIF
     C     SCDME         IFNE      *BLANKS
     C     FLAG1         IFEQ      '1'
     C                   MOVE      '0'           *IN17
     C                   MOVE      '1'           *IN18
     C                   MOVE      '1'           *IN06
     C                   GOTO      EXFWDW
     C                   ELSE
     C                   MOVE      '1'           FLAG1
     C                   ENDIF
     C                   ENDIF
     C*
     C** POPULATE THE ORIGINAL VALUES TO *IN17 AND *IN18
     C                   MOVE      IN17X         *IN17
     C                   MOVE      IN18X         *IN18
     C*
     C                   SELECT
     C*
     C* PROCEED AS PER THE OPTION SELECTED BY USER.
     C* OPT#1 (SCPRCF) : PROCEDURE CODE FILE
     C* OPT#2 (SCASRF) : ASC PAYMENT RATE FILE
     C* OPT#3 (SCASIN) : ASC PAYMENT INFO
     C*
     C     SCASRF        WHENEQ    '1'
     C*
     C                   Z-ADD     0             ##RRN4            9 0
     C                   Z-ADD     SCFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         ##EFDT
     C                   MOVEL(P)  SCPRCD        ##HCPC
     C     ASKY          SETGT     PAS01
     C     ASKY1         READPE    PAS01                                  99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRN4
     C                   ELSE                                                   *IN99 = On
     C                   MOVE      ASRRN         ##RRN4
     C                   ENDIF                                                  *IN99 = On
     C*
     C                   CALL      'F2358R01'
     C                   PARM      ' '           @@KC              1
     C                   PARM      ' '           @@KL              1
     C                   PARM      'D'           @@MODE            1
     C                   PARM      ##RRN4        @@RRN             9 0
     C*
     C     SCASIN        WHENEQ    '1'
     C*
     C                   CALL      'F0365R'
     C                   PARM      EOCO#         ##CO#
     C                   PARM      EOSUB#        ##SUB#            5 0
     C                   PARM      EOFND         ##FND
     C                   PARM      EOFYR         ##FYR             3 0
     C                   PARM      EOCASE        ##CASE            7 0
     C                   PARM      EOEOB#        ##EOB#            9 0
     C                   PARM                    ##CONM
     C                   PARM      CADSEQ        #1DSEQ            5 0
     C*
     C     SCPRCF        WHENEQ    '1'
     C*
     C*
     C                   Z-ADD     SCFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         KKFDAT
     C     CARCFL        IFEQ      'C'
     C                   MOVE      EOAST         ##ST              2
     C                   MOVEL(P)  SCPRCD        KKPRCD
     C****** Beg Add **** 11/02/15 ************************************
     C     SCPRCD        IFNE      *BLANKS
     C     SCNDC         ANDEQ     *BLANKS
     C****** End Add **** 11/02/15 ************************************
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  99
     C****** Beg Add **** 11/02/15 ************************************
     C                   ENDIF
     C****** End Add **** 11/02/15 ************************************
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRN
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     PRRRN         ##RRN
     C                   ENDIF                                                  *IN99 = On
     C     SCAST         IFEQ      'TX'
     C     KKFDAT        ANDGE     1040901
     C     SCPRCD        ANDGE     '10000'
     C     SCPRCD        ANDLE     '69999'
     C     SCCNTY        ANDNE     *BLANKS
     C     SCRGN#        ANDNE     *BLANKS
     C*
     C                   MOVEL     'TX'          XXCNTY
     C                   MOVE      SCCNTY        XXCNTY
     C                   MOVEL(P)  XXCNTY        KKKEY
     C     CCKY          CHAIN     PCC01                              99
     C*
     C                   MOVEL(P)  CC10A1        KKRGN
     C                   MOVE(P)   PR1AI         KKGRP
     C*
     C     AGKY          SETGT     PAG01
     C     AGKY1         READPE    PAG01                                  99
     C*
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             #1RRN             9 0
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     DBRRND        #1RRN
     C                   ENDIF                                                  *IN99 = On
     C*
     C                   CALL      'F2522R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    #1RRN
     C                   PARM                    ##ST
     C*
     C                   ENDIF                                                  SCAST = TX
     C                   CALL      'F2305R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRN
     C                   PARM                    ##ST
     C                   ENDIF                                                  RCFP = 'C'
     C     CARCFL        IFEQ      'Y'
     C                   MOVEL     SCZIPA        KKZIPA
     C                   MOVEL(P)  SCPRCD        KKCPT4
     C                   MOVEL     KKFDAT        KKEFDT
     C     RCKY1         SETGT     PRC01
     C     RCKY2         READPE    PRC01                                  99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRN
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     RCRRN         ##RRN
     C                   ENDIF                                                  *IN99 = On
     C                   CALL      'F2310R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRN
     C                   PARM                    ##ST
     C                   ENDIF                                                  RCFP = 'Y'
     C*
     C     SCRVU         WHENEQ    '1'
     C*
     C                   MOVEL(P)  SCPRCD        ##HCPC
     C     *LOVAL        SETLL     PRV01
     C     ASKY1         CHAIN     PRV01                              99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRN
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     RVRRN         ##RRN
     C                   ENDIF                                                  *IN99 = On
     C                   CALL      'F2366R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRN
     C*
     C     SCLAB         WHENEQ    '1'
     C*
     C                   MOVEL(P)  SCPRCD        ##HCPC
     C*** Beg Add *** 01/09/18 ******************************************
     C                   MOVEL     SCMOD1        MOD(1)
     C                   MOVEL     SCMOD2        MOD(2)
     C                   MOVEL     SCMOD3        MOD(3)
     C                   MOVEL     SCMOD4        MOD(4)
     C                   Z-ADD     1             I                 1 0
     C                   MOVEL     *BLANKS       ##MOD1
     C     I             DOWLE     4
     C     MOD(I)        IFNE      ' '
     C                   MOVEL     MOD(I)        ##MOD1
     C                   LEAVE
     C                   ENDIF
     C                   ADD       1             I
     C                   ENDDO
     C*** End Add *** 01/09/18 ******************************************
     C*** Beg Chg *** 01/09/18 ******************************************
     C***  *LOVAL        SETLL     PLB01
     C***  ASKY1         CHAIN     PLB01                              99
     C     RVKY1         SETGT     PLB01
     C     RVKY2         READPE    PLB01                                  99
     C*** End Chg *** 01/09/18 ******************************************
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRN
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     LBRRN         ##RRN
     C                   ENDIF                                                  *IN99 = On
     C                   CALL      'F2368R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRN
     C*
     C*
     C     SCAMB         WHENEQ    '1'
     C*
     C                   Z-ADD     SCFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         ##EFDT
     C*
     C                   MOVEL(P)  SCPRCD        ##HCPC
     C                   MOVEL     EOAST         KKST
     C                   MOVE      SCRGN#        KKLOC
     C*
     C     AMKY1         SETGT     PAM11
     C     AMKY2         READPE    PAM11                                  99
     C     *IN99         DOWEQ     *OFF
     C     AMSTAT        IFEQ      'I'
     C     AMKY2         READPE    PAM11                                  99
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRN
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     AMRRN         ##RRN
     C                   ENDIF
     C*
     C                   CALL      'F2369R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRN
     C*
     C**** Beg Add *** 04/04/16 ****************************************
     C     scRcF         WHENEQ    '1'
     C*
     C                   MOVEL     SCZIPA        KKZIPA
     C                   MOVEL(P)  SCPRCD        KKCPT4
     C                   MOVEL     KKFDAT        KKEFDT
     C     RCKY1         SETGT     PRC01
     C     RCKY2         READPE    PRC01                                  99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRN
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     RCRRN         ##RRN
     C                   ENDIF                                                  *IN99 = On
     C*
     C                   Call      'F2310R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRN
     C                   PARM                    ##ST
     C*
     C**** End Add *** 04/04/16 ****************************************
     C*
     C     SCDME         WHENEQ    '1'
     C     SCAST         IFEQ      'CT'
     C*** Beg Add *** 04/04/16 ****************************************
     C     xxMfs         OREQ      'C'
     C*** End Add *** 04/04/16 ****************************************
     C*** NOTE: below is the same logic from SCDMEF in PRCWDW  *******
     C                   MOVEL     SCMOD1        MOD(1)
     C                   MOVEL     SCMOD2        MOD(2)
     C                   MOVEL     SCMOD3        MOD(3)
     C                   MOVEL     SCMOD4        MOD(4)
     C                   MOVEL(P)  SCPRCD        ##HCPC
     C     ##MREL        IFEQ      '1'
     C                   MOVE      SVEFDT        ##EFDT
     C                   ELSE
     C                   MOVE      CAFDAT        ##EFDT
     C                   ENDIF                                                  ##MREL = '1'
     C*
     C                   MOVEL     ' '           XXDME             1
     C                   Z-ADD     0             I                 1 0
     C                   MOVEL     ' '           XXOUT             1
     C*
     C     *IN99         DOUEQ     *OFF
     C*
     C                   ADD       1             I
     C     I             IFLT      5
     C     MOD(I)        DOWEQ     '  '
     C                   ADD       1             I
     C     I             IFGT      4
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C                   ENDIF                                                  I < 5
     C*
     C     I             IFGE      5
     C                   LEAVE
     C                   ENDIF
     C*
     C                   Z-ADD     0             J                 1 0
     C     J             ADD       1             L                 1 0
     C*
     C     J             DOWNE     5
     C     L             ANDNE     5
     C                   ADD       1             J
     C     J             ADD       1             L
     C     MOD(J)        DOWEQ     '  '
     C                   ADD       1             J
     C     J             ADD       1             L
     C     J             IFGT      4
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     J             IFNE      I
     C*
     C     J             IFNE      5
     C     I             ANDNE     5
     C                   MOVEL     MOD(I)        MODI              2
     C                   MOVEL     MOD(J)        MODJ              2
     C*
     C     MODI          IFNE      ' '
     C     MODJ          ANDNE     ' '
     C                   MOVEL     MOD(I)        XXMOD1            2
     C                   MOVEL     MOD(J)        XXMOD2            2
     C                   ENDIF
     C*
     C     DMKY1         SETGT     PDM01                              99
     C     DMKY2         READPE    PDM01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DMRRN         ##RRNM
     C                   MOVEL     '  '          XXMOD2
     C                   MOVEL     'Y'           XXDME
     C                   CALL      'F2371R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRNM
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ENDIF                                                  I <> 5 & J <> 5
     C*
     C                   ENDIF                                                  I <> J
     C*
     C                   ENDDO                                                  J <> 5 & L <> 5
     C*
     C                   ENDDO                                                  I < 5
     C*
     C     XXDME         IFNE      'Y'
     C*
     C                   MOVEL     *BLANKS       XXMOD1
     C                   MOVEL     MOD(1)        MOD1              2
     C                   MOVEL     MOD(2)        MOD2              2
     C                   MOVEL     MOD(3)        MOD3              2
     C                   MOVEL     MOD(4)        MOD4              2
     C                   SELECT
     C     MOD1          WHENNE    ' '
     C     MOD2          ANDEQ     ' '
     C     MOD3          ANDEQ     ' '
     C     MOD4          ANDEQ     ' '
     C                   MOVEL     MOD1          XXMOD1
     C     MOD1          WHENEQ    ' '
     C     MOD2          ANDNE     ' '
     C     MOD3          ANDEQ     ' '
     C     MOD4          ANDEQ     ' '
     C                   MOVEL     MOD2          XXMOD1
     C     MOD1          WHENEQ    ' '
     C     MOD2          ANDEQ     ' '
     C     MOD3          ANDNE     ' '
     C     MOD4          ANDEQ     ' '
     C                   MOVEL     MOD3          XXMOD1
     C     MOD1          WHENEQ    ' '
     C     MOD2          ANDEQ     ' '
     C     MOD3          ANDEQ     ' '
     C     MOD4          ANDNE     ' '
     C                   MOVEL     MOD4          XXMOD1
     C                   ENDSL                                                  SELECT STMT
     C*
     C                   MOVEL     '  '          XXMOD2
     C*
     C     DMKY1         SETGT     PDM01                              99
     C     DMKY2         READPE    PDM01                                  99
     C     *IN99         DOWEQ     *OFF
     C     DESTAT        IFEQ      'I'
     C     DMKY2         READPE    PDM01                                  99
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DMRRN         ##RRNM
     C                   MOVEL     '  '          XXMOD2
     C                   MOVEL     'Y'           XXDME
     C                   CALL      'F2371R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRNM
     C                   ELSE
     C                   MOVEL     '  '          XXMOD1
     C                   MOVEL     '  '          XXMOD2
     C     DMKY1         SETGT     PDM01                              99
     C     DMKY2         READPE    PDM01                                  99
     C     *IN99         DOWEQ     *OFF
     C     DESTAT        IFEQ      'I'
     C     DMKY2         READPE    PDM01                                  99
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DMRRN         ##RRNM
     C                   MOVEL     '  '          XXMOD2
     C                   MOVEL     'Y'           XXDME
     C                   CALL      'F2371R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRNM
     C                   ELSE
     C                   Z-ADD     0             ##RRNM
     C*** DISPLAY FROM R & C BEG
     C     ##RC          IFEQ      ' '
     C                   MOVEL     SCZIPA        KKZIPA
     C                   MOVEL(P)  SCPRCD        KKCPT4
     C     ##MREL        IFEQ      '1'
     C                   MOVE      SVEFDT        ##EFDT
     C                   ELSE
     C                   MOVEL     CAFDAT        KKEFDT
     C                   ENDIF                                                  ##MREL    IFEQ '1'
     C     RCKY1         SETGT     PRC01
     C     RCKY2         READPE    PRC01                                  99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRNM
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     RCRRN         ##RRNM
     C                   ENDIF                                                  *IN99 = On
     C                   CALL      'F2371R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRNM
     C                   PARM                    ##ST
     C                   ELSE
     C     ##RC          IFEQ      'N'
     C                   EXSR      NOWDW2
     C                   ENDIF
     C                   ENDIF                                                  RCFP = 'Y'
     C*** DISPLAY FROM R & C END
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXDME <> 'Y'
     C*** NOTE: Above is the same logic from SCDMEF in PRCWDW  *******
     C                   ENDIF                                                  CT
     C*
     C                   ENDSL
     C                   ENDIF                                                  EODSTY = DR/DS
     C*** validate if new options have been entered.
     C                   SETON                                        90
     C*
     C     KIEND         ENDSR
     C*================================================================
     C* KJDO - F10=SEARCH.
     C*================================================================
     C     KJDO          BEGSR
     C*
     C*** INITIALIZE POSITION CURSOR INDICATORS
     C*
     C*
     C                   SELECT
     C*
     C*
     C*  COUNTY CODE SEARCH
     C*
     C     HFLD          WHENEQ    'SCCNTY'
     C     *IN24         ANDNE     *ON
     C*
     C                   CALL      'F2360R'
     C                   PARM      '2'           ##CALL
     C                   PARM      '0'           ##KC
     C                   PARM      '0'           ##KL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      EOAST         ##ST
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PCC02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C     CCSTAT        OREQ      'I'
     C*
     C     3             SUBST(P)  CCKEY:3       SCCNTY
     C                   SETON                                        9095
     C*
     C                   ELSE
     C                   SETON                                        9011
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C                   SETON                                        9011
     C*
     C                   ENDIF
     C*
     C*  ADJUDICATION CODE SEARCH
     C*
     C     HFLD          WHENEQ    'SCCSCD'
     C     *IN24         ANDNE     *ON
     C*
     C                   CALL      'F2300R'
     C                   PARM      '2'           ##CALL
     C                   PARM      '0'           ##KC
     C                   PARM      '0'           ##KL
     C                   PARM      *ZEROS        ##RRN
     C*
     C     ##KL          IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        9095      pos cursor
     C                   ENDIF                                                  ##KL IFEQ *ON
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PCS02                              99
     C                   MOVEL(P)  CSKEY         SCCSCD
     C                   MOVEL(P)  CSDESC        SCCSDS
     C                   SETON                                        9041      pos cursor
     C                   ENDIF                                                  ##RRN IFNE ZEROS
     C*
     C* PLACE OF SERVICE SEARCH
     C*
     C     HFLD          WHENEQ    'SCPOS'
     C                   MOVE      EOAST         ##AST
     C                   MOVE      *BLANKS       ##POS
     C                   MOVEL     SCPOS         HLPOS            15
     C                   CALL      'F2370R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##AST             2
     C                   PARM                    ##POS            15
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##POS         OREQ      *BLANKS
     C                   MOVEL(P)  HLPOS         SCPOS
     C                   SETON                                        9031      pos cursor
     C                   ELSE
 CCC C                   MOVEL     ##POS         TPPOS             3
 AAA C                   MOVE      TPPOS         SCPOS
 CCC C                   MOVE(P)   SCPOS         KKPOS
     C     PSKY          CHAIN     PPS01                              99
     C  N99              MOVEL(P)  PSDESC        SCPODS
     C                   SETON                                        9032      pos cursor
     C                   ENDIF                                                  ##KC IFEQ 1
     C*
     C*  TYPE OF SERVICE SEARCH
     C*
     C     HFLD          WHENEQ    'SCTOS'
     C                   MOVE      EOAST         ##AST
     C                   MOVEL     SCTOS         HLTOS
     C                   Z-ADD     0             ##RRN             9 0
     C                   CALL      'F2365R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM                    ##AST
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVEL     HLTOS         SCTOS
     C                   SETON                                        9032      pos cursor
     C                   ELSE
     C     ##RRN         CHAIN     PTS01                              99
     C  N99              MOVEL     TSKEY         XXTOS             3
     C  N99              MOVE      XXTOS         X1TOS             1
     C  N99              MOVEL(P)  X1TOS         SCTOS
     C  N99              MOVEL(P)  TSDESC        SCTODS
     C                   SETON                                        9054      pos cursor
     C                   ENDIF
     C*
     C* PROCEDURE CODE SEARCH
     C*
     C     HFLD          WHENEQ    'SCPRCD'
     C*
     C                   MOVE      SCPRCD        HLCPT4
     C                   MOVEL(P)  SCCDDS        HLDESC           40
     C*** Beg Add *** 06/21/12 ****************************************
      /Free

         If Eoast = 'HL';
             inCall= '2';
             inRrn = 0;
             inKc = *blanks;
             inKl = *blanks;

             Callp RtvPrcCd(inCall:inKc:inKl:inRrn);
               If inRrn <> 0;
                  Chain inRrn phl02;
                    If %found(Fmhlp);
                      scPrcd = H2Code;
                      scCdds = %subst(H2Desc:1 :25);
                      *in97 = *on;
                      *in90 = *on;
                    Else;
                      ScPrcd = HlCpt4;
                      scCdds = HlDesc;
                      *in54 = *on;
                      *in90 = *on;
                    Endif;

               Else;
                 scPrcd = HlCpt4;
                 scCdds = HlDesc;
                 *in54 = *on;
                 *in90 = *on;
               Endif;


         Else;
      /End-Free
     C*** End Add *** 06/21/12 ****************************************
     C                   MOVE      *BLANKS       ##PROC            5
     C                   MOVE      *BLANKS       ##PROD           50
       //*** Beg Add 08/06/25 **********************************************
       If EOdsTy = 'DR' or EOdsTy = 'DS';
          strMsg = 'Medicare or State Procedure Code Search';
          isMedicare = PopTwoBtn(strmsg:'Select Information':
           'Medicare':'State');
       Endif;
       //*** End Add 08/06/25 **********************************************
       //*** Beg Del 08/06/25 **********************************************
     C***  EODSTY        IFEQ      'DR'
     C***  EODSTY        OREQ      'DS'
     C***  XXMFS         OREQ      'Y'
       //*** End Del 08/06/25 **********************************************
     C*** Beg Add *** 07/26/12 ****************************************
       //*** Beg Del 08/06/25 **********************************************
     C***  EOAST         OREQ      'TN'
       //*** End Del 08/06/25 **********************************************
      /Free
       //*** Beg Add 08/06/25 **********************************************
       If ((EOdsTy = 'DR' or EOdsTy = 'DS') and isMedicare) or
        (xxMfs = 'Y' and EOast <> 'TN');
       //*** End Add 08/06/25 **********************************************
           If eoast = 'TN';
               *in54 = *on;
          Else;
             *in54 = *off;
          Endif;
      /End-free
     C*** End Add *** 07/26/12 ****************************************
      /Free
          //******Beg Add *** 11/19/13 ********************************
          if eoast = 'SC';
             ##ast = eoast;
          endif;
          //******End Add *** 11/19/13 ********************************
      /End-free
     C                   Z-ADD     @@RRN2        ##RRN1
     C                   CALL      'F2013R'
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN1
     C                   PARM                    ##PROC
     C                   PARM                    ##PROD
     C                   PARM      'E'           ##RTCD
     C                   PARM                    ##AST
     C*
     C                   ELSE
     C*
     C                   Z-ADD     0             ##RRN1            9 0
     C                   MOVE      SCAST         ##AST
     C                   CALL      'F2013R'
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN1
     C                   PARM                    ##PROC
     C                   PARM                    ##PROD
     C                   PARM      ' '           ##RTCD
     C                   PARM                    ##AST
     C                   ENDIF
     C*
     C     ##RTCD        IFEQ      '1'
     C     ##PROD        OREQ      *BLANKS
     C                   MOVEL     HLCPT4        SCPRCD
     C                   MOVEL(P)  HLDESC        SCCDDS
     C                   SETON                                        9054      pos cursor
     C                   ELSE
     C                   MOVEL     ##PROC        SCPRCD
     C                   MOVEL(P)  ##PROD        SCCDDS
     C                   SETON                                        9097
     C                   ENDIF                                                  ##RTCD = 1
     C*** End Add *** 06/21/12 ****************************************
      /Free
         Endif;
      /End-Free
     C*** End Add *** 06/21/12 ****************************************
     C*
     C* BILLED PROCEDURE CODE SEARCH
     C*
     C     HFLD          WHENEQ    'SCBLCD'
     C     EOAST         ANDEQ     'TX'
     C                   CALL      'F2532R'
     C                   PARM      '2'           ##CALL
     C                   PARM      '0'           ##KC
     C                   PARM      '0'           ##KL
     C                   PARM      EOAST         ##AST
     C                   PARM      *BLANKS       ##CPT4           15
     C                   PARM      *BLANKS       ##DESC           40
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##CPT4        OREQ      *BLANKS
     C                   SETON                                        9790      pos cursor
     C                   ELSE
     C                   MOVEL(P)  ##CPT4        SCBLCD
     C                   SETON                                        9034      pos cursor
     C                   ENDIF
     C*
     C*
     C* PROCEDURE MODIFIER SEARCH (for modifier #1)
     C*
     C     HFLD          WHENEQ    'SCMOD1'
     C                   MOVE      EOAST         ##AST
     C                   MOVE      *BLANKS       ##KEY
     C                   MOVE(P)   SCMOD1        HLMOD
     C                   CALL      'F2315R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##AST
     C                   PARM                    ##KEY            15
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##KEY         OREQ      *BLANKS
     C                   MOVE      HLMOD         SCMOD1
     C                   SETON                                        9034      pos cursor
     C                   ELSE
     C                   MOVEL     ##KEY         SCMOD1
     C                   SETON                                        9035      pos cursor
     C                   ENDIF
     C*
     C* PROCEDURE MODIFIER SEARCH (for modifier #2)
     C*
     C     HFLD          WHENEQ    'SCMOD2'
     C                   MOVE      EOAST         ##AST
     C                   MOVE      *BLANKS       ##KEY
     C                   MOVE(P)   SCMOD2        HLMOD
     C                   CALL      'F2315R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##AST
     C                   PARM                    ##KEY            15
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##KEY         OREQ      *BLANKS
     C                   MOVE      HLMOD         SCMOD2
     C                   SETON                                        9035      pos cursor
     C                   ELSE
     C                   MOVEL     ##KEY         SCMOD2
     C                   SETON                                        9036      pos cursor
     C                   ENDIF
     C*
     C* PROCEDURE MODIFIER SEARCH (for modifier #3)
     C*
     C     HFLD          WHENEQ    'SCMOD3'
     C                   MOVE      EOAST         ##AST
     C                   MOVE      *BLANKS       ##KEY
     C                   MOVE(P)   SCMOD3        HLMOD
     C                   CALL      'F2315R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##AST
     C                   PARM                    ##KEY            15
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##KEY         OREQ      *BLANKS
     C                   MOVE      HLMOD         SCMOD3
     C                   SETON                                        9036      pos cursor
     C                   ELSE
     C                   MOVEL     ##KEY         SCMOD3
     C                   SETON                                        9037      pos cursor
     C                   ENDIF
     C*
     C* PROCEDURE MODIFIER SEARCH (for modifier #4)
     C*
     C     HFLD          WHENEQ    'SCMOD4'
     C                   MOVE      EOAST         ##AST
     C                   MOVE      *BLANKS       ##KEY
     C                   MOVE(P)   SCMOD4        HLMOD
     C                   CALL      'F2315R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##AST
     C                   PARM                    ##KEY            15
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##KEY         OREQ      *BLANKS
     C                   MOVE      HLMOD         SCMOD4
     C                   SETON                                        9037      pos cursor
     C                   ELSE
     C                   MOVEL     ##KEY         SCMOD4
     C  N52              SETON                                        9038      pos cursor
     C   52              SETON                                        9053
     C                   ENDIF
     C*
    C***** Beg Add *** 11/02/15 **************************************
     C* NDC CODE SEARCH

     C     HFLD          WHENEQ    'SCNDC'
     C                   MOVEL     SCNDC         HLNDC            11
     C                   CALL      'F2350R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      *ZEROS        ##RRN
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVEL(P)  HLNDC         SCNDC
     C                   SETON                                        9005      pos cursor
     C                   ELSE
     C     ##RRN         CHAIN     PRX02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  RXNDC         SCNDC
     C                   MOVEL(P)  RXNAME        SCNDCD
     C                   SETON                                        9033      pos cursor
     C                   ENDIF
     C                   ENDIF
    C***** End Add *** 11/02/15 **************************************
     C*  BODY PART SEARCH
     C     HFLD          WHENEQ    'SCAREA'
     C                   MOVE      EOAST         ##AST
     C                   MOVE      *BLANKS       ##KEY
     C                   MOVEL     SCAREA        HLAREA           15
     C                   CALL      'F2325R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##AST
     C                   PARM                    ##KEY
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##KEY         OREQ      *BLANKS
     C                   MOVEL(P)  HLAREA        SCAREA
     C                   SETON                                        9039      pos cursor
     C                   ELSE
     C                   MOVEL(P)  ##KEY         SCAREA
     C                   MOVEL(P)  ##KEY         KKKEY
     C     TBKY          CHAIN     PPA01                              99
     C  N99              MOVEL(P)  PADESC        SCARDS
     C                   SETON                                        9055      pos cursor
     C                   ENDIF
     C*
     C* REDUCTIONS TYPE CODE SEARCHES
     C*
     C     HFLD          WHENEQ    'SCOT1C'
     C                   MOVEL(P)  SCOT1C        HLOT1C
     C                   CALL      'F2435R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      'O'           ##TYPE            1
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVEL(P)  HLOT1C        SCOT1C
     C                   SETON                                        9048
     C                   ENDIF                                                  ##KC = 1
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PBS02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  BSCODE        SCOT1C
     C                   SETON                                        9049
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRB NE *ZEROS
     C*
     C     HFLD          WHENEQ    'SCOT2C'
     C                   MOVEL(P)  SCOT2C        HLOT2C
     C                   CALL      'F2435R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      'O'           ##TYPE            1
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVEL(P)  HLOT2C        SCOT2C
     C                   SETON                                        9050
     C                   ENDIF                                                  ##KC = 1
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PBS02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  BSCODE        SCOT2C
     C                   SETON                                        90
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*** Beg Add *** 07/18/25 ***************************************
     C     HFLD          WHENEQ    'SCNPIR'
     C                   EVAL      MSNPIR = 'MCR0676'
     C                   EVAL      ERNPIR = *ON
     C                   EVAL      *IN90  = *ON
     C*** End Add *** 07/18/25 ***************************************
     C                   OTHER
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCFDAT'
     C                   SETOFF                                       91
     C                   SETON                                        929394
     C     HFLD          WHENEQ    'SCBLCD'
     C                   SETOFF                                       929394
     C                   SETON                                        91
     C     HFLD          WHENEQ    'SCDXR1'
     C                   SETOFF                                       919394
     C                   SETON                                        92
     C     HFLD          WHENEQ    'SCDXR2'
     C                   SETOFF                                       919294
     C                   SETON                                        93
     C     HFLD          WHENEQ    'SCDXR3'
     C                   SETOFF                                       919293
     C                   SETON                                        94
     C     HFLD          WHENEQ    'SCDXR4'
     C                   SETOFF                                       9394
     C                   SETON                                        9192
     C     HFLD          WHENEQ    'SCCHRG'
     C                   SETOFF                                       9194
     C                   SETON                                        9293
     C     HFLD          WHENEQ    'SCPAID'
     C                   SETOFF                                       9192
     C                   SETON                                        9394
     C     HFLD          WHENEQ    'SCMRA'
     C                   SETOFF                                       9293
     C                   SETON                                        9194
     C     HFLD          WHENEQ    'SCNTWK'
     C                   SETOFF                                       94
     C                   SETON                                        919293
     C     HFLD          WHENEQ    'SCOTHR'
     C                   SETOFF                                       92
     C                   SETON                                        919394
     C     HFLD          WHENEQ    'SCOTH2'
     C                   SETOFF                                       93
     C                   SETON                                        919294
     C     HFLD          WHENEQ    'SCUNTS'
     C                   SETON                                        919293    94
     C                   SETON                                        94
     C                   ENDSL
     C*
     C                   SETON                                        90
     C*
     C                   ENDSL
     C*
     C                   EXSR      DATSBR
     C                   SETON                                        70
     C*
     C                   ENDSR
     C*================================================================
     C* KKDO - F11=EOB/Notes
     C*================================================================
     C     KKDO          BEGSR
     C                   Z-ADD     0             ##RRN
     C                   Z-ADD     @@RRN2        ##RRN2            9 0          faeop
     C                   Z-ADD     @@RRN3        ##RRN3            9 0
     C*
     C     @@MODE        IFEQ      'C'
     C     @@MODE        OREQ      'A'
     C                   MOVE      ' '           ##CALL
     C                   ELSE
     C                   MOVE      '1'           ##CALL
     C                   ENDIF                                                  @@MODE = 'C'
     C                   CALL      'F0356R'
     C                   PARM                    ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM                    ##RRN2
     C                   PARM                    ##RRN3
     C                   SETON                                        90
     C                   ENDSR
     C*================================================================
     C* KNDO - F14=DARTS
     C*================================================================
     C     KNDO          BEGSR
     C*
     C* CALL API TO ACCESS DARTS
     C*
     C                   MOVE      EOCO#         #1CO#
     C                   MOVE      EOSUB#        #1SUB#
     C                   MOVE      EOFND         #1FND
     C                   MOVE      EOFYR         #1FYR
     C                   MOVE      EOCASE        #1CASE                   90
     C*
     C                   CALL      'FA703R'
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM                    ##WCIN            1
     C                   PARM                    ##WCDK            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE
     C                   PARM      *BLANK        #1KEY            40
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* KPDO - F15=DISPLAY LINES
     C*================================================================
     C     KPDO          BEGSR
     C                   Z-ADD     @@RRN2        ##RRN
     C                   CALL      'F0354R'
     C                   PARM      '1'           ##CALL
     C                   PARM                    ##RRN                          faeop
     C                   PARM      SCLIN#        ##LIN#
     C                   SETON                                        90
     C                   ENDSR
     C*================================================================
     C* KQDO - F16=ADL INFO
     C*================================================================
     C*
     C*
     C*
     C*================================================================
     C* KQDO - F16=ADL INFO
     C*================================================================
     C     KQDO          BEGSR
     C*
     C     EHFLIC        IFNE      *BLANKS
     C*
     C                   Z-ADD     0             Y                 5 0
     C     ' '           SCAN      EHFLIC        Y                        99
     C*
     C     *IN99         IFEQ      *ON
     C                   SUB       3             Y
     C                   ELSE
     C                   Z-ADD     11            Y
     C                   ENDIF                                                  *IN99 = *ON
     C*
     C     Y             SUBST     EHFLIC:1      XXLIC#           12
     C*
     C                   ENDIF                                                  EHFLIC <> ' '
     C*
     C                   CALL      'F8362R'
     C                   PARM      XXLIC#        ##LIC#           12
     C                   PARM      *BLANKS       ##RTCD            1
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* KSDO - F18=MEDICARE CODE INFO
     C*================================================================
     C     KSDO          BEGSR
     C*
     C                   MOVEL     SCPRCD        KKPRCD
     C                   Z-ADD     SCFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C*
     C     MCKY1         SETGT     PMC01
     C     MCKY2         READPE    PMC01                                  99
     C     *IN99         IFEQ      *OFF
     C*
     C                   CALL      'F2545R01'
     C     *INKC         PARM      ' '           ##KC              1
     C     *INKL         PARM      ' '           ##KL              1
     C                   PARM      'D'           ##MODE            1
     C                   PARM      DBRRMC        ##RRN             9 0
     C                   PARM      EOAST         ##ST              2
     C*
     C                   ENDIF
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* KYDO - F24=MORE KEYS
     C*================================================================
     C     KYDO          BEGSR
     C*
     C     *IN07         IFEQ      *OFF
     C                   SETON                                        07
     C                   ELSE
     C                   SETOFF                                       07
     C                   ENDIF
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   Z-ADD     0             SCEOB#
     C                   Z-ADD     0             SCLIN#
     C                   Z-ADD     0             SCSEQ#
     C                   MOVE      *BLANKS       SCCSCD
     C                   MOVE      *BLANKS       SCCSDS
     C                   Z-ADD     0             SCFDAT
     C                   MOVE      *ZEROS        SCPOS
     C                   MOVE      *BLANKS       SCPODS
     C                   MOVE      *BLANKS       SCTOS
     C                   MOVE      *BLANKS       SCTODS
     C                   MOVE      *BLANKS       SCPRCD
     C                   MOVE      *BLANKS       SCCDDS
     C                   MOVE      *BLANKS       SCBLCD
     C                   MOVE      *BLANKS       SCMOD1
     C                   MOVE      *BLANKS       SCMOD2
     C                   MOVE      *BLANKS       SCMOD3
     C                   MOVE      *BLANKS       SCMOD4
     C                   MOVE      *BLANKS       SCDXR1
     C                   MOVE      *BLANKS       SCDIA1
     C                   MOVE      *BLANKS       SCD1DS
     C                   MOVE      *BLANKS       SCDXR2
     C                   MOVE      *BLANKS       SCDIA2
     C                   MOVE      *BLANKS       SCD2DS
     C                   MOVE      *BLANKS       SCDXR3
     C                   MOVE      *BLANKS       SCDIA3
     C                   MOVE      *BLANKS       SCD3DS
     C                   MOVE      *BLANKS       SCDXR4
     C                   MOVE      *BLANKS       SCDIA4
     C                   MOVE      *BLANKS       SCD4DS
     C                   Z-ADD     0             SCCHRG
     C                   Z-ADD     0             SCPAID
     C                   Z-ADD     0             SCMRA
     C                   Z-ADD     0             SCNTWK
     C                   Z-ADD     0             SCOTHR
     C                   MOVE      *BLANKS       SCOT1C
     C                   Z-ADD     0             SCOTH2
     C                   MOVE      *BLANKS       SCOT2C
     C                   Z-ADD     1             SCUNTS
     C                   MOVE      *BLANKS       SCAREA
     C                   MOVE      *BLANKS       SCARDS
     C                   CLEAR                   SCEOB1
     C                   CLEAR                   SCEOB2
     C                   CLEAR                   SCEOB3
     C                   CLEAR                   SCEOB4
     C                   CLEAR                   SCNPIR
    C***** Beg Add *** 11/02/15 **************************************
     C                   MOVE      *BLANKS       SCNDC
     C                   MOVE      *BLANKS       SCNDCD
   C***** End Add *** 11/02/15 **************************************
     C*
     C                   MOVE      EOAST         SCAST
     C                   Z-ADD     EOEOB#        SCEOB#
     C                   MOVE      *BLANKS       SCCNTY
     C*
     C* SET LINE NUMBER
     C*
     C     @@MODE        IFEQ      'A'
     C                   Z-ADD     *HIVAL        KKLIN#
     C     CAKY4         SETGT     PCA04
     C     CAKY3         READPE    PCA04                                  99
     C     *IN99         IFEQ      *OFF
     C     CALIN#        ADD       1             SCLIN#
     C                   ELSE
     C                   Z-ADD     1             SCLIN#
     C                   ENDIF
     C*
     C* SET DATE ENTERED
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SCEDAT
     C*
     C* DEFAULT CLAIMS STATUS CODE FROM FAEOP
     C*
     C                   MOVEL     EOCSCD        SCCSCD
     C   24              MOVEL     '300'         SCCSCD
     C*
     C* DEFAULT 1ST DIAGNOSIS CROSS REFERENCE CODE IF ONLY ONE
     C* DIAGNOSIS CODE IS ENTERED.
     C*
     C   52              MOVE      'X'           SCDXR1
     C                   ENDIF                                                  @@MODE = 'A'
     C*
     C* FROM FAEHP
     C*
     C     EHKY          CHAIN(N)  PEH01                              99
     C     *IN99         IFEQ      *OFF
      *** Beg Add *** 02/04/14 ****************************************
      /free
       If EHicdind <> 'Y';
       //If ICD9 code is selected
      /end-free
      *** END Add *** 02/04/14 ****************************************
     C                   MOVEL(P)  EHDIA1        SCDIA1
     C                   MOVEL(P)  EHDIA2        SCDIA2
     C                   MOVEL(P)  EHDIA3        SCDIA3
     C                   MOVEL(P)  EHDIA4        SCDIA4
      *** Beg Add *** 02/04/14 ****************************************
      /free
       Else;
       //If ICD10 code is selected
          SCDIA1 = EHICD1001;
          SCDIA2 = EHICD1002;
          SCDIA3 = EHICD1003;
          SCDIA4 = EHICD1004;
       Endif;
      /end-free
      *** END Add *** 02/04/14 ****************************************
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C*
     C* GET DIAGNOSIS CODES DESCRIPTIONS
     C*
      *** Beg Add *** 02/04/14 ****************************************
      /free
       If EHicdind <> 'Y';
      /end-free
      *** END Add *** 02/04/14 ****************************************
     C                   MOVE      'D'           KKLOST
     C*
     C     SCDIA1        IFNE      *BLANKS
     C*
     C*** Beg Chg *** 07/11/14 ****************************************
     C*
     C***********        MOVE(P)   SCDIA1        KKICD9
     C                   MOVEL(P)  SCDIA1        KKICD9
     C*
     C*** End Chg *** 07/11/14 ****************************************
     C*
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCD1DS
     C                   ENDIF                                                  SCDIA1 IFNE *BLANKS
     C*
     C     SCDIA2        IFNE      *BLANKS
     C*
     C*** Beg Chg *** 07/11/14 ****************************************
     C*
     C***********        MOVE(P)   SCDIA2        KKICD9
     C                   MOVEL(P)  SCDIA2        KKICD9
     C*
     C*** End Chg *** 07/11/14 ****************************************
     C*
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCD2DS
     C                   ENDIF                                                  SCDIA2 IFNE *BLANKS
     C*
     C     SCDIA3        IFNE      *BLANKS
     C*
     C*** Beg Chg *** 07/11/14 ****************************************
     C*
     C***********        MOVE(P)   SCDIA3        KKICD9
     C                   MOVEL(P)  SCDIA3        KKICD9
     C*
     C*** End Chg *** 07/11/14 ****************************************
     C*
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCD3DS
     C                   ENDIF                                                  SCDIA3 IFNE *BLANKS
     C*
     C     SCDIA4        IFNE      *BLANKS
     C*
     C*** Beg Chg *** 07/11/14 ****************************************
     C*
     C***********        MOVE(P)   SCDIA4        KKICD9
     C                   MOVEL(P)  SCDIA4        KKICD9
     C*
     C*** End Chg *** 07/11/14 ****************************************
     C*
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCD4DS
     C                   ENDIF                                                  SCDIA4 IFNE *BLANKS
      *** Beg Add *** 02/04/14 ****************************************
      /free
       Else;
          If scDia1 <> *blanks;
             kkICD10 = scDia1;
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   SCD1DS = ICdsc1;
                Endif;
          Endif;
          If scDia2 <> *blanks;
             kkICD10 = scDia2;
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   SCD2DS = ICdsc1;
                Endif;
          Endif;
          If scDia3 <> *blanks;
             kkICD10 = scDia3;
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   SCD3DS = ICdsc1;
                Endif;
          Endif;
          If scDia4 <> *blanks;
             kkICD10 = scDia4;
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   SCD4DS = ICdsc1;
                Endif;
          Endif;
       Endif;
      /end-free
      *** END Add *** 02/04/14 ****************************************
     C*
     C     @@MODE        IFEQ      'A'                                          ADD
     C*
     C* DETERMINE DETAIL CHARGE SEQUENCE NUMBER
     C*
     C                   Z-ADD     *HIVAL        KKDSEQ
     C*
     C     CAKY          SETGT     PCA01
     C     CAKY1         READPE    PCA01                                  99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     1             SCSEQ#
     C                   ELSE
     C     CADSEQ        ADD       1             SCSEQ#
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C*
     C                   ENDIF                                                  @@MODE IFEQ 'A'
     C*
     C                   MOVEL(P)  SCCSCD        KKKEY
     C     TBKY          CHAIN     PCS01                              99
     C  N99              MOVEL(P)  CSDESC        SCCSDS
     C*
     C                   MOVE      *BLANKS       SCRGN#
     C*
     C     @@MODE        IFNE      'A'
     C*
     C     *IN51         IFEQ      *OFF
     C*
     C                   MOVE      *BLANKS       SCRGN#
     C*
     C                   Z-ADD     CAFDAT        KKEFDT
     C                   MOVEL     EOFLIC        KKFZCD
     C*
     C     ZRKY          SETGT     PZR01                              99
     C                   READP     PZR01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   Z-ADD     ZREFDT        Z1EFDT
     C*
     C     KKFZCD        IFGE      ZRFZCD
     C     KKFZCD        ANDLE     ZRTZCD
     C     ZREFDT        ANDEQ     Z1EFDT
     C*
     C                   MOVE(P)   ZRRGN#        SCRGN#
     C                   LEAVE
     C*
     C                   ENDIF
     C*
     C                   READP     PZR01                                  99
     C*
     C                   ENDDO
     C*
     C*
     C* IF ZIP CODE IS NOT IN THE REGION FILE SET THE STATE DEFAULTS
     C*
     C     SCRGN#        IFEQ      *BLANKS
     C*
     C                   SELECT
     C     SCAST         WHENEQ    'PA'
     C                   MOVE(P)   3             SCRGN#
     C     SCAST         WHENEQ    'TX'
     C                   MOVE(P)   99            SCRGN#
     C     SCAST         WHENEQ    'FL'
     C                   MOVE(P)   99            SCRGN#
     C     SCAST         WHENEQ    'NY'
     C                   MOVE(P)   1             SCRGN#
     C                   OTHER
     C                   MOVE      *BLANKS       SCRGN#
     C                   ENDSL
     C*
     C                   ENDIF
     C*
     C* FOR NY PARTICIPANTS IN TUPP SET REGION = 4 (HIGHEST)
     C*
     C     SCAST         IFEQ      'NY'
     C     5             SUBST(P)  P1CNTY:1      XXSMSA            5
     C     XXSMSA        IFEQ      'TUPP '
     C                   MOVE(P)   4             SCRGN#
     C                   ENDIF
     C                   ENDIF
     C     SCAST         IFEQ      'FL'
     C     SCRGN#        ANDEQ     '  099'
     C                   MOVE      '01/02'       SCRGN#
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN51 =
     C*
     C* FROM FDDHP
     C*
     C                   MOVE(P)   DHPOS         SCPOS
     C                   MOVE(P)   SCPOS         KKPOS
     C     PSKY          CHAIN     PPS01                              99
     C  N99              MOVEL(P)  PSDESC        SCPODS
     C*
     C                   MOVEL(P)  DHTOS         SCTOS
     C                   MOVEL(P)  EOAST         HLTOS
     C                   MOVEL     DHTOS         H1TOS             1
     C                   MOVE      H1TOS         HLTOS
     C                   MOVEL(P)  HLTOS         KKKEY
     C     TBKY          CHAIN     PTS02                              99
     C  N99              MOVEL(P)  TSDESC        SCTODS
     C*
     C                   MOVEL(P)  DHPRCD        SCPRCD
    C***** Beg Add *** 11/02/15 **************************************
     C     ##CLT#        IFEQ      '136'
     C     SCPRCD        ANDEQ     *Blanks
     C                   MOVEL(P)  *Blanks       SCCDDS
     C                   ELSE
    C***** End Add *** 11/02/15 **************************************
     C                   MOVEL(P)  DHPRDS        SCCDDS
    C***** Beg Add *** 11/02/15 **************************************
     C                   ENDIF
    C***** End Add *** 11/02/15 **************************************
     C                   MOVEL(P)  DHMOD1        SCMOD1
     C                   MOVEL(P)  DHMOD2        SCMOD2
     C                   MOVEL(P)  DHMOD3        SCMOD3
     C                   MOVEL(P)  DHMOD4        SCMOD4
     C                   MOVE      DHDXR1        SCDXR1
     C                   MOVE      DHDXR2        SCDXR2
     C                   MOVE      DHDXR3        SCDXR3
     C                   MOVE      DHDXR4        SCDXR4
     C                   Z-ADD     DHUNTS        SCUNTS
     C*
     C                   MOVEL     DHAREA        SCAREA
     C                   MOVEL(P)  SCAREA        KKKEY
     C     TBKY          CHAIN     PPA01                              99
     C  N99              MOVEL(P)  PADESC        SCARDS
     C                   MOVEL     DHNPIR        SCNPIR
     C*
     C                   EXSR      NXTSEQ
     C                   Z-ADD     ZZDSEQ        SVDSEQ
     C                   Z-ADD     ZZLIN#        SVLIN#
     C*
    C***** Beg Add *** 11/02/15 **************************************
     C                   MOVEL(P)  DHNDC         SCNDC
     C                   MOVEL(P)  DHNDCD        SCNDCD
    C***** End Add *** 11/02/15 **************************************
     C* GET INFO FROM FDCAP
     C*
     C     @@RRN3        CHAIN(N)  PCA02                              99
     C     *IN99         IFEQ      *OFF
     C*
     C* FIND OUT IF MULTI-LINE OR NOT
     C*
     C*
     C* Save recon
     C*
     C                   Z-ADD     CARCN         SVRCN
     C*
     C* DEFAULT CLAIM STATUS CODE TO 400 IF THIS IS A RECON AND
     C* TRYING TO CHANGE A LINE ITEM ALREADY IN HISTORY (46 = ON)
     C*
     C     *IN46         IFEQ      *ON
     C                   MOVEL     '400'         SCCSCD
     C                   ELSE
     C                   MOVEL     CACSCD        SCCSCD
     C                   ENDIF
     C*
     C                   Z-ADD     CAFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCFDAT
     C*
     C                   MOVEL     DHBPRC        SCBLCD
     C                   Z-ADD     CAIDT         SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEDAT
     C*
     C                   Z-ADD     CACHRG        SCCHRG
     C                   Z-ADD     CAOTHR        SCOTHR
     C                   Z-ADD     CAOTH2        SCOTH2
     C                   Z-ADD     CANDIS        SCNTWK
     C                   Z-ADD     CAMRA         SCMRA
     C                   Z-ADD     CAPAID        SCPAID
     C                   MOVE      CAOT1C        SCOT1C
     C                   MOVE      CAOT2C        SCOT2C
     C                   Z-ADD     CALIN#        SCLIN#
     C                   Z-ADD     CADSEQ        SCSEQ#
     C                   ENDIF                                                  *IN99 IFEQ *OFF
     C*
     C* GET PROCEDURE CODE DESCRIPTION
     C*
     C*
     C* DISPLAY EOB CODES FOR BILL
     C*
     C     @@MODE        IFNE      'A'
     C                   Z-ADD     0             XXSEQ#            2 0
     C     DEKY          CHAIN(N)  PDE01                              99
     C     *IN99         DOWEQ     *OFF
     C*** Beg Chg *** 07/18/23 **************************************
     C**** DEEOB         IFNE      *BLANKS
     C     DEEOBCD       IFNE      *BLANKS
     C*** End Chg *** 07/18/23 **************************************
     C                   ADD       1             XXSEQ#
     C*** Beg Del *** 07/18/23 **************************************
     C****               SELECT
     C**** XXSEQ#        WHENEQ    1
     C****               MOVEL     DEEOB         SCEOB1
     C**** XXSEQ#        WHENEQ    2
     C****               MOVEL     DEEOB         SCEOB2
     C**** XXSEQ#        WHENEQ    3
     C****               MOVEL     DEEOB         SCEOB3
     C**** XXSEQ#        WHENEQ    4
     C****               MOVEL     DEEOB         SCEOB4
     C****               ENDSL
     C*** End Del *** 07/18/23 **************************************
     C*** Beg Add *** 07/18/23 **************************************
     C                   SELECT
     C     XXSEQ#        WHENEQ    1
     C                   MOVEL     DEEOBCD       SCEOB1
     C     XXSEQ#        WHENEQ    2
     C                   MOVEL     DEEOBCD       SCEOB2
     C     XXSEQ#        WHENEQ    3
     C                   MOVEL     DEEOBCD       SCEOB3
     C     XXSEQ#        WHENEQ    4
     C                   MOVEL     DEEOBCD       SCEOB4
     C     XXSEQ#        WHENEQ    5
     C                   MOVEL     DEEOBCD       SCEOB5
     C     XXSEQ#        WHENEQ    6
     C                   MOVEL     DEEOBCD       SCEOB6
     C     XXSEQ#        WHENEQ    7
     C                   MOVEL     DEEOBCD       SCEOB7
     C     XXSEQ#        WHENEQ    8
     C                   MOVEL     DEEOBCD       SCEOB8
     C     XXSEQ#        WHENEQ    9
     C                   MOVEL     DEEOBCD       SCEOB9
     C     XXSEQ#        WHENEQ    10
     C                   MOVEL     DEEOBCD       SCEOB10
     C     XXSEQ#        WHENEQ    11
     C                   MOVEL     DEEOBCD       SCEOB11
     C     XXSEQ#        WHENEQ    12
     C                   MOVEL     DEEOBCD       SCEOB12
     C                   ENDSL
     C*** End Add *** 07/18/23 **************************************
     C                   ENDIF                                                  DEEOB NE *BLANKS
     C     DEKY          READE(N)  PDE01                                  99
     C                   ENDDO
     C                   MOVEL     CAZIPA        SCZIPA
     C                   MOVEL     CAZIPA        SCZIPB
     C                   ELSE
     C                   MOVEL     *BLANKS       SCZIPA
     C                   MOVEL     *BLANKS       SCZIPB
     C                   ENDIF                                                  @@MODE = 'C'
     C*
     C* DISPLAY RULE NUMBER
     C*
     C                   Z-ADD     CARULE        SCRULE
     C     SCRULE        IFNE      *ZEROS
     C                   SETON                                        56
     C                   ELSE
     C                   SETOFF                                       56
     C                   ENDIF                                                  SCRULE NE *ZEROS
     C*
     C                   ENDIF                                                  @@MODE IFNE 'A'
     C     P1KY          CHAIN(N)  PP101                              99
     C     *IN99         IFEQ      *OFF
     C     3             SUBST(P)  P1CNTY:6      SCCNTY
     C                   ENDIF
     C*
     C* CHECK TO SEE IF LINES SPLIT OR NOT
     C*
     C                   CALL      'SA014R'
1    C                   PARM      EOAST         ##ST              2
2    C                   PARM      EOBTYP        ##BTYP            1
3    C                   PARM      CAFDAT        ##FDAT            7 0
4    C                   PARM      *BLANK        ##SPLT            1
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C* LOAD FIELDS FOR FDDHP
     C*
 CCC C                   MOVE(P)   SCPOS         DHPOS
     C                   MOVEL(P)  SCTOS         DHTOS
     C                   MOVEL(P)  SCPRCD        DHPRCD
     C                   MOVEL(P)  SCCDDS        DHPRDS
     C                   MOVEL(P)  SCMOD1        DHMOD1
     C                   MOVEL(P)  SCMOD2        DHMOD2
     C                   MOVEL(P)  SCMOD3        DHMOD3
     C                   MOVEL(P)  SCMOD4        DHMOD4
     C                   MOVE      SCDXR1        DHDXR1
     C                   MOVE      SCDXR2        DHDXR2
     C                   MOVE      SCDXR3        DHDXR3
     C                   MOVE      SCDXR4        DHDXR4
    C***** Beg Add *** 11/02/15 **************************************
     C                   MOVEL(P)  SCNDC         DHNDC
     C                   MOVEL(P)  SCNDCD        DHNDCD
    C***** End Add *** 11/02/15 **************************************
     C                   MOVE      XXRCFL        DHRCFL
     C                   MOVE      EHPPLN        DHPPLN
     C* If billed code is blank then load paid code to billed code
     C     SCBLCD        IFEQ      *BLANKS
     C                   MOVEL(P)  SCPRCD        DHBPRC
     C                   MOVEL(P)  SCCDDS        DHBPRD
     C*
     C                   ELSE
     C                   MOVEL(P)  SCBLCD        DHBPRC
     C                   MOVEL(P)  XXBPRD        DHBPRD
     C*
     C                   ENDIF                                                  SCBLCD = BLANKS
     C                   Z-ADD     SCFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDAT            7 0
     C*
     C*** Beg Add *** 05/19/15 ***************************************
     C* Do not adjust units if this is a true reconsideration
     C     svRCN         IfEq      EOrcn
     C*
     C*** End Add *** 05/19/15 ***************************************
     C* If anes or equipment code then retain screen units else set=1
     C     SCPRCD        IFGE      '00100'
     C     SCPRCD        ANDLE     '01999'
     C     SCPRCD        ORGE      'D0001'
     C     SCPRCD        ANDLE     'D9999'
     C     SV1A2         OREQ      'Y'
     C* Include anesthesia by a surgeon for Oklahoma
     C     SCAREA        ANDNE     'AN'
     C     SCAST         ANDNE     'OK'
     C*
     C     SV1A2         OREQ      'Y'
     C     SCAREA        ANDEQ     'AN'
     C     SCAST         ANDEQ     'OK'
     C* Include mileage for ambulance travel, etc
     C*
     C     SV1A2         OREQ      'M'
     C* Include rental for HCPCS codes
     C*
     C     SV1A2         OREQ      'R'
     C* Include hourly fees
     C*
     C     SV1A2         OREQ      'H'
     C*
     C* Include HCPCS codes that are not mileage,rental, or hourly but
     C* still don't want to break out because they are purchased in
     C* large quantities.
     C*
     C     SV1A2         OREQ      'U'
      **** BEGIN ADD *** 02/28/09 *************************************
     C     ##SPLT        OREQ      'N'
      **** END ADD   *** 02/28/09 *************************************
      **** BEGIN ADD *** 08/19/11 *************************************
     C     MEDUNT        OREQ      'Y'
      **** END ADD   *** 08/19/11 *************************************
     C*
     C                   Z-ADD     SCUNTS        DHUNTS
     C*
     C                   ELSE
     C                   Z-ADD     1             DHUNTS
     C*
     C                   ENDIF                                                  SCPRCD GE 00100
     C*
     C*** Beg Add *** 05/19/15 ***************************************
     C                   EndIf
     C*** End Add *** 05/19/15 ***************************************
     C*
     C* UNITS CANNOT BE = 0, DEFAULT TO 1.
     C*
     C     DHUNTS        IFEQ      0
     C                   Z-ADD     1             DHUNTS
     C                   ENDIF
     C*
     C                   MOVEL(P)  SCAREA        DHAREA
     C                   MOVEL     SCNPIR        DHNPIR
     C*
     C* LOAD FIELDS FOR FDCAP
     C*
     C                   Z-ADD     SCFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CAFDAT
     C                   MOVEL     SRDTO         CATDAT
     C*
     C                   MOVEL     SCPRCD        CACPT4
     C                   MOVEL(P)  DHPRCD        CACODE
     C*
     C                   SELECT
     C     SCMOD1        WHENNE    *BLANKS
     C                   MOVE      SCMOD1        CAPMOD
     C     SCMOD2        WHENNE    *BLANKS
     C                   MOVE      SCMOD2        CAPMOD
     C     SCMOD3        WHENNE    *BLANKS
     C                   MOVE      SCMOD3        CAPMOD
     C     SCMOD4        WHENNE    *BLANKS
     C                   MOVE      SCMOD4        CAPMOD
     C                   OTHER
     C                   MOVE      *BLANKS       CAPMOD
     C                   ENDSL
     C*
     C                   Z-ADD     SCUNTS        CAUNTS
     C*
     C*
     C                   Z-ADD     SCCHRG        CACHRG
     C*
     C* This code will load the correct charged amount if multiple
     C* units were entered for this line item.
     C*
     C     @@MODE        IFEQ      'C'
     C     SCUNTS        ANDGT     1
     C     SV1A2         ANDEQ     *BLANK
     C     SVRCN         ANDEQ     EORCN
     C     ##SPLT        ANDNE     'N'
      **** BEGIN ADD *** 08/19/11 *************************************
     C     MEDUNT        ANDEQ     *BLANK
      **** END ADD   *** 08/19/11 *************************************
     C*
     C     @@MODE        OREQ      'C'
     C     SCUNTS        ANDGT     1
     C     SV1A2         ANDEQ     'T'
     C     SVRCN         ANDEQ     EORCN
     C     ##SPLT        ANDNE     'N'
     C*
     C                   Z-ADD     SVCHRG        CACHRG
     C*** Beg Add *** 01/18/16 ****************************************
     C                   If        CSovrd = 'Y' and CSovrp = 0
     C                   Eval      CApaid = svPaid
     C                   Eval      CAmra  = svMRA
     C                   Eval      CAndis = svNdis
     C                   Eval      CAothr = svOthr
     C                   Eval      CAOth2 = svOth2
     C                   Eval      isOvrd = 'Y'
     C                   Else
     C                   Eval      isOvrd = 'N'
     C                   Endif
     C*
     C                   Else
     C                   Eval      isOvrd = 'N'
     C*** End Add *** 01/18/16 ****************************************
     C*
     C                   ENDIF
     C*
     C                   MOVEL     EOSS#         CASS#
     C                   MOVEL     EOACDT        CAACDT
     C*** Beg Add *** 01/18/16 ****************************************
     C                   If        isOvrd = 'N'
     C                   Z-ADD     SCOTHR        CAOTHR
     C                   Z-ADD     SCOTH2        CAOTH2
     C                   Z-ADD     SCNTWK        CANDIS
     C                   Z-ADD     SCMRA         CAMRA
     C                   Z-ADD     SCPAID        CAPAID
     C                   Endif
     C*** End Add *** 01/18/16 ****************************************
     C*** Beg Del *** 01/18/16 ****************************************
     C***************
     C***************    Z-ADD     SCOTHR        CAOTHR
     C***************    Z-ADD     SCOTH2        CAOTH2
     C***************    Z-ADD     SCNTWK        CANDIS
     C***************    Z-ADD     SCMRA         CAMRA
     C*** End Del *** 01/18/16 ****************************************
     C                   MOVE      SCZIPA        CAZIPA
     C                   MOVE(P)   SCAST         CAAST
     C                   MOVEL(P)  SCAREA        CAPART
     C*
     C*** Beg Del *** 01/18/16 ****************************************
     C****************   Z-ADD     SCPAID        CAPAID
     C*** End Del *** 01/18/16 ****************************************
     C                   MOVEL(P)  SCCSCD        CACSCD
     C                   MOVEL(P)  SCOT1C        CAOT1C
     C                   MOVEL(P)  SCOT2C        CAOT2C
     C*
     C     SCDXR1        IFEQ      'X'
     C                   MOVEL(P)  SCDIA1        CAICD9
     C                   ENDIF
     C     SCDXR2        IFEQ      'X'
     C                   MOVEL(P)  SCDIA2        CAICD9
     C                   ENDIF
     C     SCDXR3        IFEQ      'X'
     C                   MOVEL(P)  SCDIA3        CAICD9
     C                   ENDIF
     C     SCDXR4        IFEQ      'X'
     C                   MOVEL(P)  SCDIA4        CAICD9
     C                   ENDIF
     C*
     C                   MOVEL     XXSRC         CASRC
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* NXTSEQ - Find next available sequence numbers
     C*================================================================
     C*
     C     NXTSEQ        BEGSR
     C*
     C                   Z-ADD     *HIVAL        KKLIN#
     C*
     C     CAKY4         SETGT     PCA04
     C     CAKY3         READPE    PCA04                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C     CALIN#        ADD       1             ZZLIN#
     C*
     C                   ELSE
     C                   Z-ADD     1             ZZLIN#
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   Z-ADD     KKDSEQ        HDDSEQ
     C                   Z-ADD     *HIVAL        KKDSEQ
     C*
     C     CAKY          SETGT     PCA01
     C     CAKY1         READPE    PCA01                                  99
     C*
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     1             ZZDSEQ
     C*
     C                   ELSE
     C     CADSEQ        ADD       1             ZZDSEQ
     C*
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C                   Z-ADD     HDDSEQ        KKDSEQ
     C*
     C                   ENDSR
     C*
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
     C* INITIALIZE INDICATORS FOR ERROR CHECKING: THESE ARE INDICATORS
     C* BEYOND 71 - 89 RESERVED BY COPYBOOK. THESE SHOULD BE SET OFF
     C* IN THE 'PGMEND' SUBROUTINE ALSO.
     C*
     C                   SETOFF                                       606162
     C                   SETOFF                                       636465
     C                   SETOFF                                       666758
     C                   SETOFF                                       703042
     C                   SETOFF                                       5728
     C*
     C*** INITIALIZE POSITION CURSOR INDICATORS
     C*
     C                   SETOFF                                       313233
     C                   SETOFF                                       343536
     C                   SETOFF                                       373839
     C                   SETOFF                                       4041
     C                   SETOFF                                       484950
     C                   SETOFF                                       535455
     C                   SETOFF                                       919293
     C                   SETOFF                                       949511
     C*
     C* Keep the cursor positioned on current field if F24=More keys
     C* is used.
     C*
     C     *INKG         IFEQ      *ON
     C     *INKI         OREQ      *ON
     C     *INKK         OREQ      *ON
     C     *INKN         OREQ      *ON
     C     *INKP         OREQ      *ON
     C     *INKQ         OREQ      *ON
     C     *INKR         OREQ      *ON
     C     *INKY         OREQ      *ON
     C                   SELECT
     C     HFLD          WHENEQ    'SCCNTY'
     C                   SETON                                        11
     C     HFLD          WHENEQ    'SCCSCD'
     C                   SETON                                        95
     C     HFLD          WHENEQ    'SCFDAT'
     C                   SETON                                        41
     C     HFLD          WHENEQ    'SCPOS'
     C                   SETON                                        31
     C     HFLD          WHENEQ    'SCTOS'
     C                   SETON                                        32
     C     HFLD          WHENEQ    'SCPRCD'
     C                   SETON                                        54
     C     HFLD          WHENEQ    'SCBLCD'
     C                   SETON                                        97
     C     HFLD          WHENEQ    'SCMOD1'
     C                   SETON                                        34
     C     HFLD          WHENEQ    'SCMOD2'
     C                   SETON                                        35
     C     HFLD          WHENEQ    'SCMOD3'
     C                   SETON                                        36
     C     HFLD          WHENEQ    'SCMOD4'
     C                   SETON                                        37
     C     HFLD          WHENEQ    'SCDXR1'
     C                   SETON                                        38
     C     HFLD          WHENEQ    'SCCHRG'
     C                   SETON                                        53
     C     HFLD          WHENEQ    'SCPAID'
     C                   SETON                                        33
     C     HFLD          WHENEQ    'SCAREA'
     C                   SETON                                        39
     C     HFLD          WHENEQ    'SCMRA'
     C                   SETON                                        55
     C                   ENDSL
     C                   ENDIF
     C*
     C*
     C     *INKA         CASEQ     *ON           KADO                           F1=Repeat
     C     *INKB         CASEQ     *ON           KBDO                           F02=Process
     C     *INKG         CASEQ     *ON           KGDO                           F07=Header info
     C     *INKI         CASEQ     *ON           KIDO                           F09=Code info
     C     *INKJ         CASEQ     *ON           KJDO                           F10=Search
     C     *INKK         CASEQ     *ON           KKDO                           F11=EOB/Notes
     C     *INKN         CASEQ     *ON           KNDO                           F14=DARTS
     C     *INKP         CASEQ     *ON           KPDO                           F15=Previous lines
     C     *INKQ         CASEQ     *ON           KQDO                           F16=ADL Info
     C     *INKS         CASEQ     *ON           KSDO                           F18=Medicare Code
     C     *INKY         CASEQ     *ON           KYDO                           F24=More Keys
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C     ##CLT#        IFEQ      '114'
     C                   MOVEL(P)  SCCSCD        KKKEY
     C     FNKY          CHAIN     PFN01                              99
     C     *IN99         IFEQ      *OFF
     C     FNNWCG        IFEQ      'Y'
     C     TBKY          CHAIN     PCS01                              99
     C     *IN99         IFEQ      *OFF
     C     CSOVRD        IFEQ      'Y'
     C     CSOVRP        ANDEQ     0
     C     EORCN         ANDNE     CARCN
     C*
     C                   CALL      'FA770R'
     C                   PARM                    @@RRN3
     C                   PARM      'D'           @1MODE            1
     C*
     C                   ENDIF                                                  CSOVRD = 'Y'
     C                   ENDIF                                                  FNNWCG = 'Y'
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  CLT = '114'
     C     @@MODE        IFEQ      'C'
     C     *INKL         ANDEQ     *OFF
     C                   EXSR      KBDO
     C                   ENDIF
     C*
     C                   SETOFF                                       606162
     C                   SETOFF                                       636465
     C                   SETOFF                                       666758
     C                   SETOFF                                       703042
     C                   SETOFF                                       5728
     C*
     C* Close state specific files that are overridden
     C*
     C* Delete overrides,
     C*
      *** Beg Chg *** 08/24/16 ****************************************
     C**** *IN05         IFEQ      *ON
     C     inFMMCP       IFEQ      '1'
      *** End Chg *** 08/24/16 ****************************************
     C*
     C                   CALL      'QCMDEXC'                              99
     C                   PARM                    QCA(7)
     C                   PARM      56            LENGTH           15 5
     C*
     C                   CLOSE     FMMCL1
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(8)
     C                   PARM      56            LENGTH           15 5
     C*
     C                   ENDIF
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*
     C                   CLOSE     FMZRL1                               99
     C*
     C                   CLOSE     FTABL
     C                   CLOSE     FMPRL
     C                   CLOSE     FTPSL
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C                   OPEN      FTABL
     C                   OPEN      FMPRL
     C                   OPEN      FTPSL
     C*
     C                   CALL      'SA009R'
     C                   PARM      *ZEROS        ##EOBD            3 0          EOMB DELETE SECURITY
     C                   PARM      *BLANKS       ##EOBI            1            EOMB INTERACIVE PROC
     C                   PARM      *BLANKS       ##ARIN            1            AUDIT/RECON INSTALLE
     C                   PARM      *BLANKS       ##ACIS            1            AUTO CHECK ISSUANCE
     C                   PARM      *BLANKS       ##PAY0            1            PRINT WCMC EOB PAY>0
     C                   PARM      *BLANKS       ##SDTS            1            ALLOW FUTURE SERV DT
     C*
     C** Get standard screen headings
     C*
     C     @@RRN2        CHAIN(N)  PEO01                              99
     C                   MOVE      EOCO#         #1CO#
     C                   MOVE      EOSUB#        #1SUB#
     C                   MOVE      EOFND         #1FND
     C                   MOVE      EOFYR         #1FYR
     C                   MOVE      EOCASE        #1CASE
     C                   MOVE      EOCO#         KKCO#
     C                   MOVE      EOSUB#        KKSUB#
     C                   MOVE      EOFND         KKFND
     C                   MOVE      EOFYR         KKFYR
     C                   MOVE      EOCASE        KKCASE
     C                   MOVE      EOEOB#        KKEOB#
     C                   MOVE      EORCN         KKRCN
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
     C*  Get flag to determine if we use FMRCP file for Adj State
     C*
     C                   MOVEL     *BLANKS       ##RC              1
     C                   MOVEL     EOAST         @@KEY
     C*
     C     STKY          CHAIN     PST01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     STBS02        ##RC
     C                   MOVEL     STMFS         XXMFS
     C*
     C                   ENDIF                                                  *IN99
     C*
     C*** Beg Add *** 05/30/13 *********************************************
     C     @@RRN3        chain(n)  PCA02                              99
     C                   if        *In99 = *Off
     C                   eval      ##efdt = cafdat
     C                   endif
     C*** End Add *** 05/30/13 *********************************************
     C                   MOVEL     EOAST         ###ST
     C                   MOVEL     'M'           ###TYP
     C     RTKY1         SETGT     PRT01
     C     RTKY2         READPE    PRT01                                  99
     C     *IN99         DOWEQ     *OFF
     C     RTSTAT        IFEQ      'I'
     C     RTKY2         READPE    PRT01                                  99
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     RTMREL        ##MREL
     C     RTMREL        IFEQ      '1'
     C                   MOVE      RTEFDT        SVEFDT
     C                   ENDIF
     C                   ENDIF
     C* SET KEY FIELDS
     C*
     C                   Z-ADD     EOSUB#        KKSUB#
     C                   Z-ADD     EOFND         KKFND
     C                   Z-ADD     EOFYR         KKFYR
     C                   Z-ADD     EOCASE        KKCASE
     C                   Z-ADD     EOFEIN        KKFEIN
     C                   Z-ADD     EOMOD         KKMOD
     C                   Z-ADD     EOEOB#        KKEOB#
     C                   Z-ADD     EORCN         KKRCN
     C*
     C*
     C*
     C* IF IN DELETE MODE, WE MUST CHAIN TO FDCAP RECORD TO GET THE
     C* DETAIL SEQUECE NUMBER AND THE RELATIVE RECORD NUMBER FOR THE
     C* PHYSICIAN DETAIL RECORD.  ELSE USE THE PASSED PHYSICIAN DETAIL
     C* RELEATIVE RECORD NUMBER TO GET THE DETAIL SEQUENCE NUMBER
     C*
     C     @@MODE        IFEQ      'E'
     C     @@RRN3        CHAIN(N)  PCA02                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     CADSEQ        KKDSEQ
     C     DHKY          CHAIN     PDH01                              99
     C                   Z-ADD     DBRRNC        @@RRN
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ELSE
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C                   Z-ADD     DHDSEQ        KKDSEQ
     C                   ENDIF                                                  @@MODE = 'E'
     C*
     C* GET LAST LINE NUMBER USED FOR THE BILL
     C*
     C*
     C* SET TOTAL CHARGE
     C*
     C                   EXSR      SMCHRG
     C*
     C** Get 'Amount & type code' headings
     C*
     C     CXKY          CHAIN     PCX01                              99
     C                   MOVEL     CXHMRA        SCHMR1
     C                   MOVEL     CXHNET        SCHNTW
     C                   MOVEL     CXHOT1        SCHO11
     C                   MOVEL     CXHOT1        SCHO12
     C                   MOVEL     CXHOT2        SCHO21
     C                   MOVEL     CXHOT2        SCHO22
     C*
     C* SET INDICATOR TO DISPLAY NEWORK REDUCTIONS FIELD IF
     C* NETWORK IS INSTALLED
     C*
     C     ##NTWK        IFEQ      'Y'
     C                   SETON                                        47
     C*
     C* DETERMINE IF THE NETWORK SELECTED HAS A MANUAL FEE SCHEDULE
     C*
     C     EOFSFL        IFEQ      'Y'
     C                   SETON                                        03
     C                   ELSE
     C                   SETOFF                                       03
     C                   ENDIF
     C                   ELSE
     C                   SETOFF                                       47
     C                   ENDIF                                                  ##NTWK = 'Y'
     C*
     C* DETERMINE IF ANY EOB/NOTES EXISTS FOR THIS BILL
     C*
     C     DEKY          CHAIN(N)  PDE01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        69
     C                   ELSE
     C                   SETOFF                                       69
     C                   ENDIF                                                  *IN99 IFEQ *OFF
     C*
     C* DISPLAY PROVIDER NETWORK IF NOT BLANKS
     C*
     C     EONTWK        IFNE      *BLANKS
     C                   MOVEL(P)  EONTWK        SCNWCD
     C                   SETON                                        68
     C                   ELSE
     C                   SETOFF                                       68
     C                   ENDIF                                                  EONTWK
     C*
     C     @@MODE        IFEQ      'A'
     C                   SETON                                        41
     C                   ENDIF
     C*
     C* POS CURSOR TO ADJUDICATION CODE IN CHANGE MODE
     C*
     C     @@MODE        IFEQ      'C'
     C                   SETON                                        95
     C                   ENDIF
     C*
     C* DIAGNOSIS CODES 2, 3 AND 4 WILL BE DISPLAYED ONLY IF EXISTS
     C* FAEHP
     C*
     C     EHKY          CHAIN(N)  PEH01                              99
     C     *IN99         IFEQ      *OFF
     C*
      *** Beg Add *** 02/04/14 ****************************************
      /free
       If EHicdind <> 'Y';
      /end-free
      *** END Add *** 02/04/14 ****************************************
     C     EHDIA2        IFNE      *BLANKS
     C                   SETON                                        43
     C                   ELSE
     C                   SETOFF                                       43
     C                   ENDIF                                                  EHDIA2 NE *BLANKS
     C*
     C     EHDIA3        IFNE      *BLANKS
     C                   SETON                                        44
     C                   ELSE
     C                   SETOFF                                       44
     C                   ENDIF                                                  EHDIA3 NE *BLANKS
     C*
     C     EHDIA4        IFNE      *BLANKS
     C                   SETON                                        45
     C                   ELSE
     C                   SETOFF                                       45
     C                   ENDIF                                                  EHDIA4 NE *BLANKS
      *** Beg Add *** 02/04/14 ****************************************
      /free
       Else;
          If EHicd1002 <> *blanks;
             *in43 = *on;
          Else;
             *in43 = *off;
          Endif;

          If EHicd1003 <> *blanks;
             *in44 = *on;
          Else;
             *in44 = *off;
          Endif;

          If EHicd1004 <> *blanks;
             *in45 = *on;
          Else;
             *in45 = *off;
          Endif;
       Endif;
      /end-free
      *** END Add *** 02/04/14 ****************************************
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C* DEFAULT 'X' TO FIRST DIAGNOSIS CODE IF IT IS THE ONLY
     C* DIAGNOSIS CODE
     C*
      *** Beg Add *** 02/04/14 ****************************************
      /free
       If EHicdind <> 'Y';
      /end-free
      *** END Add *** 02/04/14 ****************************************
     C     @@MODE        IFEQ      'A'
     C     EHDIA1        ANDNE     *BLANKS
     C     EHDIA2        ANDEQ     *BLANKS
     C     EHDIA3        ANDEQ     *BLANKS
     C     EHDIA4        ANDEQ     *BLANKS
     C                   SETON                                        52
     C                   ELSE
     C                   SETOFF                                       52
     C                   ENDIF                                                  @@MODE = 'A'
      *** Beg Add *** 02/04/14 ****************************************
      /free
       Else;
          If @@mode = 'A' AND EHicd1001 <> *Blanks AND EHicd1002 = *Blanks
             AND EHicd1003 = *Blanks AND EHicd1004 =*Blanks;
             *IN52 = *on;
          Else;
             *IN52 = *off;
          Endif;
       Endif;
      /end-free
      *** End Add *** 02/04/14 ****************************************
     C*
     C     @@MODE        COMP      'A'                                2929
     C*
     C* PROTECT ALL FIELDS EXCEPT AMOUNT FIELDS WHEN DOING A RECON
     C* IF THE CHARGE IS PART OF THE PREVIOUSLY COMPLETED BILL.
     C* ALL FIELDS ARE AVAILABLE IF ADDDED DURING THE RECON.
     C*
     C     @@RRN3        CHAIN     PCA02                              99
     C     EORCN         IFNE      *ZEROS
     C     @@MODE        ANDEQ     'C'
     C     CARCN         ANDNE     EORCN
     C                   SETON                                        46
     C                   ELSE
     C                   SETOFF                                       46
     C                   ENDIF                                                  EORCN NE *ZEROS
     C*
     C* CALL PRR-CERT SELECTION PROGRAM IF IN ADD MODE AND PRE-CERT
     C* IS INSTALLED AND PRE-CERT RECORDS EXIST FOR THIS CASE
     C*
     C     @@MODE        IFEQ      'A'
     C     ##UR          ANDEQ     'Y'
     C     EOCERT        ANDEQ     *BLANKS
     C     EORCN         ANDEQ     *ZEROS
     C*
     C     PCKY          CHAIN     PPC02                              99
     C     *IN99         IFEQ      *OFF
     C     CMKY          CHAIN     PCM01                              99        GET FMCM RRN
     C                   CALL      'U0060R'
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      '2'           ##CALL
     C                   PARM      DBRRNB        ##RRN
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PPC01                              98
     C     *IN98         IFEQ      *OFF
     C     @@RRN2        CHAIN     PEO01                              98
     C     *IN98         IFEQ      *OFF
     C                   MOVEL(P)  PCCERT        EOCERT
     C                   Z-ADD     PCEXT         EOEXT
     C                   MOVEL     Q1USER        EOUSER
     C                   MOVEL     Q1PGM         EOPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        EOTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C                   UPDATE    PEO01
     C                   ENDIF                                                  *IN98 = *0FF
     C                   ENDIF                                                  *IN98 = *OFF
     C                   ENDIF                                                  ##KC EQ *BLANKS
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ENDIF                                                  @@MODE = 'A'
     C*
     C* DISPLAY EOB CODES AND TITLE IF NOT IN ADD MODE
     C*
     C     @@MODE        IFNE      'A'
     C                   SETON                                        59
     C                   ELSE
     C                   SETOFF                                       59
     C                   ENDIF                                                  @@MODE NE 'A'
     C*
     C* PERFORM FILE OVERRIDES
     C*
     C                   SETOFF                                       51
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL(P)  QCA(1)        QQCMD
     C                   MOVEL     EOAST         QQST
     C                   CALL      'QCMDEXC'
     C                   PARM                    QQCMD
     C                   PARM                    LENGTH           15 5
     C*
     C*
     C* IS THIS CLIENT 114
     C*
     C*** Beg Chg *** 08/24/16 ****************************************
     C******             SETOFF                                       05
     C                   Eval      inFMMCP = '0'
     C*** End Chg *** 08/24/16 ****************************************
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       ##CLT#            3
     C     ##CLT#        IFEQ      '114'
     C     EOAST         ANDEQ     'NJ'
     C*** Beg Chg *** 08/24/16 ****************************************
     C*******            SETON                                        05
     C                   Eval      inFMMCP = '1'
     C*** Beg Chg *** 08/24/16 ****************************************
     C                   ENDIF
     C*
      *** Beg Chg *** 08/24/16 ****************************************
     C**** *IN05         IFEQ      *ON
     C     inFMMCP       IFEQ      '1'
      *** Beg Chg *** 08/24/16 ****************************************
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL(P)  QCA(5)        QQCMD
     C                   MOVEL     EOAST         QQST
     C                   CALL      'QCMDEXC'
     C                   PARM                    QQCMD
     C                   PARM                    LENGTH           15 5
     C*
     C                   OPEN      FMMCL1
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL(P)  QCA(6)        QQCMD2
     C                   MOVEL     EOAST         QQST2
     C                   CALL      'QCMDEXC'
     C                   PARM                    QQCMD2
     C                   PARM                    LENGTH           15 5
     C*
     C                   ENDIF
     C*
     C                   OPEN      FMZRL1                               51
     C*
     C*
     C* GET PROV AUX INFO
     C*
     C     P1KY          CHAIN(N)  PP101                              99
     C     ##CLT#        IFEQ      '025'
     C                   SETON                                        10
     C                   ENDIF
     C*** Beg Add *** 06/21/12 ****************************************
      /Free
         If Eoast = 'HL';
           *in29 = *off;
         Endif;
      /End-Free
     C*** End Add *** 06/21/12 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*================================================================
     C* SMCHRG - Sum up all charges for bill
     C*================================================================
     C     SMCHRG        BEGSR
     C*
     C* SUM UP TOTAL CHARGES FOR THIS BILL
     C*
     C                   Z-ADD     0             TLCHRG
     C     CAKY3         CHAIN     PCA03                              99
     C     *IN99         DOWEQ     *OFF
     C                   ADD       CACHRG        TLCHRG
     C     CAKY3         READE     PCA03                                  99
     C                   ENDDO
     C                   Z-ADD     TLCHRG        SCTOTC
     C                   ENDSR
     C*================================================================
     C* UFAEOP - Update FAEOP file
     C*================================================================
     C     UFAEOP        BEGSR
     C*
     C     @@RRN2        CHAIN     PEO01                              99
     C     *IN99         IFEQ      *OFF
     C     @@MODE        IFNE      'E'
     C                   Z-ADD     SCFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLFDAT
     C*
     C     EOFRDT        IFEQ      *ZEROS
     C                   Z-ADD     HLFDAT        EOFRDT
     C                   ENDIF
     C*
     C     EOFRDT        IFNE      *ZEROS
     C     HLFDAT        ANDLT     EOFRDT
     C                   Z-ADD     HLFDAT        EOFRDT
     C                   ENDIF
     C*
     C     EOTODT        IFEQ      *ZEROS
     C                   Z-ADD     HLFDAT        EOTODT
     C                   ENDIF
     C*
     C     EOTODT        IFNE      *ZEROS
     C     HLFDAT        ANDGT     EOTODT
     C                   Z-ADD     HLFDAT        EOTODT
     C                   ENDIF
     C*** BEGIN ADD *** 08/21/02 **************************************
     C*** Beg Del *** 12/05/12 **************************************
     C*****
     C* IF THE LINE WAS NOT OVERIDDEN THEN CLEAR EOCRDO
     C* WHICH WILL ALLOW BILL TO GO INTO CODEREVIEW.
     C*****
     C*****              MOVEL(P)  SCCSCD        KKKEY
     C*****TBKY          CHAIN     PCS01                              99
     C******IN99         IFEQ      *ON
     C*****CSOVRD        ORNE      'Y'
     C*****              MOVEL(P)  *BLANKS       EOCRDO
     C*****              ENDIF
     C*****
     C*****              ELSE
     C*****              MOVEL(P)  *BLANKS       EOCRDO
     C*** End Del *** 12/05/12 **************************************
     C                   ENDIF
     C*
     C                   TIME                    DSDTTM
     C*** END   ADD *** 08/21/02 **************************************
     C*
     C                   MOVEL     Q1USER        EOUSER
     C                   MOVEL     Q1PGM         EOPGM
     C                   Z-ADD     DSTIME        EOTIME
     C                   MOVEL     SRDTO         EODT
     C*
     C                   UPDATE    PEO01
     C                   ENDIF                                                  *IN99 IFEQ
     C                   ENDSR
     C*================================================================
     C* UFDCAP - Update FDCAP record with any changes
     C*================================================================
     C     UFDCAP        BEGSR
     C     @@RRN3        CHAIN     PCA02                              99
     C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C*
     C* IF ADJUDICATION CODE IS NE 1, CLEAR CAMRAA. THIS WILL ALLOW THE
     C* OVERRIDE PROCESSING TO BE PERFORMED IN THE ADJUDICTION PROGRAM
     C*
     C*** COMMENTED OUT DUE TO PROBS W/ FSMRA *** 02/02/00 ************
     C*
     C*** Beg Add *** 06/06/17 ***************************************
     C                   if        CAcsCD <> '400' and xxrcFl <> ' '
     C*** End Add *** 06/06/17 ***************************************
     C                   MOVEL     XXRCFL        CARCFL
     C*** Beg Add *** 06/06/17 ***************************************
     C                   Endif
     C*** End Add *** 06/06/17 ***************************************
     C                   MOVEL     Q1USER        CAUSER
     C                   MOVEL     Q1PGM         CAPGM
     C                   Z-ADD     DSTIME        CATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C*
     C     CSOVRD        IFNE      'Y'
     C     CAMRA         ANDEQ     0
     C     CANDIS        ANDEQ     0
     C     CAOTHR        ANDEQ     0
     C     CAOTH2        ANDEQ     0
     C     EOCRNE        ANDEQ     'Y'
     C*
     C                   Z-ADD     0             CAPAID
     C*
     C                   ENDIF
     C*
     C                   UPDATE    PCA02
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C                   TIME                    DSDTTM
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C* This code will calculate units and charged amounts if multiple
     C* units were entered for this line item.
     C*
     C*
     C     SCUNTS        IFGT      1
     C     SV1A2         ANDEQ     *BLANK
     C     SVRCN         ANDEQ     EORCN
     C     ##SPLT        ANDNE     'N'
      **** BEGIN ADD *** 08/19/11 *************************************
      **** BEGIN CHG *** 09/21/11 *************************************
      **** Beg Chg *** 08/25/14 *************************************
     C***  MEDUNT        ANDEQ     'Y'
     C     MEDUNT        ANDNE     'Y'
      **** End Chg *** 08/25/14 *************************************
      **** END   CHG *** 09/21/11 *************************************
      **** END ADD   *** 08/19/11 *************************************
     C*
     C     SCUNTS        ORGT      1
     C     SV1A2         ANDEQ     'T'
     C     SVRCN         ANDEQ     EORCN
     C     ##SPLT        ANDNE     'N'
     C*
     C     SCCHRG        DIV       SCUNTS        SVCHRG
     C     SCCHRG        SUB       SVCHRG        SCCHRG
     C                   Z-ADD     1             CAUNTS
     C                   Z-ADD     1             DHUNTS
     C*
     C*** Beg Add *** 01/18/16 ****************************************
     C                   If        CSovrd = 'Y' and CSovrp = 0
     C*
     C     scMRA         Div       scUnts        svMRA
     C     scMRA         Sub       svMRA         scMRA
     C     scPaid        Div       scUnts        svPaid
     C     scPaid        Sub       svPaid        scPaid
     C     scNtwk        Div       scUnts        svNdis
     C     scNtwk        Sub       svNdis        scNtwk
     C     scOthr        Div       scUnts        svOthr
     C     scOthr        Sub       svOthr        scOthr
     C     scOth2        Div       scUnts        svOth2
     C     scOth2        Sub       svOth2        scOth2
     C*
     C                   Endif
     C*** End Add *** 01/18/16 ****************************************
     C*
     C                   ENDIF
     C*
     C*
     C                   MOVEL     Q1USER        DHUSER
     C                   MOVEL     Q1PGM         DHPGM
     C                   Z-ADD     DSTIME        DHTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DHDT
     C                   UPDATE    DBRCDF
     C*
     C* UPDATE FAEOP
     C*
     C                   EXSR      UFAEOP
     C*
     C* UPDATE FDCAP
     C*
     C                   EXSR      UFDCAP
     C*
     C* UPDATE TOTAL CHARGES FIELD
     C*
     C                   EXSR      SMCHRG
     C*
     C                   ENDIF
     C*
     C* This code will write multiple line items if multiple
     C* units were entered for this line item.
     C*
     C*
     C     SVRCN         IFEQ      EORCN
     C     SCUNTS        ANDGT     1
     C     SV1A2         ANDEQ     *BLANK
      **** BEGIN ADD *** 02/28/09 *************************************
     C     ##SPLT        ANDNE     'N'
      **** END ADD   *** 02/28/09 *************************************
      **** BEGIN ADD *** 08/19/11 *************************************
      **** BEGIN CHG *** 09/21/11 *************************************
      **** Beg Chg *** 08/25/14 *************************************
     C***  MEDUNT        ANDEQ     'Y'
     C     MEDUNT        ANDNE     'Y'
      **** End Chg *** 08/25/14 *************************************
      **** END   CHG *** 09/21/11 *************************************
      **** END ADD   *** 08/19/11 *************************************
     C*
     C     SVRCN         OREQ      EORCN
     C     SCUNTS        ANDGT     1
     C     SV1A2         ANDEQ     'T'
      **** BEGIN ADD *** 02/28/09 *************************************
     C     ##SPLT        ANDNE     'N'
      **** END ADD   *** 02/28/09 *************************************
     C*
     C                   SUB       1             SCUNTS
     C*
     C* Load next available line# and sequence#
     C*
     C                   Z-ADD     SVLIN#        SCLIN#
     C                   Z-ADD     SVDSEQ        SCSEQ#
     C*
     C                   EXSR      WRITER
     C*
     C                   ENDIF
     C*
     C*
     C                   SETOFF                                       90
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   Z-ADD     SCFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDAT            7 0
     C*
     C     SCPRCD        IFGE      '00100'
     C     SCPRCD        ANDLE     '01999'
     C     SCPRCD        ORGE      'D0001'
     C     SCPRCD        ANDLE     'D9999'
     C     SCUNTS        ORLE      1
     C     SV1A2         OREQ      'Y'
     C* Include anesthesia by a surgeon for Oklahoma
     C     SCAREA        ANDNE     'AN'
     C     SCAST         ANDNE     'OK'
     C*
     C     SV1A2         OREQ      'Y'
     C     SCAREA        ANDEQ     'AN'
     C     SCAST         ANDEQ     'OK'
     C* Include mileage for ambulance travel, ect.
     C*
     C     SV1A2         OREQ      'M'
     C* Include rental for HCPCS codes
     C*
     C     SV1A2         OREQ      'R'
     C* Include hourly fees
     C*
     C     SV1A2         OREQ      'H'
     C* Include HCPCS codes that are not mileage,rental, or hourly but
     C* still don't want to break out because they are purchased in
     C* large quantities.
     C*
     C     SV1A2         OREQ      'U'
      **** BEGIN ADD *** 02/28/09 *************************************
     C     ##SPLT        OREQ      'N'
      **** END ADD   *** 02/28/09 *************************************
     C*** Beg Add *** 09/21/12 ****************************************
     C     Medunt        OREQ      'Y'
     C*** End Add *** 09/21/12 ****************************************
     C*
     C                   CLEAR                   DBRCDF
     C                   CLEAR                   PCA02
     C                   TIME                    DSDTTM
     C                   EXSR      MVSCDB
     C                   Z-ADD     ##CO#         DHCO#
     C                   Z-ADD     EOSUB#        DHSUB#
     C                   Z-ADD     EOFND         DHFND
     C                   Z-ADD     EOFYR         DHFYR
     C                   Z-ADD     EOCASE        DHCASE
     C                   Z-ADD     SCSEQ#        DHDSEQ
     C                   Z-ADD     EOEOB#        DHEOB#
     C                   MOVEL     EOSS#         DHSS#
     C                   Z-ADD     EOACDT        DHACDT
     C                   MOVE      'A'           DHSTAT
     C                   MOVEL     Q1USER        DHUSER
     C                   MOVEL     Q1USER        DHIUSR
     C                   MOVEL     Q1PGM         DHPGM
     C                   MOVEL     Q1PGM         DHIPGM
     C                   Z-ADD     DSTIME        DHTIME
     C                   Z-ADD     DSTIME        DHITIM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DHDT
     C                   MOVEL     SRDTO         DHIDT
     C                   WRITE     DBRCDF
     C*
     C                   Z-ADD     ##CO#         CACO#
     C                   Z-ADD     EOSUB#        CASUB#
     C                   Z-ADD     EOFND         CAFND
     C                   Z-ADD     EOFYR         CAFYR
     C                   Z-ADD     EOCASE        CACASE
     C                   MOVEL     Q1USER        CAUSER
     C                   MOVEL     Q1USER        CAIUSR
     C                   MOVEL     Q1PGM         CAPGM
     C                   MOVEL     Q1PGM         CAIPGM
     C                   Z-ADD     DSTIME        CAITIM
     C                   Z-ADD     DSTIME        CATIME
     C                   MOVEL     SRDTO         CADT
     C                   MOVEL     SRDTO         CAIDT
     C                   MOVEL     SRDTO         CARDAT
     C                   Z-ADD     SCLIN#        CALIN#
     C                   Z-ADD     SCSEQ#        CADSEQ
     C                   Z-ADD     EOEOB#        CAEOB#
     C                   Z-ADD     EORCN         CARCN
     C                   Z-ADD     EOFEIN        CAFEIN
     C                   Z-ADD     EOMOD         CAMOD
     C                   MOVE      XXRCFL        CARCFL
     C*
     C*
     C                   MOVEL(P)  '1'           CAPRTY
     C*
     C                   WRITE     PCA02
     C*
     C                   ELSE
     C*
     C* WRITE MULTIPLE RECORDS BASED ON SCUNTS
     C*
     C     SCCHRG        DIV       SCUNTS        XXCHRG            9 2
     C                   MVR                     REMAIN            9 2
     C                   Z-ADD     SCUNTS        XXUNTS
     C*
     C     CSOVRD        IFEQ      'Y'
     C     CSOVRP        ANDEQ     *ZEROS
     C     SCUNTS        ANDGT     1
     C     SCMRA         DIV       SCUNTS        XXMRA             9 2
     C                   MVR                     XRMRA             9 2
     C     SCOTHR        DIV       SCUNTS        XXOTHR            9 2
     C                   MVR                     XROTHR            9 2
     C     SCOTH2        DIV       SCUNTS        XXOTH2            9 2
     C                   MVR                     XROTH2            9 2
     C     SCNTWK        DIV       SCUNTS        XXNTWK            9 2
     C                   MVR                     XRNTWK            9 2
     C                   ENDIF                                                  CSOVRD = 'Y'
     C                   CLEAR                   DBRCDF
     C                   CLEAR                   PCA02
     C                   TIME                    DSDTTM
     C                   EXSR      MVSCDB
     C                   Z-ADD     ##CO#         DHCO#
     C                   Z-ADD     EOSUB#        DHSUB#
     C                   Z-ADD     EOFND         DHFND
     C                   Z-ADD     EOFYR         DHFYR
     C                   Z-ADD     EOCASE        DHCASE
     C                   Z-ADD     SCSEQ#        DHDSEQ
     C                   Z-ADD     EOEOB#        DHEOB#
     C                   MOVEL     EOSS#         DHSS#
     C                   Z-ADD     EOACDT        DHACDT
     C                   MOVE      'A'           DHSTAT
     C                   MOVEL     Q1USER        DHUSER
     C                   MOVEL     Q1USER        DHIUSR
     C                   MOVEL     Q1PGM         DHPGM
     C                   MOVEL     Q1PGM         DHIPGM
     C                   Z-ADD     DSTIME        DHTIME
     C                   Z-ADD     DSTIME        DHITIM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DHDT
     C                   MOVEL     SRDTO         DHIDT
     C                   WRITE     DBRCDF
     C*
     C                   Z-ADD     XXCHRG        CACHRG
     C                   ADD       REMAIN        CACHRG
     C     CSOVRD        IFEQ      'Y'
     C     CSOVRP        ANDEQ     *ZEROS
     C     SCUNTS        ANDGT     1
     C     XXMRA         ADD       XRMRA         CAMRA
     C     XXOTHR        ADD       XROTHR        CAOTHR
     C     XXOTH2        ADD       XROTH2        CAOTH2
     C     XXNTWK        ADD       XRNTWK        CANDIS
     C     CACHRG        SUB       CAMRA         CAPAID
     C                   SUB       CAOTHR        CAPAID
     C                   SUB       CAOTH2        CAPAID
     C                   SUB       CANDIS        CAPAID
     C                   ENDIF                                                  CSOBRD = 'Y'
     C                   Z-ADD     1             CAUNTS
     C                   Z-ADD     ##CO#         CACO#
     C                   Z-ADD     EOSUB#        CASUB#
     C                   Z-ADD     EOFND         CAFND
     C                   Z-ADD     EOFYR         CAFYR
     C                   Z-ADD     EOCASE        CACASE
     C                   MOVEL     Q1USER        CAUSER
     C                   MOVEL     Q1USER        CAIUSR
     C                   MOVEL     Q1PGM         CAPGM
     C                   MOVEL     Q1PGM         CAIPGM
     C                   Z-ADD     DSTIME        CATIME
     C                   Z-ADD     DSTIME        CAITIM
     C                   MOVEL     SRDTO         CADT
     C                   MOVEL     SRDTO         CAIDT
     C                   MOVEL     SRDTO         CARDAT
     C                   Z-ADD     SCLIN#        CALIN#
     C                   Z-ADD     SCSEQ#        CADSEQ
     C                   Z-ADD     EOEOB#        CAEOB#
     C                   Z-ADD     EORCN         CARCN
     C                   Z-ADD     EOFEIN        CAFEIN
     C                   Z-ADD     EOMOD         CAMOD
     C*** Beg Add *** 06/06/17 ***************************************
     C                   if        CAcsCD <> '400' and xxrcFl <> ' '
     C*** End Add *** 06/06/17 ***************************************
     C                   MOVE      XXRCFL        CARCFL
     C*** Beg Add *** 06/06/17 ***************************************
     C                   endif
     C*** End Add *** 06/06/17 ***************************************
     C*
     C*
     C                   MOVEL(P)  '1'           CAPRTY
     C     CSOVRD        IFNE      'Y'
     C     CAMRA         ANDEQ     0
     C     CANDIS        ANDEQ     0
     C     CAOTHR        ANDEQ     0
     C     CAOTH2        ANDEQ     0
     C     EOCRNE        ANDEQ     'Y'
     C*
     C                   Z-ADD     0             CAPAID
     C*
     C                   ENDIF
     C*
     C                   WRITE     PCA02
     C*** Beg Add *** 01/18/16 ****************************************
     C     XXEOB         IFNE      *BLANKS
     C     XXNOT1        ORNE      *BLANKS
     C                   CLEAR                   PDE02
     C                   Z-ADD     ##CO#         DECO#
     C                   Z-ADD     EOSUB#        DESUB#
     C                   Z-ADD     EOFND         DEFND
     C                   Z-ADD     EOFYR         DEFYR
     C                   Z-ADD     EOCASE        DECASE
     C                   MOVEL(P)  EOSS#         DESS#
     C                   Z-ADD     EOACDT        DEACDT
     C                   Z-ADD     EOEOB#        DEEOB#
     C                   Z-ADD     ZZDSEQ        DEDSEQ
     C                   Z-ADD     1             DESEQ#
     C*** Beg Chg *** 07/18/23 **************************************
     C*****              MOVEL(P)  XXEOB         DEEOB
     C                   MOVEL(P)  XXEOB         DEEOBCD
     C*** End Chg *** 07/18/23 **************************************
     C     XXNOT1        IFEQ      *BLANKS
     C                   MOVEL(P)  XXEOB         KKKEY
     C     TBKY          CHAIN     PEB01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  EBDESC        DENOT1
     C*** Beg Add *** 07/18/23 **************************************
     C                   MOVEL(P)  EBDSC2        DENOT2
     C                   MOVEL(P)  EBDSC3        DENOT3
     C                   MOVEL(P)  EBDSC4        DENOT4
     C*** End Add *** 07/18/23 **************************************
     C                   ELSE
     C                   MOVE      *BLANKS       DENOT1
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ELSE
     C                   MOVEL(P)  XXNOT1        DENOT1
     C                   MOVEL(P)  XXNOT2        DENOT2
     C*** Beg Add *** 07/18/23 **************************************
     C                   MOVEL(P)  XXNOT3        DENOT3
     C                   MOVEL(P)  XXNOT4        DENOT4
     C*** End Add *** 07/18/23 **************************************
     C                   ENDIF
     C                   MOVE      'A'           DESTAT
     C                   MOVEL(P)  Q1USER        DEUSER
     C                   MOVEL(P)  Q1USER        DEIUSR
     C                   MOVEL(P)  Q1PGM         DEPGM
     C                   MOVEL(P)  Q1PGM         DEIPGM
     C                   Z-ADD     DSTIME        DETIME
     C                   Z-ADD     DSTIME        DEITIM
     C                   MOVEL     SRDTO         DEDT
     C                   MOVEL     SRDTO         DEIDT
     C                   MOVE      'U'           DEESRC
     C                   Z-ADD     EORCN         DERCN
     C                   WRITE     PDE02
     C                   ENDIF
     C*** End Add *** 01/18/16 ****************************************
     C                   SUB       1             XXUNTS
     C     XXUNTS        DOWGT     0
     C*
     C*
     C                   EXSR      NXTSEQ
     C*
     C                   CLEAR                   DBRCDF
     C                   CLEAR                   PCA02
     C                   TIME                    DSDTTM
     C                   EXSR      MVSCDB
     C                   Z-ADD     ##CO#         DHCO#
     C                   Z-ADD     EOSUB#        DHSUB#
     C                   Z-ADD     EOFND         DHFND
     C                   Z-ADD     EOFYR         DHFYR
     C                   Z-ADD     EOCASE        DHCASE
     C                   Z-ADD     ZZDSEQ        DHDSEQ
     C                   Z-ADD     EOEOB#        DHEOB#
     C                   MOVEL     EOSS#         DHSS#
     C                   Z-ADD     EOACDT        DHACDT
     C                   MOVE      'A'           DHSTAT
     C                   MOVEL     Q1USER        DHUSER
     C                   MOVEL     Q1USER        DHIUSR
     C                   MOVEL     Q1PGM         DHPGM
     C                   MOVEL     Q1PGM         DHIPGM
     C                   Z-ADD     DSTIME        DHTIME
     C                   Z-ADD     DSTIME        DHITIM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DHDT
     C                   MOVEL     SRDTO         DHIDT
     C                   WRITE     DBRCDF
     C*
     C                   Z-ADD     XXCHRG        CACHRG
     C     CSOVRD        IFEQ      'Y'
     C     CSOVRP        ANDEQ     *ZEROS
     C     SCUNTS        ANDGT     1
     C                   Z-ADD     XXMRA         CAMRA
     C                   Z-ADD     XXOTHR        CAOTHR
     C                   Z-ADD     XXOTH2        CAOTH2
     C                   Z-ADD     XXNTWK        CANDIS
     C     CACHRG        SUB       CAMRA         CAPAID
     C                   SUB       CAOTHR        CAPAID
     C                   SUB       CAOTH2        CAPAID
     C                   SUB       CANDIS        CAPAID
     C                   ENDIF                                                  CSOVRD = 'Y'
     C                   Z-ADD     1             CAUNTS
     C                   Z-ADD     ##CO#         CACO#
     C                   Z-ADD     EOSUB#        CASUB#
     C                   Z-ADD     EOFND         CAFND
     C                   Z-ADD     EOFYR         CAFYR
     C                   Z-ADD     EOCASE        CACASE
     C                   MOVEL     Q1USER        CAUSER
     C                   MOVEL     Q1USER        CAIUSR
     C                   MOVEL     Q1PGM         CAPGM
     C                   MOVEL     Q1PGM         CAIPGM
     C                   Z-ADD     DSTIME        CATIME
     C                   Z-ADD     DSTIME        CAITIM
     C                   MOVEL     SRDTO         CADT
     C                   MOVEL     SRDTO         CAIDT
     C                   MOVEL     SRDTO         CARDAT
     C                   Z-ADD     ZZLIN#        CALIN#
     C                   Z-ADD     ZZDSEQ        CADSEQ
     C                   Z-ADD     EOEOB#        CAEOB#
     C                   Z-ADD     EORCN         CARCN
     C                   Z-ADD     EOFEIN        CAFEIN
     C                   Z-ADD     EOMOD         CAMOD
     C*** Beg Add *** 06/06/17 ***************************************
     C                   if        CAcsCD <> '400' and xxrcFl <> ' '
     C*** End Add *** 06/06/17 ***************************************
     C                   MOVE      XXRCFL        CARCFL
     C*** Beg Add *** 06/06/17 ***************************************
     C                   endif
     C*** End Add *** 06/06/17 ***************************************
     C*
     C*
     C                   MOVEL(P)  '1'           CAPRTY
     C*
     C     CSOVRD        IFNE      'Y'
     C     CAMRA         ANDEQ     0
     C     CANDIS        ANDEQ     0
     C     CAOTHR        ANDEQ     0
     C     CAOTH2        ANDEQ     0
     C     EOCRNE        ANDEQ     'Y'
     C*
     C                   Z-ADD     0             CAPAID
     C*
     C                   ENDIF
     C*
     C                   WRITE     PCA02
     C* WRITE SAME EOB CODE RECORD FOR LINE ITEMS WHEN SCUNTS > 1
     C*
     C     CSOVRD        IFEQ      'Y'
     C     CSOVRP        ANDEQ     *ZEROS
     C     XXEOB         IFNE      *BLANKS
     C     XXNOT1        ORNE      *BLANKS
     C                   CLEAR                   PDE02
     C                   Z-ADD     ##CO#         DECO#
     C                   Z-ADD     EOSUB#        DESUB#
     C                   Z-ADD     EOFND         DEFND
     C                   Z-ADD     EOFYR         DEFYR
     C                   Z-ADD     EOCASE        DECASE
     C                   MOVEL(P)  EOSS#         DESS#
     C                   Z-ADD     EOACDT        DEACDT
     C                   Z-ADD     EOEOB#        DEEOB#
     C                   Z-ADD     ZZDSEQ        DEDSEQ
     C                   Z-ADD     1             DESEQ#
     C*** Beg Chg *** 07/18/23 **************************************
     C****               MOVEL(P)  XXEOB         DEEOB
     C                   MOVEL(P)  XXEOB         DEEOBCD
     C*** End Chg *** 07/18/23 **************************************
     C     XXNOT1        IFEQ      *BLANKS
     C                   MOVEL(P)  XXEOB         KKKEY
     C     TBKY          CHAIN     PEB01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  EBDESC        DENOT1
     C                   ELSE
     C                   MOVE      *BLANKS       DENOT1
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ELSE
     C                   MOVEL(P)  XXNOT1        DENOT1
     C                   MOVEL(P)  XXNOT2        DENOT2
     C                   ENDIF
     C                   MOVE      'A'           DESTAT
     C                   MOVEL(P)  Q1USER        DEUSER
     C                   MOVEL(P)  Q1USER        DEIUSR
     C                   MOVEL(P)  Q1PGM         DEPGM
     C                   MOVEL(P)  Q1PGM         DEIPGM
     C                   Z-ADD     DSTIME        DETIME
     C                   Z-ADD     DSTIME        DEITIM
     C                   MOVEL     SRDTO         DEDT
     C                   MOVEL     SRDTO         DEIDT
     C                   MOVE      'U'           DEESRC
     C                   Z-ADD     EORCN         DERCN
     C                   WRITE     PDE02
     C                   ENDIF
     C                   ENDIF                                                  CSOVRD = 'Y'
     C                   SUB       1             XXUNTS
     C*
     C                   ENDDO
     C*
     C                   ENDIF                                                  SCPRCD GE 00100
     C*
     C* UPDATE FAEOP
     C*
     C                   EXSR      UFAEOP
     C*
     C* UPDATE TOTAL CHARGES
     C*
     C                   EXSR      SMCHRG
     C*
     C                   FEOD      FDDHP
     C                   SETON                                        41
     C                   ENDSR
     C*=====================================================================
     C*  Subroutine PRCWDW
     C*=====================================================================
     C     PRCWDW        BEGSR
     C*
     C                   MOVEL     '1'           SCPHYF
     C*
     C     EXWDW2        TAG
     C*
     C                   EXFMT     WDW02
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C                   MOVE      *OFF          *IN06
     C                   MOVE      *OFF          *IN17
     C                   MOVE      *OFF          *IN18
     C                   MOVE      '0'           FLAG1             1
     C*
     C** VERIFY IF VALID OPTION HAS BEEN ENTERED
     C*
     C                   SELECT
     C*
     C     SCPHYF        WHENNE    '1'
     C     SCPHYF        ANDNE     *BLANKS
     C                   MOVE      '1'           *IN17
     C                   MOVE      '1'           *IN18
     C                   MOVE      '0'           *IN06
     C                   GOTO      EXWDW2
     C*
     C     SCDMEF        WHENNE    '1'
     C     SCDMEF        ANDNE     *BLANKS
     C                   MOVE      '1'           *IN17
     C                   MOVE      '0'           *IN18
     C                   MOVE      '0'           *IN06
     C                   GOTO      EXWDW2
     C*
     C     SCLABF        WHENNE    '1'
     C     SCLABF        ANDNE     *BLANKS
     C                   MOVE      '0'           *IN17
     C                   MOVE      '1'           *IN18
     C                   MOVE      '0'           *IN06
     C                   GOTO      EXWDW2
     C*
     C     SCDRGF        WHENNE    '1'
     C     SCDRGF        ANDNE     *BLANKS
     C                   MOVE      '0'           *IN17
     C                   MOVE      '0'           *IN18
     C                   MOVE      '1'           *IN06
     C                   GOTO      EXWDW2
     C*
     C     SCAMBF        WHENNE    '1'
     C     SCAMBF        ANDNE     *BLANKS
     C                   MOVE      '0'           *IN17
     C                   MOVE      '1'           *IN18
     C                   MOVE      '1'           *IN06
     C                   GOTO      EXWDW2
     C*
     C     SCASCF        WHENNE    '1'
     C     SCASCF        ANDNE     *BLANKS
     C                   MOVE      '1'           *IN17
     C                   MOVE      '1'           *IN18
     C                   MOVE      '1'           *IN06
     C                   GOTO      EXWDW2
     C*
     C                   ENDSL
     C*
     C                   SELECT
     C     SCPHYF        WHENNE    *BLANKS
     C     SCDMEF        ANDNE     *BLANKS
     C                   MOVE      '1'           FLAG1
     C     SCPHYF        WHENNE    *BLANKS
     C     SCLABF        ANDNE     *BLANKS
     C                   MOVE      '1'           FLAG1
     C     SCPHYF        WHENNE    *BLANKS
     C     SCDRGF        ANDNE     *BLANKS
     C                   MOVE      '1'           FLAG1
     C     SCPHYF        WHENNE    *BLANKS
     C     SCAMBF        ANDNE     *BLANKS
     C                   MOVE      '1'           FLAG1
     C     SCPHYF        WHENNE    *BLANKS
     C     SCASCF        ANDNE     *BLANKS
     C                   MOVE      '1'           FLAG1
     C     SCDMEF        WHENNE    *BLANKS
     C     SCLABF        ANDNE     *BLANKS
     C                   MOVE      '1'           FLAG1
     C     SCDMEF        WHENNE    *BLANKS
     C     SCDRGF        ANDNE     *BLANKS
     C                   MOVE      '1'           FLAG1
     C     SCDMEF        WHENNE    *BLANKS
     C     SCAMBF        ANDNE     *BLANKS
     C                   MOVE      '1'           FLAG1
     C     SCDMEF        WHENNE    *BLANKS
     C     SCASCF        ANDNE     *BLANKS
     C                   MOVE      '1'           FLAG1
     C     SCLABF        WHENNE    *BLANKS
     C     SCDRGF        ANDNE     *BLANKS
     C                   MOVE      '1'           FLAG1
     C     SCLABF        WHENNE    *BLANKS
     C     SCAMBF        ANDNE     *BLANKS
     C                   MOVE      '1'           FLAG1
     C     SCLABF        WHENNE    *BLANKS
     C     SCASCF        ANDNE     *BLANKS
     C                   MOVE      '1'           FLAG1
     C     SCDRGF        WHENNE    *BLANKS
     C     SCAMBF        ANDNE     *BLANKS
     C                   MOVE      '1'           FLAG1
     C     SCDRGF        WHENNE    *BLANKS
     C     SCASCF        ANDNE     *BLANKS
     C                   MOVE      '1'           FLAG1
     C     SCAMBF        WHENNE    *BLANKS
     C     SCASCF        ANDNE     *BLANKS
     C                   MOVE      '1'           FLAG1
     C                   ENDSL
     C*
     C                   SELECT
     C*
     C     SCDMEF        WHENNE    *BLANKS
     C     SCLABF        ORNE      *BLANKS
     C     SCDRGF        ORNE      *BLANKS
     C     SCAMBF        ORNE      *BLANKS
     C     SCASCF        ORNE      *BLANKS
     C     FLAG1         IFEQ      '1'
     C                   MOVE      '1'           *IN17
     C                   MOVE      '0'           *IN18
     C                   MOVE      '1'           *IN06
     C                   GOTO      EXWDW2
     C                   ELSE
     C                   MOVE      '1'           FLAG1
     C                   ENDIF
     C*
     C     SCLABF        WHENNE    *BLANKS
     C     SCDRGF        ORNE      *BLANKS
     C     SCAMBF        ORNE      *BLANKS
     C     SCASCF        ORNE      *BLANKS
     C     FLAG1         IFEQ      '1'
     C                   MOVE      '1'           *IN17
     C                   MOVE      '0'           *IN18
     C                   MOVE      '1'           *IN06
     C                   GOTO      EXWDW2
     C                   ELSE
     C                   MOVE      '1'           FLAG1
     C                   ENDIF
     C*
     C*
     C     SCDRGF        WHENNE    *BLANKS
     C     SCAMBF        ORNE      *BLANKS
     C     SCASCF        ORNE      *BLANKS
     C     FLAG1         IFEQ      '1'
     C                   MOVE      '1'           *IN17
     C                   MOVE      '0'           *IN18
     C                   MOVE      '1'           *IN06
     C                   GOTO      EXWDW2
     C                   ELSE
     C                   MOVE      '1'           FLAG1
     C                   ENDIF
     C*
     C     SCAMBF        WHENNE    *BLANKS
     C     SCASCF        ORNE      *BLANKS
     C     FLAG1         IFEQ      '1'
     C                   MOVE      '1'           *IN17
     C                   MOVE      '0'           *IN18
     C                   MOVE      '1'           *IN06
     C                   GOTO      EXWDW2
     C                   ELSE
     C                   MOVE      '1'           FLAG1
     C                   ENDIF
     C*
     C     SCASCF        WHENNE    *BLANKS
     C     FLAG1         IFEQ      '1'
     C                   MOVE      '1'           *IN17
     C                   MOVE      '0'           *IN18
     C                   MOVE      '1'           *IN06
     C                   GOTO      EXWDW2
     C                   ELSE
     C                   MOVE      '1'           FLAG1
     C                   ENDIF
     C*
     C                   ENDSL
     C** POPULATE THE ORIGINAL VALUES TO *IN17 AND *IN18
     C*
     C                   MOVE      IN17X         *IN17
     C                   MOVE      IN18X         *IN18
     C*
     C*** Proceed as per the options selected.
     C** Ambulance -    call F2369R01
     C** Clinical lab - call F2368R01
     C** Drug Fees -    call F2372R01
     C** DMEPOS -       call F2371R01
     C** Physician -    call F2366R01
     C** ASC          - call F2358R01
     C*
     C                   MOVEL     DHMOD1        MOD(1)
     C                   MOVEL     DHMOD2        MOD(2)
     C                   MOVEL     DHMOD3        MOD(3)
     C                   MOVEL     DHMOD4        MOD(4)
     C*
     C                   SELECT
     C*
     C     SCPHYF        WHENEQ    '1'
     C*
     C                   MOVEL(P)  SCPRCD        ##HCPC
     C     ##MREL        IFEQ      '1'
     C                   MOVE      SVEFDT        ##EFDT
     C                   ELSE
     C                   MOVE      CAFDAT        ##EFDT
     C                   ENDIF                                                          ##MREL = '1'
     C*
     C     '26'          LOOKUP    MOD                                    98
     C     *IN98         IFEQ      *ON
     C                   MOVE      '26'          ##MOD1
     C                   ELSE
     C     '53'          LOOKUP    MOD                                    98
     C     *IN98         IFEQ      *ON
     C                   MOVE      '53'          ##MOD1
     C                   ELSE
     C     'TC'          LOOKUP    MOD                                    98
     C     *IN98         IFEQ      *ON
     C                   MOVE      'TC'          ##MOD1
     C                   ELSE
     C                   MOVE      '  '          ##MOD1
     C                   ENDIF                                                  MOD = TC
     C                   ENDIF                                                  MOD = 53
     C                   ENDIF                                                  MOD = 26
     C*** Beg Add *** 01/31/19 ******************************************
     C     ##MREL        IFEQ      '1'
     C                   MOVE      SVEFDT        ##EFDT
     C                   ELSE
     C                   Z-ADD     CAFDAT        ##EFDT
     C                   ENDIF                                                          ##MREL = '1'
     C*** End Add *** 01/31/19 ******************************************
     C*
     C     RVKY3         SETGT     PRV01
     C     RVKY2         READPE    PRV01                                  99
     C     *IN99         DOWEQ     *OFF
     C     RVSTAT        IFEQ      'I'
     C     RVKY2         READPE    PRV01                                  99
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRNM
     C*** DISPLAY FROM R & C BEG
     C     ##RC          IFEQ      ' '
     C                   MOVEL     SCZIPA        KKZIPA
     C                   MOVEL(P)  SCPRCD        KKCPT4
     C     ##MREL        IFEQ      '1'
     C                   MOVE      SVEFDT        ##EFDT
     C                   ELSE
     C                   MOVEL     CAFDAT        KKEFDT
     C                   ENDIF                                                  ##MREL = '1'
     C     RCKY1         SETGT     PRC01
     C     RCKY2         READPE    PRC01                                  99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRNM
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     RCRRN         ##RRNM
     C                   ENDIF                                                  *IN99 = On
     C                   CALL      'F2310R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRNM
     C                   PARM                    ##ST
     C                   ELSE
     C     ##RC          IFEQ      'N'
     C                   EXSR      NOWDW2
     C                   ENDIF
     C                   ENDIF                                                  RCFP = 'Y'
     C*** DISPLAY FROM R & C END
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     RVRRN         ##RRNM
     C                   CALL      'F2366R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRNM
     C                   ENDIF                                                  *IN99 = On
     C*
     C     SCDMEF        WHENEQ    '1'
     C*
     C                   MOVEL(P)  SCPRCD        ##HCPC
     C     ##MREL        IFEQ      '1'
     C                   MOVE      SVEFDT        ##EFDT
     C                   ELSE
     C                   MOVE      CAFDAT        ##EFDT
     C                   ENDIF                                                  ##MREL = '1'
     C*
     C                   MOVEL     ' '           XXDME             1
     C                   Z-ADD     0             I                 1 0
     C                   MOVEL     ' '           XXOUT             1
     C*
     C     *IN99         DOUEQ     *OFF
     C*
     C                   ADD       1             I
     C     I             IFLT      5
     C     MOD(I)        DOWEQ     '  '
     C                   ADD       1             I
     C     I             IFGT      4
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C                   ENDIF                                                  I < 5
     C*
     C     I             IFGE      5
     C                   LEAVE
     C                   ENDIF
     C*
     C*
     C                   Z-ADD     0             J                 1 0
     C     J             ADD       1             L                 1 0
     C*
     C     J             DOWNE     5
     C     L             ANDNE     5
     C                   ADD       1             J
     C     J             ADD       1             L
     C     MOD(J)        DOWEQ     '  '
     C                   ADD       1             J
     C     J             ADD       1             L
     C     J             IFGT      4
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     J             IFNE      I
     C*
     C     J             IFNE      5
     C     I             ANDNE     5
     C                   MOVEL     MOD(I)        MODI              2
     C                   MOVEL     MOD(J)        MODJ              2
     C*
     C     MODI          IFNE      ' '
     C     MODJ          ANDNE     ' '
     C                   MOVEL     MOD(I)        XXMOD1            2
     C                   MOVEL     MOD(J)        XXMOD2            2
     C                   ENDIF
     C*
     C     DMKY1         SETGT     PDM01                              99
     C     DMKY2         READPE    PDM01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DMRRN         ##RRNM
     C                   MOVEL     '  '          XXMOD2
     C                   MOVEL     'Y'           XXDME
     C                   CALL      'F2371R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRNM
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ENDIF                                                  I <> 5 & J <> 5
     C*
     C                   ENDIF                                                  I <> J
     C*
     C                   ENDDO                                                  J <> 5 & L <> 5
     C*
     C*
     C                   ENDDO                                                  I < 5
     C*
     C     XXDME         IFNE      'Y'
     C*
     C                   MOVEL     *BLANKS       XXMOD1
     C                   MOVEL     MOD(1)        MOD1              2
     C                   MOVEL     MOD(2)        MOD2              2
     C                   MOVEL     MOD(3)        MOD3              2
     C                   MOVEL     MOD(4)        MOD4              2
     C                   SELECT
     C     MOD1          WHENNE    ' '
     C     MOD2          ANDEQ     ' '
     C     MOD3          ANDEQ     ' '
     C     MOD4          ANDEQ     ' '
     C                   MOVEL     MOD1          XXMOD1
     C     MOD1          WHENEQ    ' '
     C     MOD2          ANDNE     ' '
     C     MOD3          ANDEQ     ' '
     C     MOD4          ANDEQ     ' '
     C                   MOVEL     MOD2          XXMOD1
     C     MOD1          WHENEQ    ' '
     C     MOD2          ANDEQ     ' '
     C     MOD3          ANDNE     ' '
     C     MOD4          ANDEQ     ' '
     C                   MOVEL     MOD3          XXMOD1
     C     MOD1          WHENEQ    ' '
     C     MOD2          ANDEQ     ' '
     C     MOD3          ANDEQ     ' '
     C     MOD4          ANDNE     ' '
     C                   MOVEL     MOD4          XXMOD1
     C                   ENDSL                                                  SELECT STMT
     C*
     C                   MOVEL     '  '          XXMOD2
     C*
     C     DMKY1         SETGT     PDM01                              99
     C     DMKY2         READPE    PDM01                                  99
     C     *IN99         DOWEQ     *OFF
     C     DESTAT        IFEQ      'I'
     C     DMKY2         READPE    PDM01                                  99
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DMRRN         ##RRNM
     C                   MOVEL     '  '          XXMOD2
     C                   MOVEL     'Y'           XXDME
     C                   CALL      'F2371R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRNM
     C                   ELSE
     C                   MOVEL     '  '          XXMOD1
     C                   MOVEL     '  '          XXMOD2
     C     DMKY1         SETGT     PDM01                              99
     C     DMKY2         READPE    PDM01                                  99
     C     *IN99         DOWEQ     *OFF
     C     DESTAT        IFEQ      'I'
     C     DMKY2         READPE    PDM01                                  99
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DMRRN         ##RRNM
     C                   MOVEL     '  '          XXMOD2
     C                   MOVEL     'Y'           XXDME
     C                   CALL      'F2371R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRNM
     C                   ELSE
     C                   Z-ADD     0             ##RRNM
     C*** DISPLAY FROM R & C BEG
     C     ##RC          IFEQ      ' '
     C                   MOVEL     SCZIPA        KKZIPA
     C                   MOVEL(P)  SCPRCD        KKCPT4
     C     ##MREL        IFEQ      '1'
     C                   MOVE      SVEFDT        ##EFDT
     C                   ELSE
     C                   MOVEL     CAFDAT        KKEFDT
     C                   ENDIF                                                  ##MREL    IFEQ '1'
     C     RCKY1         SETGT     PRC01
     C     RCKY2         READPE    PRC01                                  99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRNM
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     RCRRN         ##RRNM
     C                   ENDIF                                                  *IN99 = On
     C                   CALL      'F2310R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRNM
     C                   PARM                    ##ST
     C                   ELSE
     C     ##RC          IFEQ      'N'
     C                   EXSR      NOWDW2
     C                   ENDIF
     C                   ENDIF                                                  RCFP = 'Y'
     C*** DISPLAY FROM R & C END
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXDME <> 'Y'
     C*
     C*
     C     SCLABF        WHENEQ    '1'
     C*
     C                   MOVEL(P)  SCPRCD        ##HCPC
     C     ##MREL        IFEQ      '1'
     C                   MOVE      SVEFDT        ##EFDT
     C                   ELSE
     C                   MOVE      CAFDAT        ##EFDT
     C                   ENDIF                                                  ##MREL    IFEQ '1'
     C                   Z-ADD     1             I                 1 0
     C                   MOVEL     *BLANKS       ##MOD1
     C     I             DOWLE     4
     C     MOD(I)        IFNE      ' '
     C                   MOVEL     MOD(I)        ##MOD1
     C                   LEAVE
     C                   ENDIF
     C                   ADD       1             I
     C                   ENDDO
     C*
     C     RVKY1         SETGT     PLB01
     C     RVKY2         READPE    PLB01                                  99
     C     *IN99         DOWEQ     *OFF
     C     LBSTAT        IFEQ      'I'
     C     RVKY2         READPE    PLB01                                  99
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C     *IN99         IFEQ      *ON
     C                   MOVEL     *BLANKS       ##MOD1
     C     RVKY1         SETGT     PLB01
     C     RVKY2         READPE    PLB01                                  99
     C     *IN99         DOWEQ     *OFF
     C     LBSTAT        IFEQ      'I'
     C     RVKY2         READPE    PLB01                                  99
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRNM
     C*** DISPLAY FROM R & C BEG
     C     ##RC          IFEQ      ' '
     C                   MOVEL     SCZIPA        KKZIPA
     C                   MOVEL(P)  SCPRCD        KKCPT4
     C     ##MREL        IFEQ      '1'
     C                   MOVE      SVEFDT        ##EFDT
     C                   ELSE
     C                   MOVEL     CAFDAT        KKEFDT
     C                   ENDIF                                                  ##MREL    IFEQ '1'
     C     RCKY1         SETGT     PRC01
     C     RCKY2         READPE    PRC01                                  99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRNM
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     RCRRN         ##RRNM
     C                   ENDIF                                                  *IN99 = On
     C                   CALL      'F2310R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRNM
     C                   PARM                    ##ST
     C                   ELSE
     C     ##RC          IFEQ      'N'
     C                   EXSR      NOWDW2
     C                   ENDIF
     C                   ENDIF                                                  RCFP = 'Y'
     C*** DISPLAY FROM R & C END
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     LBRRN         ##RRNM
     C                   CALL      'F2368R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRNM
     C*
     C                   ENDIF                                                  *IN99 = On
     C*
     C                   ELSE                                                   *IN99 = On
     C*
     C                   Z-ADD     LBRRN         ##RRNM
     C                   CALL      'F2368R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRNM
     C*
     C                   ENDIF                                                  *IN99 = On
     C*
     C     SCDRGF        WHENEQ    '1'
     C*
     C                   MOVEL(P)  SCPRCD        ##HCPC
     C     ##MREL        IFEQ      '1'
     C                   MOVE      SVEFDT        ##EFDT
     C                   ELSE
     C                   MOVE      CAFDAT        ##EFDT
     C                   ENDIF                                                  ##MREL    IFEQ '1'
     C     ASKY          SETGT     PDF01
     C     ASKY1         READPE    PDF01                                  99
     C     *IN99         DOWEQ     *OFF
     C     DFSTAT        IFEQ      'I'
     C     ASKY1         READPE    PDF01                                  99
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRNM
     C*** DISPLAY FROM R & C BEG
     C     ##RC          IFEQ      ' '
     C                   MOVEL     SCZIPA        KKZIPA
     C                   MOVEL(P)  SCPRCD        KKCPT4
     C     ##MREL        IFEQ      '1'
     C                   MOVE      SVEFDT        ##EFDT
     C                   ELSE
     C                   MOVEL     CAFDAT        KKEFDT
     C                   ENDIF                                                  ##MREL    IFEQ '1'
     C     RCKY1         SETGT     PRC01
     C     RCKY2         READPE    PRC01                                  99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRNM
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     RCRRN         ##RRNM
     C                   ENDIF                                                  *IN99 = On
     C                   CALL      'F2310R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRNM
     C                   PARM                    ##ST
     C                   ELSE
     C     ##RC          IFEQ      'N'
     C                   EXSR      NOWDW2
     C                   ENDIF
     C                   ENDIF                                                  RCFP = 'Y'
     C*** DISPLAY FROM R & C END
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     DRRRN         ##RRNM
     C                   CALL      'F2372R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRNM            9 0
     C                   ENDIF                                                  *IN99 = On
     C*
     C     SCAMBF        WHENEQ    '1'
     C*
     C                   MOVEL(P)  SCPRCD        ##HCPC
     C     ##MREL        IFEQ      '1'
     C                   MOVE      SVEFDT        ##EFDT
     C                   ELSE
     C                   MOVE      CAFDAT        ##EFDT
     C                   ENDIF                                                  ##MREL    IFEQ '1'
     C                   MOVEL     EOAST         KKST
     C                   MOVE      SCRGN#        KKLOC
     C     AMKY1         SETGT     PAM11
     C     AMKY2         READPE    PAM11                                  99
     C     *IN99         DOWEQ     *OFF
     C     AMSTAT        IFEQ      'I'
     C     AMKY2         READPE    PAM11                                  99
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRNM
     C*** DISPLAY FROM R & C BEG
     C     ##RC          IFEQ      ' '
     C                   MOVEL     SCZIPA        KKZIPA
     C                   MOVEL(P)  SCPRCD        KKCPT4
     C     ##MREL        IFEQ      '1'
     C                   MOVE      SVEFDT        ##EFDT
     C                   ELSE
     C                   MOVEL     CAFDAT        KKEFDT
     C                   ENDIF                                                  ##MREL    IFEQ '1'
     C     RCKY1         SETGT     PRC01
     C     RCKY2         READPE    PRC01                                  99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRNM
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     RCRRN         ##RRNM
     C                   ENDIF                                                  *IN99 = On
     C                   CALL      'F2310R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRNM
     C                   PARM                    ##ST
     C                   ELSE
     C     ##RC          IFEQ      'N'
     C                   EXSR      NOWDW2
     C                   ENDIF
     C                   ENDIF                                                  RCFP = 'Y'
     C*** DISPLAY FROM R & C END
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     AMRRN         ##RRNM
     C                   CALL      'F2369R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRNM
     C                   ENDIF                                                  *IN99 = On
     C*
     C     SCASCF        WHENEQ    '1'
     C*
     C                   MOVEL(P)  SCPRCD        ##HCPC
     C     ##MREL        IFEQ      '1'
     C                   MOVE      SVEFDT        ##EFDT
     C                   ELSE
     C                   MOVE      CAFDAT        ##EFDT
     C                   ENDIF                                                  ##MREL    IFEQ '1'
     C     ASKY          SETGT     PAS01
     C     ASKY1         READPE    PAS01                                  99
     C     *IN99         DOWEQ     *OFF
     C     ASSTAT        IFEQ      'I'
     C     ASKY1         READPE    PAS01                                  99
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRNM
     C*** DISPLAY FROM R & C BEG
     C     ##RC          IFEQ      ' '
     C                   MOVEL     SCZIPA        KKZIPA
     C                   MOVEL(P)  SCPRCD        KKCPT4
     C     ##MREL        IFEQ      '1'
     C                   MOVE      SVEFDT        ##EFDT
     C                   ELSE
     C                   MOVEL     CAFDAT        KKEFDT
     C                   ENDIF                                                  ##MREL    IFEQ '1'
     C     RCKY1         SETGT     PRC01
     C     RCKY2         READPE    PRC01                                  99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRNM
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     RCRRN         ##RRNM
     C                   ENDIF                                                  *IN99 = On
     C                   CALL      'F2310R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRNM
     C                   PARM                    ##ST
     C*** Beg Add *** 11/22/10 *************************************
     C                   ELSE
     C     ##RC          IFEQ      'N'
     C                   EXSR      NOWDW2
     C                   ENDIF
     C                   ENDIF                                                  RCFP = 'Y'
     C*** DISPLAY FROM R & C END
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     ASRRN         ##RRNM
     C                   CALL      'F2358R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRNM
     C                   ENDIF                                                  *IN99 = On
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  *INKL = *OFF
     C*
     C                   ENDSR
     C     NOWDW2        BEGSR
     C                   EXFMT     WDW03
     C                   ENDSR
     OPMG01     E            RPMG01
     OPDE01     E            RPDE01
**  (QCMDEX)
OVRDBF FILE(FMZRL1) TOFILE(*LIBL/FMZRL1  )
OVRDBF FILE(FMZRP) TOFILE(*LIBL/FMZRP  )
DLTOVR FILE(FMZRL1)
DLTOVR FILE(FMZRP)
OVRDBF FILE(FMMCL1) TOFILE(*LIBL/FMMCL1  )
OVRDBF FILE(FMMCP) TOFILE(*LIBL/FMMCP  )
DLTOVR FILE(FMMCL1)
DLTOVR FILE(FMMCP)
**  (EOBR1)
S01
S02
S03
S04
S05
S06
S13
S15
S16
S17
S18
S19
S20
**  (EOBR2)
S08
S09
S11
S20
