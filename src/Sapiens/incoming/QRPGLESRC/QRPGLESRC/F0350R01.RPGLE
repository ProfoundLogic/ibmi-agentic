     Hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H***** Beg Add *** 02/15/16 *******************************************
     H bnddir('FTBNDDIR')
     H***** End Add *** 02/15/16 *******************************************
     H*** Beg Add ***** 11/22/16 **************************************
     H Bnddir('STBNDDIR')
     H*** End Add ***** 11/22/16 **************************************
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE F0350R01 - Work with Provider Bills update/display
     H*****************************************************************
     H*  F0350R01 - Provider Bill Information                         *
     H*  Created: 04/01/98  C:56 Project: MC152                       *
     H*                                                               *
     H*****************************************************************
     H** REVISIONS:                                                   *
     h*                                                               *
     h*  12/16/24  C:079 Ticket: 152/117967                           *
     h*                  Delete Bill Import Records when Deleting EOB *
     h*                                                               *
     h*  06/09/20  C:295 Project : SY420                              *
     h*                  Mask SSN                                     *
     h*                                                               *
     h*  04/10/19 C:062 Project: 152WAB                               *
     h*                 Apply CSI mods for S1 Medical.                *
     H*                                                               *
     H*  04/12/18 C:180 Project: 114MJB                               *
     H*                 Add 9th parm when calling F0032R.             *
     H*                                                               *
     H*  02/20/18 C:180 Project: 114MJB                               *
     H*                 Add Provider info to Action Panel             *
     H*                                                               *
     H*  02/12/18 C:180 Project: 114MJC                               *
     H*                 Add 835 Export logic                          *
     H*                                                               *
     H*  12/04/17 C:079 Project: 114MJB                               *
     H*                 Add 837 Bill Import Image logic               *
     H*                                                               *
     H*  11/19/17 C:079 Project: 114MJB                               *
     H*                 Add upload logic for client 114               *
     H*                                                               *
     H*  10/26/17 C:079 Support: 114/49807                            *
     H*                 Allow ClaimCheck processing on Recons         *
     H*                                                               *
     H*  07/27/16 C:079 Support: 42594                                *
     H*                 Add leading zero to FEIN and Mod              *
     H*                                                               *
     H*  02/29/16 C:309 Support: 40083                                *
     H*                 Open up State Adj Code in Prov Bills OCR      *
     H*                                                               *
     H*  02/15/16 C:212 Project: 114MXB                               *
     H*                 Load description                              *
     H*                                                               *
     H*  02/11/16 C:307 SUPPORT:37159                                 *
     H*                 To subset non adjudication state list and     *
     H*                 not to show in provider bill and work with    *
     H*                 medical cases.
     H*                                                               *
     H*  11/05/15 C:079 Project: 114MTD                               *
     H*                 Call FA781R NOT based on OptaComp Group Assn  *
     H*                                                               *
     H*  06/19/15 C:079 PhaseWare: 114/34932                          *
     H*                 Update NPI# after Prov modifier search        *
     H*                                                               *
     H*  06/12/15 C:079 Support MC129 34486                           *
     H*                 Use ##PARN to subset providers                *
     H*                                                               *
     H*  04/14/15 C:079 PhaseWare: 114/34325                          *
     H*                 Open up fields if not bill data was copied    *
     H*                                                               *
     H*  11/24/14 C:296 Support 87194                                 *
     H*                 Update EABBLF='P' for deleted EOMBs.          *
     H*                 -FixEABBLF created for existing records.      *
     H*                                                               *
     H*  08/21/14 C:296 Project NC034                                 *
     H*                 Make field SCCSDT mandatory for NC state      *
     H*                                                               *
     H*  08/13/14 C:296 Project: 114MNV                               *
     H*                 For audited bills dont allow to edit.         *
     H*                                                               *
     H*  07/16/14 c:62  Support IM114 86105                           *
     H*                 Change how the return code is populated       *
     H*                                                               *
     H*  10/01/13 C:079 SUPPORT MC114 83675                           *
     H*                 Populate NPI number on change if its blanks   *
     H*                                                               *
     H*  09/17/13 C:079 PROJECT 114MJE                                *
     H*                 Add Logic to set EOCRNE = blank for hosp bill *
     H*                                                               *
     H*  04/16/13 C:277/079 PROJECT 114MJE                            *
     H*                 Add Logic to call program FA781R              *
     H*                                                               *
     H*  12/24/12 C:284 SEQ81187                                      *
     H*                 Lift edit from SCAST when it is protected.    *
     H*                                                               *
     H*  09/28/12 C:277 SUPPORT: MC114 80851                          *
     H*                 Remove condition for P1 and P2                *
     H*                                                               *
     H*  11/15/12 C:287 Project: MC367
     H*                 Remove options/functions for GS access to Bill Entry
     H*                                                               *
     H*  06/22/12 C:280 PROJECT 114MER                                *
     H*                 OUTPATIENT HOSPITAL BILLS AN ASC'S            *
     H*                                                               *
     H*  06/06/12 C:280 SUPPORT 79959                                 *
     H*                 FIX BUG WHEN SELECTING PROVIDER               *
     H*                                                               *
     H*  06/21/12 C:277 PROJECT 114MFI                                *
     H*                 Default the adjudication state to HL          *
     H*                                                               *
     H*  06/06/12 C:280 SUPPORT 79959                                 *
     H*                 FIX BUG WHEN SELECTING PROVIDER               *
     H*                                                               *
     H*  12/05/11 C:190 PROJECT 114ICM                                *
     H*                 Clear the image ID, so that bills can be      *
     H*                 re-entered from Work with Incoming docs, if   *
     H*                 CLTID = 114.                                  *
     H*                                                               *
     H*  11/02/11 C:280 PROJECT 114MAV                                *
     H*                 Display "Correct Coding" information window   *
     H*                 as requested clt114.                          *
     H*                                                               *
     H*  08/19/11 C:079 Project: 114MYS                               *
     H*                 Dont load P1ST into SCAST                     *
     H*                                                               *
     H*  07/25/11 C:273 Project: 114MYX                               *
     H*                 Display custom msg. window and update         *
     H*                 two custom tracking fields for CSI            *
     H*                                                               *
     H*  07/06/11 C:251 Support: 76897 MC025                          *
     H*                 Changed Chain order to remove record lock     *
     H*                 issue with FAEAP.                             *
     H*                 Correctly update FDCAP record for FEIN/Mod    *
     H*                                                               *
     H*  06/22/11 C:198 Project: 114MYS                               *
     H*                 Change PA to P1 when FNNWCG = 'Y'.            *
     H*                                                               *
     H*  04/27/11 C:079 Support: MC071 76483                          *
     H*                 Load SCSTYP into the key field                *
     H*                                                               *
     H*  02/15/11 C:079 Support: MC146 75832                          *
     H*                 Update adj. state in FDCAP on Unpend          *
     H*                                                               *
     H*  08/12/10 C:198 Project: 114MWS                               *
     H*                 Added 2 parameters when calling N0037R        *
     H*                                                               *
     H*  03/22/10 C:198 Support: 71801                                *
     H*                 Fixed the issue of state not getting          *
     H*                 defaulted with TX.                            *
     H*                                                               *
     H*  03/11/10 C:220 PROJECT: FL083                                *
     H*                 FL 2010 MEIG update                           *
     H*                                                               *
     H*  11/02/09 C:232 PROJECT: 139WMR                               *
     H*                 OFAC Reporting-Add Pending Status Indicator.  *
     H*                                                               *
     H*  06/04/09 C:198 Support: 68703                                *
     H*                 Re-compiled only.                             *
     H*                                                               *
     H*  01/27/09 C:062 PROJ: MC114 66685                             *
     H*                 ADD CLAIM RESTRICTIONS.                       *
     H*                                                               *
     H*  12/11/08 C:198 Project: FS326                                *
     H*                 EDI FLAG HOSP BILLS ADJUDICATION STATE CHANGES*
     H*                                                               *
     H*  10/23/08 C:198 Project: TX049 PRISS# 8                       *
     H*                 DISPLAY A WINDOW WHEN A PROVIDER IS BEING     *
     H*                 CHANGED TO THE ONE I.E. ELIBILE FOR PSA BONUS.*
     H*                                                               *
     H*  10/07/08 C:198 Project: TX049                                *
     H*                 DISPLAY A WINDOW WHEN A PROVIDER IS BEING     *
     H*                 ELIBILE FOR PSA BONUS.                        *
     H*                                                               *
     H*  08/25/08 C:180 Project: MC294                                *
     H*                 Validate NPI# is not blank in database file.  *
     H*                                                               *
     H*  08/15/08 C:062 PROJ: FL061                                   *
     H*                 Lift edit on policy state if not required.    *
     H*                                                               *
     H*  08/08/08 C:47  Project: MC321                                *
     H*                 MOVE FIELDS FROM SCREEN AND ADD TO            *
     H*                 THE SPECIFIC STATE DETAIL SCREENS.            *
     H*                 REMOVE: SCRFEI SCRMOD SCRPNM                  *
     H*                         SCFFEI SCFPNM                         *
     H*                         SCRNAM SCBLID SCBLIC                  *
     H*                 (REMOVE MOST OF MOD 7/01/06 & ALL OF 1/09/07) *
     H*                                                               *
     H*  07/14/08 C:180 PROJECT MC294                                 *
     H*                 - Validate NPI#                               *
     H*                 - Populate NPI# when searching/entering
     H*                   provider.
     H*                                                               *
     H*  06/25/08 C:189 PROJ: TX039                                   *
     H*                 CACSCD  should only be change when XXUPLD ='Y'*
     H*                 since its changing the DRG calculated value. *
     H*                                                               *
     H*  05/05/08 C:062 PROJ: FL061                                   *
     H*                 VARIOUS CHANGES.                              *
     H*                                                               *
     H*  01/22/08 C:062 PROJECT MC289                                 *
     H*                 Allow provider changes on pended original bill*
     H*                                                               *
     H*  08/16/07 C:062 PROJECT MC307                                 *
     H*                 Add Pend logic for uploaded bills.            *
     H*                                                               *
     H*  05/22/07 C:182 TN006: TN 2007 FS                             *
     H*                 Added 'FTFEL' in call to F0350C01             *
     H*                 Added 'FTFEL1' in call to F0350C01            *
     H*                                                               *
     H*  05/16/07 C:62  Project: MC294                                *
     H*                 Add billing provider npi#                     *
     H*                                                               *
     H*  04/05/07 C:188 FL061: Multi State Claim Processing           *
     H*                 Added 'Provider State' field SCPOST to screen *
     H*                 field edit, and search functionality.         *
     H*                                                               *
     H*  02/04/07 C:079 Support: 56669                                *
     H*                 1. Only write referring provider name if not  *
     H*                    TX Pharmacy bill                           *
     H*                 2. Edit for inactive bill types               *
     H*                                                               *
     H*  01/09/07 C:188 CA002: 837 CA Medical EDI                     *
     H*                 Implement CA 837 Medical EDI Requirements     *
     H*                                                               *
     H*  07/01/06 C:079 Project: 135MHP                               *
     H*                 Add logic for Texas 837                       *
     H*                                                               *
     H*  04/19/06 C:163 PROJECT 114WMF                                *
     H*                 Back to Base - EOMB Processing (114MEX)       *
     H*                                                               *
     H*  11/28/05 C:79  Support: 50054                                *
     H*                 Update Network Code                           *
     H*                                                               *
     H*  10/21/05 C:79  Support: 49510                                *
     H*                 Update adjudication code on uploaded bills    *
     H*                                                               *
     H*  10/18/05 C:79  Support: 49468                                *
     H*                 Update FAEOP Provider fields                  *
     H*                                                               *
     H*  08/25/05 C:79  Project: 10MGS                                *
     H*                 Added code to process P2P imported bills      *
     H*                                                               *
     H*  08/08/05 C:79  Project: 129MJR                               *
     H*                 Added code to process Scandirect imported bill*
     H*                                                               *
     H*  03/09/05 C:15  Project: WC1269                               *
     H*                 Added code to load the payee out of country   *
     H*                 address field into FAEOP.                     *
     H*                                                               *
     H*  09/28/04  :079 PROJECT: TX026                                *
     H*                 ADD PARM TO F0350C01 FOR NEW FILE             *
     H*                                                               *
     H*  04/15/04  :079 PROJECT: FL046                                *
     H*                 SET EDI FLAG = C FOR RECONSIDERATION OF A     *
     H*                 BILL PREVIOUSLY SENT TO THE STATE             *
     H*                                                               *
     H*  01/10/03 C:079 PROJECT: 114MDC                               *
     H*                 SET EOCRNE FOR USE BY F0353R ON AN ADD        *
     H*                                                               *
     H*  12/20/02 C:079 PROJECT: NW075                                *
     H*                 ADD MANUAL FS FLAG TO CALL OF N0037R          *
     H*                                                               *
     H*  12/13/02 C:079 PROJECT: MC255                                *
     H*                 DISPLAY POP UP WINDOW ON CONTROVERTED CLAIMS  *
     H*                                                               *
     H*  11/26/02 C:062 PROJECT: 114MDC                               *
     H*                 WHEN DELETING EOB, CLEAR EOCRPE.              *
     H*                                                               *
     H*  11/13/02 C:062 PROJECT: FS250                                *
     H*                 ALLOW ENTRY OF CASE NUMBER IF SYSTEM FLAG = Y *
     H*                                                               *
     H*  11/11/02 C:079 PROJECT: FS251                                *
     H*                 KEEP CLAIMANT INFO IF SYSTEM FLAG = Y         *
     H*                                                               *
     H*  08/13/02 C:062 PROJECT: FS238 MC044 33219                    *
     H*                 CORRECT WHEN DEAFAULT ADJUDICATION STATE IS   *
     H*                 ASSIGNED.                                     *
     H*                                                               *
     H*  04/10/02 C:062 PROJECT: FS238                                *
     H*                 FIX THE EDIT OF ADJUDICATION STATE.           *
     H*                                                               *
     H*  02/12/02 C:062 PROJECT: MC044 31039                          *
     H*                 CHAIN OUT TO CLAIM TO GET CLAIM NUMBER.       *
     H*                                                               *
     H*  12/12/01 C:062 PROJECT: WC1060  BASE ONLY                    *
     H*                 Don't allow entry of bill if employer policy  *
     H*                 year excludes medical.                        *
     H*                                                               *
     H*  06/04/01 C:079 PROJECT: 91MEL                                *
     H*                 Change edit on SCAST to check to see if state *
     H*                 is installed ST1A3                            *
     H*                                                               *
     H*  12/29/00 C:117 PROJECT: FS209                                *
     H*                 CHANGE F16=HISTORY TO USE NEW PGMS            *
     H*                                                               *
     H*  10/13/00 C:062 Support MC238                                 *
     H*                 ADD LOGIC TO DEAL WITH NON-SEARCHABLE FIELDS. *
     H*                                                               *
     H*  08/29/00 C:079 Support MC071/#24036                          *
     H*                 Release the record lock on FALTP              *
     H*                                                               *
     H*  08/04/00 C:117 PROJECT MCMNT                                 *
     H*                 Added a PARM to call F0350C01 for the hold    *
     H*                 Total Charge Amt.                             *
     H*                                                               *
     H*  06/19/00 C:117 PROJECT FS211                                 *
     H*                 Had to add logic to include Dental RRNs       *
     H*                                                               *
     H* 05/31/00 C:53  Project: WC461                                 *
     H*                Add WCMS Daily batch lockout process           *
     H*                                                               *
     H*  05/28/00  C:117  Proj: MCMNT                                 *
     H*                   Right Adjust Provider & MOD fields          *
     H*                   (Compile Only)                              *
     H*                                                               *
     H*  04/24/00 C:062 PROJECT: MC229                                *
     H*                 SEND N0037R SPECIAL NETWORK PARM MESSAGE.     *
     H*                                                               *
     H*  04/06/00 C:062 PROJECT: MC225                                *
     H*                 PASS ADDITIONAL VIEW PARM TO F0032R.          *
     H*                                                               *
     H*  02/25/00 C:117 PROJECT: MC224 SUPPORT: MC048/21653           *
     H*                 DON'T ALLOW ENTRY OF BILL IF IT IS RECORDS    *
     H*                 ONLY                                          *
     H*                                                               *
     H*  01/31/00 C:62  PROJECT: MC048 21648                          *
     H*                 IF MOD IS SENT INTO F0032R SO THAT IT CAN     *
     H*                 POSITION TO THAT SPECIFIC MODIFIER.           *
     H*                                                               *
     H*  01/18/00 C:56  Project: MC152                                *
     H*                 Changed the write of FAEOP/EOSTAT on an add   *
     H*                 to 'A'.                                       *
     H*                                                               *
     H*  12/21/99 C:56  Support: MC025 #20963                         *
     H*                 Move defaulting of state and check on beast   *
     H*                 record to after return from provider multiple *
     H*                 modifier search                               *
     H*                                                               *
     H*  12/06/99 C:56  Support: MC044 #20844                         *
     H*                 On opt2=change, if the selection is between   *
     H*                 reconsideration and EDI changes, insure that  *
     H*                 program knows when recon has been selected.   *
     H*                                                               *
     H*  11/24/99 C:31  Support: 20083                                *
     H*                 F16 History issue.                            *
     H*                 1. Make Multi Case Search a subroutine        *
     H*                 2. Add Multi Case Search to F16               *
     H*                                                               *
     H*                                                               *
     H*  11/23/99 C:62  Support: MC044 20176                          *
     H*                 REINITIALIZE NETWORK PARM WHEN CONTINUING     *
     H*                 WORKFLOW.                                     *
     H*                                                               *
     H*  11/09/99 C:56  Support: MC044 20172                          *
     H*                 Add logic to insure that curor goes to        *
     H*                 correct filed after addition of bill.         *
     H*                                                               *
     H*  11/03/99 C:56  Support: MC044 20288                          *
     H*           1.    Look for at least one active record for the   *
     H*                 entered fien.                                 *
     H*                                                               *
     H*           2.    Move validation of provider status and type   *
     H*                 to after selection from modifier from F0032R  *
     H*                 This prevents getting edits before the user   *
     H*                 has detemined what modifier he wants.         *
     H*                                                               *
     H*           3.    Add record lockup editing to end of MODEK1    *
     H*                 subroutine.  This is done just in case the    *
     H*                 user spent alot of time selecting modifier    *
     H*                 network.                                      *
     H*                                                               *
     H*           4.    Move the writing of the lockout record to the *
     H*                 very end of ERRCK1 subroutine.                *
     H*                                                               *
     H*           5.    Cycle through provider file for entered fien  *
     H*                 looking for at leas on active status.         *
     H*                                                               *
     H*  10/26/99 C:62  PROJECT:MC209                                 *
     H*                 DON'T CHOOSE INACTIVE PROVIDERS FOR PAR/NON.  *
     H*                                                               *
     H*  10/11/99 C:56  Support: MC082 #20035                         *
     H*                 Add call to F0267R when there are multple     *
     H*                 cases with the same SS#/ACDT and case was not *
     H*                 selected from search.                         *
     H*                                                               *
     H*  09/10/99 C:56  Project: KY010                                *
     H*                 Add changes to accomodate EDI/Tape submission *
     H*                 if in changed mode.                           *
     H*                                                               *
     H*  09/08/99 C:56  Project: MC152                                *
     H*                 Allow repeated entry of bills without         *
     H*                 returning to F0350R.                          *
     H*                                                               *
     H*  08/24/99 C:62  Support: MC209                                *
     H*                 MAKE SURE N0037R IS CALLED WHEN NETWORK       *
     H*                 INSTALLED AND GROUP HAS NETWORKS.             *
     H*                                                               *
     H*  08/10/99 C:62  Support: MC178                                *
     H*                 ADD PARM TO FA720R.                           *
     H*                                                               *
     H*  07/03/99 C:56  Support: MC057 #19052                         *
     H*                 In ERRCK1 subroutine, always write screen case*
     H*                 information from FMCMP.  This will keep the   *
     H*                 case infor and the SS#/DOI consistent when the*
     H*                 user types in the SS#/DOI.                    *
     H*                                                               *
     H*  07/21/99 C:56  Support: MC072 #18948                         *
     H*                 When deleting bill, update FAEAP/EAEPFL with  *
     H*                 'D'.                                          *
     H*                                                               *
     H*  07/20/99 C:56  Support MC072 #18934                          *
     H*                 When provider has value in first name field   *
     H*                 in FMPDP, put first and last names in EOPNAM  *
     H*                                                               *
     H*  07/15/99 C:62  Project: MC209                                *
     H*                 ADD CALL TO N0037R.                           *
     H*                                                               *
     H*  07/09/99 C:62  Project: MC209                                *
     H*                 CHANGE OUT F0534R WITH F0032R.                *
     H*                                                               *
     H*  07/01/99 C:56  Project: MC202                                *
     H*                 1. When unpending bill, complete pend letter  *
     H*                 2. Display window indicating the number of    *
     H*                    pended bills for the SS#/ACDT from current *
     H*                    bill.                                      *
     H*                                                               *
     H*  06/13/99 C:56  Project: KY010                                *
     H*                 If adjudication state is Kentucky, then       *
     H*                 require 'bill sent date'.                     *
     H*                                                               *
     H*  05/05/99  C:56   MC152                                       *
     H*                   Change FAEAL to FAEAL1, duplicate logicals  *
     H*                                                               *
     H*  04/21/99 C:56 Support: MC025 #17737                          *
     H*                If bill status is deleted; clear CACHK# field  *
     H*                so line item will not appear in history.       *
     H*                                                               *
     H*  04/01/99 C:56 MC025 #17467                                   *
     H*                Add edit against seleceting an invalid provider*
     H*                                                               *
     H*  03/25/99 C:56 MC152                                          *
     H*                1. ADD CO#/SUB#/FND AS PARMS TO CALL OF        *
     H*                   HISTORY PROGRAM - F0575R                    *
     H*                                                               *
     H*  03/22/98 C:56 MC025 #17234                                   *
     H*                1. Base write of Provider name and address     *
     H*                   base on FDDAP/PDDBA1                        *
     H*                                                               *
     H*  01/18/99 C:79 PROJ:25MKO                                     *
     H*                1. Add ability to process type "V"=Vendor Fee  *
     H*                   bills                                       *
     H*****************************************************************
     FF0350F01  CF   E             WORKSTN
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F*
     F*  Physical file definition (for maintaining record)
     FFAEOP     UF A E             DISK
     F                                     RENAME(PEO01:DBRCDF)
     F                                     INFDS(DBFD4)
     F*
     F*  Provider Master FIle
     FFMPDL     IF   E           K DISK
     F*
     F*  Physician/Provider Specialist Transaction File
     FFAPPP     IF   E             DISK
     F*
     F*  State Code Table FIle
     FSTSTL1    IF   E           K DISK
     F                                     RENAME(PST01:PST02)
     F*
     F*  State Code Table FIle
     FSTSTP     IF   E             DISK
     F*
     F*  Generic Table FIle
     FFTGTP     IF   E             DISK
     F*
     F*  Fee Schedule Claims Status Code File
     FFTCSP     IF   E             DISK
     F*
     F*  Case Master File
     FFMCML1    IF   E           K DISK
     F                                     INFDS(DBFDS)
     FFMCMLB    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     F*
     F*  Generic Table File
     FFTGTL     IF   E           K DISK
     F                                     RENAME(PGT01:PGT02)
     F*
     F* TABLE FILE LOGICAL
     FFTABL     IF   E           K DISK
     F                                     RENAME(PCS01:PCS02)
     F*
     F*  Company Auxiliary File
     FFMC1L     UF   E           K DISK
     F*
     F*  EOMB Phys/HCFA Header File
     FFAEHL     IF   E           K DISK
     F                                     INFDS(DBFD2)
     F*  EOMB Hosp/UB Header File
     FFAEUL     IF   E           K DISK
     F                                     INFDS(DBFD5)
     F*  EOMB Pharmacy Header File
     FFAEPL     IF   E           K DISK
     F                                     INFDS(DBFD7)
     F*  EOMB Dental Header File
     FFAEDL     IF   E           K DISK
     F                                     INFDS(DBFD9)
     F*
     F*  EOMB Phys/HCFA Detail Transaction File
     FFDDHL     IF   E           K DISK
     F                                     INFDS(DBFD3)
     F*  EOMB Hosp/UB Bill Detail Trans File
     FFDDUL     IF   E           K DISK
     F                                     INFDS(DBFD6)
     F*  EOMB Pharmacy Bill Detail Trans File
     FFDDPL     IF   E           K DISK
     F                                     INFDS(DBFD8)
     F*  EOMB Pharmacy Bill Detail Trans File
     FFDDDL     IF   E           K DISK
     F                                     INFDS(DBFD0)
     F*
     F*  Provider Bills Control File
     FFMBEL     IF   E           K DISK
     F*
     F*  Provider Auxiliary File
     FFMP1L     IF   E           K DISK
     F*
     F*  Geographic Zip Area File
     FFMGZL     IF   E           K DISK
     F*
     F*  Detail Charge File
     FFDCALL    UF   E           K DISK
     F*
     F*  EOMB Detail EOB/Notes Transaction File
     FFAEEL     IF   E           K DISK
     F*
     F*  WCMC Case Lockout File
     FFXLOP     UF A E           K DISK
     F*  User Parameter File
     F*  WCMC Fund/Group Master File
     FFMFNL     IF   E           K DISK
     FFMF1L     IF   E           K DISK
     FFMS4L1    IF   E           K DISK
     F*  EOMB Header Amount Transaction File
     FFAEAL1    UF   E           K DISK
     FFALTL7    UF   E           K DISK
     FFAEOL33   IF   E           K DISK
     F                                     RENAME(PEO01:PEO02)
     FFMCMP     IF   E             DISK
     F                                     RENAME(PCM01:PCM03)
     FFAESL2    IF   E           K DISK
     FFAEOL4    IF   E           K DISK
     FFMCXL     IF   E           K DISK
     FFDCALR    UF   E           K DISK
     F                                     RENAME(PCA01:PCA02)
     F*
     F* Pre-Cert Master File
     FUMPCP     IF   E             DISK
     FUMPCL3    IF   E           K DISK
     F                                     RENAME(PPC01:PPC02)
     FNMNWL     IF   E           K DISK
     FWMXRL     IF   E           K DISK    USROPN
     F*
     F*
     FFAESL     IF   E           K DISK
     F                                     RENAME(PES01:PES02)
     F*
     F*  EOMB STATUS TRANSACTION
     F*
     FFAAUL1    IF   E           K DISK
     F*
     F*  WCMC MED BILL AUTH AUX
     F*
     F***** Beg Add **** 06/21/12 *************************************
     Fwmcml     if   e           k disk    usropn
     F                                     rename(PCM01:PCM04) Prefix(C4:2)
     F***** End Add **** 06/21/12 *************************************
     F*
     F*
     F***** Beg Add *** 02/15/16 *******************************************
     Ffddhl1    uf   e           k disk    rename(pdh01:pdh02) prefix(dh2:2)
     F***** End Add *** 02/15/16 *******************************************
     F*** Beg Add *** 02/20/18 ***************************************
     FFACLML8   if   e           k disk
     FFAHDRL    if   e           k DISK
     F*** End Add *** 02/20/18 ***************************************
     F*** Beg Add *** 12/16/24 ************************************************
     Ffau1l2    uf   e           k disk
     Ffau2l2    uf   e           k disk
     F*** End Add *** 12/16/24 ************************************************
      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************
     D***** Beg Add *** 02/15/16 *******************************************
      /COPY FPRPROCCD
     D***** End Add *** 02/15/16 *******************************************
     d*** Beg Add 12/04/17 *************************************************
      /Copy sprModule
     d*** End Add 12/04/17 *************************************************
      /COPY SCOPYMT01D
     D*** Beg Add ****** 12/16/24 **************************
      /Copy sPrGenPop
     D*** End Add ****** 12/16/24 *************************
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFD2           DS
     D  DBRRN2               397    400B 0
     D DBFD3           DS
     D  DBRRN3               397    400B 0
     D DBFD4           DS
     D  DBRRN4               397    400B 0
     D DBFD5           DS
     D  DBRRN5               397    400B 0
     D DBFD6           DS
     D  DBRRN6               397    400B 0
     D DBFD7           DS
     D  DBRRN7               397    400B 0
     D DBFD8           DS
     D  DBRRN8               397    400B 0
     D DBFD9           DS
     D  DBRRN9               397    400B 0
     D DBFD0           DS
     D  DBRRN0               397    400B 0
     D*
     D                 DS
     D  FN                     1     50
     D                                     DIM(10)
     D  F1NW01                 1      5
     D  F1NW02                 6     10
     D  F1NW03                11     15
     D  F1NW04                16     20
     D  F1NW05                21     25
     D  F1NW06                26     30
     D  F1NW07                31     35
     D  F1NW08                36     40
     D  F1NW09                41     45
     D  F1NW10                46     50
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Provider Bill Inform-
     D                                     ation')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)
     D*================================================================
     D*  Program specific constants
     D*================================================================
     D VLDCHR          C                   CONST('ABCDEFGHIJKLMNOPQRST-
     D                                     UVWXYZ0123456789 ')
     D*** Beg Add *** 04/12/18 ****************************************
     D MOD837p         C                   CONST('837 ASC X12 Prof')
     D MOD837i         C                   CONST('837 ASC X12 Inst')
     D*** End Add *** 04/12/18 ****************************************

     D*** Beg Add *** 06/21/12 ****************************************
     D xxopfl          s              1    inz(*blanks)
     D*** End Add *** 06/21/12 ****************************************
     D*** Beg Add *** 04/16/13 ****************************************
     D RtvGrpAsc       pr                  extpgm('WA729R')
     D inCo#                          3  0
     D inFnd                          3  0
     D outGAsc                       10
     D*
     D PopEobHdrDtl    pr                  extpgm('FA781R')
     D inDbrrn                        9  0
     D inCo#                          3  0
     D inDcid                              like(eoidoc)
     D outRrn                         9  0
     D outrtnc                        1
     D*
     D inDbrrn         s              9  0
     D outRrn          s              9  0
     D inCo#           s              3  0
     D inFnd           s              3  0
     D inDcid          s                   like(eoidoc)
     D outGAsc         s             10    inz(*blanks)
     D outrtnc         s              1    inz(*blanks)
     D x1upld          s              1    inz(*blanks)
     D*
     D*** End Add *** 04/16/13 ****************************************
     D*** Beg Add *** 12/04/17 ****************************************
     D Lod837Data      pr                  extpgm('F8193R')
     D inMode                         1
     D inBchCntl                      9
     D inRrn                          9  0
     D inDcId                        12
     D outRrn                         9  0
     D outRtCd                        1
     D*
     D inMode          s              1
     D inBchCntl       s              9
     D inRrn           s              9  0
     D inDcId2         s             12
     D outRtCd         s              1
     D*** End Add *** 12/04/17 ****************************************
     d*** Beg Add *** 11/19/17 ****************************************
     d GetUplFlag      pr                  extpgm('FA743R')
     d inMode_                        1
     d inRrn_                         9  0
     d inStCd_                        3
     d outUpLd_                       1
     d*** End Add *** 11/19/17 ****************************************
     D*** Beg Add *** 02/11/16 ****************************************
     D ##adstate       s              1
     D*** End Add *** 02/11/16 ****************************************
      *
     D*** Beg Add *** 02/15/16 ****************************************
     D kkfdat          s              7S 0 inz
     D outdescpr       s             25    inz
     D outdescbp       s             25    inz
     D*** End Add *** 02/15/16 ****************************************
     d*** Beg Add *** 12/04/17 ****************************************
     d xxRcFl          s              1
     d*** End Add *** 12/04/17 ****************************************
     d*** Beg Add *** 02/20/18 ****************************************
     D xxZip           S                   LIKE(EOzip)
     d*** End Add *** 02/20/18 ****************************************
     d*** Beg Add *** 04/12/18 ****************************************
     D ##RRN4          S              9P 0 INZ
     D QQST            S              2A   INZ
     d*** End Add *** 04/12/18 ****************************************
     d*** Beg Add *** 12/16/24 *********************************************
     d isDelBillImp    s              1n   inz(*off)
     d strMsg          s            200
     d isClearInv      s              1n   inz(*off)
     d*** End Add *** 12/16/24 *********************************************
     I*** END   ADD *** 07/15/99 **************************************
     I*
     I*** BEG ADD 07/01/99 ********************************************
     IPEO02
     I              EOCO#                       E2CO#
     I              EOFND                       E2FND
     I              EOFYR                       E2FYR
     I              EOCASE                      E2CASE
     I              EOTRDT                      E2TRDT
     I              EOFEIN                      E2FEIN
     I              EOMOD                       E2MOD
     I              EOAMT                       E2AMT
     I              EOFRDT                      E2FRDT
     I              EOTODT                      E2TODT
     I              EOTRCD                      E2TRCD
     I              EOPNAM                      E2PNAM
     I              EOADR1                      E2ADR1
     I              EOADR2                      E2ADR2
     I              EOCTY                       E2CTY
     I              EOST                        E2ST
     I              EOZIP                       E2ZIP
     I              EOEOB1                      E2EOB1
     I              EOEOB2                      E2EOB2
     I              EOEOB3                      E2EOB3
     I              EOCHK#                      E2CHK#
     I              EOEOB#                      E2EOB#
     I              EOSYS                       E2SYS
     I              EOFLG1                      E2FLG1
     I              EOFLG2                      E2FLG2
     I              EOFLG3                      E2FLG3
     I              EOFLG4                      E2FLG4
     I              EOFLG5                      E2FLG5
     I              EOFLG6                      E2FLG6
     I              EOFLG7                      E2FLG7
     I              EOFLG8                      E2FLG8
     I              EOFLG9                      E2FLG9
     I              EOAMTC                      E2AMTC
     I              EOSTAT                      E2STAT
     I              EOUSER                      E2USER
     I              EOPGM                       E2PGM
     I              EOTIME                      E2TIME
     I              EODT                        E2DT
     I              EOIUSR                      E2IUSR
     I              EOIDT                       E2IDT
     I              EOWCCK                      E2WCCK
     I              EODIA1                      E2DIA1
     I              EODIA2                      E2DIA2
     I              EODIA3                      E2DIA3
     I              EODIA4                      E2DIA4
     I              EOEMPL                      E2EMPL
     I              EODFC                       E2DFC
     I              EOFLIC                      E2FLIC
     I              EOTAPE                      E2TAPE
     I              EO#BLL                      E2#BLL
     I              EOCLSF                      E2CLSF
     I              EOAST                       E2AST
     I              EOSUB#                      E2SUB#
     I              EOAUDT                      E2AUDT
     I              EOBTFL                      E2BTFL
     I              EOHRFL                      E2HRLF
     I              EOBLFL                      E2BLFL
     I              EONLFL                      E2NLFL
     I              EODIFL                      E2DIFL
     I              EOINV#                      E2INV#
     I              EORMDS                      E2RMDS
     I              EORMK1                      E2RMK1
     I              EORMK2                      E2MRK2
     I              EORMK3                      E2RMK2
     I              EOCKCN                      E2CKCN
     I              EOOTHR                      E2OTHR
     I              EONDIS                      E2NDIS
     I              EOSB01                      E2SB01
     I              EOSB02                      E2SB02
     I              EOSB03                      E2SB03
     I              EOSB04                      E2SB04
     I              EOSB05                      E2SB05
     I              EOSB06                      E2SB06
     I              EOSB07                      E2SB07
     I              EOSB08                      E2SB08
     I              EOSB09                      E2SB09
     I              EOSB10                      E2SB10
     I              EOBAMT                      E2BAMT
     I              EOZIPA                      E2ZIPA
     I              EOCERT                      E2CERT
     I              EOEXT                       E2EXT
     I              EOSS#                       E2SS#
     I              EOACDT                      E2ACDT
     I              EOIPGM                      E2IPGM
     I              EOITIM                      E2ITIM
     I              EOSFEI                      E2SFEI
     I              EOSMOD                      E2SMOD
     I              EOBTYP                      E2BTYP
     I              EOSTYP                      E2STYP
     I              EOBLST                      E2BLST
     I              EOCSCD                      E2CSCD
     I              EOSTCD                      E2STCD
     I              EORCN                       E2RCN
     I              EOCSDT                      E2CSDT
     I              EOCRDT                      E2CRDT
     I              EOSID#                      E2SID#
     I              EOSINV                      E2SINV
     I              EOSEFT                      E2SEFT
     I              EOBID#                      E2BID#
     I              EOCID#                      E2CID#
     I              EOEID#                      E2EID#
     I              EOPID#                      E2PID#
     I              EOSPAY                      E2SPAY
     I              EOCHRG                      E2CHRG
     I              EOMRA                       E2MRA
     I              EOOTH2                      E2OTH2
     I              EOPAID                      E2PAID
     I              EONAMT                      E2NAMT
     I              EONTWK                      E2NTWK
     I              EONMTC                      E2NMTC
     I              EONMTP                      E2NMTP
     I              EOSPCD                      E2SPCD
     I              EOOHPT                      E2OHPT
     I              EOOHIW                      E2OHIW
     I              EOOHPI                      E2OHPI
     I              EOOHTN                      E2OHTN
     I              EOPEOB                      E2PEOB
     I              EOROOM                      E2ROOM
     I              EOLSN                       E2LSN
     I              EOFSN                       E2FSN
     I              EOMID                       E2MID
     I              EOCNTL                      E2CNTL
     I              EOSBDT                      E2SBDT
     I*** Beg Add *** 07/25/11 ****************************************
     I              EONDPS                      E2NDPS
     I              EODPCT                      E2DPCT
     I*** End Add *** 07/25/11 ****************************************
     I*** END ADD *** 07/01/99 ****************************************
     I***PY SCOPYMT01I
     I*** END ADD *** 09/10/99 ****************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
CALL C                   PARM                    @@SUB#            5 0
FROM C                   PARM                    @@FND             3 0
MED  C                   PARM                    @@FYR             3 0
CASESC                   PARM                    @@CASE            7 0
FUTURC*
     C                   PARM                    @@NWST            1
     C                   PARM                    @@DCID           12
     C                   PARM                    @@RTRN            6
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*  STATE PARM LIST
     C*
     C     STST          PLIST
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C                   PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      *ZEROS        ##RRN1            9 0
     C*
     C** GENERIC TABLE FILE PARM LIST
     C*
     C     GTPLT         PLIST
     C                   PARM      '2'           ##CALL
     C                   PARM      '0'           ##KC
     C                   PARM      '0'           ##KL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM                    ##TBID
     C*
     C** CLAIM STATUS CODE TABLE FILE PARM LIST
     C*
     C     CSPLT         PLIST
     C                   PARM      '2'           ##CALL
     C                   PARM      '0'           ##KC
     C                   PARM      '0'           ##KL
     C                   PARM      *ZEROS        ##RRN
     C*
     C     CHGPL         PLIST
     C                   PARM                    QQNUM            80
     C                   PARM                    QQMASK           10
     C*STAND ALONE API PLIST
     C     INSPL         PLIST
     C                   PARM      ' '           ##RTCD            1
     C                   PARM                    #@CO#             3
     C                   PARM                    #@SUB#            5
     C                   PARM                    #@FND             3
     C                   PARM      ' '           ##OUTS            1
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*** KEY LIST FOR NETWORK MASTER FILE
     C*
     C     NWKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EONTWK
     C*
     C*** KEY LIST FOR PROVIDER MASTER FILE
     C*
     C     PDKY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C     PDKY2         KLIST
     C                   KFLD                    KKFEIN
     C*
     C*** KEY LISTS FOR CASE MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     CMKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSS#
     C                   KFLD                    KKACDT
     C*
     C** KEY LIST FOR STATE FILE
     C*
     C     STKY          KLIST
     C                   KFLD                    KKAST
     C*
     C** KEY LIST FOR GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*
     C*
     C* Key List for Company Auxiliary File
     C*
     C     COKY          KLIST
     C                   KFLD                    ##CO#
     C*
     C* Key List for EOMB Phys/HCFA Header File
     C*
     C     EHKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C* Key List for EOMB Hosp/UB Header File
     C*
     C     EUKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C* Key List for Pharmacy Header Transaction File
     C*
     C     EPKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C* Key List for Dental Header Transaction File
     C*
     C     EDKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C* Key List for EOMB Phys/HCFA Detail Transaction File
     C*
     C     DHKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C* Key List for EOMB Hosp/UB Detail Transaction File
     C*
     C     DUKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C* Key List for EOMB Pharmacy Detail Transaction File
     C*
     C     DPKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C* Key List for EOMB Dental Detail Transaction File
     C*
     C     DDKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C* Key List for Provider Bills Control File (FMBEP)
     C*
     C     BEKY          KLIST
     C                   KFLD                    K1AST
     C                   KFLD                    KKSYS
     C                   KFLD                    KKBTYP
     C                   KFLD                    KKSTYP
     C*
     C* Key List for Table File
     C*
     C     TBKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKKEY
     C*
     C* Key List for Provider Auxiliary File
     C*
     C     P1KY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C* Key List for Geographic Zip Area File
     C*
     C     GZKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKZIPA
     C*
     C* Key List for EOMB Transaction File
     C*
     C     EOKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C* Key List for Detail Charge File
     C*
     C     CAKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKEOB#
     C*
     C* KEY LIST FOR EOB/Notes File
     C*
     C     EEKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C** KEY LIST FOR WCMC CASE LOCKOUT FILE
     C*
     C     LOKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C* key list for WCMC fund/group master file
     C*
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C* key list for user parameter file
     C*
     C*
     C* key list for EOMB Header Amount Transaction File
     C*
     C     EAKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C*
     C     ESKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C                   KFLD                    KKIDT
     C                   KFLD                    KKITIM
     C*
     C     ESKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C     LTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C                   KFLD                    KKLTR#
     C*
     C     LTKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C*
     C     EOKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSS#
     C                   KFLD                    KKACDT
     C                   KFLD                    KKFEIN
     C     EAKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C*
     C*   Group Control File
     C*
     C     CXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C     CAKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C* KEY LIST FOR PRE-CERT MASTER FILE
     C*
     C     PCKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C     XRKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    Q1USER
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCCASE
     C*
     C*
     C*
     C*  EOMB STATUS TRANSACTION
     C*
     C     ESKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C*  WCMC MED BILL AUTH AUX
     C*
     C     AUKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKDCID
     C*
     C***** Beg Add **** 06/21/12 *************************************
     C*
     C*  WMCMP Key list
     C*
     C     CMKY4         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C***** End Add **** 06/21/12 *************************************
     C*
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    DBRRN4        SVRRN
     C*
     C*** BEG ADD *** 06/22/12 **************************************
     C*
     C     *LIKE         DEFINE    DBRRN4        SVRRN
     C*
     C*** END ADD *** 06/22/12 **************************************
     C*
     C     *LIKE         DEFINE    EOFEIN        HLFEIN
     C     *LIKE         DEFINE    EOMOD         HLMOD
     C     *LIKE         DEFINE    EOFEIN        KKFEIN
     C     *LIKE         DEFINE    EOMOD         KKMOD
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C     *LIKE         DEFINE    CMSUB#        KKSUB#
     C     *LIKE         DEFINE    CMFND         KKFND
     C     *LIKE         DEFINE    CMFYR         KKFYR
     C     *LIKE         DEFINE    CMCASE        KKCASE
     C     *LIKE         DEFINE    STKEY         KKAST
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C     *LIKE         DEFINE    CMSS#         KKSS#
     C     *LIKE         DEFINE    CMACDT        KKACDT
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    EOAST         ##AST
     C     *LIKE         DEFINE    EOBTYP        ##BTYP
     C     *LIKE         DEFINE    EOSTYP        ##STYP
     C     *LIKE         DEFINE    EOSYS         ##SYS
     C     *LIKE         DEFINE    PCCERT        ##CERT
     C     *LIKE         DEFINE    PCEXT         ##EXT
     C     *LIKE         DEFINE    EOBTYP        KKBTYP
     C     *LIKE         DEFINE    EOSTYP        KKSTYP
     C     *LIKE         DEFINE    EOSYS         KKSYS
     C     *LIKE         DEFINE    EOAST         K1AST
     C     *LIKE         DEFINE    EOFEIN        ZZFEIN
     C     *LIKE         DEFINE    EOMOD         ZZMOD
     C     *LIKE         DEFINE    EOCRDT        HLCRDT
     C     *LIKE         DEFINE    EOCSDT        HLCSDT
     C     *LIKE         DEFINE    EOCRDT        HLSYDT
     C     *LIKE         DEFINE    EOACDT        HLACDT
     C     *LIKE         DEFINE    EORCN         KKRCN
     C     *LIKE         DEFINE    LTLTR#        KKLTR#
     C     *LIKE         DEFINE    EOCHRG        HLCHRG
     C     *LIKE         DEFINE    ESIDT         KKIDT
     C     *LIKE         DEFINE    ESITIM        KKITIM
     C     *LIKE         DEFINE    EOAST         QQPOST
     C     *LIKE         DEFINE    EOFEIN        HDFEIN
     C     *LIKE         DEFINE    EOMOD         HDMOD
     C     *LIKE         DEFINE    EOIDOC        KKDCID
     C*
     C/COPY SCOPYMT01C
       //*** Beg Add *** 10/26/17 *************************************
       //==============================================================
       // ChkClaimCheck - Determine if bill qualifies for ClaimCheck
       //==============================================================
      /Free
       BegSR ChkClaimCheck;
          If ##Crvw = 'Y' and EObTyp = 'N'
            Or ##Crvw = 'Y' and EObTyp = 'H' and EUblTy >= '130'
            and EUblTy <= '139'
            Or ##Crvw = 'Y' and EObTyp = 'H' and EUblTy >= '830'
            and EUblTy <= '839';
             Chain cxKy pcx01;
                If %Found(fmcxl) and CXcrUs = 'Y';
                   EOcrNe = 'Y';
                Else;
                   EOcrNe = 'N';
                Endif;
          Else;
             EOcrNe = 'N';
          Endif;
       EndSR;
       //*** End Add *** 10/26/17 *************************************
      /End-Free
     C*
     C*================================================================
     C* DATSBR - Date check subrountine
     C*================================================================
     C     DATSBR        BEGSR
     C* THIS SUBROUTINE IS CALLED AFTER EVERY SEARCH TO PROPERLY
     C* FORMAT ALL DATES ON SCREEN
     C*
     C                   Z-ADD     SCACDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCACDT
     C*
     C                   Z-ADD     SCCRDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCCRDT
     C*
     C                   Z-ADD     SCCSDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCCSDT
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   EXSR      WRKFLW
     C     ##KL          IFEQ      '1'
     C                   SETON                                        90
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
b01  C     @@MODE        IFEQ      'A'
     C     XXUPLD        OREQ      'Y'
b02  C     *INKC         IFNE      *ON
b03  C     *IN17         IFEQ      *OFF
     C*
     C** SOCIAL SECURITY NUMBER IS REQUIRED
     C*
b04  C     SCSS#         IFEQ      *BLANKS
     C                   SETON                                        9071
     C                   GOTO      CK1END
e04  C                   ENDIF
     C*
     C** DATE OF INJURY IS REQUIRED
     C*
b04  C     SCACDT        IFEQ      *ZEROS
     C                   SETON                                        9072
     C                   GOTO      CK1END
e04  C                   ENDIF
     C*
     C** DATE OF INJURY MUST BE A VALID DATE
     C*
     C                   Z-ADD     SCACDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
b04  C     SRERR         IFEQ      '1'
     C                   SETON                                        9078
     C                   GOTO      CK1END
x04  C                   ELSE
     C                   MOVEL     SRDTO         SCACDT
e04  C                   ENDIF
     C*
     C** VERIFY THAT CLAIM EXISTS
     C*
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVE      SCSS#         KKSS#
      /Free
         kkss# = UnMaskData('SSN':'CMSS#':scss#);
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C                   Z-ADD     SCACDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKACDT
     C     CMKY1         CHAIN     PCM02                              99
b04  C     *IN99         IFEQ      *ON
     C                   SETON                                        9082
     C                   GOTO      CK1END
x04  C                   ELSE
     C*
     C* LOAD CASE INFO TO SCREEN IF USER TYPES IN SS#/ACDT
     C*
b05  C     @@MODE        IFEQ      'A'
     C     XXUPLD        OREQ      'Y'
     C*
     C*          Call subroutine to check for multiple cases
     C*          ------------------------------------------------------
     C                   EXSR      TESTMC
     C   49              GOTO      CK1END
     C                   Z-ADD     SCSUB#        KKSUB#
     C                   Z-ADD     SCFND         KKFND
     C                   Z-ADD     SCFYR         KKFYR
     C                   Z-ADD     SCCASE        KKCASE
     C**IF*THERE IS MORE THAN ONE CASE FOR THIS SS#/ACDT, CALL
     C**PROGRAM TO SELECT APPROPRIATE CASE.
     C**IF*CASE INFORMATION MATCHES, THE USER HAS SELECTED FROM THE
     C**CASE*SEARCH, THEREFORE THERE IS NO NEED TO CALL MULTIPLE CASE
     C**SEARCH.
     C     CMKY          CHAIN     PCM01                              99
b06  C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  CMLSN         SCLSN
     C                   MOVEL(P)  CMFSN         SCFSN
     C                   CALL      'SRNAME'
     C                   PARM      CMLSN         ##LAST
     C                   PARM      CMFSN         ##FRST
     C                   PARM      '1'           ##TERM
     C*
     C                   MOVEL(P)  ##LAST        SCNAME
     C*
e06  C                   ENDIF
     C*
     C** CHANGE USER PARMS
     C*
     C                   MOVE(P)   ##CO#         #2CO#
     C                   MOVE(P)   SCSUB#        #2SUB#
     C                   MOVE(P)   SCFND         #2FND
     C                   CALL      'F0006R2'
     C                   PARM                    #2CO#
     C                   PARM                    #2SUB#
     C                   PARM                    #2FND
     C                   PARM      ' '           ##RTCD
     C*
e05  C                   ENDIF
     C*
e04  C                   ENDIF
x03  C                   ELSE
     C                   Z-ADD     SCSUB#        KKSUB#
     C                   Z-ADD     SCFND         KKFND
     C                   Z-ADD     SCFYR         KKFYR
     C                   Z-ADD     SCCASE        KKCASE
     C     CMKY          CHAIN     PCM01                              99
b45  C     *IN99         IFEQ      *ON
     C                   SETON                                        9082
     C                   GOTO      CK1END
x04  C                   ELSE
     C*
     C* LOAD CASE INFO TO SCREEN IF USER TYPES IN SS#/ACDT
     C*
b05  C     @@MODE        IFEQ      'A'
     C     XXUPLD        OREQ      'Y'
     C*
     C*          Call subroutine to check for multiple cases
     C*          ------------------------------------------------------
     C                   EXSR      TESTMC
     C*
     C   49              GOTO      CK1END
     C                   Z-ADD     SCSUB#        KKSUB#
     C                   Z-ADD     SCFND         KKFND
     C                   Z-ADD     SCFYR         KKFYR
     C                   Z-ADD     SCCASE        KKCASE
     C     CMKY          CHAIN     PCM01                              99
b06  C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  CMLSN         SCLSN
     C                   MOVEL(P)  CMFSN         SCFSN
     C*
     C                   CALL      'SRNAME'
     C                   PARM      CMLSN         ##LAST
     C                   PARM      CMFSN         ##FRST
     C                   PARM      '1'           ##TERM
     C*
     C                   MOVEL(P)  ##LAST        SCNAME
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVEL(P)  CMSS#         SCSS#
      /Free
         scss# = MaskData('SSN':'CMSS#':CMSS#);
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
e06  C                   ENDIF
     C** CHANGE USER PARMS
     C*
     C                   MOVE(P)   ##CO#         #2CO#
     C                   MOVE(P)   SCSUB#        #2SUB#
     C                   MOVE(P)   SCFND         #2FND
     C                   CALL      'F0006R2'
     C                   PARM                    #2CO#
     C                   PARM                    #2SUB#
     C                   PARM                    #2FND
     C                   PARM      ' '           ##RTCD
     C*
e05  C                   ENDIF
e04  C                   ENDIF
e03  C                   ENDIF
     C*
     C* CHECK TO SEE IF USER IS RESTRICTED FROM THIS CLAIM
     C*
b03  C     *IN90         IFEQ      *OFF
     C     ##WCIF        ANDEQ     'Y'
     C     ##WCIN        ANDEQ     'Y'
     C     ##NPWC        ANDNE     'Y'
     C                   OPEN      WMXRL
     C     XRKY          CHAIN     PXR01                              99
b04  C     *IN99         IFEQ      *OFF
     C     XRSTAT        ANDEQ     'A'
     C                   SETON                                        6890
e04  C                   ENDIF
     C                   CLOSE     WMXRL
e03  C                   ENDIF
     C*
     C** PROVIDER IS REQUIRED
     C*
b03  C     SCFEIN        IFEQ      *ZEROS
     C                   SETON                                        9081
     C                   GOTO      CK1END
e03  C                   ENDIF
     C*  VALIDATE PROVIDER
     C*
     C                   Z-ADD     SCFEIN        KKFEIN
     C*
     C     PDKY2         CHAIN     PPD01                              99
b03  C     *IN99         IFEQ      *ON
     C                   SETON                                        9086
     C                   GOTO      CK1END
     C*
x03  C                   ELSE
b04  C     *IN99         DOUEQ     *ON
b05  C     PDSTAT        IFEQ      'A'
     C     PDSTAT        OREQ      *BLANKS
     C                   LEAVE
e05  C                   ENDIF
     C     PDKY2         READE     PPD01                                  99
e04  C                   ENDDO
b04  C     *IN99         IFEQ      *ON
     C                   SETON                                        9046
     C                   GOTO      CK1END
e04  C                   ENDIF
     C                   Z-ADD     SCFEIN        KKFEIN
     C                   Z-ADD     SCMOD         KKMOD
     C     P1KY          CHAIN     PP101                              99
b04  C     *IN99         IFEQ      *OFF
     C     6             SUBST     P1CNTY:10     XXCNTY            6
     C     'PSA'         SCAN      XXCNTY                                 99
b05  C     *IN99         IFEQ      *ON
     C                   EXFMT     SCR04
b06  C     *INKL         IFEQ      *ON
     C                   SETON                                        90
     C                   GOTO      CK1END
e06  C                   ENDIF
e05  C                   ENDIF
e04  C                   ENDIF
e03  C                   ENDIF
      ********* BEGIN ADD  ******* 07/14/08 ***************************
     C     SCBNPI        IFGT      *BLANK
     C                   Z-ADD     SCFEIN        KKFEIN
     C                   Z-ADD     SCMOD         KKMOD
     C     PDKY          SETLL     PP101
     C     PDKY          READE     PP101                                  99
b03  C     *IN99         DOWEQ     *OFF
b04  C     SCBNPI        IFEQ      P1N#02
     C                   LEAVE
e04  C                   ENDIF
     C     PDKY          READE     PP101                                  99
e03  C                   ENDDO
b03  C     *IN99         IFEQ      *ON
     C                   SETON                                        9059
     C                   GOTO      CK1END
e03  C                   ENDIF
e02  C                   ENDIF
      *
      * Pre-fill NPI# if not entered.
      *
b02  C     SCBNPI        IFEQ      *BLANK
     C                   Z-ADD     SCFEIN        KKFEIN
     C                   Z-ADD     SCMOD         KKMOD
     C     PDKY          SETLL     PP101
     C     PDKY          READE     PP101                                  99
b03  C     *IN99         IFEQ      *OFF
      **** BEGIN ADD  ***** 08/25/08 **********************************
     C     P1N#02        ANDGT     *BLANKS
      **** END ADD    ***** 08/25/08 **********************************
     C                   MOVE      P1N#02        SCBNPI
     C                   SETON                                        90
     C                   GOTO      CK1END
e03  C                   ENDIF
      **** BEGIN ADD  ***** 08/25/08 **********************************
      **** END ADD    ***** 08/25/08 **********************************
e02  C                   ENDIF
      ********* END ADD  ********* 07/14/08 ***************************
     C*
     C*  CHECK FOR CORRECT PROVIDER TYPES 1 AND 4
     C*
     C**DEFAULT ADJUDICATION STATE IF NOT ENTERED
     C*
b03  C     SCAST         IFEQ      *BLANKS
     C                   MOVEL     CMAST         KKAST
     C     STKY          CHAIN     PST02                              99
b04  C     *IN99         IFEQ      *OFF
     C     STINST        ANDEQ     'Y'
     C                   MOVEL     CMAST         SCAST
e04  C                   ENDIF
e03  C                   ENDIF
     C*
b03  C     SCAST         IFEQ      *BLANKS
     C     P1KY          CHAIN     PP101                              99
b04  C     *IN99         IFEQ      *OFF
     C                   MOVEL     P1ST          SCAST
e04  C                   ENDIF
e03  C                   ENDIF
     C                   WRITE     SCR01
     C*
     C*  VALIDATE ADJUDICATION STATE IF ENTERED
     C*
     C*** Beg Add *** 12/24/12 ************************************************
     C                   IF        not(*In42 = '1' and *In55 = '0')
     C                              and *In09 = '0'
     C*** End Add *** 12/24/12 ************************************************
b03  C     SCAST         IFNE      *BLANKS
     C                   MOVEL(P)  SCAST         KKAST
     C     STKY          CHAIN     PST02                              99
b04  C     *IN99         IFEQ      *ON
     C                   SETON                                        9087
     C                   GOTO      CK1END
     C*
x04  C                   ELSE
     C*
     C* See if state is installed
b05  C     ST1A3         IFNE      'Y'
     C*** Beg Add *** 02/11/16 ************************************************
b05  C     ST1A2         OREQ      'Y'
     C*** End Add *** 02/11/16 ************************************************
     C                   SETON                                        9050
     C                   GOTO      CK1END
x05  C                   ELSE
     C                   MOVEL     STNAME        SCSTNM
e05  C                   ENDIF
     C*
e04  C                   ENDIF
e03  C                   ENDIF
     C*** Beg Add *** 12/24/12 ****************************************
     C                   ENDIF
     C*** End Add *** 12/24/12 ****************************************
     C*
     C                   Z-ADD     SCSUB#        KKSUB#
     C                   Z-ADD     SCFND         KKFND
b03  C     XXCLT#        IFEQ      '114'
     C     FNKY          CHAIN     PFN01                              99
b04  C     *IN99         IFEQ      *OFF
b05  C     FNNWCG        IFEQ      'Y'
     C                   MOVEL     'P1'          SCAST
e05  C                   ENDIF
e04  C                   ENDIF
e03  C                   ENDIF
     C*
     C*  PROVIDER STATE IS REQUIRED
     C*
b03  C     SCPOST        IFNE      *BLANKS
     C*
     C*  PROVIDER STATE VALIDATION
     C*
     C                   MOVEL(P)  SCPOST        KKAST
     C     STKY          CHAIN     PST02                              99
b04  C     *IN99         IFEQ      *ON
     C                   SETON                                        9062
     C                   GOTO      CK1END
x04  C                   ELSE
     C                   MOVEL(P)  STNAME        SCPSNM
e04  C                   ENDIF
     C*
e03  C                   ENDIF
     C**DEFAULT*ADJUDICATION STATE OF NOT ENTERED
     C*
     C*  (SYSTEM WILL BE DEFAULTED BASED ON BILL TYPE)
     C*
     C** BILL TYPE IS REQUIRED
     C*
b03  C     SCBTYP        IFEQ      *BLANKS
     C                   SETON                                        9075
     C                   GOTO      CK1END
e03  C                   ENDIF
     C*
     C*  VALIDATE BILL TYPE
     C*
     C                   MOVEL(P)  'BTYP'        KKTBID
     C                   MOVEL(P)  SCBTYP        KKKEY
     C     GTKY          CHAIN     PGT02                              99
b03  C     *IN99         IFEQ      *ON
     C     *IN99         OREQ      *OFF
     C     GTSTAT        ANDEQ     'I'
     C                   SETON                                        9088
     C                   GOTO      CK1END
e03  C                   ENDIF
     C*
     C*  BILL SUB-TYPE MUST BE BLANK IF BILL TYPE IS NOT 'H'
     C*
b03  C     SCBTYP        IFNE      'H'
     C     SCSTYP        ANDNE     *BLANKS
     C                   SETON                                        9083
     C                   GOTO      CK1END
e03  C                   ENDIF
     C*
     C*  VALIDATE BILL SUBTYPE IF ENTERED
     C*
b03  C     SCSTYP        IFNE      *BLANKS
     C                   MOVEL(P)  'STYP'        KKTBID
     C                   MOVEL(P)  SCSTYP        KKKEY
     C     GTKY          CHAIN     PGT02                              99
b04  C     *IN99         IFEQ      *ON
     C                   SETON                                        9089
     C                   GOTO      CK1END
e04  C                   ENDIF
e03  C                   ENDIF
     C*
     C*  VALIDATE ADJUDICATION CODE
     C*
b03  C     SCCSCD        IFNE      *BLANKS
     C                   MOVEL(P)  SCCSCD        KKKEY
     C     TBKY          CHAIN     PCS02                              99
b04  C     *IN99         IFEQ      *ON
     C                   SETON                                        9040
     C                   GOTO      CK1END
e04  C                   ENDIF
e03  C                   ENDIF
     C*
     C** DATE BILL RECEIVED BY CARRIER IS REQUIRED
     C*
b03  C     SCCRDT        IFEQ      *ZEROS
     C                   SETON                                        9077
     C                   GOTO      CK1END
e03  C                   ENDIF
     C*
     C** DATE BILL RECEIVED BY CARRIER MUST BE A VALID DATE
     C*
     C                   Z-ADD     SCCRDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
b03  C     SRERR         IFEQ      '1'
     C                   SETON                                        9079
     C                   GOTO      CK1END
x03  C                   ELSE
     C                   MOVEL     SRDTO         SCCRDT
e03  C                   ENDIF
     C*
     C                   Z-ADD     SCCRDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLCRDT
     C*
     C                   Z-ADD     SCACDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLACDT
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLSYDT
     C*
     C** DATE BILL RECEIVED CANNOT BE LESS THAN DOI
     C*
b03  C     HLCRDT        IFNE      *ZEROS
     C     HLCRDT        ANDLT     HLACDT
     C                   SETON                                        9074
     C                   GOTO      CK1END
e03  C                   ENDIF
     C*
     C** DATE BILL RECEIVED CANNOT BE GREATER THAN SYSTEM DATE
     C*
b03  C     HLCRDT        IFNE      *ZEROS
     C     HLCRDT        ANDGT     HLSYDT
     C                   SETON                                        9076
     C                   GOTO      CK1END
e03  C                   ENDIF
     C*
     C*
     C* REQUIRE CARRIER SENT DATE IF ADJUDICATION STATE IS KENTUCKY
     C* OR MICHIGAN
     C*
b03  C     SCCSDT        IFEQ      *ZEROS
b04  C     SCAST         IFEQ      'KY'
     C     SCAST         OREQ      'MI'
     C     SCAST         OREQ      'TX'
     C****** Beg Add ***** 08/21/14 *****************************
     C     SCAST         OREQ      'NC'
     C****** End Add ***** 08/21/14 *****************************
     C                   SETON                                        9048
     C                   GOTO      CK1END
e04  C                   ENDIF
e03  C                   ENDIF
     C*
     C** DATE BILL SENT TO CARRIER MUST BE A VALID DATE IF ENTERED
     C*
b03  C     SCCSDT        IFNE      *ZEROS
     C                   Z-ADD     SCCSDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
b04  C     SRERR         IFEQ      '1'
     C                   SETON                                        9080
     C                   GOTO      CK1END
x04  C                   ELSE
     C                   MOVEL     SRDTO         SCCSDT
e04  C                   ENDIF
e03  C                   ENDIF
e02  C                   ENDIF
     C*
     C                   Z-ADD     SCCSDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLCSDT
     C*
     C** SENT TO DATE CANNOT BE LESS THAN DOI
     C*
b02  C     HLCSDT        IFNE      *ZEROS
     C     HLCSDT        ANDLT     HLACDT
     C                   SETON                                        9085
     C                   GOTO      CK1END
e02  C                   ENDIF
     C*
     C** DATE BILL SENT CANNOT BE GREATER THAN SYSTEM DATE
     C*
b02  C     HLCSDT        IFNE      *ZEROS
     C     HLCSDT        ANDGT     HLSYDT
     C                   SETON                                        9043
     C                   GOTO      CK1END
e02  C                   ENDIF
     C*
     C* SENT TO CARRIER DATE CANNOT BE GREATER THAN RECEIVED BY CARRIER
     C*
b02  C     SCCSDT        IFNE      *ZEROS
     C     SCCRDT        ANDNE     *ZEROS
     C*
b03  C     HLCSDT        IFGT      HLCRDT
     C                   SETON                                        9041
     C                   GOTO      CK1END
e03  C                   ENDIF
e02  C                   ENDIF
     C*
e01  C                   ENDIF
     C*
     C*** Beg Add *** 10/01/13 *********************************************
      /Free
         //Beg Chg ***** 08/13/14 *******************************
         //If bill is audited then do not process further.
         If @@mode = 'C';
            Chain @@rrn DBRCDF;
              If %found(faeop) and EOpauf = 'A';
                 indAud = *on;
                 *in90 = *on;
                 LeaveSr;
              EndIf;
         EndIf;
         //End Chg ***** 08/13/14 *******************************
      /End-Free
      *
      * Pre-fill NPI# if not entered or FEIN/Mod has changed
      *
     C                   Z-ADD     SCFEIN        KKFEIN
     C                   Z-ADD     SCMOD         KKMOD
     C     PDKY          SETLL     PP101
     C     PDKY          READE     PP101                                  99
b03  C     *IN99         IFEQ      *OFF
b03  C     P1N#02        ANDNE     SCBNPI
     C                   MOVE      P1N#02        SCBNPI
     C                   SETON                                        90
     C                   GOTO      CK1END
e03  C                   ENDIF
     C*** End Add *** 10/01/13 *********************************************
     C*
b03  C     CLTID         IFEQ      '114'
     C*
     C     @@MODE        IFEQ      'A'
     C     @@MODE        OREQ      'C'
     C*
     C                   EXSR      MSG114
     C*
e03  C                   ENDIF
     C*
e02  C                   ENDIF
     C*
     C*
     C*
     C***VERIFY*THAT A PROVIDER BILL CONTROL RECORD IS IN SYSTEM FOR
     C***THE*AST, BTYP, STYP (SYS DEFAULTED) ENTERED.
     C**N99*****           MOVELGTFG01    KKSYS
     C*
     C** TOTAL CHARGE CANNOT BE ZERO
     C*
b01  C     SCCHRG        IFEQ      *ZEROS
     C                   SETON                                        9073
     C                   GOTO      CK1END
e01  C                   ENDIF
     C*
     C**CA Specific
     C**If bill type = H (hospital), provider required.
     C**CA: Validate facility provider
     C**CA: Rendering provider req'd
     C**CA: Billing provider unique bill ID req'd
     C**CA: Billing prov state license #
     C*
     C* IF GROUP IS EXPIRED, DO NOT ALLOW ADD
     C*
b01  C     @@MODE        IFEQ      'A'
     C     XXUPLD        OREQ      'Y'
     C                   Z-ADD     SCSUB#        KKSUB#
     C                   Z-ADD     SCFND         KKFND
     C     FNKY          CHAIN     PFN01                              99
b02  C     *IN99         IFEQ      *OFF
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLSYDT
e03  C     FNEXDT        IFNE      *ZEROS
     C     FNEXDT        ANDLT     HLSYDT
     C                   SETON                                        9039
     C                   GOTO      CK1END
e03  C                   ENDIF
e02  C                   ENDIF
e01  C                   ENDIF
     C*
     C* IF CLAIM IS CONTROVERTED DON'T ALLOW MEDICAL PAYMENTS
     C* IF IN CHANGE MODE RETRIEVE CONTROVERT FLAG FROM CLAIM
     C*
b01  C     @@MODE        IFEQ      'C'
     C     XXUPLD        OREQ      'Y'
     C*
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVE      SCSS#         KKSS#
      /Free
         kkss# = UnMaskData('SSN':'EOSS#':scss#);
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C                   Z-ADD     SCACDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKACDT
     C     CMKY1         CHAIN     PCM02                              99
     C*
e01  C                   ENDIF
     C*
b01  C     @@MODE        IFEQ      'A'
     C     CMCVFL        ANDEQ     'Y'
     C     @@MODE        OREQ      'C'
     C     CMCVFL        ANDEQ     'Y'
     C     XXUPLD        OREQ      'Y'
     C     CMCVFL        ANDEQ     'Y'
     C*
     C* WCMS MUST BE INSTALLED AND NO NON-PDS WC
     C*
b02  C     ##WCIF        IFEQ      'Y'
     C     ##WCIN        ANDEQ     'Y'
     C     ##NPWC        ANDNE     'Y'
     C*
     C* IF CASE IS LINKED
     C*
     C                   MOVE      ##CO#         #2CO#
     C                   MOVE      SCSUB#        #2SUB#
     C                   MOVE      SCFND         #2FND
     C*
     C                   CALL      'FA701R'
     C                   PARM      *BLANKS       ##RTCD
     C                   PARM                    #2CO#
     C                   PARM                    #2SUB#
     C                   PARM                    #2FND
     C                   PARM      *BLANKS       ##OUTS            1
     C*
b03  C     ##RTCD        IFEQ      *BLANKS
     C     ##OUTS        ANDNE     'Y'
     C*
     C* GET WCMS FUND
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       ##RTCD
     C                   PARM                    #2CO#
     C                   PARM      *BLANKS       #2WCF#            3
     C                   PARM                    #2SUB#
     C                   PARM                    #2FND
     C*
b04  C     ##RTCD        IFEQ      *BLANKS
     C*
     C                   MOVE      #2WCF#        ##WCF#
     C*
     C                   CALL      'WA115R'                             99
     C                   PARM                    ##CO#
     C                   PARM                    ##WCF#            3 0
     C                   PARM      SCFYR         ##FYR
     C                   PARM      SCCASE        ##CASE
     C                   PARM      *BLANKS       ##EXMF            1
     C                   PARM      *BLANKS       ##RTCD
     C*
b05  C     *IN99         IFEQ      *OFF
b06  C     ##EXMF        IFEQ      'Y'
     C                   SETON                                        5490
     C                   GOTO      CK1END
e06  C                   ENDIF
e05  C                   ENDIF
     C*
     C* A BILL CANNOT BE ADDED ON A CONTROVERTED CLAIM WHEN THE GROUP
     C* DOES NOT ALLOW MEDICAL PAYMENTS
     C*
     C                   CALL      'WA116R'
     C                   PARM                    ##CO#
     C                   PARM      ##WCF#        ##FND
     C                   PARM      *BLANK        ##ALMP            1
     C                   PARM      *BLANK        ##RTCD
     C*
b05  C     ##RTCD        IFEQ      *BLANK
     C     ##ALMP        ANDNE     'Y'
     C*
     C                   CALL      'FA720R'
     C                   PARM      '5'           ##MGID            1
     C                   PARM      CMCO#         ##CO#
     C                   PARM      CMSUB#        ##SUB#
     C                   PARM      CMFND         ##FND
     C     *INKL         PARM      *OFF          ##KL              1
     C*
b06  C     *INKL         IFEQ      *ON
     C                   SETON                                        9038
     C                   GOTO      CK1END
e06  C                   ENDIF
     C*
e05  C                   ENDIF
     C*
e04  C                   ENDIF
e03  C                   ENDIF
e02  C                   ENDIF
     C*
e01  C                   ENDIF
     C*
     C* IF EMPLOYER POLICY YEAR IS SET TO EXCLUDE MEDICAL, THEN STOP IT
     C*
b01  C     @@MODE        IFEQ      'A'
     C     XXUPLD        OREQ      'Y'
     C*
     C* WCMS MUST BE INSTALLED AND NO NON-PDS WC
     C*
b02  C     ##WCIF        IFEQ      'Y'
     C     ##WCIN        ANDEQ     'Y'
     C     ##NPWC        ANDNE     'Y'
     C*
     C* IF CASE IS LINKED
     C*
     C                   MOVE      ##CO#         #2CO#
     C                   MOVE      SCSUB#        #2SUB#
     C                   MOVE      SCFND         #2FND
     C*
     C                   CALL      'FA701R'
     C                   PARM      *BLANKS       ##RTCD
     C                   PARM                    #2CO#
     C                   PARM                    #2SUB#
     C                   PARM                    #2FND
     C                   PARM      *BLANKS       ##OUTS            1
     C*
b03  C     ##RTCD        IFEQ      *BLANKS
     C     ##OUTS        ANDNE     'Y'
     C*
     C* GET WCMS FUND
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       ##RTCD
     C                   PARM                    #2CO#
     C                   PARM      *BLANKS       #2WCF#            3
     C                   PARM                    #2SUB#
     C                   PARM                    #2FND
     C*
b04  C     ##RTCD        IFEQ      *BLANKS
     C*
     C                   MOVE      #2WCF#        ##WCF#
     C*
     C                   CALL      'WA115R'                             99
     C                   PARM                    ##CO#
     C                   PARM                    ##WCF#            3 0
     C                   PARM      SCFYR         ##FYR
     C                   PARM      SCCASE        ##CASE
     C                   PARM      *BLANKS       ##EXMF            1
     C                   PARM      *BLANKS       ##RTCD
     C*
b05  C     *IN99         IFEQ      *OFF
b06  C     ##EXMF        IFEQ      'Y'
     C                   SETON                                        5490
     C                   GOTO      CK1END
e06  C                   ENDIF
e05  C                   ENDIF
     C*
e04  C                   ENDIF
e03  C                   ENDIF
e02  C                   ENDIF
e01  C                   ENDIF
     C*
     C* A BILL CANNOT BE ADDED FOR A CASE THAT HAS BEEN DENIED
     C*
b01  C     @@MODE        IFEQ      'A'
     C     XXUPLD        OREQ      'Y'
     C     CMKY          CHAIN     PCM01                              99
b02  C     *IN99         IFEQ      *OFF
b03  C     CMDNY         IFEQ      'Y'
     C                   CALL      'FA720R'
     C                   PARM      '3'           ##MGID            1
     C                   PARM      CMCO#         ##CO#
     C                   PARM      CMSUB#        ##SUB#
     C                   PARM      CMFND         ##FND
     C     *INKL         PARM      *OFF          ##KL              1
     C*
b04  C     *INKL         IFEQ      *ON
     C*
     C                   SETOFF                                       KL
     C                   SETON                                        9045
     C                   GOTO      CK1END
     C*
e04  C                   ENDIF
     C*
e03  C                   ENDIF
e02  C                   ENDIF
e01  C                   ENDIF
     C*
     C*  DISPLAY WINDOW INDICATION HOW MANY OTHER BILLS WITH SAME
     C*  SS#/ACDT/FEIN ARE PENDED IF IN ADD MODE
     C*
b01  C     @@MODE        IFEQ      'A'
     C     XXUPLD        OREQ      'Y'
     C                   Z-ADD     *ZEROS        XXCNT             1 0
     C     EOKY1         CHAIN     PEO02                              99
b02  C     *IN99         DOWEQ     *OFF
     C                   ADD       1             XXCNT
     C     EOKY1         READE     PEO02                                  99
e02  C                   ENDDO
b02  C     XXCNT         IFGT      *ZEROS
     C                   Z-ADD     XXCNT         SCCNT
     C                   EXFMT     WDW01
b03  C     *INKL         IFEQ      *ON
     C                   SETON                                        90
     C                   GOTO      CK1END
e03  C                   ENDIF
e02  C                   ENDIF
e01  C                   ENDIF
     C*
     C*
     C* DO NOT ALLOW USER TO PROCESS THIS BILL IF ANOTHER USER IS
     C* WORKING ON A BILL FOR THIS CASE
     C*
b01  C     @@MODE        IFEQ      'A'
     C     @@MODE        OREQ      'C'
     C     LOKY          CHAIN(N)  PLO01                              99
b02  C     *IN99         IFEQ      *OFF
     C                   SETON                                        9044
     C                   GOTO      CK1END
e02  C                   ENDIF
e01  C                   ENDIF
     C*
     C*
b01  C     @@MODE        IFEQ      'A'
     C     XXUPLD        OREQ      'Y'
     C     *IN91         OREQ      *OFF
     C*
     C* CALL MULTIPLE MODIFIER SEARCH (WILL GET NETWORK CODE)
     C*
     C*
b02  C     ##NTWK        IFEQ      'Y'
     C*** Beg Add *** 06/12/15 ****************************************
b02  C     ##PARN        ANDEQ     'P'
     C*** End Add *** 06/12/15 ****************************************
     C*
     C                   MOVEL     'P'           ##TYPE
     C*
     C     PDKY2         CHAIN     PPD01                              99
     C*
b03  C     *IN99         DOWEQ     *OFF
     C*
b04  C     PDPARN        IFEQ      'P'
     C     PDSTAT        ANDEQ     'A'
     C     PDPARN        OREQ      'P'
     C     PDSTAT        ANDEQ     *BLANKS
     C*
     C                   LEAVE
     C*
e04  C                   ENDIF
     C*
     C     PDKY2         READE     PPD01                                  99
     C*
b04  C     *IN99         IFEQ      *ON
     C*
     C                   MOVEL     *BLANKS       ##TYPE
     C*
e04  C                   ENDIF
     C*
e03  C                   ENDDO
     C*
x02  C                   ELSE
     C*
     C*** Beg Chg *** 06/12/15 ****************************************
     C*****************  MOVEL     *BLANKS       ##TYPE
     C                   MOVEL     ##PARN        ##TYPE
     C*** End Chg *** 06/12/15 ****************************************
     C*
e02  C                   ENDIF
     C*
     C                   MOVE      SCFEIN        ##FEIN
     C                   Z-ADD     SCFEIN        HLFEIN
     C                   Z-ADD     SCMOD         HLMOD
     C                   MOVE      SCMOD         ##MOD
     C*   Determine the revision indicator,If blank load from
     C*   the BEAST record
     C*
     C*   BEAST key build
     C*
     C                   MOVE(P)   SCAST         K1AST
     C*
     C                   MOVEL(P)  'BTYP'        KKTBID
     C                   MOVEL(P)  SCBTYP        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C  N99              MOVEL     GTFG01        KKSYS
     C                   MOVE      SCBTYP        KKBTYP
     C                   MOVE      *BLANKS       KKSTYP
     C*
     C     BEKY          CHAIN     PBE01                              98
     C*
b02  C     *IN98         IFEQ      *OFF
     C                   MOVE      BEREVI        XXREVI            1
x02  C                   ELSE
     C                   MOVE      EOREVI        XXREVI
e02  C                   ENDIF
     C*
b02  C     SCBNPI        IFEQ      *BLANK
     C     SCAST         ANDEQ     'FL'
     C     SCBTYP        ANDEQ     'H'
     C     XXREVI        ANDGE     'E'
     C     @@MODE        ANDEQ     'A'
     C                   EXFMT     WDW02
     C                   SETON                                        9058
     C                   SETOFF                                       17
     C                   GOTO      CK1END
e02  C                   ENDIF
b02  C     @@MODE        IFEQ      'A'
     C     XXUPLD        OREQ      'Y'
     C*
      /Free
       //*** Begin Add *** 04/12/18 ***************************************
       If isModEnabled(Mod837P) and isModEnabled(Mod837I);
      /End-Free
      *
     C                   CALL      'F0032R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##FEIN            9
     C                   PARM                    ##MOD             3
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    ##TYPE            1
     C                   PARM      *BLANKS       @@1N4             1
     C                   PARM      *BLANKS       QQ1PRV            1
     C                   PARM                    #1PRIV
     C                   PARM                    QQST
     C                   PARM      DBRRN4        ##RRN4
     C                   ELSE
      ****** End   Add *** 04/12/18 ***************************************
     C                   CALL      'F0032R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##FEIN            9
     C                   PARM                    ##MOD             3
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    ##TYPE            1
     C                   PARM      *BLANKS       @@1N4             1
     C                   PARM      *BLANKS       QQ1PRV            1
     C                   PARM                    #1PRIV
      /Free
       //*** Begin Add *** 04/12/18 ***************************************
       Endif;
       //*** End   Add *** 04/12/18 ***************************************
      /End-Free
     C*
b03  C     ##FEIN        IFNE      *BLANKS
     C*
     C                   MOVE      ##FEIN        ZZFEIN
     C*
x03  C                   ELSE
     C*
     C                   Z-ADD     *ZEROS        ZZFEIN
     C*
e03  C                   ENDIF
     C*
b03  C     ##MOD         IFNE      *BLANKS
     C*
     C                   MOVE      ##MOD         ZZMOD
     C*
x03  C                   ELSE
     C*
     C                   Z-ADD     *ZEROS        ZZMOD
     C*
e03  C                   ENDIF
     C*
b03  C     ##RTCD        IFEQ      'A'
     C                   Z-ADD     HLFEIN        SCFEIN
     C                   Z-ADD     HLMOD         SCMOD
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *INKC
     C     LOKY          CHAIN     PLO01                              99
b04  C     *IN99         IFEQ      *OFF
     C                   DELETE    PLO01
e04  C                   ENDIF
     C                   GOTO      CK1END
e03  C                   ENDIF
     C*
b03  C     ##RTCD        IFEQ      'B'
     C     ZZFEIN        OREQ      *ZEROS
     C     ZZMOD         ANDEQ     *ZEROS
     C                   Z-ADD     HLFEIN        SCFEIN
     C                   Z-ADD     HLMOD         SCMOD
     C                   MOVE      *ON           *IN90
     C     LOKY          CHAIN     PLO01                              99
b04  C     *IN99         IFEQ      *OFF
     C                   DELETE    PLO01
e04  C                   ENDIF
     C                   GOTO      CK1END
e03  C                   ENDIF
x02  C                   ELSE
     C                   Z-ADD     SCFEIN        ZZFEIN
     C                   Z-ADD     SCMOD         ZZMOD
b03  C     SCFEIN        IFEQ      HDFEIN
     C     SCMOD         ANDEQ     HDMOD
     C                   SETON                                        91
e03  C                   ENDIF
e02  C                   ENDIF
     C*
b02  C     ZZFEIN        IFNE      SCFEIN
     C     ZZMOD         ORNE      SCMOD
     C                   MOVE(P)   ZZFEIN        SCFEIN
     C                   MOVE(P)   ZZMOD         SCMOD
     C*** Beg Add *** 06/19/15 ********************************************
     C* If the FEIN or Modifier Changed then reload the NPI#
     C                   Eval      kkFEIN = scFEIN
     C                   Eval      kkMod  = scMod
     C     pdKY          Chain     pp101                              99
     C     *in99         Ifeq      *off
     C                   Eval      scBNPI = P1N#02
     C                   Endif
     C*** End Add *** 06/19/15 ********************************************
e02  C                   ENDIF
     C*
e01  C                   ENDIF
     C*
     C* If the bill was uploaded with provider mod 0 and the user
     C* changed it then update FAEAP and FDCAP
     C*
b01  C     XXUPLD        IFEQ      'Y'
     C     *IN91         OREQ      *OFF
     C                   EXSR      UPDMOD
e01  C                   ENDIF
     C*
     C                   Z-ADD     SCFEIN        KKFEIN
     C                   Z-ADD     SCMOD         KKMOD
     C     PDKY          CHAIN     PPD01                              99
b01  C     *IN99         IFEQ      *ON
     C                   SETON                                        9086
     C                   GOTO      CK1END
     C*
x01  C                   ELSE
b02  C     PDSTAT        IFNE      'A'
     C     PDSTAT        ANDNE     *BLANKS
     C                   SETON                                        9046
     C                   GOTO      CK1END
e02  C                   ENDIF
     C*
b02  C     PDTYPE        IFNE      '1'
     C     PDTYPE        ANDNE     '4'
     C                   SETON                                        9047
     C                   GOTO      CK1END
e02  C                   ENDIF
b02  C     @@MODE        IFEQ      'C'
     C     XXUPLD        OREQ      'Y'
     C                   Z-ADD     SCFEIN        KKFEIN
     C                   Z-ADD     SCMOD         KKMOD
     C     P1KY          CHAIN     PP101                              99
b03  C     *IN99         IFEQ      *OFF
     C     6             SUBST     P1CNTY:10     XXCNTY            6
     C     'PSA'         SCAN      XXCNTY                                 99
b04  C     *IN99         IFEQ      *ON
     C                   EXFMT     SCR04
b05  C     *INKL         IFEQ      *ON
     C                   SETON                                        90
     C                   GOTO      CK1END
e05  C                   ENDIF
e04  C                   ENDIF
e03  C                   ENDIF
e02  C                   ENDIF
     C*
      *
b02  C     CLTID         IFEQ      '114'
     C                   MOVE      ' '           XXNDPS            1
     C                   MOVE      ' '           XXCMPL            1
     C     EOKY          SETLL     PES01
     C     EOKY          READE     PES01                                  99
b03  C     *IN99         DOWEQ     *OFF
b04  C     ESBLST        IFEQ      'C'
     C                   MOVE      'C'           XXCMPL
     C                   LEAVE
e04  C                   ENDIF
     C     EOKY          READE     PES01                                  99
e03  C                   ENDDO
b03  C     XXCMPL        IFEQ      ' '
b04  C     PDPARN        IFNE      'P'
     C                   WRITE     SCR01
     C                   EXFMT     WDW03
     C                   MOVE      'Y'           XXNDPS
e04  C                   ENDIF
e03  C                   ENDIF
e02  C                   ENDIF
      *
     C*
     C* DEFAULT ADJUDICATION STATE OF NOT ENTERED
     C*
b02  C     SCAST         IFEQ      *BLANKS
     C                   MOVEL(P)  CMAST         KKAST
     C     STKY          CHAIN     PST02                              99
b03  C     *IN99         IFEQ      *OFF
     C     STINST        ANDEQ     'Y'
     C                   MOVEL     CMAST         SCAST
e03  C                   ENDIF
e02  C                   ENDIF
     C*
b02  C     SCAST         IFEQ      *BLANKS
     C     P1KY          CHAIN     PP101                              99
b03  C     *IN99         IFEQ      *OFF
     C                   MOVEL     P1ST          SCAST
e03  C                   ENDIF
e02  C                   ENDIF
     C*
     C*  VALIDATE ADJUDICATION STATE IF ENTERED
     C*
     C*** Beg Add *** 12/24/12 ************************************************
     C                   IF        not(*In42 = '1' and *In55 = '0')
     C                             and *In09 = '0'
     C*** End Add *** 12/24/12 ************************************************
     C                   MOVEL(P)  SCAST         KKAST
     C     STKY          CHAIN     PST02                              99
b02  C     *IN99         IFEQ      *ON
     C                   SETON                                        9087
     C                   GOTO      CK1END
x02  C                   ELSE
     C*
     C* See if state is installed
     C*
b03  C     ST1A3         IFNE      'Y'
     C*** Beg Add *** 02/11/16 ************************************************
b05  C     ST1A2         OREQ      'Y'
     C*** End Add *** 02/11/16 ************************************************
     C                   SETON                                        9050
     C                   GOTO      CK1END
e03  C                   ENDIF
e02  C                   ENDIF
     C*** Beg Add *** 12/24/12 ****************************************
     C                   ENDIF
     C*** End Add *** 12/24/12 ****************************************
     C*
     C*  VERIFY THAT A PROVIDER BILL CONTROL RECORD IS IN SYSTEM FOR
     C*  THE AST, BTYP, STYP (SYS DEFAULTED) ENTERED.
     C*
     C                   MOVE(P)   SCAST         K1AST
     C*
     C                   MOVEL(P)  'BTYP'        KKTBID
     C                   MOVEL(P)  SCBTYP        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C  N99              MOVEL     GTFG01        KKSYS
     C*
     C                   MOVEL(P)  SCBTYP        KKBTYP
     C                   MOVEL(P)  SCSTYP        KKSTYP
     C*
     C     BEKY          CHAIN     PBE01                              99
b02  C     *IN99         IFEQ      *ON
     C                   SETON                                        9084
     C                   GOTO      CK1END
e02  C                   ENDIF
     C*
     C     FNKY          CHAIN     PF101                              99
     C*
     C                   MOVEL     *BLANKS       GRPNWK
     C*
b02  C     SCBTYP        IFEQ      'H'
     C*
     C                   MOVEL(P)  'H'           XXSYS             1
     C*
x02  C                   ELSE
     C*
     C                   MOVEL(P)  'P'           XXSYS
     C*
e02  C                   ENDIF
     C*
b02  C     F1NW01        IFNE      *BLANKS
     C     F1NW02        ORNE      *BLANKS
     C     F1NW03        ORNE      *BLANKS
     C     F1NW04        ORNE      *BLANKS
     C     F1NW05        ORNE      *BLANKS
     C     F1NW06        ORNE      *BLANKS
     C     F1NW07        ORNE      *BLANKS
     C     F1NW08        ORNE      *BLANKS
     C     F1NW09        ORNE      *BLANKS
     C     F1NW10        ORNE      *BLANKS
     C*
     C     PDKY          CHAIN     PS401                              99
     C*
b03  C     *IN99         DOWEQ     *OFF
b04  C     S4STAT        IFEQ      'A'
     C*
b05  C     XXSYS         IFEQ      S4SYS
     C*
b06  C     F1NW01        IFEQ      S4NWCD
     C     F1NW02        OREQ      S4NWCD
     C     F1NW03        OREQ      S4NWCD
     C     F1NW04        OREQ      S4NWCD
     C     F1NW05        OREQ      S4NWCD
     C     F1NW06        OREQ      S4NWCD
     C     F1NW07        OREQ      S4NWCD
     C     F1NW08        OREQ      S4NWCD
     C     F1NW09        OREQ      S4NWCD
     C     F1NW10        OREQ      S4NWCD
     C*
     C                   MOVEL     'Y'           GRPNWK            1
     C                   LEAVE
     C*
e06  C                   ENDIF
     C*
e05  C                   ENDIF
e04  C                   ENDIF
     C*
     C     PDKY          READE     PS401                                  99
     C*
e03  C                   ENDDO
     C*
e02  C                   ENDIF
     C*
b02  C     PDPARN        IFEQ      'P'
     C     @@MODE        ANDEQ     'A'
     C     ##NTWK        ANDEQ     'Y'
     C     GRPNWK        ANDEQ     'Y'
     C     PDPARN        OREQ      'P'
     C     XXUPLD        ANDEQ     'Y'
     C     ##NTWK        ANDEQ     'Y'
     C     GRPNWK        ANDEQ     'Y'
     C     PDPARN        OREQ      'P'
     C     *IN91         ANDEQ     *OFF
     C     ##NTWK        ANDEQ     'Y'
     C     GRPNWK        ANDEQ     'Y'
     C*
     C                   CALL      'N0037R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM                    ##FEIN            9
     C                   PARM                    ##MOD             3
     C                   PARM      '*SPEC'       ##NWCD            5
     C                   PARM      *BLANKS       ##CNT#           15
     C                   PARM      SCSUB#        ##SUB#            5 0
     C                   PARM      SCFND         ##FND             3 0
     C                   PARM      XXSYS         ##TYPE            1
     C                   PARM      *BLANKS       ##FSFL            1
     C                   PARM      SCAST         #1AST             2
     C                   PARM      *BLANKS       ##DSTY            2
     C*
     C*
     C                   MOVE      ' '           XXDPCT            1
b03  C     *INKL         IFEQ      *ON
     C*
     C                   SETON                                        90
     C*
     C     LOKY          CHAIN     PLO01                              99
     C*
b04  C     *IN99         IFEQ      *OFF
     C*
     C                   DELETE    PLO01
     C*
e04  C                   ENDIF
     C*
     C                   GOTO      CK1END
     C                   ELSE
b04  C     CLTID         IFEQ      '114'
b05  C     ##NWCD        IFEQ      *BLANKS
     C                   MOVE      'Y'           XXDPCT
e05  C                   ENDIF
e04  C                   ENDIF
e03  C                   ENDIF
     C*
e02  C                   ENDIF
     C*
e01  C                   ENDIF
     C*
     C* CALL PRR-CERT SELECTION PROGRAM IF IN UPLOAD MODE AND PRECERT
     C* IS INSTALLED AND PRE-CERT RECORDS EXIST FOR THIS CASE
     C*
b01  C     XXUPLD        IFEQ      'Y'
     C     ##UR          ANDEQ     'Y'
     C     EOCERT        ANDEQ     *BLANKS
     C     EORCN         ANDEQ     *ZEROS
     C*
     C     PCKY          CHAIN     PPC02                              99
     C*
b02  C     *IN99         IFEQ      *OFF
     C*
     C     CMKY          CHAIN     PCM01                              99
     C                   CALL      'U0060R'
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      '2'           ##CALL
     C                   PARM      DBRRN         ##RRN
     C*
b03  C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PPC01                              98
     C*
b04  C     *IN98         IFEQ      *OFF
     C                   MOVEL(P)  PCCERT        ##CERT
     C                   Z-ADD     PCEXT         ##EXT
e04  C                   ENDIF
     C*
e03  C                   ENDIF
     C*
e02  C                   ENDIF
     C*
e01  C                   ENDIF
     C*
     C* DO NOT ALLOW USER TO PROCESS THIS BILL IF ANOTHER USER IS
     C* WORKING ON A BILL FOR THIS CASE
     C* THIS IS DONE A SECOND TIME JUST IN CASE THE USER SPENT A
     C* SIGNIFICANT AMOUNT OF TIME SELECTING MODIFIER OR NETWORK
     C*
b01  C     @@MODE        IFEQ      'A'
     C     @@MODE        OREQ      'C'
     C     LOKY          CHAIN(N)  PLO01                              99
b02  C     *IN99         IFEQ      *OFF
     C                   SETON                                        9044
     C                   GOTO      CK1END
e02  C                   ENDIF
e01  C                   ENDIF
     C*
     C*  SEE IF STAND ALONE (NOT LINK W/ WC)
     C*
     C                   MOVE      ##CO#         #@CO#
     C                   MOVE      SCSUB#        #@SUB#
     C                   MOVE      ##FND         #@FND
     C*
     C                   CALL      'FA701R'      INSPL
     C*
b01  C     ##OUTS        IFEQ      'Y'
     C                   SETON                                        16
x01  C                   ELSE
     C                   SETOFF                                       16
e01  C                   ENDIF
     C*
     C                   MOVE      *BLANKS       XXLTFL            1
     C     CMKY          CHAIN     PCM01                              99
     C  N99              MOVEL     CMLTFL        XXLTFL
     C*
b01  C     @@MODE        IFEQ      'C'
     C     *IN16         ANDEQ     *ON
     C     XXLTFL        ANDEQ     'R'
     C                   SETON                                        70
     C                   SETON                                        9038
     C                   GOTO      CK1END
e01  C                   ENDIF
         //*** Beg Add *** 12/04/17 **************************************
      /Free
         // Update the R&C flag in FDCAP and Header files if bill was just
         // loaded via the 837 Bill Import
                         If (isModEnabled('837 ASC X12 Prof')
                            or isModEnabled('837 ASC X12 Inst'));
                            Chain ESky2 pes01;
                            If %Found (faesl2) and esStCd = '837';
                               Reade ESky2 pes01;
                               If %Eof (faesl2);
                                  Exsr srUpdRcFl;
                               Endif;
                            Endif;
                         Endif;
      /End-Free
         //*** End Add *** 12/04/17 **************************************
     C*
     C** WRITE LOCKOUT RECORD IF IN CHANGE OR ADD MODE
     C*
b01  C     @@MODE        IFEQ      'A'
     C     @@MODE        OREQ      'C'
     C                   Z-ADD     ##CO#         LOCO#
     C                   Z-ADD     SCSUB#        LOSUB#
     C                   Z-ADD     SCFND         LOFND
     C                   Z-ADD     SCFYR         LOFYR
     C                   Z-ADD     SCCASE        LOCASE
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVEL     SCSS#         LOSS#
      /Free
         LOSS# = UnMaskData('SSN':'EOSS#':scss#);
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C*
     C                   Z-ADD     SCACDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LOACDT
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        LOTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LODT
     C*
     C                   MOVEL     Q1USER        LOUSER
     C                   MOVEL     Q1PGM         LOPGM
     C                   WRITE     PLO01
     C*
e01  C                   ENDIF
     C*
     C     CK1END        ENDSR
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
     C* KFDO - F6=CASE PROVIDERS
     C*================================================================
     C     KFDO          BEGSR
     C*
     C*
     C*** INITIALIZE POSITION CURSOR INDICATORS
     C*
     C                   SETOFF                                       303132
     C*
     C* DETERMINE IF SHOULD CALL AS A SEARCH OR AS DISPLAY
     C*
     C     HFLD          IFEQ      'SCFEIN'
     C     @@MODE        ANDEQ     'A'
     C                   MOVE      '2'           ##CALL
     C                   ELSE
     C                   MOVE      ' '           ##CALL
     C                   ENDIF
     C*
     C                   MOVE      ' '           ##KC
     C                   MOVE      ' '           ##KL
     C*
     C                   Z-ADD     SCSUB#        KKSUB#
     C                   Z-ADD     SCFND         KKFND
     C                   Z-ADD     SCFYR         KKFYR
     C                   Z-ADD     SCCASE        KKCASE
     C*
     C     CMKY          CHAIN     PCM01                              99
     C                   Z-ADD     DBRRN         ##RRN1
     C*
     C                   Z-ADD     0             ##RRN
     C*
     C                   CALL      'F0277R'
     C     *INKC         PARM                    ##KC
     C                   PARM                    ##KL
     C                   PARM                    ##CALL
     C                   PARM                    ##RRN
     C                   PARM                    ##RRN1
     C*
     C* PROCESS SELECTION IF CALLED AS A SEARCH
     C*
     C     HFLD          IFEQ      'SCFEIN'
     C     HFLD          OREQ      'SCMOD '
     C*
     C     ##KL          IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        9031
     C                   ELSE
     C     ##RRN         CHAIN     PPP01                              99
     C                   Z-ADD     PPFEIN        SCFEIN
     C                   Z-ADD     PPMOD         SCMOD
     C                   Z-ADD     SCFEIN        KKFEIN
     C                   Z-ADD     SCMOD         KKMOD
     C     PDKY          CHAIN     PPD01                              99
     C                   MOVEL     PDNAME        SCPNAM
     C                   SETON                                        9032
     C                   ENDIF
     C                   ELSE
     C                   SETON                                        90
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=SEARCH.
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SETON                                        90
     C     @@MODE        IFNE      'D'
     C*
     C*
     C*** INITIALIZE POSITION CURSOR INDICATORS
     C*
     C                   SETOFF                                       29
     C                   SETOFF                                       303132
     C                   SETOFF                                       333435
     C                   SETOFF                                       363738
     C                   SETOFF                                       515253
     C                   SETOFF                                       63
     C*
     C*** CLEAR WORK FIELDS
     C*
     C                   CLEAR                   HLFEIN
     C                   CLEAR                   HLMOD
     C                   CLEAR                   ##FEIN
     C                   CLEAR                   ##MOD
     C                   CLEAR                   KKFEIN
     C                   CLEAR                   KKMOD
     C*
     C*
     C     *IN91         IFEQ      *ON
     C                   SELECT
     C*
     C*** SS# - ACCIDENT DATE SEARCH
     C*
     C*
     C     HFLD          WHENEQ    'SCSS#'
     C     *IN17         ANDEQ     *OFF
     C     HFLD          OREQ      'SCACDT'
     C     *IN17         ANDEQ     *OFF
     C     HFLD          OREQ      'SCSUB#'
     C     *IN17         ANDEQ     *ON
     C     HFLD          OREQ      'SCFND'
     C     *IN17         ANDEQ     *ON
     C     HFLD          OREQ      'SCFYR'
     C     *IN17         ANDEQ     *ON
     C     HFLD          OREQ      'SCCASE'
     C     *IN17         ANDEQ     *ON
     C                   MOVE      '2'           ##CALL
     C                   Z-ADD     0             ##SUB#            5 0
     C                   Z-ADD     0             #1FND             3 0
     C                   MOVE      *BLANKS       ##FYR             3 0
     C                   MOVE      *BLANKS       ##CASE            7 0
     C                   MOVE      *BLANKS       ##SS#             9
     C                   Z-ADD     0             ##ACDT            8 0
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C*
     C* IF SUBSETTED ON CASE INFORMATION, WE MAY BE COMING FROM
     C* F0269R.  IF SO, CREATE A COPY OF F0269R IN QTEMP TO AVOID
     C* RECURSIVE CALL
     C*
     C     @@SUB#        IFNE      *ZEROS
     C     @@FND         ANDNE     *ZEROS
     C     @@FYR         ANDNE     *ZEROS
     C     @@CASE        ANDNE     *ZEROS
     C                   MOVEL(P)  'F0269R'      #FNAME           10
     C                   MOVEL(P)  'F0269Q'      #TNAME           10
     C*
     C                   CALL      'FA711C'
     C                   PARM      ' '           ##RTN             1
     C                   PARM                    #FNAME           10
     C                   PARM                    #TNAME           10
     C*
     C     ##RTN         IFEQ      'A'
     C                   SETON                                        9071
     C                   ELSE
     C*
     C                   CALL      'F0269Q'
     C                   PARM      '2'           ##CALL
     C                   PARM      *ZEROS        ##SUB#
     C                   PARM      *ZEROS        #1FND
     C                   PARM      *ZEROS        ##FYR
     C                   PARM      *ZEROS        ##CASE
     C                   PARM      *BLANKS       ##SS#
     C                   PARM      *ZEROS        ##ACDT
     C                   PARM      *BLANKS       ##RTCD
     C                   ENDIF
     C                   ELSE
     C                   CALL      'F0269R'
     C                   PARM                    ##CALL
     C                   PARM                    ##SUB#
     C                   PARM                    #1FND
     C                   PARM                    ##FYR
     C                   PARM                    ##CASE
     C                   PARM                    ##SS#
     C                   PARM                    ##ACDT
     C                   PARM                    ##RTCD
     C                   ENDIF
     C*
     C     ##RTCD        IFEQ      'A'
     C     ##RTCD        OREQ      'B'
     C     ##SS#         OREQ      *BLANK
     C                   SETON                                        9038
     C                   ENDIF
     C*
     C     ##RTCD        IFEQ      '*'
     C     ##SUB#        ANDGT     0
     C     #1FND         ANDGT     0
     C                   MOVE      ##CO#         #2CO#             3
     C                   MOVE      ##SUB#        #2SUB#            5
     C                   MOVE      #1FND         #2FND             3
     C*
     C** CHANGE USER PARMS
     C*
     C                   CALL      'F0006R2'
     C                   PARM                    #2CO#
     C                   PARM                    #2SUB#
     C                   PARM                    #2FND
     C                   PARM      ' '           ##RTCD
     C*
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVE      ##SS#         SCSS#
      /Free
         scss# = MaskData('SSN':'##SS#':##SS#);
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C*
     C                   Z-ADD     ##ACDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCACDT
     C*
     C                   Z-ADD     ##SUB#        SCSUB#
     C                   Z-ADD     #1FND         SCFND
     C                   Z-ADD     ##FYR         SCFYR
     C                   Z-ADD     ##CASE        SCCASE
     C*
     C*** GET CLAIMAINT NAME
     C*
     C                   Z-ADD     ##SUB#        KKSUB#
     C                   Z-ADD     #1FND         KKFND
     C                   Z-ADD     ##FYR         KKFYR
     C                   Z-ADD     ##CASE        KKCASE
     C     CMKY          CHAIN     PCM01                              99
     C                   MOVEL(P)  CMPOST        SCPOST
     C*** Beg Chg *** 06/06/12 ****************************************
     C*
     C     CLTID         IFEQ      '114'
     C*
     C     CMFND         IFEQ      432
     C     CMFND         OREQ      461
     C                   MOVEL(P)  CMAST         SCAST
     C                   ENDIF
     C*
     C                   ENDIF
     C*** End Chg *** 06/06/12 ****************************************
     C*
     C                   MOVEL     SCPOST        KKAST
     C     STKY          CHAIN     PST02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     STNAME        SCSTNM
     C                   ENDIF
     C                   MOVEL     SCPOST        KKAST
     C     STKY          CHAIN     PST02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     STNAME        SCPSNM
     C                   ENDIF
     C                   MOVEL(P)  CMLSN         SCLSN
     C                   MOVEL(P)  CMFSN         SCFSN
     C                   MOVEL(P)  CMLSN         ##LAST           56
     C                   MOVEL(P)  CMFSN         ##FRST           15
     C                   MOVE      '1'           ##TERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    ##LAST
     C                   PARM                    ##FRST
     C                   PARM                    ##TERM
     C                   MOVEL     ##LAST        SCNAME
     C                   SETON                                        9031
     C*
     C                   ENDIF
     C*
     C*** PROVIDER SEARCH
     C*
     C     HFLD          WHENEQ    'SCFEIN'
     C     HFLD          OREQ      'SCMOD'
     C                   MOVE      SCFEIN        HLFEIN
     C                   MOVE      SCMOD         HLMOD
     C                   MOVE      '2'           ##CALL
     C                   MOVE      *BLANKS       ##FEIN
     C                   MOVE      *BLANKS       ##MOD
     C                   MOVE      *BLANKS       ##RTCD
     C                   MOVE      *BLANKS       ##TYPE
     C                   CALL      'F0032R'
     C                   PARM                    ##CALL            1
     C                   PARM                    ##FEIN            9
     C                   PARM                    ##MOD             3
     C                   PARM                    ##RTCD            1
     C                   PARM                    ##TYPE            1
     C                   PARM      'Y'           ##1N4             1
     C*
     C     ##RTCD        IFEQ      'A'
     C     ##RTCD        OREQ      'B'
     C     ##FEIN        OREQ      *BLANKS
     C                   MOVE      HLFEIN        SCFEIN
     C                   MOVE      HLMOD         SCMOD
     C                   SETON                                        9031
     C                   ENDIF
     C*
     C     ##RTCD        IFEQ      '*'
     C                   MOVE      ##FEIN        SCFEIN
     C                   MOVE      ##MOD         SCMOD
     C*
     C*** GET PROVIDER NAME
     C*
     C                   MOVE      SCFEIN        KKFEIN
     C                   MOVE      SCMOD         KKMOD
     C     PDKY          CHAIN     PPD01                              99
     C  N99              MOVEL(P)  PDNAME        SCPNAM
     C     PDKY          SETLL     PP101
     C     PDKY          READE     PP101                                  99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      P1N#02        SCBNPI
     C     SCBNPI        IFEQ      *BLANKS
     C                   SETON                                        58
     C                   ELSE
     C                   SETON                                        32
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C**N99                MOVELPDNAME    SCRPNM    P
     C*
     C** ADJUDICATION STATE SEARCH
     C*
     C     HFLD          WHENEQ    'SCAST'
     C*** Beg Chg *** 02/11/16 ********************************************
     C**********         CALL      'F0601R'      STST
     C                   CALL      'F0601R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C                   PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      *ZEROS        ##RRN1            9 0
     C                   PARM      'A'           ##adstate         1
     C*** End Chg *** 02/11/16 ********************************************
     C*
     C     ##KL          IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        9032
     C                   ENDIF
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PST01                              99
     C                   MOVEL(P)  STKEY         SCAST
     C                   MOVEL(P)  STNAME        SCSTNM
     C                   SETON                                        9063
     C                   ENDIF
     C*
     C*
     C** POLICY STATE SEARCH
     C*
     C     HFLD          WHENEQ    'SCPOST'
     C                   CALL      'F0601R'      STST
     C*
     C     ##KL          IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        9063
     C                   ENDIF
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PST01                              99
     C                   MOVEL(P)  STKEY         SCPOST
     C                   MOVEL(P)  STNAME        SCPSNM
     C                   SETON                                        9034
     C                   ENDIF
     C*
     C*
     C**N99                MOVELPDNAME    SCFPNM    P
     C*
     C** SYSTEM SEARCH
     C*
     C*
     C** BILL TYPE SEARCH
     C*
     C     HFLD          WHENEQ    'SCBTYP'
     C*
     C                   MOVEL(P)  'BTYP'        ##TBID
     C                   CALL      'F2380R'      GTPLT
     C*
     C     ##KL          IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        9034
     C                   ENDIF
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C                   MOVEL(P)  GTKEY         SCBTYP
     C                   MOVEL(P)  GTDESC        SCBTDS
     C                   SETON                                        9035
     C                   ENDIF
     C*
     C** BILL SUP-TYPE SEARCH
     C*
     C     HFLD          WHENEQ    'SCSTYP'
     C*
     C                   MOVEL(P)  'STYP'        ##TBID
     C                   CALL      'F2380R'      GTPLT
     C*
     C     ##KL          IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        9035
     C                   ENDIF
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C                   MOVEL(P)  GTKEY         SCSTYP
     C                   MOVEL(P)  GTDESC        SCSTDS
     C                   SETON                                        9036
     C                   ENDIF
     C*
     C** ADJUDICATION CODE SEARCH
     C*
     C     HFLD          WHENEQ    'SCCSCD'
     C*
     C                   CALL      'F2300R'      CSPLT
     C*
     C     ##KL          IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        9036
     C                   ENDIF
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PCS01                              99
     C                   MOVEL(P)  CSKEY         SCCSCD
     C                   MOVEL(P)  CSDESC        SCCSDS
     C                   SETON                                        9037
     C                   ENDIF
     C*
     C                   OTHER
     C*
     C                   SELECT
     C*
     C** BILLING PROVIDER NPI #
     C*    NOT A SEARCH, JUST USED FOR CURSOR POSITION
     C*
     C     HFLD          WHENEQ    'SCBNPI'
     C                   SETON                                        29
     C*
     C     HFLD          WHENEQ    'SCCRDT'
     C                   SETON                                        51
     C     HFLD          WHENEQ    'SCCSDT'
     C                   SETON                                        52
     C     HFLD          WHENEQ    'SCCHRG'
     C                   SETON                                        53
     C                   ENDSL
     C*
     C                   SETON                                        90
     C*
     C                   ENDSL
     C                   ELSE
     C*
     C                   SELECT
     C*
     C*** PROVIDER SEARCH
     C*
     C     HFLD          WHENEQ    'SCFEIN'
     C     HFLD          OREQ      'SCMOD'
     C                   MOVE      SCFEIN        HLFEIN
     C                   MOVE      SCMOD         HLMOD
     C                   MOVE      '2'           ##CALL
     C                   MOVE      *BLANKS       ##FEIN
     C                   MOVE      *BLANKS       ##MOD
     C                   MOVE      *BLANKS       ##RTCD
     C                   MOVE      *BLANKS       ##TYPE
     C                   CALL      'F0032R'
     C                   PARM                    ##CALL            1
     C                   PARM                    ##FEIN            9
     C                   PARM                    ##MOD             3
     C                   PARM                    ##RTCD            1
     C                   PARM                    ##TYPE            1
     C                   PARM      'Y'           ##1N4             1
     C*
     C     ##RTCD        IFEQ      'A'
     C     ##RTCD        OREQ      'B'
     C     ##FEIN        OREQ      *BLANKS
     C                   MOVE      HLFEIN        SCFEIN
     C                   MOVE      HLMOD         SCMOD
     C                   SETON                                        9031
     C                   ENDIF
     C*
     C     ##RTCD        IFEQ      '*'
     C                   MOVE      ##FEIN        SCFEIN
     C                   MOVE      ##MOD         SCMOD
     C*
     C*** GET PROVIDER NAME
     C*
     C                   MOVE      SCFEIN        KKFEIN
     C                   MOVE      SCMOD         KKMOD
     C     PDKY          CHAIN     PPD01                              99
     C  N99              MOVEL(P)  PDNAME        SCPNAM
     C                   SETON                                        9019
     C*** Beg Add *** 10/01/13 *********************************************
     C     PDKY          SETLL     PP101
     C     PDKY          READE     PP101                                  99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      P1N#02        SCBNPI
     C                   ELSE
     C                   CLEAR                   SCBNPI
     C                   ENDIF
     C*** End Add *** 10/01/13 *********************************************
     C                   ENDIF
     C                   SETON                                        90
     C*
     C                   ENDSL
     C                   ENDIF
     C*
     C                   EXSR      DATSBR
     C**
     C**IF IN CHANGE MODE, ALLOW SEACH ON FACILITY PROVIDER
     C**N99                MOVELPDNAME    SCFPNM    P
     C*
     C                   ENDIF
     C*** Beg Add *** 06/21/12 **************************************
      /Free

b03      If ##WCIF = 'Y' and ##WCIN ='Y'
            and ##NPWC <> 'Y';
         Chain cmky4 pcm04;
         // *** Beg Chg *** 09/28/12 ********************************
         //  If %found(wmcml) and c4uslh = 'Y'
         //    or cmast = 'P1' or cmast = 'P2';
           If %found(wmcml) and c4uslh = 'Y';
         // *** End Chg *** 09/28/12 ********************************
         // *** Beg Add *** 09/28/12 ********************************
             Select;
             When cmfnd = 432;
                scast = 'P1';

             When cmfnd = 461;
                scast = 'P2';

             Other;
         // *** End Add *** 09/28/12 ********************************
             scast = 'HL';
         // *** Beg Add *** 09/28/12 ********************************
             Endsl;
         // *** End Add *** 09/28/12 ********************************
             kkast = scast;
             Chain stKy pst02;
               If %found(STSTL1);
                scstnm = stname;
               Endif;
           Endif;
         Endif;

      /End-Free
     C*** End Add *** 06/21/12 **************************************
     C*
     C**
     C*
     C                   ENDSR
     C*================================================================
     C* KKDO - F11=EOB/Notes
     C*================================================================
     C     KKDO          BEGSR
     C                   Z-ADD     0             ##RRN
     C                   Z-ADD     @@RRN         ##RRN2
     C*
     C     @@MODE        IFEQ      'D'
     C                   MOVE      '1'           ##CALL
     C                   ELSE
     C                   MOVE      ' '           ##CALL
     C                   ENDIF
     C*
     C                   CALL      'F0355R'
     C                   PARM                    ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM                    ##RRN2
     C                   SETON                                        90
     C                   ENDSR
     C*================================================================
     C* KNDO - F14=DARTS
     C*================================================================
     C     KNDO          BEGSR
     C*
     C* CALL API TO ACCESS DARTS
     C*
     C                   MOVE      ##CO#         #1CO#
     C                   MOVE      SCSUB#        #1SUB#
     C                   MOVE      SCFND         #2FND
     C                   MOVE      SCFYR         #1FYR
     C                   MOVE      SCCASE        #1CASE                   90
     C*
     C                   CALL      'FA703R'
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM                    ##WCIN            1
     C                   PARM                    ##WCDK            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #2FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM      *BLANK        ##KEY            40
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* KQDO - F16=History
     C*================================================================
     C     KQDO          BEGSR
     C*
     C*
     C*          Call subroutine to check for multiple cases
     C*          ------------------------------------------------------
     C*
     C*
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVE      SCSS#         KKSS#
      /Free
         kkss# = UnMaskData('SSN':'EOSS#':scss#);
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C*
     C                   Z-ADD     SCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCACDT
     C*
     C                   Z-ADD     SCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKACDT
     C*
     C     CMKY1         CHAIN     PCM02                              99
     C*
     C*          Call subroutine to check for multiple cases
     C*
     C                   EXSR      TESTMC
     C*
     C     *IN49         IFEQ      *OFF
     C*
     C                   Z-ADD     SCSUB#        KKSUB#
     C                   Z-ADD     SCFND         KKFND
     C                   Z-ADD     SCFYR         KKFYR
     C                   Z-ADD     SCCASE        KKCASE
     C*
     C     CMKY          CHAIN     PCM01                              99
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXACDT            8 0
     C*
     C*  Call new history pgm F0391R
     C*
     C                   CALL      'F0391R'
     C                   PARM      ' '           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##CO#
     C                   PARM                    CMSUB#
     C                   PARM                    CMFND
     C                   PARM                    CMFYR
     C                   PARM                    CMCASE
     C                   PARM                    KKEOB#
     C                   ENDIF
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* MMODS - Multiple Modifier Search
     C*================================================================
     C     MMODS         BEGSR
     C                   MOVE      *BLANKS       ##PRDS           40
     C                   MOVE      *BLANKS       ##CALL            1
     C                   Z-ADD     SCFEIN        ZZFEIN
     C                   Z-ADD     SCMOD         ZZMOD
     C                   MOVE      *BLANKS       ##TYPE            1
     C                   MOVE      *BLANKS       ##NWCD            5
     C                   MOVE      *BLANKS       ##EFDT            8
     C                   CALL      'F0534R'
     C                   PARM                    ##PRDS
     C                   PARM                    ##CALL
     C                   PARM                    ZZFEIN
     C                   PARM                    ZZMOD
     C                   PARM                    ##TYPE
     C                   PARM                    ##NWCD
     C                   PARM                    ##EFDT
     C                   PARM      ' '           ##RTCD            1
     C*
     C     ZZFEIN        IFNE      SCFEIN
     C     ZZMOD         ORNE      SCMOD
     C                   MOVE(P)   ZZFEIN        SCFEIN
     C                   MOVE(P)   ZZMOD         SCMOD
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C                   SETON                                        91
     C     @@MODE        IFEQ      'C'
     C     EOBLST        ANDEQ     'P'
     C     EORCN         ANDEQ     0
     C                   SETOFF                                       91
     C                   ENDIF
     C                   MOVEL     *BLANKS       ##NWCD
     C                   MOVEL     *BLANKS       XXEDI
     C*
     C     ##KCIF        IFNE      'Y'
     C*
     C                   MOVE      *BLANKS       SCSS#
     C                   Z-ADD     0             SCACDT
     C                   MOVE      *BLANKS       SCNAME
     C                   Z-ADD     0             SCSUB#
     C                   Z-ADD     0             SCFND
     C                   Z-ADD     0             SCFYR
     C                   Z-ADD     0             SCCASE
     C*
     C                   ENDIF
     C*
     C* POSITION CURSOR TO PROVIDER FIELD IF IN ADD MODE AND FLAG IS
     C* SET TO KEEP CLAIMAINT INFO
     C*
     C     ##KCIF        IFEQ      'Y'
     C     @@MODE        ANDEQ     'A'
     C     SCSUB#        ANDNE     *ZEROS
     C     SCFND         ANDNE     *ZEROS
     C     SCFYR         ANDNE     *ZEROS
     C     SCCASE        ANDNE     *ZEROS
     C                   SETON                                        31
     C                   SETOFF                                       38
     C                   ELSE
     C     *IN91         IFEQ      *OFF
     C                   SETON                                        31
     C                   SETOFF                                       38
     C                   ELSE
     C                   SETOFF                                       31
     C                   SETON                                        38
     C                   ENDIF
     C                   ENDIF
     C*
     C                   Z-ADD     0             SCFEIN
     C                   Z-ADD     0             SCMOD
     C                   Z-ADD     0             HDFEIN
     C                   Z-ADD     0             HDMOD
     C                   MOVE      *BLANKS       SCPNAM
     C                   MOVE      *BLANKS       SCAST
     C                   MOVE      *BLANKS       SCSTNM
     C                   MOVE      *BLANKS       SCBTYP
     C                   MOVE      *BLANKS       SCBTDS
     C                   MOVE      *BLANKS       SCSTYP
     C                   MOVE      *BLANKS       SCSTDS
     C                   MOVE      *BLANKS       SCCSCD
     C                   MOVE      *BLANKS       SCBNPI
     C                   MOVE      *BLANKS       SCCSDS
     C                   Z-ADD     0             SCCRDT
     C                   Z-ADD     0             SCCSDT
     C                   Z-ADD     0             SCCHRG
     C                   MOVE      *BLANKS       SCBLST
     C                   MOVE      *BLANKS       SCPOST
     C                   MOVE      *BLANKS       SCPSNM
     C*
     C* DEFAULT CASE INFORMATION IF PASSED AND IN ADD MODE
     C*
     C     @@MODE        IFEQ      'A'
     C     @@SUB#        ANDNE     *ZEROS
     C     @@FND         ANDNE     *ZEROS
     C     @@FYR         ANDNE     *ZEROS
     C     @@CASE        ANDNE     *ZEROS
     C                   Z-ADD     @@SUB#        SCSUB#
     C                   Z-ADD     @@FND         SCFND
     C                   Z-ADD     @@FYR         SCFYR
     C                   Z-ADD     @@CASE        SCCASE
     C*** Beg Add *** 04/16/13 *********************************************
     C*** Beg Del *** 12/04/17 *********************************************
     C**
     C**/Free
     C** If XXCLT# = '114' and ##WCIF = 'Y';
     C**
     C** //*** Beg Chg *** 11/04/15 **************************************
     C** //*** outgasc = *blanks;
     C** //*** inCo# = ##co#;
     C** //*** inFnd = @@fnd;
     C** //*** Callp RtvGrpAsc(inCo#:inFnd:outGAsc);
     C** //***If outGAsc= 'OPTACOMP' and q1prms > 9;
     C**      If q1prms > 9;
     C** //*** End Chg *** 11/04/15 **************************************
     C**         kksub# = scsub#;
     C**         kkfnd  = scfnd;
     C**         kkfyr  = scfyr;
     C**         kkcase = sccase;
     C**         Chain cmky fmcml1;
     C**         If %found(fmcml1);
     C**           inDbrrn = dbrrn;
     C**           inDcid  = @@dcid;
     C**           outRtnc = *blanks;
     C**           callp PopEobHdrDtl(inDbrrn:inFnd:inDcid:outRrn:outRtnc);
     C**           x1upld = 'Y';
     C** //*** Beg Add *** 04/14/15 ******************************     ***
     C**           If outRrn <> 0;
     C** //*** End Add *** 04/14/15 ******************************     ***
     C** //*** Beg Add *** 02/29/16 ******************************     ***
     C**           eval  *in56  = *on;
     C** //*** End Add *** 02/29/16 ******************************     ***
     C**           Chain outRrn Faeop;
     C**           eval  @@rrn  = dbrrn4;
     C**           eval  @@mode = 'C';
     C**           eval  *in42  = *on;
     C**           eval  *in91 = *off;
     C** //*** Beg Add *** 02/15/16 **************************************
     C**           // update descriptions in this eob
     C**           if EObtyp = 'N';
     C**              // read through detail file fddhp
     C**              setll (EOco#:EOsub#:EOfnd:EOfyr:EOcase:EOeob#) pdh02;
     C**              reade (EOco#:EOsub#:EOfnd:EOfyr:EOcase:EOeob#) pdh02;
     C**              dow not %eof(fddhl1);
     C**                  // get service date
     C**                  Chain(n) (EOco#:EOsub#:EOfnd:EOfyr:EOcase:
     C**                            EOfein:EOmod:EOeob#) pca01;
     C**                  if %found(fdcall);
     C**                     kkfdat = CAfdat;
     C**                     // get description
     C**                     outdescpr = *blanks;
     C**                     outdescbp = *blanks;
     C**                     callp getproccddes (EObtyp:DH2prcd:EOast:
     C**                                         kkfdat:outdescpr);
     C**                     callp getproccddes (EObtyp:DH2bprc:EOast:
     C**                                         kkfdat:outdescbp);
     C**                  endif;
     C**                  if outdescpr <> *blanks;
     C**                     DH2prds = outdescpr;
     C**                  endif;
     C**                  if outdescbp <> *blanks;
     C**                     DH2bprd = outdescbp;
     C**                  endif;
     C**                  update(e) pdh02 %fields (DH2prds:DH2bprd);
     C**                  reade (EOco#:EOsub#:EOfnd:EOfyr:EOcase:EOeob#) pdh02;
     C**              enddo;
     C**           endif;
     C** //*** End Add *** 02/15/16 **************************************
     C** //*** Beg Add *** 04/14/15 ******************************     ***
     C**           EndIf;
     C** //*** End Add *** 04/14/15 ******************************     ***
     C**         Endif;
     C**      Endif;
     C** Endif;
     C**/End-Free
     C*** End Del *** 12/04/17 *********************************************
     C*** End Add *** 04/16/13 *********************************************
     C*** Beg Add *** 12/04/17 *********************************************
      /Free
         If XXCLT# = '114' and ##WCIF = 'Y' and
           (isModEnabled('837 ASC X12 Prof')
            or isModEnabled('837 ASC X12 Inst'));
              If q1prms > 9;
                 kksub# = scsub#;
                 kkfnd  = scfnd;
                 kkfyr  = scfyr;
                 kkcase = sccase;
                 Chain cmky fmcml1;
                 If %found(fmcml1);
                   inMode  = 'I';
                   inBchCntl = ' ';
                   inRrn = DBrrn;
                   inDcId2 = @@dcId;
                   outRrn = *zeros;
                   outRtCd = *blanks;
                   callp Lod837Data(inMode:inBchCntl:inRrn:inDcId2:outRrn:
                          outRtCd);
                   x1upld = 'Y';
                   If outRrn <> 0;
                      eval  *in56  = *on;
                      Chain outRrn Faeop;
                      eval  @@rrn  = dbrrn4;
                      eval  @@mode = 'C';
                      eval  *in42  = *on;
                      eval  *in91 = *off;
                   EndIf;
                 Endif;
              Endif;
         Endif;
      /End-Free
     C*** End Add *** 12/04/17 *********************************************
     C*
     C* GET SS#, DOI AND NAME
     C*
     C                   Z-ADD     SCSUB#        KKSUB#
     C                   Z-ADD     SCFND         KKFND
     C                   Z-ADD     SCFYR         KKFYR
     C                   Z-ADD     SCCASE        KKCASE
     C*
     C     CMKY          CHAIN     PCM01                              99
     C     *IN99         IFEQ      *OFF
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVEL(P)  CMSS#         SCSS#
      /Free
         scss# = MaskData('SSN':'CMSS#':CMSS#);
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C                   Z-ADD     CMACDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C*
     C                   MOVEL(P)  CMLSN         SCLSN
     C                   MOVEL(P)  CMFSN         SCFSN
     C                   MOVEL(P)  CMLSN         ##LAST           56
     C                   MOVEL(P)  CMFSN         ##FRST           15
     C                   MOVE      '1'           ##TERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    ##LAST
     C                   PARM                    ##FRST
     C                   PARM                    ##TERM
     C                   MOVEL     ##LAST        SCNAME
     C*
     C     EOPOST        IFEQ      *BLANKS
     C                   MOVEL     CMPOST        SCPOST
     C                   MOVEL     SCPOST        KKAST
     C     STKY          CHAIN     PST02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     STNAME        SCPSNM
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C     @@MODE        IFNE      'A'
     C*
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
     C                   SETON                                        69
     C                   ELSE
     C                   SETOFF                                       69
     C                   ENDIF
     C*                                                                '
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVE(P)   EOSS#         SCSS#
      /Free
         scss# = MaskData('SSN':'EOSS#':EOSS#);
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C*
     C                   Z-ADD     EOACDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C*
     C                   MOVEL(P)  EOLSN         SCLSN
     C                   MOVEL     EOFSN         SCFSN
     C                   MOVEL(P)  EOLSN         ##LAST           56
     C                   MOVEL(P)  EOFSN         ##FRST           15
     C                   MOVE      '1'           ##TERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    ##LAST
     C                   PARM                    ##FRST
     C                   PARM                    ##TERM
     C                   MOVEL     ##LAST        SCNAME
     C*
     C                   Z-ADD     EOSUB#        SCSUB#
     C                   Z-ADD     EOFND         SCFND
     C                   Z-ADD     EOFYR         SCFYR
     C                   Z-ADD     EOCASE        SCCASE
     C*
     C**N99                MOVELPDNAME    SCPNAM    P
     C*
     C                   Z-ADD     EOFEIN        SCFEIN
     C                   Z-ADD     EOMOD         SCMOD
     C                   Z-ADD     EOFEIN        HDFEIN
     C                   Z-ADD     EOMOD         HDMOD
     C*
     C     SCFEIN        IFNE      *ZEROS
     C                   MOVE      SCFEIN        KKFEIN
     C                   MOVE      SCMOD         KKMOD
     C     PDKY          CHAIN     PPD01                              99
     C  N99              MOVEL(P)  PDNAME        SCPNAM
     C                   ENDIF
     C**N99                MOVELPDNAME    SCRPNM    P
     C*
     C                   MOVE      EOAST         SCAST
     C                   MOVEL     SCAST         KKAST
     C     STKY          CHAIN     PST02                              99
     C  N99              MOVEL     STNAME        SCSTNM
     C*
     C**N99                MOVELPDNAME    SCFPNM    P
     C*
     C**N99*****           MOVELGTDESC    SCSYDS    P
     C*
     C                   MOVE      EOBTYP        SCBTYP
     C                   MOVEL(P)  'BTYP'        KKTBID
     C                   MOVEL(P)  SCBTYP        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C  N99              MOVEL(P)  GTDESC        SCBTDS
     C*
     C                   MOVE      EOSTYP        SCSTYP
     C                   MOVEL(P)  'STYP'        KKTBID
     C                   MOVEL(P)  SCSTYP        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C  N99              MOVEL(P)  GTDESC        SCSTDS
     C*
     C                   MOVE(P)   EOCSCD        SCCSCD
     C                   MOVEL(P)  SCCSCD        KKKEY
     C     TBKY          CHAIN     PCS02                              99
     C  N99              MOVEL(P)  CSDESC        SCCSDS
     C                   MOVEL(P)  EOBNPI        SCBNPI
     C*
     C                   Z-ADD     EOCRDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCRDT
     C*
     C                   Z-ADD     EOCSDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCSDT
     C                   Z-ADD     EOCHRG        SCCHRG
     C                   Z-ADD     EOCHRG        HLCHRG
     C                   MOVE      EOBLST        SCBLST
     C*
     C                   MOVEL     EOPOST        SCPOST
     C                   MOVEL     SCPOST        KKAST
     C     STKY          CHAIN     PST02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     STNAME        SCPSNM
     C                   ENDIF
     C*
     C* OPEN UP FIELDS WHEN THIS BILL WAS IMPORTED FROM A VENDOR PROCES
     C*
     C                   SELECT
     C*
     C     ##CLT#        WHENEQ    '010'
     C*
     C                   MOVE      'P2P'         ##STCD
     C                   Z-ADD     @@RRN         ##RRN
     C*
     C                   CALL      'FA743R'
     C                   PARM                    ##MODE
     C                   PARM                    ##RRN
     C                   PARM                    ##STCD
     C                   PARM      *BLANKS       ##UPLD            1
     C*
     C                   MOVE      ##UPLD        XXUPLD            1
     C     XXUPLD        IFEQ      'Y'
     C                   SETON                                        55
     C                   ELSE
     C                   SETOFF                                       55
     C                   ENDIF
     C*
     C     ##CLT#        WHENEQ    '129'
     C                   MOVE      'SCD'         ##STCD
     C                   Z-ADD     @@RRN         ##RRN
     C*
     C                   CALL      'FA743R'
     C                   PARM                    ##MODE
     C                   PARM                    ##RRN
     C                   PARM                    ##STCD
     C                   PARM      *BLANKS       ##UPLD            1
     C*
     C                   MOVE      ##UPLD        XXUPLD            1
     C     XXUPLD        IFEQ      'Y'
     C                   SETON                                        55
     C                   ELSE
     C                   MOVE      'OUT'         ##STCD
     C                   Z-ADD     @@RRN         ##RRN
     C*
     C                   CALL      'FA743R'
     C                   PARM                    ##MODE
     C                   PARM                    ##RRN
     C                   PARM                    ##STCD
     C                   PARM      *BLANKS       ##UPLD            1
     C*
     C                   MOVE      ##UPLD        XXUPLD            1
     C     XXUPLD        IFEQ      'Y'
     C                   SETON                                        55
     C                   ELSE
     C                   SETOFF                                       55
     C                   ENDIF
     C                   ENDIF
       //*** Beg Add *** 11/19/17 ****************************************
      /Free
                         When ##Clt# = '114';
                         ##UpLd = *blanks;
                         ##StCd = '837';
                         ##Rrn  = @@Rrn;
                         callp GetUplFlag(##Mode:##Rrn:##StCd:##UpLd);
                         If ##UpLd = 'Y';
                            *In55 = *On;
                         Else;
                            *in55 = *Off;
                         Endif;
                         xxUpLd = ##UpLd;
      /End-free
       //*** End Add *** 11/19/17 ****************************************
     C*
     C                   OTHER
     C     NWKY          CHAIN     PNW01                              99
     C     *IN99         IFEQ      *OFF
     C     NW10A3        ANDNE     *BLANKS
     C                   MOVEL     NW10A3        ##STCD
     C                   Z-ADD     @@RRN         ##RRN
     C*
     C                   CALL      'FA743R'
     C                   PARM                    ##MODE
     C                   PARM                    ##RRN
     C                   PARM                    ##STCD
     C                   PARM      *BLANKS       ##UPLD            1
     C*
     C                   MOVE      ##UPLD        XXUPLD            1
     C     XXUPLD        IFEQ      'Y'
     C                   SETON                                        55
     C                   ELSE
     C                   SETOFF                                       55
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   ENDIF
     C*** Beg Add *** 04/16/13 ****************************************
     C                   if        x1upld = 'Y'
     C                   eval      xxupld = 'Y'
     C                   endif
     C*** End Add *** 04/16/13 ****************************************
      /Free
       // Beg Add *** 02/20/18 *****************************************
       scBPnam  = *blanks;                         //Clear 837 fields
       scBPadr1 = *blanks;
       scBPadr2 = *blanks;
       scBPadr3 = *blanks;
       scBPhdg  = *blanks;
       scPFhdg  = *blanks;
       VisBPhdg= *off;
       VisPFhdg   = *off;
       DisblBPhdg = *on;
       DisblPFhdg = *on;
       scPFadr1 = *blanks;
       scPFadr2 = *blanks;
       scPFadr3 = *blanks;
       scPFadr4 = *blanks;
       xxZip    = *Zeros;

       If @@Mode <> 'A' and CLTID = '114';

          //Check if 837 Claim
          Chain (EOco#:EOsub#:EOfnd:EOfyr:EOcase:EOeob#) FACLML8;
          If %found(FACLML8);
             Chain (CLMicntl:CLMgctl:CLMtscntl:CLMbhttid) FAHDRL;

             //Populate 837 fields

             If HDbpFnm = *blanks;                 //Billing Provider Name
                scBPnam = %Trim(HDbpNm);
             Else;
                scBPnam = %Trim(HDbpFnm) + ' ' + %Trim(HDbpNm);
             Endif;

             If scBPnam > *blanks and HDbpSuff > *blanks;
                scBPnam = %Trim(scBPnam) + ', ' + %Trim(HDbpSuff);
             Endif;


             scBPadr1 = HDbpAdr1;                   //Billing Provider Address
             scBPadr2 = HDbpAdr2;
             scBPadr3 = *blanks;

             Monitor;
             xxZip = %Dec(HDbpPost:9:0);
             On-Error;
             xxZip = 0;
             Endmon;

             scBPhdg = 'Billing Provider Information';
             VisBPhdg= *on;
             DisblBPhdg = *off;

             If scBPadr2 <> *blanks;
                If %Subst(%EditC(xxZip:'X'):6:4)='0000';
                   If HDbpCty > *blanks and HDbpST > *blanks;
                      scBPadr3=%Trim(HDbpCty) + ', ' + HDbpST + ' ' +
                      %Subst(%EditC(xxZip:'X'):1:5);
                   Endif;
                Else;
                   If HDbpCty > *blanks and HDbpST > *blanks;
                      scBPadr3=%Trim(HDbpCty) + ', ' + HDbpST + ' ' +
                      %Subst(%EditC(xxZip:'X'):1:5) + '-' +
                      %Subst(%EditC(xxZip:'X'):6:4);
                   Endif;
                EndIf;
             Else;
                If %Subst(%EditC(xxZip:'X'):6:4)='0000';
                   If HDbpCty > *blanks and HDbpST > *blanks;
                      scBPadr2=%Trim(HDbpCty) + ', ' + HDbpST + ' ' +
                      %Subst(%EditC(xxZip:'X'):1:5);
                   EndIf;
                Else;
                   If HDbpCty > *blanks and HDbpST > *blanks;
                      scBPadr2=%Trim(HDbpCty) + ', ' + HDbpST + ' ' +
                      %Subst(%EditC(xxZip:'X'):1:5) + '-' +
                      %Subst(%EditC(xxZip:'X'):6:4);
                   EndIf;
                EndIf;
             EndIf;


             Select;
             When EObtyp = 'H';                       // Pay to Provider Info
                Chain (CLMicntl:CLMgctl:CLMtscntl:CLMbhttid) FAHDRL;
                If %found(FAHDRL) and HDPPRadr1 > *blank;
                   scPFhdg = 'Pay to Provider Information';
                   VisPFhdg= *on;
                   DisblPFhdg = *off;
                   scPFadr1 = HDPPRadr1;
                   scPFadr2 = HDPPRadr2;
                   scPFadr3 = *blanks;
                   scPFadr4 = *blanks;

                   If scPFadr2 <> *blanks;
                      If %Subst(HDPPRpost:6:4)='0000' or
                         %Subst(HDPPRpost:6:4)= *blanks;
                         If HDPPRcty > *blanks and HDPPRst > *blanks;
                            scPFadr3=%Trim(HDPPRcty) + ', ' + HDPPRst + ' ' +
                            %Subst(HDPPRpost:1:5);
                         Endif;
                      Else;
                         If HDPPRcty > *blanks and HDPPRst > *blanks;
                            scPFadr3=%Trim(HDPPRcty) + ', ' + HDPPRst + ' ' +
                            %Subst(HDPPRpost:1:5) + '-' +
                            %Subst(HDPPRpost:6:4);
                         Endif;
                      EndIf;
                   Else;
                      If %Subst(HDPPRpost:6:4)='0000' or
                         %Subst(HDPPRpost:6:4)= *blanks;
                         If HDPPRcty > *blanks and HDPPRst > *blanks;
                            scPFadr2=%Trim(HDPPRcty) + ', ' + HDPPRst + ' ' +
                            %Subst(HDPPRpost:1:5);
                         Endif;
                      Else;
                         If HDPPRcty > *blanks and HDPPRst > *blanks;
                            scPFadr2=%Trim(HDPPRcty) + ', ' + HDPPRst + ' ' +
                            %Subst(HDPPRpost:1:5) + '-' +
                            %Subst(HDPPRpost:6:4);
                         Endif;
                      EndIf;
                   EndIf;
                EndIf;

             When EObtyp = 'N' and CLMSFnm > *blanks; // Service Facility Location
                VisPFhdg = *on;
                DisblPFhdg = *off;
                scPFhdg = 'Service Facility Location';
                scPFadr1 = CLMSFnm;
                scPFadr2 = CLMSFadr1;
                scPFadr3 = CLMSFadr2;
                scPFadr4 = *blanks;

                If scPFadr3 <> *blanks;
                   If %Subst(CLMSFpost:6:4)='0000' or
                      %Subst(CLMSFpost:6:4)= *blanks;
                      If CLMSFcty > *blanks and CLMSFst > *blanks;
                         scPFadr4=%Trim(CLMSFcty) + ', ' + CLMSFst + ' ' +
                         %Subst(CLMSFpost:1:5);
                      Endif;
                   Else;
                      If CLMSFcty > *blanks and CLMSFst > *blanks;
                         scPFadr4=%Trim(CLMSFcty) + ', ' + CLMSFst + ' ' +
                         %Subst(CLMSFpost:1:5) + '-' +
                         %Subst(CLMSFpost:6:4);
                      Endif;
                   EndIf;
                Else;
                   If %Subst(CLMSFpost:6:4)='0000' or
                      %Subst(CLMSFpost:6:4)= *blanks;
                      If CLMSFcty > *blanks and CLMSFst > *blanks;
                         scPFadr3=%Trim(CLMSFcty) + ', ' + CLMSFst + ' ' +
                         %Subst(CLMSFpost:1:5);
                      Endif;
                   Else;
                      If CLMSFcty > *blanks and CLMSFst > *blanks;
                         scPFadr3=%Trim(CLMSFcty) + ', ' + CLMSFst + ' ' +
                         %Subst(CLMSFpost:1:5) + '-' +
                         %Subst(CLMSFpost:6:4);
                      Endif;
                   EndIf;
                EndIf;
             Endsl;

          Endif;
       Endif;
       // End Add *** 02/20/18 ****************************************
      /End-Free
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVE(P)   SCSS#         EOSS#
      /Free
       //EOSS# = UnMaskData('SSN':'EOSS#':SCSS#);
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C*
     C                   Z-ADD     SCACDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EOACDT
     C*
     C                   MOVEL     SCLSN         EOLSN
     C                   MOVEL     SCFSN         EOFSN
     C                   Z-ADD     ##CO#         EOCO#
     C                   Z-ADD     SCSUB#        EOSUB#
     C                   Z-ADD     SCFND         EOFND
     C                   Z-ADD     SCFYR         EOFYR
     C                   Z-ADD     SCCASE        EOCASE
     C                   Z-ADD     SCFEIN        EOFEIN
     C                   Z-ADD     SCMOD         EOMOD
     C**N99                MOVELPDNAME    EORLNM    P
     C**N99                MOVELPDFSN     EORFNM    P
     C                   MOVE      SCAST         EOAST
     C                   MOVE      SCPOST        EOPOST
     C*
     C* DEFAULT SYSTEM TYPE WITH GTFG01/FTGTP BASED ON BILL TYPE
     C*
     C                   MOVEL(P)  'BTYP'        KKTBID
     C                   MOVEL(P)  SCBTYP        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C  N99              MOVEL     GTFG01        EOSYS
     C*
     C                   MOVE(P)   SCBTYP        EOBTYP
     C                   MOVE(P)   SCSTYP        EOSTYP
     C     CLTID         IFEQ      '114'
     C     EONDPS        IFEQ      ' '
     C                   MOVE      XXNDPS        EONDPS
     C                   ENDIF
     C     EODPCT        IFEQ      ' '
     C                   MOVE      XXDPCT        EODPCT
     C                   ENDIF
     C                   ENDIF
     C*
     C* DEFAULT ADJUDICATION CODE IF NOT ENTERED
     C*
     C     SCCSCD        IFNE      *BLANKS
     C                   MOVEL(P)  SCCSCD        EOCSCD
     C                   ELSE
     C                   MOVEL(P)  BEADCD        EOCSCD
     C                   ENDIF
     C*
     C                   MOVEL(P)  SCBNPI        EOBNPI
     C                   Z-ADD     SCCRDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EOCRDT
     C*
     C                   Z-ADD     SCCSDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EOCSDT
     C*** BEG ADD *** 01/10/03 ****************************************
     C*
     C* UPDATE THE CODEREVIEW FLAG ON AN ADD FOR USE IN F0353R
     C*
     C     @@MODE        IFEQ      'A'
     C*
     C     EOBTYP        IFEQ      'N'
     C*
     C*** BEG ADD *** 06/22/12 **************************************
     C*
     C*** Beg Del *** 09/17/13 *****************************************
     C**** EOBTYP        OREQ      'H'
     C**** EUBLTY        ANDGE     '130'
     C**** EUBLTY        ANDLE     '139'
     C*** End Del *** 09/17/13 *****************************************
     C*
     C*** END ADD *** 06/22/12 **************************************
     C*
     C*** Beg Del *** 09/17/13 *****************************************
     C**** EOBTYP        OREQ      'H'
     C**** EUBLTY        ANDGE     '830'
     C**** EUBLTY        ANDLE     '839'
     C*** End Del *** 09/17/13 *****************************************
     C*
     C     ##CRVW        IFEQ      'Y'
     C     CXKY          CHAIN     PCX01                              99
     C     *IN99         IFEQ      *OFF
     C     CXCRUS        ANDEQ     'Y'
     C                   MOVEL(P)  'Y'           EOCRNE
     C                   ELSE
     C                   MOVEL(P)  'N'           EOCRNE
     C                   ENDIF
     C                   ELSE
     C                   MOVEL(P)  'N'           EOCRNE
     C                   ENDIF
     C                   ELSE
     C*** Beg Add *** 09/17/13 *****************************************
     C* For hospital bills we will determine this flag in FA713R
     C                   if        eobtyp = 'H'
     C                   eval      eocrne = ' '
     C                   else
     C*** End Add *** 09/17/13 *****************************************
     C                   MOVEL(P)  'N'           EOCRNE
     C*** Beg Add *** 09/17/13 *****************************************
     C                   endif
     C*** End Add *** 09/17/13 *****************************************
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*
     C* GET GEOGRAPHIC ZIP AREA FOR PROVIDER
     C*
     C                   Z-ADD     SCCHRG        EOCHRG
     C*
     C*
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
     C* SETOF INDICATORS FOR ERROR CHECKING:
     C*
     C                   SETOFF                                       394647
     C                   SETOFF                                       48
     C*
     C* SAVE THE CURRENT TOTAL CHARGE AMOUNT. THIS VALUE WILL BE
     C* RESTORED IF THE USER MAKES A CHANGE TO TOTAL CHARGE IN THIS
     C* PROGRAM, THEN DECIDES TO EXIT OUT OF THE HEADER AMOUNT PROGAM
     C* AND PEND THE BILL.
     C*
     C                   MOVE      *ZEROS        ##CHRG            9
     C                   MOVE      EOCHRG        ##CHRG            9
     C*
     C* SETTING OFF INDICATORS TO BE USED IN ERROR CHECKING SUBROUTINE
     C*
     C                   SETOFF                                       404143
     C                   SETOFF                                       44
     C*
     C*
     C     *INKJ         CASEQ     *ON           KJDO
     C     *INKK         CASEQ     *ON           KKDO
     C     *INKN         CASEQ     *ON           KNDO
     C     *INKF         CASEQ     *ON           KFDO
     C     *INKQ         CASEQ     *ON           KQDO
     C                   ENDCS
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   SETON                                        LR
     C     @@MODE        IFEQ      'A'
     C                   MOVE      '1'           OUTFLG
     C                   ENDIF
     C*
     C*** Beg Add *** 06/21/12 **************************************
      /Free

b03      If xxopfl = 'Y';
            Close WMCML;
         Endif;

      /End-Free
     C*** End Add *** 06/21/12 **************************************
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C*
     C     ##WCIF        IFEQ      'Y'
     C     ##WCIN        ANDEQ     'Y'
     C     ##NPWC        ANDNE     'Y'
     C*
     C     @@MODE        IFEQ      'A'
     C                   CALL      'WA044R'
     C                   PARM      *BLANKS       ##RTCD            1
     C*
     C     ##RTCD        IFEQ      'F'
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C* IF IN CHANGE MODE, PROTECT ALL FIELDS EXCEPT TOTAL CHARGE
     C*
     C     @@MODE        IFEQ      'C'
     C                   SETON                                        42
     C                   ELSE
     C                   SETOFF                                       42
     C                   ENDIF
     C*
     C* POSITION CURSOR TO PROVIDER FIELD IF SUBSETTED CASE INFO
     C* IS PASSED.
     C*
     C     @@SUB#        IFNE      *ZEROS
     C     @@FND         ANDNE     *ZEROS
     C     @@FYR         ANDNE     *ZEROS
     C     @@CASE        ANDNE     *ZEROS
     C                   SETON                                        31
     C                   SETOFF                                       38
     C                   ELSE
     C     *IN91         IFEQ      *OFF
     C                   SETON                                        31
     C                   SETOFF                                       38
     C                   ELSE
     C                   SETOFF                                       31
     C                   SETON                                        38
     C                   ENDIF
     C                   ENDIF
     C*
     C**  GET SYSTEM INFO
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
     C                   PARM      *BLANK        ##RHAB            1
     C                   PARM      *BLANK        ##MSS             1
     C                   PARM      *BLANK        ##FSCH            1
     C                   PARM      *BLANK        ##PRVB            1
     C                   PARM      *BLANK        ##NPWC            1
     C                   PARM      *BLANK        ##OS              1
16   C                   PARM                    ##ARLS            1
17   C                   PARM                    ##CCOD            1
18   C                   PARM                    ##CRVW            1
19   C                   PARM                    ##PROM            1
20   C                   PARM                    ##MCRV            1
21   C                   PARM                    ##FREQ            1
22   C                   PARM                    ##CRAC            1
23   C                   PARM                    ##PREO            1
24   C                   PARM                    ##PARN            1
25   C                   PARM                    ##MGRI            1
26   C                   PARM                    ##KCIF            1
27   C                   PARM                    ##CURS            1
28   C                   PARM                    @@PCIB            1
29   C                   PARM                    ##CLT#            3
30   C                   PARM                    ##UCPF            1
31   C                   PARM                    ##DCF             1
32   C                   PARM                    ##AUTB            1
33   C                   PARM                    ##DEIF            1
34   C                   PARM                    ##AURE            1
     C                   CALL      'SA009R'
 1   C                   PARM      *ZEROS        ##EOBD            3 0
 2   C                   PARM      *BLANKS       ##EOBI            1
 3   C                   PARM      *BLANKS       ##ARIN            1
 4   C                   PARM      *BLANKS       ##ACIS            1
 5   C                   PARM      *BLANKS       ##PAY0            1
 6   C                   PARM      *BLANKS       ##SDTS            1
 7   C                   PARM      *BLANKS       ##MCIV            1
 8   C                   PARM      *BLANKS       ##PBIV            1
 9   C                   PARM      *BLANKS       ##AINV            1
10   C                   PARM      *BLANKS       ##WHPG            1
11   C                   PARM      *BLANKS       ##PRIV            1
     C*
     C                   SELECT
     C*
     C     ##PRIV        WHENEQ    '1'
     C                   Z-ADD     1             #1PRIV            1 0
     C*
     C     ##PRIV        WHENEQ    '2'
     C                   Z-ADD     2             #1PRIV
     C*
     C     ##PRIV        WHENEQ    '3'
     C                   Z-ADD     2             #1PRIV
     C*
     C                   OTHER
     C                   Z-ADD     1             #1PRIV
     C*
     C                   ENDSL
     C                   SETOFF                                       17
     C     ##CURS        IFEQ      'Y'
     C                   SETON                                        17
     C                   ENDIF
     C                   MOVE      *BLANK        ##FSFL
     C*
     C**DISPLAY REFERRING PROVIDER FIELDS IF 837 EDI INSTALLED
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
     C*   Determine the revision indicator,If blank load from
     C*   the BEAST record
     C*
     C*   BEAST key build
     C*
     C                   MOVE(P)   SCAST         K1AST
     C*
     C                   MOVEL(P)  'BTYP'        KKTBID
     C                   MOVEL(P)  SCBTYP        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C  N99              MOVEL     GTFG01        KKSYS
     C                   MOVE      SCBTYP        KKBTYP
     C                   MOVE      *BLANKS       KKSTYP
     C*
     C     BEKY          CHAIN     PBE01                              98
     C*
     C     *IN98         IFEQ      *OFF
     C                   MOVE      BEREVI        XXREVI            1
     C                   ENDIF
     C*
     C*** Beg Add *** 06/21/12 **************************************
      /Free
         xxopfl = *blanks;
b03      If *in90 = *off and ##WCIF = 'Y'
            and ##WCIN ='Y' and ##NPWC <> 'Y';
            Open WMCML;
            xxopfl = 'Y';
         Endif;

         //*** Beg Add *** 02/29/16 ******************************     ***
         eval *in56  = *off;
         //*** End Add *** 02/29/16 ******************************     ***
      /End-Free
     C*** End Add *** 06/21/12 **************************************
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C     @@MODE        IFEQ      'G'
     C                   MOVE      'D'           XXBLST
     C                   EXSR      UPDATR
     C*** Beg Add *** 06/21/12 **************************************
      /Free

b03      If xxopfl = 'Y';
            Close WMCML;
         Endif;

      /End-Free
     C*** End Add *** 06/21/12 **************************************
     C                   RETURN
     C                   ENDIF
     C*
     C                   ENDSR
       //*** Beg Add *** 12/04/17 **************************************
       //===============================================================
       // srUpdRcFl - Update R&C flag in detail files
       //===============================================================
      /Free
           Begsr srUpdRcFl;
       // Initially set UCR flag to S=Service code for hosp or C=CPT
              If scBtyp = 'H';
                 xxRcFl = 'S';
              Else;
                 xxRcFl = 'C';
              Endif;
              kkast = scast;
              Chain stky pst02;
                 If %Found (ststl1) and scBtyp <> 'H';
                    Select;
                    When sTfsTy = 'S';
                       xxRcFl = 'C';
                    When sTfsTy = 'U';
                       xxRcFl = 'Y';
                    Other;
                       xxRcFl = 'C';
                    Endsl;
                 Endif;
            // Update FDCAP records
              Chain caky1 pca02;
              if %Found (fdcalr);
                 Dou %Eof (fdcalr);
                    If scBtyp <> 'H';
                       CArcFl = xxRcFl;
                    Endif;
                    Update pca02;
                    Reade caky1 pca02;
                 Enddo;
              Endif;
            // Update FDDHP records
              Chain dhky pdh02;
              If %Found (fddhl1);
                 Dou %Eof (fddhl1);
                    DhrcFl = xxRcFl;
                    Update pdh02;
                    Reade dhky pdh02;
                 Enddo;
              Endif;
           Endsr;
      /End-Free
       //*** End Add *** 12/04/17 **************************************
     C*================================================================
     C* TESTMC - Check for multiple cases
     C*================================================================
     C     TESTMC        BEGSR
     C*
     C* IF THERE IS MORE THAN ONE CASE FOR THIS SS#/ACDT, CALL
     C* PROGRAM TO SELECT APPROPRIATE CASE.
     C*
     C                   SETOFF                                       49
     C                   Z-ADD     0             COUNT             3 0
     C     *IN99         DOWEQ     *OFF
     C* IF CASE INFORMATION MATCHES, THE USER HAS SELECTED FROM THE
     C* CASE SEARCH, THEREFORE THERE IS NO NEED TO CALL MULTIPLE CASE
     C* SEARCH.
     C     CMSUB#        IFEQ      SCSUB#
     C     CMFND         ANDEQ     SCFND
     C     CMFYR         ANDEQ     SCFYR
     C     CMCASE        ANDEQ     SCCASE
     C                   Z-ADD     0             COUNT
     C                   LEAVE
     C                   ENDIF
     C                   ADD       1             COUNT
     C     CMKY1         READE     PCM02                                  99
     C                   ENDDO
     C*
     C     COUNT         IFGT      1
     C*
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVEL(P)  SCSS#         ##SS#
      /Free
         ##SS# = UnMaskData('SSN':'EOSS#':SCSS#);
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C                   MOVE      SCACDT        ##ACDT
     C                   CALL      'F0267R'
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      '2'           ##CALL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM                    ##SS#
     C                   PARM                    ##ACDT
     C                   PARM      'Y'           ##SDOI            1
     C                   SETON                                        90
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C                   SETON                                        49
     C                   ELSE
     C     ##RRN         CHAIN     PCM03                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     CMSUB#        SCSUB#
     C                   Z-ADD     CMFND         SCFND
     C                   Z-ADD     CMFYR         SCFYR
     C                   Z-ADD     CMCASE        SCCASE
     C                   Z-ADD     CMSUB#        KKSUB#
     C                   Z-ADD     CMFND         KKFND
     C                   Z-ADD     CMFYR         KKFYR
     C                   Z-ADD     CMCASE        KKCASE
     C     CMKY          CHAIN     PCM01                              99
     C                   MOVEL(P)  CMLSN         SCLSN
     C                   MOVEL(P)  CMFSN         SCFSN
     C                   MOVEL(P)  CMLSN         ##LAST           56
     C                   MOVEL(P)  CMFSN         ##FRST           15
     C                   MOVE      '1'           ##TERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    ##LAST
     C                   PARM                    ##FRST
     C                   PARM                    ##TERM
     C                   MOVEL     ##LAST        SCNAME
     C                   SETON                                        31
     C                   ENDIF
     C                   SETON                                        49
     C                   ENDIF
     C                   ELSE
     C                   Z-ADD     CMSUB#        SCSUB#
     C                   Z-ADD     CMFND         SCFND
     C                   Z-ADD     CMFYR         SCFYR
     C                   Z-ADD     CMCASE        SCCASE
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*
     C*    ##MODE = H = Call F0395R01 to write bill status history
     C*                 record without displaying the screen.
     C*    ##MODE = A = Call F0395R01 to write bill status history
     C*                 record with the screen displayed.  This allow
     C*                 entry of additional information on why status
     C*                 was changed.
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C* SET CURRENT STATUS
      ***** Beg Chg *** 06/09/20 *******************************************
      /Free
         EOSS# = UnMaskData('SSN':'EOSS#':SCSS#);
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C*
     C     SCBLST        IFEQ      'C'
     C                   MOVE      @@NWST        XXBLST            1
     C                   MOVE      'A'           ##MODE
     C                   ELSE
     C                   MOVE      'H'           ##MODE
     C                   ENDIF
     C*
     C     SCBLST        IFEQ      'P'
     C                   MOVE      'U'           XXBLST
     C                   ENDIF
     C*
     C*
     C* CALL F0395R01 TO WRITE NEW STATUS RECORD
     C* Do not write the status record if this program was call with
     C* a mode of 'G' (delete).
     C*
     C     @@MODE        IFNE      'G'
     C                   Z-ADD     0             ##RRN
     C                   Z-ADD     @@RRN         ##RRN2
     C                   MOVE      XXBLST        ##BLST            1
     C                   MOVEL     Q1PGM         ##PGM
     C                   Z-ADD     0             ##LTR#
     C                   MOVE      *BLANK        XXEDI
     C                   CALL      'F0395R01'
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##MODE
     C                   PARM                    ##RRN
     C                   PARM                    ##RRN2
     C                   PARM                    ##BLST
     C                   PARM      *BLANKS       ##STCD            3
     C                   PARM      *BLANKS       ##REAS           60
     C                   PARM                    ##PGM             8
     C                   PARM                    ##LTR#            9 0
     C                   PARM                    XXEDI             1
     C                   ENDIF
     C*
     C* UPDATE THE BILL STATUS IN FAEOP ONLY IF A BILL STATUS HISTORY
     C* RECORD WAS WRITTEN.
     C*
     C     ##KL          IFEQ      '1'
     C                   SETON                                        90
     C     LOKY          CHAIN     PLO01                              99
     C     *IN99         IFEQ      *OFF
     C                   DELETE    PLO01
     C                   ENDIF
     C                   GOTO      UPEND
     C                   ELSE
     C                   SETOFF                                       90
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
       //*** Beg Add *** 12/16/24 **********************************************
       isClearInv = *off;
       Setll (EOeob#) pu101;
       Reade (EOeob#) pu101;
       If Not %Eof(FAU1L2);
          strMsg = 'Press Enter to delete the Bill Import Records.  +
           Press Cancel to skip deletion of the Bill Import Records.';
          isDelBillImp = PopTwoBtn(strMsg:'Delete Bill Import Records':
           'Enter':'Cancel');
          If isDelBillImp = *on;
             isClearInv = *on;
             Dow Not %Eof(FAU1L2);
                Delete pu101;
                Setll (EOeob#) pu201;
                Reade (EOeob#) pu201;
                Dow Not %Eof(FAU2L2);
                   Delete pu201;
                   Reade (EOeob#) pu201;
                Enddo;
                Reade (EOeob#) pu101;
             Enddo;
             EOsInv = *blanks;
          Else;
             // Release the record lock.
             Update pu101;
          Endif;
       Endif;
       //*** End Add *** 12/16/24 **********************************************
     C                   MOVE      XXBLST        EOBLST
     C     XXBLST        IFEQ      'B'
     C                   MOVE      XXEDI         EOBLST
     C     XXEDI         IFEQ      ' '
     C                   MOVE      'R'           EOBLST
     C                   ENDIF
     C                   ENDIF
     C*
     C* UPATE NUMBER OF RECONS IF THIS IS A RECON
     C* IF THIS BILL HAS NOT BEEN THROUGH WCMC BATCH YET, THEN
     C* USE SAME RECON NUMBER, ELSE INCREMENT RECON NUMBER
     C*
     C     EOBLST        IFEQ      'R'
     C*
     C                   Z-ADD     EORCN         KKRCN
     C     EAKY          CHAIN(N)  PEA01                              99
     C     *IN99         IFEQ      *OFF
     C     EAEPFL        ANDEQ     'P'
     C                   ADD       1             EORCN
     C                   MOVE      'R'           EOSTAT
        //*** Beg Add *** 10/26/17 ************************************
      /Free
                         ExSr ChkClaimCheck;

      /End-Free
        //*** End Add *** 10/26/17 ************************************
     C*
     C* UPDATE THE EDI FLAG IN FAEOP TO C=CORRECTION OF A PREVIOUSLY
     C* SUBMITTED BILL.  UPDATE ALL CORRESPONDING HEADER FILES ALSO
     C*
     C     EOAST         IFEQ      'FL'
     C     EOTAPE        ANDEQ     'S'
     C                   MOVE      'C'           EOTAPE
     C                   MOVE      'Y'           XXUPHD            1
     C                   ELSE
     C                   MOVE      *BLANK        XXUPHD
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C     XXBLST        IFNE      'D'
     C                   EXSR      MVSCDB
     C                   ENDIF
     C     @@MODE        IFEQ      'G'
     C                   CLEAR                   EOCRPE
     C     XXCLT#        IFEQ      '114'
     C*** Begin Add *** 04/10/19 ****************************************
     C     XXCLT#        OREQ      '152'
     C*** End   Add *** 04/10/19 ****************************************
     C                   CLEAR                   EOIDOC
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVEL     Q1USER        EOUSER
     C                   MOVEL     Q1PGM         EOPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        EOTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C*
     C* UPDATE ADJ. CODE/PRECERT/NETWORK FOR UPLOADED BILLS
     C*
     C     XXUPLD        IFEQ      'Y'
     C     *IN91         OREQ      *OFF
     C     *IN09         ANDEQ     *OFF
     C*
     C     SCCSCD        IFNE      *BLANKS
     C                   MOVEL(P)  SCCSCD        EOCSCD
     C                   ELSE
     C                   MOVEL(P)  BEADCD        EOCSCD
     C                   ENDIF
     C                   MOVEL(P)  ##CERT        EOCERT
     C                   Z-ADD     ##EXT         EOEXT
     C                   MOVEL(P)  ##NWCD        EONTWK
     C     EOTAPE        IFEQ      *BLANKS
     C                   MOVEL(P)  BEEDI         EOTAPE
     C                   ENDIF
     C                   MOVEL(P)  'Y'           XXUPHD
     C*
     C                   ENDIF
     C     *IN55         IFEQ      *ON
     C     EOSYS         ANDEQ     'H'
     C     EOBLST        ANDEQ     'P'
     C     *IN55         OREQ      *ON
     C     EOSYS         ANDEQ     'H'
     C     EOBLST        ANDEQ     'U'
     C                   MOVE(P)   SCAST         K1AST
     C                   MOVEL(P)  'BTYP'        KKTBID
     C                   MOVEL(P)  SCBTYP        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C  N99              MOVEL     GTFG01        KKSYS
     C                   MOVEL(P)  SCBTYP        KKBTYP
     C                   MOVE      *BLANKS       KKSTYP
     C*
     C     BEKY          CHAIN     PBE01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      BEEDI         EOTAPE
     C                   ENDIF
     C                   ENDIF
     C*
     C                   UPDATE    DBRCDF
     C*
     C*
     C* UPDATE THE EDI FIELDS IN THE PROVIDER BILLS HEADER FILES
     C*
     C     XXUPHD        IFEQ      'Y'
     C                   CALL      'FA740R'
     C                   PARM      DBRRN4        ##RRN
     C                   PARM                    ##RTCD
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* IF STATUS IN UNPEND, THEN CLEAR 'P' FROM CAFLAG FIELDS IN
     C* DETAIL CHARGE FILE (FDCAP).
     C*
     C     EOBLST        IFEQ      'U'
     C                   Z-ADD     EOSUB#        KKSUB#
     C                   Z-ADD     EOFND         KKFND
     C                   Z-ADD     EOFYR         KKFYR
     C                   Z-ADD     EOCASE        KKCASE
     C     HDFEIN        IFNE      *ZEROS
     C                   Z-ADD     HDFEIN        KKFEIN
     C                   Z-ADD     HDMOD         KKMOD
     C                   ELSE
     C                   Z-ADD     EOFEIN        KKFEIN
     C                   Z-ADD     EOMOD         KKMOD
     C                   ENDIF
     C                   Z-ADD     EOEOB#        KKEOB#
     C     CAKY          CHAIN     PCA01                              99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     EOFEIN        CAFEIN
     C                   Z-ADD     EOMOD         CAMOD
     C                   MOVEL     SCAST         CAAST
     C                   MOVE      ' '           CAFLAG
     C                   MOVEL     Q1USER        CAUSER
     C                   MOVEL     Q1PGM         CAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        CATIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   UPDATE    PCA01
     C     CAKY          READE     PCA01                                  99
     C                   ENDDO
     C*
     C*
     C* COMPLETE LETTER RECORD WHEN UNPENDING BILL
     C*
     C                   Z-ADD     EORCN         KKRCN
     C                   MOVE      *HIVAL        KKLTR#
     C     LTKY          SETLL     PLT01
     C     LTKY1         READE     PLT01                                  99
     C     *IN99         IFEQ      *OFF
     C     LTDIDT        IFNE      *ZEROS
     C     LTSTAT        ORNE      'U'
     C                   Z-ADD     *ZEROS        LTDIDT
     C                   MOVE      'U'           LTSTAT
     C                   MOVEL     Q1USER        LTUSER
     C                   MOVEL     Q1PGM         LTPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        LTTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTDT
     C                   UPDATE    PLT01
     C*
     C* Release the record lock
     C                   ELSE
     C                   UPDATE    PLT01
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  DO NOT PROCESS WORKFLOW SUBROUTINE IF THIS PROGRAM WAS CALLED
     C*  BY OPT4=DELETE
     C*
     C     @@MODE        IFNE      'G'
     C                   EXSR      WRKFLW
     C*
     C*
     C* IF ADJUDICATION STATE DIFFERENT FROM POLICY STATE DETERMINE IF
     C* THE POLICY ST REQUIRES REPORTING FOR SERVICES OUTSIDE OF STATE.
     C*
     C     EOAST         IFNE      EOPOST
     C     XXEDI         ANDNE     'E'
     C                   MOVEL(P)  EOPOST        KKAST
     C     STKY          CHAIN     PST02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C     1             SUBST     STBS06:1      OSEDI             1
     C*
     C* IF OUTSIDE STATE FLAG ON, SET CAP POLICY ST FLAG, CALL WRKFLW AGAIN IF
     C* NOT IN EDIT CORRECTION MODE, THEN SET FLAG COMPLETE
     C*
     C     OSEDI         IFEQ      'Y'
     C     DBRRN4        CHAIN     DBRCDF                             98
     C     *IN98         IFEQ      *OFF
     C                   MOVEL(P)  'Y'           EOCPST
     C                   UPDATE    DBRCDF
     C                   EXSR      WRKFLW
     C                   ENDIF
     C     EOCPST        IFEQ      'Y'
     C     DBRRN4        CHAIN     DBRCDF                             98
     C     *IN98         IFEQ      *OFF
     C                   MOVEL(P)  'C'           EOCPST
     C                   UPDATE    DBRCDF
     C                   ENDIF
     C                   ENDIF
     C*
     C* FLAG WAS NOT ON, SET CAP POLICY FLAG OFF
     C*
     C                   ELSE
     C     DBRRN4        CHAIN     DBRCDF                             98
     C     *IN98         IFEQ      *OFF
     C                   MOVEL     'N'           EOCPST
     C                   UPDATE    DBRCDF
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C*
     C                   ENDIF
     C*
     C* IF DELETING A BILL, CLEAR CACHK# (FOR HISTORY PROGRAM) AND
     C* LOAD CAFLAG WITH 'P' (FOR ADJUDICATION HISTORY)
     C*
     C     @@MODE        IFEQ      'G'
     C     XXBLST        ANDEQ     'D'
     C                   Z-ADD     EOSUB#        KKSUB#
     C                   Z-ADD     EOFND         KKFND
     C                   Z-ADD     EOFYR         KKFYR
     C                   Z-ADD     EOCASE        KKCASE
     C                   Z-ADD     EOFEIN        KKFEIN
     C                   Z-ADD     EOMOD         KKMOD
     C                   Z-ADD     EOEOB#        KKEOB#
     C     CAKY          CHAIN     PCA01                              99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     0             CACHK#
     C                   MOVE      'P'           CAFLAG
     C                   MOVEL     Q1USER        CAUSER
     C                   MOVEL     Q1PGM         CAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        CATIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C*
     C                   UPDATE    PCA01
     C     CAKY          READE     PCA01                                  99
     C*
     C                   ENDDO
     C*
     C*
     C* COMPLETE LETTER RECORD WHEN DELETING BILL IF CURRENT STATUS IS
     C* PENDED
     C*
     C                   Z-ADD     EORCN         KKRCN
     C                   MOVE      *HIVAL        KKLTR#
     C     LTKY          SETLL     PLT01
     C     LTKY1         READE     PLT01                                  99
     C     *IN99         IFEQ      *OFF
     C     LTDIDT        IFNE      *ZEROS
     C     LTSTAT        ORNE      'U'
     C                   Z-ADD     *ZEROS        LTDIDT
     C                   MOVE      'U'           LTSTAT
     C                   MOVEL     Q1USER        LTUSER
     C                   MOVEL     Q1PGM         LTPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        LTTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTDT
     C                   UPDATE    PLT01
     C* Release the record lock
     C                   ELSE
     C                   UPDATE    PLT01
     C                   ENDIF
     C                   ENDIF
     C*
     C*
     C* WRITE 'D' IN FAEAP/EAEPFL
     C*
     C     EAKY1         CHAIN     PEA01                              99
     C     *IN99         DOWEQ     *OFF
     C                   MOVE      'D'           EAEPFL
     C***** Bed Add ***** 11/24/14 ******************************
      /Free
         // If this record is going to be deleted then
         // update BRS Inv proc flag to 'P'.
         EAbblf = 'P';
      /End-Free
     C***** End Add ***** 11/24/14 ******************************
       //*** Beg Add *** 12/16/24 ******************************************
       // For a deleted bill clear the invoice number if deleting Bill
       // Import records
       If isClearInv = *on;
          EAsInv = *blanks;
       Endif;
       //*** End Add *** 12/16/24 ******************************************
     C*
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        EATIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   UPDATE    PEA01
     C     EAKY1         READE     PEA01                                  99
     C                   ENDDO
     C*
     C                   ENDIF
     C*
     C     ##KL          IFEQ      '1'
     C                   SETON                                        90
     C                   ENDIF
     C     XXCLT#        IFEQ      '114'
     C*** Begin Add *** 04/10/19 ****************************************
     C     XXCLT#        OREQ      '152'
     C*** End   Add *** 04/10/19 ****************************************
     C*
     C     Q1PRMS        IFGT      9
     C     @@DCID        IFNE      *BLANKS
     C*** Begin Chg *** 07/16/14 ***********************************************
     C**********         Z-ADD     *HIVAL        KKIDT
     C**********         Z-ADD     *HIVAL        KKITIM
     C*****ESKY1         SETGT     PES01
     C*****ESKY2         READPE    PES01                                  99
     C******IN99         IFEQ      *OFF
     C*****ESBLST        IFEQ      'P'
     C*****ESSTCD        ANDEQ     'CRP'
     C*****ESBLST        OREQ      'C'
     C**********         MOVEL(P)  'REMOVE'      @@RTRN
     C**********         ELSE
     C**********         MOVEL(P)  *BLANKS       @@RTRN
     C**********         ENDIF
     C**********         ELSE
     C*****EOKY*         CHAIN     PEO01                              99
     C*****EOCRNE        IFEQ      'Y'
     C**********         MOVEL(P)  'REMOVE'      @@RTRN
     C**********         ELSE
     C**********         MOVEL     *BLANKS       @@RTRN
     C**********         ENDIF
     C**********         ENDIF
     C     @@RRN         CHAIN(n)  DBRCDF                             99
     C                   Eval      @@rtrn = ' '
     C                   If        EOblSt='C'
     C                   Eval      @@rtrn = 'REMOVE'
     C                   Else
     C                   Eval      kkSub# = EOsub#
     C                   Eval      kkFnd = EOfnd
     C                   Eval      kkFyr = EOfYr
     C                   Eval      kkCase = EOcase
     C                   Eval      kkEob# = EOeob#
     C                   Eval      kkRcn = EOrcn
     C                   Eval      kkIdt = *HIVAL
     C                   Eval      kkItim = *HIVAL
     C     ESKY1         SetGt     pes01
     C     ESKY2         ReadPe    pes01                                  99
     C                   If        *in99 = *Off And ESstCd = 'PRO'
     C                   Eval      @@rtrn = 'REMOVE'
     C                   EndIf
     C                   EndIf
     C*** End   Chg *** 07/16/14 ***********************************************
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     UPEND         ENDSR
     C*================================================================
     C* UPDMOD - Update files when the user changed the FEIN/Mod
     C*================================================================
     C     UPDMOD        BEGSR
     C*
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     ZZMOD         KKMOD
     C*
     C     PDKY          CHAIN     PPD01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     ZZFEIN        EOFEIN
     C                   Z-ADD     ZZMOD         EOMOD
     C*
     C     PDDBA1        IFEQ      'Y'
     C                   MOVEL(P)  PDADR1        EOPNAM
     C                   MOVEL(P)  PDADR2        EOADR1
     C                   MOVEL(P)  *BLANKS       EOADR2
     C                   ELSE
     C     PDFSN         IFEQ      *BLANKS
     C                   MOVEL(P)  PDNAME        EOPNAM
     C                   ELSE
     C     PDFSN         CAT(P)    PDNAME:1      EOPNAM
     C                   ENDIF
     C                   MOVEL(P)  PDADR1        EOADR1
     C                   MOVEL(P)  PDADR2        EOADR2
     C                   ENDIF
     C*
     C                   MOVEL(P)  PDCTY         EOCTY
     C                   MOVEL(P)  PDST          EOST
     C                   Z-ADD     PDZIP         EOZIP
     C                   MOVEL(P)  PDADR3        EOADR3
     C*
     C                   MOVEL     Q1USER        EOUSER
     C                   MOVEL     Q1PGM         EOPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        EOTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     ZZMOD         KKMOD
     C*
     C     PDKY          CHAIN     PPD01                              99
     C     *IN99         IFEQ      *OFF
     C     EAKY          CHAIN     PEA01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     ZZMOD         KKMOD
     C*
     C*
     C                   Z-ADD     ZZFEIN        EAFEIN
     C                   Z-ADD     ZZMOD         EAMOD
     C*
     C     PDDBA1        IFEQ      'Y'
     C                   MOVEL(P)  PDADR1        EAPNAM
     C                   MOVEL(P)  PDADR2        EAADR1
     C                   MOVEL(P)  *BLANKS       EAADR2
     C                   ELSE
     C     PDFSN         IFEQ      *BLANKS
     C                   MOVEL(P)  PDNAME        EAPNAM
     C                   ELSE
     C     PDFSN         CAT(P)    PDNAME:1      EAPNAM
     C                   ENDIF
     C                   MOVEL(P)  PDADR1        EAADR1
     C                   MOVEL(P)  PDADR2        EAADR2
     C                   ENDIF
     C*
     C                   MOVEL(P)  PDCTY         EACTY
     C                   MOVEL(P)  PDST          EAST
     C                   Z-ADD     PDZIP         EAZIP
     C                   MOVEL(P)  PDADR3        EAADR3
     C*
     C*
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        EATIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C*
     C                   UPDATE    PEA01
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     ZZMOD         KKMOD
     C*
     C     PDKY          CHAIN     PPD01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C     CAKY1         CHAIN     PCA02                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C     XXUPLD        IFEQ      'Y'
     C                   MOVEL(P)  SCCSCD        CACSCD
     C                   ENDIF
     C                   Z-ADD     ZZFEIN        CAFEIN
     C                   Z-ADD     ZZMOD         CAMOD
     C                   MOVEL     Q1USER        CAUSER
     C                   MOVEL     Q1PGM         CAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        CATIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C*
     C                   UPDATE    PCA02
     C*
     C     CAKY1         READE     PCA02                                  99
     C*
     C                   ENDDO
     C                   ENDIF
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
      ***** Beg Chg *** 06/09/20 *******************************************
      /Free
         EOSS# = UnMaskData('SSN':'CMSS#':SCSS#);
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C*
     C                   MOVE      ##FSFL        EOFSFL
     C*
     C* IF IN ADD MODE AND THE USER TYPED IN THE S#/ACDT, WE MUST
     C* GET CASE INFORMATION FROM THE CLAIM MASTER FILE
     C*
     C                   EXSR      MVSCDB
     C                   Z-ADD     0             EORCN
     C                   MOVE      ' '           EOSTAT
     C                   MOVEL     Q1USER        EOUSER
     C                   MOVEL     Q1USER        EOIUSR
     C                   MOVEL     Q1PGM         EOPGM
     C                   MOVEL     Q1PGM         EOIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        EOTIME
     C                   Z-ADD     DSTIME        EOITIM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C                   MOVEL     SRDTO         EOIDT
     C                   MOVEL     SRDTO         EOTRDT
     C*
     C*
     C     COKY          CHAIN     PC101                              99
     C     *IN99         IFEQ      *OFF
     C     C1EOB#        IFEQ      *HIVAL
     C                   Z-ADD     1             C1EOB#
     C                   ELSE
     C                   ADD       1             C1EOB#
     C                   END
     C                   UPDATE    PC101
     C                   ELSE
     C                   Z-ADD     *HIVAL        C1EOB#
     C                   END
     C*
     C                   Z-ADD     C1EOB#        EOEOB#
     C*
     C                   MOVEL(P)  ##NWCD        EONTWK
     C*
     C* GET PROVIDER NAME AND ADDRESS
     C*
     C                   Z-ADD     EOFEIN        KKFEIN
     C                   Z-ADD     EOMOD         KKMOD
     C*
     C     PDKY          CHAIN     PPD01                              99
     C     *IN99         IFEQ      *OFF
     c*
     C     PDDBA1        IFEQ      'Y'
     C                   MOVEL(P)  PDADR1        EOPNAM
     C                   MOVEL(P)  PDADR2        EOADR1
     C                   MOVEL(P)  *BLANKS       EOADR2
     C                   ELSE
     C     PDFSN         IFEQ      *BLANKS
     C                   MOVEL(P)  PDNAME        EOPNAM
     C                   ELSE
     C     PDFSN         CAT(P)    PDNAME:1      EOPNAM
     C                   ENDIF
     C                   MOVEL(P)  PDADR1        EOADR1
     C                   MOVEL(P)  PDADR2        EOADR2
     C                   ENDIF
     C*
     C                   MOVEL(P)  PDCTY         EOCTY
     C                   MOVEL(P)  PDST          EOST
     C                   Z-ADD     PDZIP         EOZIP
     C                   MOVEL(P)  PDADR3        EOADR3
     C                   ENDIF
     C*
     C* GET GEOGRAPHIC ZIP AREA FOR PROVIDER
     C*
     C     P1KY          CHAIN     PP101                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  P1ZIP         KKZIPA            3
     C     GZKY          SETGT     PGZ01
     C                   READP     PGZ01                                  99
     C     *IN99         IFEQ      *OFF
     C     KKZIPA        ANDLE     GZTOZP
     C                   MOVE      GZZIPA        EOZIPA
     C                   ELSE
     C                   MOVE      *BLANKS       EOZIPA
     C                   ENDIF
     C                   ENDIF
     C*
     C     XXCLT#        IFEQ      '114'
     C*** Begin Add *** 04/10/19 ****************************************
     C     XXCLT#        OREQ      '152'
     C*** End   Add *** 04/10/19 ****************************************
     C*
     C     Q1PRMS        IFGT      9
     C     @@DCID        IFNE      *BLANKS
     C                   MOVEL(P)  @@DCID        EOIDOC
     C                   MOVE      *ON           OUTFLG
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C                   MOVEL     ##DSTY        EODSTY
     C*
     C                   WRITE     DBRCDF
     C*
     C*** BEG ADD *** 06/22/12 ****************************************
     C*
     C                   EVAL      SVRRN = DBRRN4
     C*
     C*** END ADD *** 06/22/12 ****************************************
     C*
     C                   FEOD      FAEOP
     C*
     C*** BEG ADD *** 06/22/12 ****************************************
     C*
     C                   EVAL      DBRRN4 = SVRRN
     C*
     C*** END ADD *** 06/22/12 ****************************************
     C*
     C                   SETOFF                                       32
     C*
     C                   EXSR      WRKFLW
     C     ##KL          IFEQ      '1'
     C                   SETON                                        90
     C                   ENDIF
     C*
     C*
     C* IF ADJUDICATION STATE DIFFERENT FROM POLICY STATE DETERMINE IF
     C* THE POLICY ST REQUIRES REPORTING FOR SERVICES OUTSIDE OF STATE.
     C*
     C     EOAST         IFNE      EOPOST
     C                   MOVEL(P)  EOPOST        KKAST
     C     STKY          CHAIN     PST02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C     1             SUBST     STBS06:1      OSEDI             1
     C*
     C* IF OUTSIDE STATE FLAG ON, SET CAP POLICY ST FLAG, CALL WRKFLW AGAIN
     C*  THEN SET FLAG COMPLETE
     C*
     C     OSEDI         IFEQ      'Y'
     C     DBRRN4        CHAIN     DBRCDF                             98
     C     *IN98         IFEQ      *OFF
     C                   MOVEL(P)  'Y'           EOCPST
     C                   UPDATE    DBRCDF
     C                   EXSR      WRKFLW
     C                   ENDIF
     C     EOCPST        IFEQ      'Y'
     C     DBRRN4        CHAIN     DBRCDF                             98
     C     *IN98         IFEQ      *OFF
     C                   MOVEL(P)  'C'           EOCPST
     C                   UPDATE    DBRCDF
     C                   ENDIF
     C                   ENDIF
     C*
     C* FLAG WAS NOT ON, SET CAP POLICY FLAG OFF
     C*
     C                   ELSE
     C     DBRRN4        CHAIN     DBRCDF                             98
     C     *IN98         IFEQ      *OFF
     C                   MOVEL     'N'           EOCPST
     C                   UPDATE    DBRCDF
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C*
     C* RESET INITIAL CURSOR POSITION
     C*
     C     @@SUB#        IFNE      *ZEROS
     C     @@FND         ANDNE     *ZEROS
     C     @@FYR         ANDNE     *ZEROS
     C     @@CASE        ANDNE     *ZEROS
     C                   SETON                                        31
     C                   SETOFF                                       38
     C                   ELSE
     C                   SETOFF                                       31
     C                   SETON                                        38
     C                   ENDIF
     C*
     C     XXCLT#        IFEQ      '114'
     C*** Begin Add *** 04/10/19 ****************************************
     C     XXCLT#        OREQ      '152'
     C*** End   Add *** 04/10/19 ****************************************
     C*
     C     Q1PRMS        IFGT      9
     C     @@DCID        IFNE      *BLANKS
     C*** Begin Chg *** 07/16/14 ***********************************************
     C**********         Z-ADD     *HIVAL        KKIDT
     C**********         Z-ADD     *HIVAL        KKITIM
     C*****ESKY1         SETGT     PES01
     C*****ESKY2         READPE    PES01                                  99
     C******IN99         IFEQ      *OFF
     C*****ESBLST        IFEQ      'P'
     C*****ESSTCD        ANDEQ     'CRP'
     C*****ESBLST        OREQ      'C'
     C**********         MOVEL(P)  'REMOVE'      @@RTRN
     C**********         ELSE
     C**********         MOVEL(P)  *BLANKS       @@RTRN
     C**********         ENDIF
     C**********         ELSE
     C*****EOKY*         CHAIN     PEO01                              99
     C*****EOCRNE        IFEQ      'Y'
     C**********         MOVEL(P)  'REMOVE'      @@RTRN
     C**********         ELSE
     C**********         MOVEL(P)  *BLANKS       @@RTRN
     C**********         ENDIF
     C**********         ENDIF
     C                   Eval      @@rtrn = ' '
     C                   If        EOblSt='C'
     C                   Eval      @@rtrn = 'REMOVE'
     C                   EndIf
     C*** End   Chg *** 07/16/14 ***********************************************
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRKFLW - Call Workflow Program
     C*================================================================
     C     WRKFLW        BEGSR
     C*
     C** CHANGE USER PARMS
     C*
     C                   MOVE      EOCO#         #2CO#
     C                   MOVE      EOSUB#        #2SUB#
     C                   MOVE      EOFND         #2FND
     C                   CALL      'F0006R2'
     C                   PARM                    #2CO#
     C                   PARM                    #2SUB#
     C                   PARM                    #2FND
     C                   PARM      ' '           ##RTCD
     C*
     C* WORKFLOW
     C*
     C                   MOVE      EOAST         ##AST
     C                   MOVE      EOSYS         ##SYS
     C                   MOVE      EOBTYP        ##BTYP
     C                   MOVE      EOSTYP        ##STYP
     C                   MOVE      @@MODE        ##MODE            1
     C                   MOVE      EOSUB#        KKSUB#
     C                   MOVE      EOFND         KKFND
     C                   MOVE      EOFYR         KKFYR
     C                   MOVE      EOCASE        KKCASE
     C                   MOVE      EOEOB#        KKEOB#
     C                   MOVE      EOCHRG        ##CHRG
     C                   MOVE      HLCHRG        ##HCHG            9
     C*
     C* GET HEADER AND DETAIL INFORMATION IF NOT ADD MODE
     C*
     C                   Z-ADD     0             ##RRN
     C                   Z-ADD     0             ##RRN3
     C     @@MODE        IFNE      'A'
      *** BEG ADD *** 04/05/07 ****************************************
     C     EOCPST        OREQ      'Y'
      *** END ADD *** 04/05/07 ****************************************
     C*
     C     EOSYS         IFEQ      'P'
     C     EOBTYP        ANDEQ     'N'
     C*
      *** BEG ADD *** 01/18/99 ****************************************
     C     EOSYS         OREQ      'V'
     C     EOBTYP        ANDEQ     'V'
      *** END ADD *** 01/18/99 ****************************************
     C*
     C     EHKY          CHAIN     PEH01                              99
     C  N99              Z-ADD     DBRRN2        ##RRN
     C     DHKY          CHAIN     PDH01                              99
     C  N99              Z-ADD     DBRRN3        ##RRN3            9 0
     C                   ENDIF
     C*
     C     EOSYS         IFEQ      'H'
     C     EOBTYP        ANDEQ     'H'
     C     EUKY          CHAIN     PEU01                              99
     C  N99              Z-ADD     DBRRN5        ##RRN
     C     DUKY          CHAIN     PDU01                              99
     C  N99              Z-ADD     DBRRN6        ##RRN3            9 0
     C                   ENDIF
     C*
     C     EOSYS         IFEQ      'P'
     C     EOBTYP        ANDEQ     'X'
     C     EPKY          CHAIN     PEP01                              99
     C  N99              Z-ADD     DBRRN7        ##RRN
     C     DPKY          CHAIN     PDP01                              99
     C  N99              Z-ADD     DBRRN8        ##RRN3
     C                   ENDIF
     C*
     C     EOSYS         IFEQ      'P'
     C     EOBTYP        ANDEQ     'D'
     C     EDKY          CHAIN     PED01                              99
     C  N99              Z-ADD     DBRRN9        ##RRN
     C     DDKY          CHAIN     PDD01                              99
     C  N99              Z-ADD     DBRRN0        ##RRN3
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* GET FAEOP RELATIVE RECORD NUMBER
     C*
     C     @@MODE        IFEQ      'A'
     C                   Z-ADD     DBRRN4        ##RRN2
     C                   ELSE
     C                   Z-ADD     @@RRN         ##RRN2            9 0
     C                   ENDIF
     C*
     C                   MOVEL(P)  'FTABL'       ##FIL1           10
     C                   MOVEL(P)  'FMPRL'       ##FIL2           10
     C                   MOVEL(P)  'FMSVP'       ##FIL3           10
     C                   MOVEL(P)  'FMSVL1'      ##FIL4           10
     C                   MOVEL(P)  'FMSVL2'      ##FIL5           10
     C                   MOVEL(P)  'FTPSL'       ##FIL6           10
     C                   MOVEL(P)  'FMPRP'       ##FIL7           10
     C                   MOVEL(P)  'FAAGP'       ##FIL8           10
     C*
     C                   MOVEL(P)  'FTFEL'       ##FIL9           10
     C                   MOVEL(P)  'FTFEL1'      ##FL10           10
     C*
     C     EOCPST        IFEQ      'Y'
     C     XXEDI         OREQ      'E'
     C     EOPOST        ANDNE     *BLANKS
     C                   MOVEL(P)  EOPOST        QQPOST
     C                   ELSE
     C                   MOVE      *BLANKS       QQPOST
     C                   ENDIF
     C*
     C*
     C                   CALL      'F0350C01'
     C                   PARM                    ##AST
     C                   PARM                    ##SYS
     C                   PARM                    ##BTYP
     C                   PARM                    ##STYP
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##MODE
     C                   PARM                    ##RRN
     C                   PARM                    ##RRN2
     C                   PARM                    ##RRN3
     C                   PARM                    ##FIL1
     C                   PARM                    ##FIL2
     C                   PARM                    ##FIL3
     C                   PARM                    ##NWCD
     C                   PARM                    ##CHRG
     C                   PARM                    ##FIL4
     C                   PARM                    ##FIL5
     C                   PARM                    ##FIL6
     C                   PARM                    ##FIL7
     C                   PARM                    XXEDI
     C                   PARM                    ##HCHG
     C                   PARM                    ##FIL8
     C                   PARM                    QQPOST
     C                   PARM                    ##FIL9
     C                   PARM                    ##FL10
     C*
     C                   ENDSR
     C*
     C*
     C*================================================================
     C* MSG114 - Display "Correct Coding" Info message for clt114
     C*================================================================
     C*
     C     MSG114        BEGSR
     C*
     C                   MOVE      *BLANKS       KKDCID
     C*
     C* TEST PARMS IF NO @@DCID THEN GET FROM EOMB TRANS FILE
     C*
b01  C     Q1PRMS        IFLT      10
     C*
     C     EOKY          CHAIN     PEO01                              99
     C*
b02  C     *IN99         IFEQ      *OFF
     C                   MOVEL     EOIDOC        KKDCID
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   MOVEL     @@DCID        KKDCID
     C*
     C                   ENDIF
     C*
     C*
     C*  IF no record in FAESP where ESBLST=  C (COMPLETED BILL)
     C*
     C     ESKY          CHAIN     PES02                              60
     C*
b01  C     *IN60         DOWEQ     *OFF
     C     ESBLST        ANDNE     'C'
     C     ESKY          READE     PES02                                  60
e01  C                   ENDDO
     C*
b01  C     *IN60         IFEQ      *ON
     C*
     C*  Chain out to FAAUL1 using
     C*  EOCO#/EOSUB#/EOFND/EOFYR/EOCASE/EOIDOC
     C*  If a record is found then Display info window.
     C*
     C     AUKY          CHAIN     PAU01                              60
     C*
b02  C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  AUFRC1        SCFRC1
     C                   MOVEL(P)  AUFRC2        SCFRC2
     C                   MOVEL(P)  AUTOC1        SCTOC1
     C                   MOVEL(P)  AUTOC2        SCTOC2
     C                   MOVEL(P)  AUREAS        SCREAS
     C                   WRITE     SCR01
     C                   EXFMT     WDW114
e02  C                   ENDIF
     C*
e01  C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*
