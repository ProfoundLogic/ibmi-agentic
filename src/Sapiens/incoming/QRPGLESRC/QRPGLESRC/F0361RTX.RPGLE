     Hcopyright('(c) 2025 Sapiens, Inc. All rights reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H BNDDIR('WTBNDDIR')
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H*  Program: F0361RTX                                            *
     H*  Description: Hospital (UB) Header Information - TEXAS        *
     H*  Created: 11/30/98  C:56 Project: MC152                       *
     H*****************************************************************
     H*  REVISIONS
     H*
     H*  07/18/25  C:342 Project MC451                                *
     H*                  NCCI Med Data Call Enhancements              *
     H*                                                               *
     H*  02/08/18 C:180 PHASWARE: 64328 Client 099
     H*                 Fix Occurrance codes not displaying.
     H*                 Use only first 2 characters per RM.
     H*
     H*  04/10/15 C:079 PHASWARE: 32921 Client 025
     H*                 Lift edit on discharge date if OPPS bill
     H*
     H*  10/01/15  C:079 PHW37605                                     *
     H*                  Check ICD-10 Indicator based on date and     *
     H*                  only in add mod                              *
     H*                                                               *
     H*  06/16/15  C:309 PHW34136                                     *
     H*                  Add check on NPI# so that it can contain only*
     H*                  numeric values                               *
     H*
     H*  09/17/15 C:079 Support: 099/37167                            *
     H*                 Add type to ICD-10 Key list for editing       *
     H*
     H*  04/16/15 C:079 Project: MC395
     H*                 Change edit on ICD9 procedure codes
     H*
     H*  01/23/15 C:280 PHASWARE: 32491 Client 025
     H*                 Add validation to scacct do not allow special
     H*                 characters
     H*
     H*  01/21/15 C:079 SUPPORT: MC025 32880
     H*                 Use MoveL instead of move on patient reason
     H*
     H*  08/28/14 C:296 SUPPORT: 86539
     H*                 Use MoveL instead of move in admission and
     H*                 discharge hours.
     H*
     H*  06/16/14 C:274 SUPPORT: 82891
     H*                 Discharge date required for outpatient bills.
     H*
     H*  04/24/14 C:280 Support: 85416
     H*                 change field size on screen rlic recompile
     H*
     H*  02/16/14 C:079 PROJECT: MC329 Phase II
     H*                 Correct loading of SCPREA
     H*
     H*  02/06/14 C:0790PROJECT: MC329 Phase II
     H*                 Correct clearing of ICD10 fields
     H*
     H*  08/05/13 C:277 PROJECT: MC329 Phase II
     H*                 Add search functionality and Validate ICD10
     H*
     H*  05/23/13 C:277 PROJECT: MC329
     H*                 Activate the ICD10 fields if ICD10 is active
     H*
     H*  01/10/12  C:251  Project 25MSO
     H*                   Add field 'Attending Provider License Number'
     H*
     H*  09/30/11  C:280  SUPPORT 78098 MC144
     H*                   ADD PADDING TO EURLIC
     H*
     H*  09/09/11  C:251  SUPPORT 77922 MC025
     H*                   Removed edit on Patient reason code
     H*
     H*  07/07/11  C:251  Project TX070
     H*                   Change edits on fields,add new screens
     H*
     H*  04/05/11  C:257  Support 71931
     H*                   Assigning RRN to @@RRN after WRITEDBRCDF to
     H*                   resolve duplicate records in file FAEHP.
     H*
     H*  11/10/09  C:062  Project 144MBP
     H*                   -Move primary diagnosis to screen 2
     H*                   -Default the submission date
     H*
     H*  05/20/09  C:198  Project FS343
     H*                   Re-compiled as FTOPP structure has been changed
     H*
     H*  05/07/09  C:079  Support 67061
     H*                   Load SCRLNM, SCRFNM to the database
     H*
     H*  02/07/09  C:079  Support 67061
     H*                   Load SCRLNM, SCRFNM to the database
     H*
     H*  12/18/08  C:198  Support 66191
     H*                   -PCM edits are removed when SCICN is in
     H*                    (AO, AS, HO, HS).
     H*
     H*  10/30/08  C:198  Project TX048
     H*                   Field mapping for SCRLIC changed from EORLIC to
     H*                    EURLIC
     H*
     H*  09/24/08  C:198  Project TX039 (Priss# 25)
     H*                   -Discharge date and Hr should not be made
     H*                    mandatory for outpatients.
     H*
     H*  09/15/08  C:198  Support: 64777 (Project TX039)
     H*                   -Fix issue with SCDRG# where it was mandatory
     H*                    for admission date less than 1080301
     H*
     H*  08/21/08 C:180 PROJECT:MC294 PRISS 22
     H*                         Do not underline fields of SCR03 when
     H*                         in display mode.
     H*
     H*
     H*  08/11/08  C:189  Project: TX039
     H*                   -Fix issue with SCMCAR  after the change in
     H*                    keylist for FTPFL(PFEFDT to PFFFDT)
     H*
     H*  07/15/08 C:180 PROJECT:MC294 Phase II
     H*                         Add NPI# fields to SCR03.
     H*
     H*  06/27/08  C:079  Project: TX039
     H*                   -Fix issue with screen2 after trans status
     H*                    search
     H*
     H*  06/25/08  C:189  Project: TX039
     H*                   - Make Disharge date mandatory as required in calc.
     H*
     H*
     H*  06/12/08  C:198  Project: TX039
     H*                   - Changed FTOIP to FTOPP
     H*
     H*  04/07/08  C:189  Project: TX039
     H*                   -Added DRG# and Transfer status field
     H*
     H*  04/03/08  C:198  Project: TX039
     H*                   -Added Implant Status Field.
     H*
     H*  04/18/07 C:188 PROJECT:FL061
     H*                 SET EDITST TO ADJ OR POLICY STATE BASED ON EOCPST
     H*
     H*  03/13/07 C:079 SUPPORT: 57363
     H*                 -REMOVE ROOM CHARGES FIELD
     H*
     H*  01/29/07 C:079 SUPPORT: 56760
     H*                 -REMOVE SOME EDITS ON PRINC PROC CODE
     H*
     H*  05/10/06 C:079 SUPPORT: 52497
     H*                 -ADDITIONAL EDITS ON THE ADMIT HOURS
     H*
     H*  09/21/05 C:079 PROJECT: 25MNZ
     H*                 -ADD REFERRING PROVIDER LIC#
     H*
     H*  02/18/05 C:079 PROJECT: 25MNZ
     H*                 -ADD NEW FIELD TYPE OF ADMISSION
     H*
     H*  08/06/03 C:062 PROJECT: FSMNT # 264
     H*                 -NO SEARCH ON PRINCIPAL PROCEDURE CODE IF
     H*                  PCM IS 0, 4 OR 5
     H*                 -IF SEARCHING ON THIS FIELD THEN SET
     H*                  INDICATOR 90 ON TO KEEP FROM GOING TO NEXT
     H*
     H*  03/12/03 C:079 SUPPORT: MC044 36319
     H*                 CHECK THE STATUS OF DIAG CODES
     H*
     H*  04/21/02 C:079 PROJECT: GA015
     H*                 Use state id for Proc Code Method lookup
     H*
     H*  02/15/02 C:079 SUPPORT: MC044 31188
     H*                 If specialty code is blank on SCR01 then it
     H*                 s/b blank on SCR03
     H*
     H*  10/19/01  C:62   Project: MC025 29389
     H*                   Add edit on submission date:
     H*                   - Submission date < EUSCPT
     H*
     H*  10/15/01  C:62   Project: MC025 29089
     H*                   Add edit on submission date:
     H*                   - RCN = 0; submission date > EOCRDT
     H*                   - RCN > 0; submission date > ESRRDT
     H*
     H*  10/05/01  C:79   Project: FSMNT
     H*                   Add edit on admit date. Cant be > Statement
     H*                   covers from.
     H*
     H*  07/12/01  C:79   Support: MC025 25456
     H*                   1. If procedure code method is not 0 then
     H*                      search and edit on 'O' types for procedure
     H*                      codes.
     H*
     H*  07/06/01 C:079 PROJECT: FSMNT #196
     H*                 ADD EDIT TO SPECIALTY CODE
     H*
     H*  05/30/01 C:062 PROJECT: FS229
     H*                 POPULATE EOMB# SCREEN FIELD
     H*
     H*  04/05/01  C:79   Support: MC025 25456
     H*                   1. If procedure code method is not 0 then
     H*                      require principle procedure code.
     H*                   2. If type of service= ISI,ISF,HSI,HSF,ASI
     H*                      ASF require principle procedure code and
     H*                      method.
     H*
     H*  01/02/01  C:062  Proj: MCMNT   BASE ONLY
     H*                   WHEN ENTERING SCR01 POSITION TO PAT ACCT #
     H*
     H*  12/05/00  C:079  Proj: MCMNT
     H*                   DSPATR(HI) ON ROOM CHARGES FIELD IF INPATIENT
     H*                   BILL.
     H*
     H*  11/22/00  C:062  Proj: FS212
     H*                   COMPILE ONLY.
     H*
     H*  11/08/00  C:62   PROJECT:FS146
     H*                   CHANGE 'BLTY' TO 'ZZBT' AND 'FLBT'.
     H*
     H*  11/06/00  C:062  Support FS215
     H*                   SWAP OUT F2310R WITH F2013R.
     H*
     H*  10/13/00  C:062  Support MC238
     H*                   ADD LOGIC TO DEAL WITH NON-SEARCHABLE FIELDS.
     H*
     H*  08/30/00  C:062  PROJ: MC071 24169
     H*                   ERROR ON TYPE OF BILL FIELD.
     H*
     H*  08/29/00  C:062  PROJ: MCMNT
     H*                   ADD DISCHARGE DATE.
     H*
     H*  06/29/00  C:117 PROJECT: TX011                               *
     H*                - ADDED EDIT FOR TYPE OF SERVICE (STATUS) I/F  *
     H*                - CHG EDIT TO REQUIRE PRINCIPAL DIAG CODE ONLY *
     H*                  IF TYPE OF SERVICE IS ISI, ISF, HSI, HSF,    *
     H*                  ASI, AND ASF.  IT IS MADE UP OF 2 CHAR TYPE, *
     H*                  AND 1 CHAR STATUS. (EUICN & EUDCN)           *
     H*                                                               *
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
     H*  02/06/00 C:79  Support: MC048/#21639
     H*                 If tape flag=N don't edit TX Lic#             *
     H*                                                               *
     H*  11/12/99 C:79  MC152
     H*                 Delete/Change efficiency isssue.
     H*
     H*  09/10/99 C:56  Project: KY010                                *
     H*                 Write SCEDI to EOTAPE                         *
     H*                                                               *
     H*  06/24/99  C:56  Support: MC025 #18405
     H*                  Load LOS value of 'D' just before validating
     H*                  Principle Pocedure Code.  This will prevent
     H*                  old LOS value from validation on SCR03.
     H*
     H*  02/17/99  C:94  PROJ: SUP16673
     H*                  CHANGE PROGRAM SO THAT IF NOTHING IS FOUND
     H*                  ON THE CHAIN WITH AN 'O' THEN CHAIN WITH A
     H*                  'D'.  IF STILL NO MATCH, THEN SETON ERROR
     H*                  MESSAGE.
     H*
     H*****************************************************************
     H*
     FF0361FTX  CF   E             WORKSTN
     F                                     Handler('PROFOUNDUI(HANDLER)')
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
     FSTSTL1    IF   E           K DISK
     F                                     RENAME(PST01:PST02)
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
     FFAEUL     IF   E           K DISK
     F                                     INFDS(DBFDSA)
     FFTGTP     IF   E             DISK
     FFTGTL     IF   E           K DISK
     F                                     RENAME(PGT01:PGT02)
     F*  PROCEDURE CODE MASTER FILE
     F*  RESONABLE AND CUSTOMARY FILE
     FFAESL2    IF   E           K DISK
     FFTOPL     IF   E           K DISK
     F                                     RENAME(POP01:LOP01)
     FFTOPP     IF   E             DISK
     FFTPFL     IF   E           K DISK
     F                                     RENAME(PTP01:LII01)
     FFTPFP     IF   E             DISK
     F*FTIIL   IF  E           K        DISK
     FFTDGP     IF   E             DISK
     FFTDGL     IF   E           K DISK
     F                                     RENAME(PDG01:PDG02)
     F*** Beg Add *** 08/05/13 ***************************************
     Ffmicd10l1 if   e           k disk
     F*** End Add *** 08/05/13 ***************************************
     F*** Beg Add *** 07/18/25 ***************************************
     F*  Provider Auxillary File
     FFMP1L     IF   E           K DISK
     F*** End Add *** 07/18/25 ***************************************
      /COPY SCOPYMT05D
     D*
     D**/COPY SCOPYMT01I**
     D*
     D DBFDSA          DS
     D  DBRRNA               397    400B 0
     D                 DS
     D  SCTOS                  1      3
     D  SCICN                  1      2
     D  SCDCN                  3      3
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Hospital (UB) Header-
     D                                      Information')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(4)

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
     D*** Beg Add *** 01/23/15 ***************************************
     D error           s              1
     D mode            s              1
      /COPY sprstring
     D*** End Add *** 01/23/15 ***************************************
     D***** Beg Add *** 06/16/15 **************************************
     DXXANPI           S             30A   Inz(*Blanks)
     DXXNPI1           S             30A   Inz(*Blanks)
     DXXNPI2           S             30A   Inz(*Blanks)
     D***** End Add *** 06/16/15 **************************************
     D*** Beg Add *** 07/18/25 ***************************************
     D invalidPos      s              2  0
      /copy sPrModule
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
     C* Key List for Bill History File
     C*
     C     ESKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C*
     C     ESKY2         KLIST
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
     C**KEY*LIST FOR REASONABLE AND CUSTOMARY FILE
     C     DGKY          KLIST
     C                   KFLD                    KKDRG#
     C                   KFLD                    KIEFDT
     C     IPKY          KLIST
     C                   KFLD                    KKPROV
     C*
     C*** Beg Add *** 08/05/13 ***************************************
     C*** KEY LIST FOR DIAGNOSIS CODE 10 MASTER FILE
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
     C     *LIKE         DEFINE    SCPCM         HLPCM
     C     *LIKE         DEFINE    SCPDSC        HLDESC
     C     *LIKE         DEFINE    EUSBDT        HLSBDT
     C     *LIKE         DEFINE    EUDCDT        HLDCDT
     C     *LIKE         DEFINE    EORCN         KKRCN
     C     *LIKE         DEFINE    ESIDT         KKIDT
     C     *LIKE         DEFINE    ESITIM        KKITIM
     C     *LIKE         DEFINE    EOAST         EDITST
     C     *LIKE         DEFINE    OPPROV        KKPROV
     C     *LIKE         DEFINE    OPEFDT        KPEFDT
     C     *LIKE         DEFINE    DGDRG#        KKDRG#
     C     *LIKE         DEFINE    DGEFDT        KIEFDT
     C     *LIKE         DEFINE    SCASRC        HLASRC
     C     *LIKE         DEFINE    SCDCST        HLDCST
     C     *LIKE         DEFINE    SCCOC1        HLOCA
     C     *LIKE         DEFINE    SCOCD1        HLOCD
     C     *LIKE         DEFINE    EUADDT        HLFDAT
     C     *LIKE         DEFINE    EUADDT        HLTDAT
     C     *LIKE         DEFINE    EUADDT        HLODAT
     C     *LIKE         DEFINE    SCOSD         HLOSD
     C     *LIKE         DEFINE    SCVC01        HLVCD
     C     *LIKE         DEFINE    EUADDI        HLADDI
     C     *LIKE         DEFINE    SCPREA        HLPREA
     C*
     C/COPY SCOPYMT05C
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
     C                   Z-ADD     SCSBDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCSBDT
     C*
     C                   ENDSR
     C*================================================================
     C* DTSBR2 - Date check subrountine screen 2
     C*================================================================
     C     DTSBR2        BEGSR
     C* THIS SUBROUTINE IS CALLED AFTER EVERY SEARCH TO PROPERLY
     C* FORMAT ALL DATES ON SCREEN
     C*
     C                   Z-ADD     SCODT1        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCODT1
     C*
     C                   Z-ADD     SCODT2        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCODT2
     C*
     C                   Z-ADD     SCODT3        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCODT3
     C*
     C                   Z-ADD     SCODT4        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCODT4
     C*
     C                   Z-ADD     SCOSFD        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCOSFD
     C*
     C                   Z-ADD     SCOSTD        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCOSTD
     C*
     C                   Z-ADD     SCOFD2        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCOFD2
     C*
     C                   Z-ADD     SCOTD2        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCOTD2
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
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C                   SETOFF                                       252653
     C                   SETOFF                                       71
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

       //*** Beg add ***** 07/18/25 ***********************************
       If isModEnabled('NCCI Med Data Call');
          If scfzip = *zeros and EOrcn = 0;
             msfzip = 'MCR0503';
             erfzip = *on;
             *in90 = *on;
             leavesr;
          Endif;
       Endif;
       //*** End add ***** 07/18/25 ************************************
     C*
     C* SET FLAG TO INDICATE ERROR CHECK HAS BEEN DONE ON SCREEN ONE
     C* SO NEXT TIME MVDBSC IS CALLED WE KNOW WE ARE ON SCR03
     C*
     C** SCR2 HAS BEEN CHANGED TO SCR 3
     C*
     C                   MOVE      *ON           XXSCR3
     C*
     C*
     C* PRINCIPAL DIAGNOSIS CODE IS REQUIRED
     C*
     C*
     C** VALIDATE PRINCIPAL DIAGNOSIS CODE IF ENTERED
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
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9073
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
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
     C* STATEMENT COVERS TO DATE MUST BE < STATEMENT COVERS TO DATE
     C*
     C     HLSCPF        IFGT      HLSCPT
     C                   SETON                                        9080
     C                   GOTO      CK1END
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
     C                   SETON                                        9081
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     SRDTO         SCADDT
     C                   ENDIF
     C                   ENDIF                                                  SCADDT NE *ZEROS
     C*
     C                   Z-ADD     SCADDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLADDT
     C*
     C* ADMISSION DATE CANNOT BE LESS THAN ACCIDENT DATE
     C*
     C     HLADDT        IFNE      *ZEROS
     C     HLADDT        ANDLT     EOACDT
     C                   SETON                                        9089
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLADDT NE *ZEROS
     C*
     C* ADMISSION DATE CANNOT BE GREATER THAN SYSTEM DATE
     C*
     C     HLADDT        IFNE      *ZEROS
     C     HLADDT        ANDGT     HLSYDT
     C                   SETON                                        9050
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLADDT NE *ZEROS
     C*
     C* ADMISSION DATE CANNOT BE GREATER THAN STATEMENT COVERS FROM
     C*
     C     HLADDT        IFNE      *ZEROS
     C     HLADDT        ANDGT     HLSCPF
     C                   SETON                                        9067
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLADDT NE *ZEROS
     C     SCADDT        IFNE      *ZEROS
     C     SCADHR        ANDEQ     *ZEROS
     C* Admission hr is required for all inpatient except for skilled nursing (b
     C     SCICN         IFEQ      'IR'
     C     SCICN         OREQ      'IP'
     C     SCICN         OREQ      'IM'
     C     SCICN         OREQ      'IS'
     C                   MOVEL     SCBLTY        XXBLT             2
     C*
     C     XXBLT         IFLT      '21'
     C     XXBLT         ORGT      '22'
     C*
     C                   SETON                                        9093
     C                   GOTO      CK1END
     C                   ENDIF                                                  XXBLT  <> '21' - '22
     C*
     C                   ELSE
     C                   MOVEL     SCBLTY        XXBLTY            1
     C     XXBLTY        IFGE      '1'
     C     XXBLTY        ANDLE     '3'
     C     XXBLTY        OREQ      '7'
     C                   SETON                                        9093
     C                   GOTO      CK1END
     C                   ENDIF                                                  XXBLTY IFGE
     C                   ENDIF                                                  INPATIENT BILL
     C                   ENDIF                                                  SCADDT IFNE
     C*
     C* ADMIT HOUR AND DISCHARGE HOUR MUST BE BETWEEN 1 AND 24
     C* IF ENTERED
     C*
     C     SCADHR        IFNE      *ZEROS
     C     SCADHR        IFGT      24
     C                   SETON                                        9051
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCADHR LT 1
     C                   ENDIF                                                  SCADHR NE *ZEROS
     C*
     C* DISCHARGE DATE IS REQUIRED
     C*
     C                   MOVE      '0'           *IN60
     C     SCICN         IFEQ      'IR'
     C     SCICN         OREQ      'IP'
     C     SCICN         OREQ      'IM'
     C     SCICN         OREQ      'IS'
     C*** Beg Add *** 06/16/14 ****************************************
     C*** Beg Del *** 04/10/15 ****************************************
     C**** SCICN         OREQ      'HO'
     C**** SCICN         OREQ      'HS'
     C**** SCICN         OREQ      'AS'
     C**** SCICN         OREQ      'AO'
     C*** End Del *** 04/10/15 ****************************************
     C*** End Add *** 06/16/14 ****************************************
     C     SCDCDT        IFEQ      *ZEROS
     C                   SETON                                        9060
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCDCDT = *ZEROS
     C                   ENDIF
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
     C* TYPE OF ADMISSION IS REQUIRED
     C*
     C     SCADTY        IFEQ      *BLANKS
     C                   SETON                                        9095
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C* VALIDATE TYPE OF ADMISSION
     C*
     C                   MOVEL(P)  'TXTA'        KKTBID
     C                   MOVEL(P)  SCADTY        KKKEY
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *ON
     C     GTSTAT        OREQ      'I'
     C                   SETON                                        9094
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C*
     C     SCASRC        IFEQ      *BLANKS
     C                   SETON                                        9025
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCASRC = *BLANKS
     C*
     C* VALIDATE ADMISSION SOURCE
     C*
     C                   MOVEL(P)  'ADSR'        KKTBID
     C                   MOVEL(P)  SCASRC        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9026
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C*
     C     SCDCDT        IFNE      *ZEROS
     C     SCDCHR        ANDEQ     *ZEROS
     C*
     C* Discharge hr is required for all inpatient except for skilled nursing (b
     C     SCICN         IFEQ      'IR'
     C     SCICN         OREQ      'IP'
     C     SCICN         OREQ      'IM'
     C     SCICN         OREQ      'IS'
     C                   MOVEL     SCBLTY        XXBLT             2
     C                   MOVE      SCBLTY        XXBLTL            1
     C*
     C     XXBLT         IFLT      '21'
     C     XXBLT         ORGT      '22'
     C*
     C     XXBLTL        IFEQ      '1'
     C     XXBLTL        OREQ      '4'
     C*
     C                   SETON                                        9092
     C                   GOTO      CK1END
     C                   ENDIF                                                  XXBLTL <> 1,4
     C                   ENDIF                                                  XXBLT  <> '21' - '22
     C*
     C                   ELSE
     C                   SETON                                        9092
     C                   GOTO      CK1END
     C                   ENDIF                                                  NOT AN INPATIENT BIL
     C                   ENDIF                                                  SCDCHR LT 1
     C*
     C     SCDCHR        IFNE      *ZEROS
     C     SCDCHR        IFGT      24
     C                   SETON                                        9052
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCDCHR LT 1
     C                   ENDIF                                                  SCADHR NE *ZEROS
     C*
     C* SUBMISSSION DATE IS REQUIRED
     C*
     C     SCSBDT        IFEQ      *ZEROS
     C                   SETON                                        9057
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCSBDT = *ZEROS
     C*
     C* VALIDATE THE SUBMISSION DATE
     C*
     C                   Z-ADD     SCSBDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9058
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     SRDTO         SCSBDT
     C                   ENDIF
     C*
     C* SUBMISSION DATE CANNOT BE LESS THAN DOI
     C*
     C*
     C                   Z-ADD     SCSBDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLSBDT
     C*
     C     HLSBDT        IFLT      EOACDT
     C                   SETON                                        9059
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLSBDT LT EOACDT
     C*
     C* IF RECON IS 0, THEN SUBMISSION DATE MUST BE LESS THAN
     C* OR EQUAL TO CARRIER RECIEVE DATE.
     C*
     C     EORCN         IFEQ      *ZEROS
     C*
     C     HLSBDT        IFGT      EOCRDT
     C                   SETON                                        6990
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C* IF RECON <> 0, THEN SUBMISSION DATE MUST BE LESS THAN
     C* OR EQUAL TO RECON RECIEVED DATE.
     C*
     C                   Z-ADD     *HIVAL        KKIDT
     C                   Z-ADD     *HIVAL        KKITIM
     C                   Z-ADD     EORCN         KKRCN
     C                   SETON                                        9790
     C*
     C     ESKY2         SETGT     PES01
     C     ESKY1         READPE    PES01                                  99
     C     *IN99         DOWEQ     *OFF
     C     ESBLST        IFEQ      'R'
     C     HLSBDT        ANDLE     ESRRDT
     C                   SETOFF                                       9790
     C                   LEAVE
     C                   ENDIF
     C     ESKY1         READPE    PES01                                  99
     C                   ENDDO
     C*
     C                   ENDIF
     C*
     C* SUBMISSION DATE MUST BE >= STATEMENT COVERS TO DATE
     C*
     C     HLSBDT        IFLT      HLSCPT
     C                   SETON                                        9690
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C     SCDCST        IFEQ      *BLANKS
     C                   SETON                                        9053
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCDCST = *BLANKS
     C*
     C* VALIDATE DISCHARGE STATUS
     C*
     C                   MOVEL(P)  'DCST'        KKTBID
     C                   MOVEL(P)  SCDCST        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9071
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C*
     C* VALIDATE BOTH TYPES OF SERVICE
     C*
     C     SCICN         IFEQ      *BLANKS
     C                   SETON                                        9082
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCICN = *ZEROS
     C*
     C                   MOVE      'SVTY'        KKTBID
     C                   MOVEL(P)  SCICN         KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9083
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C*
     C*  TYPE OF SERVICE (STATUS) CANNOT BE BLANK
     C*
     C     SCDCN         IFEQ      *BLANKS
     C                   SETON                                        9065
     C                   GOTO      CK1END
     C                   ELSE
     C                   SETOFF                                       9065
     C                   ENDIF
     C*
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
     C* IF THIS IS AN INPATIENT BILL THEN DSPATR(UL) ON SCROOM (SCR03)
     C     SCBLTY        IFGE      '111'
     C     SCBLTY        ANDLE     '115'
     C                   SETON                                        66
     C                   ENDIF
     C*
     C* THE FOLLOWING CORRESPONDENCE SHOULD EXIST BETWEEN
     C*          TYPE OF BILL AND TYPE OF SERVICE
     C*          --------------------------------
     C*          111 - 115    -->   IR, IP, IM, IS
     C*          131 - 135    -->   HS, HO, AS, AO
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
     C*** beg add *** 01/23/15 ****************************************
     C*
      /Free

       ERACCT1 = *OFF;
       MODE    = 'S';
       ERROR   = ' ';

       eracct1 = validchar(scacct:mode:error);

       If  eracct1 = *on;
         *IN90 = *ON;
       ENDIF;

      /End-Free
     C*
     C*** end add *** 01/23/15 ****************************************
     C** POSITION CURSOR FOR SECOND SCREEN
     C*
     C     *IN90         IFNE      *ON
     C                   SETON                                        93
     C                   ENDIF
     C*
     C     CK1END        ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C*** INITIALIZE ERROR INDICATORS
     C*
     C                   SETOFF                                       6769
     C                   SETOFF                                       717273
     C                   SETOFF                                       747576
     C                   SETOFF                                       777879
     C                   SETOFF                                       808283
     C                   SETOFF                                       838485
     C                   SETOFF                                       868788
     C                   SETOFF                                       899192
     C                   SETOFF                                       939495
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLSYDT
     C*
     C* VALIDATE CONDITION CODES 1 -10
     C*
     C     SCCOC1        IFNE      *BLANKS
     C                   MOVEL(P)  'CCDE'        KKTBID
     C                   MOVEL(P)  SCCOC1        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        907193
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCCOC1 <> *BLANKS
     C*
     C     SCCOC2        IFNE      *BLANKS
     C                   MOVEL(P)  'CCDE'        KKTBID
     C                   MOVEL(P)  SCCOC2        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        907131
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCCOC2 <> *BLANKS
     C*
     C     SCCOC3        IFNE      *BLANKS
     C                   MOVEL(P)  'CCDE'        KKTBID
     C                   MOVEL(P)  SCCOC3        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        907132
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCCOC3 <> *BLANKS
     C*
     C     SCCOC4        IFNE      *BLANKS
     C                   MOVEL(P)  'CCDE'        KKTBID
     C                   MOVEL(P)  SCCOC4        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        907133
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCCOC4 <> *BLANKS
     C*
     C     SCCOC5        IFNE      *BLANKS
     C                   MOVEL(P)  'CCDE'        KKTBID
     C                   MOVEL(P)  SCCOC5        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        907134
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCCOC5 <> *BLANKS
     C*
     C     SCCOC6        IFNE      *BLANKS
     C                   MOVEL(P)  'CCDE'        KKTBID
     C                   MOVEL(P)  SCCOC6        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        907135
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCCOC6 <> *BLANKS
     C*
     C     SCCOC7        IFNE      *BLANKS
     C                   MOVEL(P)  'CCDE'        KKTBID
     C                   MOVEL(P)  SCCOC7        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        907136
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCCOC7 <> *BLANKS
     C*
     C     SCCOC8        IFNE      *BLANKS
     C                   MOVEL(P)  'CCDE'        KKTBID
     C                   MOVEL(P)  SCCOC8        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        907195
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCCOC8 <> *BLANKS
     C*
     C     SCCOC9        IFNE      *BLANKS
     C                   MOVEL(P)  'CCDE'        KKTBID
     C                   MOVEL(P)  SCCOC9        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        907138
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCCOC9 <> *BLANKS
     C*
     C     SCCO10        IFNE      *BLANKS
     C                   MOVEL(P)  'CCDE'        KKTBID
     C                   MOVEL(P)  SCCO10        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        907139
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCCO10 <> *BLANKS
     C*
     C     SCCO11        IFNE      *BLANKS
     C                   MOVEL(P)  'CCDE'        KKTBID
     C                   MOVEL(P)  SCCO11        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        907140
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCCO11 <> *BLANKS
     C*
     C*** VALIDATE OCCURANCE CODE 1-4 AND DATES 1-4
     C*
     C     SCOCD1        IFNE      *BLANKS
     C                   MOVEL(P)  'OCDE'        KKTBID
     C                   MOVEL(P)  SCOCD1        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        907141
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCOCD1 <> *BLANKS
     C*
     C* Occurance date 1
     C*
     C     SCODT1        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCODT1        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        907342
     C                   GOTO      CK2END
     C                   ELSE
     C                   MOVEL     SRDTO         SCODT1
     C                   ENDIF
     C*
     C** Should not be greater than system date
     C*
     C                   MOVE      *ZEROS        HLODAT
     C                   Z-ADD     SCODT1        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLODAT
     C*
     C     HLODAT        IFGT      HLSYDT
     C                   SETON                                        907442
     C                   GOTO      CK2END
     C                   ENDIF
     C*
     C                   ENDIF                                                  SCODT1 <> 0
     C*
     C*
     C*
     C     SCOCD2        IFNE      *BLANKS
     C                   MOVEL(P)  'OCDE'        KKTBID
     C                   MOVEL(P)  SCOCD2        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        907143
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCOCD2 <> *BLANKS
     C*
     C*
     C* Occurance date 2
     C*
     C     SCODT2        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCODT2        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        907344
     C                   GOTO      CK2END
     C                   ELSE
     C                   MOVEL     SRDTO         SCODT2
     C                   ENDIF
     C*
     C** Should not be greater than system date
     C*
     C                   MOVE      *ZEROS        HLODAT
     C                   Z-ADD     SCODT2        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLODAT
     C*
     C     HLODAT        IFGT      HLSYDT
     C                   SETON                                        907444
     C                   GOTO      CK2END
     C                   ENDIF
     C*
     C                   ENDIF                                                  SCODT2 <> 0
     C*
     C     SCOCD3        IFNE      *BLANKS
     C                   MOVEL(P)  'OCDE'        KKTBID
     C                   MOVEL(P)  SCOCD3        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        907145
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCOCD3 <> *BLANKS
     C*
     C* Occurance date 3
     C*
     C     SCODT3        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCODT3        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        907346
     C                   GOTO      CK2END
     C                   ELSE
     C                   MOVEL     SRDTO         SCODT3
     C                   ENDIF
     C*
     C** Should not be greater than system date
     C*
     C                   MOVE      *ZEROS        HLODAT
     C                   Z-ADD     SCODT3        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLODAT
     C*
     C     HLODAT        IFGT      HLSYDT
     C                   SETON                                        907446
     C                   GOTO      CK2END
     C                   ENDIF
     C*
     C                   ENDIF                                                  SCODT3 <> 0
     C*
     C     SCOCD4        IFNE      *BLANKS
     C                   MOVEL(P)  'OCDE'        KKTBID
     C                   MOVEL(P)  SCOCD4        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        907194
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCOCD4 <> *BLANKS
     C*
     C* Occurance date 4
     C*
     C     SCODT4        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCODT4        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        907348
     C                   GOTO      CK2END
     C                   ELSE
     C                   MOVEL     SRDTO         SCODT4
     C                   ENDIF
     C*
     C** Should not be greater than system date
     C*
     C                   MOVE      *ZEROS        HLODAT
     C                   Z-ADD     SCODT4        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLODAT
     C*
     C     HLODAT        IFGT      HLSYDT
     C                   SETON                                        907448
     C                   GOTO      CK2END
     C                   ENDIF
     C*
     C                   ENDIF                                                  SCODT4 <> 0
     C*
     C*** Occurance span code, from/to dates
     C*
     C     SCOSD         IFNE      *BLANKS
     C                   MOVEL(P)  'OSCD'        KKTBID
     C                   MOVEL(P)  SCOSD         KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        907149
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCOCD1 <> *BLANKS
     C*
     C* Occurance span from date
     C*
     C     SCOSFD        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCOSFD        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        907350
     C                   GOTO      CK2END
     C                   ELSE
     C                   MOVEL     SRDTO         SCOSFD
     C                   ENDIF
     C*
     C** Should not be greater than system date
     C*
     C                   MOVE      *ZEROS        HLFDAT
     C                   Z-ADD     SCOSFD        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLFDAT
     C*
     C     HLFDAT        IFGT      HLSYDT
     C                   SETON                                        907450
     C                   GOTO      CK2END
     C                   ENDIF
     C*
     C* If one date is entered, the other is mandatory
     C     SCOSTD        IFEQ      *ZEROS
     C                   SETON                                        907550
     C                   GOTO      CK2END
     C                   ELSE
     C*
     C* from date <= to date
     C* Convert from date
     C                   MOVE      *ZEROS        HLTDAT
     C                   Z-ADD     SCOSTD        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLTDAT
     C*
     C     HLFDAT        IFGT      HLTDAT
     C                   SETON                                        907250
     C                   GOTO      CK2END
     C                   ENDIF
     C*
     C                   ENDIF                                                  SCOSTD = *ZEROS
     C*
     C                   ENDIF                                                  SCOSFD <> 0
     C*
     C* Occurance span to date
     C*
     C     SCOSTD        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCOSTD        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        907351
     C                   GOTO      CK2END
     C                   ELSE
     C                   MOVEL     SRDTO         SCOSTD
     C                   ENDIF
     C*
     C** Should not be greater than system date
     C*
     C                   MOVE      *ZEROS        HLTDAT
     C                   Z-ADD     SCOSTD        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLTDAT
     C*
     C     HLTDAT        IFGT      HLSYDT
     C                   SETON                                        907451
     C                   GOTO      CK2END
     C                   ENDIF
     C*
     C* If one date is entered, the other is mandatory
     C     SCOSFD        IFEQ      *ZEROS
     C                   SETON                                        907551
     C                   GOTO      CK2END
     C                   ENDIF
     C*
     C                   ENDIF                                                  SCOSTD <> 0
     C*
     C*
     C     SCOSD2        IFNE      *BLANKS
     C                   MOVEL(P)  'OSCD'        KKTBID
     C                   MOVEL(P)  SCOSD2        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        907152
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCOSD2 <> *BLANKS
     C*
     C* Occurance span from date 2
     C*
     C     SCOFD2        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCOFD2        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        907353
     C                   GOTO      CK2END
     C                   ELSE
     C                   MOVEL     SRDTO         SCOFD2
     C                   ENDIF
     C*
     C** Should not be greater than system date
     C*
     C                   MOVE      *ZEROS        HLFDAT
     C                   Z-ADD     SCOFD2        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLFDAT
     C*
     C     HLFDAT        IFGT      HLSYDT
     C                   SETON                                        907453
     C                   GOTO      CK2END
     C                   ENDIF
     C*
     C* If one date is entered, the other is mandatory
     C     SCOTD2        IFEQ      *ZEROS
     C                   SETON                                        907553
     C                   GOTO      CK2END
     C                   ELSE
     C*
     C* from date <= to date
     C* Convert from date
     C                   MOVE      *ZEROS        HLTDAT
     C                   Z-ADD     SCOTD2        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLTDAT
     C*
     C     HLFDAT        IFGT      HLTDAT
     C                   SETON                                        907253
     C                   GOTO      CK2END
     C                   ENDIF
     C*
     C                   ENDIF                                                  SCOTD2 = *ZEROS
     C*
     C                   ENDIF                                                  SCOFD2 <> 0
     C*
     C* Occurance span to date
     C*
     C     SCOTD2        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCOTD2        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        907354
     C                   GOTO      CK2END
     C                   ELSE
     C                   MOVEL     SRDTO         SCOTD2
     C                   ENDIF
     C*
     C** Should not be greater than system date
     C*
     C                   MOVE      *ZEROS        HLTDAT
     C                   Z-ADD     SCOTD2        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLTDAT
     C*
     C     HLTDAT        IFGT      HLSYDT
     C                   SETON                                        907454
     C                   GOTO      CK2END
     C                   ENDIF
     C*
     C* If one date is entered, the other is mandatory
     C     SCOFD2        IFEQ      *ZEROS
     C                   SETON                                        907554
     C                   GOTO      CK2END
     C                   ENDIF
     C*
     C                   ENDIF                                                  SCOTD2 <> 0
     C*
     C*** VALUE CODE 1(A)
     C*
     C     SCVC01        IFNE      *BLANKS
     C                   MOVEL(P)  'VCDE'        KKTBID
     C                   MOVEL(P)  SCVC01        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        555671
     C                   SETON                                        90
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCVC01 <> *BLANKS
     C*
     C*** VALUE CODE 1(B)
     C*
     C     SCVC02        IFNE      *BLANKS
     C                   MOVEL(P)  'VCDE'        KKTBID
     C                   MOVEL(P)  SCVC02        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        557190
     C                   SETOFF                                       56
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCVC02 <> *BLANKS
     C*
     C*** VALUE CODE 1(C)
     C*
     C     SCVC03        IFNE      *BLANKS
     C                   MOVEL(P)  'VCDE'        KKTBID
     C                   MOVEL(P)  SCVC03        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        587190
     C                   SETOFF                                       57
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCVC03 <> *BLANKS
     C*
     C*** VALUE CODE 1(D)
     C*
     C     SCVC04        IFNE      *BLANKS
     C                   MOVEL(P)  'VCDE'        KKTBID
     C                   MOVEL(P)  SCVC04        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        598571
     C                   SETON                                        90
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCVC04 <> *BLANKS
     C*
     C*** VALUE CODE 2(A)
     C*
     C     SCVC05        IFNE      *BLANKS
     C                   MOVEL(P)  'VCDE'        KKTBID
     C                   MOVEL(P)  SCVC05        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        597190
     C                   SETOFF                                       85
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCVC05 <> *BLANKS
     C*
     C*** VALUE CODE 2(B)
     C*
     C     SCVC06        IFNE      *BLANKS
     C                   MOVEL(P)  'VCDE'        KKTBID
     C                   MOVEL(P)  SCVC06        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        627190
     C                   SETOFF                                       61
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCVC06 <> *BLANKS
     C*
     C*** VALUE CODE 2(C)
     C*
     C     SCVC07        IFNE      *BLANKS
     C                   MOVEL(P)  'VCDE'        KKTBID
     C                   MOVEL(P)  SCVC07        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        636471
     C                   SETON                                        90
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCVC07 <> *BLANKS
     C*
     C*** VALUE CODE 2(D)
     C*
     C     SCVC08        IFNE      *BLANKS
     C                   MOVEL(P)  'VCDE'        KKTBID
     C                   MOVEL(P)  SCVC08        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        637190
     C                   SETOFF                                       64
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCVC08 <> *BLANKS
     C*
     C*** VALUE CODE 3(A)
     C*
     C     SCVC09        IFNE      *BLANKS
     C                   MOVEL(P)  'VCDE'        KKTBID
     C                   MOVEL(P)  SCVC09        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        877190
     C                   SETOFF                                       65
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCVC09 <> *BLANKS
     C*
     C*** VALUE CODE 3(B)
     C*
     C     SCVC10        IFNE      *BLANKS
     C                   MOVEL(P)  'VCDE'        KKTBID
     C                   MOVEL(P)  SCVC10        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        678671
     C                   SETON                                        90
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCVC10 <> *BLANKS
     C*
     C*** VALUE CODE 3(C)
     C*
     C     SCVC11        IFNE      *BLANKS
     C                   MOVEL(P)  'VCDE'        KKTBID
     C                   MOVEL(P)  SCVC11        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        677190
     C                   SETOFF                                       86
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCVC11 <> *BLANKS
     C*
     C*** VALUE CODE 3(D)
     C*
     C     SCVC12        IFNE      *BLANKS
     C                   MOVEL(P)  'VCDE'        KKTBID
     C                   MOVEL(P)  SCVC12        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        718190
     C                   SETOFF                                       69
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCVC12 <> *BLANKS
     C*
     C     CK2END        ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C**ROOM*CHARGES ARE REQUIRED WHEN BILL TYPE IS 111 - 115
     C*
     C* PRINCIPAL DIAGNOSIS CODE IS REQUIRED
     C*
     C     SCDIA1        IFEQ      *BLANKS
     C                   SETON                                        9086
     C                   GOTO      CK3END
     C                   ENDIF                                                  SCDIA1 = *BLANKS
     C*
     C** VALIDATE PRINCIPAL DIAGNOSIS CODE IF ENTERED
     C*
     C                   MOVE      'D'           KKLOST
     C     SCDIA1        IFNE      *BLANKS
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxICD10 = '1' and scICdInd ='Y';
          kkICd10 = scDIa1;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If not %found(fmicd10l1)
                or ICstat = 'I';
                *in90 = *on;
                *in87 = *on;
                leavesr;
             Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   SCDIA1        KKICD9
     C                   MOVEL(P)  SCDIA1        KKICD9
     C*** End Chg *** 05/23/13 ****************************************
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        9087
     C                   GOTO      CK3END
     C                   ENDIF                                                  *IN99 = *ON
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   ENDIF                                                  SCDIA1 NE BLANKS
     C*
     C* VALIDATE THE OTHER DIAGNOSIS CODES
     C*
     C     SCDIA2        IFNE      *BLANKS
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxICD10 = '1' and scICdInd ='Y';
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
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   MOVEL     SCDIA2        KKICD9
     C                   MOVE      'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        9077
     C                   GOTO      CK3END
     C                   ENDIF                                                  *IN99 = *ON
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   ENDIF                                                  SCDIA1 NE *BLANKS
     C*
     C     SCDIA3        IFNE      *BLANKS
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxICD10 = '1' and scICdInd ='Y';
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
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   MOVEL     SCDIA3        KKICD9
     C                   MOVE      'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        9078
     C                   GOTO      CK3END
     C                   ENDIF                                                  *IN99 = *ON
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   ENDIF                                                  SCDIA1 NE *BLANKS
     C*
     C     SCDIA4        IFNE      *BLANKS
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxICD10 = '1' and scICdInd ='Y';
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
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   MOVEL     SCDIA4        KKICD9
     C                   MOVE      'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        9079
     C                   GOTO      CK3END
     C                   ENDIF                                                  *IN99 = *ON
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   ENDIF                                                  SCDIA1 NE *BLANKS
     C*
     C* PROCDURE CODE METHOD IS REQUIRED
     C*
     C     SCPCM         IFEQ      *BLANKS
     C                   SETON                                        9071
     C                   GOTO      CK3END
     C                   ENDIF                                                  SCPCM = *BLANKS
     C*
     C* VALIDATE PROCEDURE CODE METHOD TO BE 0 OR 9
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
     C* IF TYPE OF SERVICE=ISI,ISF,HSI,HSF,ASI,ASF THEN PCM CANNOT
     C* BE 0.
     C     SCICN         IFEQ      'IS'
     C*
     C     SCPCM         IFEQ      '0'
     C                   SETON                                        9085
     C                   GOTO      CK3END
     C                   ENDIF                                                  SCPCM IFEQ 0
     C*
     C                   ENDIF                                                  SCICN IFEQ IS
     C*
     C* MEDICARE NUMBER IS REQUIRED FOR TEXAS HOSPITAL BILL
     C* IF DATA SUB FLAG='Y'
     C*
      **** REQUIRED ONLY WHEN ADMISSION DATE > 03/01/2008
     C                   Z-ADD     SCADDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXADDT            7 0
     C     XXADDT        IFGE      1080301
     C     *IN09         IFNE      *ON
     C     *IN29         ANDNE     *ON
     C     SCMCAR        IFEQ      *BLANKS
     C     SCEDI         ANDEQ     'Y'
     C     SCMCAR        OREQ      '0'
     C     SCEDI         ANDEQ     'Y'
     C                   SETON                                        616364
     C                   SETON                                        499056
     C                   SETOFF                                       62
     C                   GOTO      CK3END
     C                   ELSE
     C     SCMCAR        IFNE      ' '
     C     SCEDI         ANDEQ     'Y'
     C     SCICN         IFEQ      'HO'
     C     SCICN         OREQ      'HS'
     C     SCICN         OREQ      'AO'
     C     SCICN         OREQ      'AS'
     C                   MOVEL     SCMCAR        KKPROV
     C     KKPROV        CHAIN     LOP01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        6456
     C                   SETON                                        4990
     C                   SETOFF                                       616263
     C                   GOTO      CK3END
     C                   ENDIF                                                  SCMCAR <> OIPROV
     C                   ENDIF                                                  SCICN = 'HO'
     C     SCICN         IFEQ      'IM'
     C     SCICN         OREQ      'IR'
     C     SCICN         OREQ      'IP'
     C     SCICN         OREQ      'IS'
     C                   MOVEL     SCMCAR        KKPROV
     C                   MOVE      20080301      KPEFDT
     C     IPKY          CHAIN     LII01                              99
     C     SCMCAR        IFNE      PFPROV
     C                   SETON                                        6456
     C                   SETON                                        4990
     C                   SETOFF                                       616263
     C                   GOTO      CK3END
     C                   ENDIF                                                  SCMCAR <> OIPROV
     C                   ENDIF                                                  SCICN = 'IM'
     C                   ENDIF                                                  SCMCAR <> ' '
     C                   ENDIF                                                  SCMCAR = *BLANKS
     C                   ENDIF                                                  *IN09 & *IN29 = *ON
     C                   ENDIF                                                  SCADDT > 1080301
     C*
     C*
     C     SCRLIC        IFEQ      *BLANKS
     C     SCEDI         ANDEQ     'Y'
     C                   SETON                                        906162
     C                   SETON                                        49
     C                   SETOFF                                       6364
     C                   GOTO      CK3END
     C                   ENDIF                                                  SCLIC# = *BLANKS
     C     SCEDI         IFEQ      'Y'
     C     SCEDI         OREQ      'N'
     C     SCRLIC        ANDNE     *BLANKS
     C                   MOVEL     SCRLIC        XX2A              2
     C                   MOVEL     SCRLIC        XX3A              3
     C*
     C     XX2A          IFNE      'AC'
     C     XX2A          ANDNE     'AU'
     C     XX2A          ANDNE     'CR'
     C     XX2A          ANDNE     'DC'
     C     XX2A          ANDNE     'DP'
     C     XX2A          ANDNE     'DO'
     C     XX2A          ANDNE     'IL'
     C     XX2A          ANDNE     'MT'
     C     XX2A          ANDNE     'MD'
     C     XX2A          ANDNE     'NP'
     C     XX2A          ANDNE     'OD'
     C     XX2A          ANDNE     'OT'
     C     XX2A          ANDNE     'PA'
     C     XX2A          ANDNE     'PT'
     C     XX2A          ANDNE     'LS'
     C     XX2A          ANDNE     'SP'
     C     XX2A          ANDNE     'AM'
     C     XX2A          ANDNE     'AS'
     C     XX2A          ANDNE     'CN'
     C     XX2A          ANDNE     'CP'
     C     XX2A          ANDNE     'CS'
     C     XX2A          ANDNE     'DM'
     C     XX2A          ANDNE     'DS'
     C     XX2A          ANDNE     'LP'
     C     XX2A          ANDNE     'MS'
     C     XX2A          ANDNE     'NF'
     C     XX2A          ANDNE     'OP'
     C     XX2A          ANDNE     'PM'
     C     XX2A          ANDNE     'PS'
     C     XX2A          ANDNE     'RA'
     C     XX2A          ANDNE     'RN'
     C     XX3A          ANDNE     'AMB'
     C     XX3A          ANDNE     'ASC'
     C     XX3A          ANDNE     'CNS'
     C     XX3A          ANDNE     'CSW'
     C     XX3A          ANDNE     'CPS'
     C     XX3A          ANDNE     'DME'
     C     XX3A          ANDNE     'LPC'
     C     XX3A          ANDNE     'LSA'
     C     XX3A          ANDNE     'NFA'
     C     XX3A          ANDNE     'PSY'
     C     XX3A          ANDNE     'RAD'
     C*
     C     SCEDI         IFEQ      'N'
     C     XX2A          ANDEQ     'OP'
     C                   ELSE
     C                   SETON                                          9049
     C                   SETOFF                                         64
     C                   SETOFF                                         6261
     C                   SETOFF                                         63
     C                   GOTO      CK3END
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     ' '           SCAN      SCRLIC        Y                 2 0    99
     C                   SUB       2             Y
     C*
     C     *IN99         IFEQ      *ON
     C     2             SUBST     SCRLIC:Y      XXST              2
     C*
     C                   ELSE
     C     2             SUBST     SCRLIC:12     XXST
     C*
     C                   ENDIF
     C*
     C                   MOVEL(P)  XXST          ##KEY
     C     STKY          CHAIN     PST02                              99
     C*
     C     *IN99         IFEQ      *ON
     C     Y             ORLE      2
     C                   SETOFF                                         4964
     C                   SETOFF                                         6263
     C                   SETOFF                                         61
     C                   SETON                                          9061
     C                   SETON                                          4963
     C                   GOTO      CK3END
     C                   ENDIF
     C*
     C                   ENDIF                                                  SCEDI EQ Y
     C*
     C* Rend/ref/admit last and first name are required
     C*
     C     SCRLNM        IFEQ      *BLANKS
     C*
     C                   SETON                                        2690
     C                   GOTO      CK3END
     C*
     C                   ENDIF                                                  SCRLNM EQ BLANK
     C*
     C     SCRFNM        IFEQ      *BLANKS
     C*
     C                   SETON                                        2590
     C                   GOTO      CK3END
     C*
     C                   ENDIF                                                  SCRFNM EQ BLANK
     C*
     C** ELIGIBLE FOR DATA SUBMISSION FIELD IS REQUIRED IF IN30 IS ON
     C*          (DETERMINED IN PGMSTR SUBROUINTE)
     C*
     C   30SCEDI         IFEQ      *BLANKS
     C                   SETON                                        9080
     C                   GOTO      CK3END
     C                   ENDIF                                                  SCEDI IFEQ BLANKS
     C*
     C     SCEDI         IFNE      *BLANKS
     C     SCEDI         ANDNE     'Y'
     C     SCEDI         ANDNE     'N'
     C                   SETON                                        9081
     C                   GOTO      CK3END
     C                   ENDIF                                                  SCEDI IFNE BLANKS
     C*
     C* VALIDATE PROCEDURE CODES BASE ON PROCEDURE CODE METHODS ENTERED
     C*
     C     SCPCM         IFNE      '0'
     C     SCPRC1        ANDEQ     *BLANKS
     C                   SETON                                        8490
     C                   GOTO      CK3END
     C                   ENDIF
     C*
     C*
     C* VALIDATE PROCEDURE CODES BASE ON PROCEDURE CODE METHODS ENTERED
     C*
     C*
     C     SCPRC1        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '9'
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxICD10 = '1' and scICdInd ='Y';
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
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   MOVEL     SCPRC1        BLANK
     C     BLANK         IFNE      *BLANKS
     C                   MOVEL     SCPRC1        CODE4
     C                   MOVE(P)   CODE4         KKICD9
     C                   ENDIF
     C                   MOVEL     'O'           SCLOS1
     C                   MOVE      SCLOS1        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C*** Beg Add *** 04/16/15 ****************************************
     C                   MOVEL(P)  SCPRC1        KKICD9
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C*** End Add *** 04/16/15 ****************************************
     C                   SETON                                        9073
     C                   GOTO      CK3END
     C*** Beg Add *** 04/16/15 ****************************************
     C                   ENDIF
     C*** End Add *** 04/16/15 ****************************************
     C                   ENDIF                                                  *IN99 = *ON
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   ENDIF                                                  SCPRC1 NE *BLANKS
     C*
     C*
     C     SCPRC2        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '9'
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxICD10 = '1' and scICdInd ='Y';
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
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   MOVEL     SCPRC2        BLANK
     C     BLANK         IFNE      *BLANKS
     C                   MOVEL     SCPRC2        CODE4
     C                   MOVE(P)   CODE4         KKICD9
     C                   ENDIF
     C                   MOVEL     'O'           SCLOS2
     C                   MOVE      SCLOS2        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C*** Beg Add *** 04/16/15 ****************************************
     C                   MOVEL(P)  SCPRC2        KKICD9
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C*** End Add *** 04/16/15 ****************************************
     C                   SETON                                        9074
     C                   GOTO      CK3END
     C*** Beg Add *** 04/16/15 ****************************************
     C                   ENDIF
     C*** End Add *** 04/16/15 ****************************************
     C                   ENDIF                                                  *IN99 = *ON
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   ENDIF                                                  SCPRC2 NE *BLANKS
     C*
     C*
     C     SCPRC3        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '9'
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxICD10 = '1' and scICdInd ='Y';
          kkICd10 = scPRc3;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'O';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If not %found(fmicd10l1)
                or ICstat = 'I';
                *in90 = *on;
                *in75 = *on;
                leavesr;
             Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   MOVEL     SCPRC3        BLANK
     C     BLANK         IFNE      *BLANKS
     C                   MOVEL     SCPRC3        CODE4
     C                   MOVE(P)   CODE4         KKICD9
     C                   ENDIF
     C                   MOVEL     'O'           SCLOS3
     C                   MOVE      SCLOS3        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C*** Beg Add *** 04/16/15 ****************************************
     C                   MOVEL(P)  SCPRC3        KKICD9
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C*** End Add *** 04/16/15 ****************************************
     C                   SETON                                        9075
     C                   GOTO      CK3END
     C*** Beg Add *** 04/16/15 ****************************************
     C                   ENDIF
     C*** End Add *** 04/16/15 ****************************************
     C                   ENDIF                                                  *IN99 = *ON
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   ENDIF                                                  SCPRC3 NE *BLANKS
     C*
     C*
     C     SCPRC4        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '9'
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxICD10 = '1' and scICdInd ='Y';
          kkICd10 = scPRc3;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'O';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If not %found(fmicd10l1)
                or ICstat = 'I';
                *in90 = *on;
                *in76 = *on;
                leavesr;
             Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   MOVEL     SCPRC4        BLANK
     C     BLANK         IFNE      *BLANKS
     C                   MOVEL     SCPRC4        CODE4
     C                   MOVE(P)   CODE4         KKICD9
     C                   ENDIF
     C                   MOVEL     'O'           SCLOS4
     C                   MOVE      SCLOS4        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C*** Beg Add *** 04/16/15 ****************************************
     C                   MOVEL(P)  SCPRC4        KKICD9
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C*** End Add *** 04/16/15 ****************************************
     C                   SETON                                        9076
     C                   GOTO      CK3END
     C*** Beg Add *** 04/16/15 ****************************************
     C                   ENDIF
     C*** End Add *** 04/16/15 ****************************************
     C                   ENDIF                                                  *IN99 = *ON
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   ENDIF                                                  SCPRC4 NE *BLANKS
     C*
     C* VALIDATE THE OTHER DIAGNOSIS CODES
     C*
     C*
     C*
     C     *IN09         IFNE      *ON
     C     *IN29         ANDNE     *ON
     C     SCIMST        IFEQ      *BLANK
     C*
     C                   SETON                                        6349
     C                   SETON                                        90
     C                   SETOFF                                       616264
     C                   GOTO      CK3END
     C*
     C                   ELSE                                                   SCIMST = ' '
     C*
     C*
     C                   MOVEL(P)  SCIMST        KKKEY
     C                   MOVEL(P)  'IMST'        KKTBID
     C     GTKY          CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *ON
     C                   SETON                                        626349
     C                   SETON                                        90
     C                   SETOFF                                       6164
     C                   GOTO      CK3END
     C                   ENDIF                                                  *IN99 = *ON
     C*
     C                   ENDIF                                                  SCIMST = ' '
     C                   ENDIF                                                  *IN29 & *IN09 = *ON
     C*
     C*
     C* VALIDATE DRG#
     C*
     C     *IN09         IFNE      *ON
     C     *IN29         ANDNE     *ON
     C     SCICN         ANDEQ     'IM'
     C     SCICN         OREQ      'IP'
     C     SCICN         OREQ      'IR'
     C     SCICN         OREQ      'IS'
     C     SCDRG#        IFEQ      0
      **** REQUIRED ONLY WHEN ADMISSION DATE > 03/01/2008
     C                   Z-ADD     SCADDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXADDT
     C     XXADDT        IFGE      1080301
     C                   SETON                                        90
     C                   SETON                                        6162
     C                   SETON                                        6449
     C                   SETOFF                                       63
     C                   GOTO      CK3END
     C                   ENDIF
     C*
     C                   ELSE
     C     SCDRG#        CHAIN     PDG02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        90
     C                   SETON                                        6162
     C                   SETON                                        64
     C                   SETOFF                                       6349
     C                   GOTO      CK3END
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C*  VALIDATE TRANSFER STATUS
     C*
     C     *IN09         IFNE      *ON
     C     *IN29         ANDNE     *ON
     C     SCICN         ANDEQ     'IM'
     C     SCICN         OREQ      'IP'
     C     SCICN         OREQ      'IR'
     C     SCICN         OREQ      'IS'
     C     SCTRST        IFEQ      *BLANK
     C*
     C                   SETON                                        90
     C                   SETON                                        6263
     C                   SETON                                        64
     C                   SETOFF                                       6149
     C                   GOTO      CK3END
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  SCTRST        KKKEY
     C                   MOVEL(P)  'TRST'        KKTBID
     C     GTKY          CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   SETON                                        90
     C                   SETON                                        6263
     C                   SETON                                        64
     C                   SETOFF                                       6149
     C*
     C                   ENDIF                                                  *IN99 = *ON
     C*
     C                   ENDIF
     C                   ENDIF                                                  *IN29 & *IN09 = *ON
     C*
     C*
     C     @@MODE        IFNE      'D'
     C*
     C     SCICN         IFEQ      'IR'
     C     SCICN         OREQ      'IP'
     C     SCICN         OREQ      'IM'
     C     SCICN         OREQ      'IS'
     C                   SETON                                        18
     C                   ELSE
     C                   SETOFF                                       18
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C     SCICN         IFEQ      'IR'
     C     SCICN         OREQ      'IP'
     C     SCICN         OREQ      'IM'
     C     SCICN         OREQ      'IS'
     C                   SETON                                        18
     C                   ELSE
     C                   SETOFF                                       18
     C                   ENDIF
     C*
     C*
     C                   ENDIF                                                  NO ERROR *IN90
     C     CK3END        ENDSR
     C*================================================================
     C* ERRCK4 - Error checking for srceen 4 (SCR04)
     C*================================================================
     C     ERRCK4        BEGSR
     C*
     C     SCICN         IFEQ      'IR'
     C     SCICN         OREQ      'IP'
     C     SCICN         OREQ      'IM'
     C     SCICN         OREQ      'IS'
     C*
     C     SCADDI        IFEQ      *BLANKS
     C                   SETON                                        9072
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      'D'           KKLOST
     C*
     C     SCADDI        IFNE      *BLANKS
     C*** Beg Chg *** 05/23/13 **************************************
     C********           MOVE(P)   SCADDI        KKICD9
     C                   MOVEL(P)  SCADDI        KKICD9
     C*** Beg Chg *** 05/23/13 **************************************
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxICD10 = '1' and scICdInd ='Y';
          kkICd10 = scADdi;
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
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        9071
     C                   ENDIF
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   ENDIF
     C*
     C*
     C     SCPREA        IFNE      *BLANKS
     C*** Beg Chg *** 01/21/15 ****************************************
     C***************    MOVE(P)   SCPREA        KKICD9
     C                   MOVEL(P)  SCPREA        KKICD9
     C*** End Chg *** 01/21/15 ****************************************
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxICD10 = '1' and scICdInd ='Y';
          kkICd10 = scPRea;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If not %found(fmicd10l1)
                or ICstat = 'I';
                *in90 = *on;
                *in73 = *on;
                leavesr;
             Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        9073
     C                   ENDIF
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   ENDIF
     C*
     C     SCATLC        IFNE      *BLANKS
     C                   MOVEL     SCATLC        XX2A              2
     C                   MOVEL     SCATLC        XX3A              3
     C*
     C     XX2A          IFNE      'AC'
     C     XX2A          ANDNE     'AU'
     C     XX2A          ANDNE     'CR'
     C     XX2A          ANDNE     'DC'
     C     XX2A          ANDNE     'DP'
     C     XX2A          ANDNE     'DO'
     C     XX2A          ANDNE     'IL'
     C     XX2A          ANDNE     'MT'
     C     XX2A          ANDNE     'MD'
     C     XX2A          ANDNE     'NP'
     C     XX2A          ANDNE     'OD'
     C     XX2A          ANDNE     'OT'
     C     XX2A          ANDNE     'PA'
     C     XX2A          ANDNE     'PT'
     C     XX2A          ANDNE     'LS'
     C     XX2A          ANDNE     'SP'
     C     XX2A          ANDNE     'AM'
     C     XX2A          ANDNE     'AS'
     C     XX2A          ANDNE     'CN'
     C     XX2A          ANDNE     'CP'
     C     XX2A          ANDNE     'CS'
     C     XX2A          ANDNE     'DM'
     C     XX2A          ANDNE     'DS'
     C     XX2A          ANDNE     'LP'
     C     XX2A          ANDNE     'MS'
     C     XX2A          ANDNE     'NF'
     C     XX2A          ANDNE     'OP'
     C     XX2A          ANDNE     'PM'
     C     XX2A          ANDNE     'PS'
     C     XX2A          ANDNE     'RA'
     C     XX2A          ANDNE     'RN'
     C     XX3A          ANDNE     'AMB'
     C     XX3A          ANDNE     'ASC'
     C     XX3A          ANDNE     'CNS'
     C     XX3A          ANDNE     'CSW'
     C     XX3A          ANDNE     'CPS'
     C     XX3A          ANDNE     'DME'
     C     XX3A          ANDNE     'LPC'
     C     XX3A          ANDNE     'LSA'
     C     XX3A          ANDNE     'NFA'
     C     XX3A          ANDNE     'PSY'
     C     XX3A          ANDNE     'RAD'
     C*
     C     SCEDI         IFEQ      'N'
     C     XX2A          ANDEQ     'OP'
     C                   ELSE
     C                   SETON                                          9078
     C                   GOTO      CK4END
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     ' '           SCAN      SCATLC        Y                 2 0    99
     C                   SUB       2             Y
     C*
     C     *IN99         IFEQ      *ON
     C     2             SUBST     SCATLC:Y      XXST              2
     C*
     C                   ELSE
     C     2             SUBST     SCATLC:12     XXST
     C*
     C                   ENDIF
     C*
     C                   MOVEL(P)  XXST          ##KEY
     C     STKY          CHAIN     PST02                              99
     C*
     C     *IN99         IFEQ      *ON
     C     Y             ORLE      2
     C                   SETON                                          9079
     C                   GOTO      CK4END
     C                   ENDIF
     C*
     C                   ENDIF                                                  SCATLC <>  ' '
     C*
     C     SCOLIC        IFNE      *BLANKS
     C                   MOVEL     SCOLIC        XX2A              2
     C                   MOVEL     SCOLIC        XX3A              3
     C*
     C     XX2A          IFNE      'AC'
     C     XX2A          ANDNE     'AU'
     C     XX2A          ANDNE     'CR'
     C     XX2A          ANDNE     'DC'
     C     XX2A          ANDNE     'DP'
     C     XX2A          ANDNE     'DO'
     C     XX2A          ANDNE     'IL'
     C     XX2A          ANDNE     'MT'
     C     XX2A          ANDNE     'MD'
     C     XX2A          ANDNE     'NP'
     C     XX2A          ANDNE     'OD'
     C     XX2A          ANDNE     'OT'
     C     XX2A          ANDNE     'PA'
     C     XX2A          ANDNE     'PT'
     C     XX2A          ANDNE     'LS'
     C     XX2A          ANDNE     'SP'
     C     XX2A          ANDNE     'AM'
     C     XX2A          ANDNE     'AS'
     C     XX2A          ANDNE     'CN'
     C     XX2A          ANDNE     'CP'
     C     XX2A          ANDNE     'CS'
     C     XX2A          ANDNE     'DM'
     C     XX2A          ANDNE     'DS'
     C     XX2A          ANDNE     'LP'
     C     XX2A          ANDNE     'MS'
     C     XX2A          ANDNE     'NF'
     C     XX2A          ANDNE     'OP'
     C     XX2A          ANDNE     'PM'
     C     XX2A          ANDNE     'PS'
     C     XX2A          ANDNE     'RA'
     C     XX2A          ANDNE     'RN'
     C     XX3A          ANDNE     'AMB'
     C     XX3A          ANDNE     'ASC'
     C     XX3A          ANDNE     'CNS'
     C     XX3A          ANDNE     'CSW'
     C     XX3A          ANDNE     'CPS'
     C     XX3A          ANDNE     'DME'
     C     XX3A          ANDNE     'LPC'
     C     XX3A          ANDNE     'LSA'
     C     XX3A          ANDNE     'NFA'
     C     XX3A          ANDNE     'PSY'
     C     XX3A          ANDNE     'RAD'
     C*
     C     SCEDI         IFEQ      'N'
     C     XX2A          ANDEQ     'OP'
     C                   ELSE
     C                   SETON                                          9076
     C                   GOTO      CK4END
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     ' '           SCAN      SCOLIC        Y                 2 0    99
     C                   SUB       2             Y
     C*
     C     *IN99         IFEQ      *ON
     C     2             SUBST     SCOLIC:Y      XXST              2
     C*
     C                   ELSE
     C     2             SUBST     SCOLIC:12     XXST
     C*
     C                   ENDIF
     C*
     C                   MOVEL(P)  XXST          ##KEY
     C     STKY          CHAIN     PST02                              99
     C*
     C     *IN99         IFEQ      *ON
     C     Y             ORLE      2
     C                   SETON                                          9077
     C                   GOTO      CK4END
     C                   ENDIF
     C*
     C                   ENDIF                                                  SCEDI EQ Y
     C*
     C***** Beg Add *** 06/16/15 **************************************
      /Free
        // Validate Attending Physician NPI#
        ErANPI=*Off;
        Mode    = 'N';
        Error   = ' ';
        XXANPI  =%Trim(SCANPI);
        ErANPI = validchar(XXANPI:Mode:Error);
        If ErANPI = *On;
          *IN90 = *On;
        EndIf;

        // Validate Other 1 NPI#
        ErNPI1=*Off;
        Error   = ' ';
        XXNPI1  =%Trim(SCNPI1);
        ErNPI1 =validchar(XXNPI1:Mode:Error);
        If ErNPI1=*ON;
          *IN90 = *On;
          //*** Beg add ***** 07/18/25 *************************************
           mssnpi = 'MC50007';
          //*** End add ***** 07/18/25 *************************************
        EndIf;

        //*** Beg add ***** 07/18/25 ************************************
        If isModEnabled('NCCI Med Data Call');
           // %check return non zero if non numeric value including blank
           invalidPos = %check('0123456789': %trim(scnpi1));
           if invalidPos <> 0 or scnpi1 = *blanks;
              mssnpi = 'MC50007';
              ernpi1 = *on;
              *in90 = *on;
              leavesr;
           endif;
        Endif;
        //*** End add ***** 07/18/25 ************************************
        // Validate Other 2 NPI#
        ErNPI2=*Off;
        Error   = ' ';
        XXNPI2  =%Trim(SCNPI2);
        ErNPI2 =validchar(XXNPI2:Mode:Error);
        If ErNPI2=*ON;
          *IN90 = *On;
         EndIf;

      /End-Free
     C***** End Add *** 06/16/15 **************************************
     C     CK4END        ENDSR
     C*================================================================
     C* ERRCK5 - Error checking for srceen 4 (SCR04)
     C*================================================================
     C     ERRCK5        BEGSR
     C     CK5END        ENDSR
     C*================================================================
     C* KJDO - F10=SEARCH.
     C*================================================================
     C     KJDO          BEGSR
     C*
     C*** INITIALIZE POSITION CURSOR INDICATORS
     C*
     C                   SETOFF                                       939495
     C                   SETOFF                                       27
     C*
     C*** CLEAR HOLD FIELDS
     C*
     C                   CLEAR                   HLDIA
     C                   CLEAR                   HLADDI
     C                   CLEAR                   HLPREA
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
     C                   SETON                                        9037      pos cursor
     C                   ENDIF
     C*** Beg Add *** 07/18/25 ***************************************
     C     HFLD          WHENEQ    'SCFZIP'
     C                   EVAL       *in90  =*on
     C                   EVAL       erfzip =*on
     C                   EVAL       msfzip ='MCR0676'
     C     HFLD          WHENEQ    'SCNPI1'
     C                   EVAL       *in90  =*on
     C                   EVAL       ernpi1 = *on
     C                   EVAL       mssnpi = 'MCR0676'
     C*** End Add *** 07/18/25 ***************************************
     C*
     C*
     C** PRINCIPAL DIAGNOSIS CODE SEARCH
     C*
     C     HFLD          WHENEQ    'SCDIA1'
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   SCDIA1        HLDIA             5
     C                   MOVEL(P)  SCDIA1        HLDIA             7
     C*** End Chg *** 05/23/13 ****************************************
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxICD10 = '1' and scICdInd ='Y';
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
             *in36 = *on;
          Else;
             scDIa1 = outICD10;
             *in90 = *on;
             *in43 = *on;
          Endif;

          If scDia1 <> *blanks;
             kkICD10 = scDIa1;
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
     C                   CALL      'F2010R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##ICD9
     C                   PARM      'D'           ##LOST
     C*
     C     ##KL          IFEQ      '1'
     C     ##ICD9        OREQ      *BLANKS
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   HLDIA         SCDIA1
     C                   MOVEL(P)  HLDIA         SCDIA1
     C*** End Chg *** 05/23/13 ****************************************
     C                   SETON                                        9036      pos cursor
     C                   ELSE
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   ##ICD9        SCDIA1
     C                   MOVEL(P)  ##ICD9        SCDIA1
     C*** End Chg *** 05/23/13 ****************************************
     C                   SETON                                        9043      pos cursor
     C                   ENDIF
     C*
     C     SCDIA1        IFNE      *BLANKS
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   SCDIA1        KKICD9
     C                   MOVEL(P)  SCDIA1        KKICD9
     C*** End Chg *** 05/23/13 ****************************************
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
     C                   MOVEL(P)  'TXTA'        ##TBID
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
     C                   SETON                                        9045
     C*
     C                   ELSE
     C                   SETON                                        9028
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C                   SETON                                        9028
     C*
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C* Type of Admission Source
     C*
     C     HFLD          WHENEQ    'SCASRC'
     C                   Z-ADD     0             ##RRN
     C                   MOVE(P)   SCASRC        HLASRC
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM      'ADSR'        ##TBID
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVE      HLASRC        SCASRC
     C                   SETON                                        9045
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTKEY         SCASRC
     C                   SETON                                        9027
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C* Discharge Status
     C*
     C     HFLD          WHENEQ    'SCDCST'
     C                   Z-ADD     0             ##RRN
     C                   MOVE      SCDCST        HLDCST
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM      'DCST'        ##TBID
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVE      HLDCST        SCDCST
     C                   SETON                                        9056
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCDCST
     C                   MOVEL     GTDESC        SCSTDS
     C                   SETON                                        9032
     C                   ENDIF                                                  *IN99 = *OFF
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
     C                   SETON                                        9091
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C*
     C* Search on condition codes 1-11
     C*
     C     HFLD          WHENEQ    'SCCOC1'
     C                   Z-ADD     0             ##RRN
     C                   MOVE      SCCOC1        HLOCA
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM      'CCDE'        ##TBID
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVE      HLOCA         SCCOC1
     C                   SETON                                        9091
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCCOC1
     C                   SETON                                        9031
     C                   SETOFF                                       71
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C*** CONDITION CODE 2
     C*
     C     HFLD          WHENEQ    'SCCOC2'
     C                   Z-ADD     0             ##RRN
     C                   MOVE      SCCOC2        HLOCA
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM      'CCDE'        ##TBID
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVE      HLOCA         SCCOC2
     C                   SETON                                        9031
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCCOC2
     C                   SETON                                        9032
     C                   SETOFF                                       71
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C*** CONDITION CODE 3
     C*
     C     HFLD          WHENEQ    'SCCOC3'
     C                   Z-ADD     0             ##RRN
     C                   MOVE      SCCOC3        HLOCA
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM      'CCDE'        ##TBID
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVE      HLOCA         SCCOC3
     C                   SETON                                        9032
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCCOC3
     C                   SETON                                        9033
     C                   SETOFF                                       71
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C*** CONDITION CODE 4
     C*
     C     HFLD          WHENEQ    'SCCOC4'
     C                   Z-ADD     0             ##RRN
     C                   MOVE      SCCOC4        HLOCA
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM      'CCDE'        ##TBID
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVE      HLOCA         SCCOC4
     C                   SETON                                        9033
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCCOC4
     C                   SETON                                        9034
     C                   SETOFF                                       71
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C*** CONDITION CODE 5
     C*
     C     HFLD          WHENEQ    'SCCOC5'
     C                   Z-ADD     0             ##RRN
     C                   MOVE      SCCOC5        HLOCA
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM      'CCDE'        ##TBID
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVE      HLOCA         SCCOC5
     C                   SETON                                        9034
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCCOC5
     C                   SETON                                        9035
     C                   SETOFF                                       71
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C*** CONDITION CODE 2
     C*
     C     HFLD          WHENEQ    'SCCOC6'
     C                   Z-ADD     0             ##RRN
     C                   MOVE      SCCOC6        HLOCA
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM      'CCDE'        ##TBID
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVE      HLOCA         SCCOC6
     C                   SETON                                        9035
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCCOC6
     C                   SETON                                        9036
     C                   SETOFF                                       71
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C*** CONDITION CODE 7
     C*
     C     HFLD          WHENEQ    'SCCOC7'
     C                   Z-ADD     0             ##RRN
     C                   MOVE      SCCOC7        HLOCA
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM      'CCDE'        ##TBID
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVE      HLOCA         SCCOC7
     C                   SETON                                        9036
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCCOC7
     C                   SETON                                        9095
     C                   SETOFF                                       71
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C*** CONDITION CODE 8
     C*
     C     HFLD          WHENEQ    'SCCOC8'
     C                   Z-ADD     0             ##RRN
     C                   MOVE      SCCOC8        HLOCA
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM      'CCDE'        ##TBID
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVE      HLOCA         SCCOC8
     C                   SETON                                        9095
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCCOC8
     C                   SETON                                        9038
     C                   SETOFF                                       71
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C*** CONDITION CODE 9
     C*
     C     HFLD          WHENEQ    'SCCOC9'
     C                   Z-ADD     0             ##RRN
     C                   MOVE      SCCOC9        HLOCA
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM      'CCDE'        ##TBID
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVE      HLOCA         SCCOC9
     C                   SETON                                        9038
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCCOC9
     C                   SETON                                        9039
     C                   SETOFF                                       71
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C*** CONDITION CODE 10
     C*
     C     HFLD          WHENEQ    'SCCO10'
     C                   Z-ADD     0             ##RRN
     C                   MOVE      SCCO10        HLOCA
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM      'CCDE'        ##TBID
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVE      HLOCA         SCCO10
     C                   SETON                                        9039
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCCO10
     C                   SETON                                        9040
     C                   SETOFF                                       71
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C*** CONDITION CODE 11
     C*
     C     HFLD          WHENEQ    'SCCO11'
     C                   Z-ADD     0             ##RRN
     C                   MOVE      SCCO11        HLOCA
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM      'CCDE'        ##TBID
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVE      HLOCA         SCCO11
     C                   SETON                                        9040
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCCO11
     C                   SETON                                        9041
     C                   SETOFF                                       71
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C** Search on Occurance code 1- 4
     C*
     C*** Occurance code 1
     C*
     C     HFLD          WHENEQ    'SCOCD1'
     C                   Z-ADD     0             ##RRN
     C                   MOVE      SCOCD1        HLOCD
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM      'OCDE'        ##TBID
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVE      HLOCD         SCOCD1
     C                   SETON                                        9041
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCOCD1
     C                   SETON                                        9042
     C                   SETOFF                                       71
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C*** Occurance code 2
     C*
     C     HFLD          WHENEQ    'SCOCD2'
     C                   Z-ADD     0             ##RRN
     C                   MOVE      SCOCD2        HLOCD
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM      'OCDE'        ##TBID
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVE      HLOCD         SCOCD2
     C                   SETON                                        9043
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCOCD2
     C                   SETON                                        9044
     C                   SETOFF                                       71
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C*** Occurance code 3
     C*
     C     HFLD          WHENEQ    'SCOCD3'
     C                   Z-ADD     0             ##RRN
     C                   MOVE      SCOCD3        HLOCD
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM      'OCDE'        ##TBID
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVE      HLOCD         SCOCD3
     C                   SETON                                        9045
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCOCD3
     C                   SETON                                        9046
     C                   SETOFF                                       71
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C*** Occurance code 4
     C*
     C     HFLD          WHENEQ    'SCOCD4'
     C                   Z-ADD     0             ##RRN
     C                   MOVE      SCOCD4        HLOCD
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM      'OCDE'        ##TBID
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVE      HLOCD         SCOCD4
     C                   SETON                                        9094
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCOCD4
     C                   SETON                                        9048
     C                   SETOFF                                       71
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C*** Occurance span code 1
     C*
     C     HFLD          WHENEQ    'SCOSD'
     C                   Z-ADD     0             ##RRN
     C                   MOVE      SCOSD         HLOCD
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM      'OSCD'        ##TBID
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVE      HLOSD         SCOSD
     C                   SETON                                        9049
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCOSD
     C                   SETON                                        9050
     C                   SETOFF                                       71
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C*** Occurance span code 2
     C*
     C     HFLD          WHENEQ    'SCOSD2'
     C                   Z-ADD     0             ##RRN
     C                   MOVE      SCOSD2        HLOCD
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM      'OSCD'        ##TBID
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVE      HLOSD         SCOSD2
     C                   SETON                                        9052
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCOSD2
     C                   SETON                                        9053
     C                   SETOFF                                       71
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C*
     C*** VALUE CODE 1(A)
     C*
     C     HFLD          WHENEQ    'SCVC01'
     C                   Z-ADD     0             ##RRN
     C                   MOVE      SCVC01        HLVCD
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM      'VCDE'        ##TBID
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVE      HLVCD         SCVC01
     C                   SETON                                        905556
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCVC01
     C                   SETON                                        9056
     C                   SETOFF                                       5571
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C*** VALUE CODE 1(B)
     C*
     C     HFLD          WHENEQ    'SCVC02'
     C                   Z-ADD     0             ##RRN
     C                   MOVE      SCVC02        HLVCD
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM      'VCDE'        ##TBID
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVE      HLVCD         SCVC02
     C                   SETON                                        9055
     C                   SETOFF                                       56
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCVC02
     C                   SETON                                        905758
     C                   SETOFF                                       71
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C*** VALUE CODE 1(C)
     C*
     C     HFLD          WHENEQ    'SCVC03'
     C                   Z-ADD     0             ##RRN
     C                   MOVE      SCVC03        HLVCD
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM      'VCDE'        ##TBID
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVE      HLVCD         SCVC03
     C                   SETON                                        9058
     C                   SETON                                        57
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCVC03
     C                   SETON                                        9057
     C                   SETOFF                                       5871
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C*** VALUE CODE 1(D)
     C*
     C     HFLD          WHENEQ    'SCVC04'
     C                   Z-ADD     0             ##RRN
     C                   MOVE      SCVC04        HLVCD
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM      'VCDE'        ##TBID
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVE      HLVCD         SCVC04
     C                   SETON                                        905985
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCVC04
     C                   SETON                                        9085
     C                   SETOFF                                       5971
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C*** VALUE CODE 2(A)
     C*
     C     HFLD          WHENEQ    'SCVC05'
     C                   Z-ADD     0             ##RRN
     C                   MOVE      SCVC05        HLVCD
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM      'VCDE'        ##TBID
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVE      HLVCD         SCVC05
     C                   SETON                                        9059
     C                   SETOFF                                       85
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCVC05
     C                   SETON                                        906162
     C                   SETOFF                                       71
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C*** VALUE CODE 2(B)
     C*
     C     HFLD          WHENEQ    'SCVC06'
     C                   Z-ADD     0             ##RRN
     C                   MOVE      SCVC06        HLVCD
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM      'VCDE'        ##TBID
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVE      HLVCD         SCVC06
     C                   SETON                                        9062
     C                   SETOFF                                       61
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCVC06
     C                   SETON                                        9061
     C                   SETOFF                                       6271
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C*** VALUE CODE 2(C)
     C*
     C     HFLD          WHENEQ    'SCVC07'
     C                   Z-ADD     0             ##RRN
     C                   MOVE      SCVC07        HLVCD
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM      'VCDE'        ##TBID
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVE      HLVCD         SCVC07
     C                   SETON                                        906364
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCVC07
     C                   SETON                                        9064
     C                   SETOFF                                       6371
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C*** VALUE CODE 2(D)
     C*
     C     HFLD          WHENEQ    'SCVC08'
     C                   Z-ADD     0             ##RRN
     C                   MOVE      SCVC08        HLVCD
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM      'VCDE'        ##TBID
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVE      HLVCD         SCVC08
     C                   SETON                                        9063
     C                   SETOFF                                       64
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCVC08
     C                   SETON                                        906587
     C                   SETOFF                                       71
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C*** VALUE CODE 3(A)
     C*
     C     HFLD          WHENEQ    'SCVC09'
     C                   Z-ADD     0             ##RRN
     C                   MOVE      SCVC09        HLVCD
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM      'VCDE'        ##TBID
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVE      HLVCD         SCVC09
     C                   SETON                                        9087
     C                   SETOFF                                       65
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCVC09
     C                   SETON                                        9065
     C                   SETOFF                                       8771
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C*** VALUE CODE 3(B)
     C*
     C     HFLD          WHENEQ    'SCVC10'
     C                   Z-ADD     0             ##RRN
     C                   MOVE      SCVC10        HLVCD
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM      'VCDE'        ##TBID
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVE      HLVCD         SCVC10
     C                   SETON                                        906786
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCVC10
     C                   SETON                                        9086
     C                   SETOFF                                       6771
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C*** VALUE CODE 3(C)
     C*
     C     HFLD          WHENEQ    'SCVC11'
     C                   Z-ADD     0             ##RRN
     C                   MOVE      SCVC11        HLVCD
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM      'VCDE'        ##TBID
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVE      HLVCD         SCVC11
     C                   SETON                                        9067
     C                   SETOFF                                       86
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCVC11
     C                   SETON                                        906981
     C                   SETOFF                                       71
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C*** VALUE CODE 3(D)
     C*
     C     HFLD          WHENEQ    'SCVC12'
     C                   Z-ADD     0             ##RRN
     C                   MOVE      SCVC12        HLVCD
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM      'VCDE'        ##TBID
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVE      HLVCD         SCVC12
     C                   SETON                                        9081
     C                   SETOFF                                       69
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCVC12
     C                   SETON                                        9069
     C                   SETOFF                                       8171
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C** F10 = SEARCH OPTION
     C*
     C     HFLD          WHENEQ    'SCADDI'
     C*** Beg Chg *** 05/23/13 **************************************
     C********           MOVE      SCADDI        HLADDI
     C                   MOVEL(P)  SCADDI        HLADDI
     C*** End Chg *** 05/23/13 **************************************
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxICD10 = '1' and scICdInd ='Y';
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scADdi = HLADDI;
             *in90 = *on;
             *in31 = *on;
          Else;
             scADdi = outICD10;
             *in90 = *on;
             *in32 = *on;
          Endif;

          If scADdi <> *blanks;
             kkICD10 = scADdi;
       //*** Beg Add *** 09/17/15 ****************************************
             kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   scA1ds = ICdsc1;
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
     C*** Beg Chg *** 05/23/13 **************************************
     C********           MOVE(P)   HLADDI        SCADDI
     C                   MOVEL(P)  HLADDI        SCADDI
     C*** End Chg *** 05/23/13 **************************************
     C                   SETON                                        9031      pos cursor
     C                   ELSE
     C*** Beg Chg *** 05/23/13 **************************************
     C********           MOVE(P)   ##ICD9        SCADDI
     C                   MOVEL(P)  ##ICD9        SCADDI
     C*** End Chg *** 05/23/13 **************************************
     C*
     C                   SETON                                        9032      pos cursor
     C                   ENDIF
     C*
     C     SCADDI        IFNE      *BLANKS
     C*** Beg Chg *** 05/23/13 **************************************
     C********           MOVE(P)   SCADDI        KKICD9
     C                   MOVEL(P)  SCADDI        KKICD9
     C*** End Chg *** 05/23/13 **************************************
     C                   MOVE      ##LOST        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCA1DS
     C                   ENDIF
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C*
     C** PATIENT REASON CODES
     C*
     C     HFLD          WHENEQ    'SCPREA'
     C                   MOVE      SCPREA        HLADDI
     C*** Beg Add *** 08/05/13 ****************************************
     C                   MOVEL(P)  SCPREA        HLADDI
      /Free
       If xxICD10 = '1' and scICdInd ='Y';
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scPRea = HLADDI;
             *in90 = *on;
             *in32 = *on;
          Else;
             scPRea = outICD10;
             *in90 = *on;
             *in33 = *on;
          Endif;

          If scPRea <> *blanks;
             kkICD10 = scPRea;
       //*** Beg Add *** 09/17/15 ****************************************
             kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   scP1ds = ICdsc1;
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
     C*** Beg Chg *** 02/16/14 ****************************************
     C****************   MOVE(P)   HLPREA        SCPREA
     C                   eval      scprea = hlprea
     C*** End Chg *** 02/16/14 ****************************************
     C                   SETON                                        9032      pos cursor
     C                   ELSE
     C*** Beg Chg *** 02/16/14 ****************************************
     C*****************  MOVE(P)   ##ICD9        SCPREA
     C                   eval      scprea = ##icd9
     C*** End Chg *** 02/16/14 ****************************************
     C*
     C                   SETON                                        9033      pos cursor
     C                   ENDIF
     C*
     C     SCPREA        IFNE      *BLANKS
     C*** Beg Chg *** 02/16/14 ****************************************
     C**************     MOVE(P)   SCPREA        KKICD9
     C                   eval      kkicd9 = scprea
     C*** End Chg *** 02/16/14 ****************************************
     C                   MOVE      ##LOST        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCP1DS
     C                   ENDIF
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
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
     C                   SETON                                        9039
     C                   ENDIF                                                  SCPCM = '0'
     C     SCPCM         IFEQ      '4'
     C     SCPCM         OREQ      '5'
     C                   SETON                                        7290
     C                   ENDIF
     C*
     C*
      *
      *  DID NOT MAKE A SELECTION
      *
     C*
     C*
     C     SCPCM         IFEQ      '9'
     C                   MOVEL     SCPRC1        HLPRC1            7
     C                   MOVEL     SCLOS1        HLLOS1            1
     C                   MOVEL     SCP1DS        HLP1DS           40
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxICD10 = '1' and scICdInd ='Y';
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
             kkICD10 = scPRc1;
       //*** Beg Add *** 09/17/15 ****************************************
             kkLost = 'O';
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
     C                   MOVEL     ##ICD9        BLANK             1
     C     BLANK         IFEQ      *BLANKS
     C                   MOVE      ##ICD9        CODE4             4
     C                   MOVEL(P)  CODE4         SCPRC1
     C                   ENDIF
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
     C                   SETON                                        9040
     C                   ENDIF                                                  SCPCM = '0'
     C     SCPCM         IFEQ      '4'
     C     SCPCM         OREQ      '5'
     C                   SETON                                        7290
     C                   ENDIF
     C*
     C*
     C*
     C     SCPCM         IFEQ      '9'
     C                   MOVEL     SCPRC2        HLPRC2            7
     C                   MOVE      SCLOS2        HLLOS2            1
     C                   MOVEL     SCP2DS        HLP2DS           40
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxICD10 = '1' and scICdInd ='Y';
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
             *in41 = *on;
          Endif;

          If scPRc2 <> *blanks;
             kkICD10 = scPRc2;
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
     C                   MOVEL     ##ICD9        BLANK             1
     C     BLANK         IFEQ      *BLANKS
     C                   MOVE      ##ICD9        CODE4             4
     C                   MOVEL(P)  CODE4         SCPRC2
     C                   ENDIF
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP2DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   SETON                                        9041
     C                   ENDIF                                                  ##ICD9 = *BLANKS
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   ENDIF                                                  SCPCM = '9'
     C*
     C     HFLD          WHENEQ    'SCPRC3'
     C     HFLD          OREQ      'SCLOS3'
     C*
     C* PROCEDURE CODE MUST BE ENTERED TO DO THIS SEARCH
     C*
     C     SCPCM         IFEQ      *BLANKS
     C                   SETON                                        9054
     C                   ENDIF
     C*
     C     SCPCM         IFEQ      '0'
     C                   SETON                                        9041
     C                   ENDIF                                                  SCPCM = '0'
     C     SCPCM         IFEQ      '4'
     C     SCPCM         OREQ      '5'
     C                   SETON                                        7290
     C                   ENDIF
     C*
     C*
     C     SCPCM         IFEQ      '9'
     C                   MOVEL     SCPRC3        HLPRC3            7
     C                   MOVE      SCLOS3        HLLOS3            1
     C                   MOVEL     SCP3DS        HLP3DS           40
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxICD10 = '1' and scICdInd ='Y';
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
             *in41 = *on;
          Else;
             scPRc3 = outICD10;
             *in90 = *on;
             *in42 = *on;
          Endif;

          If scPRc3 <> *blanks;
             kkICD10 = scPRc3;
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
     C                   MOVE      HLLOS3        SCLOS3
     C                   MOVEL(P)  HLP3DS        SCP3DS
     C                   SETON                                        9041
     C                   ELSE
     C                   MOVEL(P)  ##ICD9        SCPRC3
     C                   MOVE      ##LOST        SCLOS3
     C                   MOVEL(P)  SCPRC3        KKICD9
     C                   MOVE      SCLOS3        KKLOST
     C                   MOVEL     ##ICD9        BLANK             1
     C     BLANK         IFEQ      *BLANKS
     C                   MOVE      ##ICD9        CODE4             4
     C                   MOVEL(P)  CODE4         SCPRC3
     C                   ENDIF
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP3DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   SETON                                        9042
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
     C                   SETON                                        9055
     C                   ENDIF
     C*
     C     SCPCM         IFEQ      '0'
     C                   SETON                                        9042
     C                   ENDIF                                                  SCPCM = '0'
     C     SCPCM         IFEQ      '4'
     C     SCPCM         OREQ      '5'
     C                   SETON                                        7290
     C                   ENDIF
     C*
     C*
     C     SCPCM         IFEQ      '9'
     C                   MOVEL     SCPRC4        HLPRC4            7
     C                   MOVE      SCLOS4        HLLOS4            1
     C                   MOVEL     SCP4DS        HLP4DS           40
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxICD10 = '1' and scICdInd ='Y';
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
             *in42 = *on;
          Else;
             scPRc4 = outICD10;
             *in90 = *on;
             *in49 = *on;
             *in63 = *on;
             *in64 = *on;
             *in61 = *off;
             *in62 = *off;
          Endif;

          If scPRc4 <> *blanks;
             kkICD10 = scPRc4;
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
     C                   MOVE      HLLOS4        SCLOS4
     C                   MOVEL(P)  HLP4DS        SCP4DS
     C                   SETON                                        9042
     C                   ELSE
     C                   MOVEL(P)  ##ICD9        SCPRC4
     C                   MOVE      ##LOST        SCLOS4
     C                   MOVEL(P)  SCPRC4        KKICD9
     C                   MOVE      SCLOS4        KKLOST
     C                   MOVEL     ##ICD9        BLANK             1
     C     BLANK         IFEQ      *BLANKS
     C                   MOVE      ##ICD9        CODE4             4
     C                   MOVEL(P)  CODE4         SCPRC4
     C                   ENDIF
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP4DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   SETON                                        906364
     C                   SETON                                        49
     C                   SETOFF                                       6162
     C                   ENDIF                                                  ##ICD9 = *BLANKS
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   ENDIF                                                  SCPCM = '9'
     C*
     C* OTHER DIAGNOSIS CODES SEARCHES
     C*
     C     HFLD          WHENEQ    'SCDIA2'
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   SCDIA2        HLDIA             5
     C                   MOVEL(P)  SCDIA2        HLDIA             7
     C*** End Chg *** 05/23/13 ****************************************
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxICD10 = '1' and scICdInd ='Y';
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
             kkICD10 = scDIa2;
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
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   HLDIA         SCDIA2
     C                   MOVEL(P)  HLDIA         SCDIA2
     C*** End Chg *** 05/23/13 ****************************************
     C                   SETON                                        9043      pos cursor
     C                   ELSE
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   ##ICD9        SCDIA2
     C                   MOVEL(P)  ##ICD9        SCDIA2
     C*** End Chg *** 05/23/13 ****************************************
     C                   SETON                                        9044      pos cursor
     C                   ENDIF
     C*
     C     SCDIA2        IFNE      *BLANKS
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   SCDIA2        KKICD9
     C                   MOVEL(P)  SCDIA2        KKICD9
     C*** End Chg *** 05/23/13 ****************************************
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
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   SCDIA3        HLDIA             5
     C                   MOVEL(P)  SCDIA3        HLDIA             7
     C*** End Chg *** 05/23/13 ****************************************
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxICD10 = '1' and scICdInd ='Y';
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
             kkICD10 = scDIa3;
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
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   HLDIA         SCDIA3
     C                   MOVEL(P)  HLDIA         SCDIA3
     C*** End Chg *** 05/23/13 ****************************************
     C                   SETON                                        9044      pos cursor
     C                   ELSE
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   ##ICD9        SCDIA3
     C                   MOVEL(P)  ##ICD9        SCDIA3
     C*** End Chg *** 05/23/13 ****************************************
     C                   SETON                                        9045      pos cursor
     C                   ENDIF
     C*
     C     SCDIA3        IFNE      *BLANKS
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   SCDIA3        KKICD9
     C                   MOVEL(P)  SCDIA3        KKICD9
     C*** End Chg *** 05/23/13 ****************************************
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
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   SCDIA4        HLDIA             5
     C                   MOVEL(P)  SCDIA4        HLDIA             7
     C*** End Chg *** 05/23/13 ****************************************
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxICD10 = '1' and scICdInd ='Y';
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
             *in46 = *on;
          Endif;

          If scDia4 <> *blanks;
             kkICD10 = scDIa4;
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
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   HLDIA         SCDIA4
     C                   MOVEL(P)  HLDIA         SCDIA4
     C*** End Chg *** 05/23/13 ****************************************
     C                   SETON                                        9045      pos cursor
     C                   ELSE
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   ##ICD9        SCDIA4
     C                   MOVEL(P)  ##ICD9        SCDIA4
     C*** End Chg *** 05/23/13 ****************************************
     C                   SETON                                        9046      pos cursor
     C                   ENDIF
     C*
     C     SCDIA4        IFNE      *BLANKS
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   SCDIA4        KKICD9
     C                   MOVEL(P)  SCDIA4        KKICD9
     C*** End Chg *** 05/23/13 ****************************************
     C                   MOVE      ##LOST        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCD4DS
     C                   ENDIF
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C* MEDICARE PROVIDER NUMBER SEARCH
     C     HFLD          WHENEQ    'SCMCAR'
     C     SCICN         IFEQ      'HO'
     C     SCICN         OREQ      'HS'
     C     SCICN         OREQ      'AO'
     C     SCICN         OREQ      'AS'
     C                   MOVEL     SCMCAR        KKPROV
     C                   CALL      'F2357R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##RRN
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     POP01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     OPPROV        SCMCAR
     C                   SETON                                        906263
     C                   SETON                                        6449
     C                   SETOFF                                       61
     C                   ELSE
     C                   SETON                                        9056
     C                   ENDIF
     C*
     C                   ENDIF                                                  ##RRN <> 0
     C                   ELSE
     C     SCICN         IFEQ      'IM'
     C     SCICN         OREQ      'IR'
     C     SCICN         OREQ      'IP'
     C     SCICN         OREQ      'IS'
     C                   MOVEL     SCMCAR        KKPROV
     C                   CALL      'F2356R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##RRN
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PTP01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     PFPROV        SCMCAR
     C                   SETON                                        906263
     C                   SETON                                        6449
     C                   SETOFF                                       61
     C                   ELSE
     C                   SETON                                        9056
     C                   ENDIF
     C*
     C                   ENDIF                                                  ##RRN <> 0
     C                   ENDIF                                                  EUICN = 'IM'
     C                   ENDIF                                                  EUICN = 'HO'
     C     HFLD          WHENEQ    'SCRLIC'
     C                   SETON                                        906163
     C                   SETOFF                                       626449
     C*
     C* IMPLANT STATUS SEARCH
     C     HFLD          WHENEQ    'SCIMST'
     C                   Z-ADD     0             ##RRN
     C                   MOVEL     'IMST'        ##TBID
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
     C                   MOVEL     GTKEY         SCIMST
     C                   MOVEL(P)  GTDESC        SCIMDS
     C*                    SETON                     9046
     C                   SETON                                        90
     C                   SETON                                        6149
     C                   SETOFF                                       626364
     C                   ELSE
     C                   SETON                                        906364
     C                   SETON                                        49
     C                   SETOFF                                       6162
     C                   ENDIF
     C                   ELSE
     C                   SETON                                        906364
     C                   SETON                                        49
     C                   SETOFF                                       6162
     C*
     C                   ENDIF                                                  ##RRN <> 0
     C*
     C* TRANSFER STATUS SEARCH
     C     HFLD          WHENEQ    'SCTRST'
     C                   Z-ADD     0             ##RRN
     C                   MOVEL     'TRST'        ##TBID
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
     C                   SETON                                        90
     C                   MOVEL     GTKEY         SCTRST
     C                   MOVEL     GTDESC        SCTRDS
     C                   ENDIF
     C*
     C                   ENDIF                                                  ##RRN <> 0
     C*   SCDRG# SEARCH
     C     HFLD          WHENEQ    'SCDRG#'
     C*
     C                   CALL      'F2354R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      0             ##RRN
     C                   PARM      0             KKDRG#
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PDG01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     DGDRG#        SCDRG#
     C                   MOVEL     DGDESC        SCDGDS
     C                   SETON                                        90
     C                   SETON                                        6249
     C                   SETOFF                                       616364
     C                   ENDIF
     C                   ELSE
     C                   SETON                                        90
     C                   SETON                                        6149
     C                   SETOFF                                       626364
     C                   ENDIF
     C*
     C                   OTHER
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCACCT'
     C                   SETOFF                                       61
     C                   SETON                                        626364
     C     HFLD          WHENEQ    'SCSCPF'
     C                   SETOFF                                       626364
     C                   SETOFF                                       49
     C                   SETON                                        61
     C     HFLD          WHENEQ    'SCSCPT'
     C                   SETOFF                                       49
     C                   SETOFF                                       616364
     C                   SETON                                        62
     C     HFLD          WHENEQ    'SCADDT'
     C                   SETOFF                                       626164
     C                   SETON                                        63
     C     HFLD          WHENEQ    'SCADHR'
     C                   SETOFF                                       626361
     C                   SETON                                        64
     C     HFLD          WHENEQ    'SCDCDT'
     C                   SETOFF                                       6364
     C                   SETON                                        6162
     C     HFLD          WHENEQ    'SCDCHR'
     C                   SETOFF                                       6164
     C                   SETON                                        6263
     C     HFLD          WHENEQ    'SCSBDT'
     C                   SETOFF                                       6162
     C                   SETON                                        6364
     C     HFLD          WHENEQ    'SCMCAR'
     C                   SETOFF                                       64
     C                   SETON                                        616263
     C     HFLD          WHENEQ    'SCEDI'
     C                   SETOFF                                       62
     C                   SETON                                        616364
     C                   ENDSL
     C*
     C                   SETON                                        90
     C*
     C                   ENDSL
     C*
     C*
     C                   SETON                                        90
     C                   EXSR      DATSBR
     C                   EXSR      DTSBR2
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
     C*** INITIALIZE POSITION CURSOR INDICATORS
     C*
     C                   SETOFF                                       313233
     C                   SETOFF                                       343536
     C                   SETOFF                                       373839
     C                   SETOFF                                       404142
     C                   SETOFF                                       434445
     C                   SETOFF                                       465053
     C                   SETOFF                                       545556
     C                   SETOFF                                       575861
     C                   SETOFF                                       626364
     C                   SETOFF                                       2728
     C                   SETOFF                                       60
     C                   SETOFF                                       658767
     C                   SETOFF                                       868169
     C                   CLEAR                   SCCHRG
     C                   CLEAR                   SCMRA
     C                   CLEAR                   SCNTWK
     C                   CLEAR                   SCOTHR
     C                   CLEAR                   SCOTH2
     C                   CLEAR                   SCPAID
     C                   MOVE      *BLANKS       SCANPI
     C                   MOVE      *BLANKS       SCONPI
     C                   MOVE      *BLANKS       SCNPI1
     C                   MOVE      *BLANKS       SCNPI2
     C                   MOVE      *BLANKS       SCADDI
     C                   MOVE      *BLANKS       SCA1DS
     C                   MOVE      *BLANKS       SCPREA
     C                   MOVE      *BLANKS       SCP1DS
     C                   MOVE      *BLANKS       SCOFSN
     C                   MOVE      *BLANKS       SCOLSN
     C                   MOVE      *BLANKS       SCOLIC
     C     @@MODE        IFNE      'D'
     C                   SETON                                        37
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
     C                   ELSE
     C     PDKY          CHAIN     PPD01                              99
     C  N99              MOVEL     PDSPCD        SCSPCD
     C                   ENDIF
     C*
     C*** Beg Add *** 07/18/25 ***************************************
     C     EOFLIC        IFNE      *BLANKS
     C                   MOVEL     EOFLIC        SCFZIP
     C                   MOVE      EOFLIC        SCZIP
     C                   ELSE
     C     PDKY          CHAIN     PP101                              99
     C  N99              MOVEL     P1ZIP         SCFZIP
     C  N99              MOVE      P1ZIP         SCZIP
     C                   ENDIF
     C*** End Add *** 07/18/25 ***************************************
     C* If on SCR02 then get specialty code from EOSPCD
     C*
     C     XXSCR3        IFEQ      *ON
     C     @@MODE        OREQ      'D'
     C                   MOVEL     EOSPCD        SCSPCD
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
     C     ##CLT#        IFEQ      '144'
     C     EUSBDT        ANDEQ     *ZEROS
     C     ##CLT#        OREQ      '025'
     C     EUSBDT        ANDEQ     *ZEROS
     C                   Z-ADD     EOCSDT        EUSBDT
     C                   Z-ADD     EUSBDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCSBDT
     C                   ENDIF
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
     C     ##CLT#        IFEQ      '144'
     C     EUSBDT        ANDEQ     *ZEROS
     C     ##CLT#        OREQ      '025'
     C     EUSBDT        ANDEQ     *ZEROS
     C                   Z-ADD     EOCSDT        EUSBDT
     C                   ENDIF
     C                   Z-ADD     EUSBDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCSBDT
     C*
     C***** Beg Chg ***** 08/28/14 ******************************
     C*****              MOVE      EUADHR        SCADHR
     C*****              Z-ADD     EUDCHR        SCDCHR
     C                   MOVEL     EUADHR        SCADHR
     C                   MOVEL     EUDCHR        SCDCHR
     C***** End Chg ***** 08/28/14 ******************************
     C                   MOVE      EUADSC        SCASRC
     C                   MOVE      EUDCST        SCDCST
     C                   MOVEL     'DCST'        KKTBID
     C                   MOVEL(P)  SCDCST        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCSTDS
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   MOVEL     EUICN         SCICN
     C                   MOVEL     'SVTY'        KKTBID
     C                   MOVEL(P)  EUICN         KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCICDS
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
     C* Condition codes
     C                   MOVE      EUCOC1        SCCOC1
     C                   MOVE      EUCOC2        SCCOC2
     C                   MOVE      EUCOC3        SCCOC3
     C                   MOVE      EUCOC4        SCCOC4
     C                   MOVE      EUCOC5        SCCOC5
     C                   MOVE      EUCOC6        SCCOC6
     C                   MOVE      EUCOC7        SCCOC7
     C                   MOVE      EUCOC8        SCCOC8
     C                   MOVE      EUCOC9        SCCOC9
     C                   MOVE      EUCO10        SCCO10
     C                   MOVE      EUCO11        SCCO11
     C*
     C* Occurence codes
      *** Begin Change *** 02/08/18 ***************************
     C***                   MOVE      EUOCD1        SCOCD1
     C***                   MOVE      EUOCD2        SCOCD2
     C***                   MOVE      EUOCD3        SCOCD3
     C***                   MOVE      EUOCD4        SCOCD4
      /Free
       SCOCD1 = %Subst(EUOCD1:1:2);
       SCOCD2 = %Subst(EUOCD2:1:2);
       SCOCD3 = %Subst(EUOCD3:1:2);
       SCOCD4 = %Subst(EUOCD4:1:2);
      /End-Free
      *** End Change *** 02/08/18 ***************************
     C*
     C* Occurence code dates
     C                   Z-ADD     EUODT1        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCODT1
     C*
     C                   Z-ADD     EUODT2        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCODT2
     C*
     C                   Z-ADD     EUODT3        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCODT3
     C*
     C                   Z-ADD     EUODT4        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCODT4
     C*
     C*
     C* Occurence span codes
     C                   MOVE      EUOSCD        SCOSD
     C                   MOVE      EUOSC2        SCOSD2
     C*
     C* Occurence span from/to dates
     C                   Z-ADD     EUOSFD        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCOSFD
     C*
     C                   Z-ADD     EUOSTD        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCOSTD
     C*
     C                   Z-ADD     EUOFD2        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCOFD2
     C*
     C                   Z-ADD     EUOTD2        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCOTD2
     C*
     C* Value codes
     C                   MOVE      EUVC01        SCVC01
     C                   MOVE      EUVC02        SCVC02
     C                   MOVE      EUVC03        SCVC03
     C                   MOVE      EUVC04        SCVC04
     C                   MOVE      EUVC05        SCVC05
     C                   MOVE      EUVC06        SCVC06
     C                   MOVE      EUVC07        SCVC07
     C                   MOVE      EUVC08        SCVC08
     C                   MOVE      EUVC09        SCVC09
     C                   MOVE      EUVC10        SCVC10
     C                   MOVE      EUVC11        SCVC11
     C                   MOVE      EUVC12        SCVC12
     C*
     C* Value amounts
     C                   Z-ADD     EUVA01        SCVA01
     C                   Z-ADD     EUVA02        SCVA02
     C                   Z-ADD     EUVA03        SCVA03
     C                   Z-ADD     EUVA04        SCVA04
     C                   Z-ADD     EUVA05        SCVA05
     C                   Z-ADD     EUVA06        SCVA06
     C                   Z-ADD     EUVA07        SCVA07
     C                   Z-ADD     EUVA08        SCVA08
     C                   Z-ADD     EUVA09        SCVA09
     C                   Z-ADD     EUVA10        SCVA10
     C                   Z-ADD     EUVA11        SCVA11
     C                   Z-ADD     EUVA12        SCVA12
     C*
     C*
     C*
     C                   MOVEL     EUPCM         SCPCM
     C                   MOVE      'PCMT'        KKTBID
     C                   MOVEL(P)  SCPCM         KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCPDSC
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   MOVEL     EUMCAR        SCMCAR
     C*                    MOVELEURLIC    SCRLIC
     C                   MOVEL(P)  EURLIC        SCRLIC
     C                   MOVEL     EUDGDS        SCDGDS
     C                   Z-ADD     EUDRG#        SCDRG#
     C                   MOVEL     EUTRST        SCTRST
     C     EUDRG#        CHAIN     PDG02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     DGDESC        SCDGDS
     C                   ENDIF
     C                   MOVE      'TRST'        KKTBID
     C                   MOVEL(P)  EUTRST        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTDESC        SCTRDS
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   MOVEL     EUPRC1        SCPRC1
     C                   MOVEL     EUPRC1        BLANK
     C     BLANK         IFEQ      *BLANKS
     C                   MOVEL     EUPRC1        CODE5             5
     C                   MOVE      CODE5         CODE4
     C                   MOVEL(P)  CODE4         SCPRC1
     C                   ENDIF
     C                   MOVE      EUPRC1        SCLOS1
     C                   MOVE      EUIMST        SCIMST
     C                   MOVEL(P)  'IMST'        KKTBID
     C                   MOVEL(P)  SCIMST        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCIMDS
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C*** Beg Chg *** 08/05/13 ***************************************
     C*****SCPCM         IFEQ      '9'
     C*****SCPRC1        ANDNE     *BLANKS
      /Free
         If SCpcm = '9' and (Scprc1 <> ' ' or EUPRC1001 <> ' ' );
      /End-Free
     C*** End Chg *** 08/05/13 ***************************************
     C                   MOVEL     EUPRC1        KKICD9
     C                   MOVE      EUPRC1        KKLOST
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       If xxICD10 = '1' and EuicdInd ='Y';
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
     C                   MOVEL     EUPRC2        BLANK
     C     BLANK         IFEQ      *BLANKS
     C                   MOVEL     EUPRC2        CODE5
     C                   MOVE      CODE5         CODE4
     C                   MOVEL(P)  CODE4         SCPRC2
     C                   ENDIF
     C                   MOVE      EUPRC2        SCLOS2
     C*
     C*** Beg Chg *** 08/05/13 ***************************************
     C*****SCPCM         IFEQ      '9'
     C*****SCPRC2        ANDNE     *BLANKS
      /Free
         If SCpcm = '9' and (Scprc2 <> ' ' or EUPRC1002 <> ' ' );
      /End-Free
     C*** End Chg *** 08/05/13 ***************************************
     C                   MOVEL     EUPRC2        KKICD9
     C                   MOVE      EUPRC2        KKLOST
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
     C                   MOVEL     EUPRC3        SCPRC3
     C                   MOVEL     EUPRC3        BLANK
     C     BLANK         IFEQ      *BLANKS
     C                   MOVEL     EUPRC3        CODE5
     C                   MOVE      CODE5         CODE4
     C                   MOVEL(P)  CODE4         SCPRC3
     C                   ENDIF
     C                   MOVE      EUPRC3        SCLOS3
     C*
     C*** Beg Chg *** 08/05/13 ***************************************
     C*****SCPCM         IFEQ      '9'
     C*****SCPRC3        ANDNE     *BLANKS
      /Free
         If SCpcm = '9' and (Scprc3 <> ' ' or EUPRC1003 <> ' ' );
      /End-Free
     C*** End Chg *** 08/05/13 ***************************************
     C                   MOVEL     EUPRC3        KKICD9
     C                   MOVE      EUPRC3        KKLOST
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
     C                   MOVEL     EUPRC4        BLANK
     C     BLANK         IFEQ      *BLANKS
     C                   MOVEL     EUPRC4        CODE5
     C                   MOVE      CODE5         CODE4
     C                   MOVEL(P)  CODE4         SCPRC4
     C                   ENDIF
     C                   MOVE      EUPRC4        SCLOS4
     C*
     C*** Beg Chg *** 08/05/13 ***************************************
     C*****SCPCM         IFEQ      '9'
     C*****SCPRC4        ANDNE     *BLANKS
      /Free
         If SCpcm = '9' and (Scprc4 <> ' ' or EUPRC1004 <> ' ' );
      /End-Free
     C*** End Chg *** 08/05/13 ***************************************
     C                   MOVEL     EUPRC4        KKICD9
     C                   MOVE      EUPRC4        KKLOST
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
     C                   MOVEL     EUDIA2        SCDIA2
     C*** Beg Chg *** 08/05/13 ***************************************
     C*****SCDIA2        IFNE      *BLANKS
      /Free
         If SCDIA2 <> ' ' or EUICD1002 <> ' ';
      /End-Free
     C*** End Chg *** 08/05/13 ***************************************
     C                   MOVEL(P)  SCDIA2        KKICD9
     C                   MOVE      'D'           KKLOST
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
     C*****SCDIA3        IFNE      *BLANKS
      /Free
         If SCDIA3 <> ' ' or EUICD1003 <> ' ';
      /End-Free
     C*** End Chg *** 08/05/13 ***************************************
     C                   MOVEL(P)  SCDIA3        KKICD9
     C                   MOVE      'D'           KKLOST
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
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCD3DS
     C                   ENDIF                                                  *IN99 = *OFF
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C                   ENDIF                                                  SCDIA4 = *BLANKS
     C*
     C                   MOVEL     EUDIA4        SCDIA4
     C*** Beg Chg *** 08/05/13 ***************************************
     C*****SCDIA4        IFNE      *BLANKS
      /Free
         If SCDIA4 <> ' ' or EUICD1004 <> ' ';
      /End-Free
     C*** End Chg *** 08/05/13 ***************************************
     C                   MOVEL(P)  SCDIA4        KKICD9
     C                   MOVE      'D'           KKLOST
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       If xxICD10 = '1' and EuicdInd ='Y';
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
     C                   MOVE      EUEDI         SCEDI
     C*
     C*
     C                   Z-ADD     EOMRA         SCMRA
     C                   Z-ADD     EONDIS        SCNTWK
     C                   Z-ADD     EOOTHR        SCOTHR
     C                   Z-ADD     EOOTH2        SCOTH2
     C                   Z-ADD     EOPAID        SCPAID
     C                   MOVE      EUADTY        SCADTY
     C                   MOVE      EUANPI        SCANPI
     C                   MOVE      EUONPI        SCONPI
     C                   MOVE      EUNPI1        SCNPI1
     C                   MOVE      EUNPI2        SCNPI2
     C*
     C*** Beg Chg *** 05/23/13 **************************************
     C*******            MOVE      EUADDI        SCADDI
     C                   MOVEL(P)  EUADDI        SCADDI
     C*** End Chg *** 05/23/13 **************************************
     C                   MOVEL     SCADDI        KKICD9
     C                   MOVE      'D'           KKLOST
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       If xxICD10 = '1' and EUicdInd ='Y';
          KKICD10 = EUadia1001;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
               scA1ds = ICdsc1;
             Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCA1DS
     C                   ENDIF                                                  *IN99 = *OFF
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C*
     C*** Beg Chg *** 02/16/14 ****************************************
     C*******************MOVE      EUPREA        SCPREA
     C                   eval      scprea = euprea
     C*** End Chg *** 02/16/14 ****************************************
     C                   MOVEL     SCPREA        KKICD9
     C                   MOVE      'D'           KKLOST
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       If xxICD10 = '1' and EUicdInd ='Y';
          KKICD10 = EUPREA10;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
               scP1ds = ICdsc1;
             Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP1DS
     C                   ENDIF                                                  *IN99 = *OFF
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C*
     C                   MOVE      EUOFSN        SCOFSN
     C                   MOVE      EUOLSN        SCOLSN
     C                   MOVE      EUOLIC        SCOLIC
     C                   MOVE      EUADSC        SCASRC
     C                   MOVE      EUDCST        SCDCST
     C                   MOVE      EUATLC        SCATLC
     C*
     C                   ENDIF                                                  @@MODE NE 'A'
     C*
     C                   MOVEL(P)  EORLNM        SCRLNM
     C                   MOVEL(P)  EORFNM        SCRFNM
     C*** Beg Add *** 05/23/13 ****************************************
      /Free
          // Beg Chg *** 08/05/13 ****************************************
          //scICdind = 'N';
          //If xxicd10 = *on;
          If xxicd10 = *on and EUicdind = 'Y';
          // End Chg *** 08/05/13 ****************************************
             scDIa1  = EUicd1001;
             scDIa2  = EUicd1002;
             scDIa3  = EUicd1003;
             scDIa4  = EUicd1004;
             scICdind= EUicdind ;
             scADdi  = EUadia1001;
             // Beg Chg *** 08/05/13 ****************************************
             scPRea  = EUPREA10;
               If EUPCM = '9';
                 scPRc1  = EUprc1001;
                 scPRc2  = EUprc1002;
                 scPRc3  = EUprc1003;
                 scPRc4  = EUprc1004;
               Else;
                 scPRc1  = EUPRC1;
                 scPRc2  = EUPRC2;
                 scPRc3  = EUPRC3;
                 scPRc4  = EUPRC4;
               Endif;
          // End Chg *** 08/05/13 ****************************************
          // Beg Add *** 08/05/13 ****************************************
          Elseif EUicdind ='N';
             scICdind = EUicdind;
          // End Add *** 08/05/13 ****************************************
          Endif;
      /End-Free
     C*** End Add *** 05/23/13 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     EOCO#         EUCO#
     C                   Z-ADD     EOSUB#        EUSUB#
     C                   Z-ADD     EOFND         EUFND
     C                   Z-ADD     EOFYR         EUFYR
     C                   Z-ADD     EOCASE        EUCASE
     C                   MOVE      EOSS#         EUSS#
     C                   Z-ADD     EOACDT        EUACDT
     C*
     C*** Beg Add *** 07/18/25 ***************************************
     C     SCFZIP        IFEQ      *ZEROS
     C                   MOVEL     '     '       EOFLIC
     C     PDKY          CHAIN     PP101                              99
     C  N99              MOVEL     P1ZIP         SCFZIP
     C  N99              MOVE      P1ZIP         SCZIP
     C                   ENDIF
     C                   MOVEL     SCFZIP        EOFLIC
     C                   MOVE      SCZIP         EOFLIC
     C*** End Add *** 07/18/25 ***************************************
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
     C                   Z-ADD     SCSBDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EUSBDT
     C*
     C***** Beg Chg ***** 08/28/14 ******************************
     C*****              MOVE      SCADHR        EUADHR
     C*****              Z-ADD     SCDCHR        EUDCHR
     C                   MOVEL     SCADHR        EUADHR
     C                   MOVEL     SCDCHR        EUDCHR
     C***** End Chg ***** 08/28/14 ******************************
     C                   MOVE      SCASRC        EUADSC
     C                   MOVE      SCDCST        EUDCST
     C                   MOVEL     SCICN         EUICN
     C                   MOVEL     SCDCN         EUDCN
     C*
     C* Condition codes
     C                   MOVE      SCCOC1        EUCOC1
     C                   MOVE      SCCOC2        EUCOC2
     C                   MOVE      SCCOC3        EUCOC3
     C                   MOVE      SCCOC4        EUCOC4
     C                   MOVE      SCCOC5        EUCOC5
     C                   MOVE      SCCOC6        EUCOC6
     C                   MOVE      SCCOC7        EUCOC7
     C                   MOVE      SCCOC8        EUCOC8
     C                   MOVE      SCCOC9        EUCOC9
     C                   MOVE      SCCO10        EUCO10
     C                   MOVE      SCCO11        EUCO11
     C*
     C* Occurence codes
      ** Begin Change *** 02/08/18 **************************
     C***                MOVE      SCOCD1        EUOCD1
     C***                MOVE      SCOCD2        EUOCD2
     C***                MOVE      SCOCD3        EUOCD3
     C***                MOVE      SCOCD4        EUOCD4
      /Free
        EUOCD1 = SCOCD1;
        EUOCD2 = SCOCD2;
        EUOCD3 = SCOCD3;
        EUOCD4 = SCOCD4;
      /End-Free
      ** End   Change *** 02/08/18 **************************
     C*
     C* Occurence code dates
     C                   Z-ADD     SCODT1        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EUODT1
     C*
     C                   Z-ADD     SCODT2        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EUODT2
     C*
     C                   Z-ADD     SCODT3        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EUODT3
     C*
     C                   Z-ADD     SCODT4        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EUODT4
     C*
     C*
     C* Occurence span codes
     C                   MOVE      SCOSD         EUOSCD
     C                   MOVE      SCOSD2        EUOSC2
     C*
     C* Occurence span from/to dates
     C                   Z-ADD     SCOSFD        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EUOSFD
     C*
     C                   Z-ADD     SCOSTD        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EUOSTD
     C*
     C                   Z-ADD     SCOFD2        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EUOFD2
     C*
     C                   Z-ADD     SCOTD2        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EUOTD2
     C*
     C* Value codes
     C                   MOVE      SCVC01        EUVC01
     C                   MOVE      SCVC02        EUVC02
     C                   MOVE      SCVC03        EUVC03
     C                   MOVE      SCVC04        EUVC04
     C                   MOVE      SCVC05        EUVC05
     C                   MOVE      SCVC06        EUVC06
     C                   MOVE      SCVC07        EUVC07
     C                   MOVE      SCVC08        EUVC08
     C                   MOVE      SCVC09        EUVC09
     C                   MOVE      SCVC10        EUVC10
     C                   MOVE      SCVC11        EUVC11
     C                   MOVE      SCVC12        EUVC12
     C*
     C* Value amounts
     C                   Z-ADD     SCVA01        EUVA01
     C                   Z-ADD     SCVA02        EUVA02
     C                   Z-ADD     SCVA03        EUVA03
     C                   Z-ADD     SCVA04        EUVA04
     C                   Z-ADD     SCVA05        EUVA05
     C                   Z-ADD     SCVA06        EUVA06
     C                   Z-ADD     SCVA07        EUVA07
     C                   Z-ADD     SCVA08        EUVA08
     C                   Z-ADD     SCVA09        EUVA09
     C                   Z-ADD     SCVA10        EUVA10
     C                   Z-ADD     SCVA11        EUVA11
     C                   Z-ADD     SCVA12        EUVA12
     C*
     C                   MOVEL     SCPCM         EUPCM
     C     SCPCM         IFNE      '9'
     C                   MOVE      *BLANKS       SCLOS1
     C                   MOVE      *BLANKS       SCLOS2
     C                   MOVE      *BLANKS       SCLOS3
     C                   MOVE      *BLANKS       SCLOS4
     C                   ENDIF
     C                   MOVEL(P)  SCMCAR        EUMCAR
     C                   MOVEL(P)  SCDRG#        EUDRG#
     C     SCDRG#        CHAIN     PDG02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     DGDESC        EUDGDS
     C                   ENDIF
     C                   MOVEL(P)  SCTRST        EUTRST
     C                   MOVEL(P)  SCIMST        EUIMST
     C                   MOVE      SCEDI         EUEDI
     C                   MOVEL(P)  SCPRC1        EUPRC1
     C*
     C                   MOVEL     SCPRC1        BLANK
     C     BLANK         IFNE      *BLANKS
     C                   MOVEL     SCPRC1        CODE4
     C                   MOVE(P)   CODE4         CODE5
     C                   MOVEL(P)  CODE5         EUPRC1
     C                   ENDIF
     C*
     C     EUPRC1        IFNE      *BLANKS
     C                   MOVE      SCLOS1        EUPRC1
     C                   ENDIF
     C*
     C                   MOVEL(P)  SCPRC2        EUPRC2
     C*
     C                   MOVEL     SCPRC2        BLANK
     C     BLANK         IFNE      *BLANKS
     C                   MOVEL     SCPRC2        CODE4
     C                   MOVE(P)   CODE4         CODE5
     C                   MOVEL(P)  CODE5         EUPRC2
     C                   ENDIF
     C*
     C     EUPRC2        IFNE      *BLANKS
     C                   MOVE      SCLOS2        EUPRC2
     C                   ENDIF
     C*
     C                   MOVEL(P)  SCPRC3        EUPRC3
     C*
     C                   MOVEL     SCPRC3        BLANK
     C     BLANK         IFNE      *BLANKS
     C                   MOVEL     SCPRC3        CODE4
     C                   MOVE(P)   CODE4         CODE5
     C                   MOVEL(P)  CODE5         EUPRC3
     C                   ENDIF
     C*
     C     EUPRC3        IFNE      *BLANKS
     C                   MOVE      SCLOS3        EUPRC3
     C                   ENDIF
     C*
     C                   MOVEL(P)  SCPRC4        EUPRC4
     C*
     C                   MOVEL     SCPRC4        BLANK
     C     BLANK         IFNE      *BLANKS
     C                   MOVEL     SCPRC4        CODE4
     C                   MOVE(P)   CODE4         CODE5
     C                   MOVEL(P)  CODE5         EUPRC4
     C                   ENDIF
     C*
     C     EUPRC4        IFNE      *BLANKS
     C                   MOVE      SCLOS4        EUPRC4
     C                   ENDIF
     C                   MOVEL(P)  SCDIA2        EUDIA2
     C                   MOVEL(P)  SCDIA3        EUDIA3
     C                   MOVEL(P)  SCDIA4        EUDIA4
     C                   MOVE      SCSPCD        EOSPCD
     C                   MOVE      SCADTY        EUADTY
     C                   MOVEL     SCRLIC        EUFLIC
     C                   MOVEL     SCRLIC        EURLIC
     C*
     C                   MOVE      SCANPI        EUANPI
     C                   MOVE      SCONPI        EUONPI
     C                   MOVE      SCNPI1        EUNPI1
     C                   MOVE      SCNPI2        EUNPI2
     C*
     C*** Beg Chg *** 05/23/13 **************************************
     C********           MOVE      SCADDI        EUADDI
     C                   MOVEL(P)  SCADDI        EUADDI
     C*** End Chg *** 05/23/13 **************************************
     C*** Beg Chg *** 02/16/14 ****************************************
     C*****************  MOVE      SCPREA        EUPREA
     C                   eval      euprea = scprea
     C*** End Chg *** 02/16/14 ****************************************
     C                   MOVE      SCOFSN        EUOFSN
     C                   MOVE      SCOLSN        EUOLSN
     C                   MOVE      SCOLIC        EUOLIC
     C*
     C                   MOVE      SCATLC        EUATLC
     C*** Beg Add *** 05/23/13 ****************************************
      /Free
          //Beg Chg *** 08/05/13 ****************************************
          EUicdind  = scICdind;
          //Beg Chg *** 02/06/14 ****************************************
          //***If xxICD10 = '1';
          If xxICD10 = '1' and scICdInd ='Y';
          //End Chg *** 02/06/14 ****************************************
          //If xxICD10 = '1' and scICdInd ='Y';
          //End Chg *** 08/05/13 ****************************************
             EUicd1001 = scDIa1;
             EUicd1002 = scDIa2;
             EUicd1003 = scDIa3;
             EUicd1004 = scDIa4;
             EUadia1001= scADdi;
             EUPrea10  = scPRea;

             // If ICD10 is Active then no need of ICD9 fields
             EUdia1 = *Blanks;
             EUdia2 = *Blanks;
             EUdia3 = *Blanks;
             EUdia4 = *Blanks;
             EUprc1 = *Blanks;
             EUprc2 = *Blanks;
             EUprc3 = *Blanks;
             EUprc4 = *Blanks;
             EUaddi = *Blanks;
             //*** Beg Add *** 08/05/13 ******************************
             EUPrea = *blanks;
             //*** End Add *** 08/05/13 ******************************
             //*** Beg Chg *** 08/05/13 ******************************
               If EUPCM = '9';
                 EUprc1001 = scPRc1;
                 EUprc1002 = scPRc2;
                 EUprc1003 = scPRc3;
                 EUprc1004 = scPRc4;
                 EUprc1 = *Blanks;
                 EUprc2 = *Blanks;
                 EUprc3 = *Blanks;
                 EUprc4 = *Blanks;
               Else;
                 EUprc1001 = *Blanks;
                 EUprc1002 = *Blanks;
                 EUprc1003 = *Blanks;
                 EUprc1004 = *Blanks;
                 EUprc1 = scPRc1;
                 EUprc2 = scPRc2;
                 EUprc3 = scPRc3;
                 EUprc4 = scPRc4;
               EndIf;
          Else;
             // If ICD9 is Active then no need of ICD10 fields
             EUicd1001 = *Blanks;
             EUicd1002 = *Blanks;
             EUicd1003 = *Blanks;
             EUicd1004 = *Blanks;
             EUadia1001= *Blanks;
             EUPrea10  = *Blanks;
             EUprc1001 = *Blanks;
             EUprc1002 = *Blanks;
             EUprc1003 = *Blanks;
             EUprc1004 = *Blanks;
             //*** End Chg *** 08/05/13 ******************************
          Endif;
      /End-Free
     C*** End Add *** 05/23/13 ****************************************
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
     C* SET OF INDICATORS FOR ERROR CHECKING. THESE ARE NOT PART OF
     C* COPYBOOK ERROR INDICATOR SET.
     C*
     C                   SETOFF                                       515248
     C                   SETOFF                                       575859
     C                   SETOFF                                       50
     C                   SETOFF                                       252653
     C                   SETOFF                                       7165
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
     C                   SETOFF                                       575861
     C                   SETOFF                                       626364
       //*** Beg add ***** 07/18/25 ************************************
       erfzip = *off;
       msfzip = *blanks;
       ernpi1 = *off;
       mssnpi = *blanks;
       //*** End add ***** 07/18/25 ************************************
     C*
     C     *INKJ         CASEQ     *ON           KJDO                           F10=search
     C     *INKK         CASEQ     *ON           KKDO                           F11=EOB/Notes
     C                   ENDCS
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
     C     EUKY          CHAIN     PEU01                              99
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
     C                   SETON                                        LR
     C                   MOVE      '1'           OUTFLG
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C** Get standard screen headings
     C*
     C     @@RRN2        CHAIN(N)  PEO01                              99
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       ##CLT#            3
     C*
     C*  SET EDITST TO POLICY OR ADJ STATE BASED ON CPST FLAG
     C*
     C     EOCPST        IFEQ      'Y'
     C                   MOVEL(P)  EOPOST        EDITST
     C                   ELSE
     C                   MOVEL(P)  EOAST         EDITST
     C                   ENDIF
     C*
     C                   Z-ADD     EOEOB#        SCEOB#
     C     EDITST        CAT(P)    'BT':0        BTTBID            4
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
     C  N98              MOVE      BEEDI         SCEDI
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
     C                   MOVE      *OFF          XXSCR3            1
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
     C                   MOVE      SCEDI         EOTAPE
     C                   MOVEL     SCRLIC        EORLIC
     C                   MOVEL(P)  SCRLNM        EORLNM
     C                   MOVEL(P)  SCRFNM        EORFNM
     C                   MOVEL     Q1USER        EOUSER
     C                   MOVEL     Q1PGM         EOPGM
     C                   Z-ADD     DSTIME        EOTIME
     C                   MOVEL     SRDTO         EODT
     C*** Beg Add *** 07/18/25 ***************************************
     C                   MOVEL     SCFZIP        EOFLIC
     C                   MOVE      SCZIP         EOFLIC
     C*** End Add *** 07/18/25 ***************************************
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
     C                   MOVE      SCEDI         EOTAPE
     C                   MOVEL     SCRLIC        EORLIC
     C                   MOVEL(P)  SCRLNM        EORLNM
     C                   MOVEL(P)  SCRFNM        EORFNM
     C                   MOVEL     Q1USER        EOUSER
     C                   MOVEL     Q1PGM         EOPGM
     C                   Z-ADD     DSTIME        EOTIME
     C                   MOVEL     SRDTO         EODT
     C*** Beg Add *** 07/18/25 ***************************************
     C                   MOVEL     SCFZIP        EOFLIC
     C                   MOVE      SCZIP         EOFLIC
     C*** End Add *** 07/18/25 ***************************************
     C                   UPDATE    PEO01
     C                   ENDIF
     C*
     C                   FEOD      FAEUP
     C*
     C                   MOVE      '1'           OUTFLG
     C                   ENDSR
