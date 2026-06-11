     Hcopyright('(c) 2025 Sapiens, Inc. All rights reserved.')
     H option(*nodebugio:*srcstmt)
     H**** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  Program: F0361R                                              *
     H*  Description: Hospital (UB) Header Information                *
     H*  Created: 11/30/98  C:56 Project: MC152                       *
     H*****************************************************************
     H*                                                               *
     H*     # # # # # IMPORTANT PROGRAMMERS NOTE # # # # #            *
     H*                                                               *
     H* CHANGES TO THIS PROGRAM SHOULD ALSO BE MADE TO STATE SPECIFIC *
     H* PROGRAMS AS WELL (F0361XX) WHERE XX IS THE STATE IF NECESSARY *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS                                                    *
     H*                                                               *
     H*  07/18/25  C:342 Project MC451                                *
     H*                  NCCI Med Data Call Enhancements              *
     H*                                                               *
     H*  04/04/16 C:212 PROJECT: OK022                                *
     H*                 Modify edit condition                         *
     H*                                                               *
     H*  10/06/15  C:062 Project FS475                                *
     H*                  Require Medicare Provider ID if discharge    *
     H*                  date is greater than 8/14/2015               *
     H*                                                               *
     H*  10/01/15  C:079 PHW37605                                     *
     H*                  Check ICD-10 Indicator based on date and     *
     H*                  only in add mod                              *
     H*                                                               *
     H*  09/17/15 C:079 Support: 099/37167                            *
     H*                 Add type to ICD-10 Key list for editing       *
     H*                                                               *
     H*  03/13/15 C:296 PROJECT: DE012                                *
     H*                 Add edits for state DE.                       *
     H*                                                               *
     H*  08/05/13 C:277 PROJECT: MC329 Phase II                       *
     H*                 Add search functionality and Validate ICD10   *
     H*                                                               *
     H*  05/23/13 C:277 PROJECT: MC329                                *
     H*                 Activate the ICD10 fields if ICD10 is active  *
     H*                                                               *
     H*  04/17/13 C:284 Project: 114MID                               *
     H*                 Include rendering information for CSI.        *
     H*                                                               *
     H*  04/15/13 C:284 Project: SY316
     H*                 Correct activation groups for Provider Bills pgms
     H*
     H*  11/15/12 C:287 Project: MC367
     H*                 Disable function code F11 for GS access to Bill Entry
     H*
     H*  06/21/12  C:277  Project:114MFI
     H*                   If Medicare number is not in the IPPS PSF;
     H*                   allow entry when state is 'HL'
     H*
     H*  04/20/12 C:251 Support: 79739 MC146
     H*                 Edit for zip to be done only in case of non-recons
     H*
     H*  02/21/12 C:277 PROJECT IL004
     H*                 Add processing for state 'IL'
     H*                 Retrieve region from file FTHZPIL if speciality <>'ASC'
     H*                 else retrieve region from file FTNZPIL.
     H*
     H*  10/17/11  C:251  PROJECT 114MAR
     H*                   1. F10 =Search, Edit on IRF provider file
     H*                   2. Changed Edit on SCDRG, not required for IRF bills
     H*                   3. New field 'Patient Status' validity and F10 checks
     H*
     H*  10/12/11  C:280  PROJECT 114MBE FOR CLIENT 114
     H*                   DEFAULT SCTRST = 99
     H*                   DEFAULT SCIMST = N
     H*
     H*  10/06/11  C:251  PROJECT 114MZO
     H*                   F10 =Search, Edit on SNF provider file for
     H*                   SNF bill type
     H*
     H*  09/20/11  C:251  PROJECT 114MYS PRISS 50
     H*                   Update Drg code desc on detail line based on
     H*                   header drg code
     H*
     H*  08/03/11 C:273 Project: 146MEK                               *
     H*                 Present SCR02 with Rendering Provider data    *
     H*                                                               *
     H*  06/22/11 C:198 Project:114MYS
     H*                         Medicare provider ID edit will be basedr
     H*                         on the discount code type.
     H*
     H*  05/03/11 C:079 Project:114MWT
     H*                         Medicare provider ID edit will be basedr
     H*                         on the discount code type.
     H*
     H*  04/08/11 C:251 Project:114MWT
     H*                         Remove edit to require medicare provider
     H*                         when Bill type = 83x for all states,
     H*                         Remove edit for 'NJ' and non-PPO .
     H*                         Added two fields city,state and processing
     H*
     H*  04/05/11 C:257 Support:71931
     H*                         Assigning RRN to @@RRN after WRITEDBRCDF to
     H*                         resolve duplicate records in file FAEHP.
     H*
     H*  03/16/11 C:251 Project:FS382  Priss 17
     H*                         Added edit to require medicare provider
     H*                         when PPO is installed.
     H*
     H*  03/01/11 C:251 Project:SC018  Priss 20
     H*                         Removed edit for medicare provider when
     H*                         bill type = 83x and state = 'SC'
     H*
     H*  11/30/10 C:198 Project:114MWS Priss 32
     H*                         Updated DRG desc in ERRCK1 when DRG code
     H*                         clears the edit.
     H*
     H*  11/12/10 C:251 Project: SC018
     H*                         Added indicator 90 to stop on error for
     H*                         field transfer status.
     H*
     H*  10/01/10 C:198 Project:WV009
     H*                         Added extra edits for medicare fee
     H*                         schedule states.
     H*
     H*  08/12/10 C:198 Project:114MWS
     H*                         Added functionality for Medicare PPO
     H*                         processing.
     H*
     H*  08/11/10 C:227 PROJECT:IL001
     H*                         Require Type of Bill, Type of Service,
     H*                         and Discharge Date for IL
     H*                         Add & require Facility Zipcode for IL
     H*                         Require DRG code and Date statement covers -
     H*                         from/to for IL for Inaptient bills
     H*
     H*  08/31/09 C:079 PROJECT:CT005
     H*                         Require Type of Bill for CT
     H*
     H*  07/22/09 C:227 PROJECT:AR038
     H*                         Code added for AR State
     H*
     H*  07/21/09 C:198 SUPPORT:69178
     H*                         Do not require DRG Code for NJ
     H*
     H*  06/11/09 C:079 PROJECT:DE005
     H*                         Do not require DRG Code for DE
     H*
     H*  07/02/09 C:079 PROJECT:MD066
     H*                         Do not require provider id for MD bills
     H*
     H*  04/03/09 C:198 PROJECT:MD066
     H*                         DRG Update
     H*
     H*  11/28/08 C:191 PROJECT:MS017
     H*                         DRG Update
     H*
     H*  08/21/08 C:180 PROJECT:MC294 PRISS 22
     H*                         Do not underline NPI# fields when
     H*                         in display mode. - DSPF changes only.
     H*
     H*  07/14/08 C:180 PROJECT:MC294 Phase II
     H*                         Add SCR02.
     H*
     H*  06/16/08 C:198 PROJECT:DE002
     H*                 Added DRG code field on scr01.
     H*
     H*  10/17/07 C:062 PROJECT:MS014
     H*                 ADDITIONAL EDITS.                              ST
     H*
     H*  04/17/07 C:188 PROJECT:FL061
     H*                 SET EDITST TO ADJ OR POLICY STATE BASED ON EOCPST
     H*
     H*  07/03/05 C:079 PROJECT:TN002
     H*                 REQUIRE TYPE OF BILL/SERVICE FOR STATE = TN
     H*
     H*  04/26/05 C:079 PROJECT:MI001
     H*                 REQUIRE TYPE OF BILL/SERVICE FOR STATE = MI
     H*
     H*  07/24/04 C:079 PROJECT:MA012
     H*                 REQUIRE TYPE OF BILL/SERVICE FOR STATE = MA
     H*
     H*  04/07/04 C:079 PROJECT: AL051
     H*                 REQUIRE TYPE OF SERVICE FOR AL
     H*
     H*  03/12/03 C:079 SUPPORT: MC044 36319
     H*                 CHECK THE STATUS OF DIAG CODES
     H*
     H*  07/06/01 C:079 PROJECT: FSMNT #196
     H*                 ADD EDIT TO SPECIALTY CODE
     H*
     H*  05/30/01 C:062 PROJECT: FS229
     H*                 POPULATE EOMB# SCREEN FIELD
     H*
     H*  01/02/01  C:062  Proj: MCMNT  BASE ONLY
     H*                   WHEN ENTERING SCR01 POSITION TO PAT ACCT #
     H*
     H*  11/22/00  C:062  Proj: FS212
     H*                   COMPILE ONLY.
     H*
     H*  11/08/00  C:62   PROJECT:FS146
     H*                   CHANGE 'BLTY' TO 'ZZBT' AND 'FLBT'.
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
     H*  05/28/00  C:117  PROJ: MCMNT
     H*                   CORRECTED UNDERLINES IN DISPLAY MODE
     H*                   (COMPILE ONLY)
     H*
     H*  04/11/00  C:079  PROJ: FS211
     H*                   Allow future dates for Statement covers from
     H*                   and to based on FBAL04 pos 8
     H*
     H*  03/08/00  C:062  PROJ: MCMNT
     H*                   PASS "D" LOS TYPE TO F2010R SO THAT F2010R
     H*                   WILL ONLY SUBSET ON "D" LOS TYPE CODES.
     H*
     H*  02/21/00  C:117  PROJ: FS208
     H*                   ADDED LOGIC TO PROTECT SPECIALTY IF ON A
     H*                   RECON; ALSO SAVES SPECIALTY.
     H*
     H*  02/16/00 C:53  Project: KY010
     H*                 ADD KY REQUIRED FIELD.
     H*
     H*  11/12/99 C:79  MC152
     H*                 Delete/Change efficiency isssue.
     H*                                                               *
     H*  09/10/99 C:56  Project: KY010                                *
     H*                 Write SCEDI to EOTAPE                         *
     H*                                                               *
     H*****************************************************************
     H*
     FF0361F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
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
     F*
     F*  DRG Code File
     FFTDGP     IF   E             DISK
     FFTDGL     IF   E           K DISK
     F                                     RENAME(PDG01:PDG02)
     FFTIIP     IF   E             DISK
     FFTIIL     IF   E           K DISK
     F                                     RENAME(PII01:PII02)
     F*
     F*  Fee Schedule Base Control File
     F*SSYLF   IF  E           K        DISK
     F*           SSY01                             KIGNORE
     F*           SPC01                             KIGNORE
     F*           SWC01                             KIGNORE
     F*           SRC01                             KIGNORE
     F*           SDR01                             KIGNORE
     F*           SBC01                             KIGNORE
     F*           SCS01                             KIGNORE
     F*           SGL01                             KIGNORE
     F*           SP201                             KIGNORE
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
     FFTOPP     IF   E             DISK
     FFTOPL     IF   E           K DISK
     F                                     RENAME(POP01:LOP01)
     FFTPFP     IF   E             DISK
     FFTPFL     IF   E           K DISK
     F                                     RENAME(PTP01:LTP01)
     FFTSAL3    IF   E           K DISK
     FFTSAL4    IF   E           K DISK
     F                                     RENAME(PSA01:PSA02)
     FFTSAP     IF   E             DISK
     F                                     RENAME(PSA01:PSA03)
     FFDDUL3    UF   E           K DISK
     FFMSKP     IF   E             DISK
     FFMSKL1    IF   E           K DISK
     F                                     RENAME(PSK01:PSK02)
     FFMFNL     IF   E           K DISK
     FFMIRP     IF   E             DISK
     FFMIRL1    IF   E           K DISK
     F                                     RENAME(PIR01:PIR02)
      *
      *  IL Zip Code to Region Hospital File
     FFTHZL1IL  IF   E           K DISK
     FFTHZPIL   IF   E             DISK
     F                                     RENAME(PHZ01:PHZ02)
      *
      *  IL Zip Code to Region Non Hospital File
     FFTNZL1IL  IF   E           K DISK
     FFTNZPIL   IF   E             DISK
     F                                     RENAME(PNZ01:PNZ02)
     F*** Beg Add *** 08/05/13 ***************************************
     Ffmicd10l1 if   e           k disk
     F*** End Add *** 08/05/13 ***************************************
     D LO              C                   CONST('abcdefghijklmnopqrst-
     D                                     uvwxyz')
     D ALPHA           C                   CONST('ABCDEFGHIJKLMNOPQRST-
     D                                     UVWXYZ')
     D*
      /COPY SCOPYMT01D
     D*
     D DBFDSA          DS
     D  DBRRNA               397    400B 0
     D*
     D                 DS
     D  SCBLTY                 1      3
     D  XXBLTY                 1      2
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Hospital (UB) Header-
     D                                      Information')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(2)
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
     D*** Begin Add *** 10/06/15 ***************************************
     D ConvertDate     pr                  extpgm('SRCVTDAT')
     D srDtI                          8  0
     D srFFmt                         8
     D srTFmt                         8
     D srDtO                         10
     D srErr                          1
     D*** End Add *** 08/05/13 ***************************************
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
     C     IIKY          KLIST
     C                   KFLD                    KKPROV
      *
     C     DGKY          KLIST
     C                   KFLD                    KKDRG#
     C* KEY LIST FOR FTSAP
     C     SAKY          KLIST
     C                   KFLD                    KKRCTY           25
     C                   KFLD                    KKRST             2
     C                   KFLD                    KKEFDT            7 0
     C*
     C     SAKY1         KLIST
     C                   KFLD                    KKRCTY
     C                   KFLD                    KKRST
     C*
     C     DUKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRVCD            3 0
     C                   KFLD                    KKRATE            5
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C     HZKY          KLIST
     C                   KFLD                    KKZIP
     C*
     C     NZKY          KLIST
     C                   KFLD                    KKZIP
     C*
     C*** Beg Add *** 08/05/13 ***************************************
     C*** KEY LIST FOR DIAGNOSIS CODE MASTER FILE
     C     fmicd10l1key  klist
     C                   kfld                    kkicd10
     C*** Beg Add *** 09/17/15 ***************************************
     C                   kfld                    kkLost
     C*** End Add *** 09/17/15 ***************************************
     C*** End Add *** 08/05/13 ***************************************
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
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C     *LIKE         DEFINE    EUSCPF        HLSCPF
     C     *LIKE         DEFINE    EUSCPT        HLSCPT
     C     *LIKE         DEFINE    EUADDT        HLADDT
     C     *LIKE         DEFINE    EUADDT        HLSYDT
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C     *LIKE         DEFINE    EUSBDT        HLSBDT
     C     *LIKE         DEFINE    EUICN         HLICN
     C     *LIKE         DEFINE    EUDCN         HLDCN
     C     *LIKE         DEFINE    EUDCDT        HLDCDT
     C     *LIKE         DEFINE    EOAST         EDITST
     C     *LIKE         DEFINE    DGDRG#        KKDRG#
     C     *LIKE         DEFINE    DGDRG#        HLDRG
     C     *LIKE         DEFINE    IIPROV        HLPROV
     C     *LIKE         DEFINE    IIPROV        KKPROV
     C     *LIKE         DEFINE    DGDESC        KKDESC
     C     *LIKE         DEFINE    OPPROV        K1PROV
     C     *LIKE         DEFINE    STMFS         XXMFS
     C*
     C     *LIKE         DEFINE    STMPPO        XXMPPO
     C     *LIKE         DEFINE    EUPATS        HLPATS
     C     *LIKE         DEFINE    NZZIP         KKZIP
     C     *LIKE         DEFINE    NZCNTY        XXCNTY
     C*
     C/COPY SCOPYMT01C
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
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C                   SETOFF                                       616263
     C                   SETOFF                                       35
     C** SPECIALTY CODE MUST BE VALID IF ENTERED AND NOT A RECON
     C*
     C     SCSPCD        IFNE      *BLANKS
     C     *IN29         ANDEQ     *OFF
     C     EDITST        OREQ      'MS'
     C     SCSPCD        ANDEQ     *BLANKS
     C     SCICN         ANDEQ     'HS'
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
     C******ZIP CODE MUST BE ENTERED & CONTAIN ONLY NUMERIC VALUE
     C*
     C     SCFZIP        IFEQ      *ZEROS
     C     *IN29         ANDEQ     *OFF
     C*
     C                   SETON                                        902392
     C                   GOTO      CK1END
     C*
     C                   ENDIF
       //*** Beg add ***** 07/18/25 *************************************
        If isModEnabled('NCCI Med Data Call');
           If scfzip = *zeros and EOrcn = 0;
              *in92 = *on;
              *in90 = *on;
              leavesr;
           Endif;
        Endif;
       //*** End add ***** 07/18/25 *************************************
     C* IF STATE IS IL
     C     EDITST        IFEQ      'IL'
     C     *IN29         ANDEQ     *OFF
     C                   MOVE      SCFZIP        KKZIP
     C     SCSPCD        IFEQ      'ASC'
     C     NZKY          CHAIN     PNZ01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     NZCNTY        XXCNTY
     C                   MOVEL     NZRGN#        XXRGN#            3 0
     C     NZKY          READE     PNZ01                                  99
     C     *IN99         DOWEQ     *OFF
     C     NZCNTY        IFNE      XXCNTY
     C* If counties are different then call F2549R for selection
     C                   CALL      'F2549R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      'IL'          ##ST              2
     C                   PARM                    ##RRN
     C                   PARM      '1'           ##VIEW            1
     C                   PARM      KKZIP         ##ZIP             5 0
     C                   LEAVE
     C                   ENDIF
     C     NZKY          READE     PNZ01                                  99
     C                   ENDDO
     C     ##RRN         IFNE      0
     C     ##RRN         CHAIN     PNZ02                              99
     C                   MOVE      NZRGN#        XXRGN#
     C                   ENDIF
     C                   ELSE
     C** Send Error Message - "Invalid Illinois Zip Code"
     C                   SETON                                        1990
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ELSE
     C* If Speciality code is not equal to 'ASC'
     C     HZKY          CHAIN     PHZ01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     HZCNTY        XXCNTY
     C                   MOVEL     HZRGN#        XXRGN#
     C     HZKY          READE     PHZ01                                  99
     C     *IN99         DOWEQ     *OFF
     C     HZCNTY        IFNE      XXCNTY
     C* If counties are different then call F2548R for region selection
     C                   CALL      'F2548R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      'IL'          ##ST              2
     C                   PARM                    ##RRN
     C                   PARM      '1'           ##VIEW            1
     C                   PARM      KKZIP         ##ZIP             5 0
     C                   LEAVE
     C                   ENDIF
     C     HZKY          READE     PHZ01                                  99
     C                   ENDDO
     C     ##RRN         IFNE      0
     C     ##RRN         CHAIN     PHZ02                              99
     C                   MOVE      HZRGN#        XXRGN#
     C                   ENDIF
     C                   ELSE
     C** Send Error Message - "Invalid Illinois Zip Code"
     C                   SETON                                        1990
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF                                                       SCSPCD <> 'ASC'
     C                   ENDIF
     C*** IF KY BILL AND EDI IS INSTALLED PAT ACCT IS REQD
     C*
     C     EDITST        IFEQ      'KY'
     C     ##EDI         ANDEQ     'Y'
     C     SCACCT        ANDEQ     *BLANKS
     C                   SETON                                        9091
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCEDI IFNE BLANKS
     C*
     C* PRINCIPAL DIAGNOSIS CODE IS REQUIRED
     C*
     C     SCDIA1        IFEQ      *BLANKS
     C                   SETON                                        9045
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C** VALIDATE 1ST DIAGNOSIS CODE
     C*
     C     SCDIA1        IFNE      *BLANKS
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxicd10 = '1' and scICdInd ='Y';
          kkICd10 = scDIa1;
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
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   SCDIA1        KKICD9
     C                   MOVEL(P)  SCDIA1        KKICD9
     C*** End Chg *** 05/23/13 ***************************************
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        9073
     C                   GOTO      CK1END
     C                   ENDIF
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   ENDIF                                                  SCDIA1 NE *BLANKS
     C*
     C* VALIDATE TYPE OF BILL
     C*
     C     SCBLTY        IFEQ      *BLANKS
     C     EDITST        ANDEQ     'MA'
     C     SCBLTY        OREQ      *BLANKS
     C     EDITST        ANDEQ     'MI'
     C     SCBLTY        OREQ      *BLANKS
     C     EDITST        ANDEQ     'TN'
     C     SCBLTY        OREQ      *BLANKS
     C     EDITST        ANDEQ     'AR'
     C     SCBLTY        OREQ      *BLANKS
     C     EDITST        ANDEQ     'CT'
     C     SCBLTY        OREQ      *BLANKS
     C     EDITST        ANDEQ     'IL'
     C*** Type of bill is required when EODSTY = 'DR'/'DS'.
     C     SCBLTY        OREQ      *BLANKS
     C     EODSTY        ANDEQ     'DR'
     C     SCBLTY        OREQ      *BLANKS
     C     EODSTY        ANDEQ     'DS'
     C*** Type of bill is required when STMFS = 'Y'.
     C     SCBLTY        OREQ      *BLANKS
     C     XXMFS         ANDEQ     'Y'
     C                   SETON                                        9049
     C                   GOTO      CK1END
     C                   ENDIF
     C     SCBLTY        IFNE      *BLANKS
     C                   MOVEL(P)  BTTBID        KKTBID
     C                   MOVEL(P)  SCBLTY        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9071
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCBLTY NE *BLANKS
     C*
     C* VALIDATE THE FROM STATEMENT COVERS DATE
     C*
     C     EDITST        IFEQ      'IL'
     C     SCICN         IFEQ      'IM'
     C     SCICN         OREQ      'IS'
     C     SCICN         OREQ      'IP'
     C     SCICN         OREQ      'IR'
     C     SCSCPF        IFEQ      0
     C                   SETON                                        9026
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*** from date of service is required when EODSTY = 'DR/DS' and
     C*** type of service in inpatient.
     C     EODSTY        IFEQ      'DR'
     C     EODSTY        OREQ      'DS'
     C     SCICN         IFEQ      'IM'
     C     SCICN         OREQ      'IS'
     C     SCICN         OREQ      'IP'
     C     SCICN         OREQ      'IR'
     C     SCSCPF        IFEQ      *ZEROS
     C                   SETON                                        9095
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*** From date of service is required when STMFS = 'Y'.
     C     XXMFS         IFEQ      'Y'
     C                   SETON                                        5724
     C                   SETOFF                                       6450
     C     SCSCPF        IFEQ      *ZEROS
     C                   SETON                                        9095
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
     C     SCSCPF        IFNE      *ZEROS
     C                   Z-ADD     SCSCPF        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9074
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     SRDTO         SCSCPF
     C                   ENDIF                                                  SRERR = '1'
     C                   ENDIF                                                  SCSCPF NE *ZEROS
     C*
     C* VALIDATE THE STATEMENT COVERS TO DATE
     C*
     C     EDITST        IFEQ      'IL'
     C     SCICN         IFEQ      'IM'
     C     SCICN         OREQ      'IS'
     C     SCICN         OREQ      'IP'
     C     SCICN         OREQ      'IR'
     C     SCSCPT        IFEQ      0
     C                   SETON                                        9028
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*** to date of service is required when EODSTY = 'DR/DS' and
     C*** type of service in inpatient.
     C     EODSTY        IFEQ      'DR'
     C     EODSTY        OREQ      'DS'
     C     SCICN         IFEQ      'IM'
     C     SCICN         OREQ      'IS'
     C     SCICN         OREQ      'IP'
     C     SCICN         OREQ      'IR'
     C     SCSCPT        IFEQ      *ZEROS
     C                   SETON                                        9096
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*** To date of service is required when STMFS = 'Y'.
     C     XXMFS         IFEQ      'Y'
     C                   SETON                                        5724
     C                   SETOFF                                       6450
     C     SCSCPT        IFEQ      *ZEROS
     C                   SETON                                        9096
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
     C     SCSCPT        IFNE      *ZEROS
     C                   Z-ADD     SCSCPT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9075
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     SRDTO         SCSCPT
     C                   ENDIF                                                  SRERR = '1'
     C                   ENDIF                                                  SCSCPT NE *ZEROS
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
     C                   SETON                                        9076
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLSCPF NE *ZEROS
     C*
     C     HLSCPT        IFNE      *ZEROS
     C     HLSCPT        ANDLT     EOACDT
     C                   SETON                                        9080
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLSCPT NE *ZEROS
     C*
     C* STATEMENT COVERS TO AND FROM DATES CANNOT BE GREATER THAN SYSTEM DATE
     C* UNLESS SYSTEM FLAG ALLOWS                                      M DATE
     C     ##SDTS        IFNE      'Y'
     C*
     C     HLSCPF        IFNE      *ZEROS
     C     HLSCPF        ANDGT     HLSYDT
     C                   SETON                                        9081
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLSCPF NE *ZEROS
     C*
     C     HLSCPT        IFNE      *ZEROS
     C     HLSCPT        ANDGT     HLSYDT
     C                   SETON                                        9082
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLSCPF NE *ZEROS
     C*
     C                   ENDIF                                                  ##SDTS IFNE
     C*
     C* STATEMENT COVERS TO DATE MUST BE < STATEMENT COVERS TO DATE
     C*
     C     SCSCPF        IFNE      *ZEROS
     C     SCSCPT        ANDNE     *ZEROS
     C     HLSCPF        ANDGT     HLSCPT
     C                   SETON                                        9083
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C*  FROM/TO SERVICE DATES MUST BE BLANK OR BOTH ENTERED
     C*
     C     SCSCPF        IFEQ      *ZEROS
     C     SCSCPT        ANDNE     *ZEROS
     C     SCSCPF        ORNE      *ZEROS
     C     SCSCPT        ANDEQ     *ZEROS
     C                   SETON                                        9084
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCSCPF = *ZEORS
     C*
     C* VALIDATE ADMISSION DATE IF ENTERED
     C*
     C     EDITST        IFEQ      'AR'
     C     SCICN         ANDEQ     'IM'
     C     SCICN         OREQ      'IS'
     C     SCICN         OREQ      'IP'
     C     SCICN         OREQ      'IR'
     C     SCADDT        IFEQ      *ZEROS
     C                   SETON                                        906163
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
     C     EODSTY        IFEQ      'DR'
     C     EODSTY        OREQ      'DS'
     C     SCICN         IFEQ      'IM'
     C     SCICN         OREQ      'IP'
     C     SCICN         OREQ      'IR'
     C     SCICN         OREQ      'IS'
     C     SCICN         OREQ      'HO'
     C     SCICN         OREQ      'HS'
     C     SCICN         OREQ      'AO'
     C     SCICN         OREQ      'AS'
     C                   SETON                                        6450
     C                   SETOFF                                       5724
     C     SCADDT        IFEQ      *ZEROS
     C                   SETON                                        906163
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF                                                  SCICN = o/p
     C                   ENDIF                                                  EODSTY = DR/DS
     C*** Addmission date is required when STMFS = 'Y'.
     C     XXMFS         IFEQ      'Y'
     C                   SETON                                        5724
     C                   SETOFF                                       6450
     C     SCADDT        IFEQ      *ZEROS
     C                   SETON                                        906163
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
     C     SCADDT        IFNE      *ZEROS
     C                   Z-ADD     SCADDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9072
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
     C                   SETON                                        9077
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLADDT NE *ZEROS
     C*
     C* ADMISSION DATE CANNOT BE GREATER THAN SYSTEM DATE
     C*
     C     HLADDT        IFNE      *ZEROS
     C     HLADDT        ANDGT     HLSYDT
     C                   SETON                                        9085
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLADDT NE *ZEROS
     C*
     C* ADMIT HOUR AND DISCHARGE HOUR MUST BE BETWEEN 1 AND 24
     C* IF ENTERED
     C*
     C     SCADHR        IFNE      *ZEROS
     C     SCADHR        IFGT      24
     C                   SETON                                        9086
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCADHR LT 1
     C                   ENDIF                                                  SCADHR NE *ZEROS
     C*
     C* DISCHARGE DATE IS MANDATORY FOR MS STATE
     C     EDITST        IFEQ      'MS'
     C     EODSTY        OREQ      'DR'
     C     SCICN         ANDEQ     'IM'
     C     EODSTY        OREQ      'DR'
     C     SCICN         ANDEQ     'IP'
     C     EODSTY        OREQ      'DR'
     C     SCICN         ANDEQ     'IR'
     C     EODSTY        OREQ      'DR'
     C     SCICN         ANDEQ     'IS'
     C     EODSTY        OREQ      'DS'
     C     SCICN         ANDEQ     'IM'
     C     EODSTY        OREQ      'DS'
     C     SCICN         ANDEQ     'IP'
     C     EODSTY        OREQ      'DS'
     C     SCICN         ANDEQ     'IR'
     C     EODSTY        OREQ      'DS'
     C     SCICN         ANDEQ     'IS'
     C*
     C     EODSTY        OREQ      'DR'
     C     SCICN         ANDEQ     'HO'
     C     EODSTY        OREQ      'DR'
     C     SCICN         ANDEQ     'HS'
     C     EODSTY        OREQ      'DR'
     C     SCICN         ANDEQ     'AO'
     C     EODSTY        OREQ      'DR'
     C     SCICN         ANDEQ     'AS'
     C     EODSTY        OREQ      'DS'
     C     SCICN         ANDEQ     'HO'
     C     EODSTY        OREQ      'DS'
     C     SCICN         ANDEQ     'HS'
     C     EODSTY        OREQ      'DS'
     C     SCICN         ANDEQ     'AO'
     C     EODSTY        OREQ      'DS'
     C     SCICN         ANDEQ     'AS'
     C     XXMFS         OREQ      'Y'
     C     SCDCDT        IFEQ      *ZEROS
     C                   SETON                                        9058
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
     C     EDITST        IFEQ      'IL'
     C     SCICN         IFEQ      'IM'
     C     SCICN         OREQ      'IP'
     C     SCICN         OREQ      'IR'
     C     SCICN         OREQ      'IS'
     C     SCDCDT        IFEQ      *ZEROS
     C                   SETON                                        9058
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C     EDITST        IFEQ      'AR'
     C     SCICN         ANDEQ     'IM'
     C     SCICN         OREQ      'IS'
     C     SCICN         OREQ      'IP'
     C     SCICN         OREQ      'IR'
     C     SCDCDT        IFEQ      *ZEROS
     C                   SETON                                        906235
     C                   GOTO      CK1END
     C                   ENDIF
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
     C* DISCHARGE DATE CANNOT BE LESS THAN ACCIDENT DATE.
     C*
     C*
     C                   Z-ADD     SCDCDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLDCDT
     C*
     C     HLDCDT        IFNE      *ZEROS
     C     HLDCDT        ANDLT     EOACDT
     C                   SETON                                        9048
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLADDT NE *ZEROS
     C*
     C     SCDCHR        IFNE      *ZEROS
     C     SCDCHR        IFGT      24
     C                   SETON                                        9087
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCDCHR LT 1
     C                   ENDIF                                                  SCADHR NE *ZEROS
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
     C* VALIDATE THE SUBMISSION DATE
     C*
     C     SCSBDT        IFNE      *ZEROS
     C                   Z-ADD     SCSBDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9088
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
     C                   SETON                                        9089
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLSBDT LT EOACDT
     C                   ENDIF                                                  SCSBDT NE *ZEROS
     C     EDITST        IFEQ      'AL'
     C     SCICN         ANDEQ     *BLANKS
     C     EDITST        OREQ      'MA'
     C     SCICN         ANDEQ     *BLANKS
     C     EDITST        OREQ      'MI'
     C     SCICN         ANDEQ     *BLANKS
     C     EDITST        OREQ      'MS'
     C     SCICN         ANDEQ     *BLANKS
     C     EDITST        OREQ      'TN'
     C     SCICN         ANDEQ     *BLANKS
     C     EDITST        OREQ      'AR'
     C     SCICN         ANDEQ     *BLANKS
     C     EDITST        OREQ      'CT'
     C     SCICN         ANDEQ     *BLANKS
     C     EDITST        OREQ      'IL'
     C     SCICN         ANDEQ     *BLANKS
     C                   SETON                                        9067
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C*
     C*
     C* VALIDATE BOTH TYPES OF SERVICE
     C*
     C     EODSTY        IFEQ      'DR'
     C     EODSTY        OREQ      'DS'
     C     XXMFS         OREQ      'Y'
     C*
     C     SCICN         IFEQ      *BLANKS
     C                   SETON                                        9067
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C                   ENDIF                                                  EODSTY = DR/DS
     C     SCICN         IFNE      *BLANKS
     C                   MOVE      'SVTY'        KKTBID
     C                   MOVEL(P)  SCICN         KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9038
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCICN NE *BLANKS
     C*
     C     SCDCN         IFNE      *BLANKS
     C                   MOVE      'SVTZ'        KKTBID
     C                   MOVEL(P)  SCDCN         KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9039
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCDCN NE *BLANKS
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
     C                   SETON                                        9041
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
     C*
     C*
     C** ELIGIBLE FOR DATA SUBMISSION FEILD IS REQUIRED IF IN30 IS ON
     C*          (DETERMINED IN PGMSTR SUBROUINTE)
     C*
     C   30SCEDI         IFEQ      *BLANKS
     C                   SETON                                        9078
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCEDI IFEQ BLANKS
     C*
     C     SCEDI         IFNE      *BLANKS
     C     SCEDI         ANDNE     'Y'
     C     SCEDI         ANDNE     'N'
     C                   SETON                                        9079
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCEDI IFNE BLANKS
     C*
     C*
     C     CLTID         IFEQ      '114'
     C*
     C     SCICN         IFEQ      'IR'
     C     XXMFS         ANDEQ     'Y'
     C     SCPATS        ANDEQ     *BLANKS
     C*
     C                   SETON                                        9018
     C                   SETOFF                                       19
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C     SCPATS        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  'PATS'        KKTBID
     C                   MOVEL(P)  SCPATS        KKKEY
     C*
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9019
     C                   SETOFF                                       18
     C                   GOTO      CK1END
     C                   ENDIF                                                              *IN99 =
     C                   ENDIF                                                              SCPATS <
     C*
     C                   ENDIF                                                              CLTID =
     C**** IF STATE IS DE
     C*
     C* Highlight SCPROV if PPO INSTALLED
     C     XXMPPO        IFEQ      'Y'
     C     XXBLTY        ANDNE     '83'
     C                   SETON                                        40
     C                   ENDIF
     C     XXBLTY        IFEQ      '83'
     C     XXMPPO        ANDEQ     'Y'
     C     EOAST         ANDEQ     'NJ'
     C     *IN40         ANDEQ     *ON
     C                   SETOFF                                       40
     C                   ENDIF
     C     EDITST        IFEQ      'MS'
     C*** DRG code mandatory when bill is medicare PPO.
     C     EODSTY        OREQ      'DR'
     C     EODSTY        OREQ      'DS'
     C*** Drug# is required when STMFS = 'Y'.
     C     XXMFS         OREQ      'Y'
     C***** Beg Add *** 04/04/16 **************************************
     C     SCAST         OREQ      'OK'
     C***** End Add *** 04/04/16 **************************************
     C     SCICN         IFEQ      'IM'
     C     SCICN         OREQ      'IP'
     C     SCICN         OREQ      'IR'
     C     SCICN         OREQ      'IS'
     C* Highlight SCPROV if inpatient bill
     C                   SETON                                        40
     C* Validate SCDRG for inpatient
     C* Do not validate SCDRG for inpatient 'IR' and Medicare fee schedule
     C     SCICN         IFNE      'IR'
     C     XXMFS         ORNE      'Y'
     C* Don't mandate SCDRG for SNF bills
     C     XXBLTY        IFNE      '18'
     C     XXBLTY        ANDNE     '21'
     C     XXMFS         ORNE      'Y'
     C     SCDRG         IFEQ      0
     C                   SETON                                        905152
     C                   SETON                                        53
     C                   SETOFF                                       54
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCDRG=0
     C                   ENDIF                                                  SNF bills
     C                   ENDIF                                                  XXMFS = 'Y', SCICN =
     C                   ELSE
     C     XXMFS         IFNE      'Y'
     C                   SETOFF                                       40
     C                   ENDIF                                                     XXMFS     IFNE 'Y
     C                   ENDIF                                                  SCICN=INPATIENT
     C                   ENDIF                                                  EDITST <> 'MD'
     C     EDITST        IFEQ      'IL'
     C     SCICN         IFEQ      'IM'
     C     SCICN         OREQ      'IP'
     C     SCICN         OREQ      'IS'
     C* Don't mandate SCDRG for SNF bills
     C     XXBLTY        IFNE      '18'
     C     XXBLTY        ANDNE     '21'
     C     XXMFS         ORNE      'Y'
     C     SCDRG         IFEQ      0
     C                   SETON                                        905152
     C                   SETON                                        5355
     C                   SETOFF                                       54
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCDRG=0
     C                   ENDIF                                                  SNF bills
     C                   ENDIF                                                  SCICN=IS,IH
     C                   ENDIF                                                  EDITST = 'IL'
     C**
      * VALIDATE DRG CODE
     C     SCDRG         IFNE      *ZEROS
     C                   MOVEL     SCDRG         KKDRG#
     C     DGKY          CHAIN     PDG02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9056
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     DGDESC        KKDESC
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCDRG NE *ZEROS
     C     XXBLTY        IFNE      '83'
     C** delete to make it for all states and not only 'SC'
     C     XXMFS         IFEQ      'Y'
     C     XXMPPO        OREQ      'Y'
     C                   SETON                                        40
     C                   ELSE
     C                   SETOFF                                       40
     C                   ENDIF
     C                   ENDIF
     C     XXBLTY        IFEQ      '83'
     C     XXMPPO        ANDEQ     'Y'
     C     EOAST         ANDEQ     'NJ'
     C     XXMFS         OREQ      'Y'
     C     SCSPCD        ANDEQ     'ASC'
     C                   SETON                                        97
     C                   ELSE
     C                   SETOFF                                       97
     C                   ENDIF
     C***** Beg Add ****** 03/13/15 ***********************************
      /Free
       if editst = 'DE';

       *in92 = *off;
       *in49 = *off;
       *in67 = *off;
       *in51 = *off;
       *in52 = *off;
       *in53 = *off;
       *in54 = *on;
       *in61 = *off;

          select;
          //*** Beg Chg ***** 07/18/25 *************************************
          //*** when scfzip = *zero;
          when scfzip = *zero and EOrcn = 0;
          //*** End Chg ***** 07/18/25 *************************************
             *in92 = *on;
             *in90 = *on;
             leaveSr;
          when scBlTy = *blanks;
             *in49 = *on;
             *in90 = *on;
             leaveSr;
          when scIcn = *blanks;
             *in67 = *on;
             *in90 = *on;
             leaveSr;
          other;
          endsl;

          if scicn = 'IP' or scicn = 'IS';
             select;
             when scDrg = *zero;
                *in51 = *on;
                *in52 = *on;
                *in53 = *on;
                *in54 = *off;
                *in90 = *on;
                leaveSr;
             when scAddt = *zero;
                *in61 = *on;
                *in90 = *on;
                leaveSr;
             other;
             endsl;
          endif;

       endif;
      /End-Free
     C***** End Add ****** 03/13/15 ***********************************
     C     CK1END        ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
       //*** Beg add ***** 07/18/25 *************************************
        If isModEnabled('NCCI Med Data Call');
           // %check return non zero if non numeric value including blank
           invalidPos = %check('0123456789': %trim(scnpi1));
           if invalidPos <> 0 or scnpi1 = *blanks;
              mssnpi = 'MC50007';
              ersnpi = *on;
              *in90 = *on;
           endif;
        Endif;
       //*** End add ***** 07/18/25 *************************************
     C*** Beg Add *** 06/21/12 ****************************************
      /Free
         If Eoast <> 'HL';
      /End-Free
     C*** End Add *** 06/21/12 ****************************************
     C***** Provider Id
     C     SCICN         IFEQ      'IM'
     C     SCICN         OREQ      'IP'
     C     SCICN         OREQ      'IR'
     C     SCICN         OREQ      'IS'
     C     SCPROV        IFEQ      ' '
     C     EDITST        ANDEQ     'MS'
     C                   SETON                                        9026
     C                   GOTO      CK2END
     C                   ENDIF
     C                   ENDIF
     C     XXBLTY        IFNE      '83'
     C     XXMFS         IFEQ      'Y'
     C     XXMPPO        OREQ      'Y'
     C                   SETON                                        40
     C     SCPROV        IFEQ      ' '
     C                   SETON                                        9026
     C                   GOTO      CK2END
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
      *VALIDATE PROVIDER ID
      *
     C     SCPROV        IFNE      *BLANKS
     C     XXBLTY        ANDNE     '83'
     C                   MOVEL     SCPROV        KKPROV
     C                   MOVEL     SCPROV        K1PROV
     C*** Change edit based on if bill is medicare PPO processing
     C***   and type of service = I/P or O/P.
     C                   SELECT
     C     XXBLTY        WHENEQ    '18'
     C     XXMFS         ANDEQ     'Y'
     C     XXBLTY        OREQ      '21'
     C     XXMFS         ANDEQ     'Y'
     C*
     C     K1PROV        CHAIN     PSK02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9028
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C*
     C     SCICN         WHENEQ    'IR'
     C     XXMFS         ANDEQ     'Y'
     C*
     C     K1PROV        CHAIN     PIR02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9028
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C*
     C     EODSTY        WHENEQ    'DR'
     C     SCICN         ANDEQ     'IP'
     C     EODSTY        OREQ      'DR'
     C     SCICN         ANDEQ     'IR'
     C     EODSTY        OREQ      'DR'
     C     SCICN         ANDEQ     'IM'
     C     EODSTY        OREQ      'DR'
     C     SCICN         ANDEQ     'IS'
     C     EODSTY        OREQ      'DS'
     C     SCICN         ANDEQ     'IP'
     C     EODSTY        OREQ      'DS'
     C     SCICN         ANDEQ     'IR'
     C     EODSTY        OREQ      'DS'
     C     SCICN         ANDEQ     'IS'
     C     EODSTY        OREQ      'DS'
     C     SCICN         ANDEQ     'IM'
     C     XXMFS         OREQ      'Y'
     C     SCICN         ANDEQ     'IM'
     C     XXMFS         OREQ      'Y'
     C     SCICN         ANDEQ     'IR'
     C     XXMFS         OREQ      'Y'
     C     SCICN         ANDEQ     'IP'
     C     XXMFS         OREQ      'Y'
     C     SCICN         ANDEQ     'IS'
     C     K1PROV        CHAIN     LTP01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9028
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C     EODSTY        WHENEQ    'DR'
     C     SCICN         ANDEQ     'AO'
     C     EODSTY        OREQ      'DR'
     C     SCICN         ANDEQ     'AS'
     C     EODSTY        OREQ      'DR'
     C     SCICN         ANDEQ     'HO'
     C     EODSTY        OREQ      'DR'
     C     SCICN         ANDEQ     'HS'
     C     EODSTY        OREQ      'DS'
     C     SCICN         ANDEQ     'AO'
     C     EODSTY        OREQ      'DS'
     C     SCICN         ANDEQ     'AS'
     C     EODSTY        OREQ      'DS'
     C     SCICN         ANDEQ     'HO'
     C     EODSTY        OREQ      'DS'
     C     SCICN         ANDEQ     'HS'
     C     XXMFS         OREQ      'Y'
     C     SCICN         ANDEQ     'HO'
     C     XXMFS         OREQ      'Y'
     C     SCICN         ANDEQ     'HS'
     C     XXMFS         OREQ      'Y'
     C     SCICN         ANDEQ     'AO'
     C     XXMFS         OREQ      'Y'
     C     SCICN         ANDEQ     'AS'
     C     K1PROV        CHAIN     LOP01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9028
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   OTHER
     C     EOAST         IFEQ      'MS'
     C     SCICN         ANDEQ     'IM'
     C     EOAST         OREQ      'MS'
     C     SCICN         ANDEQ     'IR'
     C     EOAST         OREQ      'MS'
     C     SCICN         ANDEQ     'IS'
     C     EOAST         OREQ      'MS'
     C     SCICN         ANDEQ     'IP'
     C     IIKY          CHAIN     PII02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9028
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  EOAST IFEQ MS
     C                   ENDSL                                                  EODSTY = DR/DS
     C                   ENDIF                                                  SCPROV NE *BLANKS
     C*** Change edit based on if bill is medicare PPO processing
     C***   and type of service = I/P or O/P.
     C     EODSTY        IFEQ      'DR'
     C     EODSTY        OREQ      'DS'
     C*** Drug# is required when STMFS = 'Y'.
     C     XXMFS         OREQ      'Y'
     C                   SETON                                          37
     C     *IN37         IFEQ      *ON
     C     SCICN         IFEQ      'IM'
     C     SCICN         OREQ      'IP'
     C     SCICN         OREQ      'IR'
     C     SCICN         OREQ      'IS'
     C     SCTRST        IFEQ      *BLANKS
     C     XXBLTY        IFEQ      '21'
     C     XXMFS         ANDEQ     'Y'
     C     XXBLTY        OREQ      '18'
     C     XXMFS         ANDEQ     'Y'
     C** Don't validate
     C                   ELSE
     C                   SETOFF                                       57
     C                   SETON                                        64
     C                   SETOFF                                         2450
     C                   SETON                                        90
     C                   GOTO      CK2END
     C                   ENDIF
     C                   ELSE
     C                   MOVEL(P)  SCTRST        KKKEY
     C                   MOVEL(P)  'TRST'        KKTBID
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        906457
     C                   SETOFF                                         2450
     C                   GOTO      CK2END
     C                   ELSE
     C                   MOVEL     GTDESC        SCTRDS
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  sctrst = ' '
     C                   ENDIF                                                  scicn = i/p
     C                   ENDIF                                                  *INXX = *ON
     C                   ENDIF                                                  EODSTY = DR/DS
     C     XXBLTY        IFNE      '83'
     C*** Add edit for implant status if STMFS = 'Y'
     C     XXMFS         IFEQ      'Y'
     C                   SETON                                          25
     C     *IN25         IFEQ      *ON
     C     SCICN         IFEQ      'IM'
     C     SCICN         OREQ      'IP'
     C     SCICN         OREQ      'IR'
     C     SCICN         OREQ      'IS'
     C     SCIMST        IFEQ      *BLANKS
     C     XXBLTY        IFEQ      '21'
     C     XXMFS         ANDEQ     'Y'
     C     XXBLTY        OREQ      '18'
     C     XXMFS         ANDEQ     'Y'
     C** Don't validate
     C                   ELSE
     C                   SETON                                        9050
     C                   SETOFF                                         24
     C                   SETOFF                                         5764
     C                   GOTO      CK2END
     C                   ENDIF
     C                   ELSE
     C                   MOVEL(P)  SCIMST        KKKEY
     C                   MOVEL(P)  'IMST'        KKTBID
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        905024
     C                   SETOFF                                         5764
     C                   GOTO      CK2END
     C                   ELSE
     C                   MOVEL     GTDESC        SCIMDS
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  scimst = ' '
     C                   ENDIF                                                  scicn = i/p
     C                   ENDIF                                                  *IN25 = *ON
     C                   ENDIF                                                  stmfs = 'Y'
     C                   ENDIF                                                  blty <> 83x
     C*** CITY IS REQUIRED
     C*
     C     *IN97         IFEQ      *ON
     C     SCRCTY        ANDEQ     *BLANKS
     C                   SETON                                        9021
     C                   GOTO      CK2END
     C                   ENDIF                                                  SCRCTY IFEQ BLANKS
     C*
     C** VALIDATE CITY
     C*
     C     SCRCTY        IFNE      *BLANKS
     C*          SCRST     ANDEQ*BLANKS
     C                   MOVEL(P)  SCRCTY        KKRCTY
     C     LO:ALPHA      XLATE     KKRCTY        KKRCTY
     C     KKRCTY        CHAIN     PSA01                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        9022
     C                   GOTO      CK2END
     C                   ELSE
     C                   MOVEL(P)  SACTY         SCRCTY
     C                   ENDIF
     C                   ENDIF
     C*
     C*** STATE IS REQUIRED
     C*
     C     *IN97         IFEQ      *ON
     C     SCRST         ANDEQ     *BLANKS
     C                   SETON                                        9062
     C                   GOTO      CK2END
     C                   ENDIF                                                  SCRST IFEQ BLANKS
     C*
     C** VALIDATE STATE
     C*
     C     SCRST         IFNE      *BLANKS
     C*          SCRCTY    ANDEQ*BLANKS
     C                   MOVEL(P)  SCRST         KKRST
     C     LO:ALPHA      XLATE     KKRST         KKRST
     C     KKRST         CHAIN     PSA02                              60
     C     *IN60         IFEQ      *ON
     C                   SETON                                        9063
     C                   GOTO      CK2END
     C                   ELSE
     C                   MOVEL(P)  SAST          SCRST
     C                   ENDIF
     C                   ENDIF
     C*
     C** VALIDATE CITY/STATE COMBINATION
     C*
     C     SCRCTY        IFNE      *BLANKS
     C     SCRST         ANDNE     *BLANKS
     C                   MOVEL(P)  SCRCTY        KKRCTY
     C                   MOVEL(P)  SCRST         KKRST
     C     LO:ALPHA      XLATE     KKRCTY        KKRCTY
     C     LO:ALPHA      XLATE     KKRST         KKRST
     C                   Z-ADD     EOCRDT        KKEFDT
     C     SAKY          SETGT     PSA01                              60
     C     SAKY1         READPE    PSA01                                  60
     C     *IN60         IFEQ      *OFF
     C     SASTAT        ANDEQ     'A'
     C                   MOVE      SACBSA        XXCBSA            5
     C                   ELSE
     C                   SETON                                        9094
     C                   GOTO      CK2END
     C                   ENDIF
     C                   ENDIF
     C*** Beg Add *** 06/21/12 ****************************************
      /Free
       //** Begin Add *** 10/06/15 *****************************************
         Else;
       //
       // Medicare Provider ID is required if discharge date >= 08/14/2015
       //
            srDtI  = scDcDt;
            srFFmt = ' ';
            srTFmt = '*CYMD';
            srDtO  = ' ';
            srErr  = ' ';
            hlDcDt = 0;

            ConvertDate(srDtI:srFFmt:srTFmt:srDtO:srErr);

            If srErr = *Off;
               hlDcDt = %Dec(%Trim(srDtO):7:0);
            EndIf;

            If        scProv = *Blanks And (scICN = 'HO' Or
                      scICN = 'HS') And hlDcDt >= 1150814;
               *in26 = *On;
               *in90 = *On;
            EndIf;
       //** End   Add *** 10/06/15 *****************************************
         Endif;
      /End-Free
     C*** End Add *** 06/21/12 ****************************************
     C*
     C     CK2END        ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=SEARCH.
     C*================================================================
     C     KJDO          BEGSR
     C*
     C*** INITIALIZE POSITION CURSOR INDICATORS
     C*
     C                   SETOFF                                       313233
     C                   SETOFF                                       343536
     C                   SETOFF                                         4243
     C                   SETOFF                                       44
     C                   SETOFF                                       55
     C                   SETOFF                                       22
     C                   SETOFF                                       27
     C                   SETOFF                                       2061
     C*
     C*** CLEAR HOLD FIELDS
     C*
     C                   CLEAR                   HLDIA
     C*
     C                   SELECT
     C*
     C** DIAGNOSIS CODE #2 SEARCH
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
             kkICD10 = scDIa1;
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   SCD1DS = ICdsc1;
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
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCD1DS
     C                   ENDIF
     C*
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
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
     C                   SETON                                        9033      pos cursor
     C                   ENDIF
     C     HFLD          WHENEQ    'SCFZIP'
     C*** Beg Chg *** 07/18/25 ***************************************
     C***                SETON                                        2393
     C                   SETON                                        239390
     C*** End Chg *** 07/18/25 ***************************************
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
     C                   SETON                                        9044
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCICN
     C                   MOVEL     GTDESC        SCICDS
     C                   SETON                                        9042
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
     C                   SETON                                        9042
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCDCN
     C                   MOVEL     GTDESC        SCDCDS
     C                   SETON                                        9043
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C     HFLD          WHENEQ    'SCPATS'
     C                   Z-ADD     0             ##RRN
     C                   MOVE      SCPATS        HLPATS
     C                   MOVE      'PATS'        ##TBID
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM                    ##TBID
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVE      HLPATS        SCPATS
     C                   SETON                                        9020
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCPATS
     C                   SETON                                        9044
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C** DRG Code F10 Search
     C     HFLD          WHENEQ    'SCDRG'
     C                   Z-ADD     0             ##RRN
     C                   MOVE      SCDRG         HLDRG
     C                   CALL      'F2354R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      0             ##RRN
     C                   PARM      0             KKDRG#
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVE      HLDRG         SCDRG
     C                   SETON                                        9055
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PDG01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     DGDRG#        SCDRG
     C                   MOVEL     DGDESC        KKDESC
     C                   SETON                                        9036
     C                   ENDIF
     C                   ENDIF
     C** CITY SEARCH
     C*
     C     HFLD          WHENEQ    'SCRCTY'
     C                   MOVE(P)   SCRCTY        HLRCTY           25
     C                   CLEAR                   ##RRN
     C                   CALL      'F2359R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##RRN             9 0
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVEL(P)  HLRCTY        SCRCTY
     C                   SETON                                        9020      pos cursor
     C                   ELSE
     C     ##RRN         CHAIN     PSA03                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  SACTY         SCRCTY
     C                   MOVEL(P)  SAST          SCRST
     C                   SETOFF                                       2162      error if blank
     C                   SETOFF                                       229463     invalid value
     C                   SETON                                        9020      pos cursor
     C                   ENDIF
     C                   ENDIF
     C*
     C** STATE SEARCH
     C*
     C     HFLD          WHENEQ    'SCRST'
     C                   MOVE(P)   SCRST         HLRST             2
     C                   CLEAR                   ##RRN
     C                   CALL      'F2359R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##RRN             9 0
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVEL(P)  HLRST         SCRST
     C                   SETON                                        9061      pos cursor
     C                   ELSE
     C     ##RRN         CHAIN     PSA03                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  SAST          SCRST
     C                   MOVEL(P)  SACTY         SCRCTY
     C                   SETOFF                                       2162      error if blank
     C                   SETOFF                                       229463     invalid value
     C                   SETON                                        9061      pos cursor
     C                   ENDIF
     C                   ENDIF
     C**
     C** Provider Id F10 search
     C     HFLD          WHENEQ    'SCPROV'
     C     XXBLTY        ANDNE     '83'
     C     XXBLTY        IFEQ      '18'
     C     XXMFS         ANDEQ     'Y'
     C     XXBLTY        OREQ      '21'
     C     XXMFS         ANDEQ     'Y'
     C                   MOVEL     SCPROV        KKPROV
     C                   CALL      'F2379R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##RRN
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PSK01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  SKPROV        SCPROV
     C                   SETON                                        9057
     C                   SETOFF                                       64
     C                   SETOFF                                         2450
     C                   MOVE      *ZEROS        ##RRN
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   SETON                                        5790
     C                   SETON                                        2450
     C                   SETOFF                                       64
     C                   ENDIF
     C*
     C                   ELSE
     C     SCICN         IFEQ      'IR'
     C     XXMFS         ANDEQ     'Y'
     C                   MOVEL     SCPROV        KKPROV
     C                   CALL      'F2383R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##RRN
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PIR01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  IRPROV        SCPROV
     C                   SETON                                        9057
     C                   SETOFF                                       64
     C                   SETOFF                                         2450
     C                   MOVE      *ZEROS        ##RRN
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   SETON                                        5790
     C                   SETON                                        2450
     C                   SETOFF                                       64
     C                   ENDIF
     C*
     C                   ELSE
     C     EODSTY        IFEQ      'DR'
     C     EODSTY        OREQ      'DS'
     C*** Help when STMFS = 'Y
     C     XXMFS         OREQ      'Y'
     C*** Help when STMFS = 'Y
     C     XXMPPO        OREQ      'Y'
     C*
     C     SCICN         IFEQ      'HO'
     C     SCICN         OREQ      'HS'
     C     SCICN         OREQ      'AO'
     C     SCICN         OREQ      'AS'
     C                   MOVEL     SCPROV        KKPROV
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
     C                   MOVEL(P)  OPPROV        SCPROV
     C                   SETON                                        9057
     C                   SETOFF                                       64
     C                   SETOFF                                         2450
     C                   MOVE      *ZEROS        ##RRN
     C                   ENDIF
     C                   ELSE
     C                   SETON                                        5790
     C                   SETON                                        2450
     C                   SETOFF                                       64
     C*
     C                   ENDIF                                                  ##RRN <> 0
     C                   ELSE
     C     SCICN         IFEQ      'IM'
     C     SCICN         OREQ      'IR'
     C     SCICN         OREQ      'IP'
     C     SCICN         OREQ      'IS'
     C                   MOVEL     SCPROV        KKPROV
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
     C                   MOVEL(P)  PFPROV        SCPROV
     C                   SETON                                        9057
     C                   SETOFF                                       64
     C                   SETOFF                                         2450
     C                   MOVE      *ZEROS        ##RRN
     C                   ENDIF
     C                   ELSE
     C                   SETON                                        5790
     C                   SETON                                        2450
     C                   SETOFF                                       64
     C*
     C                   ENDIF                                                  ##RRN <> 0
     C                   ENDIF                                                  SCICN = 'IM'
     C                   ENDIF                                                  EUICN = 'HO'
     C                   ELSE
     C     EOAST         IFEQ      'MS'
     C     SCICN         ANDEQ     'IM'
     C     EOAST         OREQ      'MS'
     C     SCICN         ANDEQ     'IR'
     C     EOAST         OREQ      'MS'
     C     SCICN         ANDEQ     'IS'
     C     EOAST         OREQ      'MS'
     C     SCICN         ANDEQ     'IP'
     C                   Z-ADD     0             ##RRN
     C                   MOVE      SCPROV        HLPROV
     C                   CALL      'F2353R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      0             ##RRN
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVE      HLPROV        SCPROV
     C                   SETON                                        5790
     C                   SETON                                        2450
     C                   SETOFF                                       64
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PII01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  IIPROV        SCPROV
     C                   MOVEL(P)  IINAME        SCOVDS
     C                   SETON                                        9036
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     C                   SETON                                        90
     C*
     C                   ENDIF                                                  EOAST = MS
     C                   ENDIF                                                  EODSTY = DR/DS
     C                   ENDIF                                                  SCICN = IR, IRF PSF
     C                   ENDIF                                                  XXblty = 18/21
     C     HFLD          WHENEQ    'SCTRST'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      'TRST'        ##TBID
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        9057
     C                   SETOFF                                         2450
     C                   SETOFF                                       64
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCTRST
     C                   MOVEL     GTDESC        SCTRDS
     C                   SETON                                          24
     C                   SETOFF                                         5764    50
     C                   SETON                                        90
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C     HFLD          WHENEQ    'SCIMST'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      'IMST'        ##TBID
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        9024
     C                   SETOFF                                       50
     C                   SETOFF                                         5764
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCIMST
     C                   MOVEL     GTDESC        SCIMDS
     C                   SETON                                        90
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C                   OTHER
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCACCT'
     C                   SETOFF                                       51
     C                   SETON                                        525354
     C     HFLD          WHENEQ    'SCSCPF'
     C                   SETOFF                                       525354
     C                   SETON                                        51
     C     HFLD          WHENEQ    'SCSCPT'
     C                   SETOFF                                       515354
     C                   SETON                                        52
     C     HFLD          WHENEQ    'SCADDT'
     C                   SETOFF                                       525154
     C                   SETON                                        53
     C     HFLD          WHENEQ    'SCADHR'
     C                   SETOFF                                       525351
     C                   SETON                                        54
     C     HFLD          WHENEQ    'SCDCDT'
     C                   SETOFF                                       5354
     C                   SETON                                        5152
     C     HFLD          WHENEQ    'SCDCHR'
     C                   SETOFF                                       5154
     C                   SETON                                        5253
     C     HFLD          WHENEQ    'SCSBDT'
     C                   SETOFF                                       5152
     C                   SETON                                        5354
     C     HFLD          WHENEQ    'SCEDI'
     C                   SETOFF                                       5253
     C                   SETON                                        5154
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
     C                   SETOFF                                       17
     C                   CLEAR                   SCDIA1
     C                   CLEAR                   SCD1DS
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
     C                   MOVE      *BLANKS       SCRCTY
     C                   MOVE      *BLANKS       SCRST
     C                   CLEAR                   SCRFEI
     C                   CLEAR                   SCRLSN
     C                   CLEAR                   SCRFSN
     C                   CLEAR                   SCRAD1
     C                   CLEAR                   SCRAD2
     C                   CLEAR                   SCRNCT
     C                   CLEAR                   SCRNST
     C                   CLEAR                   SCRZIP
     C                   CLEAR                   SCPATS
     C     EDITST        IFEQ      'IL'
     C     @@MODE        ANDNE     'D'
     C                   SETON                                        36
     C                   ELSE
     C     @@MODE        IFNE      'D'
     C                   SETON                                        33
     C                   ENDIF
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
     C                   MOVEL     SCSPCD        ##KEY
     C     SPKY          CHAIN     PSP01                              99
     C  N99              MOVEL     SPDESC        SCSCDS
     C*
     C                   MOVEL     EOAST         SCAST
     C                   MOVE(P)   EOZIPA        SCZIPA
     C*
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
     C                   SETON                                        68
     C                   ELSE
     C                   SETOFF                                       68
     C                   ENDIF                                                  *IN99 IFEQ *OFF
     C*
     C     @@MODE        IFNE      'A'
     C     @@MODE        OREQ      'A'
     C     @@RRN         ANDNE     *ZEROS
     C*
     C                   MOVEL     EUACCT        SCACCT
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   EUDIA1        SCDIA1
     C********           MOVE(P)   SCDIA1        KKICD9
     C                   MOVEL(P)  EUDIA1        SCDIA1
     C                   MOVEL(P)  SCDIA1        KKICD9
     C*** End Chg *** 05/23/13 ***************************************
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       If xxICD10 = '1' and EUicdind ='Y';
          KKICD10 = EUicd1001;
          Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
               scD1ds = ICdsc1;
             Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDSC1        SCD1DS
     C*
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C     EUEDI         IFNE      *BLANKS
     C                   MOVE      EUEDI         SCEDI
     C                   ENDIF                                                  EUEDI NE *BLANKS
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
     C                   Z-ADD     EUSBDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCSBDT
     C*
     C                   MOVE      EUADHR        SCADHR
     C                   Z-ADD     EUDCHR        SCDCHR
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
     C                   MOVE      EUPATS        SCPATS
     C                   Z-ADD     EOCHRG        SCCHRG
     C                   Z-ADD     EOMRA         SCMRA
     C                   Z-ADD     EONDIS        SCNTWK
     C                   Z-ADD     EOOTHR        SCOTHR
     C                   Z-ADD     EOOTH2        SCOTH2
     C                   Z-ADD     EOPAID        SCPAID
     C                   MOVEL(P)  EOFLIC        SCFZIP
     C*** Beg Add *** 07/18/25 ***************************************
     C                   MOVE      EOFLIC        SCZIP4
     C*** End Add *** 07/18/25 ***************************************
     C*
     C                   MOVE      EUANPI        SCANPI
     C                   MOVE      EUONPI        SCONPI
     C                   MOVE      EUNPI1        SCNPI1
     C                   MOVE      EUNPI2        SCNPI2
     C                   MOVEL     EUDRG#        SCDRG
     C                   MOVEL     EUDGDS        KKDESC
     C                   MOVEL     EUMCAR        SCPROV
     C                   MOVEL     EUTRST        SCTRST
     C                   MOVEL     EUIMST        SCIMST
     C                   MOVEL     EURCTY        SCRCTY
     C                   MOVEL     EURST         SCRST
     C                   ENDIF                                                  @@MODE NE 'A'
     C     CLTID         IFEQ      '146'
     C*** Beg Add *** 04/17/13 ************************************************
     C     CLTID         OREQ      '114'
     C*** End Add *** 04/17/13 ************************************************
     C                   SETON                                        17
     C                   MOVE      EURFEI        SCRFEI
     C                   MOVE      EURLSN        SCRLSN
     C                   MOVE      EURFSN        SCRFSN
     C                   MOVE      EURAD1        SCRAD1
     C                   MOVE      EURAD2        SCRAD2
     C                   MOVE      EURNCT        SCRNCT
     C                   MOVE      EURNST        SCRNST
     C                   MOVE      EURZIP        SCRZIP
     C                   ENDIF
     C*
     C     CLTID         IFEQ      '114'
     C*
     C     FNKY          CHAIN     PFN01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C     FNNWCG        ANDEQ     'Y'
     C*
     C                   MOVE      '99'          SCTRST
     C                   MOVE      'N '          SCIMST
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** Beg Add *** 05/23/13 ****************************************
      /Free
          //Beg Chg *** 08/05/13 ***************************************
          //scICdind = 'N';
          //If xxicd10 = *on;
          If xxicd10 = *on and EUicdind ='Y';
          //End Chg *** 08/05/13 ***************************************
             scDIa1  = EUICD1001;
             scICdind= EUICDind ;
          // Beg Add *** 08/05/13 ****************************************
          Elseif EUICDind ='N';
             scICdind = EUICDind;
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
     C                   MOVEL(P)  SCACCT        EUACCT
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   SCDIA1        EUDIA1
     C                   MOVEL(P)  SCDIA1        EUDIA1
     C*** End Chg *** 05/23/13 ***************************************
     C                   MOVE      SCEDI         EUEDI
     C*
     C                   MOVEL     SCPROV        EUMCAR
     C                   MOVEL     SCDRG         EUDRG#
     C                   MOVEL     KKDESC        EUDGDS
     C                   MOVEL     SCTRST        EUTRST
     C                   MOVEL     SCIMST        EUIMST
     C                   MOVEL(P)  SCBLTY        EUBLTY
     C                   MOVEL(P)  SCRCTY        EURCTY
     C                   MOVEL(P)  SCRST         EURST
     C                   MOVEL     XXCBSA        EUCBSA
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
     C                   MOVE      SCADHR        EUADHR
     C                   Z-ADD     SCDCHR        EUDCHR
     C                   MOVEL     SCICN         EUICN
     C                   MOVEL     SCDCN         EUDCN
     C                   MOVE      SCSPCD        EOSPCD
     C                   MOVEL(P)  SCFZIP        EOFLIC
     C*** Beg Add *** 07/18/25 ***************************************
     C                   MOVE      SCZIP4        EOFLIC
     C*** End Add *** 07/18/25 ***************************************
     C                   MOVEL     SCPATS        EUPATS
     C                   MOVE      SCANPI        EUANPI
     C                   MOVE      SCONPI        EUONPI
     C                   MOVE      SCNPI1        EUNPI1
     C                   MOVE      SCNPI2        EUNPI2
     C     CLTID         IFEQ      '146'
     C*** Beg Add *** 04/17/13 ************************************************
     C     CLTID         OREQ      '114'
     C*** End Add *** 04/17/13 ************************************************
     C                   MOVE      SCRFEI        EURFEI
     C                   MOVE      SCRLSN        EURLSN
     C                   MOVE      SCRFSN        EURFSN
     C                   MOVE      SCRAD1        EURAD1
     C                   MOVE      SCRAD2        EURAD2
     C                   MOVE      SCRNCT        EURNCT
     C                   MOVE      SCRNST        EURNST
     C                   MOVE      SCRZIP        EURZIP
     C                   ENDIF
     C*** Beg Add *** 05/23/13 ****************************************
      /Free
          //Beg Chg *** 08/05/13 ***************************************
          EUICDind  = scICdind;
          If xxicd10 = *on and scICdInd ='Y';
          //End Chg *** 08/05/13 ***************************************
             EUICD1001 = scDIa1;
             // If ICD10 is Active then no need of ICD9 fields
             EUDIA1 = *Blanks;
          //Beg Chg *** 08/05/13 ***************************************
          Else;
             EUDIA1 = scDIa1;
             // If ICD09 is Active then no need of ICD10 fields
             EUicd1001 = *Blanks;
          //End Chg *** 08/05/13 ***************************************
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
     C* SET OFF INDICATORS TO BE USED FOR ERROR CHECKING
     C*
     C                   SETOFF                                       383941
     C                   SETOFF                                       4567
     C                   SETOFF                                       5623
     C                   SETOFF                                       212294
     C                   SETOFF                                       6263
       //*** Beg add ***** 07/18/25 ************************************
       mssnpi = *blanks;
       ersnpi = *off;
       //*** End add ***** 07/18/25 ************************************
     C     *INKJ         CASEQ     *ON           KJDO                           F10=search
     C*** Beg Del *** MC367 *** 11/15/12 ******************************
     C******INKK         CASEQ     *ON           KKDO                           F11=EOB/Notes
     C*** End Del *** MC367 *** 11/15/12 ******************************
     C                   ENDCS
     C*** Beg Add *** MC367 *** 11/15/12 ******************************
      /Free
      /If Defined(WEB)
         If *INKK;
           ExSr KKDO;  //F11=EOB/Notes
         EndIf;
      /EndIf
      /End-Free
     C*** End Add *** MC367 *** 11/15/12 ******************************
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
     C     STMFS         IFEQ      'Y'
     C                   MOVEL     STMFS         XXMFS
     C                   ENDIF
     C     STMPPO        IFEQ      'Y'
     C     EODSTY        ANDEQ     'DS'
     C                   MOVEL     STMPPO        XXMPPO
     C                   SETON                                        40
     C                   ENDIF
     C                   ENDIF                                                  *IN99 = *OFF
     C     XXMFS         IFEQ      'Y'
     C                   SETON                                        3725
     C                   SETON                                        5724
     C                   SETOFF                                       6450
     C                   ELSE
     C                   SETOFF                                       405724
     C                   SETOFF                                       3725
     C                   ENDIF
     C     EODSTY        IFEQ      'DR'
     C     EODSTY        OREQ      'DS'
     C                   SETON                                        6450
     C                   SETOFF                                       572437
     C                   ELSE
     C                   SETOFF                                         6450
     C                   ENDIF
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
     C                   SETON                                        69
     C                   ELSE
     C                   SETOFF                                       69
     C                   ENDIF                                                  EONTWK
     C*
     C* SET INITIAL CURSOR POSITON TO DATE FIRST CONSULTED
     C                   SETON                                        37
     C*
     C*
     C* PROTECT SPECIALTY IF EORCN>0
     C*
     C     EORCN         IFGT      *ZERO
     C                   SETON                                        29
     C                   ELSE
     C*
     C                   SETOFF                                       29
     C                   ENDIF                                                  EORCN>0
     C     EDITST        IFEQ      'AL'
     C                   SETON                                        66
     C                   ELSE
     C                   SETOFF                                       66
     C                   ENDIF
     C     EDITST        IFEQ      'MA'
     C     EDITST        OREQ      'MI'
     C     EDITST        OREQ      'TN'
     C     EDITST        OREQ      'AR'
     C     EDITST        OREQ      'CT'
     C     XXMFS         OREQ      'Y'
     C     EODSTY        OREQ      'DR'
     C     EODSTY        OREQ      'DS'
     C                   SETON                                        6566
     C                   ELSE
     C                   SETOFF                                       6566
     C                   ENDIF
     C     EDITST        IFEQ      'MS'
     C     XXMFS         OREQ      'Y'
     C     EODSTY        OREQ      'DR'
     C     EODSTY        OREQ      'DS'
     C                   SETON                                        27
     C                   ELSE
     C                   SETOFF                                       27
     C                   ENDIF
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
     C*
     C     EODSTY        IFEQ      'DR'
     C     EODSTY        OREQ      'DS'
     C     XXMFS         OREQ      'Y'
     C                   SETON                                        37
     C                   ELSE
     C                   SETOFF                                       37
     C                   ENDIF
     C     CLTID         IFEQ      '114'
     C                   SETON                                        16
     C                   ELSE
     C                   SETOFF                                       16
     C                   ENDIF
     C*** Beg Add *** 06/21/12 ****************************************
      /Free
         If EDITST = 'HL';
           *in98 = *on;
         Else;
           *in98 = *off;
         Endif;
      /End-Free
     C*** End Add *** 06/21/12 ****************************************
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
     C** Update drg description for line '100' drg based on drg desc of header
     C*
     C                   MOVE      '100'         KKRVCD
     C                   MOVE      *BLANKS       KKRATE
     C*
     C     DUKY          CHAIN     PDU01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EUDGDS        DUDESC
     C                   UPDATE    PDU01
     C                   ENDIF
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
     C                   MOVEL(P)  SCFZIP        EOFLIC
     C*** Beg Add *** 07/18/25 ***************************************
     C                   MOVE      SCZIP4        EOFLIC
     C*** End Add *** 07/18/25 ***************************************
     C                   MOVEL     Q1USER        EOUSER
     C                   MOVEL     Q1PGM         EOPGM
     C                   Z-ADD     DSTIME        EOTIME
     C                   MOVEL     SRDTO         EODT
     C     *IN29         IFEQ      *OFF
     C                   MOVE      XXRGN#        EORGN#
     C                   ENDIF
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
     C                   Z-ADD     EOCSDT        EUSBDT
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
     C                   MOVEL(P)  SCFZIP        EOFLIC
     C*** Beg Add *** 07/18/25 ***************************************
     C                   MOVE      SCZIP4        EOFLIC
     C*** End Add *** 07/18/25 ***************************************
     C                   MOVEL     Q1USER        EOUSER
     C                   MOVEL     Q1PGM         EOPGM
     C                   Z-ADD     DSTIME        EOTIME
     C                   MOVEL     SRDTO         EODT
     C     *IN29         IFEQ      *OFF
     C                   MOVE      XXRGN#        EORGN#
     C                   ENDIF
     C                   UPDATE    PEO01
     C                   ENDIF
     C*
     C                   FEOD      FAEUP
     C*
     C                   MOVE      '1'           OUTFLG
     C                   ENDSR
