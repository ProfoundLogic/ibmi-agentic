     Hcopyright('(c) 2025 Sapiens, Inc. All rights reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H*  Program: F0351RFL                                       copy *
     H*  Description: Physician (HCFA) Header Info update/display     *
     H*               FLORIDA VERSION                                 *
     H*  Created: 04/05/99  C:56 Project: MC152                       *
     H*****************************************************************
     H*                                                               *
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
     H*  09/01/16  C:212 FL091 Phase 2                                *
     H*                  Renamed Dia1/2/3/4 to A/B/C/D                *
     H*                  Added Dia E-L and related changes            *
     H*                                                               *
     H*  01/15/16  C:079 114/39337                                    *
     H*                  Only process Lic # PPO if client 114         *
     H*                                                               *
     H*  10/01/15  C:079 PHW37605                                     *
     H*                  Check ICD-10 Indicator based on date and     *
     H*                  only in add mod                              *
     H*                                                               *
     H*  06/03/15 C:079 PROJECT: MC329 Phase III                      *
     H*                 Validate specific indicator for ICD10 codes   *
     H*                                                               *
     H*  09/08/14 C:280 PROJECT: 147MDC                               *
     H*                 Add new field dispecific and validate         *
     H*                                                               *
     H*  08/05/13 C:277 PROJECT: MC329 Phase II                       *
     H*                 Add search functionality and Validate ICD10   *
     H*                                                               *
     H*  05/23/13 C:277 PROJECT: MC329                                *
     H*                 Activate the ICD10 fields if ICD10 is active  *
     H*                                                               *
     H*  04/30/13 C:284 Project: 147MAS                               *
     H*                 Call new API FA782R.                          *
     H*                                                               *
     H*  04/17/13 C:284 Project: 114MID                               *
     H*                 Include rendering information for CSI.        *
     H*                                                               *
     H*  11/19/12 C:287 Project: MC367                                *
     H*                 Disable function code F11 for GS access to Bill Entry
     H*                                                               *
     H*  11/02/11 C:280 PROJECT 114MAV                                *
     H*                 Disable "Correct Coding" information window   *
     H*                                                               *
     H*  09/08/11 C:062 Support: MC114 77880                          *
     H*                 Don't call CC screen if F3 hit.               *
     H*                                                               *
     H*  06/16/11 C:061 PROJECT 114MZP                                *
     H*                 Display "Correct Coding" information window   *
     H*                 at program end as requested clt114.           *
     H*                                                               *
     H*   04/05/11 C:257 Support: 71931                               *
     H*              Assigning RRN to @@RRN after WRITEDBRCDF to      *
     H*              resolve duplicate records in file FAEHP.         *
     H*                                                               *
     H*   07/12/10 C:079 Support: 73533                               *
     H*              Do not validate license prefix in this program   *
     H*                                                               *
     H*   03/11/10 C:227 PROJECT: FL083                               *
     H*              220 Added Service date in parameter of API.      *
     H*                                                               *
     H*   10/03/08 C:180 SUPPORT 65159                                *
     H*                  Fourth position of FL Phy lic can be blank   *
     F*                                                               *
     H*   03/20/08 C:198                                              *
     H*                  Project: FS297                               *
     H*                  MDS to reject selected prefixes for specialt-*
     H*                  -ies not associate with WC - Scope change    *
     H*                                                               *
     H* 12/10/07 C:198 Project: FL064                                 *
     H*                Expanded license# field to 13 characters.      *
     H*                                                               *
     F*  11/27/07  C:047  PROJECT: MC295                              *
     F*                   PROGRAM NEW INFORMATION NEEDED FOR CMS1500. *
     F*                   ADD FLDS: SERVICING & REFERRING NPI ON SCR02*
     H*                                                              *
     H*  08/28/07  C:180  SUPP 59201                                  *
     H*                   Validate SCLIC# based on following formats: *
     H*                                                               *
     H*                   AANNNNNNNNNNN                               *
     H*                   (OR)                                        *
     H*                   AAANNNNNNNNNN                               *
     H*                   (OR)                                        *
     H*                   AAAANNNNNNNNN                               *
     H*                                                               *
     H*                   Left justify and space fill to end of field *
     H*                   Key alpha prefix and numeric digits of      *
     H*                   license number                              *
     H*                   Must be valid value                         *
     H*                   DO NOT zero pad numeric portion             *
     H*                                                               *
     H*  04/20/07 C:188 PROJECT:FL061                                 *
     H*                 SET EDITST TO ADJ OR POLICY STATE BASED ON EOCPST
     H*                                                               *
     H* 03/21/07  C:182  Project: FS297                              *
     H*                  Set EDI flag = N if license# is invali      *
     H*                                                              *
     H* 03/14/07  C:182  Project: FL067                              *
     H*                  1. Changed Fld Heading for 'Payment plan'   *
     H*                     to be 'Payment code'.                    *
     H*                                                              *
     H*                                                               *
     H* 02/27/07 C:182 PROJECT: FS294                                *
     H*                ADDED SEARCH OPTION TO FIELD SCTAPE           *
     H*                                                               *
     H*  11/08/05 C:163 PROJECT: 48MWK                                *
     H*                 FL EDI CORRECTION LICENSE #                   *
     H*                                                               *
     H*  03/25/05 C:079 PROJECT: FL050                                *
     H*                 1. CHANGE EDIT ON LICENSE NUMBER              *
     H*                                                               *
     H*  04/15/04 C:079 PROJECT: FL046                                *
     H*                 1. CHANGE EDIT ON ELIGIBLE FOR DATA SUBMISSION*
     H*                 2. ADD DESCRIPTION FIELD                      *
     H*                                                               *
     H*  03/19/04 C:079 SUPPORT: MC044 41821                          *
     H*                 REMOVE CONDITION OF WRITING EOFLIC TO SERV ZIP*
     H*                 CHANGE CONDITION OF WRITING EOTAPE TO SCEDI   *
     H*                                                               *
     H*  12/12/03 C:079 PROJECT: FL048                                *
     H*                 REQUIRE SERVICE ZIP. INITIALIZE TO 0          *
     H*                                                               *
     H*  04/28/03 C:079 PROJECT: FL043                                *
     H*                 EXPAND LIC# FIELD TO 13A                      *
     H*                                                               *
     H*  03/12/03 C:079 SUPPORT: MC044 36319                          *
     H*                 CHECK THE STATUS OF DIAG CODES                *
     H*                                                               *
     H*  12/09/01 C:079 PROJECT: FL040                                *
     H*                 Allow 4th digit of Lic# to be alpha           *
     H*                                                               *
     H*  10/01/01 C:079 SUPPORT: MC114 28975                          *
     H*                 DO NOT REQUIRE SERVICE ZIP CODE               *
     H*                                                               *
     H*  07/06/01 C:079 PROJECT: FSMNT #196                           *
     H*                 1. ADD EDIT TO SPECIALTY CODE                 *
     H*                 2. CORRECT CONFLICTING INDICATOR ON SCLIC#    *
     H*                                                               *
     H*  05/30/01 C:062 PROJECT: FS229                                *
     H*                 POPULATE EOMB# SCREEN FIELD                   *
     H*                                                               *
     H*  05/25/01 C:062 PROJECT: FS228                                *
     H*                 ADD SERVICE ZIP CODE                          *
     H*                                                               *
     H*  05/09/01  C:079  SUPPORT: MC044 27425                        *
     H*                   CHANGE EDIT ON SCLIC# TO ALLOW 3RD POSITION *
     H*                   TO BE ALPHA OR NUMERIC                      *
     H*                                                               *
     H*  03/30/01  C:062  PROJECT: FL036                              *
     H*                   ADDED LOGIC FOR ERROR WINDOW FOR LICENSE #. *
     H*                                                               *
     H*  02/01/00  C:117  PROJECT: FLMNT  SUPP SEQ#: 25597            *
     H*                   NEED TO CORRECT WHERE SCTAPE IS BEING PULLED*
     H*                   FROM AND WHAT IS UPDATING IT.               *
     H*                                                               *
     H*  11/22/00  C:062  Proj: FS212                                 *
     H*                   COMPILE ONLY.                               *
     H*                                                               *
     H*  10/13/00  C:062  Support MC238                               *
     H*                   ADD LOGIC TO DEAL WITH NON-SEARCHABLE FIELDS.
     H*                                                               *
     H*  09/20/00  C:117  PROJECT:FL031                               *
     H*                   CHANGEING THE EDITTING FOR SCLIC#;          *
     H*                   ADDING SEARCH TO SAME FIELD.                *
     H*                                                               *
     H*  03/08/00  C:062  PROJ: MCMNT                                 *
     H*                   PASS "D" LOS TYPE TO F2010R FOR DIAGNOSIS SCH
     H*                   SO THAT F2010R WILL SUBSET ON "D" LOS TYPES.*
     H*                   PASS "O" LOS TYPE TO F2010R FOR ICD9 SEARCH *
     H*                   SO THAT F2010R WILL SUBSET ON "O" LOS TYPES.*
     H*                                                               *
     H*  02/21/00  C:117  PROJ: FS208                                 *
     H*                   ADDED LOGIC TO PROTECT SPECIALTY IF ON A    *
     H*                   RECON; ALSO SAVES SPECIALTY.                *
     H*                                                               *
     H*  01/10/00 C:79  Project: MC048                                *
     H*                 Sup Seq: 21276                                *
     H*                 If tape flag=N don't edit FL Lic#             *
     H*                                                               *
     H*  12/13/99 C:117 Project: FLMNT                                *
     H*                 Sup Seq: 20979                                *
     H*                 Put cond around FL Pay Plan validation        *
     H*                                                               *
     H*  11/12/99 C:79  MC152                                         *
     H*                 Delete/Change efficiency isssue.              *
     H*                                                               *
     H*  10/21/99 C:117 Project: FLMNT                                *
     H*                 Sup Seq: 20175                                *
     H*                 Added error checking for last 7 digits of FL  *
     H*                 Lic#; they must all be numeric.               *
     H*                                                               *
     H*  09/27/99  C:117 PROJ: MC152                                  *
     H*                  Making corrections to the editing of FL      *
     H*                  Payment Plan field                           *
     H*                                                               *
     H*  09/17/99 C:56  Project: MC152                                *
     H*                 Condition requiring Medicare(License #) based *
     H*                 on state edi flag (STSTP/ST1A4).              *
     H*                                                               *
     H*  09/10/99 C:56  Project: KY010                                *
     H*                 Write SCEDI to EOTAPE                         *
     H*                                                               *
     H*  08/16/99 C:56 Project: MC152                                 *
     H*             1. Add default of payment plan to subroutine      *
     H*                PGMSTR.                                        *
     H*                                                               *
     H*  06/01/99 C:56 Project: MC152                                 *
     H*                Add Master License number to screen            *
     H*****************************************************************
     H*  INDICATORS - SCR01
     H*
     H*  09-    INQUIRY MODE
     H*  24-    DATA SUBMISSION PROTECT IF SENT
     H*  30-    ELIGIBLE FOR DATA SUBMISSION (ALL SCREENS)
     H*  37-    RECON (ALL SCREENS)
     H*  40-    ADD MODE
     H*  60-    TESTN INDICATOR
     H*  71-89  ERROR
     H*  90-    COPYBOOK ERROR
     H*  97-    INVALID SEARCH FIELD
     H*  98-99  GENERAL CHAIN
     H*** Beg Add *** 04/17/13 ****************************************
     H*
     H*  INDICATORS - SCR02
     H*
     H*  09-    INQUIRY MODE
     H*** End Add *** 04/17/13 ****************************************
     H*****************************************************************
     FF0351FFL  CF   E             WORKSTN
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F*
     F*  Physical file definition (for maintaining record)
     FFAEHP     UF A E             DISK
     F                                     RENAME(PEH01:DBRCDF)
     F*
     F*  EOMB Transaction File
     FFAEOP     UF   E             DISK
     F*
     F*  WCMC Control File
     FFMCXL     IF   E           K DISK
     F*
     F*  Diagnosis Code Physical File
     FFMDIL     IF   E           K DISK
     F*  Geographic Zip Area File
     FFMGZL     IF   E           K DISK
     F*  Provider Auxillary File
     FFMP1L     IF   E           K DISK
     F*
     F*  State Code Table File
     FSTSTL     IF   E           K DISK
     F*
     F*  Generic Table File
     FFTABL     IF   E           K DISK
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
     FFAEHL     IF   E           K DISK
     F                                     INFDS(DBFDSA)
     F*
     F*  Generic Table FIle
     FFTGTL     IF   E           K DISK
     FFTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PGT02)
     FFMPLLFL   IF   E           K DISK
      *
      *  EOMB STATUS TRANSACTION
     FFAESL     IF   E           K DISK
      *
      *  WCMC MED BILL AUTH AUX
     FFAAUL1    IF   E           K DISK
     F*** Beg Add *** 08/05/13 ***************************************
     Ffmicd10l1 if   e           k disk
     F*** End Add *** 08/05/13 ***************************************
      /COPY SCOPYMT01D
     d*** Beg Add *** 06/27/19 ***************************************
      /copy sprmodule
     d isPbillsLite    S               n
     d*** End Add *** 06/27/19 ***************************************
     D @A              S              1    DIM(26) CTDATA PERRCD(26)
     D @N              S              1    DIM(10) CTDATA PERRCD(10)
     D LL              S              1    DIM(11)
     D*
     D*
     D DBFDSA          DS
     D  DBRRNA               397    400B 0
     D**LICENSE*NUMBER DATA STRUCTURE
     D*
     D* LICENSE NUMBER DATA STRUCTURE
     D*
     D                 DS
     D  SCLIC#                 1     13
     D  SCLIC1                 1      1
     D  SCLIC2                 2      2
     D  SCLIC3                 3      3
     D  SCLIC4                 4      4
     D*
     D  SCLICA                 3     13
     D  SCLICB                 4     13
     D  SCLICC                 5     13
     D ALPHA           C                   CONST('ABCDEFGHIJKLMNOPQRST-
     D                                     UVWXYZ')
     D*
     D ALL             C                   CONST('ABCDEFGHIJKLMNOPQRST-
     D                                     UVWXYZ0123456789 ')
     D*
     D NUM09B          C                   CONST('0123456789 ')
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Physician (HCFA) Hea-
     D                                     der Information')
     D COMAPI          C                   CONST('FA007R')
     D*** Beg Del ************ 04/17/13 ***********************************
     D*** #OFSCR          C                   CONST(1)
     D*** End Del ************ 04/17/13 ***********************************
     D ZZLIC#          C                   CONST('ZZ99999999999')
     D XXLIC#          C                   CONST('XX99999999999')

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
     C                   PARM                    @@RRN             9 0          for FAEHP
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
      *
      *  EOMB STATUS TRANSACTION
      *
     C     ESKY          KLIST
     C                   KFLD                    EHCO#
     C                   KFLD                    EHSUB#
     C                   KFLD                    EHFND
     C                   KFLD                    EHFYR
     C                   KFLD                    EHCASE
     C                   KFLD                    EHEOB#
      *
      *  WCMC MED BILL AUTH AUX
      *
     C     AUKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOIDOC
      *
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
     C* Key List for Geographic Zip Area File
     C*
     C     GZKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKZIPA
     C*
     C*  Key List for Provider Auxillary File
     C*
     C     P1KY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
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
     C     EHKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C** KEY LIST FOR GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*
     C* FL Provider Lic#
     C*
     C     PLKY          KLIST
     C                   KFLD                    KKLIC#           13
     C*** Beg Add *** 08/05/13 ***************************************
     C*** KEY LIST FOR DIAGNOSIS CODE MASTER FILE
     C     fmicd10l1key  klist
     C                   kfld                    kkicd10
     C                   kfld                    kklost
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
     C     *LIKE         DEFINE    EHCO#         KKCO#
     C     *LIKE         DEFINE    EHSUB#        KKSUB#
     C     *LIKE         DEFINE    EHFND         KKFND
     C     *LIKE         DEFINE    EHFYR         KKFYR
     C     *LIKE         DEFINE    EHCASE        KKCASE
     C     *LIKE         DEFINE    BEAST         KKAST
     C     *LIKE         DEFINE    BESYS         KKSYS
     C     *LIKE         DEFINE    BEBTYP        KKBTYP
     C     *LIKE         DEFINE    BESTYP        KKSTYP
     C     *LIKE         DEFINE    PDFEIN        KKFEIN
     C     *LIKE         DEFINE    PDMOD         KKMOD
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C     *LIKE         DEFINE    EOAST         EDITST
     C     *LIKE         DEFINE    EOFRDT        @@SDTE
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
     C                   Z-ADD     SCDFC         SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDFC
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
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
    C*** Beg Add *** 06/27/19 ****************************************
     C                   if        not isPbillsLite
    C*** End Add *** 06/27/19 ****************************************
    C***** Beg Add *** 09/01/16 **************************************
     C** initialize indicators
     C                   Eval      erDiaE = *off
     C                   Eval      erDiaF = *off
     C                   Eval      erDiaG = *off
     C                   Eval      erDiaH = *off
     C                   Eval      erDiaI = *off
     C                   Eval      erDiaJ = *off
     C                   Eval      erDiaK = *off
     C                   Eval      erDiaL = *off
     C                   Eval      MsDiaE = *blanks
     C                   Eval      MsDiaF = *blanks
     C                   Eval      MsDiaG = *blanks
     C                   Eval      MsDiaH = *blanks
     C                   Eval      MsDiaI = *blanks
     C                   Eval      MsDiaJ = *blanks
     C                   Eval      MsDiaK = *blanks
     C                   Eval      MsDiaL = *blanks

     C                   Eval      erRSubCd = *off
     C                   Eval      MsRSubCd = *blanks
    C***** End Add *** 09/01/16 **************************************
     C*
     C                   SETOFF                                       80
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
     C                   SETON                                        9083
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  SERVICE ZIP CODE CANNOT BE ZERO
     C*
     C     SCSVZP        IFEQ      *ZEROS
     C     *IN29         ANDEQ     *OFF
     C                   SETON                                        8990
     C                   GOTO      CK1END
     C                   ENDIF
     C*** Beg Add *** 07/18/25 ***************************************
       If isModEnabled('NCCI Med Data Call');
          If scsvzp = *zeros and EOrcn = 0;
             *in89 = *on;
             *in90 = *on;
          Endif;
       Endif;
     C*** End Add *** 07/18/25 ***************************************
     C*
     C** VALIDATE DATE FIRST CONSULTED
     C*
     C     SCDFC         IFNE      *ZEROS
     C                   Z-ADD     SCDFC         SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9071
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     SRDTO         SCDFC
     C                   ENDIF                                                  SRERR IFEQ 1
     C                   ENDIF                                                  SCDFC IFNE ZEROS
     C*
     C*** DIAGNOSIS CODE ONE IS REQUIRED
     C*
     C     SCDIA1        IFEQ      *BLANKS
     C                   SETON                                        9072
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCDIA1 IFEQ BLANKS
     C*
     C** VALIDATE 1ST DIAGNOSIS CODE
     C*
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
       // Beg Add ***** 06/03/15 *******************************************
              *in91 = *off;
           if scdia1 <> *blanks
              and ICvld = '0';
              *in90 = *on;
              *in91 = *on;
              leavesr;
           endif;
       // End Add ***** 06/03/15 *******************************************

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
     C                   SETON                                        9073
     C                   GOTO      CK1END
     C                   ENDIF
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C*
     C*** Beg Add *** 09/08/14 ****************************************
     C*
      /Free

              *in91 = *off;
           if scdia1 <> *blanks
       // Beg Add ***** 06/03/15 *******************************************
              and scICdInd <> 'Y'
       // End Add ***** 06/03/15 *******************************************
              and dispecific = '0';
              *in90 = *on;
              *in91 = *on;
              leavesr;
           endif;

      /End-Free
     C*
     C*** Beg Add *** 09/08/14 ****************************************
     C*
     C*
     C** VALIDATE DIAGNOSIS CODE #2 IF ENTERED
     C*
     C     SCDIA2        IFNE      *BLANKS
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxicd10 = '1' and scICdInd ='Y';
          kkICd10 = scDIa2;
          Chain fmicd10l1key pic01;
             If not %found(fmicd10l1)
                or ICstat = 'I';
                *in90 = *on;
                *in74 = *on;
                leavesr;
             Endif;
       // Beg Add ***** 06/03/15 *******************************************
              *in92 = *off;
           if scdia2 <> *blanks
              and ICvld = '0';
              *in90 = *on;
              *in92 = *on;
              leavesr;
           endif;
       // End Add ***** 06/03/15 *******************************************
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   SCDIA2        KKICD9
     C                   MOVEL(P)  SCDIA2        KKICD9
     C*** End Chg *** 05/23/13 ****************************************
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        9074
     C                   GOTO      CK1END
     C                   ENDIF
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*
     C*** Beg Add *** 09/08/14 ****************************************
     C*
      /Free

              *in92 = *off;
           if scdia2 <> *blanks
       // Beg Add ***** 06/03/15 *******************************************
              and scICdInd <> 'Y'
       // End Add ***** 06/03/15 *******************************************
              and dispecific = '0';
              *in90 = *on;
              *in92 = *on;
              leavesr;
           endif;

      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   ENDIF                                                  SCDIA2 IFNE BLANKS
     C*
     C** VALIDATE DIAGNOSIS CODE #3 IF ENTERED
     C*
     C     SCDIA3        IFNE      *BLANKS
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxicd10 = '1' and scICdInd ='Y';
          kkICd10 = scDIa3;
          Chain fmicd10l1key pic01;
             If not %found(fmicd10l1)
                or ICstat = 'I';
                *in90 = *on;
                *in75 = *on;
                leavesr;
             Endif;
       // Beg Add ***** 06/03/15 *******************************************
              *in93 = *off;
           if scdia3 <> *blanks
              and ICvld = '0';
              *in90 = *on;
              *in93 = *on;
              leavesr;
           endif;
       // End Add ***** 06/03/15 *******************************************
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   SCDIA3        KKICD9
     C                   MOVEL(P)  SCDIA3        KKICD9
     C*** End Chg *** 05/23/13 ****************************************
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        9075
     C                   GOTO      CK1END
     C                   ENDIF
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*
     C*** Beg Add *** 09/08/14 ****************************************
     C*
      /Free

              *in93 = *off;
           if scdia3 <> *blanks
       // Beg Add ***** 06/03/15 *******************************************
              and scICdInd <> 'Y'
       // End Add ***** 06/03/15 *******************************************
              and dispecific = '0';
              *in90 = *on;
              *in93 = *on;
              leavesr;
           endif;

      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   ENDIF                                                  SCDIA2 IFNE BLANKS
     C*
     C** VALIDATE DIAGNOSIS CODE #4 IF ENTERED
     C*
     C     SCDIA4        IFNE      *BLANKS
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxicd10 = '1' and scICdInd ='Y';
          kkICd10 = scDIa4;
          Chain fmicd10l1key pic01;
             If not %found(fmicd10l1)
                or ICstat = 'I';
                *in90 = *on;
                *in76 = *on;
                leavesr;
             Endif;
       // Beg Add ***** 06/03/15 *******************************************
              *in94 = *off;
           if scdia4 <> *blanks
              and ICvld = '0';
              *in90 = *on;
              *in94 = *on;
              leavesr;
           endif;
       // End Add ***** 06/03/15 *******************************************
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   SCDIA4        KKICD9
     C                   MOVEL(P)  SCDIA4        KKICD9
     C*** End Chg *** 05/23/13 ****************************************
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        9076
     C                   GOTO      CK1END
     C                   ENDIF
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*
     C*** Beg Add *** 09/08/14 ****************************************
     C*
      /Free

              *in94 = *off;
           if scdia4 <> *blanks
       // Beg Add ***** 06/03/15 *******************************************
              and scICdInd <> 'Y'
       // End Add ***** 06/03/15 *******************************************
              and dispecific = '0';
              *in90 = *on;
              *in94 = *on;
              leavesr;
           endif;

      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   ENDIF                                                  SCDIA2 IFNE BLANKS
     C*
    C***** Beg Add *** 09/01/16 **************************************
     C*
     C** VALIDATE DIAGNOSIS CODE #5 IF ENTERED
     C*
     C     SCDIAE        IFNE      *BLANKS
      /Free
        If xxicd10 = '1' and scICdInd ='Y';
           kkICd10 = scDIaE;
           Chain fmicd10l1key pic01;
            If not %found(fmicd10l1)
               or ICstat = 'I';
               *in90 = *on;
               erDiaE = *on;
               MsDiaE = 'MCR2383';
               leavesr;
            Endif;

           if scdiaE <> *blanks
              and ICvld = '0';
              *in90 = *on;
              erDiaE = *on;
              MsDiaE = 'MC50006';
              leavesr;
           endif;
        Else;
      /End-Free
     C                   MOVEL(P)  SCDIAE        KKICD9
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   EVAL      erDiaE = *on
     C                   EVAL      MsDiaE = 'MCR2383'
     C                   SETON                                        90
     C                   GOTO      CK1END
     C                   ENDIF
      /Free
        Endif;
      /End-Free
     C*
      /Free
        if scdiaE <> *blanks
           and scICdInd <> 'Y'
           and dispecific = '0';
           *in90 = *on;
           erDiaE = *on;
           MsDiaE = 'MC50006';
           leavesr;
        endif;

      /End-Free
     C                   ENDIF
     C*
     C*
     C** VALIDATE DIAGNOSIS CODE #6 IF ENTERED
     C*
     C     SCDIAF        IFNE      *BLANKS
      /Free
        If xxicd10 = '1' and scICdInd ='Y';
           kkICd10 = scDIaF;
           Chain fmicd10l1key pic01;
            If not %found(fmicd10l1)
               or ICstat = 'I';
               *in90 = *on;
               erDiaF = *on;
               MsDiaF = 'MCR2383';
               leavesr;
            Endif;

           if scdiaF <> *blanks
              and ICvld = '0';
              *in90 = *on;
              erDiaF = *on;
              MsDiaF = 'MC50006';
              leavesr;
           endif;
        Else;
      /End-Free
     C                   MOVEL(P)  SCDIAF        KKICD9
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   EVAL      erDiaF = *on
     C                   EVAL      MsDiaF = 'MCR2383'
     C                   SETON                                        90
     C                   GOTO      CK1END
     C                   ENDIF
      /Free
        Endif;
      /End-Free
     C*
      /Free
        if scdiaF <> *blanks
           and scICdInd <> 'Y'
           and dispecific = '0';
           *in90 = *on;
           erDiaF = *on;
           MsDiaF = 'MC50006';
           leavesr;
        endif;

      /End-Free
     C                   ENDIF
     C*
     C*
     C** VALIDATE DIAGNOSIS CODE #7 IF ENTERED
     C*
     C     SCDIAG        IFNE      *BLANKS
      /Free
        If xxicd10 = '1' and scICdInd ='Y';
           kkICd10 = scDIaG;
           Chain fmicd10l1key pic01;
            If not %found(fmicd10l1)
               or ICstat = 'I';
               *in90 = *on;
               erDiaG = *on;
               MsDiaG = 'MCR2383';
               leavesr;
            Endif;

           if scdiaG <> *blanks
              and ICvld = '0';
              *in90 = *on;
              erDiaG = *on;
              MsDiaG = 'MC50006';
              leavesr;
           endif;
        Else;
      /End-Free
     C                   MOVEL(P)  SCDIAG        KKICD9
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   EVAL      erDiaG = *on
     C                   EVAL      MsDiaG = 'MCR2383'
     C                   SETON                                        90
     C                   GOTO      CK1END
     C                   ENDIF
      /Free
        Endif;
      /End-Free
     C*
      /Free
        if scdiaG <> *blanks
           and scICdInd <> 'Y'
           and dispecific = '0';
           *in90 = *on;
           erDiaG = *on;
           MsDiaG = 'MC50006';
           leavesr;
        endif;

      /End-Free
     C                   ENDIF
     C*
     C*
     C** VALIDATE DIAGNOSIS CODE #8 IF ENTERED
     C*
     C     SCDIAH        IFNE      *BLANKS
      /Free
        If xxicd10 = '1' and scICdInd ='Y';
           kkICd10 = scDIaH;
           Chain fmicd10l1key pic01;
            If not %found(fmicd10l1)
               or ICstat = 'I';
               *in90 = *on;
               erDiaH = *on;
               MsDiaH = 'MCR2383';
               leavesr;
            Endif;

           if scdiaH <> *blanks
              and ICvld = '0';
              *in90 = *on;
              erDiaH = *on;
              MsDiaH = 'MC50006';
              leavesr;
           endif;
        Else;
      /End-Free
     C                   MOVEL(P)  SCDIAH        KKICD9
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   EVAL      erDiaH = *on
     C                   EVAL      MsDiaH = 'MCR2383'
     C                   SETON                                        90
     C                   GOTO      CK1END
     C                   ENDIF
      /Free
        Endif;
      /End-Free
     C*
      /Free
        if scdiaH <> *blanks
           and scICdInd <> 'Y'
           and dispecific = '0';
           *in90 = *on;
           erDiaH = *on;
           MsDiaH = 'MC50006';
           leavesr;
        endif;

      /End-Free
     C                   ENDIF
     C*
     C*
     C** VALIDATE DIAGNOSIS CODE #9 IF ENTERED
     C*
     C     SCDIAI        IFNE      *BLANKS
      /Free
        If xxicd10 = '1' and scICdInd ='Y';
           kkICd10 = scDIaI;
           Chain fmicd10l1key pic01;
            If not %found(fmicd10l1)
               or ICstat = 'I';
               *in90 = *on;
               erDiaI = *on;
               MsDiaI = 'MCR2383';
               leavesr;
            Endif;

           if scdiaI <> *blanks
              and ICvld = '0';
              *in90 = *on;
              erDiaI = *on;
              MsDiaI = 'MC50006';
              leavesr;
           endif;
        Else;
      /End-Free
     C                   MOVEL(P)  SCDIAI        KKICD9
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   EVAL      erDiaI = *on
     C                   EVAL      MsDiaI = 'MCR2383'
     C                   SETON                                        90
     C                   GOTO      CK1END
     C                   ENDIF
      /Free
        Endif;
      /End-Free
     C*
      /Free
        if scdiaI <> *blanks
           and scICdInd <> 'Y'
           and dispecific = '0';
           *in90 = *on;
           erDiaI = *on;
           MsDiaI = 'MC50006';
           leavesr;
        endif;

      /End-Free
     C                   ENDIF
     C*
     C*
     C** VALIDATE DIAGNOSIS CODE #10 IF ENTERED
     C*
     C     SCDIAJ        IFNE      *BLANKS
      /Free
        If xxicd10 = '1' and scICdInd ='Y';
           kkICd10 = scDIaJ;
           Chain fmicd10l1key pic01;
            If not %found(fmicd10l1)
               or ICstat = 'I';
               *in90 = *on;
               erDiaJ = *on;
               MsDiaJ = 'MCR2383';
               leavesr;
            Endif;

           if scdiaJ <> *blanks
              and ICvld = '0';
              *in90 = *on;
              erDiaJ = *on;
              MsDiaJ = 'MC50006';
              leavesr;
           endif;
        Else;
      /End-Free
     C                   MOVEL(P)  SCDIAJ        KKICD9
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   EVAL      erDiaJ = *on
     C                   EVAL      MsDiaJ = 'MCR2383'
     C                   SETON                                        90
     C                   GOTO      CK1END
     C                   ENDIF
      /Free
        Endif;
      /End-Free
     C*
      /Free
        if scdiaJ <> *blanks
           and scICdInd <> 'Y'
           and dispecific = '0';
           *in90 = *on;
           erDiaJ = *on;
           MsDiaJ = 'MC50006';
           leavesr;
        endif;

      /End-Free
     C                   ENDIF
     C*
     C*
     C** VALIDATE DIAGNOSIS CODE #11 IF ENTERED
     C*
     C     SCDIAK        IFNE      *BLANKS
      /Free
        If xxicd10 = '1' and scICdInd ='Y';
           kkICd10 = scDIaK;
           Chain fmicd10l1key pic01;
            If not %found(fmicd10l1)
               or ICstat = 'I';
               *in90 = *on;
               erDiaK = *on;
               MsDiaK = 'MCR2383';
               leavesr;
            Endif;

           if scdiaK <> *blanks
              and ICvld = '0';
              *in90 = *on;
              erDiaK = *on;
              MsDiaK = 'MC50006';
              leavesr;
           endif;
        Else;
      /End-Free
     C                   MOVEL(P)  SCDIAK        KKICD9
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   EVAL      erDiaK = *on
     C                   EVAL      MsDiaK = 'MCR2383'
     C                   SETON                                        90
     C                   GOTO      CK1END
     C                   ENDIF
      /Free
        Endif;
      /End-Free
     C*
      /Free
        if scdiaK <> *blanks
           and scICdInd <> 'Y'
           and dispecific = '0';
           *in90 = *on;
           erDiaK = *on;
           MsDiaK = 'MC50006';
           leavesr;
        endif;

      /End-Free
     C                   ENDIF
     C*
     C*
     C** VALIDATE DIAGNOSIS CODE #12 IF ENTERED
     C*
     C     SCDIAL        IFNE      *BLANKS
      /Free
        If xxicd10 = '1' and scICdInd ='Y';
           kkICd10 = scDIaL;
           Chain fmicd10l1key pic01;
            If not %found(fmicd10l1)
               or ICstat = 'I';
               *in90 = *on;
               erDiaL = *on;
               MsDiaL = 'MCR2383';
               leavesr;
            Endif;

           if scdiaL <> *blanks
              and ICvld = '0';
              *in90 = *on;
              erDiaL = *on;
              MsDiaL = 'MC50006';
              leavesr;
           endif;
        Else;
      /End-Free
     C                   MOVEL(P)  SCDIAL        KKICD9
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   EVAL      erDiaL = *on
     C                   EVAL      MsDiaL = 'MCR2383'
     C                   SETON                                        90
     C                   GOTO      CK1END
     C                   ENDIF
      /Free
        Endif;
      /End-Free
     C*
      /Free
        if scdiaL <> *blanks
           and scICdInd <> 'Y'
           and dispecific = '0';
           *in90 = *on;
           erDiaL = *on;
           MsDiaL = 'MC50006';
           leavesr;
        endif;

      /End-Free
     C                   ENDIF
     C*

    C***** End Add *** 09/01/16 **************************************
     C***** Beg Add *** 09/01/16 **************************************
      /Free

       // Resubmission Reason Code is required
        If EOcntl <> *blanks and EOrcn > 0;
           If scRSubCd = *blanks;
              *in90 = *on;
              erRSubCd = *on;
              MsRSubCd = 'MCR0503';
              leavesr;
           EndIf;
        EndIf;

       // Resubmission Reason Code should be valid
        If scRSubCd <> *blanks;
           kktbid = 'FLRC';
           kkkey = scRSubCd;
           Chain(n) gtky pgt01;
           If not %found(ftgtl) or
              (%found(ftgtl) and GTstat = 'I');
              *in90 = *on;
              erRSubCd = *on;
              MsRSubCd = 'MCR2383';
              leavesr;
           EndIf;
        EndIf;

      /End-Free
    C***** End Add *** 09/01/16 **************************************

     C* MEDICARE NUMBER IS REQUIRED
     C*
     C     SCEDI         IFNE      'N'
     C     SCEDI         ANDNE     *BLANKS
     C     SCLIC#        IFEQ      *BLANKS
     C                   SETON                                        9077
     C                   GOTO      CK1END
     C                   ELSE
     C     SCLIC1        IFEQ      'X'
     C     SCLIC2        ANDEQ     'X'
     C     SCLIC#        ANDNE     XXLIC#
     C                   SETON                                        9082
     C                   GOTO      CK1END
     C                   ENDIF
     C     SCLIC1        IFEQ      'Z'
     C     SCLIC2        ANDEQ     'Z'
     C     SCLIC#        ANDNE     ZZLIC#
     C                   SETON                                        9084
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF                                                  SCEDI = *BLANKS
     C*
     C** ELIGIBLE FOR DATA SUBMISSION FEILD IS REQUIRED IF IN30 IS ON
     C*          (DETERMINED IN PGMSTR SUBROUINTE)
     C*
     C   30SCEDI         IFEQ      *BLANKS
     C                   SETON                                        9078
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCEDI IFEQ BLANKS
     C*
     C* Validate if not protected (*in25 = *off)
     C*
     C     *IN25         IFEQ      *OFF
     C*
     C                   MOVEL(P)  'FLES'        KKTBID
     C                   MOVEL(P)  SCEDI         KKKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *ON
     C     GTSTAT        OREQ      'I'
     C*
     C                   SETON                                        9079
     C                   GOTO      CK1END
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  Check to see if the Provider DB installed
     C*
     C                   MOVEL     *BLANKS       ##KEY
     C                   MOVEL     'FL'          ##KEY
     C     STKY          CHAIN     PST01                              99
     C     *IN99         IFEQ      *OFF
     C     STBS01        ANDEQ     'Y'
     C     SCEDI         ANDNE     'N'
     C     SCEDI         ANDNE     *BLANKS
     C*
     C* edit against the Prov Lic# database
     C*
     C                   MOVEL     SCLIC#        KKLIC#
     C     PLKY          CHAIN     PPL01                              99
     C     *IN99         IFEQ      *ON
     C     EOFLG7        IFNE      'Y'
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
     C*
     C                   ELSE                                                   STBS01=Y
     C*
     C     SCLIC#        IFNE      *BLANKS
     C     SCEDI         ANDNE     'N'
     C     SCEDI         ANDNE     *BLANKS
     C                   MOVE      *ZEROS        LCNT              5 0
     C*
     C* FIRST TWO CHARACTERS OF LICENSE NUMBER MUST BE ALPHA
     C*
     C     SCLIC1        LOOKUP    @A                                     60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        9080
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C                   ADD       1             LCNT
      *
     C     SCLIC2        LOOKUP    @A                                     60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        9080
     C                   GOTO      CK1END
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
     C     SCLIC3        LOOKUP    @N                                     40
     C     *IN40         IFEQ      *OFF
     C                   SETON                                        9080
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
      *
     C*
     C* 4TH position can be alpha or numeric or blanks
     C*
     C     SCLIC4        IFGT      *BLANKS
      *
     C     SCLIC4        LOOKUP    @A                                     60
     C     *IN60         IFEQ      *ON
      *
     C     LCNT          IFEQ      3
     C                   ADD       1             LCNT              5 0
     C                   ELSE
     C                   SETON                                        9080
     C                   GOTO      CK1END
     C                   ENDIF
      *
     C                   ELSE
     C     SCLIC4        LOOKUP    @N                                     40
     C     *IN40         IFEQ      *OFF
     C                   SETON                                        9080
     C                   GOTO      CK1END
     C                   ENDIF
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
     C                   SETON                                        9064
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
      *
     C                   ELSE
     C                   SETON                                        9064
     C                   ENDIF
      *
     C     *IN90         IFEQ      *ON
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C*
     C* FIRST TWO CHARACTERS OF LICENSE NUMBER MUST BE ALPHA
     C*
     C*
     C*
     C* 3rd position can be alpha or numeric
     C*
     C*
     C*
     C*
     C*
     C* 4TH position can be alpha or numeric
     C*
     C*
     C*
     C*
     C*
     C*
     C*
     C**  The last 7 digits MUST BE numeric
     C*
     C*
     C*
     C*
     C                   ENDIF                                                  SCLIC# <> *BLANKS
     C                   ENDIF                                                  STBS01=Y
     C*
     C* PAYMENT PLAN IS REQUIRED
     C*
     C  N37SCPPLN        IFEQ      *BLANKS
     C                   SETON                                        9088
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCPPLN = *BLANKS
     C*
     C*  VALIDATE PAYMENT PLAN
     C*
     C  N37SCPPLN        IFNE      *BLANKS
     C     SCEDI         IFEQ      'Y'
     C                   MOVEL(P)  'FLPP'        KKTBID
     C                   ELSE
     C                   MOVEL(P)  'FLPA'        KKTBID
     C                   ENDIF
     C                   MOVEL(P)  SCPPLN        KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9085
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C*
     C*  EMPLOYMENT RELATED IS REQUIRED
     C*
     C     SCEMPL        IFEQ      *BLANKS
     C                   SETON                                        9086
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCEMPL = *BLANKS
     C*
     C*  VALIDATE EMPLOYMENT RELATED FIELD
     C*
     C     SCEMPL        IFNE      'Y'
     C     SCEMPL        ANDNE     'N'
     C                   SETON                                        9087
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCEMPL NE Y
     C*
     C*
     C*  VALIDATE THE REVISION INDICATOR
     C*
     C     SCREVI        IFGE      'E'
     C*
     C     'ASC'         SCAN      SCLIC#        XXA1              2 0
     C     'HH'          SCAN      SCLIC#        XXA2              2 0
     C     'NH'          SCAN      SCLIC#        XXA3              2 0
     C*
     C     XXA1          IFNE      *ZEROS
     C     XXA2          ORNE      *ZEROS
     C     XXA3          ORNE      *ZEROS
     C                   SETON                                        9046
     C                   GOTO      CK1END
     C                   ENDIF                                                  ASC
     C                   ENDIF                                                  REVI
     C*
     C     SCREVI        IFEQ      *BLANK
     C                   SETON                                        9044
     C                   GOTO      CK1END
     C                   ELSE
     C*
     C                   MOVEL(P)  'REVI'        KKTBID
     C                   MOVEL(P)  SCREVI        KKKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *ON
     C     GTSTAT        OREQ      'I'
     C*
     C                   SETON                                        9045
     C                   GOTO      CK1END
     C*
     C                   ENDIF
     C                   ENDIF                                                  REVI
    C*** Beg Add *** 06/27/19 ****************************************
     C                   Endif
    C*** End Add *** 06/27/19 ****************************************
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
     C* KJDO - F10=SEARCH.
     C*================================================================
     C     KJDO          BEGSR
     C*
     C*** INITIALIZE POSITION CURSOR INDICATORS
     C*
     C                   SETOFF                                       313233
     C                   SETOFF                                       343536
     C                   SETOFF                                       373839
     C                   SETOFF                                       42
     C                   SETOFF                                       9394
     C*
    C***** Beg Add *** 09/01/16 **************************************
     C** initialize indicators
     C                   Eval      indFoDiaE = *off
     C                   Eval      indFoDiaF = *off
     C                   Eval      indFoDiaG = *off
     C                   Eval      indFoDiaH = *off
     C                   Eval      indFoDiaI = *off
     C                   Eval      indFoDiaJ = *off
     C                   Eval      indFoDiaK = *off
     C                   Eval      indFoDiaL = *off

     C                   Eval      indFoRSub = *off
    C***** End Add *** 09/01/16 **************************************
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
     C*** Beg Chg *** 05/23/13 ****************************************
     C*******            MOVE(P)   SCDIA1        HLDIA             5
     C                   MOVEL(P)  SCDIA1        HLDIA             7
     C*** End Chg *** 05/23/13 ****************************************
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
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   HLDIA         SCDIA1
     C                   MOVEL(P)  HLDIA         SCDIA1
     C*** End Chg *** 05/23/13 ****************************************
     C                   SETON                                        9034      pos cursor
     C                   ELSE
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   ##ICD9        SCDIA1
     C                   MOVEL(P)  ##ICD9        SCDIA1
     C*** End Chg *** 05/23/13 ****************************************
     C                   SETON                                        9031      pos cursor
     C                   ENDIF
     C*
     C     SCDIA1        IFNE      *BLANKS
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   SCDIA1        KKICD9
     C                   MOVEL(P)  SCDIA1        KKICD9
     C*** End Chg *** 05/23/13 ****************************************
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCD1DS
     C                   ENDIF
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C*
     C** DIAGNOSIS CODE #2 SEARCH
     C*
     C     HFLD          WHENEQ    'SCDIA2'
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   SCDIA2        HLDIA             5
     C                   MOVEL(P)  SCDIA2        HLDIA             7
     C*** End Chg *** 05/23/13 ****************************************
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
             *in31 = *on;
          Else;
             scDIa2 = outICD10;
             *in90 = *on;
             *in32 = *on;
          Endif;

          If scDia2 <> *blanks;
             kkICD10 = scDIa2;
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   SCD2DS = ICdsc1;
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
     C                   SETON                                        9031      pos cursor
     C                   ELSE
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   ##ICD9        SCDIA2
     C                   MOVEL(P)  ##ICD9        SCDIA2
     C*** End Chg *** 05/23/13 ****************************************
     C                   SETON                                        9032      pos cursor
     C                   ENDIF
     C*
     C     SCDIA2        IFNE      *BLANKS
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   SCDIA2        KKICD9
     C                   MOVEL(P)  SCDIA2        KKICD9
     C*** End Chg *** 05/23/13 ****************************************
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCD2DS
     C                   ENDIF
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C*
     C** DIAGNOSIS CODE #3 SEARCH
     C*
     C     HFLD          WHENEQ    'SCDIA3'
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   SCDIA3        HLDIA             5
     C                   MOVEL(P)  SCDIA3        HLDIA             7
     C*** End Chg *** 05/23/13 ****************************************
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
             *in32 = *on;
          Else;
             scDIa3 = outICD10;
             *in90 = *on;
             *in33 = *on;
          Endif;

          If scDia3 <> *blanks;
             kkICD10 = scDIa3;
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   SCD3DS = ICdsc1;
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
     C                   SETON                                        9032      pos cursor
     C                   ELSE
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   ##ICD9        SCDIA3
     C                   MOVEL(P)  ##ICD9        SCDIA3
     C*** End Chg *** 05/23/13 ****************************************
     C                   SETON                                        9033      pos cursor
     C                   ENDIF
     C*
     C     SCDIA3        IFNE      *BLANKS
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   SCDIA3        KKICD9
     C                   MOVEL(P)  SCDIA3        KKICD9
     C*** End Chg *** 05/23/13 ****************************************
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCD3DS
     C                   ENDIF
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C*
     C** DIAGNOSIS CODE #4 SEARCH
     C*
     C     HFLD          WHENEQ    'SCDIA4'
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   SCDIA4        HLDIA             5
     C                   MOVEL(P)  SCDIA4        HLDIA             7
     C*** End Chg *** 05/23/13 ****************************************
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
             *in33 = *on;
          Else;
             scDIa4 = outICD10;
             *in90 = *on;
       //***** Beg Chg *** 09/01/16 ****************************************
             indFoDiaE = *on;
            //*****  *in35 = *on;
       //***** End Chg *** 09/01/16 ****************************************
          Endif;

          If scDia4 <> *blanks;
             kkICD10 = scDIa4;
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   SCD4DS = ICdsc1;
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
     C                   SETON                                        9033      pos cursor
     C                   ELSE
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   ##ICD9        SCDIA4
     C                   MOVEL(P)  ##ICD9        SCDIA4
     C*** End Chg *** 05/23/13 ****************************************
     C***** Beg Chg *** 09/01/16 ****************************************
     C****               SETON                                        9035      pos cursor
     C                   EVAL      *in90 = *on
     C                   EVAL      indFoDiaE = *on
     C***** End Chg *** 09/01/16 ****************************************
     C                   ENDIF
     C***** Beg Dlt *** 09/01/16 ****************************************
     C*** Beg Add *** 08/05/13 ****************************************
      *** /Free
      ***  Endif;
      *** /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C***** End Dlt *** 09/01/16 ****************************************
     C*
     C     SCDIA4        IFNE      *BLANKS
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   SCDIA4        KKICD9
     C                   MOVEL(P)  SCDIA4        KKICD9
     C*** End Chg *** 05/23/13 ****************************************
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCD4DS
     C                   ENDIF
     C***** Beg Add *** 09/01/16 ****************************************
      /Free
       Endif;
      /End-Free
     C***** End Add *** 09/01/16 ****************************************

    C***** Beg Add *** 09/01/16 **************************************
     C** Diagnosis Code #5 Search
     C*
     C     HFLD          WHENEQ    'SCDIAE'
     C                   MOVEL(P)  SCDIAE        HLDIA             7
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
             scDiaE = HLDIA;
             *in90 = *on;
             indFoDiaE = *on;
          Else;
             scDiaE = outICD10;
             *in90 = *on;
             indFoDiaF = *on;
          Endif;

          If scDiaE <> *blanks;
             kkICD10 = scDiaE;
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   scdEds = ICdsc1;
                Endif;
          Endif;
       Else;
      /End-Free
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
     C                   MOVEL(P)  HLDIA         SCDIAE
     C                   SETON                                        90        pos cursor
     C                   Eval      indFoDiaE = *on
     C                   ELSE
     C                   MOVEL(P)  ##ICD9        SCDIAE
     C                   SETON                                        90        pos cursor
     C                   Eval      indFoDiaF = *on
     C                   ENDIF
     C*
     C     SCDIAE        IFNE      *BLANKS
     C                   MOVEL(P)  SCDIAE        KKICD9
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCDEDS
     C                   ENDIF
      /Free
       Endif;
      /End-Free

    C** Diagnosis Code #6 Search
     C*
     C     HFLD          WHENEQ    'SCDIAF'
     C                   MOVEL(P)  SCDIAF        HLDIA             7
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
             scDiaF = HLDIA;
             *in90 = *on;
             indFoDiaF = *on;
          Else;
             scDiaF = outICD10;
             *in90 = *on;
             indFoDiaG = *on;
          Endif;

          If scDiaF <> *blanks;
             kkICD10 = scDIaF;
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   scdFds = ICdsc1;
                Endif;
          Endif;
       Else;
      /End-Free
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
     C                   MOVEL(P)  HLDIA         SCDIAF
     C                   SETON                                        90        pos cursor
     C                   Eval      indFoDiaF = *on
     C                   ELSE
     C                   MOVEL(P)  ##ICD9        SCDIAF
     C                   SETON                                        90        pos cursor
     C                   Eval      indFoDiaG = *on
     C                   ENDIF
     C*
     C     SCDIAF        IFNE      *BLANKS
     C                   MOVEL(P)  SCDIAF        KKICD9
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCDFDS
     C                   ENDIF
      /Free
       Endif;
      /End-Free

    C** Diagnosis Code #7 Search
     C*
     C     HFLD          WHENEQ    'SCDIAG'
     C                   MOVEL(P)  SCDIAG        HLDIA             7
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
             scDiaG = HLDIA;
             *in90 = *on;
             indFoDiaG = *on;
          Else;
             scDiaG = outICD10;
             *in90 = *on;
             indFoDiaH = *on;
          Endif;

          If scDiaG <> *blanks;
             kkICD10 = scDiaG;
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   scdGds = ICdsc1;
                Endif;
          Endif;
       Else;
      /End-Free
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
     C                   MOVEL(P)  HLDIA         SCDIAG
     C                   SETON                                        90        pos cursor
     C                   Eval      indFoDiaG = *on
     C                   ELSE
     C                   MOVEL(P)  ##ICD9        SCDIAG
     C                   SETON                                        90        pos cursor
     C                   Eval      indFoDiaH = *on
     C                   ENDIF
     C*
     C     SCDIAG        IFNE      *BLANKS
     C                   MOVEL(P)  SCDIAG        KKICD9
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCDGDS
     C                   ENDIF
      /Free
       Endif;
      /End-Free

    C** Diagnosis Code #8 Search
     C*
     C     HFLD          WHENEQ    'SCDIAH'
     C                   MOVEL(P)  SCDIAH        HLDIA             7
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
             scDiaH = HLDIA;
             *in90 = *on;
             indFoDiaH = *on;
          Else;
             scDiaH = outICD10;
             *in90 = *on;
             indFoDiaI = *on;
          Endif;

          If scDiaH <> *blanks;
             kkICD10 = scDiaH;
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   scdHds = ICdsc1;
                Endif;
          Endif;
       Else;
      /End-Free
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
     C                   MOVEL(P)  HLDIA         SCDIAH
     C                   SETON                                        90        pos cursor
     C                   Eval      indFoDiaH = *on
     C                   ELSE
     C                   MOVEL(P)  ##ICD9        SCDIAH
     C                   SETON                                        90        pos cursor
     C                   Eval      indFoDiaI = *on
     C                   ENDIF
     C*
     C     SCDIAH        IFNE      *BLANKS
     C                   MOVEL(P)  SCDIAH        KKICD9
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCDHDS
     C                   ENDIF
      /Free
       Endif;
      /End-Free

    C** Diagnosis Code #9 Search
     C*
     C     HFLD          WHENEQ    'SCDIAI'
     C                   MOVEL(P)  SCDIAI        HLDIA             7
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
             scDiaI = HLDIA;
             *in90 = *on;
             indFoDiaI = *on;
          Else;
             scDiaI = outICD10;
             *in90 = *on;
             indFoDiaJ = *on;
          Endif;

          If scDiaI <> *blanks;
             kkICD10 = scDiaI;
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   scdIds = ICdsc1;
                Endif;
          Endif;
       Else;
      /End-Free
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
     C                   MOVEL(P)  HLDIA         SCDIAI
     C                   SETON                                        90        pos cursor
     C                   Eval      indFoDiaI = *on
     C                   ELSE
     C                   MOVEL(P)  ##ICD9        SCDIAI
     C                   SETON                                        90        pos cursor
     C                   Eval      indFoDiaJ = *on
     C                   ENDIF
     C*
     C     SCDIAI        IFNE      *BLANKS
     C                   MOVEL(P)  SCDIAI        KKICD9
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCDIDS
     C                   ENDIF
      /Free
       Endif;
      /End-Free

    C** Diagnosis Code #10 Search
     C*
     C     HFLD          WHENEQ    'SCDIAJ'
     C                   MOVEL(P)  SCDIAJ        HLDIA             7
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
             scDiaJ = HLDIA;
             *in90 = *on;
             indFoDiaJ = *on;
          Else;
             scDiaJ = outICD10;
             *in90 = *on;
             indFoDiaK = *on;
          Endif;

          If scDiaJ <> *blanks;
             kkICD10 = scDiaJ;
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   scdJds = ICdsc1;
                Endif;
          Endif;
       Else;
      /End-Free
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
     C                   MOVEL(P)  HLDIA         SCDIAJ
     C                   SETON                                        90        pos cursor
     C                   Eval      indFoDiaJ = *on
     C                   ELSE
     C                   MOVEL(P)  ##ICD9        SCDIAJ
     C                   SETON                                        90        pos cursor
     C                   Eval      indFoDiaK = *on
     C                   ENDIF
     C*
     C     SCDIAJ        IFNE      *BLANKS
     C                   MOVEL(P)  SCDIAJ        KKICD9
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCDJDS
     C                   ENDIF
      /Free
       Endif;
      /End-Free

    C** Diagnosis Code #11 Search
     C*
     C     HFLD          WHENEQ    'SCDIAK'
     C                   MOVEL(P)  SCDIAK        HLDIA             7
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
             scDiaK = HLDIA;
             *in90 = *on;
             indFoDiaK = *on;
          Else;
             scDiaK = outICD10;
             *in90 = *on;
             indFoDiaL = *on;
          Endif;

          If scDiaK <> *blanks;
             kkICD10 = scDiaK;
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   scdKds = ICdsc1;
                Endif;
          Endif;
       Else;
      /End-Free
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
     C                   MOVEL(P)  HLDIA         SCDIAK
     C                   SETON                                        90        pos cursor
     C                   Eval      indFoDiaK = *on
     C                   ELSE
     C                   MOVEL(P)  ##ICD9        SCDIAK
     C                   SETON                                        90        pos cursor
     C                   Eval      indFoDiaL = *on
     C                   ENDIF
     C*
     C     SCDIAK        IFNE      *BLANKS
     C                   MOVEL(P)  SCDIAK        KKICD9
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCDKDS
     C                   ENDIF
      /Free
       Endif;
      /End-Free

    C** Diagnosis Code #12 Search
     C*
     C     HFLD          WHENEQ    'SCDIAL'
     C                   MOVEL(P)  SCDIAL        HLDIA             7
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
             scDiaL = HLDIA;
             *in90 = *on;
             indFoDiaL = *on;
          Else;
             scDiaL = outICD10;
             *in90 = *on;
             *in35 = *on;
          Endif;

          If scDiaL <> *blanks;
             kkICD10 = scDiaL;
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   scdLds = ICdsc1;
                Endif;
          Endif;
       Else;
      /End-Free
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
     C                   MOVEL(P)  HLDIA         SCDIAL
     C                   SETON                                        90        pos cursor
     C                   Eval      indFoDiaL = *on
     C                   ELSE
     C                   MOVEL(P)  ##ICD9        SCDIAL
     C                   SETON                                        90        pos cursor
     C                   Eval      indFoRSub = *on
     C                   ENDIF
     C*
     C     SCDIAL        IFNE      *BLANKS
     C                   MOVEL(P)  SCDIAL        KKICD9
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCDLDS
     C                   ENDIF
      /Free
       Endif;
      /End-Free

    C***** End Add *** 09/01/16 **************************************
     C*
    C***** Beg Add *** 09/01/16 **************************************

     C* ReSubmission Code Search
     C*
     C     HFLD          WHENEQ    'SCRSUBCD'
     C                   Eval      ##TBID = 'FLRC'
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
     C                   SETON                                        90
     C                   Eval      indFoRSub = *on
     C                   ELSE
     C     ##RRN         CHAIN     PGT02                              99
     C     *IN99         IFEQ      *OFF
     C                   Eval      scRSubCd = GTkey
     C                   Eval      scRSubDs = GTdesc
     C                   SETON                                        90
     C                   Eval      *in35 = *on
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##KC = '1'
    C***** End Add *** 09/01/16 **************************************
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
     C                   SETON                                        9042      pos cursor
     C                   ENDIF
     C*

     C* PAYMENT PLAN SEARCH
     C*
     C     HFLD          WHENEQ    'SCPPLN'
     C     SCEDI         IFEQ      'Y'
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
     C                   SETON                                        9038
     C                   ELSE
     C     ##RRN         CHAIN     PGT02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTKEY         SCPPLN
     C                   MOVEL(P)  GTDESC        SCPDSC
     C                   SETON                                        9039
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##KC = '1'
     C*
     C     HFLD          WHENEQ    'SCLIC#'
     C*
     C                   MOVEL     *BLANKS       ##KEY
     C                   MOVEL     'FL'          ##KEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      *OFF
     C     STBS01        ANDEQ     'Y'
     C                   MOVEL     SCLIC#        HLLIC#           13
     C                   CALL      'F2395R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##LIC#           13
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C                   SETON                                        9043
     C                   SETOFF                                       38
     C                   MOVEL(P)  HLLIC#        SCLIC#
     C                   ELSE
     C     ##LIC#        IFEQ      *BLANKS
     C                   MOVEL(P)  HLLIC#        SCLIC#
     C                   SETON                                        9043
     C                   ELSE
     C                   MOVEL(P)  ##LIC#        SCLIC#
     C                   SETON                                        9038
     C                   SETOFF                                       43
     C                   ENDIF                                                  ##LIC#=*BLAN
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE                                                   *IN60=*OFF
     C                   SETON                                        9043
     C                   ENDIF                                                  *IN60=*OFF
     C                   OTHER
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCDFC'
     C                   SETON                                        61
     C     HFLD          WHENEQ    'SCACCT'
     C                   SETON                                        62
     C     HFLD          WHENEQ    'SCEMPL'
     C                   SETON                                        63
    C*
    C*
    C*
    C*
    C*
     C     HFLD          WHENEQ    'SCEDI'
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
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SCEDI
     C*
     C                   SETON                                        9048
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C*
     C                   SETON                                        9048
     C*
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C     HFLD          WHENEQ    'SCSVZP'
     C                   SETON                                        97
     C*** Beg Add *** 07/18/25 ***************************************
     C     HFLD          WHENEQ    'SCSNPI'
     C                   EVAL      MSSNPI = 'MCR0676'
     C                   EVAL      ERSNPI = *ON
     C*** End Add *** 07/18/25 ***************************************
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
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SCREVI
     C                   MOVEL(P)  GTDESC        SCREVD
     C*
     C                   SETON                                        9048
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C*
     C                   SETON                                        9040
     C*
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C                   ENDSL
     C*
     C                   SETON                                        90
     C                   ENDSL
     C*
     C                   SETON                                        90
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
     C                   CLEAR                   SCDFC
     C                   CLEAR                   SCDIA1
     C                   CLEAR                   SCDIA2
     C                   CLEAR                   SCDIA3
     C                   CLEAR                   SCDIA4
     C***** Beg Add *** 09/01/16 **************************************
     C                   CLEAR                   SCDIAE
     C                   CLEAR                   SCDIAF
     C                   CLEAR                   SCDIAG
     C                   CLEAR                   SCDIAH
     C                   CLEAR                   SCDIAI
     C                   CLEAR                   SCDIAJ
     C                   CLEAR                   SCDIAK
     C                   CLEAR                   SCDIAL
     C***** End Add *** 09/01/16 **************************************
     C***** Beg Add *** 09/01/16 **************************************
     C                   Clear                   scRSubCd
     C                   Clear                   scRSubDs
     C***** End Add *** 09/01/16 **************************************
     C                   CLEAR                   SCD1DS
     C                   CLEAR                   SCD2DS
     C                   CLEAR                   SCD3DS
     C                   CLEAR                   SCD4DS
     C***** Beg Add *** 09/01/16 **************************************
     C                   CLEAR                   SCDEDS
     C                   CLEAR                   SCDFDS
     C                   CLEAR                   SCDGDS
     C                   CLEAR                   SCDHDS
     C                   CLEAR                   SCDIDS
     C                   CLEAR                   SCDJDS
     C                   CLEAR                   SCDKDS
     C                   CLEAR                   SCDLDS
     C***** End Add *** 09/01/16 **************************************
     C                   CLEAR                   SCACCT
     C                   CLEAR                   SCCHRG
     C                   CLEAR                   SCMRA
     C                   CLEAR                   SCNTWK
     C                   CLEAR                   SCOTHR
     C                   CLEAR                   SCOTH2
     C                   CLEAR                   SCPAID
     C                   CLEAR                   SCFLG7
     C                   CLEAR                   SCSNPI
     C                   CLEAR                   SCRNPI
     C                   CLEAR                   SCREVI
     C*** Beg Add *** 04/17/13 ****************************************
     C                   SETOFF                                       17
     C                   CLEAR                   SCRFEI
     C                   CLEAR                   SCRLSN
     C                   CLEAR                   SCRFSN
     C                   CLEAR                   SCRAD1
     C                   CLEAR                   SCRAD2
     C                   CLEAR                   SCRNCT
     C                   CLEAR                   SCRNST
     C                   CLEAR                   SCRZIP
     C*** End Add *** 04/17/13 ****************************************
     C*
     C* DEFAULT PAYMENT PLAN TO 'FS' AND EMPLOYEMENT RELATED TO 'Y'
     C*
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
     C  N99              MOVEL(P)  PDLIC#        SCFLIC
     C                   ELSE
     C     PDKY          CHAIN     PPD01                              99
     C  N99              MOVEL     PDSPCD        SCSPCD
     C  N99              MOVEL(P)  PDLIC#        SCFLIC
     C                   ENDIF
     C                   MOVEL     EOFLIC        SCSVZP
     C*** Beg Add *** 07/18/25 ***************************************
     C                   MOVE      EOFLIC        SCZIP
     C*** End Add *** 07/18/25 ***************************************
     C*
     C*
     C*
     C                   MOVEL     SCSPCD        ##KEY
     C     SPKY          CHAIN     PSP01                              99
     C  N99              MOVEL     SPDESC        SCSCDS
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
     C                   Z-ADD     EHDFC         SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDFC
     C*
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   EHDIA1        SCDIA1
     C********           MOVE(P)   SCDIA1        KKICD9
     C                   MOVEL(P)  EHDIA1        SCDIA1
     C                   MOVEL(P)  SCDIA1        KKICD9
     C*** End Chg *** 05/23/13 ****************************************
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       If xxICD10 = '1' and EHicdind ='Y';
          KKICD10 = EHicd1001;
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
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   EHDIA2        SCDIA2
     C********           MOVE(P)   SCDIA2        KKICD9
     C                   MOVEL(P)  EHDIA2        SCDIA2
     C                   MOVEL(P)  SCDIA2        KKICD9
     C*** End Chg *** 05/23/13 ****************************************
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       If xxICD10 = '1' and EHicdind ='Y';
          KKICD10 = EHicd1002;
          Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
               scD2ds = ICdsc1;
             Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDSC1        SCD2DS
     C*
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   EHDIA3        SCDIA3
     C********           MOVE(P)   SCDIA3        KKICD9
     C                   MOVEL(P)  EHDIA3        SCDIA3
     C                   MOVEL(P)  SCDIA3        KKICD9
     C*** End Chg *** 05/23/13 ****************************************
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       If xxICD10 = '1' and EHicdind ='Y';
          KKICD10 = EHicd1003;
          Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
               scD3ds = ICdsc1;
             Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDSC1        SCD3DS
     C*
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   EHDIA4        SCDIA4
     C********           MOVE(P)   SCDIA4        KKICD9
     C                   MOVEL(P)  EHDIA4        SCDIA4
     C                   MOVEL(P)  SCDIA4        KKICD9
     C*** End Chg *** 05/23/13 ****************************************
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       If xxICD10 = '1' and EHicdind ='Y';
          KKICD10 = EHicd1004;
          Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
               scD4ds = ICdsc1;
             Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDSC1        SCD4DS
     C*
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************

    C***** Beg Add *** 09/01/16 **************************************
     C                   MOVEL(P)  EHICDE        SCDIAE
     C                   MOVEL(P)  SCDIAE        KKICD9
      /Free
       If xxICD10 = '1' and EHicdind ='Y';
          KKICD10 = EHicdE;
          Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
               scdEds = ICdsc1;
             Endif;
       Else;
      /End-Free
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDSC1        SCDEDS
     C*
      /Free
       Endif;
      /End-Free
     *
     C                   MOVEL(P)  EHICDF        SCDIAF
     C                   MOVEL(P)  SCDIAF        KKICD9
      /Free
       If xxICD10 = '1' and EHicdind ='Y';
          KKICD10 = EHicdF;
          Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
               scdFds = ICdsc1;
             Endif;
       Else;
      /End-Free
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDSC1        SCDFDS
     C*
      /Free
       Endif;
      /End-Free

     *
     C                   MOVEL(P)  EHICDG        SCDIAG
     C                   MOVEL(P)  SCDIAG        KKICD9
      /Free
       If xxICD10 = '1' and EHicdind ='Y';
          KKICD10 = EHicdG;
          Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
               scdGds = ICdsc1;
             Endif;
       Else;
      /End-Free
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDSC1        SCDGDS
     C*
      /Free
       Endif;
      /End-Free

     *
     C                   MOVEL(P)  EHICDH        SCDIAH
     C                   MOVEL(P)  SCDIAH        KKICD9
      /Free
       If xxICD10 = '1' and EHicdind ='Y';
          KKICD10 = EHicdH;
          Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
               scdHds = ICdsc1;
             Endif;
       Else;
      /End-Free
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDSC1        SCDHDS
     C*
      /Free
       Endif;
      /End-Free

     *
     C                   MOVEL(P)  EHICDI        SCDIAI
     C                   MOVEL(P)  SCDIAI        KKICD9
      /Free
       If xxICD10 = '1' and EHicdind ='Y';
          KKICD10 = EHicdI;
          Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
               scdIds = ICdsc1;
             Endif;
       Else;
      /End-Free
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDSC1        SCDIDS
     C*
      /Free
       Endif;
      /End-Free

     *
     C                   MOVEL(P)  EHICDJ        SCDIAJ
     C                   MOVEL(P)  SCDIAJ        KKICD9
      /Free
       If xxICD10 = '1' and EHicdind ='Y';
          KKICD10 = EHicdJ;
          Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
               scdJds = ICdsc1;
             Endif;
       Else;
      /End-Free
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDSC1        SCDJDS
     C*
      /Free
       Endif;
      /End-Free

     *
     C                   MOVEL(P)  EHICDK        SCDIAK
     C                   MOVEL(P)  SCDIAK        KKICD9
      /Free
       If xxICD10 = '1' and EHicdind ='Y';
          KKICD10 = EHicdK;
          Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
               scdKds = ICdsc1;
             Endif;
       Else;
      /End-Free
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDSC1        SCDKDS
     C*
      /Free
       Endif;
      /End-Free

     *
     C                   MOVEL(P)  EHICDL        SCDIAL
     C                   MOVEL(P)  SCDIAL        KKICD9
      /Free
       If xxICD10 = '1' and EHicdind ='Y';
          KKICD10 = EHicdL;
          Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
               scdLds = ICdsc1;
             Endif;
       Else;
      /End-Free
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDSC1        SCDLDS
     C*
      /Free
       Endif;
      /End-Free
    C***** End Add *** 09/01/16 **************************************
     C                   MOVE(P)   EHACCT        SCACCT
     C                   MOVEL(P)  EHLIC#        SCLIC#
     C     EHFLIC        IFNE      *BLANKS
     C                   MOVEL(P)  EHFLIC        SCLIC#
     C                   ENDIF
     C*
     C     EOTAPE        IFNE      *BLANKS
     C                   MOVEL     EOTAPE        SCEDI
     C                   MOVEL(P)  'FLES'        KKTBID
     C                   MOVEL(P)  SCEDI         KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTDESC        SCDSDS
     C                   ELSE
     C                   CLEAR                   SCDSDS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   Z-ADD     EOCHRG        SCCHRG
     C                   Z-ADD     EOMRA         SCMRA
     C                   Z-ADD     EONDIS        SCNTWK
     C                   Z-ADD     EOOTHR        SCOTHR
     C                   Z-ADD     EOOTH2        SCOTH2
     C                   Z-ADD     EOPAID        SCPAID
     C                   MOVEL(P)  EHPPLN        SCPPLN
     C                   MOVEL     EHEMPL        SCEMPL
     C*
     C                   MOVEL     EOREVI        SCREVI
     C     @@MODE        IFEQ      'A'
     C*
     C     SCPPLN        IFEQ      *BLANKS                                      ADD 08/16/99
     C                   MOVEL(P)  BEPPLN        SCPPLN
     C                   ENDIF                                                  ADD 08/16/99
     C*
     C     SCEMPL        IFEQ      *BLANKS
     C                   MOVE      'Y'           SCEMPL
     C                   ENDIF                                                  SCEMPL = *BLANKS
     C                   MOVEL     BEREVI        SCREVI
     C                   ENDIF                                                  @@MODE = 'A'
     C     SCEDI         IFEQ      'Y'
     C                   MOVEL(P)  'FLPP'        KKTBID
     C                   ELSE
     C                   MOVEL(P)  'FLPA'        KKTBID
     C                   ENDIF
     C                   MOVEL(P)  SCPPLN        KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCPDSC
     C                   ELSE
     C                   MOVE      *BLANKS       SCPDSC
     C                   ENDIF
     C                   MOVEL(P)  EHSNPI        SCSNPI
     C                   MOVEL(P)  EHRNPI        SCRNPI
     C*** Beg Add *** 04/17/13 ****************************************
     C     CLTID         IFEQ      '114'
     C                   SETON                                        17
     C                   MOVE      EHRFEI        SCRFEI
     C                   MOVE      EHRLSN        SCRLSN
     C                   MOVE      EHRFSN        SCRFSN
     C                   MOVE      EHRAD1        SCRAD1
     C                   MOVE      EHRAD2        SCRAD2
     C                   MOVE      EHRCTY        SCRNCT
     C                   MOVE      EHRST         SCRNST
     C                   MOVE      EHRZIP        SCRZIP
     C                   ENDIF
     C*** End Add *** 04/17/13 ****************************************
     C*** Beg Add *** 05/23/13 ****************************************
      /Free
          //Beg Chg *** 08/05/13 ***************************************
          //scICdind = 'N';
          //If xxicd10 = *on;
          If xxicd10 = *on and EHicdind ='Y';
          //End Chg *** 08/05/13 ***************************************
             scDIa1  = EHicd1001;
             scDIa2  = EHicd1002;
             scDIa3  = EHicd1003;
             scDIa4  = EHicd1004;
             //*** Beg Add *** 09/01/16 ***********************
             scDiaE =  EHICDE;
             scDiaF =  EHICDF;
             scDiaG =  EHICDG;
             scDiaH =  EHICDH;
             scDiaI =  EHICDI;
             scDiaJ =  EHICDJ;
             scDiaK =  EHICDK;
             scDiaL =  EHICDL;
             //*** End Add *** 09/01/16 ***********************
             scICdind= EHicdind;
          // Beg Add *** 08/05/13 ****************************************
          Elseif EHicdind ='N';
             scICdind = EHicdind;
          // End Add *** 08/05/13 ****************************************
          Endif;
      /End-Free
     C*** End Add *** 05/23/13 ****************************************
     C***** Beg Add *** 09/01/16 **************************************
     C/Free

        scRSubCd = EHrrcd;
        If scRSubCd <> *blanks;
           kktbid = 'FLRC';
           kkkey = scRSubCd;
           Chain(n) gtky pgt01;
           If %found(ftgtl);
              scRSubDs = GTdesc;
           EndIf;
        EndIf;

      /End-Free
     C***** End Add *** 09/01/16 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C                   MOVE      *OFF          LICCHG            1
     C     SCLIC#        IFNE      EHFLIC
     C                   MOVE      *ON           LICCHG
     C                   ENDIF
     C*
     C                   Z-ADD     EOCO#         EHCO#
     C                   Z-ADD     EOSUB#        EHSUB#
     C                   Z-ADD     EOFND         EHFND
     C                   Z-ADD     EOFYR         EHFYR
     C                   Z-ADD     EOCASE        EHCASE
     C                   MOVE      EOSS#         EHSS#
     C                   Z-ADD     EOACDT        EHACDT
     C*
     C                   Z-ADD     SCDFC         SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EHDFC
     C*
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   SCDIA1        EHDIA1
     C********           MOVE(P)   SCDIA2        EHDIA2
     C********           MOVE(P)   SCDIA3        EHDIA3
     C********           MOVE(P)   SCDIA4        EHDIA4
     C                   MOVEL(P)  SCDIA1        EHDIA1
     C                   MOVEL(P)  SCDIA2        EHDIA2
     C                   MOVEL(P)  SCDIA3        EHDIA3
     C                   MOVEL(P)  SCDIA4        EHDIA4
     C***** Beg Add *** 09/01/16 **************************************
     C                   MOVEL(P)  SCDIAE        EHICDE
     C                   MOVEL(P)  SCDIAF        EHICDF
     C                   MOVEL(P)  SCDIAG        EHICDG
     C                   MOVEL(P)  SCDIAH        EHICDH
     C                   MOVEL(P)  SCDIAI        EHICDI
     C                   MOVEL(P)  SCDIAJ        EHICDJ
     C                   MOVEL(P)  SCDIAK        EHICDK
     C                   MOVEL(P)  SCDIAL        EHICDL
     C***** End Add *** 09/01/16 **************************************
     C***** Beg Add *** 09/01/16 **************************************
     C                   Eval      EHRRCD = scRSubCd
     C***** End Add *** 09/01/16 **************************************
     C*** End Chg *** 05/23/13 ****************************************
     C                   MOVE(P)   SCACCT        EHACCT
     C                   MOVEL(P)  SCLIC#        EHFLIC
     C     SCLIC1        CAT       SCLIC2        XXLIC2            2
     C     XXLIC2        CAT       SCLIC3        XXLIC3            3
     C*
     C                   MOVE      SCEDI         EHEDI
     C                   MOVEL(P)  SCPPLN        EHPPLN
     C                   MOVE      SCEMPL        EHEMPL
     C                   MOVE      SCSPCD        EOSPCD
     C*
     C                   MOVEL(P)  SCSNPI        EHSNPI
     C                   MOVEL(P)  SCRNPI        EHRNPI
     C                   MOVE      SCREVI        EOREVI
     C*** Beg Add *** 04/17/13 ****************************************
     C     CLTID         IFEQ      '114'
     C                   MOVE      SCRFEI        EHRFEI
     C                   MOVE      SCRLSN        EHRLSN
     C                   MOVE      SCRFSN        EHRFSN
     C                   MOVE      SCRAD1        EHRAD1
     C                   MOVE      SCRAD2        EHRAD2
     C                   MOVE      SCRNCT        EHRCTY
     C                   MOVE      SCRNST        EHRST
     C                   MOVE      SCRZIP        EHRZIP
     C                   ENDIF
     C*** End Add *** 04/17/13 ****************************************
     C*** Beg Add *** 05/23/13 ****************************************
      /Free
          //Beg Chg *** 08/05/13 ***************************************
          EHicdind  = scICdind;
          If xxicd10 = *on and scICdInd ='Y';
          //End Chg *** 08/05/13 ***************************************
             EHicd1001 = scDIa1;
             EHicd1002 = scDIa2;
             EHicd1003 = scDIa3;
             EHicd1004 = scDIa4;

             //*** Beg Add *** 09/01/16 ***********************
             EHICDE = scDiaE;
             EHICDF = scDiaF;
             EHICDG = scDiaG;
             EHICDH = scDiaH;
             EHICDI = scDiaI;
             EHICDJ = scDiaJ;
             EHICDK = scDiaK;
             EHICDL = scDiaL;
             //*** End Add *** 09/01/16 ***********************

             // If ICD10 is Active then no need of ICD9 fields

             EHDIA1 = *blanks;
             EHDIA2 = *blanks;
             EHDIA3 = *blanks;
             EHDIA4 = *blanks;
          //Beg Chg *** 08/05/13 ***************************************
          Else;
             EHDIA1 = scDIa1;
             EHDIA2 = scDIa2;
             EHDIA3 = scDIa3;
             EHDIA4 = scDIa4;
             //*** Beg Add *** 09/01/16 ***********************
             EHICDE = SCDIAE ;
             EHICDF = SCDIAF ;
             EHICDG = SCDIAG ;
             EHICDH = SCDIAH ;
             EHICDI = SCDIAI ;
             EHICDJ = SCDIAJ ;
             EHICDK = SCDIAK ;
             EHICDL = SCDIAL ;
             //*** End Add *** 09/01/16 ***********************

             // If ICD09 is Active then no need of ICD10 fields

             EHicd1001 = *Blanks;
             EHicd1002 = *Blanks;
             EHicd1003 = *Blanks;
             EHicd1004 = *Blanks;
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
       //*** Beg add ***** 07/18/25 ************************************
       mssnpi = *blanks;
       ersnpi = *off;
       *in97  = *off;
       //*** End add ***** 07/18/25 ************************************
     C     *INKJ         CASEQ     *ON           KJDO                           F10=search
     C                   ENDCS
     C*** Beg Add *** 06/03/15 ******************************************
     C                   SETOFF                                       919293
     C                   SETOFF                                       94
     C*** End Add *** 06/03/15 ******************************************
      /Free
      /If Defined(WEB)
         If *INKK;
           ExSr KKDO;  //F11=EOB/Notes
         EndIf;
      /EndIf
      /End-Free
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
     C     EHKY          CHAIN     PEH01                              99
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
     C                   ELSE
      *
     C     CLTID         IFEQ      '114'
     C     @@MODE        ANDEQ     'A'
      *
     C     CLTID         OREQ      '114'
     C     @@MODE        ANDEQ     'C'
     C                   EXSR      MSG114
     C                   ENDIF
      *
     C                   ENDIF                                                  *INKC = *ON
     C*
     C* CALL 'ADJUDICATION CALL' PROGRAM IF IN UNPEND MODE
     C* THIS IS DONE BECAUSE OTHER BILLS MAY HAVE BEEN PROCOSSED
     C* FOR THIS CASE SINCE THIS BILL HAS BEEN ADJUDICATED
     C*
     C*
     C*
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
     C                   MOVE      EDITST        KKAST
     C                   MOVE      EOSYS         KKSYS
     C                   MOVE      EOBTYP        KKBTYP
     C                   MOVEL     EOSTYP        KKSTYP
     C     BEKY          CHAIN     PBE01                              98
     C*
     C** DETERMINE IF ELIGIBLE FOR DATA SUBMISSION SHOULD BE DISPLAYED
     C** AND REQUIRED
     C*
     C                   MOVEL     EDITST        ##KEY
     C     STKY          CHAIN     PST01                              99
     C     *IN99         IFEQ      *OFF
     C     ST1A4         ANDEQ     'Y'
     C                   SETON                                        30
     C*
     C     SCEDI         IFEQ      *BLANKS
     C  N98              MOVE      BEEDI         SCEDI
     C                   ENDIF
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
     C* SET INITIAL CURSOR POSITON TO DATE FIRST CONSULTED UNLESS
     C* ORIGINAL BILL THEN SET CURSOR TO SERVICE ZIP
     C     EORCN         IFGT      0
     C                   SETON                                        37
     C                   ELSE
     C                   SETON                                        42
     C                   ENDIF
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
     C* PROTECT ELIG FOR DATA SUBMISSION IF SENT TO THE STATE ALREADY
     C* IT CAN BE CHANGE IN THE EDI CORRECTION SCREENS
     C*
     C     EOTAPE        IFNE      ' '
     C     EOTAPE        ANDNE     'Y'
     C     EOTAPE        ANDNE     'N'
     C     EOTAPE        ANDNE     'E'
     C                   SETON                                        25
     C                   ELSE
     C                   SETOFF                                       25
     C                   ENDIF
     C*
     C                   MOVEL(P)  'FLES'        KKTBID
     C                   MOVEL(P)  SCEDI         KKKEY
     C     GTKY          CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTDESC        SCDSDS
     C                   ELSE
     C                   CLEAR                   SCDSDS
     C                   ENDIF
     C*** Beg Add *** 04/17/13 ****************************************
     C     CLTID         IFEQ      '114'
     C                   MOVE      2             #OFSCR            1 0
     C                   ELSE
     C                   MOVE      1             #OFSCR
     C                   ENDIF
     C*** End Add *** 04/17/13 ****************************************
       //*** Beg add ***** 07/18/25 *************************************
       If isModEnabled('NCCI Med Data Call');
          #OFSCR = 2;
       Endif;
       //*** End add ***** 07/18/25 *************************************
     C*** Beg Add *** 05/23/13 ****************************************
      /Free

         // Call System API to check wheather the ICD10 is active or not

         @@rtcd = *blanks;
         callp SA015R(@@rtcd);

         Select;
         // ICD10 is inactive
         When @@rtcd = '1';
         xxicd10 = '0';
         DspicdInd = '1';

         // ICD10 is active
         When @@rtcd = '2';
         xxicd10 = '1';
         DspicdInd = '0';
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
     C                   MOVEL     Q1USER        EHUSER
     C                   MOVEL     Q1PGM         EHPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        EHTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EHDT
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
     C                   MOVEL(P)  SCDIA2        EODIA2
     C                   MOVEL(P)  SCDIA3        EODIA3
     C                   MOVEL(P)  SCDIA4        EODIA4
     C                   MOVE      SCEDI         EOTAPE
     C                   MOVEL(P)  SCFLG7        EOFLG7
     C*
     C     SCSVZP        IFEQ      *ZEROS
     C                   MOVEL     '     '       EOFLIC
     C     P1KY          CHAIN     PP101                              99
     C  N99              MOVEL     P1ZIP         SCSVZP
     C*** Beg Add *** 07/18/25 ***************************************
     C  N99              MOVE      P1ZIP         SCZIP
     C*** End Add *** 07/18/25 ***************************************
     C                   ENDIF
     C                   MOVEL     SCSVZP        EOFLIC
     C*** Beg Add *** 07/18/25 ***************************************
     C                   MOVE      SCZIP         EOFLIC
     C*** End Add *** 07/18/25 ***************************************
     C                   MOVEL(P)  EOFLIC        KKZIPA            3
     C     GZKY          SETGT     PGZ01
     C                   READP     PGZ01                                  99
     C     *IN99         IFEQ      *OFF
     C     KKZIPA        ANDLE     GZTOZP
     C                   MOVE      GZZIPA        EOZIPA
     C                   ELSE
     C                   MOVE      *BLANKS       EOZIPA
     C                   ENDIF                                                  *IN99 IFEQ *OFF
     C*
     C                   MOVE      SCREVI        EOREVI
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
     C* If in ADD mode and @@RRN (FAEHP) is not zeros, then the user
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
     C                   Z-ADD     EOEOB#        EHEOB#
     C                   MOVEL     Q1USER        EHUSER
     C                   MOVEL     Q1USER        EHIUSR
     C                   MOVEL     Q1PGM         EHPGM
     C                   MOVEL     Q1PGM         EHIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        EHTIME
     C                   Z-ADD     DSTIME        EHITIM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EHDT
     C                   MOVEL     SRDTO         EHIDT
     C*
     C                   WRITE     DBRCDF
     C     EHKY          CHAIN     PEH01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DBRRNA        @@RRN
     C                   ENDIF                                                  *IN99 = *OFF
     C     @@RRN2        CHAIN     PEO01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  SCSPCD        EOSPCD
     C                   MOVEL(P)  SCDIA1        EODIA1
     C                   MOVEL(P)  SCDIA2        EODIA2
     C                   MOVEL(P)  SCDIA3        EODIA3
     C                   MOVEL(P)  SCDIA4        EODIA4
     C                   MOVE      SCEDI         EOTAPE
     C                   MOVEL     SCFLG7        EOFLG7
     C                   MOVE      SCREVI        EOREVI
     C     SCSVZP        IFEQ      *ZEROS
     C     P1KY          CHAIN     PP101                              99
     C  N99              MOVEL     P1ZIP         SCSVZP
     C*** Beg Add *** 07/18/25 ***************************************
     C  N99              MOVE      P1ZIP         SCZIP
     C*** End Add *** 07/18/25 ***************************************
     C                   ENDIF
     C                   MOVEL     SCSVZP        EOFLIC
     C*** Beg Add *** 07/18/25 ***************************************
     C                   MOVE      SCZIP         EOFLIC
     C*** End Add *** 07/18/25 ***************************************
     C                   MOVEL(P)  SCSVZP        KKZIPA            3
     C     GZKY          SETGT     PGZ01
     C                   READP     PGZ01                                  99
     C     *IN99         IFEQ      *OFF
     C     KKZIPA        ANDLE     GZTOZP
     C                   MOVE      GZZIPA        EOZIPA
     C                   ELSE
     C                   MOVE      *BLANKS       EOZIPA
     C                   ENDIF                                                  *IN99 IFEQ *OFF
     C                   MOVEL     Q1USER        EOUSER
     C                   MOVEL     Q1PGM         EOPGM
     C                   Z-ADD     DSTIME        EOTIME
     C                   MOVEL     SRDTO         EODT
     C                   UPDATE    PEO01
     C                   ENDIF
     C*
     C                   FEOD      FAEHP
     C*
     C                   MOVE      '1'           OUTFLG
     C                   ENDSR
      *
     C*
     C*================================================================
     C* MSG114 - Display "Correct Coding" Info message for clt114
     C*================================================================
     C     MSG114        BEGSR
      *
      *  IF no record in FAESP where ESBLST=  C (COMPLETED BILL)
      *
     C     ESKY          CHAIN     PES01                              60
      *
     C     *IN60         DOWEQ     *OFF
     C     ESBLST        ANDNE     'C'
     C     ESKY          READE     PES01                                  60
     C                   ENDDO
      *
     C     *IN60         IFEQ      *ON
      *
      *  Chain out to FAAUL1 using
      *  EOCO#/EOSUB#/EOFND/EOFYR/EOCASE/EOIDOC
      *  If a record is found then Display info window.
      *
     C     AUKY          CHAIN     PAU01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  AUFRC1        SCFRC1
     C                   MOVEL(P)  AUFRC2        SCFRC2
     C                   MOVEL(P)  AUTOC1        SCTOC1
     C                   MOVEL(P)  AUTOC2        SCTOC2
     C                   MOVEL(P)  AUREAS        SCREAS
     C                   EXFMT     WDW114
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ENDSR                                                  MSG114
      *
**
ABCDEFGHIJKLMNOPQRSTUVWXYZ
**
0123456789
