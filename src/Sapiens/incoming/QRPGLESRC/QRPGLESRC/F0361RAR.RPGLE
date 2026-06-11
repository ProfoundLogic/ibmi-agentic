     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
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
     H*  REVISIONS
     H*
     H*  04/04/12 C:251 PROJECT AR041
     H*                 Copied F0361R, F0361RTX to make F0361RAR.
     H*                 Added two new fields
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
     FF0361FAR  CF   E             WORKSTN
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
     FSTSTL1    IF   E           K DISK
     F                                     RENAME(PST01:PST02)
     F*
     F*  Generic Table File
     FFTABL     IF   E           K DISK
     F*
     FFAESL2    IF   E           K DISK
     F*  DRG Code File
     FFTDGP     IF   E             DISK
     FFTDGL     IF   E           K DISK
     F                                     RENAME(PDG01:PDG02)
     FFTIIP     IF   E             DISK
     FFTIIL     IF   E           K DISK
     F                                     RENAME(PII01:PII02)
     F*
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
     F*
     FFTOPP     IF   E             DISK
     FFTOPL     IF   E           K DISK
     F                                     RENAME(POP01:LOP01)
     FFTPFP     IF   E             DISK
     FFTPFL     IF   E           K DISK
     F                                     RENAME(PTP01:LTP01)
     F*
     FFTSAL3    IF   E           K DISK
     FFTSAL4    IF   E           K DISK
     F                                     RENAME(PSA01:PSA02)
     FFTSAP     IF   E             DISK
     F                                     RENAME(PSA01:PSA03)
     F*
     FFDDUL3    UF   E           K DISK
     F*
     FFMSKP     IF   E             DISK
     FFMSKL1    IF   E           K DISK
     F                                     RENAME(PSK01:PSK02)
     F*
     FFMFNL     IF   E           K DISK
     F*
     FFMIRP     IF   E             DISK
     FFMIRL1    IF   E           K DISK
     F                                     RENAME(PIR01:PIR02)
     D*
     D LO              C                   CONST('abcdefghijklmnopqrst-
     D                                     uvwxyz')
     D ALPHA           C                   CONST('ABCDEFGHIJKLMNOPQRST-
     D                                     UVWXYZ')
     D*
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
     D #OFSCR          C                   CONST(4)
     C/COPY SCOPYMT05C
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
     F*** Beg Add *** 10/12/11 ****************************************
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     F*** End Add *** 10/12/11 ****************************************
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
     C     *LIKE         DEFINE    SCASRC        HLASRC
     C     *LIKE         DEFINE    SCCOC1        HLOCA
     C     *LIKE         DEFINE    SCOCD1        HLOCD
     C     *LIKE         DEFINE    EUADDT        HLFDAT
     C     *LIKE         DEFINE    EUADDT        HLTDAT
     C     *LIKE         DEFINE    EUADDT        HLODAT
     C     *LIKE         DEFINE    SCOSD         HLOSD
     C     *LIKE         DEFINE    SCVC01        HLVCD
     C     *LIKE         DEFINE    EUADDI        HLADDI
     C     *LIKE         DEFINE    SCPREA        HLPREA
     C     *LIKE         DEFINE    SCPCM         HLPCM
     C     *LIKE         DEFINE    SCPDSC        HLDESC
     C     *LIKE         DEFINE    EUSBDT        HLSBDT
     C     *LIKE         DEFINE    EUDCDT        HLDCDT
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
     C*
     C                   SETOFF                                       252653
     C                   SETOFF                                       616263
     C                   SETOFF                                       35
     C                   SETOFF                                       71
     C** SPECIALTY CODE MUST BE VALID IF ENTERED AND NOT A RECON
     C*
     C     SCSPCD        IFNE      *BLANKS
     C     *IN29         ANDEQ     *OFF
     C*
     C     EDITST        OREQ      'MS'
     C     SCSPCD        ANDEQ     *BLANKS
     C     SCICN         ANDEQ     'HS'
     C     *IN29         ANDEQ     *OFF
     C*
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
     C*
     C*
     C******ZIP CODE MUST BE ENTERED & CONTAIN ONLY NUMERIC VALUE
     C*
     C     SCFZIP        IFEQ      *ZEROS
     C     *IN29         ANDEQ     *OFF
     C*
     C                   SETON                                        902392
     C                   GOTO      CK1END
     C*
     C                   ENDIF
     C*
     C*
     C*
     C*** IF KY BILL AND EDI IS INSTALLED PAT ACCT IS REQD
     C*
     C*
     C     EDITST        IFEQ      'KY'
     C     ##EDI         ANDEQ     'Y'
     C     SCACCT        ANDEQ     *BLANKS
     C                   SETON                                        9091
     C                   GOTO      CK1END
     C                   ENDIF
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
     C*
     C*** Type of bill is required when EODSTY = 'DR'/'DS'.
     C     SCBLTY        OREQ      *BLANKS
     C     EODSTY        ANDEQ     'DR'
     C     SCBLTY        OREQ      *BLANKS
     C     EODSTY        ANDEQ     'DS'
     C*
     C*** Type of bill is required when STMFS = 'Y'.
     C     SCBLTY        OREQ      *BLANKS
     C     XXMFS         ANDEQ     'Y'
     C                   SETON                                        9049
     C                   GOTO      CK1END
     C                   ENDIF
     C*
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
     C*
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
     C*
     C*** From date of service is required when STMFS = 'Y'.
     C     XXMFS         IFEQ      'Y'
     C                   SETON                                        5724
     C                   SETOFF                                       6450
     C     SCSCPF        IFEQ      *ZEROS
     C                   SETON                                        9095
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
     C*
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
     C*
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
     C*
     C*** To date of service is required when STMFS = 'Y'.
     C     XXMFS         IFEQ      'Y'
     C                   SETON                                        5724
     C                   SETOFF                                       6450
     C     SCSCPT        IFEQ      *ZEROS
     C                   SETON                                        9096
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
     C*
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
     C*
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
     C*
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
     C*
     C                   SETON                                        6450
     C                   SETOFF                                       5724
     C*
     C     SCADDT        IFEQ      *ZEROS
     C                   SETON                                        906163
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF                                                  SCICN = o/p
     C                   ENDIF                                                  EODSTY = DR/DS
     C*
     C*** Addmission date is required when STMFS = 'Y'.
     C     XXMFS         IFEQ      'Y'
     C                   SETON                                        5724
     C                   SETOFF                                       6450
     C     SCADDT        IFEQ      *ZEROS
     C                   SETON                                        906163
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
     C*
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
     C*
     C     SCDCDT        IFEQ      *ZEROS
     C                   SETON                                        9058
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
     C*
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
     C*
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
     C*
     C     SCDCHR        IFNE      *ZEROS
     C     SCDCHR        IFGT      24
     C                   SETON                                        9087
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCDCHR LT 1
     C                   ENDIF                                                  SCADHR NE *ZEROS
     C*
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
     C*
     C* TYPE OF ADMISSION IS REQUIRED
     C*
     C     SCADTY        IFEQ      *BLANKS
     C                   SETON                                        9093
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C* VALIDATE TYPE OF ADMISSION
     C*
     C                   MOVEL(P)  'ARTA'        KKTBID
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
     C* VALIDATE ADMISSION SOURCE
     C     SCASRC        IFEQ      *BLANKS
     C                   SETON                                        9034
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCASRC = *BLANKS
     C*
     C                   MOVEL(P)  'ADSR'        KKTBID
     C                   MOVEL(P)  SCASRC        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9040
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C*
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
     C*
     C                   SETON                                        9067
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C* VALIDATE BOTH TYPES OF SERVICE
     C*
     C*** Type of service is required when EODSTY = 'DR'/'DS'.
     C     EODSTY        IFEQ      'DR'
     C     EODSTY        OREQ      'DS'
     C*
     C*** Type of service is required when STMFS = 'Y'.
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
     C     EDITST        IFEQ      'MS'
     C*** DRG code mandatory when bill is medicare PPO.
     C     EODSTY        OREQ      'DR'
     C     EODSTY        OREQ      'DS'
     C*
     C*** Drug# is required when STMFS = 'Y'.
     C     XXMFS         OREQ      'Y'
     C*
     C     SCICN         IFEQ      'IM'
     C     SCICN         OREQ      'IP'
     C     SCICN         OREQ      'IR'
     C     SCICN         OREQ      'IS'
     C* Validate SCDRG for inpatient
     C*
     C* Do not validate SCDRG for inpatient 'IR' and Medicare fee schedule
     C     SCICN         IFNE      'IR'
     C     XXMFS         ORNE      'Y'
     C*
     C*
     C* Don't mandate SCDRG for SNF bills
     C     XXBLTY        IFNE      '18'
     C     XXBLTY        ANDNE     '21'
     C     XXMFS         ORNE      'Y'
     C*
     C     SCDRG         IFEQ      0
     C                   SETON                                        905152
     C                   SETON                                        53
     C                   SETOFF                                       54
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCDRG=0
     C*
     C                   ENDIF                                                  SNF bills
     C                   ENDIF                                                  XXMFS = 'Y', SCICN =
     C*
     C                   ELSE
     C*
     C                   ENDIF                                                  SCICN=INPATIENT
     C*
     C                   ENDIF                                                  EDITST <> 'MD'
     C*
     C*
     C     EDITST        IFEQ      'IL'
     C     SCICN         IFEQ      'IM'
     C     SCICN         OREQ      'IP'
     C     SCICN         OREQ      'IS'
     C**** DRUG CODE
     C* Don't mandate SCDRG for SNF bills
     C     XXBLTY        IFNE      '18'
     C     XXBLTY        ANDNE     '21'
     C     XXMFS         ORNE      'Y'
     C*
     C     SCDRG         IFEQ      0
     C                   SETON                                        905152
     C                   SETON                                        5355
     C                   SETOFF                                       54
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCDRG=0
     C*
     C                   ENDIF                                                  SNF bills
     C*
     C                   ENDIF                                                  SCICN=IS,IH
     C                   ENDIF                                                  EDITST = 'IL'
     C*
     C**
      * VALIDATE DRG CODE
     C     SCDRG         IFNE      *ZEROS
     C                   MOVEL     SCDRG         KKDRG#
     C     DGKY          CHAIN     PDG02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9056
     C                   GOTO      CK1END
     C*
     C                   ELSE
     C                   MOVEL     DGDESC        KKDESC
     C*
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCDRG NE *ZEROS
     C*
     C     XXBLTY        IFEQ      '83'
     C     XXMPPO        ANDEQ     'Y'
     C     EOAST         ANDEQ     'NJ'
     C     XXMFS         OREQ      'Y'
     C     SCSPCD        ANDEQ     'ASC'
     C                   SETON                                        97
     C                   ELSE
     C                   SETOFF                                       97
     C                   ENDIF
     C*
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
     C                   MOVE(P)   SCDIA1        KKICD9
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        9087
     C                   GOTO      CK3END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCDIA1 NE BLANKS
     C*
     C* VALIDATE THE OTHER DIAGNOSIS CODES
     C*
     C     SCDIA2        IFNE      *BLANKS
     C                   MOVEL     SCDIA2        KKICD9
     C                   MOVE      'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        9077
     C                   GOTO      CK3END
     C*
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCDIA1 NE *BLANKS
     C*
     C     SCDIA3        IFNE      *BLANKS
     C                   MOVEL     SCDIA3        KKICD9
     C                   MOVE      'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        9078
     C                   GOTO      CK3END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCDIA1 NE *BLANKS
     C*
     C     SCDIA4        IFNE      *BLANKS
     C                   MOVEL     SCDIA4        KKICD9
     C                   MOVE      'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        9079
     C                   GOTO      CK3END
     C                   ENDIF                                                  *IN99 = *ON
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
     C* VALIDATE PROCEDURE CODES BASE ON PROCEDURE CODE METHODS ENTERED
     C*
     C     SCPCM         IFNE      '0'
     C     SCPRC1        ANDEQ     *BLANKS
     C                   SETON                                        8490
     C                   GOTO      CK3END
     C                   ENDIF
     C*
     C* VALIDATE PROCEDURE CODES BASE ON PROCEDURE CODE METHODS ENTERED
     C*
     C     SCPRC1        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '9'
     C                   MOVEL     SCPRC1        BLANK
     C     BLANK         IFNE      *BLANKS
     C                   MOVEL     SCPRC1        CODE4
     C                   MOVE(P)   CODE4         KKICD9
     C                   ENDIF
     C                   MOVEL     'O'           SCLOS1
     C                   MOVE      SCLOS1        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9073
     C                   GOTO      CK3END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCPRC1 NE *BLANKS
     C*
     C*
     C     SCPRC2        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '9'
     C                   MOVEL     SCPRC2        BLANK
     C     BLANK         IFNE      *BLANKS
     C                   MOVEL     SCPRC2        CODE4
     C                   MOVE(P)   CODE4         KKICD9
     C                   ENDIF
     C                   MOVEL     'O'           SCLOS2
     C                   MOVE      SCLOS2        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9074
     C                   GOTO      CK3END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCPRC2 NE *BLANKS
     C*
     C*
     C     SCPRC3        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '9'
     C                   MOVEL     SCPRC3        BLANK
     C     BLANK         IFNE      *BLANKS
     C                   MOVEL     SCPRC3        CODE4
     C                   MOVE(P)   CODE4         KKICD9
     C                   ENDIF
     C                   MOVEL     'O'           SCLOS3
     C                   MOVE      SCLOS3        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9075
     C                   GOTO      CK3END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCPRC3 NE *BLANKS
     C*
     C     SCPRC4        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '9'
     C                   MOVEL     SCPRC4        BLANK
     C     BLANK         IFNE      *BLANKS
     C                   MOVEL     SCPRC4        CODE4
     C                   MOVE(P)   CODE4         KKICD9
     C                   ENDIF
     C                   MOVEL     'O'           SCLOS4
     C                   MOVE      SCLOS4        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9076
     C                   GOTO      CK3END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCPRC4 NE *BLANKS
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
     C                   GOTO      CK4END
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      'D'           KKLOST
     C*
     C     SCADDI        IFNE      *BLANKS
     C                   MOVE(P)   SCADDI        KKICD9
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        9071
     C                   GOTO      CK4END
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCPREA        IFNE      *BLANKS
     C                   MOVE(P)   SCPREA        KKICD9
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        9073
     C                   GOTO      CK4END
     C                   ENDIF
     C                   ENDIF
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
     C     XX2A          IFEQ      'OP'
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
     C                   ENDIF
     C*
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
     C                   SETOFF                                       313233
     C                   SETOFF                                       343536
     C                   SETOFF                                       414243
     C                   SETOFF                                       404455
     C                   SETOFF                                       505152
     C                   SETOFF                                       222759
     C                   SETOFF                                       495354
     C                   SETOFF                                       206148
     C                   SETOFF                                       939495
     C*
     C*** CLEAR HOLD FIELDS
     C*
     C                   CLEAR                   HLDIA
     C                   CLEAR                   HLADDI
     C                   CLEAR                   HLPREA
     C*
     C                   SELECT
     C*
     C** DIAGNOSIS CODE #2 SEARCH
     C*
     C     HFLD          WHENEQ    'SCDIA1'
     C                   MOVE(P)   SCDIA1        HLDIA             5
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
     C                   MOVE(P)   HLDIA         SCDIA1
     C                   SETON                                        9034      pos cursor
     C                   ELSE
     C                   MOVE(P)   ##ICD9        SCDIA1
     C                   SETON                                        9031      pos cursor
     C                   ENDIF
     C*
     C     SCDIA1        IFNE      *BLANKS
     C                   MOVE(P)   SCDIA1        KKICD9
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCD1DS
     C                   ENDIF
     C*
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
     C*
     C* TYPE OF ADMISSION
     C*
     C     HFLD          WHENEQ    'SCADTY'
     C                   MOVEL(P)  'ARTA'        ##TBID
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
     C                   ELSE
     C                   SETON                                        9022
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C                   SETON                                        9022
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
     C                   SETON                                        9044
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
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
     C                   MOVE      SCOSD         HLOSD
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
     C                   MOVE      SCOSD2        HLOSD
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
     C                   MOVE      SCADDI        HLADDI
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
     C                   MOVE(P)   HLADDI        SCADDI
     C                   SETON                                        9031      pos cursor
     C                   ELSE
     C                   MOVE(P)   ##ICD9        SCADDI
     C*
     C                   SETON                                        9032      pos cursor
     C                   ENDIF
     C*
     C     SCADDI        IFNE      *BLANKS
     C                   MOVE(P)   SCADDI        KKICD9
     C                   MOVE      ##LOST        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCA1DS
     C                   ENDIF
     C*
     C** PATIENT REASON CODES
     C*
     C     HFLD          WHENEQ    'SCPREA'
     C                   MOVE      SCPREA        HLPREA
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
     C                   MOVE(P)   HLPREA        SCPREA
     C                   SETON                                        9032      pos cursor
     C                   ELSE
     C                   MOVE(P)   ##ICD9        SCPREA
     C*
     C                   SETON                                        9033      pos cursor
     C                   ENDIF
     C*
     C     SCPREA        IFNE      *BLANKS
     C                   MOVE(P)   SCPREA        KKICD9
     C                   MOVE      ##LOST        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCP1DS
     C                   ENDIF
     C*
     C* PROCEDURE CODE METHOD SEARCH
     C*
     C     HFLD          WHENEQ    'SCPCM'
     C                   MOVEL     SCPCM         HLPCM
     C                   MOVEL     SCPDSC        HLDESC
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
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      '0'           ##KC
     C                   PARM      '0'           ##KL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      KKTBID        ##TBID
     C*
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
     C     SCPCM         IFEQ      '9'
     C                   MOVEL     SCPRC1        HLPRC1            7
     C                   MOVEL     SCLOS1        HLLOS1            1
     C                   MOVEL     SCP1DS        HLP1DS           40
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
     C*
     C                   MOVEL     ##ICD9        BLANK             1
     C     BLANK         IFEQ      *BLANKS
     C                   MOVE      ##ICD9        CODE4             4
     C                   MOVEL(P)  CODE4         SCPRC1
     C                   ENDIF
     C*
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP1DS
     C                   ENDIF                                                  *INPP = *OFF
     C                   SETON                                        9040
     C                   ENDIF                                                  ##ICD9 = *BLANKS
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
     C     SCPCM         IFEQ      '9'
     C                   MOVEL     SCPRC2        HLPRC2            7
     C                   MOVE      SCLOS2        HLLOS2            1
     C                   MOVEL     SCP2DS        HLP2DS           40
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
     C*
     C                   MOVEL     ##ICD9        BLANK             1
     C     BLANK         IFEQ      *BLANKS
     C                   MOVE      ##ICD9        CODE4             4
     C                   MOVEL(P)  CODE4         SCPRC2
     C                   ENDIF
     C*
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP2DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   SETON                                        9041
     C                   ENDIF                                                  ##ICD9 = *BLANKS
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
     C     SCPCM         IFEQ      '9'
     C                   MOVEL     SCPRC3        HLPRC3            7
     C                   MOVE      SCLOS3        HLLOS3            1
     C                   MOVEL     SCP3DS        HLP3DS           40
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
     C*
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP3DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   SETON                                        9042
     C                   ENDIF                                                  ##ICD9 = *BLANKS
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
     C     SCPCM         IFEQ      '9'
     C                   MOVEL     SCPRC4        HLPRC4            7
     C                   MOVE      SCLOS4        HLLOS4            1
     C                   MOVEL     SCP4DS        HLP4DS           40
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
     C*
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP4DS
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   SETON                                        906364
     C                   SETON                                        49
     C                   SETOFF                                       6162
     C                   ENDIF                                                  ##ICD9 = *BLANKS
     C                   ENDIF                                                  SCPCM = '9'
     C*
     C* OTHER DIAGNOSIS CODES SEARCHES
     C*
     C     HFLD          WHENEQ    'SCDIA2'
     C                   MOVE(P)   SCDIA2        HLDIA             5
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
     C                   MOVE(P)   HLDIA         SCDIA2
     C                   SETON                                        9043      pos cursor
     C                   ELSE
     C                   MOVE(P)   ##ICD9        SCDIA2
     C                   SETON                                        9044      pos cursor
     C                   ENDIF
     C*
     C     SCDIA2        IFNE      *BLANKS
     C                   MOVE(P)   SCDIA2        KKICD9
     C                   MOVE      ##LOST        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCD2DS
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCDIA3'
     C                   MOVE(P)   SCDIA3        HLDIA             5
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
     C                   MOVE(P)   HLDIA         SCDIA3
     C                   SETON                                        9044      pos cursor
     C                   ELSE
     C                   MOVE(P)   ##ICD9        SCDIA3
     C                   SETON                                        9045      pos cursor
     C                   ENDIF
     C*
     C     SCDIA3        IFNE      *BLANKS
     C                   MOVE(P)   SCDIA3        KKICD9
     C                   MOVE      ##LOST        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCD3DS
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCDIA4'
     C                   MOVE(P)   SCDIA4        HLDIA             5
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
     C                   MOVE(P)   HLDIA         SCDIA4
     C                   SETON                                        9045      pos cursor
     C                   ELSE
     C                   MOVE(P)   ##ICD9        SCDIA4
     C                   SETON                                        9046      pos cursor
     C                   ENDIF
     C*
     C     SCDIA4        IFNE      *BLANKS
     C                   MOVE(P)   SCDIA4        KKICD9
     C                   MOVE      ##LOST        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCD4DS
     C                   ENDIF
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
     C*
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
     C                   CLEAR                   SCADDI
     C                   CLEAR                   SCA1DS
     C                   CLEAR                   SCPREA
     C                   CLEAR                   SCP1DS
     C                   CLEAR                   SCOFSN
     C                   CLEAR                   SCOLSN
     C                   CLEAR                   SCOLIC
     C*
     C     EDITST        IFEQ      'IL'
     C     @@MODE        ANDNE     'D'
     C                   SETON                                        36
     C                   ELSE
     C*
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
     C*
     C*  IF THERE IS A SPECIALTY CODE IN THE FAEOP FILE, USE IT;
     C*    OTHERWISE, USE THE ON IN FMPDP
     C     EOSPCD        IFNE      *BLANKS
     C                   MOVEL     EOSPCD        SCSPCD
     C                   ELSE
     C     PDKY          CHAIN     PPD01                              99
     C  N99              MOVEL     PDSPCD        SCSPCD
     C                   ENDIF
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
     C     @@MODE        IFNE      'A'
     C     @@MODE        OREQ      'A'
     C     @@RRN         ANDNE     *ZEROS
     C*
     C                   MOVEL     EUACCT        SCACCT
     C                   MOVE(P)   EUDIA1        SCDIA1
     C                   MOVE(P)   SCDIA1        KKICD9
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDSC1        SCD1DS
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
     C                   Z-ADD     EOCHRG        SCCHRG
     C                   Z-ADD     EOMRA         SCMRA
     C                   Z-ADD     EONDIS        SCNTWK
     C                   Z-ADD     EOOTHR        SCOTHR
     C                   Z-ADD     EOOTH2        SCOTH2
     C                   Z-ADD     EOPAID        SCPAID
     C*
     C                   MOVEL(P)  EOFLIC        SCFZIP
     C*
     C*
     C                   MOVE      EUANPI        SCANPI
     C                   MOVE      EUONPI        SCONPI
     C                   MOVE      EUNPI1        SCNPI1
     C                   MOVE      EUNPI2        SCNPI2
     C                   MOVEL     EUDRG#        SCDRG
     C                   MOVEL     EUDGDS        KKDESC
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
     C                   MOVE      EUOCD1        SCOCD1
     C                   MOVE      EUOCD2        SCOCD2
     C                   MOVE      EUOCD3        SCOCD3
     C                   MOVE      EUOCD4        SCOCD4
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
     C                   MOVEL     EUPCM         SCPCM
     C                   MOVE      'PCMT'        KKTBID
     C                   MOVEL(P)  SCPCM         KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCPDSC
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
     C     SCPCM         IFEQ      '9'
     C     SCPRC1        ANDNE     *BLANKS
     C                   MOVEL     EUPRC1        KKICD9
     C                   MOVE      EUPRC1        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP1DS
     C                   ENDIF                                                  *IN99 = *OFF
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
     C     SCPCM         IFEQ      '9'
     C     SCPRC2        ANDNE     *BLANKS
     C                   MOVEL     EUPRC2        KKICD9
     C                   MOVE      EUPRC2        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP2DS
     C                   ENDIF                                                  *IN99 = *OFF
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
     C     SCPCM         IFEQ      '9'
     C     SCPRC3        ANDNE     *BLANKS
     C                   MOVEL     EUPRC3        KKICD9
     C                   MOVE      EUPRC3        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP3DS
     C                   ENDIF                                                  *IN99 = *OFF
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
     C     SCPCM         IFEQ      '9'
     C     SCPRC4        ANDNE     *BLANKS
     C                   MOVEL     EUPRC4        KKICD9
     C                   MOVE      EUPRC4        KKLOST
     C*
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP4DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  SCPCM = '9'
     C*
     C                   MOVEL     EUDIA2        SCDIA2
     C     SCDIA2        IFNE      *BLANKS
     C                   MOVEL(P)  SCDIA2        KKICD9
     C                   MOVE      'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCD2DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  SCDIA2 = *BLANKS
     C*
     C                   MOVEL     EUDIA3        SCDIA3
     C     SCDIA3        IFNE      *BLANKS
     C                   MOVEL(P)  SCDIA3        KKICD9
     C                   MOVE      'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCD3DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  SCDIA4 = *BLANKS
     C*
     C                   MOVEL     EUDIA4        SCDIA4
     C     SCDIA4        IFNE      *BLANKS
     C                   MOVEL(P)  SCDIA4        KKICD9
     C                   MOVE      'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCD4DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  SCDIA4 = *BLANKS
     C*
     C*
     C                   Z-ADD     EOMRA         SCMRA
     C                   Z-ADD     EONDIS        SCNTWK
     C                   Z-ADD     EOOTHR        SCOTHR
     C                   Z-ADD     EOOTH2        SCOTH2
     C                   Z-ADD     EOPAID        SCPAID
     C*
     C                   MOVE      EUADTY        SCADTY
     C*
     C                   MOVE      EUANPI        SCANPI
     C                   MOVE      EUONPI        SCONPI
     C                   MOVE      EUNPI1        SCNPI1
     C                   MOVE      EUNPI2        SCNPI2
     C*
     C                   MOVE      EUADDI        SCADDI
     C                   MOVEL     SCADDI        KKICD9
     C                   MOVE      'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCA1DS
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   MOVE      EUPREA        SCPREA
     C                   MOVEL     SCPREA        KKICD9
     C                   MOVE      'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP1DS
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   MOVE      EUOFSN        SCOFSN
     C                   MOVE      EUOLSN        SCOLSN
     C                   MOVE      EUOLIC        SCOLIC
     C                   MOVE      EUADSC        SCASRC
     C*** Beg Add *** 04/04/12 *******************************************
      /free
        scafsn = euafsn;
        scalsn = eualsn;
      /end-free
     C*** End Add *** 04/04/12 *******************************************
     C                   ENDIF                                                  @@MODE NE 'A'
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
     C                   MOVE(P)   SCDIA1        EUDIA1
     C*
     C                   MOVEL     SCDRG         EUDRG#
     C                   MOVEL     KKDESC        EUDGDS
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
     C*
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
     C                   MOVE      SCANPI        EUANPI
     C                   MOVE      SCONPI        EUONPI
     C                   MOVE      SCNPI1        EUNPI1
     C                   MOVE      SCNPI2        EUNPI2
     C                   MOVE      SCASRC        EUADSC
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
     C                   MOVE      SCOCD1        EUOCD1
     C                   MOVE      SCOCD2        EUOCD2
     C                   MOVE      SCOCD3        EUOCD3
     C                   MOVE      SCOCD4        EUOCD4
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
     C*
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
     C*
     C                   MOVEL(P)  SCDIA2        EUDIA2
     C                   MOVEL(P)  SCDIA3        EUDIA3
     C                   MOVEL(P)  SCDIA4        EUDIA4
     C*
     C                   MOVE      SCSPCD        EOSPCD
     C                   MOVE      SCADTY        EUADTY
     C                   MOVE      SCANPI        EUANPI
     C                   MOVE      SCONPI        EUONPI
     C                   MOVE      SCNPI1        EUNPI1
     C                   MOVE      SCNPI2        EUNPI2
     C*
     C                   MOVE      SCADDI        EUADDI
     C                   MOVE      SCPREA        EUPREA
     C                   MOVE      SCOFSN        EUOFSN
     C                   MOVE      SCOLSN        EUOLSN
     C                   MOVE      SCOLIC        EUOLIC
     C*** Beg Add *** 04/04/12 *******************************************
      /free
        euafsn = scafsn;
        eualsn = scalsn;
      /end-free
     C*** End Add *** 04/04/12 *******************************************
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
     C
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
     C*
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
     C*
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
     C*
     C     *IN99         IFEQ      *OFF
     C     ST1A4         ANDEQ     'Y'
     C*
     C                   MOVE      EDITST        KKAST
     C                   MOVE      EOSYS         KKSYS
     C                   MOVE      EOBTYP        KKBTYP
     C                   MOVEL     EOSTYP        KKSTYP
     C                   ENDIF
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
     C*
     C                   SETON                                        6566
     C                   ELSE
     C                   SETOFF                                       6566
     C                   ENDIF
     C*
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
     C*
     C     EODSTY        IFEQ      'DR'
     C     EODSTY        OREQ      'DS'
     C     XXMFS         OREQ      'Y'
     C                   SETON                                        37
     C                   ELSE
     C                   SETOFF                                       37
     C                   ENDIF
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       CLTID             3
     C*
     C     CLTID         IFEQ      '114'
     C                   SETON                                        16
     C                   ELSE
     C                   SETOFF                                       16
     C                   ENDIF
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
     C*
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
     C*
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
     C                   MOVEL(P)  SCFZIP        EOFLIC
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
     C*
     C     EUKY          CHAIN     PEU01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DBRRNA        @@RRN
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C     @@RRN2        CHAIN     PEO01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  SCSPCD        EOSPCD
     C                   MOVEL(P)  SCDIA1        EODIA1
     C                   MOVEL(P)  SCFZIP        EOFLIC
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
