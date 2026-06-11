     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*  Program: F0356R                                              *
     H*  Description: Hospital (UB) Header Information                *
     H*  Created: 09/07/04  C:79 Project: NY009 FROM F0361R           *
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
     H*  08/11/21  C:295 Project: NY056                               *
     H*                  2021 NY Hospital FS Update                   *
     H*                                                               *
     H*  05/09/17  C:079 Support: 99/47070                            *
     H*                  Load scDRG with blanks for processing        *
     H*                                                               *
     H*  04/05/17  C:309 NY041                                        *
     H*                  Adding processing for "DRG Code" &           *
     H*                  "Severity Level" field added on screen.      *
     H*                                                               *
     H*  06/23/16  C:212 NY038                                        *
     H*                  change program call on provider search       *
     H*                                                               *
     H*  03/07/16  C:212 PHW40121                                     *
     H*                  add error message in DSPF                    *
     H*                  validate Bill Type to Type of Service        *
     H*                                                               *
     H*  10/01/15  C:079 PHW37605                                     *
     H*                  Check ICD-10 Indicator based on date and     *
     H*                  only in add mod                              *
     H*                                                               *
     H*  09/17/15 C:079 Support: 099/37167                            *
     H*                 Add type to ICD-10 Key list for editing       *
     H*                                                               *
     H*  08/05/13 C:277 PROJECT: MC329 Phase II                       *
     H*                 Add search functionality and Validate ICD10   *
     H*                                                               *
     H*  05/23/13 C:277 PROJECT: MC329                                *
     H*                 Activate the ICD10 fields if ICD10 is active  *
     H*                                                               *
     H*  04/05/11 C:257 Support:71931                                 *
     H*                         Assigning RRN after WRITEDBRCDF to    *
     H*                         resolve duplicate record in file FAEHP*
     H*                                                               *
     H*  04/19/10 C:227 PROJECT:NY020                                 *
     H*                         - Changed edits for SCICN field.      *
     H*                         - Changed search on SCICN field.      *
     H*                         - Added edit for SCDCDT to be required*
     H*                         - Added edits and search for new field*
     H*                           'Transfer Status'                   *
     H*                                                               *
     H*  08/21/08 C:180 PROJECT:MC294 PRISS 22                        *
     H*                         Do not underline NPI# fields when     *
     H*                         in display mode. - DSPF changes only. *
     H*                                                               *
     H*  07/16/08 C:180 PROJECT:MC294 Phase II                        *
     H*                         Add NPI# fields to SCR02.             *
     H*                                                               *
     H*  04/18/07 C:188 PROJECT:FL061 SET EDITST TO ADJ OR POLICY     *
     H*                         STATE BASED ON EOCPST                 *
     H*                                                               *
     H*  12/06/04 C:079 SUPPORT:44898                                 *
     H*                         DEFAULT OPCERT IF ONLY 1 FOR THE FEIN *
     H*                                                               *
     H*  09/07/04 C:079 PROJECT:NY009                                 *
     H*                         1. ADD REQUIRED FIELD TO HOLD OPCERT  *
     H*                         2. REQUIRE TYPE OF BILL               *
     H*                         3. OVERRIDE FMFMP AND LOGICALS        *
     H*                         4. USE NY TYPE OF SERVICE/EDITS       *
     H*                                                               *
     H*  07/24/04 C:079 PROJECT:MA012 REQUIRE TYPE OF BILL/SERVICE    *
     H*                         FOR STATE = MA                        *
     H*                                                               *
     H*  04/07/04 C:079 PROJECT:AL051                                 *
     H*                         REQUIRE TYPE OF SERVICE FOR AL        *
     H*                                                               *
     H*  03/12/03 C:079 SUPPORT:MC044 36319                           *
     H*                         CHECK THE STATUS OF DIAG CODES        *
     H*                                                               *
     H*  07/06/01 C:079 PROJECT:FSMNT #196                            *
     H*                         ADD EDIT TO SPECIALTY CODE            *
     H*                                                               *
     H*  05/30/01 C:062 PROJECT:FS229                                 *
     H*                         POPULATE EOMB# SCREEN FIELD           *
     H*                                                               *
     H*  01/02/01 C:062 PROJECT:MCMNT  BASE ONLY WHEN ENTERING SCR01  *
     H*                         POSITION TO PAT ACCT #                *
     H*                                                               *
     H*  11/22/00 C:062 PROJECT:FS212                                 *
     H*                         COMPILE ONLY.                         *
     H*                                                               *
     H*  11/08/00 C:062 PROJECT:FS146                                 *
     H*                         CHANGE 'BLTY' TO 'ZZBT' AND 'FLBT'.   *
     H*                                                               *
     H*  10/13/00 C:062 Support MC238                                 *
     H*                         ADD LOGIC TO DEAL WITH NON-SEARCHABLE *
     H*                         FIELDS.                               *
     H*                                                               *
     H*  08/30/00 C:062 PROJECT MC071 24169                           *
     H*                         ERROR ON TYPE OF BILL FIELD.          *
     H*                                                               *
     H*  08/29/00 C:062 PROJECT MCMNT                                 *
     H*                         ADD DISCHARGE DATE.                   *
     H*                                                               *
     H*  05/28/00 C:117 PROJECT MCMNT                                 *
     H*                         CORRECTED UNDERLINES IN DISPLAY MODE  *
     H*                         (COMPILE ONLY)                        *
     H*                                                               *
     H*  04/11/00 C:079 PROJECT FS211                                 *
     H*                         Allow future dates for Statement cover*
     H*                         from and to based on FBAL04 pos 8     *
     H*                                                               *
     H*  03/08/00 C:062 PROJECT MCMNT                                 *
     H*                         PASS "D" LOS TYPE TO F2010R SO THAT   *
     H*                         F2010R WILL ONLY SUBSET ON "D" LOS    *
     H*                         TYPE CODES.                           *
     H*                                                               *
     H*  02/21/00 C:117 PROJECT FS208                                 *
     H*                         ADDED LOGIC TO PROTECT SPECIALTY IF ON*
     H*                         RECON; ALSO SAVES SPECIALTY.          *
     H*                                                               *
     H*  02/16/00 C:53  PROJECT:KY010                                 *
     H*                         ADD KY REQUIRED FIELD.                *
     H*                                                               *
     H*  11/12/99 C:79  PROJECT MC152                                 *
     H*                         Delete/Change efficiency isssue.      *
     H*                                                               *
     H*  09/10/99 C:56  Project:KY010                                 *
     H*                         Write SCEDI to EOTAPE                 *
     H*                                                               *
     H*****************************************************************
     H*
     FF0361FNY  CF   E             WORKSTN
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
     FFMFMPNY   IF   E             DISK
     FFMFML2NY  IF   E           K DISK
     F                                     RENAME(PFM01:PFM02)
     FFMEXLNY   IF   E           K DISK
     F*FTEML1NY  IF   E           K DISK
     FFTEML2NY  IF   E           K DISK
     F*** Beg Add *** 08/05/13 ***************************************
     Ffmicd10l1 if   e           k disk
     F*** End Add *** 08/05/13 ***************************************
    F*** Beg Add *** 04/05/17 ***************************************
     F* DRG Code file
     Fftadpny   IF   E             DISK    Rename(pad01:pad0p)
     Fftadl1NY  IF   E           K DISK
    F*** End Add *** 04/05/17 ***************************************
     F*** Beg Add *** 07/18/25 ***************************************
     F*  Provider Auxillary File
     FFMP1L     IF   E           K DISK
     F*** End Add *** 07/18/25 ***************************************

      /COPY SCOPYMT01D
     D QCA             S             56    DIM(6) CTDATA PERRCD(1)
     D*
     D*
     D DBFDSA          DS
     D  DBRRNA               397    400B 0
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
    D*** Beg Add *** 04/05/17 ***************************************
     D CurDate         s                   like(ADefdt)
    D*** End Add *** 04/05/17 ***************************************

    D*** Beg Add *** 08/11/21 ***************************************
     D intPos          s              1  0
     D intLen          s              3  0
     D kkhnam          s                   like(emdesc)
    D*** End Add *** 08/11/21 ***************************************
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
     C     EXKY          KLIST
     C                   KFLD                    KKOPCT
     C                   KFLD                    KKEFDT
     C*
     C     EXKY1         KLIST
     C                   KFLD                    KKOPCT
     C*
     C     HRKY          KLIST
     C                   KFLD                    KKOPCT
     C*
     C     PGKY          KLIST
     C                   KFLD                    KKOPCT
     C     FMKY          KLIST
     C                   KFLD                    KKFEIN
     C*
     C     EMKY          KLIST
     C                   KFLD                    KKOPCT
     C***** Beg Add **********08/11/21 ************************
     C                   KFLD                    KKhnam
     C***** End Add **********08/11/21 ************************
     C                   KFLD                    XXDCDT
     C*
     C     EMKY1         KLIST
     C                   KFLD                    KKOPCT
     C***** Beg Add **********08/11/21 ************************
     C                   KFLD                    KKhnam
     C***** End Add **********08/11/21 ************************
     C*
     C*** Beg Add *** 08/05/13 ***************************************
     C*** KEY LIST FOR DIAGNOSIS CODE 10 MASTER FILE
     C     fmicd10l1key  klist
     C                   kfld                    kkicd10
     C*** End Add *** 08/05/13 ***************************************
     C*** Beg Add *** 09/17/15 ***************************************
     C                   kfld                    kkLost
     C*** End Add *** 09/17/15 ***************************************
     C*
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
     C     *LIKE         DEFINE    EOFEIN        ##FEIN
     C     *LIKE         DEFINE    EXOPCT        KKOPCT
     C     *LIKE         DEFINE    EXEFDT        KKEFDT
     C     *LIKE         DEFINE    EXEFDT        HLSCFD
     C     *LIKE         DEFINE    EOAST         EDITST
     C     *LIKE         DEFINE    EUTRST        HLTRST
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
     C***** Beg Add *** 03/07/16 *******************************************
     C/free

           // initialize error indicators and messages;
           msBlty = *blanks;
           erBlty = *off;
           msIcn  = *blanks;
           erIcn  = *off;

      //***** Beg Add *** 04/05/17 ***********************************
           msDRG  = *Blanks;
           msDGSV = *Blanks;
           erDRG  = *Off;
           erDGSV = *Off;
      //***** End Add *** 04/05/17 ***********************************
      /end-free
     C***** End Add *** 03/07/16 *******************************************
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
             msfzip= 'MCR0503';
             erfzip= *on;
             *in90 = *on;
             leavesr;
          Endif;
       Endif;
       //*** End add ***** 07/18/25 ************************************
     C*
     C** OPCERT IS REQUIRED
     C*
     C     SCOPCT        IFEQ      *BLANKS
     C     *IN29         ANDEQ     *OFF
     C*
     C                   SETON                                        9064
     C                   GOTO      CK1END
     C*
     C                   ENDIF
     C*
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
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   SCDIA1        KKICD9
     C                   MOVEL(P)  SCDIA1        KKICD9
     C*** End Chg *** 05/23/13 ****************************************
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
     C     SCSCPF        IFEQ      *ZEROS
     C                   SETON                                        9062
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCSCPF
     C*
     C* VALIDATE THE FROM STATEMENT COVERS DATE
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
     C     SCSCPT        IFEQ      *ZEROS
     C                   SETON                                        9063
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCSCPT
     C*
     C* VALIDATE THE STATEMENT COVERS TO DATE
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
     C* VALIDATE DISCHARGE DATE IF ENTERED
     C*
     C*
     C* DISCHARGE DATE IS REQUIRED
     C*
     C     SCDCDT        IFEQ      *ZEROS
     C                   SETON                                        905535
     C                   GOTO      CK1END
     C                   ENDIF
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
     C     SCICN         IFEQ      *BLANKS
     C                   SETON                                        9067
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C* VALIDATE BOTH TYPES OF SERVICE
     C*
     C     SCICN         IFNE      *BLANKS
     C                   Z-ADD     SCDCDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDCDT
     C     XXDCDT        IFGE      1091201
     C                   MOVE      'NYT1'        KKTBID
     C                   ELSE
     C                   MOVE      'NYST'        KKTBID
     C                   ENDIF                                                  XXDCDT>=1091201
     C                   MOVEL(P)  SCICN         KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9038
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C*
     C                   Z-ADD     SCSCPF        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C***                MOVEL     SRDTO         KKEFDT
     C                   MOVEL(P)  SCOPCT        KKOPCT
       /Free
          intPos = %scan('-':scOPct);
          intlen = %len(%Trim(scOPct));
          If intPos > *zero;
             kkOpct = %Trim(%Subst(scOPct:1:intPos-1));
             kkhnam = %Trim(%Subst(scOPct:intPos+1:intlen));
          Endif;

       /End-Free
     C*
     C* VALIDATE AGAINST NY EXEMPT UNITS TABLE FTEMPNY IF DISCHARGE DATE
     C* IS GREATER THAN 12/01/09
     C*
     C     XXDCDT        IFGE      1091201
     C     EMKY          SETGT     PEM01
     C     EMKY1         READPE    PEM01                                  99
     C*
     C* VALIDATE AGAINST NY EXEMPT UNITS TABLE
     C*
     C     SCICN         IFEQ      'IS'
     C     EMC001        ANDEQ     *ZEROS
     C*
     C     SCICN         OREQ      'IP'
     C     EMC003        ANDEQ     *ZEROS
     C*
     C     SCICN         OREQ      'IC'
     C     EMC005        ANDEQ     *ZEROS
     C*
     C     SCICN         OREQ      'IM'
     C     EMC009        ANDEQ     *ZEROS
     C*
     C     SCICN         OREQ      'IW'
     C     EMC011        ANDEQ     *ZEROS
     C*
     C     SCICN         OREQ      'IX'
     C     EMC007        ANDEQ     *ZEROS
     C*
     C                   SETON                                        9061
     C                   GOTO      CK1END
     C*
     C                   ENDIF                                                  SCICN IFEQ
     C*
     C                   ELSE                                                   XXDCDT>=1091201
     C     EXKY          SETGT     PEX01
     C     EXKY1         READPE    PEX01                                  99
     C*
     C* VALIDATE AGAINST NY EXEMPT UNITS TABLE
     C*
     C     SCICN         IFEQ      'IA'
     C     EXEXAD        ANDEQ     *ZEROS
     C*
     C     SCICN         OREQ      'IH'
     C     EXEXHS        ANDEQ     *ZEROS
     C*
     C     SCICN         OREQ      'IP'
     C     EXEXPS        ANDEQ     *ZEROS
     C*
     C     SCICN         OREQ      'IL'
     C     EXEXAL        ANDEQ     *ZEROS
     C*
     C     SCICN         OREQ      'IM'
     C     EXEXRH        ANDEQ     *ZEROS
     C*
     C     SCICN         OREQ      'IE'
     C     EXEXEP        ANDEQ     *ZEROS
     C*
     C     SCICN         OREQ      'IO'
     C     EXEXOT        ANDEQ     *ZEROS
     C*
     C     SCICN         OREQ      'IR'
     C     EXEXOT        ANDNE     2
     C*
     C                   SETON                                        9061
     C                   GOTO      CK1END
     C*
     C                   ENDIF                                                  SCICN IFEQ
     C*
     C                   ENDIF                                                  XXDCDT>=1091201
     C                   ENDIF                                                  SCICN NE *BLANKS
    C****** Beg Add *** 04/05/17 ******************************
     C     SCICN         IFEQ      'ID'
     C     SCDCDT        ANDGE     06012015
     C     SCDRG         ANDEQ     *Blanks
     C                   EVAL      erDRG = *On
     C                   EVAL      msDRG = 'MCR0503'
     C                   SETON                                        90
     C                   GOTO      CK1END
     C                   ENDIF
      *
     C     SCDRG         IFNE      *Blanks
      /Free
                         Chain (scDRG) pad01;
                         If Not %found(ftadl1ny);
      /End-Free
     C                   EVAL      erDRG = *On
     C                   EVAL      msDRG = 'MCR2383'
     C                   SETON                                        90
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
      *
     C     SCDRG         IFNE      *Blanks
     C     SCDGSV        ANDEQ     *Blanks
     C                   EVAL      erDGSV= *On
     C                   EVAL      msDGSV= 'MCR0503'
     C                   SETON                                        90
     C                   GOTO      CK1END
     C                   ENDIF
    C****** End Add *** 04/05/17 ******************************
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
     C**THE*FOLLOWING CORRESPONDENCE SHOULD EXIST BETWEEN
     C*
     C***** Beg Add *** 03/07/16 *******************************************
     C/free

           if %subst(scBlTy:2:1) = '1' or %subst(scBlTy:2:1) = '2' or
              %subst(scBlTy:2:1) = '5' or %subst(scBlTy:2:1) = '6' or
              %subst(scBlTy:2:1) = '7' or %subst(scBlTy:2:1) = '8';

              if scIcn <> 'IC' and scIcn <> 'ID' and
                 scIcn <> 'IM' and scIcn <> 'IP' and
                 scIcn <> 'IR' and scIcn <> 'IS' and
                 scIcn <> 'IW' and scIcn <> 'IX' and
                 scIcn <> 'MS' and scIcn <> 'MD';
                  erIcn = *on;
                  *in90 = *on;
                  msIcn = 'Invalid Type of Service for an Inpatient bill +
                           type. Enter a valid Inpatient Type of Service.';
                  leavesr;
              endif;

           endif;

           if %subst(scBlTy:2:1) = '3' or %subst(scBlTy:2:1) = '4';

              if scIcn <> 'AO' and scIcn <> 'AS' and
                 scIcn <> 'HO' and scIcn <> 'HS';
                  erIcn = *on;
                  *in90 = *on;
                  msIcn = 'Invalid Type of Service for an Outpatient bill +
                           type. Enter a valid Outpatient Type of Service.';
                  leavesr;
              endif;

           endif;

      /end-free
     C***** End Add *** 03/07/16 *******************************************
     C*
     C*
     C* REQUIRE TRANSFER STATUS FOR INPATIENT BILLS
     C*
     C     SCICN         IFEQ      'ID'
     C     SCTRST        IFEQ      *BLANKS
     C                   SETON                                        905650
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
     C*
     C* VALIDATE TRANSFER STATUS IF ENTERED
     C*
     C     SCTRST        IFNE      *BLANKS
     C                   MOVE      'TRST'        KKTBID
     C                   MOVEL(P)  SCTRST        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        905750
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCTRST<>*BLANKS
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
     C     CK1END        ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
       //*** Beg add ***** 07/18/25 *************************************
       If isModEnabled('NCCI Med Data Call');
          // %check return non zero if non numeric value including blank
          invalidPos = %check('0123456789': %trim(scnpi1));
          If invalidPos <> 0 or scnpi1 = *blanks;
             mssnpi = 'MC50007';
             ersnpi = *on;
             *in90 = *on;
             leavesr;
          Endif;
       Endif;
       //*** End add ***** 07/18/25 ************************************
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
     C                   SETOFF                                       374243
     C                   SETOFF                                       4450
     C*
    C****** Beg Add *** 04/05/17 ******************************
     C                   Eval      indfocDRG  = *Off
     C                   Eval      indfocDGSV = *Off
    C****** End Add *** 04/05/17 *******************************
     C*
     C                   CLEAR                   HLDIA
     C*
     C                   SELECT
     C*** Beg Add *** 07/18/25 ***************************************
     C     HFLD          WHENEQ    'SCFZIP'
     C                   EVAL       *in90  =*on
     C                   EVAL       erfzip =*on
     C                   EVAL       msfzip ='MCR0676'
     C     HFLD          WHENEQ    'SCNPI1'
     C                   EVAL      mssnpi = 'MCR0676'
     C                   EVAL      ersnpi = *on
     C                   EVAL      *in90  = *on
     C*** End Add *** 07/18/25 ***************************************
     C*
     C** DIAGNOSIS CODE #2 SEARCH
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
             *in34 = *on;
          Else;
             scDIa1 = outICD10;
             *in90 = *on;
             *in31 = *on;
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
     C                   SETON                                        9050      pos cursor
     C                   ENDIF
     C************ 08/11/21 ******************Beg Del**************************
     C*****HFLD          WHENEQ    'SCOPCT'
     C*****
     C*****              Z-ADD     0             ##RRN
     C*****
     C***** Beg Chg *** 06/23/16 *******************************************
     C*****              CALL      'F2556R'
     C*****              PARM      '2'           ##CALL
     C*****              PARM      '0'           ##KC
     C*****              PARM      '0'           ##KL
     C*****              PARM      EDITST        ##ST              2
     C*****              PARM      *ZEROS        ##RRN
     C*****              PARM      EOFEIN        ##FEIN
     C************ 08/11/21 ***************** End Del**************************
     C*
     C****               CALL      'F2531R'
     C****               PARM      '2'           ##CALL
     C****               PARM      '0'           ##KC
     C****               PARM      '0'           ##KL
     C****               PARM      EDITST        ##ST              2
     C****               PARM      *ZEROS        ##RRN
     C****               PARM      EOFEIN        ##FEIN
     C***** End Chg *** 06/23/16 *******************************************
     C*
     C     ##RRN         IFNE      0
     C*
     C     ##RRN         CHAIN     PFM01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  FMMED#        SCOPCT
     C                   SETON                                        9033
     C                   ENDIF
     C*
     C                   ELSE
     C                   SETON                                        9050
     C*
     C                   ENDIF
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
     C                   Z-ADD     SCDCDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDCDT            7 0
     C     XXDCDT        IFGE      1091201
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM      'NYT1'        ##TBID
     C                   ELSE
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM      'NYST'        ##TBID
     C                   ENDIF
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
    C******* Beg Chg *** 04/05/17 *************************
     C*****              SETON                                        9042
     C                   EVAL      indfocDRG = *ON
     C                   SETON                                        90
    C******* End Chg *** 04/05/17 *************************
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
    C******* Beg Add *** 04/05/17 *************************
      * DRG Code Search

     C     HFLD          WHENEQ    'SCDRG'
     C                   CALL      'F2541R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      'NY'          ##ST              2
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   EVAL      indfocDRG = *ON
     C                   SETON                                        90
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZERO
     C     ##ST          ANDEQ     'NY'
     C     ##RRN         CHAIN     PAD0P                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  ADdrg         scDRG
     C                   MOVEL(P)  ADdesc        scDgDS
     C                   EVAL      indfocDGSV = *ON
     C                   SETON                                        90
     C                   ENDIF                                                  *IN99=*OFF
     C                   ENDIF                                                  ##RRN <> 0
     C*
     C* Severity Levle Search
     C*
     C     HFLD          WHENEQ    'SCDGSV'
     C                   Z-ADD     0             ##RRN
     C                   MOVE      SCDGSV        HLDGSV            1
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM      'NYSL'        ##TBID
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVE      HLDGSV        SCDGSV
     C                   EVAL      indfocDGSV = *ON
     C                   SETON                                        90
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCDGSV
     C                   MOVEL     GTDESC        SCDGSVDS
     C                   SETON                                        9042
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
    C******* End Add *** 04/05/17 *************************
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
     C                   SETON                                        9040
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C* TRANSFER STATUS SEARCH
     C*
     C     HFLD          WHENEQ    'SCTRST'
     C                   Z-ADD     0             ##RRN
     C                   MOVE      SCTRST        HLTRST
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM      'TRST'        ##TBID
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVE      HLTRST        SCICN
     C                   SETON                                        9040
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCTRST
     C                   SETON                                        9043
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
     C                   CLEAR                   SCOPCT
    C****** Beg Add *** 04/05/17 ******************************
     C                   EVAL      SCDRG  = *BLANKS
     C                   EVAL      SCDGSV = '1'
    C****** End Add *** 04/05/17 ******************************
     C****** Beg Add *** 08/11/21 ******************************
      /free
          scOPct = %Trim(EUMCAR) + '-' + %Trim(EUHNAM);
      /End-free
     C****** End Add *** 08/11/21 ******************************
     C****** Beg Del *** 08/11/21 ******************************
     C*****              MOVEL(P)  EUMCAR        SCOPCT
     C****** End Del *** 08/11/21 ******************************
     C     @@MODE        IFNE      'D'
     C                   SETON                                        33
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
     C     SCOPCT        IFEQ      *BLANKS
     C*
     C     FMKY          CHAIN     PFM02                              99
     C     *IN99         IFEQ      *OFF
     C     FMSTAT        ANDEQ     'A'
     C*
     C                   MOVEL(P)  FMMED#        SCOPCT
     C     FMKY          READE     PFM02                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C     FMSTAT        ANDEQ     'A'
     C                   CLEAR                   SCOPCT
     C                   ENDIF
     C*
     C                   ENDIF                                                  IN99 = OFF
     C*
     C                   ENDIF                                                  SCOPCT = BLANKS
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
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   EUDIA1        SCDIA1
     C********           MOVE(P)   SCDIA1        KKICD9
     C                   MOVEL(P)  EUDIA1        SCDIA1
     C                   MOVEL(P)  SCDIA1        KKICD9
     C*** End Chg *** 05/23/13 ****************************************
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
     C  N99              MOVEL     DIDSC1        SCD1DS
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C*
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
    C***** Beg Add *** 04/05/17 ***************************
      /Free
                         scDRG   = %Editc(EUdrg#:'X');

                         CurDate = %Dec(%Char(%Date():*cymd0):7:0);
                         Setgt (scDRG:CurDate) pad01;
                         Readpe scDRG pad01;
                         If Not %Eof(ftadl1ny);
                            scDgDS = ADdesc;
                         EndIf;

                         If EUdgsv <> *Blanks ;
                            scDGSV = EUdgsv  ;
                         Else;
                            scDGSV = '1' ;
                         EndIf;
        //*** Beg Add *** 05/09/17 ***********************************
                         if scDRG = *zeros;
                            scDRG = *blanks;
                         endif;
        //*** End Add *** 05/09/17 ***********************************
      /End-Free
     C                   MOVEL     'NYSL'        KKTBID
     C                   MOVEL(P)  EUDGSV        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        scDGSVDS
     C                   ENDIF                                                  *IN99 = *OFF
     C*
    C***** End Add *** 04/05/17 ***************************
     C*
     C                   MOVEL     EUTRST        SCTRST
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
     C                   MOVE      EUANPI        SCANPI
     C                   MOVE      EUONPI        SCONPI
     C                   MOVE      EUNPI1        SCNPI1
     C                   MOVE      EUNPI2        SCNPI2
     C*** Beg Add *** 05/23/13 ****************************************
      /Free
          //Beg Chg *** 08/05/13 ***************************************
          //scICdind = 'N';
          If xxicd10 = *on and  Euicdind = 'Y';
          //End Chg *** 08/05/13 ***************************************
             scDIa1  = EUICD1001;
             scICdind= EUICDind ;
          // Beg Add *** 08/05/13 ****************************************
          Elseif EUICDind ='N';
             scICdind = EUICDind ;
          // End Add *** 08/05/13 ****************************************
          Endif;
      /End-Free
     C*** End Add *** 05/23/13 ****************************************
      *
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
     C*
     C                   MOVEL(P)  SCACCT        EUACCT
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   SCDIA1        EUDIA1
     C                   MOVEL(P)  SCDIA1        EUDIA1
     C*** End Chg *** 05/23/13 ****************************************
     C                   MOVE      SCEDI         EUEDI
     C*
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
     C                   MOVE      SCADHR        EUADHR
     C                   Z-ADD     SCDCHR        EUDCHR
     C                   MOVEL     SCICN         EUICN
    C***** Beg Add *** 04/05/17 ***************************
      /Free
                         If scDRG <> *Blanks;
                            EUdrg#= %Dec(scDRG:5:0);
                         EndIf;
                         EUdgsv= scDGSV ;
      /End-Free
    C***** End Add *** 04/05/17 ***************************
     C                   MOVEL     SCDCN         EUDCN
     C                   MOVE      SCTRST        EUTRST
     C                   MOVE      SCSPCD        EOSPCD
     C*
     C*** Beg Chg *** 08/11/21 ****************************************
     C*****              MOVEL(P)  SCOPCT        EUMCAR
      /Free

         intPos = %scan('-':scOPct);
         intlen = %len(%Trim(scOPct));
         If intPos > *zero;
            EUmcar = %Subst(scOPct:1:intPos-1);
            EUhnam = %Subst(scOPct:intPos+1:intlen);
         Endif;
      /End-Free
     C*** End Chg *** 08/11/21 ****************************************
     C                   MOVE      SCANPI        EUANPI
     C                   MOVE      SCONPI        EUONPI
     C                   MOVE      SCNPI1        EUNPI1
     C                   MOVE      SCNPI2        EUNPI2
     C*** Beg Add *** 05/23/13 ****************************************
      /Free
          //Beg Chg *** 08/05/13 ***************************************
          EUICDind  = scICdind;
          If xxICD10 = '1' and scICdInd ='Y';
          //End Chg *** 08/05/13 ***************************************
             EUICD1001 = scDIa1;
             // If ICD10 is Active then no need of ICD9 fields
             EUDIA1 = *Blanks;
          //Beg Chg *** 08/05/13 ***************************************
          Else;
             EUDIA1 = scDIa1;
             // If ICD9 is Active then no need of ICD10 fields
             EUICD1001 = *Blanks;
          //End Chg *** 08/05/13 ***************************************
          Endif;
      /End-Free
     C*** End Add *** 05/23/13 ****************************************
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
       //*** Beg add ***** 07/18/25 ************************************
       erfzip = *off;
       msfzip = *blanks;
       ersnpi = *off;
       mssnpi = *blanks;
       //*** End add ***** 07/18/25 ************************************
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
     C* Delete overrides,
     C*
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*
     C* Delete overrides,
     C*
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C*
     C* Delete overrides,
     C*
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(6)
     C                   PARM                    LENGTH           15 5
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
     C                   SETON                                        69
     C                   ELSE
     C                   SETOFF                                       69
     C                   ENDIF                                                  EONTWK
     C*
     C* SET INITIAL CURSOR POSITON TO OPCERT
     C                   SETON                                        50
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
     C                   SETON                                        6566
     C                   ELSE
     C                   SETOFF                                       6566
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
     C* PERFORM FILE OVERRIDES
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL(P)  QCA(1)        QQCMD
     C                   CALL      'QCMDEXC'
     C                   PARM                    QQCMD            56
     C                   PARM                    LENGTH           15 5
     C*
     C* PERFORM FILE OVERRIDES
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL(P)  QCA(3)        QQCMD
     C                   CALL      'QCMDEXC'
     C                   PARM                    QQCMD            56
     C                   PARM                    LENGTH           15 5
     C*
     C* PERFORM FILE OVERRIDES
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL(P)  QCA(5)        QQCMD
     C                   CALL      'QCMDEXC'
     C                   PARM                    QQCMD            56
     C                   PARM                    LENGTH           15 5
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
**  (QCMDEX)
OVRDBF FILE(FMFML1) TOFILE(*LIBL/FMFML1NY)
DLTOVR FILE(FMFML1)
OVRDBF FILE(FMFML2) TOFILE(*LIBL/FMFML2NY)
DLTOVR FILE(FMFML2)
OVRDBF FILE(FMFMP) TOFILE(*LIBL/FMFMPNY)
DLTOVR FILE(FMFMP)
