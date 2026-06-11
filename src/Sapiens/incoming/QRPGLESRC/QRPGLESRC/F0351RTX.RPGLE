     Hcopyright('(c) 2025 Sapiens, Inc. All rights reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H BNDDIR('WTBNDDIR')
     H*****************************************************************
     H*  Program: F0351RTX                                            *
     H*  Description: Physician (HCFA) Header Info update/display     *
     H*               TEXAS version                                   *
     H*  Created: 04/01/98  C:56 Project: MC152                       *
     H*****************************************************************
     H*  REVISIONS
     H*                                                               *
     H*  07/18/25  C:342 Project MC451                                *
     H*                  NCCI Med Data Call Enhancements              *
     H*                  Only DSPF changes                            *
     H*
     H*  10/01/15  C:079 PHW37605                                     *
     H*                  Check ICD-10 Indicator based on date and     *
     H*                  only in add mod                              *
     H*
     H*  06/16/15  C:309 PHW34136                                     *
     H*                  Add check on NPI# so that it can contain only*
     H*                  numeric values                               *
     H*                                                               *
     H*  01/23/15 C:280 PHASWARE: 32491                               *
     H*                 Add validation to scacct do not allow special
     H*                 characters
     H*
     H*  08/05/13 C:277 PROJECT: MC329 Phase II                       *
     H*                 Add search functionality and Validate ICD10   *
     H*                                                               *
     H*  05/23/13 C:277 PROJECT: MC329                                *
     H*                 Activate the ICD10 fields if ICD10 is active  *
     H*                                                               *
     H*  09/18/12 C:284 Project:TX079
     H*                 Add fields for Medicare Zip + 4
     H*
     H*  01/10/12 C:251 Project:25MSO
     H*                 Add fields for Rendering and attending license
     H*                 provider number .
     H*
     F*  09/23/11  C:277  SUPPORT: SEQ77908 MC144                     *
     F*                   Make 'Rendering Provider License Number' as *
     F*                   non mandatory on screen 2                   *
     F*                                                               *
     H*  07/07/11 C:257 Project:TX070
     H*                 Change of heading in display file.Changed edit
     H*                 on 'Billing Licens' and 'Referring Provider NPI'
     H*
     H*  04/05/11 C:257 Support:71931
     H*                 Assigning RRN to @@RRN after WRITEDBRCDF to
     H*                 resolve duplicate records in file FAEHP.
     H*
     H*  12/09/10 C:227 Project:25MRV
     H*                 Added call for ADL functionality for client 25
     H*
     H*  05/12/09 C:227 Project:TX061
     H*                 Remove TWCC-ADL functionality
     H*
     H*  03/26/09 C:227 Support:67149
     H*                 Auto populate Rendering Provider #
     H*
     H*  02/18/09 C:212 PROJECT:TX043
     H*                 CORRECT INITIALIZATION OF INDICATORS
     H*
     H*  02/12/09 C:212 PROJECT:TX057
     H*                 ADD LOGIC TO VALIDATE SELECTED FIELDS ON SCR02
     H*                 IRRESPECTIVE OF THE CASE ENTERED.
     H*
     H*  11/14/08 C:212 PROJECT:TX043
     H*                 ADD PROCESSING FOR NEW FIELDS ADDED - IMPLANT
     H*                 STATUS, CITY AND STATE
     H*
     H*  10/30/08 C:198 PROJECT:TX048
     H*                 Added Rendering provider license # processing.
     H*
     H*  05/23/07 C:062 PROJECT:MC295
     H*                 ADD NPI PROCESSING
     H*
     H*  05/11/07 C:188 SUPPORT: 58130
     H*                 XX2A EDIT CHANGED TO ALLOW 'LS' INSTEAD OF 'SA'
     H*
     H*  09/29/04 C:079 PROJECT: TX024
     H*                 CHANGE LICENSE NUMBER EDITS PER TWCC
     H*                 USE EHFLIC 13A
     H*
     H*  01/07/04 C:079 SUPPORT: 40630
     H*                 ALLOW DP + 3 DIGITS FOR PODIATRISTS LIC#'S
     H*
     H*  12/04/03 C:079 SUPPORT: 40208
     H*                 ALLOW MD + 5 DIGITS FOR LIC#'S IN MEXICO
     H*
     H*  09/19/03 C:062 PROJECT: TX021
     H*                 ADD ADL SCREENS
     H*
     H*  08/25/03 C:062 PROJECT: WC025 38631
     H*                 CHANGE THE LICENSE EDITS
     H*
     H*  06/27/03 C:079 PROJECT: TX020
     H*                 REQUIRE SERV ZIP FOR BILLS ON OR AFTER 8/1/03
     H*
     H*  06/19/03 C:079 SUPPORT: MC025 37510
     H*                 CHANGE THE EDIT ON LIC# FOR OT, PT,
     H*
     H*  03/12/03 C:079 SUPPORT: MC044 36319
     H*                 CHECK THE STATUS OF DIAG CODES
     H*
     H*  10/01/01 C:079 SUPPORT: MC114 28975
     H*                 DO NOT REQUIRE SERVICE ZIP CODE
     H*
     H*  07/06/01 C:079 PROJECT: FSMNT #196
     H*                 ADD EDIT TO SPECIALTY CODE
     H*
     H*  05/30/01 C:062 PROJECT: FS229
     H*                 POPULATE EOMB# SCREEN FIELD
     H*
     H*  05/25/01  C:062  PROJECT: FS228
     H*                   ADD SERVICE ZIP CODE
     H*
     H*  11/22/00  C:062  Proj: FS212
     H*                   COMPILE ONLY.
     H*
     H*  10/13/00  C:062  Support MC238
     H*                   ADD LOGIC TO DEAL WITH NON-SEARCHABLE FIELDS.
     H*
     H*  06/29/00  C:117  PROJECT: TX011
     H*                   ADDED MT & PA PREFX; DELETED DS AS A VALID
     H*                   PREFX
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
     H*  02/06/00 C:79  Support: MC048/#21639
     H*                 If tape flag=N don't edit TX Lic#             *
     H*                                                               *
     H*  09/10/99 C:56  Project: KY010                                *
     H*                 Write SCEDI to EOTAPE                         *
     H*****************************************************************
     H*
     FF0351FTX  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
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
     FSTSTL1    IF   E           K DISK
     F                                     RENAME(PST01:PST02)
     F*
     F*  Generic Table File
     FFTABL     IF   E           K DISK
     F*
     FFTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PGT02)
     FFTGTL     IF   E           K DISK
     FFTSAL3    IF   E           K DISK
     FFTSAL4    IF   E           K DISK
     F                                     RENAME(PSA01:PSA02)
     FFTSAP     IF   E             DISK
     F                                     RENAME(PSA01:PSA03)
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
     F*** Beg Add *** 08/05/13 ***************************************
     Ffmicd10l1 if   e           k disk
     F*** End Add *** 08/05/13 ***************************************
      /COPY SCOPYMT01D
     D*
     D*
     D DBFDSA          DS
     D  DBRRNA               397    400B 0
     D*
     D* LICENSE NUMBER DATA STRUCTURE
     D*
     D*
     D ALPHA           C                   CONST('ABCDEFGHIJKLMNOPQRST-
     D                                     UVWXYZ')
     D*
     D ALL             C                   CONST('ABCDEFGHIJKLMNOPQRST-
     D                                     UVWXYZ0123456789 ')
     D*
     D NUM09B          C                   CONST('0123456789 ')
     D*
     D LO              C                   CONST('abcdefghijklmnopqrst-
     D                                     uvwxyz')
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Physician (HCFA) Hea-
     D                                     der Information')
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
     D*** Beg Add *** 01/23/15 ***************************************
     D error           s              1
     D mode            s              1
      /COPY sprstring
     D*** End Add *** 01/23/15 ***************************************
     D***** Beg Add *** 06/16/15 **************************************
     D XXSNPI          S             30A   Inz(*Blanks)
     D XXRNPI          S             30A   Inz(*Blanks)
     D***** End Add *** 06/16/15 **************************************
     D*** Beg Add *** 07/18/25 ***************************************
     D invalidPos      s              2  0
      /copy sPrModule
     D*** End Add *** 07/18/25 ***************************************
     C*** Beg Add *** 09/18/12 ************************************************
     C/COPY SCOPYMT01C
     C*** End Add *** 09/18/12 ************************************************

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
     C*
     C* KEY LIST FOR FTGTP
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##TBID            4
     C                   KFLD                    ##KY             10
     C*
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
     C*** Beg Add *** 08/05/13 ***************************************
     C*** KEY LIST FOR DIAGNOSIS CODE MASTER FILE
     C     fmicd10l1key  klist
     C                   kfld                    kkicd10
     C*** End Add *** 08/05/13 ***************************************
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
     C*
     C*** Beg Del *** 09/18/12 *************************************************
     C****/COPY SCOPYMT01C
     C*** End Del *** 09/18/12 *************************************************
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
     C* SUBSTR - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
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
     C                   SETON                                        9086
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  SERVICE ZIP CODE CANNOT BE ZERO
     C*
     C     SCSVZP        IFEQ      *ZEROS
     C*** Beg Add *** 07/18/25 ***************************************
     C     *IN29         ANDEQ     *OFF
     C*** End Add *** 07/18/25 ***************************************
     C                   SETON                                        8490
     C                   GOTO      CK1END
     C                   ENDIF
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
     C*** End Add *** 08/05/13 ****************************************
     C                   ENDIF                                                  SCDIA2 IFNE BLANKS
     C*
     C* MEDICARE NUMBER IS REQUIRED
     C*
     C* Only edit license# if Data Sub flag='Y'
     C     SCEDI         IFEQ      'Y'
     C     SCEDI         OREQ      'N'
     C     SCLIC#        ANDNE     *BLANKS
     C*
     C     SCLIC#        IFEQ      *BLANKS
     C                   SETON                                        9077
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C*
     C*
     C*
     C*
     C*
     C*
     C*
     C*
     C*
     C*
     C*
     C*
     C*
     C*
     C*
     C*
     C*
     C*
     C*
     C*
     C*
     C*
     C*
     C*
     C                   MOVEL     SCLIC#        XX2A              2
     C                   MOVEL     SCLIC#        XX3A              3
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
     C                   SETON                                          9080
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     ' '           SCAN      SCLIC#        Y                 2 0    99
     C                   SUB       2             Y
     C*
     C     *IN99         IFEQ      *ON
     C     2             SUBST     SCLIC#:Y      XXST              2
     C*
     C                   ELSE
     C     2             SUBST     SCLIC#:12     XXST
     C*               .
     C                   ENDIF
     C*
     C                   MOVEL(P)  XXST          ##KEY
     C     STKY          CHAIN     PST02                              99
     C*
     C     *IN99         IFEQ      *ON
     C     Y             ORLE      2
     C                   SETON                                          9089
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C*
     C                   ENDIF                                                  SCEDI EQ Y
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
     C* CHECK TO ADL INFORMATION - STRIP OFF THE STATE SUFFIX
     C*
     C*
     C*
     C*
     C*
     C*
     C*
     C*
     C*
     C* CHECK TO ADL INFORMATION - STRIP OFF THE STATE SUFFIX
     C*
     C     CLTID         IFEQ      '025'
     C     *IN90         IFEQ      *OFF
     C*
     C     SCLIC#        IFNE      *BLANKS
     C*
     C                   Z-ADD     0             Y
     C     ' '           SCAN      SCLIC#        Y                        99
     C*
     C     *IN99         IFEQ      *ON
     C                   SUB       3             Y
     C                   ELSE
     C                   Z-ADD     11            Y
     C                   ENDIF                                                  *IN99 = *ON
     C*
     C     Y             SUBST     SCLIC#:1      XXLIC#           12
     C                   CALL      'F8362R'
     C                   PARM      XXLIC#        ##LIC#           12
     C                   PARM      *BLANKS       ##RTRN            1
     C*
     C                   ENDIF                                                  SCLIC# <> ' '
     C*
     C     ##RTRN        IFNE      *BLANKS
     C                   SETON                                        90
     C                   ENDIF                                                  ##RTRN <> ' '
     C                   ENDIF                                                  *IN90 = *OFF
     C                   ENDIF                                                  CLTID = '025'
     C     SCRNLC        IFEQ      *BLANKS
     C                   MOVEL     SCLIC#        SCRNLC
     C                   ENDIF
     C*
     C* FOR ASC BILLS DISPLAY REQUIRED FIELDS IN HI INTENSITY
     C*
     C     SCSPCD        IFEQ      'ASC'
     C                   SETON                                        50
     C*
     C                   ELSE
     C                   SETOFF                                       50
     C*
     C                   ENDIF
     C*
     C*
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
     C***** Beg Add *** 06/16/15 **************************************
      /Free

         // Validate Servicing facility NPI#, should be Numeric Only
         ErSNPI=*Off;
         Mode  = 'N';
         Error = ' ';
         XXSNPI= %Trim(SCSNPI);
         ErSNPI = validchar(XXSNPI:Mode:Error);
         If ErSNPI = *On;
           *IN90  = *On;
           //*** Beg add ***** 07/18/25 *************************************
           mssnpi = 'MC50007';
           //*** End add ***** 07/18/25 *************************************
         EndIf;

         // Validate Referring provider NPI#, should be Numeric Only
         ErRNPI=*Off;
         Error   = ' ';
         XXRNPI= %Trim(SCRNPI);
         ErRNPI =  validchar(XXRNPI:Mode:Error);
         IF ErRNPI = *On;
           *IN90  = *On;
         EndIf;

      /End-Free
     C***** End Add *** 06/16/15 **************************************
     C*
     C*
     C     CK1END        ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C* Only edit license# if Data Sub flag='Y'
     C     SCEDI         IFEQ      'Y'
     C     SCEDI         OREQ      'N'
     C     SCRNLC        ANDNE     *BLANKS
     C*
     C     SCRNLC        IFNE      *BLANKS
     C*
     C                   MOVEL     SCRNLC        XX2A              2
     C                   MOVEL     SCRNLC        XX3A              3
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
     C                   SETON                                          9080
     C                   GOTO      CK2END
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     ' '           SCAN      SCRNLC        Y                 2 0    99
     C                   SUB       2             Y
     C*
     C     *IN99         IFEQ      *ON
     C     2             SUBST     SCRNLC:Y      XXST              2
     C*
     C                   ELSE
     C     2             SUBST     SCRNLC:12     XXST
     C*
     C                   ENDIF
     C*
     C                   MOVEL(P)  XXST          ##KEY
     C     STKY          CHAIN     PST02                              99
     C*
     C     *IN99         IFEQ      *ON
     C     Y             ORLE      2
     C                   SETON                                          9089
     C                   GOTO      CK2END
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF                                                  SCEDI EQ Y
     C*
     C     SCRFLC        IFNE      *BLANKS
     C*
     C                   MOVEL     SCRFLC        XX2A              2
     C                   MOVEL     SCRFLC        XX3A              3
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
     C                   SETON                                          9077
     C                   SETOFF                                         81
     C                   GOTO      CK2END
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     ' '           SCAN      SCRFLC        Y                 2 0    99
     C                   SUB       2             Y
     C*
     C     *IN99         IFEQ      *ON
     C     2             SUBST     SCRFLC:Y      XXST              2
     C*
     C                   ELSE
     C     2             SUBST     SCRFLC:12     XXST
     C*
     C                   ENDIF
     C*
     C                   MOVEL(P)  XXST          ##KEY
     C     STKY          CHAIN     PST02                              99
     C*
     C     *IN99         IFEQ      *ON
     C     Y             ORLE      2
     C                   SETON                                          9081
     C                   SETOFF                                         77
     C                   GOTO      CK2END
     C                   ENDIF
     C                   ENDIF                                                                    SC
     C*
     C     SCATLC        IFNE      *BLANKS
     C*
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
     C                   SETON                                          9082
     C                   SETOFF                                         83
     C                   GOTO      CK2END
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
     C                   SETON                                          9083
     C                   SETOFF                                         82
     C                   GOTO      CK2END
     C                   ENDIF
     C                   ENDIF                                                                    SC
     C*
     C                   SETOFF                                       919293
     C                   SETOFF                                       878895
     C                   SETOFF                                       94
     C*
     C*** IMPLANT STATUS IS REQUIRED
     C*
     C     SCIMST        IFEQ      *BLANKS
     C     SCSPCD        ANDEQ     'ASC'
     C                   SETON                                        9091
     C                   GOTO      CK2END
     C                   ENDIF                                                  SCIMST IFEQ BLANKS
     C*
     C** VALIDATE IMPLANT STATUS
     C*
     C     SCIMST        IFNE      *BLANKS
     C                   MOVEL     'IMST'        ##TBID
     C                   MOVEL(P)  SCIMST        ##KY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCIMDS
     C                   ELSE
     C                   SETON                                        9087
     C                   GOTO      CK2END
     C                   ENDIF
     C                   ENDIF
     C*
     C*** CITY IS REQUIRED
     C*
     C     SCRCTY        IFEQ      *BLANKS
     C     SCSPCD        ANDEQ     'ASC'
     C                   SETON                                        9092
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
     C                   SETON                                        9088
     C                   GOTO      CK2END
     C                   ELSE
     C                   MOVEL(P)  SACTY         SCRCTY
     C                   ENDIF
     C                   ENDIF
     C*
     C*** STATE IS REQUIRED
     C*
     C     SCRST         IFEQ      *BLANKS
     C     SCSPCD        ANDEQ     'ASC'
     C                   SETON                                        9093
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
     C                   SETON                                        9095
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
     C                   SETOFF                                       3738
     C                   SETOFF                                       394041
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
     C********           MOVE(P)   SCDIA1        HLDIA             5
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
             *in35 = *on;
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
     C                   SETON                                        9035      pos cursor
     C                   ENDIF
     C*
     C     SCDIA4        IFNE      *BLANKS
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           MOVE(P)   SCDIA4        KKICD9
     C                   MOVEL(P)  SCDIA4        KKICD9
     C*** End Chg *** 05/23/13 ****************************************
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCD4DS
     C                   ENDIF
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
     C                   SETON                                        9038      pos cursor
     C                   ENDIF
     C*
     C** IMPLANT STATUS SEARCH
     C*
     C     HFLD          WHENEQ    'SCIMST'
     C                   MOVE(P)   SCIMST        HLIMST            2
     C                   CLEAR                   ##RRN
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##RRN             9 0
     C                   PARM      'IMST'        ##TBID
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVEL(P)  HLIMST        SCIMST
     C                   SETON                                        9039      pos cursor
     C                   ELSE
     C     ##RRN         CHAIN     PGT02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTKEY         SCIMST
     C                   MOVEL(P)  GTDESC        SCIMDS
     C                   SETOFF                                       91        error if blank
     C                   SETON                                        9040      pos cursor
     C                   ENDIF
     C                   ENDIF
     C*
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
     C                   SETON                                        9040      pos cursor
     C                   ELSE
     C     ##RRN         CHAIN     PSA03                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  SACTY         SCRCTY
     C                   MOVEL(P)  SAST          SCRST
     C                   SETOFF                                       9293      error if blank
     C                   SETOFF                                       889495     invalid value
     C                   SETON                                        9041      pos cursor
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
     C                   SETON                                        9041      pos cursor
     C                   ELSE
     C     ##RRN         CHAIN     PSA03                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  SAST          SCRST
     C                   MOVEL(P)  SACTY         SCRCTY
     C                   SETOFF                                       9293      error if blank
     C                   SETOFF                                       889495     invalid value
     C                   SETON                                        9041      pos cursor
     C                   ENDIF
     C                   ENDIF
     C*
     C*
     C                   OTHER
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCDFC'
     C                   SETON                                        61
     C     HFLD          WHENEQ    'SCACCT'
     C                   SETON                                        62
     C     HFLD          WHENEQ    'SCLIC#'
     C                   SETON                                        63
     C     HFLD          WHENEQ    'SCEDI'
     C                   SETON                                        64
     C     HFLD          WHENEQ    'SCSVZP'
     C                   SETON                                        85
     C     HFLD          WHENEQ    'SCRFLC'
     C                   SETON                                        7781
     C     HFLD          WHENEQ    'SCATLC'
     C                   SETON                                        8283
     C*** Beg Add *** 07/18/25 ***************************************
     C     HFLD          WHENEQ    'SCSNPI'
     C                   EVAL      MSSNPI = 'MCR0676'
     C                   EVAL      ERSNPI = *ON
     C*** End Add *** 07/18/25 ***************************************
     C                   ENDSL
     C*
     C                   SETON                                        90
     C*
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
     C                   CLEAR                   SCDFC
     C                   CLEAR                   SCDIA1
     C                   CLEAR                   SCDIA2
     C                   CLEAR                   SCDIA3
     C                   CLEAR                   SCDIA4
     C                   CLEAR                   SCD1DS
     C                   CLEAR                   SCD2DS
     C                   CLEAR                   SCD3DS
     C                   CLEAR                   SCD4DS
     C                   CLEAR                   SCACCT
     C                   CLEAR                   SCCHRG
     C                   CLEAR                   SCMRA
     C                   CLEAR                   SCNTWK
     C                   CLEAR                   SCOTHR
     C                   CLEAR                   SCOTH2
     C                   CLEAR                   SCPAID
     C                   CLEAR                   SCSNPI
     C                   CLEAR                   SCRNPI
     C                   CLEAR                   SCIMST
     C                   CLEAR                   SCRCTY
     C                   CLEAR                   SCRST
     C                   CLEAR                   SCIMDS
     C                   CLEAR                   SCRFLC
     C                   CLEAR                   SCATLC
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
     C* Load service zip code
     C*
     C                   MOVE      *BLANKS       SCSVZP
     C*
     C     EOFLIC        IFNE      *BLANKS
     C*** Beg Chg *** 09/18/12 *************************************************
     C********           MOVEL     EOFLIC        SCSVZP
      /Free
       ##key = Eoast;
       Chain Stky Pst02;
       If %found(Ststl1) and Stzip4 = 'Y';
          Scsvzp = %dec(%subst(Eoflic:1:5):5:0);
          If (%subst(Eoflic:6:4) = *blanks);
             Sczip = 00000;
          Else;
             Sczip = %dec(%subst(Eoflic:6:4):4:0);
          Endif;
       Else;
          Scsvzp = %dec(Eoflic:5:0);
       Endif;
      /End-Free
     C*** End Chg *** 09/18/12 *************************************************
     C                   ELSE
     C     P1KY          CHAIN     PP101                              99
     C  N99              MOVEL     P1ZIP         SCSVZP
     C                   ENDIF                                                  *IN99=OFF
     C*
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
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C*
     C                   MOVE(P)   EHACCT        SCACCT
     C                   MOVEL(P)  EHFLIC        SCLIC#
     C*
     C     EHEDI         IFNE      *BLANKS
     C                   MOVE      EHEDI         SCEDI
     C                   ENDIF                                                  EHEDI NE *BLANKS
     C*
     C                   Z-ADD     EOCHRG        SCCHRG
     C                   Z-ADD     EOMRA         SCMRA
     C                   Z-ADD     EONDIS        SCNTWK
     C                   Z-ADD     EOOTHR        SCOTHR
     C                   Z-ADD     EOOTH2        SCOTH2
     C                   Z-ADD     EOPAID        SCPAID
     C                   MOVEL     EHSNPI        SCSNPI
     C                   MOVEL     EHRNPI        SCRNPI
     C                   MOVEL(P)  EHRNLC        SCRNLC
     C                   MOVEL(P)  EHRFLC        SCRFLC
     C                   MOVEL(P)  EHATLC        SCATLC
     C                   MOVEL     EHIMST        SCIMST
     C                   MOVEL     EHRCTY        SCRCTY
     C                   MOVEL     EHRST         SCRST
     C                   MOVEL     'IMST'        ##TBID
     C                   MOVEL(P)  SCIMST        ##KY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCIMDS
     C                   ENDIF
     C*
     C*
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
             scICdind= EHicdind;
          // Beg Add *** 08/05/13 ****************************************
          Elseif EHicdind ='N';
             scICdind = EHicdind;
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
     C*** End Chg *** 05/23/13 ****************************************
     C                   MOVE(P)   SCACCT        EHACCT
     C                   MOVEL(P)  SCLIC#        EHFLIC
     C                   MOVE      SCEDI         EHEDI
     C                   MOVE      SCSPCD        EOSPCD
     C                   MOVEL     SCSNPI        EHSNPI
     C                   MOVEL     SCRNPI        EHRNPI
     C                   MOVEL(P)  SCRNLC        EHRNLC
     C                   MOVEL(P)  SCRFLC        EHRFLC
     C                   MOVEL(P)  SCATLC        EHATLC
     C                   MOVEL     SCIMST        EHIMST
     C                   MOVEL     SCRCTY        EHRCTY
     C                   MOVEL     SCRST         EHRST
     C                   MOVEL     XXCBSA        EHCBSA
     C*
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
     C                   ENDIF                                                  *INKC = *ON
     C*
     C* CALL 'ADJUDICATION CALL' PROGRAM IF IN UNPEND MODE
     C* THIS IS DONE BECAUSE OTHER BILLS MAY HAVE BEEN PROCOSSED
     C* FOR THIS CASE SINCE THIS BILL HAS BEEN ADJUDICATED
     C*
     C     EOBLST        IFEQ      'U'
     C     *INKL         ANDNE     *ON
     C     ##RTCD        ANDNE     'P'
     C                   Z-ADD     @@RRN2        ##RRN
     C                   CALL      'FA713R'
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   ENDIF                                                  EOBLST = 'U'
     C*
     C                   SETON                                        LR
     C                   MOVE      '1'           OUTFLG
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*
     C** Get standard screen headings
     C*
     C     @@RRN2        CHAIN(N)  PEO01                              99
     C                   Z-ADD     EOEOB#        SCEOB#
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
       //*** Beg Add *** 09/18/12 **********************************************
       ##Key = Eoast;
       Chain Stky Pst02;
       If %Found(Ststl1) and Stzip4 = 'Y';
         *In42 = *On;
       Endif;
       //*** End Add *** 09/18/12 **********************************************
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
     C                   MOVEL     EOAST         ##KEY
     C     STKY          CHAIN     PST01                              99
     C     *IN99         IFEQ      *OFF
     C     ST1A4         ANDEQ     'Y'
     C                   SETON                                        30
     C                   MOVE      EOAST         KKAST
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
     C* SET INITIAL CURSOR POSITON TO SERVICE ZIP
     C*
     C                   SETON                                        38
     C*
     C* PROTECT SPECIALTY IF EORCN>0 AND EOBTYP<>'P'
     C*
     C     EORCN         IFGT      *ZERO
     C                   SETON                                        29
     C                   ELSE
     C*
     C                   SETOFF                                       29
     C                   ENDIF                                                  EORCN>0
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
     C*** Beg Add *** 09/18/12 *************************************************
      /Free
       Clear Eoflic;
      /End-free
     C*** End Add *** 09/18/12 *************************************************
     C*
     C     SCSVZP        IFEQ      *ZEROS
     C                   MOVEL     '     '       EOFLIC
     C     P1KY          CHAIN     PP101                              99
     C  N99              MOVEL     P1ZIP         SCSVZP
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
     C*** Beg Add *** 09/18/12 *************************************************
      /Free
       ##key = Eoast;
       Chain Stky Pst02;
       If %found(Ststl1) and Stzip4 = 'Y';
          Eoflic = %trim(Eoflic) + %char(Sczip);
       Endif;
      /End-Free
     C*** End Add *** 09/18/12 *************************************************
     C*
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
     C     SCSVZP        IFEQ      *ZEROS
     C     P1KY          CHAIN     PP101                              99
     C  N99              MOVEL     P1ZIP         SCSVZP
     C                   ENDIF
     C*** Beg Add *** 09/18/12 ****************************************
      /Free
       Clear Eoflic;
      /End-Free
     C*** End Add *** 09/18/12 ****************************************
     C                   MOVEL     SCSVZP        EOFLIC
     C*** Beg Add *** 07/18/25 ***************************************
     C                   MOVE      SCZIP         EOFLIC
     C*** End Add *** 07/18/25 ***************************************
     C*** Beg Add *** 09/18/12 *************************************************
      /Free
       ##key = Eoast;
       Chain Stky Pst02;
       If %found(Ststl1) and Stzip4 = 'Y';
          Eoflic = %trim(Eoflic) + %char(Sczip);
       Endif;
      /End-free
     C*** End Add *** 09/18/12 *************************************************
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
