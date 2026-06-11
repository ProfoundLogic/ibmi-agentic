     Hcopyright('(c) 2025 Sapiens, Inc. All rights reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H*  Program: F0361RCA                                            *
     H*  Description: Hospital (UB) Header Information                *
     H*  Created: 11/30/98  C:56 Project: MC152                       *
     H*  Copied: 01/11/07
     H*****************************************************************
     H*                                                               *
     H*     # # # # # IMPORTANT PROGRAMMERS NOTE # # # # #            *
     H*                                                               *
     H* CHANGES TO THIS PROGRAM SHOULD ALSO BE MADE TO STATE SPECIFIC *
     H* PROGRAMS AS WELL (F0361XX) WHERE XX IS THE STATE IF NECESSARY *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS
     H*                                                               *
     H*  07/18/25  C:342 Project MC451                                *
     H*                  NCCI Med Data Call Enhancements              *
     H*                                                               *
     H*  10/01/15  C:079 PHW37605                                     *
     H*                  Check ICD-10 Indicator based on date and     *
     H*                                                               *
     H*  09/17/15 C:079 Support: 099/37167                            *
     H*                 Add type to ICD-10 Key list for editing       *
     H*
     H*  08/05/13 C:277 PROJECT: MC329 Phase II
     H*                 Add search functionality and Validate ICD10
     H*
     H*  06/24/11 C:079 Project:SY255
     H*                 Set EOTAPE = Y
     H*
     H*  05/23/13 C:277 PROJECT: MC329
     H*                 Activate the ICD10 fields if ICD10 is active
     H*
     H*  04/05/11 C:257 Support:71931
     H*                 Assigning RRN to @@RRN after WRITEDBRCDF to
     H*                 resolve duplicate records in file FAEHP.
     H*
     H*  08/21/08 C:180 PROJECT:MC294 PRISS 22
     H*                         Do not underline NPI# fields when
     H*                         in display mode. - DSPF changes only.
     H*
     H*  08/11/08  C:47   Project: MC321
     H*                   ADDED: FACILITY PROV AND NAME
     H*                   REND PROV LAT/GROUP,
     H*                   BILLING PROV UNIQUE BILL ID,
     H*                   BILLING PROV STATE LICENSE# TO SCR03.
     H*
     H*  07/15/08 C:180 PROJECT:MC294 Phase II
     H*                         Add NPI# fields to SCR02.
     H*
     H*  04/17/07 C:188 PROJECT:FL061
     H*                 SET EDITST TO ADJ OR POLICY STATE BASED ON EOCPST
     H*
     H*  01/11/07 C:188 CA002: 837 CA Medical EDI
     H*                 Implement CA 837 Medical EDI Requirements
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
     FF0361FCA  CF   E             WORKSTN
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
     F*  PROCEDURE CODE MASTER FILE
     FFMPRL     IF   E           K DISK
     F*  RESONABLE AND CUSTOMARY FILE
     FFMRCL1    IF   E           K DISK
     F                                     RENAME(PRC01:PRC02)
     F*** Beg Add *** 08/05/13 ***************************************
     Ffmicd10l1 if   e           k disk
     F*** End Add *** 08/05/13 ***************************************
     F*** Beg Add *** 07/18/25 ***************************************
     F*  Provider Auxillary File
     FFMP1L     IF   E           K DISK
     F*** End Add *** 07/18/25 ***************************************
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDSA          DS
     D  DBRRNA               397    400B 0
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Hospital (UB) Header-
     D                                      Information')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(3)
     D VLDCHR          C                   CONST('ABCDEFGHIJKLMNOPQRST-
     D                                     UVWXYZ0123456789 ')

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
     C     PDKY2         KLIST
     C                   KFLD                    KKFEIN
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
     C*** Beg Add *** 08/05/13 ***************************************
     C*** KEY LIST FOR DIAGNOSIS CODE MASTER FILE
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
     C     *LIKE         DEFINE    PRCPT4        KKCPT4
     C     *LIKE         DEFINE    PREFDT        KKEFDT
     C     *LIKE         DEFINE    EOAST         EDITST
     C     *LIKE         DEFINE    EOFEIN        HLFEIN
     C     *LIKE         DEFINE    EOMOD         XXFMOD
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
     C*
     C                   SETOFF                                       262827
     C                   SETOFF                                       9542
     C*
     C**  SPECIALTY CODE IS REQUIRED AND VALID
     C*
     C     SCSPCD        IFEQ      *BLANKS
     C                   SETON                                        9067
     C                   GOTO      CK1END
     C                   ENDIF
       //*** Beg add ***** 07/18/25 *************************************
        If isModEnabled('NCCI Med Data Call');
           If scfzip = *zeros and EOrcn = 0;
              erfzip = *on;
              msfzip = 'MCR0503';
              *in90 = *on;
           Endif;
        Endif;
       //*** End add ***** 07/18/25 *************************************
     C*
     C                   MOVEL(P)  SCSPCD        ##KEY
     C     SPKY          CHAIN     PSP01                              99
     C*
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9070
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C** PATIENTS ACCOUNT NUMBER IS REQUIRED
     C*
     C     SCACCT        IFEQ      *BLANKS
     C                   SETON                                        9092
     C                   GOTO      CK1END
     C                   ENDIF
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
     C                   MOVE      'D'           KKLOST
     C     SCDIA1        IFNE      *BLANKS
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxicd10 = '1' and scICdInd ='Y';
          kkICd10 = SCDIA1;
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
     C* TYPE OF BILL IS REQUIRED
     C*
     C     SCBLTY        IFEQ      *BLANKS
     C                   SETON                                        6590
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C* VALIDATE TYPE OF BILL
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
     C*
     C* TYPE OF SERVICE IS REQUIRED FOR CALIFORNIA
     C*
     C     SCICN         IFEQ      *BLANKS
     C                   SETON                                        6690
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C* VALIDATE BOTH TYPES OF SERVICE
     C*
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
     C** DRG/CONT CODES REQUIRED WHEN TYPE OF SVC IS IM-IS, 1-999
     C*
     C     SCICN         IFEQ      'IM'
     C     SCICN         OREQ      'IP'
     C     SCICN         OREQ      'IR'
     C     SCICN         OREQ      'IS'
     C*
     C     SCDRG#        IFLT      1
     C     SCDRG#        ORGT      999
     C                   SETON                                        9056
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C     SCCONT        IFNE      '01'
     C     SCCONT        ANDNE     '09'
     C                   SETON                                        9057
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C** DRG/CONT CODES SHOULD BE BLANK IF NON-INPATIENT
     C*
     C     SCDRG#        IFNE      0
     C                   SETON                                        9093
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C     SCCONT        IFNE      *BLANKS
     C                   SETON                                        9094
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*
     C     CK1END        ENDSR
     C*
     C*================================================================
     C* ERRCK2 - Error checking for screen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C                   SETOFF                                       33
     C*
     C* RENDERING PROVIDER STATE LIC#
     C*
     C     SCLIC#        IFEQ      *BLANKS
     C                   SETON                                        9061
     C                   GOTO      CK2END
     C                   ENDIF
     C*
     C* PROCEDURE CODE METHOD IS REQUIRED
     C*
     C     SCPCM         IFEQ      *BLANKS
     C                   SETON                                        9062
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
     C                   SETON                                        9063
     C                   GOTO      CK2END
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
     C* VALIDATE PROCEDURE CODES BASE ON PROCEDURE CODE METHODS ENTERED
     C* IF PROC CODE METHOD NE 0 THEN PRINC PROCEDURE CODE REQUIRED
     C     SCPCM         IFNE      '0'
     C*
     C     SCPCM         IFEQ      '4'
     C                   MOVEL     SCPRC1        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *ON
     C                   MOVEL     *BLANKS       SCP1DS
     C                   MOVEL     *BLANKS       SCLOS1
     C                   SETON                                        9073
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCPCM=4
     C*
     C     SCPCM         IFEQ      '5'
     C                   MOVEL     SCPRC1        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  SCPRC1        KKCPT4
     C     RCKY          CHAIN     PRC02                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL     *BLANKS       SCP1DS
     C                   MOVEL     *BLANKS       SCLOS1
     C                   SETON                                        9073
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCPCM=5
     C                   ENDIF
     C*
     C     SCPCM         IFEQ      '9'
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
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   MOVEL(P)  SCPRC1        KKICD9
     C                   MOVE      SCLOS1        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL     *BLANKS       SCP1DS
     C                   MOVEL     *BLANKS       SCLOS1
     C                   SETON                                        9073
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
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
     C                   ENDIF                                                  SCPRC2 NE *BLANKS
     C*
     C                   ENDIF                                                  SCPCM NE 0
     C*
     C* PRINCIPLE PROCEDURE CODE IS REQUIRED IF EUICN = AS, IS, OR HS
     C*
     C     EUICN         IFEQ      'AS'
     C     EUICN         OREQ      'IS'
     C     EUICN         OREQ      'HS'
     C     SCPRC1        IFEQ      *BLANKS
     C                   SETON                                        9074
     C                   GOTO      CK2END
     C                   ENDIF
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   ENDIF
     C*
     C* PRINCIPLE PROCEDURE DATE IS REQUIRED IF PRINCIPLE PROC IS NOT BLANK
     C*
     C     SCPRC1        IFNE      *BLANKS
     C     SCPRD1        ANDEQ     *ZEROS
     C                   SETON                                        9023
     C                   GOTO      CK2END
     C                   ENDIF
     C*
     C* VALIDATE THE PRINCIPLE PROCEDURE DATE
     C*
     C     SCPRD1        IFNE      0
     C*
     C                   Z-ADD     SCPRD1        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9021
     C                   GOTO      CK2END
     C                   ELSE
     C                   MOVEL     SRDTO         SCPRD1
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* VALIDATE THE OTHER DIAGNOSIS CODES
     C*
     C     SCDIA2        IFNE      *BLANKS
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxicd10 = '1' and scICdInd ='Y';
          kkICd10 = SCDIA2;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If not %found(fmicd10l1)
                or ICstat = 'I';
                *in90 = *on;
                *in19 = *on;
                leavesr;
             Else;
                *in19 = *off;
             Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C*** Beg Chg *** 05/23/13 ***************************************
     C*****              MOVEL     SCDIA2        KKICD9
     C                   MOVEL(P)  SCDIA2        KKICD9
     C*** End Chg *** 05/23/13 ***************************************
     C                   MOVE      'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        9019
     C                   GOTO      CK2END
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
       If xxicd10 = '1' and scICdInd ='Y';
          kkICd10 = SCDIA3;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If not %found(fmicd10l1)
                or ICstat = 'I';
                *in90 = *on;
                *in18 = *on;
                leavesr;
             Else;
                *in18 = *off;
             Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVEL     SCDIA3        KKICD9
     C                   MOVEL(P)  SCDIA3        KKICD9
     C*** End Chg *** 05/23/13 ***************************************
     C                   MOVE      'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        9018
     C                   GOTO      CK2END
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
       If xxicd10 = '1' and scICdInd ='Y';
          kkICd10 = SCDIA4;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If not %found(fmicd10l1)
                or ICstat = 'I';
                *in90 = *on;
                *in17 = *on;
                leavesr;
             Else;
                *in17 = *off;
             Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVEL     SCDIA4        KKICD9
     C                   MOVEL(P)  SCDIA4        KKICD9
     C*** End Chg *** 05/23/13 ***************************************
     C                   MOVE      'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        9017
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   ENDIF                                                  SCDIA1 NE *BLANKS
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
     C*
     C     CK2END        ENDSR
     C*
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C*
     C                   SETOFF                                       241112
     C                   SETOFF                                       229660
     C                   SETOFF                                       2697
     C                   SETOFF                                       1314
     C*
     C*
     C* If bill type = H (hospital), provider required.
     C*
     C     SCFFEI        IFEQ      *ZERO
     C     EOBTYP        ANDEQ     'H'
     C                   SETON                                        9011
     C                   GOTO      CK3END
     C                   ENDIF
     C*
     C* CA: Validate facility provider
     C*
     C     SCFFEI        IFNE      *ZERO
     C                   Z-ADD     SCFFEI        KKFEIN
     C     PDKY2         CHAIN     PPD01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9012
     C                   GOTO      CK3END
     C                   ENDIF
     C                   ENDIF
     C*
     C* REND PROV LAST/GROUP NAME
     C*
     C     SCRNAM        IFEQ      *BLANK
     C                   SETON                                        9022
     C                   GOTO      CK3END
     C                   ENDIF
     C*
     C* CA: Billing provider unique bill ID req'd
     C*
     C     SCBLID        IFEQ      *BLANK
     C                   SETON                                        9026
     C                   GOTO      CK3END
     C                   ELSE
     C     VLDCHR        CHECK     SCBLID        POS               2 0    95
     C     *IN95         IFEQ      '1'
     C                   SETON                                        9097
     C                   GOTO      CK3END
     C                   ENDIF
     C                   ENDIF
     C*
     C* CA: Billing prov state license #
     C*
     C     SCBLIC        IFEQ      *BLANK
     C                   SETON                                        9013
     C                   GOTO      CK3END
     C                   ELSE
     C     VLDCHR        CHECK     SCBLIC        POS               2 0    95
     C     *IN95         IFEQ      '1'
     C                   SETON                                        9014
     C                   GOTO      CK3END
     C                   ENDIF
     C                   ENDIF
     C*
     C*
     C*
     C     CK3END        ENDSR
     C*================================================================
     C* KJDO - F10=SEARCH.
     C*================================================================
     C     KJDO          BEGSR
     C*
     C*** INITIALIZE POSITION CURSOR INDICATORS
     C*
     C                   SETOFF                                       15
     C                   SETOFF                                       272823
     C                   SETOFF                                       313233
     C                   SETOFF                                       343536
     C                   SETOFF                                       374243
     C                   SETOFF                                       443946
     C                   SETOFF                                       2460
     C                   SETOFF                                       1112
     C*
     C*** CLEAR HOLD FIELDS
     C*
     C                   CLEAR                   HLDIA
     C*
     C                   SELECT
     C*** Beg Add *** 07/18/25 ***************************************
     C     HFLD          WHENEQ    'SCFZIP'
     C                   EVAL       *in90  =*on
     C                   EVAL       erfzip =*on
     C                   EVAL       msfzip ='MCR0676'
     C*** End Add *** 07/18/25 ***************************************
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
       *In73 = *Off;
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
     C                   SETON                                        9027
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C* PROCEDURE CODE METHOD SEARCH
     C*
     C     HFLD          WHENEQ    'SCPCM'
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
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      '0'
     C                   MOVEL(P)  GTKEY         SCPCM
     C                   MOVEL(P)  GTDESC        SCPDSC
     C                   SETON                                        9039      pos cursor
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C                   ELSE
     C                   SETON                                        9046      pos cursor
     C                   ENDIF
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
     C     SCPCM         IFEQ      '4'
     C     SCPCM         OREQ      '5'
     C                   MOVEL     SCPRC1        HLPRC1
     C                   MOVEL     SCP1DS        HLP1DS
     C                   MOVE      EDITST        ##ST              2
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
     C                   SETON                                        9025
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
             scP1ds = HLP1DS;
             *in90 = *on;
             *in39 = *on;
          Else;
             scPRc1 = outICD10;
             *in90 = *on;
             *in25 = *on;
          Endif;

          If scPRc1 <> *blanks;
             kkICd10 = scPRc1;
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
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP1DS
     C                   ENDIF                                                  *INPP = *OFF
     C                   SETON                                        9025
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
     C*  DID NOT MAKE A SELECTION
     C*
     C*
     C     SCPCM         IFEQ      '4'
     C     SCPCM         OREQ      '5'
     C                   MOVEL     SCPRC2        HLPRC2
     C                   MOVEL     SCP2DS        HLP2DS
     C                   MOVE      EDITST        ##ST              2
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
     C                   SETON                                        9025
     C                   ELSE
     C                   MOVEL(P)  ##CPT4        SCPRC2
     C                   MOVEL(P)  ##DESC        SCP2DS
     C                   MOVE      *BLANKS       SCLOS2
     C                   SETON                                        9043
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
             scP1ds = HLP2DS;
             *in90 = *on;
             *in25 = *on;
          Else;
             scPRc2 = outICD10;
             *in90 = *on;
             *in43 = *on;
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
     C                   SETON                                        9025
     C                   ELSE
     C                   MOVEL(P)  ##ICD9        SCPRC2
     C                   MOVE      ##LOST        SCLOS2
     C                   MOVEL(P)  SCPRC2        KKICD9
     C                   MOVE      SCLOS2        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP2DS
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
     C* OTHER DIAGNOSIS CODES SEARCHES
     C*
     C     HFLD          WHENEQ    'SCDIA2'
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   SCDIA2        HLDIA             5
     C                   MOVEL(P)  SCDIA2        HLDIA             7
     C*** End Chg *** 05/23/13 ***************************************
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       *In19 = *Off;
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
             kkICD10 = scDIa2;
       //*** Beg Add *** 09/17/15 ****************************************
             kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
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
       *In18 = *Off;
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
       *In17 = *Off;
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
     C* FACILITY PROVIDER
     C*
     C     HFLD          WHENEQ    'SCFFEI'
     C                   MOVE      SCFFEI        HLFEIN
     C                   MOVE      '2'           ##CALL
     C                   MOVE      *BLANK        ##FEIN
     C                   MOVE      *BLANK        ##MOD
     C                   MOVE      *BLANK        ##RTCD
     C                   MOVE      *BLANK        ##TYPE
     C                   CALL      'F0032R'
     C                   PARM                    ##CALL            1
     C                   PARM                    ##FEIN            9
     C                   PARM                    ##MOD             3
     C                   PARM                    ##RTCD            1
     C                   PARM                    ##TYPE            1
     C                   PARM      'Y'           ##1N4             1            type 1 & 4 only
     C*
     C     ##RTCD        IFEQ      'A'
     C     ##RTCD        OREQ      'B'
     C     ##FEIN        OREQ      *BLANK
     C                   MOVE      HLFEIN        SCFFEI
     C                   SETON                                        9024      pos cursor
     C                   ENDIF
     C*
     C     ##RTCD        IFEQ      '*'
     C                   MOVE      ##FEIN        SCFFEI
     C                   MOVE      ##MOD         XXFMOD
     C*
     C*** GET PROVIDER NAME
     C*
     C                   MOVE      SCFFEI        KKFEIN
     C                   MOVE      XXFMOD        KKMOD
     C     PDKY          CHAIN     PPD01                              99
     C  N99              MOVEL(P)  PDNAME        SCFPNM
     C                   SETON                                        9060      pos cursor
     C                   ENDIF
     C*
     C                   OTHER
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCPRD1'
     C                   SETON                                        23
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
     C     HFLD          WHENEQ    'SCLIC#'
     C                   SETOFF                                       5154
     C                   SETON                                        49
     C     HFLD          WHENEQ    'SCDRG#'
     C                   SETOFF                                       49
     C                   SETON                                        28
     C     HFLD          WHENEQ    'SCCONT'
     C                   SETOFF                                       28
     C                   SETON                                        15
     C     HFLD          WHENEQ    'SCPRC1'
     C                   SETOFF                                       15
     C                   SETON                                        32
     C     HFLD          WHENEQ    'SCPRC2'
     C                   SETOFF                                       32
     C                   SETON                                        33
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
     C     @@MODE        IFNE      'D'
     C                   SETON                                        36
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
     C  N99              MOVEL(P)  PDFLIC        SCLIC#
     C  N99              MOVEL(P)  PDFLIC        SCFLIC
     C                   ELSE
     C     PDKY          CHAIN     PPD01                              99
     C  N99              MOVEL     PDSPCD        SCSPCD
     C  N99              MOVEL(P)  PDFLIC        SCLIC#
     C  N99              MOVEL(P)  PDFLIC        SCFLIC
     C                   ENDIF
     C*
     C                   MOVEL     EOSPCD        SCSPCD
     C                   MOVEL(P)  EUFLIC        SCLIC#
     C                   MOVEL(P)  EUFLIC        SCFLIC
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
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
               scD1ds = ICDsc1;
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
     C                   Z-ADD     EUDRG#        SCDRG#
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
       If xxICD10 = '1' and EUicdind ='Y';
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
     C*
     C*** End Chg *** 08/05/13 ***************************************
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       If xxICD10 = '1' and EUicdind ='Y';
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
     C                   MOVEL     EUDIA2        SCDIA2
     C*** Beg Chg *** 08/05/13 ***************************************
     C     SCDIA2        IFNE      *BLANKS
     C     EUicd1002     ORNE      *BLANKS
     C*** End Chg *** 08/05/13 ***************************************
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       If xxICD10 = '1' and EUicdind ='Y';
          KKICD10 = EUicd1002;
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
     C     SCDIA3        IFNE      *BLANKS
     C     EUicd1003     ORNE      *BLANKS
     C*** End Chg *** 08/05/13 ***************************************
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       If xxICD10 = '1' and EUicdind ='Y';
          KKICD10 = EUicd1003;
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
     C                   ENDIF                                                  SCDIA4 = *BLANKS
     C*
     C                   MOVEL     EUDIA4        SCDIA4
     C*** Beg Chg *** 08/05/13 ***************************************
     C     SCDIA4        IFNE      *BLANKS
     C     EUicd1004     ORNE      *BLANKS
     C*** End Chg *** 08/05/13 ***************************************
      /Free
       If xxICD10 = '1' and EUicdind ='Y';
          KKICD10 = EUicd1004;
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
     C                   Z-ADD     EOCHRG        SCCHRG
     C                   Z-ADD     EOMRA         SCMRA
     C                   Z-ADD     EONDIS        SCNTWK
     C                   Z-ADD     EOOTHR        SCOTHR
     C                   Z-ADD     EOOTH2        SCOTH2
     C                   Z-ADD     EOPAID        SCPAID
     C                   MOVEL(P)  EOCONT        SCCONT
     C*
     C                   Z-ADD     EUPRD1        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPRD1
     C*
     C                   MOVE      EUANPI        SCANPI
     C                   MOVE      EUONPI        SCONPI
     C                   MOVE      EUNPI1        SCNPI1
     C                   MOVE      EUNPI2        SCNPI2
     C*
     C                   ENDIF                                                  @@MODE NE 'A'
     C*** Beg Add *** 05/23/13 ****************************************
      /Free
          //Beg Chg *** 08/05/13 ****************************************
          //scICdind = 'N';
          //If xxicd10 = *on;
          If xxicd10 = *on and EUicdind ='Y';
          //End Chg *** 08/05/13 ****************************************
             scDIa1  = EUICD1001;
             scDIa2  = EUICD1002;
             scDIa3  = EUICD1003;
             scDIa4  = EUICD1004;
             scICdind= EUICDind ;
          //Beg Chg *** 08/05/13 ****************************************
             If EUPCM = '9';
                scPRc1  = EUPRC1001;
                scPRc2  = EUPRC1002;
             Else;
                scPRc1  = EUPRC1;
                scPRc2  = EUPRC2;
             EndIf;
          Elseif EUICDind ='N';
             scICdind = EUICDind;
          //End Chg *** 08/05/13 ****************************************
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
     C*** Beg Chg *** 05/23/13 ***************************************
     C********           MOVE(P)   SCDIA1        EUDIA1
     C                   MOVEL(P)  SCDIA1        EUDIA1
     C*** End Chg *** 05/23/13 ***************************************
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
     C                   MOVEL     SCDCN         EUDCN
     C                   MOVE      SCSPCD        EOSPCD
     C*
     C                   MOVEL(P)  SCCONT        EOCONT
     C                   MOVEL(P)  SCCONT        EUCTCD
     C                   Z-ADD     SCDRG#        EUDRG#
     C*
     C     SCPCM         IFNE      '9'
     C                   MOVE      *BLANKS       SCLOS1
     C                   MOVE      *BLANKS       SCLOS2
     C                   ENDIF
     C*
     C                   MOVE      EOFEIN        KKFEIN
     C                   MOVE      EOMOD         KKMOD
     C*
     C                   MOVEL     EOSPCD        SCSPCD
     C*
     C                   MOVE      SCEDI         EUEDI
     C                   MOVEL(P)  SCPCM         EUPCM
     C                   MOVEL(P)  SCLIC#        EULIC#
     C                   MOVEL(P)  SCLIC#        EUFLIC
     C*
     C                   Z-ADD     SCPRD1        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EUPRD1
     C*
     C                   MOVEL(P)  SCPRC1        EUPRC1
     C                   MOVE      SCLOS1        EUPRC1
     C                   MOVEL(P)  SCPRC2        EUPRC2
     C                   MOVE      SCLOS2        EUPRC2
     C                   MOVEL(P)  SCDIA2        EUDIA2
     C                   MOVEL(P)  SCDIA3        EUDIA3
     C                   MOVEL(P)  SCDIA4        EUDIA4
     C                   MOVE      SCSPCD        EOSPCD
     C*
     C                   MOVE      SCANPI        EUANPI
     C                   MOVE      SCONPI        EUONPI
     C                   MOVE      SCNPI1        EUNPI1
     C                   MOVE      SCNPI2        EUNPI2
     C*** Beg Add *** 05/23/13 ****************************************
      /Free
          //Beg Chg *** 08/05/13 ****************************************
          EUICDind  = scICdind;
          If xxicd10 = *on and scICdInd ='Y';
          //End Chg *** 08/05/13 ****************************************
             EUICD1001 = scDIa1;
             EUICD1002 = scDIa2;
             EUICD1003 = scDIa3;
             EUICD1004 = scDIa4;
          //Beg Chg *** 08/05/13 ****************************************
             If SCPCM = '9';
                EUPRC1001 = scPRc1;
                EUPRC1002 = scPRc2;
                EUPRC1 = *Blanks;
                EUPRC2 = *Blanks;
             Else;
                EUPRC1001 = *Blanks;
                EUPRC1002 = *Blanks;
                EUPRC1 = scPRc1;
                EUPRC2 = scPRc2;
             EndIf;
          //End Chg *** 08/05/13 ****************************************

             // If ICD10 is Active then no need of ICD9 fields

             EUDIA1 = *Blanks;
             EUDIA2 = *Blanks;
             EUDIA3 = *Blanks;
             EUDIA4 = *Blanks;
          //Beg Chg *** 08/05/13 ****************************************
          Else;
             EUDIA1 = scDIa1;
             EUDIA2 = scDIa2;
             EUDIA3 = scDIa3;
             EUDIA4 = scDIa4;

             // If ICD09 is Active then no need of ICD10 fields

             EUICD1001 = *Blanks;
             EUICD1002 = *Blanks;
             EUICD1003 = *Blanks;
             EUICD1004 = *Blanks;
             EUPRC1001 = *Blanks;
             EUPRC1002 = *Blanks;
          //End Chg *** 08/05/13 ****************************************
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
     C                   SETOFF                                       45
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
     C                   Z-ADD     EOFFEI        SCFFEI
     C     SCFFEI        IFNE      *ZEROS
     C                   MOVE      SCFFEI        KKFEIN
     C     PDKY          CHAIN     PPD01                              99
     C  N99              MOVEL(P)  PDNAME        SCFPNM
     C                   ENDIF                                                  SCFFEI IFNE ZEROS
     C                   MOVEL(P)  EORNAM        SCRNAM
     C                   MOVEL(P)  EOBLID        SCBLID
     C                   MOVEL(P)  EOBLIC        SCBLIC
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
     C                   MOVEL(P)  SCCONT        EUCTCD
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
     C                   MOVE      'Y'           EOTAPE
     C                   Z-ADD     SCFFEI        EOFFEI
     C                   MOVEL(P)  SCRNAM        EORNAM
     C                   MOVEL(P)  SCBLID        EOBLID
     C                   MOVEL(P)  SCBLIC        EOBLIC
     C                   MOVEL     Q1USER        EOUSER
     C                   MOVEL     Q1PGM         EOPGM
     C                   Z-ADD     DSTIME        EOTIME
     C                   MOVEL     SRDTO         EODT
     C                   MOVEL(P)  SCCONT        EOCONT
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
     C                   MOVE      'Y'           EOTAPE
     C                   MOVE      SCCONT        EOCONT
     C                   Z-ADD     SCFFEI        EOFFEI
     C                   MOVEL(P)  SCRNAM        EORNAM
     C                   MOVEL(P)  SCBLID        EOBLID
     C                   MOVEL(P)  SCBLIC        EOBLIC
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
