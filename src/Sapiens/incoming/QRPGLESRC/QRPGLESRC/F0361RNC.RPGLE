     Hcopyright('(c) 2025 Sapiens, Inc. All rights reserved.')
     H option(*nodebugio:*srcstmt)
     H**** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  Program: F0361RNC                                            *
     H*  Description: Hospital (UB) Header Information for state NC   *
     H*  Created: 02/26/14  C:296 Project: NC034                       *
     H*****************************************************************
     H*                                                               *
     H*     # # # # # IMPORTANT PROGRAMMERS NOTE # # # # #            *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS                                                    *
     H*                                                               *
     H*  07/18/25 C:342 Project MC451                                 *
     H*                 NCCI Med Data Call Enhancements               *
     H*                                                               *
     H*  09/30/15 C:079 Support: 146/37584                            *
     H*                 Load rendering address/city/state/zip         *
     H*                                                               *
     H*  09/17/15 C:079 Support: 099/37167                            *
     H*                 Add type to ICD-10 Key list for editing       *
    H*                                                               *
     H*  07/21/15 C:079 Project: NC039                                *
    H*                 Lift edit on DRG code if CAH                  *
    H*                 Load XXMFS correctly when STMFS = A           *
    H*                 Change edit on discharge date                 *
    H*                 Add screen 3 (Rendering fields for SCS)       *
    H*                                                               *
     H*  05/18/15 C:212 Project: NC039                                *
    H*                 multiple changes - add edits, provide search etc
     H*                                                               *
     H*****************************************************************
     H*
     FF0361FNC  CF   E             WORKSTN
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
     Ffmicd10l1 if   e           k disk
     F*
     F*  PROCEDURE CODE MASTER FILE
     FFMPRL     IF   E           K DISK
     F*
     F*  RESONABLE AND CUSTOMARY FILE
     FFMRCL1    IF   E           K DISK
     F                                     RENAME(PRC01:PRC02)
     D******************************************************************
     D* Definition Specification starts here -
     D******************************************************************
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
     D*** Beg Chg *** 07/21/15 ****************************************
     D**** #OFSCR          C                   CONST(2)
     D #OFSCR          s              1  0
     D*** End Chg *** 07/21/15 ****************************************
     D SA015R          pr                  extpgm('SA015R')
     D @@rtcd                         1
     D*
     D xxicd10         s              1
     D @@rtcd          s              1
     D XXRGN#          S              3  0
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
     D VLDCHR          C                   CONST('ABCDEFGHIJKLMNOPQRST-
     D                                     UVWXYZ0123456789 ')
     D XXCBSA          s              5A
     D***** Beg Add *** 05/18/15 *******************************************
     D XXDCDT          s              7  0 inz
     D***** End Add *** 05/18/15 *******************************************
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
     C*    IIKY          KLIST
     C*                  KFLD                    KKPROV
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
     C*** KEY LIST FOR DIAGNOSIS CODE MASTER FILE
     C     fmicd10l1key  klist
     C                   kfld                    kkicd10
     C*** Beg Add *** 09/17/15 ***************************************
     C                   kfld                    kkLost
     C*** End Add *** 09/17/15 ***************************************
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
     C     RCKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKCPT4
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
     C     *LIKE         DEFINE    EOAST         EDITST
     C     *LIKE         DEFINE    DGDRG#        KKDRG#
     C     *LIKE         DEFINE    DGDRG#        HLDRG
     C     *LIKE         DEFINE    IIPROV        HLPROV
     C*    *LIKE         DEFINE    IIPROV        KKPROV
     C***** Beg Add *** 05/18/15 *******************************************
     C     *LIKE         DEFINE    IIPROV        KKPROV
     C***** End Add *** 05/18/15 *******************************************
     C     *LIKE         DEFINE    DGDESC        KKDESC
     C*    *LIKE         DEFINE    OPPROV        K1PROV
     C     *LIKE         DEFINE    STMFS         XXMFS
     C     *LIKE         DEFINE    SCPCM         HLPCM
     C     *LIKE         DEFINE    SCPDSC        HLDESC
     C     *LIKE         DEFINE    PRCPT4        KKCPT4
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
     C***** Beg Add *** 05/18/15 *******************************************
      /free
         erDrg = *off;
         msDrg = *blanks;
         erDcdt = *off;
         msDcdt = *blanks;
      /end-free
     C***** End Add *** 05/18/15 *******************************************
       //*** Beg add ***** 07/18/25 ***********************************
       If isModEnabled('NCCI Med Data Call');
          If scfzip = *zeros and EOrcn = 0;
             *in92 = *on;
             *in90 = *on;
             leavesr;
          Endif;
       Endif;
       //*** End add ***** 07/18/25 ************************************
     C* Patient account number cannot be blank.
      /Free
         if SCACCT = ' ' ;
            *in91 = '1' ;
            *in90 = '1' ;
      /End-free
     C                   GOTO      CK1END
      /Free
         endif ;
      /End-free
     C*
     C** SPECIALTY CODE MUST BE VALID IF ENTERED AND NOT A RECON
     C*
     C     SCSPCD        IFNE      *BLANKS
     C     *IN29         ANDEQ     *OFF
     C     EDITST        OREQ      'NC'
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
     C*** IF KY BILL AND EDI IS INSTALLED PAT ACCT IS REQD
     C*
     C*
     C* PRINCIPAL DIAGNOSIS CODE IS REQUIRED
     C*
     C*
     C* VALIDATE TYPE OF BILL
     C*
     C     SCBLTY        IFEQ      *BLANKS
     C     EDITST        ANDEQ     'NC'
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
     C                   SETON                                        90
     C                   eval      indSCpf = '1'
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     SRDTO         SCSCPF
     C                   ENDIF                                                  SRERR = '1'
     C                   ENDIF                                                  SCSCPF NE *ZEROS
     C*
     C* VALIDATE THE STATEMENT COVERS TO DATE
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
     C* TYPE OF ADMISSION IS REQUIRED.
     C*
     C     SCADTY        IFEQ      *BLANKS
     C                   SETON                                        90
     C                   Eval      indADTY = '1'
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCADTY EQ BLANK
     C* VALIDATE ADMISSION DATE IF ENTERED
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
     C                   SETON                                        90
     C                   eval      indADDt = '1'
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
     C***** Beg Add *** 05/18/15 *******************************************
     C/free

         // Validation for Codes
         monitor;
           xxDCDT = %dec(%char(%date(SCDCDT:*USA):*cymd0):7:0);
         on-error;
         //*** Beg Add *** 07/21/15 ******************************
           monitor;
             xxDCDT = %dec(%char(%date(SCDCDT:*MDY):*cymd0):7:0);
           on-error;
             xxDCDT = *zeros;
           endmon;
         //*** End Add *** 07/21/15 ******************************
         //*** Beg Del *** 07/21/15 ******************************
         //***  xxDCDT = *zeros;
         //*** End Del *** 07/21/15 ******************************
         endmon;
         If xxDCDT >= 1150401 and
            (SCicn = 'IS' or SCicn = 'IM' or SCicn = 'IP' or SCicn = 'IR');

         // DRG code is required unless this is a CAH

         //*** Beg Chg *** 07/21/15 ***************************************
         //***  if SCdrg = *zeros;
           if SCdrg = *zeros and scSpCd <> 'CAH';
         //*** End Chg *** 07/21/15 ***************************************
              *in90 = '1' ;
              erDrg = *on;
              msDrg = 'This field cannot be blank';
           endif;

           if SCscpf = *zeros;
              *in90 = '1' ;
              *in95 = '1' ;
           endif;

           if SCscpt = *zeros;
              *in90 = '1' ;
              *in96 = '1' ;
           endif;

           if SCaddt = *zeros;
              *in90 = '1' ;
              *in61 = '1' ;
           endif;

         EndIf;

         // Discharge date should not be blanks for some scenario

         //*** Beg Del *** 07/21/15 ******************************
         //*******************************************************
         //****monitor;
         //****  xxDCDT = %dec(%char(%date(SCDCDT:*USA):*cymd0):7:0);
         //****on-error;
         //****  xxDCDT = *zeros;
         //****endmon;
         //*** End Del *** 07/21/15 ******************************
         if xxDCDT = *zeros and (xxMFS = 'A' or xxMFS = 'Y') and
            (SCicn = 'HO' or SCicn = 'HS' or SCicn = 'IS' or SCicn = 'IP');
           *in90 = '1' ;
           erDcdt = *on;
           msDcdt = 'This field cannot be blank';
         endif;

      /end-free
     C*
     C***** End Add *** 05/18/15 *******************************************
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
     C     EDITST        IFEQ      'NC'
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
     C                   SETON                                        9010
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
     C*****CLTID         IFEQ      '114'
     C*
     C     SCICN         IFEQ      'IR'
     C     XXMFS         ANDEQ     'Y'
     C*    SCPATS        ANDEQ     *BLANKS
     C*
     C                   SETON                                        9018
     C                   SETOFF                                       19
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C*    SCPATS        IFNE      *BLANKS
     C*
     C*                  MOVEL(P)  'PATS'        KKTBID
     C*                  MOVEL(P)  SCPATS        KKKEY
     C*
     C*    GTKY          CHAIN     PGT02                              99
     C*    *IN99         IFEQ      *ON
     C*                  SETON                                        9019
     C*                  SETOFF                                       18
     C*                  GOTO      CK1END
     C*                  ENDIF                                                              *IN99 =
     C*                  ENDIF                                                              SCPATS <
     C*
     C*****              ENDIF                                                              CLTID =
     C**** IF STATE IS DE
     C*
     C* Highlight SCPROV if PPO INSTALLED
     C     XXMPPO        IFEQ      'Y'
     C     XXBLTY        ANDNE     '83'
     C                   SETON                                        40
     C                   ENDIF
     C     XXBLTY        IFEQ      '83'
     C     XXMPPO        ANDEQ     'Y'
     C     EOAST         ANDEQ     'NC'
     C     *IN40         ANDEQ     *ON
     C                   SETOFF                                       40
     C                   ENDIF
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
     C     EOAST         ANDEQ     'NC'
     C     XXMFS         OREQ      'Y'
     C     SCSPCD        ANDEQ     'ASC'
     C                   SETON                                        97
     C                   ELSE
     C                   SETOFF                                       97
     C                   ENDIF
     C*
     C* VALIDATE TYPE OF ADMISSION
     C*
     C                   MOVEL(P)  'NCTA'        KKTBID
     C                   MOVEL(P)  SCADTY        KKKEY
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *ON
     C     GTSTAT        OREQ      'I'
     C                   SETON                                        9095
     C                   Eval      indADTY = '1'
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C*
     C     CK1END        ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C                   Setoff                                       264573
     C                   Setoff                                       141274
     C                   Setoff                                       776271
     C***** Beg Add *** 05/18/15 *******************************************
      /free
         erRcty = *off;
         msRcty = *blanks;
         erRst  = *off;
         msRst  = *blanks;
         erProv = *off;
         msProv = *blanks;
         indProvRqd = *off;
         erTrst = *off;
         msTrst = *blanks;
         erImst = *off;
         msImst = *blanks;
      /end-free
     C***** End Add *** 05/18/15 *******************************************
     C                   Eval      indPcm = '0'
     C                   Eval      indPRC1a = '0'
     C                   Eval      indDIA1a = '0'
     C                   Eval      indDIA3a = '0'
     C                   Eval      indDIA4a = '0'
     C                   Eval      indSCdAd = '0'
     C* PROCDURE CODE METHOD IS REQUIRED
     C*
     C     SCPCM         IFEQ      *BLANKS
     C                   SETON                                        90
     C                   Eval      indPcm = '1'
     C                   GOTO      CK2END
     C                   ENDIF                                                  SCPCM = *BLANKS
     C* CA: Billing provider unique bill ID req'd
     C*
     C*    SCBLID        IFEQ      *BLANK
     C*                  SETON                                        9026
     C*                  GOTO      CK2END
     C*                  ELSE
     C*    VLDCHR        CHECK     SCBLID        POS               2 0    95
     C*    *IN95         IFEQ      '1'
     C*                  SETON                                        9097
     C*                  GOTO      CK2END
     C*                  ENDIF
     C*                  ENDIF
     C*
     C     SCDIA1        IFEQ      *BLANKS
     C                   SETON                                        90
     C                   Eval      indDIA1a = '1'
     C                   GOTO      CK2END
     C                   ENDIF
     C*
     C** VALIDATE 1ST DIAGNOSIS CODE
     C*
     C     SCDIA1        IFNE      *BLANKS
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
       Else;
      /End-Free
     C********           MOVE(P)   SCDIA1        KKICD9
     C                   MOVEL(P)  SCDIA1        KKICD9
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        9071
     C                   GOTO      CK2END
     C                   ENDIF
      /Free
       Endif;
      /End-Free
     C                   ENDIF                                                  SCDIA1 NE *BLANKS
     C*
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
     C                   SETON                                        90
     C                   eval      indPRD1a = '1'
     C                   ENDIF
     C*
     C* IF PROC CODE METHOD =0 PRINCIPAL PROCEDURE CODE MUST BE BLANK
     C*
     C     SCPCM         IFEQ      '0'
     C     SCPRC1        ANDNE     *BLANKS
     C                   SETON                                        90
     C                   eval      indPRC1a = '1'
     C                   GOTO      CK2END
     C                   ENDIF                                                  SCPCM = *BLANKS
     C*
     C* IF PROC CODE METHOD =0 SECONDARY PROCEDURE CODE MUST BE BLANK
     C*
     C     SCPCM         IFEQ      '0'
     C     SCPRC2        ANDNE     *BLANKS
     C                   SETON                                        90
     C                   eval      indPRc2b = '1'
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
     C                   SETON                                        90
     C                   eval      indPRC4c = '1'
     C                   GOTO      CK2END
     C                   ENDIF                                                  SCPCM = *BLANKS
     C*
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
     C                   MOVEL(P)  SCPRC1        KKICD9
     C                   MOVE      SCLOS1        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9073
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
      /Free
       Endif;
      /End-Free
     C                   ENDIF                                                  SCPCM =9
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
     C                   MOVEL(P)  SCPRC2        KKICD9
     C                   MOVE      SCLOS2        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9074
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
      /Free
       Endif;
      /End-Free
     C                   ENDIF                                                  SCPRC2 NE *BLANKS
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
       Else;
      /End-Free
     C                   MOVEL(P)  SCPRC3        KKICD9
     C                   MOVE      SCLOS3        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9012
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
      /Free
       Endif;
      /End-Free
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
     C                   SETON                                        90
     C                   eval      indPRC4a = '1'
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
     C                   SETON                                        90
     C                   eval      indPRC4a = '1'
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
                indPRC4a = '1' ;
                leavesr;
             Endif;
       Else;
      /End-Free
     C                   MOVEL(P)  SCPRC4        KKICD9
     C                   MOVE      SCLOS4        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        90
     C                   eval      indPRC4a = '1'
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
      /Free
       Endif;
      /End-Free
     C                   ENDIF                                                  SCPRC4 NE *BLANKS
     C* VALIDATE THE OTHER DIAGNOSIS CODES
     C     SCDIAD        IFNE      *BLANKS
      /Free
       If xxicd10 = '1' and scICdInd ='Y';
          kkICd10 = scDIaD;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If not %found(fmicd10l1)
                or ICstat = 'I';
                *in90 = *on;
                indSCdAd = '1' ;
                leavesr;
             Endif;
       Else;
      /End-Free
     C                   MOVEL     SCDIAD        KKICD9
     C                   MOVE      'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        90
     C                   eval      indSCdAd = '1'
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
      /Free
       Endif;
      /End-Free
     C                   ENDIF                                                  SCDIA1 NE *BLANKS
     C*
     C* Validate diag. code 2 -
     C*
     C     SCDIA2        IFNE      *BLANKS
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
       Else;
      /End-Free
     C                   MOVEL     SCDIA2        KKICD9
     C                   MOVE      'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        9077
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
      /Free
       Endif;
      /End-Free
     C                   ENDIF                                                  SCDIA1 NE *BLANKS
     C*
     C     SCDIA3        IFNE      *BLANKS
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
                indDIA3a = '1';
                leavesr;
             Endif;
       Else;
      /End-Free
     C                   MOVEL     SCDIA3        KKICD9
     C                   MOVE      'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        9078
     C                   eval      indDIA3a = '1'
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
      /Free
       Endif;
      /End-Free
     C                   ENDIF                                                  SCDIA1 NE *BLANKS
     C*
     C     SCDIA4        IFNE      *BLANKS
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
                indDIA4a = '1' ;
                leavesr;
             Endif;
       Else;
      /End-Free
     C                   MOVEL     SCDIA4        KKICD9
     C                   MOVE      'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        90
     C                   eval      indDIA4a = '1'
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
      /Free
       Endif;
      /End-Free
     C                   ENDIF                                                  SCDIA1 NE *BLANKS
     C     @@MODE        IFNE      'D'
     C                   SETON                                        23
     C                   ENDIF
     C*
     C*** STATE IS REQUIRED
     C*
     C     *IN97         IFEQ      *ON
     C*    SCRST         ANDEQ     *BLANKS
     C                   SETON                                        9062
     C                   GOTO      CK2END
     C                   ENDIF                                                  SCRST IFEQ BLANKS
     C*
     C** VALIDATE STATE
     C*
      /Free
         Endif;
      /End-Free
     C***** Beg Add *** 05/18/15 *******************************************

     C** City is required
     C     *IN97         IFEQ      *ON
     C     SCRCTY        ANDEQ     *BLANKS
     C                   EVAL      erRcty = *on
     C                   EVAL      msRcty = 'This field cannot be blank'
     C                   SETON                                        90
     C                   GOTO      CK2END
     C                   ENDIF                                                  SCRCTY IFEQ BLANKS
     C*
     C** VALIDATE CITY
     C*
     C     SCRCTY        IFNE      *BLANKS
     C                   MOVEL(P)  SCRCTY        KKRCTY
     C     LO:ALPHA      XLATE     KKRCTY        KKRCTY
     C     KKRCTY        CHAIN     PSA01                              60
     C     *IN60         IFEQ      *ON
     C                   EVAL      erRcty = *on
     C                   EVAL      msRcty = 'Invalid value'
     C                   SETON                                        90
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
     C                   EVAL      erRst = *on
     C                   EVAL      msRst = 'This field cannot be blank'
     C                   SETON                                        90
     C                   GOTO      CK2END
     C                   ENDIF                                                  SCRST IFEQ BLANKS
     C*
     C** VALIDATE STATE
     C*
     C     SCRST         IFNE      *BLANKS
     C                   MOVEL(P)  SCRST         KKRST
     C     LO:ALPHA      XLATE     KKRST         KKRST
     C     KKRST         CHAIN     PSA02                              60
     C     *IN60         IFEQ      *ON
     C                   EVAL      erRst = *on
     C                   EVAL      msRst = 'Invalid value'
     C                   SETON                                        90
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
     C                   EVAL      erRcty = *on
     C                   EVAL      erRst = *on
     C                   EVAL      msRcty = 'Invalid combination'
     C                   EVAL      msRst = 'Invalid combination'
     C                   SETON                                        90
     C                   GOTO      CK2END
     C                   ENDIF
     C                   ENDIF
     C** Provider is required in some cases
      /Free
       monitor;
         xxDCDT = %dec(%char(%date(SCDCDT:*USA):*cymd0):7:0);
       on-error;
         xxDCDT = *zeros;
       endmon;
       If xxDCDT >= 1150401 and
          (xxMfs = 'A' or xxMFS = 'Y') and
          SCicn <> 'AO' and SCicn <> 'AS' and
          SCspcd <> 'CAH' and
          SCprov = *blanks;

         erProv = *on;
         indProvRqd = *on;
         msProv = 'This field cannot be blank';
         *in90 = *on;
         leavesr;
       Endif;
      /End-Free

     C** Transfer Status is required in some cases

     C     EODSTY        IFEQ      'DR'
     C     EODSTY        OREQ      'DS'
     C*** Drug# is required when STMFS = 'Y'.
     C     XXMFS         OREQ      'Y'
     C     XXMFS         OREQ      'A'

     C     SCICN         IFEQ      'IM'
     C     SCICN         OREQ      'IP'
     C     SCICN         OREQ      'IR'
     C     SCICN         OREQ      'IS'

     C     SCTRST        ifeq      *BLANKS

     C     XXBLTY        IFEQ      '21'
     C     XXMFS         ANDEQ     'Y'
     C     XXBLTY        OREQ      '18'
     C     XXMFS         ANDEQ     'Y'
     C     XXBLTY        OREQ      '21'
     C     XXMFS         ANDEQ     'A'
     C     XXBLTY        OREQ      '18'
     C     XXMFS         ANDEQ     'A'
     C** Don't validate
     C                   ELSE
     C*** Beg Add *** 07/21/15 ****************************************
     C     SCSPCD        IFNE      'CAH'
     C*** End Add *** 07/21/15 ****************************************
     C                   EVAL      erTrst = *on
     C                   EVAL      msTrst = 'This field cannot be blank'
     C                   SETON                                        90
     C                   GOTO      CK2END
     C*** Beg Add *** 07/21/15 ****************************************
     C                   ENDIF
     C*** End Add *** 07/21/15 ****************************************
     C                   ENDIF

     C                   Else
     C                   Eval      KKKEY = SCTRST
     C                   Eval      KKTBID = 'TRST'
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        90
     C                   Eval      erTrst = *on
     C                   Eval      msTrst = 'Invalid value'
     C                   GOTO      CK2END
     C                   ELSE
     C                   MOVEL     GTDESC        SCTRDS
     C                   ENDIF
     C                   Endif

     C                   ENDIF
     C                   ENDIF

     C** Implant Status is required in some cases

     C     XXBLTY        IFNE      '83'

     C*** Add edit for implant status if STMFS = 'Y'
     C     XXMFS         IFEQ      'Y'
     C     XXMFS         OREQ      'A'

     C     SCICN         IFEQ      'IM'
     C     SCICN         OREQ      'IP'
     C     SCICN         OREQ      'IR'
     C     SCICN         OREQ      'IS'

     C     SCIMST        Ifeq      *BLANKS

     C     XXBLTY        IFEQ      '21'
     C     XXMFS         ANDEQ     'Y'
     C     XXBLTY        OREQ      '18'
     C     XXMFS         ANDEQ     'Y'
     C     XXBLTY        OREQ      '21'
     C     XXMFS         ANDEQ     'A'
     C     XXBLTY        OREQ      '18'
     C     XXMFS         ANDEQ     'A'
     C** Don't validate
     C                   ELSE
     C*** Beg Add *** 07/21/15 ****************************************
     C     SCSPCD        IFNE      'CAH'
     C*** End Add *** 07/21/15 ****************************************
     C                   EVAL      erImst = *on
     C                   EVAL      msImst = 'This field cannot be blank'
     C                   SETON                                          90
     C                   GOTO      CK2END
     C*** Beg Add *** 07/21/15 ****************************************
     C                   ENDIF
     C*** End Add *** 07/21/15 ****************************************
     C                   ENDIF

     C                   Else
     C                   Eval      KKKEY = SCIMST
     C                   Eval      KKTBID = 'IMST'
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        90
     C                   Eval      erImst = *on
     C                   Eval      msImst = 'Invalid value'
     C                   GOTO      CK2END
     C                   ELSE
     C                   MOVEL     GTDESC        SCIMDS
     C                   ENDIF

     C                   EndIf

     C                   ENDIF
     C                   ENDIF
     C                   ENDIF

     C***** End Add *** 05/18/15 *******************************************
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
     C                   SETOFF                                       441513
     C                   SETOFF                                       55
     C                   SETOFF                                       2221
     C                   SETOFF                                       27
     C                   SETOFF                                       2061
     C                   eval      indPRC4b = '0'
     C***** Beg Add *** 05/18/15 *******************************************
     C                   eval      indRctyFoc = *off
     C                   Eval      indRstFoc = *off
     C                   eval      indProvFoc = *off
     C                   eval      indTrstFoc = *off
     C                   eval      indImstFoc = *off
     C***** End Add *** 05/18/15 *******************************************
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
     C********           MOVE(P)   SCDIA1        HLDIA             5
     C                   MOVEL(P)  SCDIA1        HLDIA             7
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
       //*** Beg Add *** 09/17/15 ****************************************
             kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   SCD1DS = ICdsc1;
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
     C********           MOVE(P)   HLDIA         SCDIA1
     C                   MOVEL(P)  HLDIA         SCDIA1
     C                   SETON                                        9034      pos cursor
     C                   ELSE
     C********           MOVE(P)   ##ICD9        SCDIA1
     C                   MOVEL(P)  ##ICD9        SCDIA1
     C                   SETON                                        9031      pos cursor
     C                   ENDIF
     C*
     C     SCDIA1        IFNE      *BLANKS
     C********           MOVE(P)   SCDIA1        KKICD9
     C                   MOVEL(P)  SCDIA1        KKICD9
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCD1DS
     C                   ENDIF
     C*
      /Free
       Endif;
      /End-Free
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
     C* TYPE OF ADMISSION
     C*
     C     HFLD          WHENEQ    'SCADTY'
     C                   MOVEL(P)  'NCTA'        ##TBID
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
     C                   SETON                                        9063
     C*
     C                   ELSE
     C                   SETON                                        9039
     C                   Eval      indADTY = '1'
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C                   SETON                                        9039
     C*
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
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
     C                   SETON                                        90
     C                   eval      INDPRC1 = '1'
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
     C                   SETON                                        90
     C                   eval      indPCM1b = '1'
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
             indPCM1b = '1' ;
          Else;
             scPRc1 = outICD10;
             *in90 = *on;
             *in63 = *on;
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
     C                   SETON                                        90
     C                   eval      indPCM1b = '1'
     C                   ELSE
     C                   MOVEL(P)  ##ICD9        SCPRC1
     C                   MOVE      ##LOST        SCLOS1
     C                   MOVEL(P)  SCPRC1        KKICD9
     C                   MOVE      SCLOS1        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP1DS
     C                   ENDIF                                                  *INPP = *OFF
     C                   SETON                                        9063
     C                   ENDIF                                                  ##ICD9 = *BLANKS
      /Free
       Endif;
      /End-Free
     C                   ENDIF                                                  SCPCM = '9'
     C*
     C     HFLD          WHENEQ    'SCPRC2'
     C     HFLD          OREQ      'SCLOS2'
     C*
     C* PROCEDURE CODE MUST BE ENTERED TO DO THIS SEARCH
     C*
     C     SCPCM         IFEQ      *BLANKS
     C                   SETON                                        90
     C                   eval      indPRC2a = '1'
     C                   ENDIF
     C*
     C     SCPCM         IFEQ      '0'
     C                   SETON                                        90
     C                   eval      indPRC2c = '1'
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
      /Free
       Endif;
      /End-Free
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
      /Free
       Endif;
      /End-Free
     C                   ENDIF                                                  SCPCM = '9'
     C*
     C     HFLD          WHENEQ    'SCPRC4'
     C     HFLD          OREQ      'SCLOS4'
     C*
     C* PROCEDURE CODE MUST BE ENTERED TO DO THIS SEARCH
     C*
     C     SCPCM         IFEQ      *BLANKS
     C                   SETON                                        90
     C                   eval      indPRC4b = '1'
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
      /Free
       Endif;
      /End-Free
     C                   ENDIF                                                  SCPCM = '9'
     C*
     C* OTHER DIAGNOSIS CODES SEARCHES
     C*
     C     HFLD          WHENEQ    'SCDIAD'
     C                   MOVEL(P)  SCDIAD        HLDIA             7
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
             scDIaD = HLDIA;
             *in90 = *on;
          //   *in43 = *on;
          Else;
             scDIaD = outICD10;
             *in90 = *on;
          //   *in44 = *on;
          Endif;

          If scDiaD <> *blanks;
             kkICd10 = scDIaD;
       //*** Beg Add *** 09/17/15 ****************************************
             kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   scDAds = ICdsc1;
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
     C                   MOVEL(P)  HLDIA         SCDIAD
     C*                  SETON                                        9043      pos cursor
     C                   SETON                                        90        pos cursor
     C                   ELSE
     C                   MOVEL(P)  ##ICD9        SCDIAD
     C*                  SETON                                        9044      pos cursor
     C                   SETON                                        90        pos cursor
     C                   ENDIF
     C*
     C     SCDIAD        IFNE      *BLANKS
     C                   MOVEL(P)  SCDIAD        KKICD9
     C                   MOVE      ##LOST        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCDADS
     C                   ENDIF
      /Free
       Endif;
      /End-Free
     C*
     C* Diag. code 2nd search -
     C*
     C     HFLD          WHENEQ    'SCDIA2'
     C********           MOVE(P)   SCDIA2        HLDIA             5
     C                   MOVEL(P)  SCDIA2        HLDIA             7
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
     C********           MOVE(P)   HLDIA         SCDIA2
     C                   MOVEL(P)  HLDIA         SCDIA2
     C                   SETON                                        9043      pos cursor
     C                   ELSE
     C********           MOVE(P)   ##ICD9        SCDIA2
     C                   MOVEL(P)  ##ICD9        SCDIA2
     C                   SETON                                        9044      pos cursor
     C                   ENDIF
     C*
     C     SCDIA2        IFNE      *BLANKS
     C********           MOVE(P)   SCDIA2        KKICD9
     C                   MOVEL(P)  SCDIA2        KKICD9
     C                   MOVE      ##LOST        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCD2DS
     C                   ENDIF
      /Free
       Endif;
      /End-Free
     C*
     C     HFLD          WHENEQ    'SCDIA3'
     C********           MOVE(P)   SCDIA3        HLDIA             5
     C                   MOVEL(P)  SCDIA3        HLDIA             7
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
     C********           MOVE(P)   HLDIA         SCDIA3
     C                   MOVEL(P)  HLDIA         SCDIA3
     C                   SETON                                        9044      pos cursor
     C                   ELSE
     C********           MOVE(P)   ##ICD9        SCDIA3
     C                   MOVEL(P)  ##ICD9        SCDIA3
     C                   SETON                                        9045      pos cursor
     C                   ENDIF
     C*
     C     SCDIA3        IFNE      *BLANKS
     C********           MOVE(P)   SCDIA3        KKICD9
     C                   MOVEL(P)  SCDIA3        KKICD9
     C                   MOVE      ##LOST        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCD3DS
     C                   ENDIF
      /Free
       Endif;
      /End-Free
     C*
     C     HFLD          WHENEQ    'SCDIA4'
     C********           MOVE(P)   SCDIA4        HLDIA             5
     C                   MOVEL(P)  SCDIA4        HLDIA             7
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
     C********           MOVE(P)   HLDIA         SCDIA4
     C                   MOVEL(P)  HLDIA         SCDIA4
     C                   SETON                                        9045      pos cursor
     C                   ELSE
     C********           MOVE(P)   ##ICD9        SCDIA4
     C                   MOVEL(P)  ##ICD9        SCDIA4
     C                   SETON                                        9045      pos cursor
     C                   ENDIF
     C*
     C     SCDIA4        IFNE      *BLANKS
     C********           MOVE(P)   SCDIA4        KKICD9
     C                   MOVEL(P)  SCDIA4        KKICD9
     C                   MOVE      ##LOST        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCD4DS
     C                   ENDIF
      /Free
       Endif;
      /End-Free
     C*
     C***** Beg Add *** 05/18/15 *******************************************
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
     C                   SETON                                        90
     C                   Eval      indRctyFoc = *on
     C                   ELSE
     C     ##RRN         CHAIN     PSA03                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  SACTY         SCRCTY
     C                   MOVEL(P)  SAST          SCRST
     C                   SETON                                        90
     C                   Eval      indRctyFoc = *off
     C                   Eval      indRstFoc = *off
     C                   Eval      indProvFoc = *on
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
     C                   SETON                                        90
     C                   Eval      indRstFoc = *on
     C                   ELSE
     C     ##RRN         CHAIN     PSA03                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  SAST          SCRST
     C                   MOVEL(P)  SACTY         SCRCTY
     C                   SETON                                        90
     C                   Eval      indRctyFoc = *off
     C                   Eval      indRstFoc = *off
     C                   Eval      indProvFoc = *on
     C                   ENDIF
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCPROV'
     C*
     C     SCICN         IFEQ      'IM'
     C     SCICN         OREQ      'IR'
     C     SCICN         OREQ      'IP'
     C     SCICN         OREQ      'IS'
     C                   eval      KKPROV = SCPROV
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
     C                   SETON                                        90
     C                   Eval      indProvFoc = *off
     C                   Eval      indTrstFoc = *on
     C                   MOVE      *ZEROS        ##RRN
     C                   ENDIF
     C                   ELSE
     C                   Eval      indProvFoc = *on
     C                   SETON                                        90
     C*
     C                   ENDIF                                                  ##RRN <> 0
     C                   ENDIF                                                  SCICN = 'IM'
     C*
     C     SCICN         IFEQ      'HO'
     C     SCICN         OREQ      'HS'
     C                   eval      KKPROV = SCPROV
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
     C                   Eval      indProvFoc = *off
     C                   Eval      indTrstFoc = *on
     C                   SETON                                        90
     C                   MOVE      *ZEROS        ##RRN
     C                   ENDIF
     C                   ELSE
     C                   Eval      indProvFoc = *on
     C                   SETON                                        90
     C*
     C                   ENDIF                                                  ##RRN <> 0
     C                   ENDIF                                                  SCICN = 'IM'
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
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        90
     C                   Eval      indTrstFoc = *on
     C                   ENDIF
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        90
     C                   MOVEL     GTKEY         SCTRST
     C                   MOVEL     GTDESC        SCTRDS
     C                   Eval      indTrstFoc = *off
     C                   Eval      indImstFoc = *on
     C                   ELSE
     C                   Eval      indTrstFoc = *on
     C                   SETON                                        90
     C                   ENDIF
     C*
     C                   ENDIF                                                  ##RRN <> 0
     C*
     C* IMPLANT STATUS SEARCH
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
     C                   SETON                                        90
     C                   Eval      indImstFoc = *on
     C                   ENDIF
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCIMST
     C                   MOVEL     GTDESC        SCIMDS
     C                   Eval      indImstFoc = *off
     C                   SETON                                        90
     C                   ELSE
     C                   Eval      indImstFoc = *on
     C                   SETON                                        90
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C***** End Add *** 05/18/15 *******************************************
     C*** Beg Add *** 07/18/25 ***************************************
     C     HFLD          WHENEQ    'SCNPI1'
     C                   EVAL      MSSNPI = 'MCR0676'
     C                   EVAL      ERSNPI = *ON
     C                   EVAL      *IN90  = *ON
     C*** End Add *** 07/18/25 ***************************************
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
     C                   SETOFF                                       17
     C                   CLEAR                   SCDIA1
     C                   CLEAR                   SCD1DS
     C                   CLEAR                   SCCHRG
     C                   CLEAR                   SCMRA
     C                   CLEAR                   SCNTWK
     C                   CLEAR                   SCOTHR
     C                   CLEAR                   SCOTH2
     C                   CLEAR                   SCPAID
     C*** Beg Add *** 07/18/25 ***************************************
     C                   MOVE      *BLANKS       SCNPI1
     C*** End Add *** 07/18/25 ***************************************
     C*                  MOVE      *BLANKS       SCANPI
     C*                  MOVE      *BLANKS       SCONPI
     C*                  MOVE      *BLANKS       SCNPI1
     C*                  MOVE      *BLANKS       SCNPI2
     C*                  MOVE      *BLANKS       SCRCTY
     C*                  MOVE      *BLANKS       SCRST
     C                   CLEAR                   SCRFEI
     C                   CLEAR                   SCRLSN
     C                   CLEAR                   SCRFSN
     C*                  CLEAR                   SCRAD1
     C*                  CLEAR                   SCRAD2
     C*                  CLEAR                   SCRNCT
     C*                  CLEAR                   SCRNST
     C*                  CLEAR                   SCRZIP
     C*                  CLEAR                   SCPATS
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
     C********           MOVE(P)   EUDIA1        SCDIA1
     C********           MOVE(P)   SCDIA1        KKICD9
     C                   MOVEL(P)  EUDIA1        SCDIA1
     C                   MOVEL(P)  SCDIA1        KKICD9
      /Free
       If xxICD10 = '1' and EUicdind ='Y';
          KKICD10 = EUicd1001;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
               scD1ds = ICdsc1;
             Endif;
       Else;
      /End-Free
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDSC1        SCD1DS
     C*
      /Free
       Endif;
      /End-Free
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
     C                   Z-ADD     EUPRD1        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPRD1
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
     C*                  MOVE      EUPATS        SCPATS
     C                   Z-ADD     EOCHRG        SCCHRG
     C                   Z-ADD     EOMRA         SCMRA
     C                   Z-ADD     EONDIS        SCNTWK
     C                   Z-ADD     EOOTHR        SCOTHR
     C                   Z-ADD     EOOTH2        SCOTH2
     C                   Z-ADD     EOPAID        SCPAID
     C                   MOVEL(P)  EOFLIC        SCFZIP
     C*** Beg Add *** 07/18/25 ***************************************
     C                   MOVE      EOFLIC        SCZIP4
     C                   MOVE      EUNPI1        SCNPI1
     C*** End Add *** 07/18/25 ***************************************
     C*
     C                   MOVEL     EUDRG#        SCDRG
     C                   MOVEL     EUDGDS        KKDESC
     C                   MOVE      EUADTY        SCADTY
     C                   ENDIF                                                  @@MODE NE 'A'
     C***** Beg Add *** 05/18/15 *******************************************
     C                   Eval      SCPROV = EUMCAR
     C                   Eval      SCIMST = EUIMST
     C                   Eval      SCTRST = EUTRST
     C                   Eval      SCRCTY = EURCTY
     C                   Eval      SCRST  = EURST
     C***** End Add *** 05/18/15 *******************************************
     C*****CLTID         IFEQ      '146'
     C*****CLTID         OREQ      '114'
     C                   SETON                                        17
     C                   MOVE      EURFEI        SCRFEI
     C                   MOVE      EURLSN        SCRLSN
     C                   MOVE      EURFSN        SCRFSN
     C*****              ENDIF
     C*
     C*****CLTID         IFEQ      '114'
     C*
     C     FNKY          CHAIN     PFN01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C     FNNWCG        ANDEQ     'Y'
     C*
     C*                  MOVE      '99'          SCTRST
     C*                  MOVE      'N '          SCIMST
     C*
     C                   ENDIF
     C*
     C******             ENDIF
     C*
     C* Procedure codes:
     C*
     C                   MOVEL     EUPCM         SCPCM
     C                   MOVE      'ZZPM'        KKTBID
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
     C*****SCPCM         IFEQ      '9'
     C*****SCPRC1        ANDNE     *BLANKS
      /Free
         If SCpcm = '9' and (Scprc1 <> ' ' or EUPRC1001 <> ' ' );
      /End-Free
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
     C                   MOVEL(P)  SCPRC1        KKICD9
     C                   MOVE      SCLOS1        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP1DS
     C                   ENDIF                                                  *IN99 = *OFF
      /Free
       Endif;
      /End-Free
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
     C*****SCPCM         IFEQ      '9'
     C*****SCPRC2        ANDNE     *BLANKS
      /Free
         If SCpcm = '9' and (Scprc2 <> ' ' or EUPRC1002 <> ' ' );
      /End-Free
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
     C                   MOVEL(P)  SCPRC2        KKICD9
     C                   MOVE      SCLOS2        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP2DS
     C                   ENDIF                                                  *IN99 = *OFF
      /Free
       Endif;
      /End-Free
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
     C*****SCPCM         IFEQ      '9'
     C*****SCPRC3        ANDNE     *BLANKS
      /Free
         If SCpcm = '9' and (Scprc3 <> ' ' or EUPRC1003 <> ' ' );
      /End-Free
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
     C                   MOVEL(P)  SCPRC3        KKICD9
     C                   MOVE      SCLOS3        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP3DS
     C                   ENDIF                                                  *IN99 = *OFF
      /Free
       Endif;
      /End-Free
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
     C*****SCPCM         IFEQ      '9'
     C*****SCPRC4        ANDNE     *BLANKS
      /Free
         If SCpcm = '9' and (Scprc4 <> ' ' or EUPRC1004 <> ' ' );
      /End-Free
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
     C                   MOVEL(P)  SCPRC4        KKICD9
     C                   MOVE      SCLOS4        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP4DS
     C                   ENDIF                                                  *IN99 = *OFF
      /Free
       Endif;
      /End-Free
     C                   ENDIF                                                  SCPCM = '9'
     C* Diagnosis code:
     C                   MOVEL     EUADDI        SCDIAD
      /Free
         If SCDIAD <> ' ' or EUADIA1001 <> ' ' ;
      /End-Free
      /Free
       If xxICD10 = '1' and EUicdInD = 'Y';
          KKICD10 = EUADIA1001 ;
       //*** Beg Add *** 09/17/15 ****************************************
          kkLost = 'D';
       //*** End Add *** 09/17/15 ****************************************
          Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
               scDads = ICdsc1;
             Endif;
       Else;
      /End-Free
     C                   MOVEL(P)  SCDIAD        KKICD9
     C                   MOVE      'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCDADS
     C                   ENDIF                                                  *IN99 = *OFF
      /Free
       Endif;
      /End-Free
     C                   ENDIF                                                  SCDIA2 = *BLANKS
     C*
     C*
     C*
     C                   MOVEL     EUDIA2        SCDIA2
     C*****SCDIA2        ANDNE     *BLANKS
      /Free
         If SCDIA2 <> ' ' or EUICD1002 <> ' ' ;
      /End-Free
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
     C                   MOVEL(P)  SCDIA2        KKICD9
     C                   MOVE      'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCD2DS
     C                   ENDIF                                                  *IN99 = *OFF
      /Free
       Endif;
      /End-Free
     C                   ENDIF                                                  SCDIA2 = *BLANKS
     C*
     C                   MOVEL     EUDIA3        SCDIA3
     C*****SCDIA3        ANDNE     *BLANKS
      /Free
         If SCDIA3 <> ' ' or EUICD1003 <> ' ' ;
      /End-Free
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
     C                   MOVEL(P)  SCDIA3        KKICD9
     C                   MOVE      'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCD3DS
     C                   ENDIF                                                  *IN99 = *OFF
      /Free
       Endif;
      /End-Free
     C                   ENDIF                                                  SCDIA3 = *BLANKS
     C*
     C                   MOVEL     EUDIA4        SCDIA4
      /Free
         If SCDIA4 <> ' ' or EUICD1004 <> ' ' ;
      /End-Free
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
     C                   MOVEL(P)  SCDIA4        KKICD9
     C                   MOVE      'D'           KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCD4DS
     C                   ENDIF                                                  *IN99 = *OFF
      /Free
       Endif;
      /End-Free
     C                   ENDIF                                                  SCDIA4 = *BLANKS
     C*
      /Free
          If xxicd10 = *on and EUicdind = 'Y';
             scDIa1  = EUICD1001;
             scDIaD  = EUADIA1001 ;
             scDIa2  = EUICD1002;
             scDIa3  = EUICD1003;
             scDIa4  = EUICD1004;
             scICdind= EUICDind ;
             If EUPCM = '9';
                scPRc1  = EUPRC1001;
                scPRc2  = EUPRC1002;
                scPRc3  = EUPRC1003;
                scPRc4  = EUPRC1004;
             Else;
                scPRc1  = EUPRC1;
                scPRc2  = EUPRC2;
                scPRc3  = EUPRC3;
                scPRc4  = EUPRC4;
             EndIf;
          Elseif EUICDind ='N';
             scICdind = EUICDind;
          Endif;
      /End-Free
     C*** Beg Add *** 09/30/15 ****************************************
     C                   Eval      SCRAD1 = EUrAd1
     C                   Eval      SCRAD2 = EUrAd2
     C                   Eval      SCRNCT = EUrnCt
     C                   Eval      SCRNST = EUrnSt
     C                   Eval      SCRZIP = EUrZip
     C*** End Add *** 09/30/15 ****************************************
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
     C********           MOVE(P)   SCDIA1        EUDIA1
     C                   MOVEL(P)  SCDIA1        EUDIA1
     C                   MOVE      SCEDI         EUEDI
     C*                  MOVEL(P)  SCBLID        EOBLID
     C*
     C***** Beg Add *** 05/18/15 *******************************************
     C                   Eval      EUMCAR = SCPROV
     C                   Eval      EUIMST = SCIMST
     C                   Eval      EUTRST = SCTRST
     C                   Eval      EURCTY = SCRCTY
     C                   Eval      EURST  = SCRST
     C***** End Add *** 05/18/15 *******************************************
     C***** Beg Add *** 09/30/15 *******************************************
     C                   Eval      EUrAd1 = scrAd1
     C                   Eval      EUrAd2 = scrAd2
     C                   Eval      EUrnCt = scrnCt
     C                   Eval      EUrnSt = scrnSt
     C                   Eval      EUrZip = scrZip
     C***** End Add *** 09/30/15 *******************************************
     C*                  MOVEL     SCPROV        EUMCAR
     C                   MOVEL     SCDRG         EUDRG#
     C                   MOVEL     KKDESC        EUDGDS
     C*                  MOVEL     SCTRST        EUTRST
     C*                  MOVEL     SCIMST        EUIMST
     C                   MOVEL(P)  SCBLTY        EUBLTY
     C*                  MOVEL(P)  SCRCTY        EURCTY
     C*                  MOVEL(P)  SCRST         EURST
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
     C                   Z-ADD     SCPRD1        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EUPRD1
     C*
     C                   Z-ADD     SCSBDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EUSBDT
     C*
     C                   MOVE      SCADHR        EUADHR
     C                   Z-ADD     SCDCHR        EUDCHR
     C                   MOVE      SCADTY        EUADTY
     C                   MOVEL     SCICN         EUICN
     C                   MOVEL     SCDCN         EUDCN
     C                   MOVE      SCSPCD        EOSPCD
     C                   MOVEL(P)  SCFZIP        EOFLIC
     C*** Beg Add *** 07/18/25 ***************************************
     C                   MOVE      SCZIP4        EOFLIC
     C                   MOVE      SCNPI1        EUNPI1
     C*** End Add *** 07/18/25 ***************************************
     C*                  MOVEL     SCPATS        EUPATS
     C*                  MOVE      SCANPI        EUANPI
     C*                  MOVE      SCONPI        EUONPI
     C*                  MOVE      SCNPI1        EUNPI1
     C*                  MOVE      SCNPI2        EUNPI2
     C                   MOVEL     SCPCM         EUPCM
     C     SCPCM         IFNE      '9'
     C                   MOVE      *BLANKS       SCLOS1
     C                   MOVE      *BLANKS       SCLOS2
     C                   ENDIF
     C                   MOVEL(P)  SCPRC1        EUPRC1
     C                   MOVE      SCLOS1        EUPRC1
     C                   MOVEL(P)  SCPRC2        EUPRC2
     C                   MOVE      SCLOS2        EUPRC2
     C                   MOVEL(P)  SCPRC3        EUPRC3
     C                   MOVE      SCLOS3        EUPRC3
     C                   MOVEL(P)  SCPRC4        EUPRC4
     C                   MOVE      SCLOS4        EUPRC4
     C                   MOVEL(P)  SCDIAD        EUADDI
     C                   MOVEL(P)  SCDIA2        EUDIA2
     C                   MOVEL(P)  SCDIA3        EUDIA3
     C                   MOVEL(P)  SCDIA4        EUDIA4
     C*****CLTID         IFEQ      '146'
     C*****CLTID         OREQ      '114'
     C                   MOVE      SCRFEI        EURFEI
     C                   MOVE      SCRLSN        EURLSN
     C                   MOVE      SCRFSN        EURFSN
     C*                  MOVE      SCRAD1        EURAD1
     C*                  MOVE      SCRAD2        EURAD2
     C*                  MOVE      SCRNCT        EURNCT
     C*                  MOVE      SCRNST        EURNST
     C*                  MOVE      SCRZIP        EURZIP
     C*****              ENDIF
      /Free
          EUICDind  = scICdind;
          If xxicd10 = *on and scICdInd ='Y';
             EUICD1001 = scDIa1;
             EUADIA1001 = scDIaD;
             EUICD1002 = scDIa2;
             EUICD1003 = scDIa3;
             EUICD1004 = scDIa4;
                If SCPCM = '9';
                  EUPRC1001 = scPRc1;
                  EUPRC1002 = scPRc2;
                  EUPRC1003 = scPRc3;
                  EUPRC1004 = scPRc4;
                Else;
                  EUPRC1001 = *Blanks;
                  EUPRC1002 = *Blanks;
                  EUPRC1003 = *Blanks;
                  EUPRC1004 = *Blanks;
                  EUPRC1 = scPRc1;
                  EUPRC2 = scPRc2;
                  EUPRC3 = scPRc3;
                  EUPRC4 = scPRc4;
                EndIf;
             // If ICD10 is Active then no need of ICD9 fields
             EUDIa1 = *Blanks;
             EUDIa2 = *Blanks;
             EUDIa3 = *Blanks;
             EUDIa4 = *Blanks;
          Else;
             EUDIA1 = scDIa1;
             EUADDI = scDIaD;
             EUDIA2 = scDIa2;
             EUDIA3 = scDIa3;
             EUDIA4 = scDIa4;
             // If ICD09 is Active then no need of ICD10 fields
             EUicd1001 = *Blanks;
             EUICD1002 = *Blanks;
             EUICD1003 = *Blanks;
             EUICD1004 = *Blanks;
             EUPRC1001 = *Blanks;
             EUPRC1002 = *Blanks;
             EUPRC1003 = *Blanks;
             EUPRC1004 = *Blanks;
          Endif;
      /End-Free
     C*
     C*
     C                   ENDSR                                                  For MVDSC
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
       ersnpi = *off;
       mssnpi = *blanks;
       //*** End add ***** 07/18/25 ************************************
     C     *INKJ         CASEQ     *ON           KJDO                           F10=search
     C******INKK         CASEQ     *ON           KKDO                           F11=EOB/Notes
     C                   ENDCS
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
     C                   SETOFF                                       66
     C*    EDITST        IFEQ      'NC'
     C*    XXMFS         OREQ      'Y'
     C*    EODSTY        OREQ      'DR'
     C*    EODSTY        OREQ      'DS'
     C*                  SETON                                        6566
     C*                  ELSE
     C*                  SETOFF                                       6566
     C*                  ENDIF
     C*    EDITST        IFEQ      'NC'
     C*    XXMFS         OREQ      'Y'
     C*    EODSTY        OREQ      'DR'
     C*    EODSTY        OREQ      'DS'
     C*                  SETON                                        27
     C*                  ELSE
     C*                  SETOFF                                       27
     C*                  ENDIF
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
     C*****CLTID         IFEQ      '114'
     C*****              SETON                                        16
     C*****              ELSE
     C*****              SETOFF                                       16
     C*****              ENDIF
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
         If @@mode = 'A' ;
            scICdInd ='Y';
         else ;
            scICdind = euICDind ;
         endif ;
         Endsl;

         If @@MODE = 'D';
            DspicdInd = '1';
         Endif;

      /End-Free
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
     C*                  MOVEL(P)  SCBLID        EOBLID
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
