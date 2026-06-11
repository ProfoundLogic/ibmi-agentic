     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: F0086R                                            *
     H*    DESCRIPTION: REQUEST EOMB PRINT/WC PAYMENT                 *
     H*                 TAPE SUBMISSION VERSION                       *
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  10/21/99 C:53  Project: WCMNT
     H*                 REMOVE DELETED FORMATS FROM KIGNORE
     H*
     H*  10/07/99 C:56  Project: MC212
     H*                 Remove F15=T/M entry per Lorri Dapper
     H*
     H*  09/21/99 C:31  Project: MC212
     H*                 Rollup of MC PGMR_DEV
     H*                 1. Correct ##NTWK flag test
     H*
     H*  04/08/99 C:56  Support: MC095 #17568
     H*              1. Add CO#/SUB#/FND as entry parms
     H*
     H*              2. Use entry parm CO#/SUB#/FND instead of UPCO#
     H*                 UPSUB#/UPFND
     H*
     H*  02/12/99 C:31  Project: 81MOZ
     H*                 Pass Dt Rcvd to Chk Rqst
     H*                 1. Add Date Received parm (passed from F0086R)
     H*
     H*  08/06/98 C:94
     H*                 MADE F15=T/M ENTRY CONDITIONAL ON SFBPF/FBAL01
     H*                 POS.9
     H*
     H*  08/02/98 C:53  Project: MC145
     H*                 CORRECT TAPE FLAG
     H*
     H*  07/27/98 C:31  Support: 12860
     H*                 FL Tape Submission - Payment plan issue
     H*                 1. Add prompt for Florida payment plan
     H*
     H*  06/05/98 C:53  Project: MC145 SI# 11891
     H*                 NETWORK FLAG USAGE
     H*
     H*  05/26/98 C:53  Project: MC058
     H*                 ADD EOMB CHECK MISMATCH TRACKINGS
     H*
     H*  05/22/98 C:53  Project: MC051 # 11677
     H*                 REMOVE LIC# EDIT ON FL-HOSP BILLS
     H*
     H*  03/27/98 C:31  Project: MC145
     H*                 Yr 2000, RISC Developemnt
     H*                 1. Replace W2143R call with W2142R
     H*
     H*  02/13/98 C:53  Project: MC159
     H*              1. REPLACE NMHSP, NMPHP WITH FMS4L2, FMS4L3 FOR
     H*                 PROVIDER NETWORK RE-WRITE.
     H*
     H* 01/29/98 C: 53  PROJ: MC145
     H*                 REMOVE RECON REFUND FLAGS
     H*
     H* 12/15/97 C:56 Project: FS139
     H*            1. Add update of MGEOB# in FDMGP when EOB# is
     H*               assinged.
     H*
     H*            2. Add call to F0074R to delete bill entry
     H*               messages
     H*
     H*
     H* 11/07/97 C: 53  PROJ: MC145                                   *
     H*                 1. CHG PARMS FOR V3 WCMS
     H*                 2. RMV BWC INFO/EDITS
     H*                 3. QUALIFY F0070R CALL ON EDI
     H*                                                               *
     H* 07/03/95 C: 35  PROJ: NW008                                   *
     H*                 1. ADDED AN ADDITIONAL PARM TO THE INTERACTIVE*
     H*                    BATCH PGM (F6000CI) TO CONDITION IF EOMB'S *
     H*                    WILL BE PRINTED.  THIS PGM WILL ALWAYS     *
     H*                    PASS A 'N'.                                *
     H*                 2. ADDED PARM PASSED TO F600CI TO INDICATE IF *
     H*                    T/M ENTRY SHOULD BE CALLED.                *
     H*                 3. ADDED PARM PASSED TO F6000CI TO INDICATE   *
     H*                    IF WCMS CHECK REQUEST SHOULD BE PROCESSED. *
     H*                 4. ADDED NEW PARM TO CALL TO F6000CI FOR      *
     H*                    SYSTEM TYPE.  THIS WILL BE LOADED FROM     *
     H*                    QQSYS.                                     *
     H*                                                               *
     H* 07/21/95 C: 02  PROJ: 25MAL, F006, TX002,ETC..                *
     H*                 1. TEMP COMMENT/WAIT FOR WC BASW SEE '*** #'  *
     H*                                                               *
     H* 07/27/95 C: 20  REQ/PROJ: 25MAE                               *
     H*                 1. ADDED PARM PASSED FROM BILL ENTRY QQAUDF   *
     H*                    THIS PARM WILL INDICATE THE AUDIT NUMBER   *
     H*                    THAT THIS IS FOR THIS BILL                 *
     H*                    THIS VALUE WILL BE WRITTEN TO FAE1P E120A1 *
     H*                    POS. 9-11                                  *
     H*                 2. IF AUDIT OR RECON CALL CREDIT MEMO PGM     *
     H*                                                               *
     H*  10/01/95 C:20 PROJECT: MCMNT                                 *
     H*                1. THE PROGRAM WAS LOADING THE PROC CODEING    *
     H*                   METHOD FROM DC10A1 POS.10.  THIS FIELD NEEDS*
     H*                   TO BE LOADED FROM DC10A2 POS. 10.           *
     H*                                                               *
     H* 02/21/96 C:41  REQ/PROJ: FSMNT                                *
     H*                1. IF XXAST = 'FL'                             *
     H*                   AND QQSYS = 'P'                             *
     H*                   THAN CARRIER RECEIVE DATE REQUIRED.         *
     H*
     H* 12/02/96 C:59 Req/Proj: 203/FS134                             *
     H*               Call to program CHKSTR to verify: if            *
     H*               State is installed.  Returns '1' If found       *
     H*               and '0' if not found.                           *
     H*                                                               *
     H* 12/03/96 C:32  REQ/PROJ: 60MAX - (REAPPLY 60MAE, 60MAL CHGS)  *
     H*   60MAE        1. CHG 'DATE 1ST CONSULTED' TO 'EMPLOYER       *
     H*                   RECEIVED DATE' (SCREEN ONLY CHG)            *
     H*   60MAL        2. ADDED FLAGS TO SCREEN FOR RECONSIDERATIONS  *
     H*                   AND REFUND.  BOTH FLAGS WILL DEFAULT TO 'N'.*
     H*                   REFUND IS ONLY ALLOWED IF RECON.  IF RECON  *
     H*                   IS A 'Y' WILL CALL F0087F/R FOR RECON       *
     H*                   BREAKDOWN.  RECON EOMB WILL BE FORCED THRU  *
     H*                   IMMEDIATE PROCESSING.                       *
     H*
     H*  02/25/97 C:43  REQ/PROJ: 87MAW (PHASE 1)
     H*                1. DO NOT INVOKE W0080R WHEN FUND TYPE=STATE
     H*
     H*
     H*  03/11/97 C:43  REQ/PROJ: 87MAW (PHASE 2 FOR 3/17 INSTALL)
     H*                1. NEW FADCP
     H*                2. NEW FAE1P
     H*                3. REMOVED INJURY DUE TO EMPLOYMENT
     H*                   REMOVED DATE FIRST CONSULTED
     H*                   REMOVED PATIENT ACCT#
     H*                   REMOVED DATE BILL WENT TO CARRIER
     H*                   REMOVED DATE BILL RECEIVED BY CARRIER
     H*                   REMOVED LICENSE
     H*                4. ADDED DISPLAY OF NON-BWC
     H*                5. ADDED REQUIRE SCEOB1
     H*
     H* 1.  SETON *IN12 AT TOP OF PGM SO THAT PROVIDER ADDRESS IS     *
     H*     PROTECTED                                                 *
     H* 2.  CHECK PROVIDER AUXILIARY FOR ADJUDICATION STATE:          *
     H*        CROSS REFERENCE STATE TO FS CONTROL FILE, IF STATE     *
     H*        IF VALID AND TAPE SUBMISSION IS REQUIRED, THEN         *
     H*        FLIP SCTAPE FIELD TO 'Y' AND REQUIRE LICENSE #.        *
     H* 3.  IF SYSTEM NOT LINKED WITH WCMS, THEN UPDATE CMRT01 W/ 'ME'*
     H*     AND CMAR01 WITH EOMB PAYMENT AMOUNT IN CLAIM MASTER FILE  *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS:                                                  *
     H*  PRIOR TO SCREEN DISPLAY, EDIT ROUTINE: (RETURN TO F0070R)    *
     H*   20        INVALID CASE                                      *
     H*   22        INVALID PROVIDER (EITHER NO HIT OR INACTIVE)      *
     H*   28        NO CLAIMANT DEMOGRAPHICS                          *
     H*  SCR01:                                                       *
     H*   09        INQUIRY MODE                                      *
     H*   12        PROTECT ALL PROVIDER FIELDS
     H*  N12        COLUMN SEPARATORS ON ALL PROVIDER FIELDS          *
     H*   20        TOTAL CHARGES FIELD CANNOT BE LE ZERO             *
     H*   21        AMOUNT EXCEEDS RESERVE AMOUNT (NOT USED NOW)      *
     H*   22        EMPLOYMENT FLAG MUST BE Y OR N                    *
     H*   23        INVALID DATE FIRST CONSULTED                      *
     H*   24        INVALID EOB CODE 1                                *
     H*   25        INVALID EOB CODE 2                                *
     H*   26        INVALID EOB CODE 3                                *
     H*   27        TAPE FLAG MUST BE Y OR N                          *
     H*   28        PROVIDER ADDRESS CANNOT BE BLANK                  *
     H*   29        PROVIDER CITY CANNOT BE BLANK                     *
     H*   30        INVALID STATE CODE                                *
     H*   31        PROVIDER ZIP CODE CANNOT BE BLANK                 *
     H*   32        PROVIDER FL LICENSE NUMBER CANNOT BE BLANK        *
     H*   33        DISPLAY BUT PROTECT FL LICENSE NUMBER             *
     H*  N33        COLUMN SEPARATORS ON FL LICENSE NUMBER            *
     H*   34        1ST TWO CHARACTERS OF FL LICENSE # MUST BE ALPHA  *
     H*   35        PROVIDER NAME CANNOT BE BLANK                     *
     H*   36        IMMEDIATE PROCESS FLAG MUST BE Y OR N             *
     H*   37        CARRIER RECEIVED DATE MUST BE VALID IF ENTERED    *
     H*   38        BILL SENT TO CARRIER DATE MUST BE VALID IF ENTRD  *
     H*   39        BILL SENT TO CARRIER REQUIRED                     *
     H*   40        CARRIER RECEIVED DATE REQUIRED                    *
     H*   41        TRANS CODE IS INVALID                             *
     H*   42        ADDTL EOB CODES (1) ARE REQUIRED IF TX,SYS=....   *
     H*   57        UPDATE MODE (NOT USED NOW - ALWAYS ADD MODE)      *
     H*   60        GENERAL CHAIN                                     *
     H*   71   WORKERS COMP INTERFACE                                 *
     H*   74   NETWORK/PPO INSTALLED                                  *
     H*   75   FBAL04 POS 1 = 'Y'                                     *
     H*   81   TAPE SUBMISSION INSTALLED                              *
     H*   90   NETWORK INSTALLED AND HBA                              *
     H*   98   INVALID SC#BLL ENTERED                                 *
     H*****************************************************************
     FF0086F    CF   E             WORKSTN USROPN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFMF1L     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMCML1    UF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFMPDL     UF   E           K DISK
     FFAEOP     O    E             DISK
     FFTABL     IF   E           K DISK    USROPN
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PPS01)
     FFDCAL     UF   E           K DISK
     FFMC1L     UF   E           K DISK
     FFADCL     UF   E           K DISK
     FFADUP     UF   E           K DISK
     FFADUL     UF   E           K DISK
     F                                     RENAME(PDU01:PDU02)
     FFADUL1    UF   E           K DISK
     F                                     RENAME(PDU01:PDU03)
     FFMP1L     IF   E           K DISK
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SRC01)
     FFMSML     IF   E           K DISK
     FFAHBP     O    E             DISK
     FNATRP     O    E             DISK
     FFXEOP     O    E             DISK
     F                                     RENAME(PEO01:PEO02)
     F*** BEGIN CHG *** 02/13/98 *************************************
     F** NMPHL   IF  E           K        DISK
     FFMS4L3    IF   E           K DISK
     F** NMHSL   IF  E           K        DISK
     FFMS4L2    IF   E           K DISK
     F                                     RENAME(PS401:PS402)
     F*** END   CHG *** 02/13/98 *************************************
     FFAE1P     O    E             DISK
     F*** ADD 7/21/95 *************************************
     F*** # TEMP CMNT OUT *** WMCIL   IF  E           K        DISK
     FWMCML1    IF   E           K DISK
     F                                     RENAME(PCM01:PWCM1)
     F*** BEGIN ADD *** 07/27/98 **************************************
     FFDHCL     UF   E           K DISK    USROPN
     FFTGTL     IF   E           K DISK    USROPN
     F*** END ADD *** 07/27/98 ****************************************
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PAT01)
     F                                     IGNORE(PSP01)
     F                                     IGNORE(PNC01)
     F                                     IGNORE(PNA01)
     F                                     IGNORE(PPA01)
     F                                     IGNORE(PCA01)
     F                                     IGNORE(PB201)
     F                                     IGNORE(PB701)
     F                                     IGNORE(PBS01)
     F                                     IGNORE(PTT01)
     F                                     IGNORE(PCC01)
     F*** BEG DEL *** 10/21/99 **********************
     F**********  PLM01                             KIGNORE
     F**********  PCR01                             KIGNORE
     F*** BEG DEL *** 10/21/99 **********************
     F                                     IGNORE(PBC01)
     F                                     IGNORE(PCS01)
     F                                     IGNORE(PCN01)
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PLR01)
     F                                     IGNORE(PVS01)
     F                                     IGNORE(PTR01)
     F                                     IGNORE(PHY01)
     F                                     IGNORE(PSY01)
     F                                     IGNORE(PBA01)
     F                                     IGNORE(PID01)
     F                                     IGNORE(PIS01)
     F*** # TEMP REMOVE ***  WTPSL   IF  E           K        DISK
     F*** END ADD 7/21/95 *************************************
     F*** END ADD *** 11/16/93 ****************************************
     FFAEOL4    UF   E           K DISK
     F                                     RENAME(PEO01:PEO03)
     FFAE1L     UF   E           K DISK
     F                                     RENAME(PE101:PE102)
     F*** BEGIN ADD *** 12/15/97 *************************************
     FFDMGL     UF   E           K DISK
     F*** END ADD *** 12/15/97 ***************************************
     F*** ADD NEXT LINE *** TAPE SUBMISSION ***************************

     D*** FILE ARRAYS
     D SSY             S              1    DIM(10)                              SUB SYSTEM CO DE
     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D XX              S              2    DIM(52) CTDATA PERRCD(26)
     D LT              S              1    DIM(26) CTDATA PERRCD(26)            ALPHABET
     D PTL             S             40    DIM(3) CTDATA PERRCD(1)              CALLED PGM TITLES
     D RT              S              2    DIM(10)                              CLM RESERVE TYPES
     D AR              S              9  2 DIM(10)                              CLM PAYMENTS
     D QCA             S             55    DIM(4) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     D*** BEG ADD *** 02/25/97
     D**            CMSCL#                          WCSCL#
     D*** END ADD *** 02/25/97
     D*** # TMP     CMCVFL                          WCCVFL
     D*** #  TMP    CMOCC                           WCOCC
     D**** END ADD 7/15/95 *********************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*** DATA STRUCTURE OF FL LICENSE # TO MAKE SURE FIRST TWO
     D*** CHARACTERS OF LICENSE # ARE ALPHA
     D                 DS
     D  SCLIC#                 1      9
     D  SCLIC1                 1      1
     D  SCLIC2                 2      2
     D                 DS
     D  ST                     1     20
     D                                     DIM(10)                              ADJ ST FROM SFBPF
     D  FBST01                 1      2
     D  FBST02                 3      4
     D  FBST03                 5      6
     D  FBST04                 7      8
     D  FBST05                 9     10
     D  FBST06                11     12
     D  FBST07                13     14
     D  FBST08                15     16
     D  FBST09                17     18
     D  FBST10                19     20
     D                 DS
     D  TP                     1     20
     D                                     DIM(10)                              ST TAPE FROM SFBPF
     D  FBTP01                 1      2
     D  FBTP02                 3      4
     D  FBTP03                 5      6
     D  FBTP04                 7      8
     D  FBTP05                 9     10
     D  FBTP06                11     12
     D  FBTP07                13     14
     D  FBTP08                15     16
     D  FBTP09                17     18
     D  FBTP10                19     20
     D*** QCMDEXC CMD STRING MAPPED
     D                 DS
     D  @@CMD                  1     55
     D  @@ST                  38     39
     D                 DS
     D  FSY                    1     10
     D                                     DIM(10)                              FILE SYSTEM CODE
     D  FNSY01                 1      1
     D  FNSY02                 2      2
     D  FNSY03                 3      3
     D  FNSY04                 4      4
     D  FNSY05                 5      5
     D  FNSY06                 6      6
     D  FNSY07                 7      7
     D  FNSY08                 8      8
     D  FNSY09                 9      9
     D  FNSY10                10     10
     D                 DS
     D  FBR                    1     30
     D                                     DIM(10)                              FILE BILL/RATE CD
     D  FNBR01                 1      3
     D  FNBR02                 4      6
     D  FNBR03                 7      9
     D  FNBR04                10     12
     D  FNBR05                13     15
     D  FNBR06                16     18
     D  FNBR07                19     21
     D  FNBR08                22     24
     D  FNBR09                25     27
     D  FNBR10                28     30
     D                 DS
     D  X120A1                 1     20
     D  X1NWCD                 1      5
     D  X1TYPE                 6      8
     D*** BEGIN ADD *** 07/27/95 ***********************************************
     D  X1AUDF                 9     11
     D*** END ADD *** 07/27/95 *************************************************
     D  X1STS                 13     14
     D*** BEG CHG *** 03/11/97 ***
     D************************************** 15  16 X1TOB
     D  X1TOB                 15     17
     D*** END CHG *** 03/11/97 ***
     D  X1DISH                18     19
     D  X1PCM                 20     20
     D*** BEGIN ADD *** 07/27/95 ***********************************************
     D*** BEGIN ADD *** 03/11/97 ***********************************************
     D                 DS
     D  E1AMT4                 1      9  2
     D  X1AMT4                 1      9
     D*** END  ADD *** 03/11/97 ***********
     D                 DS
     D  X120A2                 1     20
     D  X2GRTC                 1      1
     D  X2GRTP                 2      6
     D  X2HTOS                 7      9
     D*** END ADD *** 07/27/95 *************************************************
     D*** BEGIN ADD *** 12/02/96 *************************************
     D*  STATE RELATED FIELDS
     D                 DS
     D  XXSTAT                 1      3
     D  XXTAPE                 1      1
     D  XXPHAR                 2      2
     D  XXDENT                 3      3
     D*** END ADD ***** 12/02/96 *************************************
     D*** BEGIN ADD *** 07/27/98 **************************************
     D*
     D*  FLORIDA PAYMENT PLAN WORK AREA
     D*
     D                 DS
     D  HC10A1                 1     10
     D  XX10A1                 5      6
     D*** END ADD *** 07/27/98 ****************************************
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     ISCS01
     I              CSKEY                       CSKEYS
     I*** DEL 7/25/95 *** PRC0W
     I*** DEL 7/25/95 ***        RCKEY                           RCKEYW
     I**** ADD 7/15/95 *********************************************
     IPWCM1
     I              CMCO#                       WCCO#
     I              CMFND                       WCFND
     I              CMFYR                       WCFYR
     I              CMEMP#                      WCEMP#
     I              CMDIV                       WCDIV
     I              CMCASE                      WCCASE
     I              CMSS#                       WCSS#
     I              CMACDT                      WCACDT
     I              CMACTM                      WCACTM
     I              CMDTRP                      WCDTRP
     I              CMDTLW                      WCDTLW
     I              CMACDS                      WCACDS
     I              CMMCON                      WCMCON
     I              CMRT01                      WCRT01
     I              CMAR01                      WCAR01
     I              CMDTRE                      WCDTRE
     I              CMSTAT                      WCSTAT
     I              CMUSER                      WCUSER
     I              CMPGM                       WCPGM
     I              CMTIME                      WCTIME
     I              CMDT                        WCDT
     I              CMENTD                      WCENTD
     I              CMRCVD                      WCRCVD
     I              CMOSCL                      WCOSCL

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    SCCO#
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    XXSS#
     C*
     C*  CASE DETAIL FILE
     C*
     C     CAKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    QQFYR
     C                   KFLD                    SCCASE
     C                   KFLD                    SCFEIN
     C                   KFLD                    SCMOD
     C                   KFLD                    XXTRCD            2
     C                   KFLD                    SCBR2             2
     C*
     C*  PAYEE DEMOGRAPHICS FILE
     C*
     C     PDKEY         KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C*  TABLE FILE KEY
     C*
     C     TBKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    XXKEY            10
     C*
     C*   DIAG CODE WORK FILE
     C*
     C     DCKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    QQFYR
     C                   KFLD                    SCCASE
     C                   KFLD                    SCFEIN
     C                   KFLD                    SCMOD
     C                   KFLD                    XXDCS#
     C*
     C*  DUPLICATE CLAIM KEY
     C*
     C     DUKEY         KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CADSEQ
     C*
     C*  DUPLICATE CLAIM KEY (PDU02 - FADUL)
     C*
     C     DUKEY2        KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C                   KFLD                    CAFDAT
     C                   KFLD                    XXCODE            5
     C                   KFLD                    CAPMOD
     C*  DUPLICATE CLAIM KEY (PDU03 - FADUL1)
     C     DUKEY3        KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C                   KFLD                    CAFDAT
     C                   KFLD                    CASVCD
     C                   KFLD                    XXCODE            5
     C                   KFLD                    CAUNTS
     C*  PROVIDER AUXILIARY FILE
     C     P1KEY         KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C*  FS BASE CONTROL FILE
     C     FBKY          KLIST
     C                   KFLD                    XSKEY             2
     C*
     C*   KEY LIST FOR THE SUBSCRIBER
     C*
     C     SMKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCSUB#
     C*
     C*** BEG   CHG *** 02/13/98 *************************************
     C***PROVIDER*FILE**NMPHL**-*PHYSICIANS*ONLY**
     C*  PROVIDER FILE  FMS4LP - PHYSICIANS ONLY
     C*** END   CHG *** 02/13/98 *************************************
     C*
     C     PHKEY         KLIST
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C                   KFLD                    SCNWCD
     C                   KFLD                    EOTODT
     C*
     C*** BEG   CHG *** 02/13/98 *************************************
     C***PROVIDER*FILE**NMHSL**-*HOSPITALS*ONLY***
     C*  PROVIDER FILE  FMS4LH - HOSPITALS ONLY
     C*** END   CHG *** 02/13/98 *************************************
     C*
     C     HSKEY         KLIST
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C                   KFLD                    SCNWCD
     C                   KFLD                    EOTODT
     C*
     C*  CLAIM STATUS CODE TABLE FILE
     C*
     C     TCSKY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    TABKEY           10
     C*
     C*   KEY LIST FOR THE FUND AUXILLARY FILE
     C*
     C     F1KEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C*** BEGIN ADD *** 07/27/98 **************************************
     C*
     C*          PHC01 - DETAIL CHARGE AUXILIARY
     C*
     C     HCKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CADSEQ
     C*
     C*          FGT01 - GENERIC TABLE FILE
     C     GTKY          KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    XXID
     C                   KFLD                    XXKEY
     C     *LIKE         DEFINE    GTTBID        XXID
     C*
     C*** END ADD *** 07/27/98 ****************************************
     C***  ADD 7/21/95 ********************************************8
     C     CIKY          KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFND
     C                   KFLD                    X@PTYP
     C                   KFLD                    QQSYS
     C*
     C*   WORK COMP CLAIM MASTER
     C*
     C     CMKY          KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    HLWCF#
     C                   KFLD                    QQFYR
     C                   KFLD                    SCCASE
     C***  END ADD 7/21/95 ********************************************8
     C*
     C*** BEGIN ADD *** 07/26/95 ***********************************************
     C*
     C* KEY LIST FOR THE EOB FILE
     C*
     C     EOKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    QQFYR
     C                   KFLD                    SCCASE
     C                   KFLD                    SCEOB#
     C*
     C* KEY LIST FOR THE EOB AUXILIARY FILE
     C*
     C     E1KEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    QQFYR
     C                   KFLD                    SCCASE
     C                   KFLD                    SCEOB#
     C*** END ADD *** 07/26/95 *************************************************
     C*
     C*** BEGIN ADD *** 12/15/97 *************************************
     C*          PMG01 - Bill Entry Message File
     C     MGKY          KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    QQFYR
     C                   KFLD                    SCCASE
     C                   KFLD                    XXDCS#
     C*** END ADD *** 12/15/97 ***************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQPMCD            2
     C                   PARM                    QQFEIN
     C                   PARM                    QQMOD
     C                   PARM                    QQSFDT            8 0
     C                   PARM                    QQSTDT            8 0
     C                   PARM                    QQTOTP            9 2
     C                   PARM                    QQTOTC            9 2
     C                   PARM                    QQMRAR            9 2
     C                   PARM                    QQOTHR            9 2
     C                   PARM                    QQNDIS            9 2
     C                   PARM                    QQBR2             2
     C                   PARM                    QQSYS             1            USE W/PPO,HBA
     C                   PARM                    QQDCS#            5
     C                   PARM                    QQPRTY            1            N/H
     C                   PARM                    QQAST             3
     C                   PARM                    QQAUDF            3
     C                   PARM                    QQEOB#            9
     C                   PARM                    QQRTCD            1
     C*** BEG ADD *** 04/08/99 ****************************************
     C     SCCO#         PARM                    QQCO#             3 0
     C     SCSUB#        PARM                    QQSUB#            5 0
     C     SCFND         PARM                    QQFND             3 0
     C*** END ADD *** 04/08/99 ****************************************
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C* INITIALIZE WORK FIELDS
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C     *LIKE         DEFINE    CACO#         XXCO#
     C     *LIKE         DEFINE    CASUB#        XXSUB#
     C     *LIKE         DEFINE    CAFND         XXFND
     C     *LIKE         DEFINE    CAFYR         XXFYR
     C     *LIKE         DEFINE    CACASE        XXCASE
     C     *LIKE         DEFINE    CAFYR         QQFYR
     C     *LIKE         DEFINE    CACASE        QQCASE
     C     *LIKE         DEFINE    CMSS#         XXSS#
     C     *LIKE         DEFINE    CAFEIN        XXFEIN
     C     *LIKE         DEFINE    CAMOD         XXMOD
     C     *LIKE         DEFINE    CAFEIN        QQFEIN
     C     *LIKE         DEFINE    CAMOD         QQMOD
     C     *LIKE         DEFINE    CADCS#        XXDCS#
     C     *LIKE         DEFINE    EOAST         XXAST
     C     *LIKE         DEFINE    FNFND         HLFND
     C     *LIKE         DEFINE    FNWCF#        HLWCF#
     C     *LIKE         DEFINE    FNDT          XXTEST
     C     *LIKE         DEFINE    FNDT          XTDATE
     C     *LIKE         DEFINE    FNDT          XXACDT
     C     *LIKE         DEFINE    S4BGDT        XXFROM
     C     *LIKE         DEFINE    S4ENDT        XXTO
     C*** BEG ADD *** 04/08/99 ****************************************
     C     *LIKE         DEFINE    EOCO#         SCCO#
     C*** END ADD *** 04/08/99 ****************************************
     C                   MOVE      *BLANK        QFCO#             3
     C                   MOVE      *BLANK        QFFND             3
     C                   MOVE      *BLANK        QFRTCD            1
     C*
     C     QQRTCD        CABEQ     'P'           TERM                           PRE-OPEN FILES
     C*
     C*** BEG ADD *** 06/05/98 ***********************
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C*** END ADD *** 06/05/98 ***********************
     C*
     C                   OPEN      F0086F
     C*
     C                   Z-ADD     *ZERO         SCYR
     C                   MOVE      *ZERO         SCCASE
     C                   MOVEL     *BLANK        SCTRCD
     C                   MOVE      *ZERO         SCFEIN
     C                   MOVE      *ZERO         SCMOD
     C                   MOVE      *ZERO         SCFRDT
     C                   MOVE      *ZERO         SCTODT
     C                   MOVE      *ZERO         SCTOTP
     C                   MOVE      *ZERO         SCMRAR
     C                   MOVE      *ZERO         SCOTHR
     C                   MOVE      *ZERO         SCNDIS
     C                   MOVE      *BLANK        SCBR2
     C*** BEGIN ADD *** 07/26/95 ***********************************************
     C                   Z-ADD     *ZEROS        SCEOB#
     C                   Z-ADD     *ZEROS        SCPRVP
     C                   Z-ADD     *ZEROS        SCPAY
     C                   MOVE      '0'           *IN56
     C                   MOVE      '0'           *IN57
     C*** BEG DEL *** 3/11/97 ***
     C***********          MOVE *BLANKS   SCACCT
     C***********          Z-ADD*ZEROS    SCDFC
     C*** END DEL *** 3/11/97 ***
     C*** END ADD *** 07/26/95 *************************************************
     C*** BEGIN ADD *** 12/03/96 **************************************
     C**********           MOVE 'N'       SCRECO           DEL 01/29/98
     C**********           MOVE 'N'       SCRFND           DEL 01/29/98
     C*** END ADD *** 12/03/96 ****************************************
     C                   Z-ADD     QQFYR         SCYR
     C                   MOVE      QQCASE        SCCASE
     C                   MOVEL     QQPMCD        SCTRCD
     C                   MOVE      QQFEIN        SCFEIN
     C                   MOVE      QQMOD         SCMOD
     C                   MOVE      QQSFDT        SCFRDT
     C                   MOVE      QQSTDT        SCTODT
     C                   MOVE      QQTOTP        SCTOTP
     C                   MOVE      QQTOTP        SCTOTP
     C                   MOVE      QQTOTC        SCTOTC
     C                   MOVE      QQMRAR        SCMRAR
     C                   MOVE      QQOTHR        SCOTHR
     C                   MOVE      QQNDIS        SCNDIS
     C                   MOVE      QQBR2         SCBR2
     C                   MOVE      QQDCS#        XXDCS#
     C                   MOVEL     QQAST         XXAST
     C*** BEGIN ADD *** 07/27/95 ***********************************************
     C                   MOVE      QQAUDF        X1AUDF
     C                   MOVE      QQEOB#        SCEOB#
     C*** END ADD *** 07/37/95 *************************************************
     C*** BEG DEL *** 08/02/98 ***********************
     C***DETERMINE WHAT TO DEFAULT TO IN SCTAPE FLAG
     C***WHICH WILL BE USED TO DETERMINE IF THIS EOMB IS SENT TO STATE
     C********** QQPRTY    IFEQ 'N'
     C**********
     C********** XXAST     IFEQ 'FL'
     C********** QQSYS     ANDEQ'P'
     C********** XXAST     OREQ 'FL'                       DEL 05/22/98
     C********** QQSYS     ANDEQ'H'                        DEL 05/22/98
     C********** XXAST     OREQ 'TX'
     C********** QQSYS     ANDEQ'P'
     C********** XXAST     OREQ 'TX'
     C********** QQSYS     ANDEQ'H'
     C**********           MOVE 'Y'       SCTAPE
     C**********           ELSE
     C**********           MOVEL'N'       SCTAPE
     C**********           ENDIF                           F/TX P/H FOR Y DF
     C**********
     C**********           ELSE                            ELSE EQ NON-HOSP
     C**********           MOVE 'N'       SCTAPE
     C**********           END                             END EQ NON-HOSP
     C*** END DEL *** 08/02/98 ***********************
     C*
     C     QQSYS         IFEQ      'X'
     C                   MOVE      'N'           SCTAPE
     C                   ELSE                                                   ADD 08/02/98
     C                   EXSR      SRTAPE                                       ADD 08/02/98
     C                   ENDIF
     C*
     C*** BEG DEL *** 3/11/97 ***
     C*************        MOVE 'Y'       SCEMPL
     C*** END DEL *** 3/11/97 ***
     C                   MOVE      '1'           *IN12                          02/21/91 MOVE
     C                   MOVEL     QQAST         XXAST
     C*
     C     QQRTCD        COMP      *BLANK                                 99
     C*
     C** CHECK FOR INTERFACE WITH WORKERS' COMP
     C                   MOVE      *BLANKS       XSKEY
     C                   MOVEL     'FB'          XSKEY
     C     FBKY          CHAIN     SFB01                              60
     C     *IN60         IFEQ      '0'
     C     FBFL01        IFEQ      'Y'
     C                   MOVE      '1'           *IN71
     C                   ELSE                                                   ELSE FBFL01 = 'Y'
     C                   MOVE      '0'           *IN71
     C                   END                                                    END FBFL01 = 'Y'
     C*
     C* CHECK TO SEE IF PPO/NETWORK IS INSTALLED.  FBFL06 = 'Y' SET
     C*  *IN74 = '1'
     C*
     C*********  FBFL06    IFEQ 'Y'                        DEL 06/05/98
     C     ##NTWK        IFEQ      'Y'                                          ADD 06/05/98
     C                   MOVE      '1'           *IN74
     C                   ELSE
     C                   MOVE      '0'           *IN74
     C                   END
     C*** BEG ADD *** 08/06/98 ***************************************
     C     ##MED         IFNE      'Y'
     C                   MOVE      '0'           *IN79
     C                   ELSE
     C                   MOVE      '1'           *IN79
     C                   ENDIF
     C*** END ADD *** 08/06/98 ***************************************
     C*
     C* CHECK TO SEE WHAT VALUE IS IN FBFL09 FOR INTERACTIVE BATCH OR
     C* NOT
     C*
     C                   MOVE      '0'           XXSKIP            1
     C     FBFL09        IFEQ      'Y'
     C                   MOVE      'Y'           SCIPFL            1
     C                   ELSE
     C                   MOVE      'N'           SCIPFL
     C                   END
     C                   END                                                    END *IN60 SFB01
     C*
     C* CHECK TO SEE IF POS 1 OF FBAL04 = 'Y'
     C*  *IN75 = '1'
     C*
     C                   MOVEL     FBAL04        #@01              1
     C     #@01          IFEQ      'Y'
     C                   MOVE      '1'           *IN75
     C                   ELSE
     C                   MOVE      '0'           *IN75
     C                   END
     C*
     C*
     C     TOP           TAG
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPSUB#
     C                   MOVE      *ZERO         UPFND
     C                   END                                                    END 1 *IN60
     C*** BEG CHG *** 04/08/99 ****************************************
     C                   MOVE      SCCO#         UPCO#
     C                   MOVE      SCSUB#        UPSUB#
     C                   MOVE      SCFND         UPFND
     C*** END CHG *** 04/08/99 ****************************************
     C*
     C*** BEGIN ADD *** 07/26/95 ***********************************************
     C                   MOVE      *BLANKS       SCEOB3
     C*
     C                   Z-ADD     1             X
     C     1             SUBST     X1AUDF:X      XXAR              1
     C*
     C* IF REGULAR BILL LOAD PAYMENT FIELDC
     C*
     C     XXAR          IFEQ      *BLANKS
     C                   Z-ADD     SCTOTP        SCPAY
     C                   END
     C*
     C* IF 2ND+ TIME THROUGH AUDIT
     C*
     C     X1AUDF        IFGE      'A02'
     C                   MOVE      '1'           *IN57
     C                   END
     C*
     C* IF 1ST TIME THROUGH AUDIT
     C*
     C     X1AUDF        IFEQ      'A01'
     C                   MOVEL     '401'         SCEOB3
     C                   Z-ADD     SCTOTP        SCPAY
     C                   MOVE      '1'           *IN56
     C                   END
     C*
     C* IF RECON
     C*
     C     XXAR          IFEQ      'R'
     C                   MOVEL     '404'         SCEOB3
     C                   Z-ADD     SCTOTP        SCPAY
     C                   MOVE      '1'           *IN57
     C                   END
     C*
     C* IF AUDIT 2ND + TIME, CHAIN TO EOB FILE TO GET APPROPRIATE RCD
     C*
     C     *IN57         IFEQ      '1'
     C     EOKEY         CHAIN     PEO03                              60
     C     E1KEY         CHAIN     PE102                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           QQRTCD
     C                   GOTO      TERM
     C                   END                                                    END *IN60
     C                   MOVEL     SCSTS         X1STS
     C                   Z-ADD     18            X
     C     2             SUBST     E120A1:X      X1DISH                 65      ADD 07/21/95
     C                   Z-ADD     15            X
     C     3             SUBST     E120A1:X      X1TOB                  65      ADD 07/21/95
     C                   Z-ADD     20            X
     C     1             SUBST     E120A1:X      X1PCM                  65      ADD 07/21/95
     C                   Z-ADD     1             X
     C     5             SUBST     E120A1:X      X1NWCD                 65
     C                   MOVE      *BLANKS       SCNWCD
     C                   MOVEL     X1NWCD        SCNWCD
     C                   Z-ADD     6             X
     C     3             SUBST     E120A1:X      X1TYPE                 65      ADD 05/25/94
     C     EOAST         IFNE      XXAST
     C     QQSYS         IFEQ      'X'
     C                   MOVE      'N'           SCTAPE
     C                   ELSE                                                   ADD 08/02/98
     C                   EXSR      SRTAPE                                       ADD 08/02/98
     C                   ENDIF
     C*** BEG DEL *** 08/02/98 ***********************
     C*** DELETED OBSOLETE CODE
     C**********           Z-ADD1         X
     C********** EOAST     LOKUPST,X                     60
     C********** *IN60     IFEQ '1'
     C****IF*THIS IS A VALID ADJUDICATION STATE, SEE IF STATE HAS TAPE SUBM
     C**********           Z-ADD1         X
     C********** EOAST     LOKUPTP,X                     60
     C********** *IN60     IFEQ '1'
     C********** EOAST     IFEQ 'FL'
     C********** QQSYS     ANDEQ'P'
     C********** EOAST     OREQ 'FL'                       DEL 05/22/98
     C********** QQSYS     ANDEQ'H'                        DEL 05/23/98
     C********** EOAST     OREQ 'TX'
     C********** QQSYS     ANDEQ'P'
     C********** EOAST     OREQ 'TX'
     C********** QQSYS     ANDEQ'H'
     C**********
     C**********           MOVE 'Y'       SCTAPE
     C**********           ELSE
     C**********           MOVEL'N'       SCTAPE
     C**********           ENDIF                           F/TX P/H FOR Y DF
     C**********
     C**********           MOVE '1'       *IN81            ADD 02/05/92
     C**********           ELSE                            ELSE *IN60 LOKUP TP
     C**********           MOVE 'N'       SCTAPE
     C**********           MOVE '0'       *IN81            ADD 02/05/92
     C**********           END                             END *IN60 LOKUP TP
     C**********           END                             END *IN60 LOKUP ST
     C*** END DEL *** 08/02/98 ***********************
     C                   END                                                    END EOAST
     C*
     C     X1AUDF        IFEQ      'A02'
     C     X1AUDF        OREQ      'R01'
     C                   Z-ADD     EOAMT         SCPRVP
     C                   ELSE                                                   ELSEQ 2
     C                   Z-ADD     EOAMT         SCPRVP
     C                   END                                                    ENDEQ 2
     C     SCTOTP        SUB       SCPRVP        SCPAY
     C     XXAR          IFEQ      'A'
     C     SCPAY         IFGE      *ZERO
     C                   MOVEL     '402'         SCEOB3
     C                   ELSE                                                   ELSEGE *ZERO
     C                   MOVEL     '403'         SCEOB3
     C                   END                                                    ENDGE *ZERO
     C                   END                                                    END XXAR EQ 'A'
     C                   END                                                    END *IN57
     C*** END ADD *** 07/26/95 *************************************************
     C     *IN71         IFEQ      '1'
     C     F1KEY         CHAIN     PF101                              50
     C     *IN50         IFEQ      '0'
     C     F1OUTA        IFEQ      'Y'
     C                   MOVE      '0'           *IN71
     C                   END                                                    END F1OUTA
     C                   END                                                    END *IN50
     C                   END                                                    END *IN71
     C*
     C* CHECK TO SEE IF FBAL04 IS 'Y' AND FBFL01 IS "Y" AND
     C*  F1OUTA IS 'N' THEN SETON 76.
     C*
     C     *IN71         IFEQ      *ON
     C     *IN75         ANDEQ     *ON
     C                   MOVE      '1'           *IN76
     C                   ELSE
     C                   MOVE      '0'           *IN76
     C                   END
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*** BEGIN ADD *** 07/27/98 **************************************
     C     *IN78         IFEQ      *OFF
     C     COST          ANDEQ     'FL'
     C                   SETON                                        78
     C                   OPEN      FDHCL
     C                   OPEN      FTGTL
     C                   ENDIF                                                  *IN78 = Off
     C*** END ADD *** 07/27/98 ****************************************
     C*
     C*  GET THE SUBSCRIBER RECORD
     C     SMKEY         CHAIN     PSM01                              60
     C                   MOVE      SMSUB#        SCSUB#
     C*
     C*  GET THE FUND RECORD
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVE      SCFND         SCFND
     C                   MOVE      SCFND         HLFND                          ADD 10/07/92
     C                   MOVE      FNWCF#        HLWCF#                         ADD 10/07/92
     C*
     C*********  *IN74     IFEQ '1'                        DEL 06/05/98
     C     ##AR          IFEQ      'Y'                                          ADD 06/05/98
     C     QQSYS         IFEQ      'H'
     C                   MOVE      '1'           *IN90
     C                   ELSE                                                   ELSE EQ H
     C                   MOVE      '0'           *IN90
     C                   END                                                    END EQ H
     C                   END                                                    END *IN74
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C                   MOVE      '0'           *IN72
     C     QQCALL        COMP      '1'                                    09
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*  EDIT SCREEN AND RETURN IF ERROR
     C*
     C                   EXSR      EDIT
     C     ERR           CABEQ     '1'           TERM
     C*
     C*  PAYEE MUST BE ON FILE IF ENTERED
     C*
     C     SCFEIN        IFNE      *ZERO
     C                   Z-ADD     SCFEIN        XXFEIN
     C                   MOVE      SCMOD         XXMOD
     C*
     C     PDKEY         CHAIN     PPD01                              22
     C  N22PDSTAT        COMP      'I'                                    22
     C     *IN22         IFEQ      '1'
     C                   MOVE      '1'           ERR
     C                   GOTO      TERM
     C                   END
     C*
     C*** SINCE PPD01 RECORDS ARE READ IN AS UPDATE, MUST NOW RELEASE THE
     C*** RECORD WE JUST READ TO PREVENT REC LOCKS...
     C                   EXCEPT    EPD01
     C*** BEG ADD *** 3/11/97 ***
     C*****      PDWCCE    IFEQ 'N'                        DEL 11/07/97
     C*****      PDWCCE    OREQ *BLANK                     DEL 11/07/97
     C*****                MOVE 'NON-BWC' SCBWC            DEL 11/07/97
     C*****                MOVE *ON       *IN82            DEL 11/07/97
     C*****                ELSE                            DEL 11/07/97
     C*****                MOVE *BLANKS   SCBWC            DEL 11/07/97
     C*****                MOVE *OFF      *IN82            DEL 11/07/97
     C*****                ENDIF                           DEL 11/07/97
     C*** END ADD *** 3/11/97 ***
     C*
     C                   END
     C*
     C*  MOVE DEMOGRAPHIC DATA IF NO PAYEE ENTERED
     C                   MOVE      CMSS#         SCSS#
     C                   Z-ADD     CMACDT        XXACDT                         ##
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C                   MOVE      CMSS#         XXSS#
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN28
     C                   MOVE      '1'           ERR
     C                   GOTO      TERM
     C                   END                                                    END 1 *IN60
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     CDLSN         XXLAST           56
     C                   MOVEL     CDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        SCCNAM
     C     SCFEIN        IFEQ      *ZERO
     C                   MOVEL     SCCNAM        SCPNAM
     C                   MOVEL     CDADR1        SCADR1
     C                   MOVEL     CDADR2        SCADR2
     C                   MOVEL     CDCTY         SCCTY
     C                   MOVE      CDST          SCST
     C                   MOVEL     CDZIP         SCZP1
     C                   MOVE      CDZIP         SCZIP2
     C                   ELSE                                                   ELSE EQ *ZERO
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     PDNAME        XXLAST           56
     C     PDFSN         IFNE      *BLANK
     C                   MOVEL     PDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   END                                                    END NE *BLANK
     C                   MOVEL     XXLAST        SCPNAM
     C                   MOVEL     PDADR1        SCADR1
     C                   MOVEL     *BLANKS       SCADR2
     C                   MOVEL     PDCTY         SCCTY
     C                   MOVE      PDST          SCST
     C*
     C********** QQPRTY    IFEQ 'N'                        ***  DEL 7/21/95
     C*** GET SYSTEM FS BASE CONTROL FILE TO DETERMINE IF THIS **
     C*** IF A VALID ADJUDICATION STATE
     C*
     C*** BEGIN ADD *** 12/02/96 **************************************
     C*** BEG DEL *** 08/02/98 ***********************
     C* INCREASING NUMBER OF STATES,  CALL CHKSTR IF EXISTS
     C**********
     C**********           MOVELXXAST     QQSTT
     C**********           CALL 'CHKSTR'               99
     C**********           PARM           QQSTT   2
     C**********           PARM           QQSTRT  1
     C**********           PARM           QQFLRT  3
     C********** *IN99     IFEQ '0'
     C**********           MOVELQQFLRT    XXSTAT
     C********** QQSTRT    IFEQ '1'
     C********** XXTAPE    IFEQ 'Y'
     C****BEG*ADD ***** 07/30/98 **************************************
     C********** XXAST     IFEQ 'FL'
     C********** QQSYS     ANDEQ'P'
     C********** XXAST     OREQ 'TX'
     C********** QQSYS     ANDEQ'P'
     C********** XXAST     OREQ 'TX'
     C********** QQSYS     ANDEQ'H'
     C****END*ADD ***** 07/30/98 **************************************
     C**********
     C****BEG*DEL ***** 07/30/98 **************************************
     C********** XXAST     ANDNE'FL'                       ADD 05/22/98
     C********** QQSYS     ANDNE'H'                        ADD 05/22/98
     C****END*DEL ***** 07/30/98 **************************************
     C**********           MOVE 'Y'       SCTAPE
     C**********           MOVE '1'       *IN81
     C****BEG*ADD ***** 07/30/98 **************************************
     C**********           ENDIF                           ADD 07/30/98
     C****END*ADD ***** 07/30/98 **************************************
     C**********           ELSE
     C**********           MOVE 'N'       SCTAPE
     C**********           MOVE '0'       *IN81
     C**********           ENDIF                           END LOKUP ST
     C**********           ELSE
     C**********           MOVE 'N'       SCTAPE
     C**********           MOVE '0'       *IN81
     C**********           ENDIF                           END LOKUP ST
     C**********           ELSE
     C****END*ADD ***** 12/02/96 **************************************
     C**********           MOVE *BLANKS   XSKEY
     C**********           MOVEL'FB'      XSKEY
     C********** FBKY      CHAINSFB01                60
     C********** *IN60     IFEQ '0'
     C**********           Z-ADD1         X
     C********** XXAST     LOKUPST,X                     60
     C********** *IN60     IFEQ '1'
     C****IF*THIS IS A VALID ADJUDICATION STATE, SEE IF STATE HAS TAPE SUBM
     C**********           Z-ADD1         X
     C********** XXAST     LOKUPTP,X                     60
     C********** *IN60     IFEQ '1'
     C**********
     C********** XXAST     IFEQ 'FL'
     C********** QQSYS     ANDEQ'P'
     C********** XXAST     OREQ 'FL'                       DEL 05/22/98
     C********** QQSYS     ANDEQ'H'                        DEL 05/22/98
     C********** XXAST     OREQ 'TX'
     C********** QQSYS     ANDEQ'P'
     C********** XXAST     OREQ 'TX'
     C********** QQSYS     ANDEQ'H'
     C**********
     C**********           MOVE 'Y'       SCTAPE
     C**********           ELSE
     C**********           MOVEL'N'       SCTAPE
     C**********           ENDIF                           F/TX P/H FOR Y DF
     C**********
     C**********           MOVE '1'       *IN81            ADD 02/05/92
     C**********           ELSE                            ELSE *IN60 LOKUP TP
     C**********           MOVE 'N'       SCTAPE
     C**********           MOVE '0'       *IN81            ADD 02/05/92
     C**********           END                             END *IN60 LOKUP TP
     C**********           END                             END *IN60 LOKUP ST
     C**********           END                             END *IN60 SFB01
     C*** END DEL *** 08/02/98 ***********************
     C*
     C     QQSYS         IFEQ      'X'
     C                   MOVE      'N'           SCTAPE
     C                   ELSE                                                   ADD 08/02/98
     C                   EXSR      SRTAPE                                       ADD 08/02/98
     C                   ENDIF
     C*** BEGIN ADD *** 12/02/96 **************************************
     C                   ENDIF                                                  END *IN99
     C*** END ADD ***** 12/02/96 **************************************
     C*
     C* BECAUSE OF THE ABILITY TO CHANGE THE ADJUDICATION STATE IN BILL
     C* ENTRY TEMPORARILY WE MUST CHECK TO MAKE SURE WHAT STATE THE
     C* CHARGES WERE RUN AGAINST TO DISPLAY TAPE 'Y' OR 'N' CORRECTLY
     C*
     C     DCKEY         CHAIN     PDC01                              60
     C     *IN60         IFEQ      '0'
     C                   EXCEPT    EDC01
     C                   Z-ADD     1             X
     C     5             SUBST     DC10A1:X      X1NWCD                 65
     C                   MOVE      *BLANKS       SCNWCD
     C                   MOVEL     X1NWCD        SCNWCD
     C                   Z-ADD     6             X
     C     3             SUBST     DC10A1:X      X1TYPE                 65      ADD 05/25/94
     C                   Z-ADD     9             X
     C     2             SUBST     DC10A1:X      X1DISH                 65      ADD 07/21/95
     C*
     C*
     C                   Z-ADD     7             X
     C     3             SUBST     DC10A2:X      X1TOB                  65      ADD 07/21/95
     C*
     C*
     C                   Z-ADD     10            X
     C     1             SUBST     DC10A2:X      X1PCM                  65      ADD 07/21/95
     C*
     C     DCAST         IFNE      XXAST
     C*
     C*** BEGIN ADD *** 12/02/96 **************************************
     C*** BEG DEL *** 08/02/98 ***********************
     C* INCREASING NUMBER OF STATES,  CALL CHKSTR IF EXISTS
     C**********
     C**********           MOVELDCAST     QQSTT
     C**********           CALL 'CHKSTR'               99
     C**********           PARM           QQSTT   2
     C**********           PARM           QQSTRT  1
     C**********           PARM           QQFLRT  3
     C********** *IN99     IFEQ '0'
     C**********           MOVELQQFLRT    XXSTAT
     C********** QQSTRT    IFEQ '1'
     C********** XXTAPE    IFEQ 'Y'
     C********** XXAST     ANDNE'FL'                       ADD 05/22/98
     C********** QQSYS     ANDNE'H'                        ADD 05/22/98
     C**********           MOVE 'Y'       SCTAPE
     C**********           MOVE '1'       *IN81
     C**********           ELSE
     C**********           MOVE 'N'       SCTAPE
     C**********           MOVE '0'       *IN81
     C**********           ENDIF                           END TAPE
     C**********           ELSE                            ELSE *IN60 LOKUP TP
     C**********           MOVE 'N'       SCTAPE
     C**********           MOVE '0'       *IN81            ADD 02/05/92
     C**********           ENDIF                           END QQSTRT
     C**********           ELSE
     C****END*ADD ***** 12/02/96 **************************************
     C**********           Z-ADD1         X
     C********** DCAST     LOKUPST,X                     60
     C********** *IN60     IFEQ '1'
     C****IF*THIS IS A VALID ADJUDICATION STATE, SEE IF STATE HAS TAPE SUBM
     C**********           Z-ADD1         X
     C********** DCAST     LOKUPTP,X                     60
     C********** *IN60     IFEQ '1'
     C********** DCAST     IFEQ 'FL'
     C********** QQSYS     ANDEQ'P'
     C********** DCAST     OREQ 'FL'                       DEL 05/22/98
     C********** QQSYS     ANDEQ'H'                        DEL 05/22/98
     C********** DCAST     OREQ 'TX'
     C********** QQSYS     ANDEQ'P'
     C********** DCAST     OREQ 'TX'
     C********** QQSYS     ANDEQ'H'
     C**********
     C**********           MOVE 'Y'       SCTAPE
     C**********           ELSE
     C**********           MOVEL'N'       SCTAPE
     C**********           ENDIF                           F/TX P/H FOR Y DF
     C**********
     C**********           MOVE '1'       *IN81            ADD 02/05/92
     C**********           ELSE                            ELSE *IN60 LOKUP TP
     C**********           MOVE 'N'       SCTAPE
     C**********           MOVE '0'       *IN81            ADD 02/05/92
     C**********           END                             END *IN60 LOKUP TP
     C**********           END                             END *IN60 LOKUP ST
     C**********           END                             END DCAST
     C**********           END                             END *IN60
     C*** END DEL *** 08/02/98 ***********************
     C*
     C     QQSYS         IFEQ      'X'
     C                   MOVE      'N'           SCTAPE
     C                   ELSE                                                   ADD 08/02/98
     C                   EXSR      SRTAPE                                       ADD 08/02/98
     C                   ENDIF
     C*** BEGIN ADD *** 12/02/96 **************************************
     C                   END                                                    END *IN99
     C*** END ADD ***** 12/02/96 **************************************
     C*
     C*** DEL 7/21/95 **************************************************
     C**********           ELSE
     C*          DCKEY     CHAINPDC01                60
     C*          *IN60     IFEQ '0'
     C*                    EXCPTEDC01
     C*
     C*                    Z-ADD1         X
     C*          5         SUBSTDC10A1:X  X1NWCD       65
     C*                    MOVE *BLANKS   SCNWCD
     C*                    MOVELX1NWCD    SCNWCD
     C*                    Z-ADD6         X
     C*          3         SUBSTDC10A1:X  X1TYPE       65  ADD 05/25/94
     C*                    END                             END *IN60
     C***** DEL 7/21/95  **END                             END QQPRTY = N
     C*** END DEL 7/21/95 **************************************************
     C*
     C                   Z-ADD     9             X
     C     2             SUBST     DC10A1:X      X1DISH                 65      ADD 07/21/95
     C*
     C*
     C                   Z-ADD     7             X
     C     3             SUBST     DC10A2:X      X1TOB                  65      ADD 07/21/95
     C*
     C*
     C                   Z-ADD     10            X
     C     1             SUBST     DC10A2:X      X1PCM                  65      ADD 07/21/95
     C*
     C*** CLOSE FILE IF OPENED
     C     STOPN         IFEQ      '1'
     C                   CLOSE     FTABL
     C                   MOVE      ' '           STOPN             1
     C                   MOVE      ' '           STOVR
     C                   END
     C*
     C*** DELETE OVERRIDE
     C     STOVR         IFEQ      '1'
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           STOVR             1
     C                   END
     C*
     C*** OVERRIDE FMPRL FILE TO SELECTED STATE CODE
     C     STOVR         IFEQ      ' '
     C                   Z-ADD     55            LENGTH
     C                   MOVEL     QCA(2)        @@CMD
     C                   MOVEL     XXAST         @@ST
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           STOVR             1
     C                   END
     C*
     C*** OPEN FILE FOR PROCESSING
     C     STOPN         IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FTABL
     C                   MOVE      '1'           STOPN
     C                   END
     C*
     C                   MOVEL     PDZIP         SCZP1
     C                   MOVE      PDZIP         SCZIP2
     C*
     C                   MOVE      *BLANKS       SCLIC#
     C                   END                                                    END EQ *ZERO
     C*
     C*  CLEAR FIELDS FOR SCREEN 1
     C*
     C*** BEGIN CHG *** 07/26/95 ***********************************************
     C     *IN57         IFEQ      '0'
     C                   MOVE      UA(1)         SCMODE
     C                   ELSE
     C                   MOVE      UA(2)         SCMODE
     C                   END
     C*** END CHG *** 07/26/95 *************************************************
     C                   MOVE      *BLANKS       SCEOB1
     C                   MOVE      *BLANKS       SCEOB2
     C*** BEGIN ADD *** 07/27/98 **************************************
     C                   MOVE      'FS'          SCPMNT
     C*** END ADD *** 07/27/98 ****************************************
     C*** BEG DEL *** 3/11/97 ***
     C***************      Z-ADD*ZEROS    SCCRDT           ADD 07/15/93
     C***************      Z-ADD*ZEROS    SCDT9            ADD 07/21/95
     C*** END DEL *** 3/11/97 ***
     C*
     C**** ADD 7/21/95 ************************************************
     C*
     C*** GET WORK COMP CLAIMS FILE
     C*
     C     CMKY          CHAIN     PWCM1                              60
     C     *IN60         IFEQ      '1'
     C*** # TEMP CMMNT **  MOVEL' '       WCCVFL
     C                   ENDIF
     C*
     C     *IN76         IFEQ      '1'
     C* DEFAULT TRANS CODE
     C                   MOVEL     'P'           X@PTYP            1
     C*** TEMP. COMMENT OUT THE SOURCE HERE ****************
     C*** #      CIKY      CHAINPCI01                60
     C*** #      *IN60     IFEQ '0'
     C*** #      WCCVFL    IFEQ 'Y'
     C*** #                MOVELCITRCV    SCTRCD
     C*** #                ELSE
     C*** #                MOVELCITRCD    SCTRCD
     C*** #                ENDIF
     C*** #                ELSE
     C*** #                MOVEL*BLANKS   SCTRCD
     C*** #                ENDIF
     C***
     C***** PEND STATUS
     C*** #                MOVELCISTS     SCSTS
     C*** END  TEMP COMMENT OUT THE SOURCE HERE ****************
     C*
     C                   ENDIF
     C*
     C*
     C**** END ADD 7/21/95 ********************************************
     C*
     C                   Z-ADD     1             SC#BLL
     C*** BEG DEL *** 3/11/97 ***
     C****************     MOVE *ZERO     SCDFC
     C*** END DEL *** 3/11/97 ***
     C*
     C                   SETON                                        50
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C*****************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C***************************************
     C*   COMMAND 03 = END OF JOB           *
     C***************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C***************************************
     C*  TABLE FILE SEARCH                  *
     C***************************************
     C***************************************
     C     *INKJ         IFEQ      '1'
     C*** BEGIN ADD *** 07/27/98 **************************************
     C**************
     C* Payment Plan
     C**************
     C*
     C     *IN78         IFEQ      *ON
     C     COST          ANDEQ     'FL'
     C     HFLD          ANDEQ     'SCPMNT'
     C                   MOVE      *BLANK        QTCALL            1
     C                   MOVE      'FLPP'        QQID              4
     C                   MOVE      *BLANKS       QQKEY            10
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'F2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QTCALL
     C                   PARM                    QQID
     C                   PARM                    QQKEY
     C                   PARM                    QQRTCD
     C*
     C*
     C*                 Return payment plan to screen
     C*                ------------------------------------------------
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL     QQKEY         SCPMNT
     C                   ENDIF                                                  = '*'
     C                   ENDIF                                                  *IN78 = On
     C*
     C*** END ADD *** 07/27/98 ****************************************
     C******************
     C*  EOMB CODE 1   *
     C******************
     C                   MOVEL     SCEOB1        XX2               2
     C     XX2           IFEQ      '? '
     C*
     C                   MOVE      *BLANK        SCKEY            10
     C                   MOVE      *BLANK        QQPRDS
     C                   MOVEL     PTL(1)        QQPRDS
     C                   MOVEL     XXAST         QQST
     C                   CALL      'F2131R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCKEY
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQST              2
     C                   MOVEL     SCKEY         SCEOB1
     C                   MOVE      '1'           *IN41
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END EOB 1
     C******************
     C*  EOMB CODE 2   *
     C******************
     C                   MOVEL     SCEOB2        XX2               2
     C     XX2           IFEQ      '? '
     C*
     C                   MOVE      *BLANK        SCKEY            10
     C                   MOVE      *BLANK        QQPRDS
     C                   MOVEL     PTL(1)        QQPRDS
     C                   MOVEL     XXAST         QQST
     C                   CALL      'F2131R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCKEY
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQST              2
     C                   MOVEL     SCKEY         SCEOB2
     C                   MOVE      '1'           *IN41
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END EOB 2
     C******************
     C*  EOMB CODE 3   *
     C******************
     C                   MOVEL     SCEOB3        XX2               2
     C     XX2           IFEQ      '? '
     C*
     C                   MOVE      *BLANK        SCKEY            10
     C                   MOVE      *BLANK        QQPRDS
     C                   MOVEL     PTL(1)        QQPRDS
     C                   MOVEL     XXAST         QQST
     C                   CALL      'F2131R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCKEY
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQST              2
     C                   MOVEL     SCKEY         SCEOB3
     C                   MOVE      '1'           *IN41
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END EOB3
     C*
     C*  CMD 10 - TABLE FILE SEARCH - PAYMENT CODE
     C*
     C                   MOVEL     SCTRCD        XX2               2
     C     XX2           IFEQ      '? '
     C*
     C                   MOVE      *BLANK        SCKEY            10
     C                   CALL      'W2141R'
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    SCKEY
     C                   PARM                    QQRTCD            1
     C                   MOVEL     SCKEY         SCTRCD
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  CMD 10 - TABLE FILE SEARCH - SCREEN 1- REMARK 1
     C*
     C                   MOVEL     SCRMK1        XX2               2
     C     XX2           IFEQ      '? '
     C*
     C                   MOVE      *BLANK        SCKEY            10
     C                   CALL      'W2171R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCKEY
     C                   PARM                    QQRTCD            1
     C                   MOVEL     SCKEY         SCRMK1
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*  CMD 10 - TABLE FILE SEARCH - SCREEN 1- REMARK 2
     C*
     C                   MOVEL     SCRMK2        XX2               2
     C     XX2           IFEQ      '? '
     C*
     C                   MOVE      *BLANK        SCKEY            10
     C                   CALL      'W2171R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCKEY
     C                   PARM                    QQRTCD            1
     C                   MOVEL     SCKEY         SCRMK2
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*  CMD 10 - TABLE FILE SEARCH - SCREEN 1- REMARK 3
     C*
     C                   MOVEL     SCRMK3        XX2               2
     C     XX2           IFEQ      '? '
     C*
     C                   MOVE      *BLANK        SCKEY            10
     C                   CALL      'W2171R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCKEY
     C                   PARM                    QQRTCD            1
     C                   MOVEL     SCKEY         SCRMK3
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*  CMD 10 - TABLE FILE SEARCH - PEND STS
     C*
     C                   MOVEL     SCSTS         XX2               2
     C     XX2           IFEQ      '? '
     C*
     C*** BEGIN DEL *** 03/27/98 **************************************
     C********             MOVE *BLANK    SCKEY  10
     C********             CALL 'W2143R'
     C********             PARM           QQPRDS 40
     C********             PARM           QQCALL  1
     C********             PARM           SCKEY
     C********             PARM           QQRTCD  1
     C********             MOVELSCKEY     SCSTS
     C********   QQRTCD    CABEQ'A'       TERM
     C********
     C********             WRITESCR99
     C********             MOVE *BLANK    Q2RECF
     C********             MOVEL'SCR01'   Q2RECF
     C********             GOTO STRT
     C*** END DEL *** 03/27/98 ****************************************
     C*** BEGIN ADD *** 03/27/98 **************************************
     C                   CALL      'W2142R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C                   PARM      '0'           ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
     C                   MOVEL     ##KEY         SCSTS
     C     ##KC          CABEQ     '1'           TERM
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*** END ADD *** 03/27/98 ****************************************
     C                   ENDIF
     C*
     C*
     C                   GOTO      STRT
     C                   END                                                    END *INKJ
     C***************************************
     C*   F15 = T/M ENTRY                   *
     C***************************************
     C*** BEG DEL *** 10/07/99 ****************************************
     C********** *INKP     IFEQ '1'
     C**********           MOVE '1'       *IN72
     C**********
     C**********           GOTO SCR01T
     C********** TMMT      TAG
     C********** *IN74     IFEQ '1'
     C**********           MOVE 'Y'       Q1NTFL
     C**********           ELSE
     C**********           MOVE 'N'       Q1NTFL
     C**********           END
     C********** SCIPFL    IFEQ 'Y'
     C**********           MOVE 'Y'       Q1CHK
     C**********           ELSE
     C**********           MOVE 'N'       Q1CHK
     C**********           END
     C**********           CALL 'F6000CI'
     C**********           PARM           Q1CO#   3
     C**********           PARM           Q1SUB#  5
     C**********           PARM           Q1FND   3
     C**********           PARM           Q1FYR   3
     C**********           PARM           Q1CASE  7
     C**********           PARM           Q1EOB#  9
     C**********           PARM           Q1NTFL  1
     C**********           PARM 'N'       Q1CERT  1        ADD 04/28/93
     C**********           PARM 'Y'       Q1TM    1
     C**********           PARM           Q1CHK   1
     C**********           PARM           Q1SYS   1
     C**********           MOVE '1'       XXSKIP  1
     C**********
     C**********           MOVE *BLANK    Q1PRDS
     C**********           MOVELPTL,3     Q1PRDS
     C**********           CALL 'N0030R'
     C**********           PARM           Q1PRDS 40
     C**********           PARM '2'       Q1CALL  1
     C**********           PARM           Q1SUB#
     C**********           PARM           Q1FND
     C**********           PARM           Q1FYR
     C**********           PARM           Q1CASE
     C**********           PARM           Q1EOB#
     C**********           PARM           Q1RTCD  1
     C**********           MOVE '0'       *IN72
     C**********
     C**********           GOTO TERM
     C**********
     C**********           END                             END *INKP
     C**********
     C*** END ADD *** 10/07/99 ****************************************
     C***************************************
     C*   PROCESS HELP REQUESTS             *
     C***************************************
     C     *IN10         IFEQ      '1'
     C*
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END                                                    END *IN10
     C***************************************
     C*   CMD 22 - BYPASS THIS ITEM         *
     C***************************************
     C     *INKW         IFEQ      '1'
     C*** BEGIN ADD *** 08/09/95 ***********************************************
     C     *IN57         IFEQ      '1'
     C                   EXCEPT    PEO03E
     C                   END
     C*** END ADD *** 08/09/95 *************************************************
     C     *IN74         IFEQ      '1'
     C                   MOVEL     *BLANKS       XXTRCD
     C     CAKEY         CHAIN     PCA01                              60
     C     *IN60         DOWEQ     '0'
     C                   MOVE      *BLANKS       TABKEY
     C                   MOVEL     CACSCD        TABKEY
     C     TCSKY         CHAIN     PCS01                              60
     C     *IN60         IFEQ      '0'
     C     CSDISC        ANDEQ     'C'
     C                   UPDATE    PCA01
     C                   END                                                    END EQ 0/'C'
     C     CAKEY         READE     PCA01                                  60
     C                   END                                                    END 0 *IN60
     C                   END                                                    END 1 *IN74
     C                   GOTO      TERM
     C                   END                                                    END *INKW
     C*
     C                   END                                                    END CMDKEYS
     C*****************************************************************
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C* PROCESS SCREEN 1 - PAYMENT INFORMATION
     C*
     C     SCR01T        TAG
     C                   SETOFF                                       50
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTEST
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XTDATE
     C*
     C*  PATIENT EMPLOYMENT MUST BE YES OR NO
     C*******
     C*** BEG DEL *** 3/11/97 ***
     C*******    SCEMPL    IFNE 'Y'
     C*******    SCEMPL    ANDNE'N'
     C*******              MOVE '1'       *IN22
     C*******              GOTO STRT
     C*******              END                             END NE Y/N
     C*** END DEL *** 3/11/97 ***
     C*
     C*  TAPE SUBMISSION FLAG MUST BE YES OR NO
     C     SCTAPE        IFNE      'Y'
     C     SCTAPE        ANDNE     'N'
     C                   MOVE      '1'           *IN27                          Y/N REQD
     C                   GOTO      STRT
     C                   END                                                    END NE Y/N
     C*** BEGIN ADD *** 07/27/98 **************************************
     C*
     C     *IN78         IFEQ      *ON
     C     COST          ANDEQ     'FL'
     C                   MOVE      'FLPP'        XXID
     C                   MOVEL(P)  SCPMNT        XXKEY
     C     GTKY          CHAIN     PGT01                              50
     C     *IN50         IFEQ      *ON
     C                   SETON                                        38
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN50 = On
     C                   ENDIF                                                  *IN78 = On
     C*** END ADD *** 07/27/98 ****************************************
     C*
     C*  THE LICENSE # FIELD CANNOT BE BLANKS
     C*  REQUIRED FOR STATE PROCESSING   IF TX/FL AND P/H(FL) P(TX)
     C     XXAST         IFEQ      'FL'
     C     QQSYS         ANDEQ     'P'
     C*** BEG CHG *** 05/22/98 *************************
     C********** XXAST     OREQ 'FL'
     C     XXAST         OREQ      'TX'
     C*** BEG CHG *** 05/22/98 *************************
     C     QQSYS         ANDEQ     'H'
     C     XXAST         OREQ      'TX'
     C     QQSYS         ANDEQ     'P'
     C     SCLIC#        CABEQ     *BLANKS       STRT                     32
     C                   ENDIF
     C*
     C*  THE FL LICENSE MUST BEGIN WITH 2 LETTERS
     C*  IF IT IS A PHYS. BILL                             ADD 05/22/98
     C     XXAST         IFEQ      'FL'                                         02/21/91 ADD
     C     QQSYS         ANDEQ     'P'                                          ADD 05/22/98
     C     *IN81         ANDEQ     '1'                                          02/05/92 ADD
     C     SCTAPE        ANDEQ     'Y'                                          06/02/92 ADD
     C                   Z-ADD     1             X                 2 0
     C     SCLIC1        LOOKUP    LT(X)                                  60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '1'           *IN34
     C                   GOTO      STRT
     C                   END
     C*
     C                   Z-ADD     1             X
     C     SCLIC2        LOOKUP    LT(X)                                  60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '1'           *IN34
     C                   GOTO      STRT
     C                   END
     C                   END                                                    END EQ 'FL'
     C*  IMMEDIATE PROCESS FLAG MUST BE YES OR NO
     C*
     C     SCIPFL        IFNE      'Y'
     C     SCIPFL        ANDNE     'N'
     C                   MOVE      '1'           *IN36                          Y/N REQD
     C                   GOTO      STRT
     C                   END                                                    END NE Y/N
     C*
     C*   CARRIER RECEIVED DATE MUST BE VALID IF ENTERED
     C*******
     C*** BEG DEL *** 3/11/97 ***
     C*******    SCCRDT    IFNE *ZERO
     C*******              MOVE SCCRDT    SRDTE
     C*******              CALL 'SRDCK'
     C*******              PARM           SRDTE   6
     C*******              PARM ' '       SRERR   1
     C*******              PARM ' '       SRTERM  1
     C*******    SRERR     IFEQ '1'
     C*******              MOVE '1'       *IN37
     C*******              GOTO STRT
     C*******              END
     C*******              END
     C*** END DEL *** 3/11/97 ***
     C*** BEGIN ADD ***    02/21/96 **************************
     C*** BEG DEL *** 3/11/97 ***
     C********   XXAST     IFEQ 'FL'
     C********   QQSYS     ANDEQ'P'
     C********   SCCRDT    ANDEQ*ZEROS
     C********             MOVE '1'       *IN37
     C********             GOTO STRT
     C********             END
     C*** END DEL *** 3/11/97 ***
     C*** END ADD *****    02/21/96 **************************
     C*** ADD 7/21/95 *************************************************
     C*
     C*** CARIER RECEIVED DATE MUST BE ENTERED IF ADJ ST IS LA ADN SYS IS
     C*** = P OR TX IS STATE AND SYS IS P OR H
     C*******
     C*** END DEL *** 3/11/97 ***
     C*******    XXAST     IFEQ 'LA'
     C*******    QQSYS     ANDEQ'P'
     C*******    XXAST     OREQ 'TX'
     C*******    QQSYS     ANDEQ'P'
     C*******    XXAST     OREQ 'TX'
     C*******    QQSYS     ANDEQ'H'
     C*******    SCCRDT    CABEQ*ZEROS    STRT           40
     C*******              ENDIF
     C*** END DEL *** 3/11/97 ***
     C*
     C*
     C*** BILL SENT TO CARRIER DATE:
     C*** BEG DEL *** 3/11/97 ***
     C*******    SCDT9     IFNE *ZERO
     C*******              MOVE SCDT9     SRDTE
     C*******              CALL 'SRDCK'
     C*******              PARM           SRDTE   6
     C*******              PARM ' '       SRERR   1
     C*******              PARM ' '       SRTERM  1
     C*******    SRERR     IFEQ '1'
     C*******              MOVE '1'       *IN38
     C*******              GOTO STRT
     C*******              END
     C*******              END
     C*** END DEL *** 3/11/97 ***
     C*
     C*** BILL SENT TO CARRIER DATE MUST BE ENTERED IF ADJ ST IS TX AND SYS
     C*** = H
     C*
     C*** BEG DEL *** 3/11/97 ***
     C*******    XXAST     IFEQ 'TX'
     C*******    QQSYS     ANDEQ'H'
     C*******    SCDT9     CABEQ*ZEROS    STRT           39
     C*******              ENDIF
     C*** END DEL *** 3/11/97 ***
     C*
     C     *IN76         IFEQ      '1'
     C*
     C*** IS TRCD VALID?
     C                   MOVEL     *BLANKS       XXKEY
     C                   MOVEL     SCTRCD        XXKEY
     C     TBKEY         CHAIN     PPC01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN41
     C                   MOVEL     *BLANK        XXTRTP
     C                   GOTO      STRT
     C                   ENDIF
     C** * * IF PC TRTP IS NOT A PAYMENT TYPE... ERROR
     C                   MOVEL     PCRECD        XXRECD            2
     C                   MOVEL     PCTRTP        XXTRTP            1
     C*
     C     XXTRTP        CABNE     'P'           STRT                 4141
     C***
     C*** ** RESERVE TYPE MUST BE VALID
     C*
     C                   MOVEL     *BLANK        XXKEY
     C                   MOVEL     XXRECD        XXKEY
     C     TBKEY         CHAIN     PRT01                              60
     C*
     C     *IN60         CABEQ     *ON           STRT                     41
     C*
     C     RTTYPE        IFNE      'M'
     C     RTTYPE        ANDNE     'E'
     C                   MOVE      '1'           *IN41
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*
     C*** #      WCCVFL    IFEQ 'Y'
     C* CHAIN TO TRCD TABLE TO SEE IF CODE TYPE IS AN "E"
     C*** #      RTTYPE    CABNE'E'       STRT       4141
     C*** #                ELSE
     C* CHAIN TO TRCD TABLE TO SEE IF CODE TYPE IS AN "M"
     C*** #      RTTYPE    CABNE'M'       STRT       4141
     C*** #                ENDIF
     C***
     C*
     C*** IS PEND STATUS VALID?
     C                   MOVEL     *BLANKS       XXKEY
     C*** # TMP            MOVELSCSTS     XXKEY
     C*** #      TBKEY     CHAINPPS01                60
     C*** #      *IN60     IFEQ '1'
     C*** #                MOVE '1'       *IN43
     C*** #                GOTO STRT
     C*** #                ENDIF
     C*
     C*
     C*** IS RMK1  VALID?
     C     SCRMK1        IFNE      *BLANKS
     C                   MOVEL     *BLANKS       XXKEY
     C                   MOVEL     SCRMK1        XXKEY
     C     TBKEY         CHAIN     PRC01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN44
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C***
     C*
     C*** IS RMK2  VALID?
     C     SCRMK2        IFNE      *BLANKS
     C                   MOVEL     *BLANKS       XXKEY
     C                   MOVEL     SCRMK2        XXKEY
     C     TBKEY         CHAIN     PRC01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN45
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C***
     C*
     C*** IS RMK1  VALID?
     C     SCRMK3        IFNE      *BLANKS
     C                   MOVEL     *BLANKS       XXKEY
     C                   MOVEL     SCRMK3        XXKEY
     C     TBKEY         CHAIN     PRC01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN46
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C***
     C*
     C*
     C                   ENDIF
     C*
     C*
     C*** END ADD 7/21/95 *************************************************
     C*** BEGIN ADD *** 12/03/96 ***********************************************
     C**********                                           DEL 01/29/98
     C**THE*RECONSIDERATION FLAG MUST BE A 'Y' OR A 'N'    DEL 01/29/98
     C**********                                           DEL 01/29/98
     C********** SCRECO    IFNE 'Y'                        DEL 01/29/98
     C********** SCRECO    ANDNE'N'                        DEL 01/29/98
     C**********           MOVE '1'       *IN47            DEL 01/29/98
     C**********           GOTO STRT                       DEL 01/29/98
     C**********           END                             DEL 01/29/98
     C**********                                           DEL 01/29/98
     C**THE*REFUND FLAG MUST BE A 'Y' OR A 'N'             DEL 01/29/98
     C**********                                           DEL 01/29/98
     C********** SCRFND    IFNE 'Y'                        DEL 01/29/98
     C********** SCRFND    ANDNE'N'                        DEL 01/29/98
     C**********           MOVE '1'       *IN48            DEL 01/29/98
     C**********           GOTO STRT                       DEL 01/29/98
     C**********           END                             DEL 01/29/98
     C**********                                           DEL 01/29/98
     C**THE*REFUND FLAG IS ONLY ALLOWED IF RECON FLAG IS A 'Y' 01/29/98
     C**********                                           DEL 01/29/98
     C********** SCRFND    IFEQ 'Y'                        DEL 01/29/98
     C********** SCRECO    IFNE 'Y'                        DEL 01/29/98
     C**********           MOVE '1'       *IN49            DEL 01/29/98
     C**********           GOTO STRT                       DEL 01/29/98
     C**********           END                             DEL 01/29/98
     C**********           END                             DEL 01/29/98
     C*** END ADD *** 12/03/96 *************************************************
     C*
     C*  AMOUNT MAY NOT BE GREATER THAN REMAINING RESERVE
     C*
     C     SCTOTP        COMP      *ZERO                                2020     JUST EOB!!!
     C*
     C*  PROVIDER NAME CANNOT BE BLANK
     C     SCPNAM        IFEQ      *BLANK
     C                   MOVE      '1'           *IN35
     C                   GOTO      STRT
     C                   END
     C*
     C*  ADDRESS CANNOT BE BLANK
     C*
     C     *IN12         IFEQ      '0'
     C     SCADR1        IFEQ      *BLANK
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   END
     C*
     C*  THE CITY CANNOT BE BLANK
     C*
     C     SCCTY         IFEQ      *BLANK
     C                   MOVE      '1'           *IN29
     C                   GOTO      STRT
     C                   END
     C*
     C*  THE STATE MUST BE VALID
     C*
     C     SCST          LOOKUP    XX                                     60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '1'           *IN30
     C                   GOTO      STRT
     C                   END
     C*
     C*  THE ZIP CODE CANNOT BE ZERO
     C*
     C     SCZP1         IFLE      *ZERO
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C     *IN74         IFEQ      '1'
     C     SC#BLL        IFLT      1
     C     SC#BLL        ORGT      999
     C                   SETON                                        98
     C     *IN98         CABEQ     '1'           STRT
     C                   END                                                    END LT/GT 1/999
     C                   END                                                    END *IN74
     C*
     C*** EOMB CODES - VALIDATE ***
     C*
     C*** BEG ADD *** 3/11/97 ****
     C                   MOVE      *OFF          *IN77
     C*** END ADD *** 3/11/97 ****
     C     SCEOB1        IFNE      *BLANK
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCEOB1        XXKEY
     C     TBKEY         CHAIN     PEB01                              24
     C  N24EBSTAT        COMP      'I'                                    24
     C     *IN24         CABEQ     '1'           STRT
     C*** BEG ADD *** 3/11/97 ****
     C*** BEG DEL *** 11/07/97 ****
     C****                 ELSE                            SCEOB1=BLNK
     C****       PDWCCE    IFEQ 'N'                        REQ CODE
     C****       PDWCCE    OREQ ' '                        IF NON-BWC
     C****                 MOVE *ON       *IN77
     C****                 GOTO STRT
     C****                 ENDIF                           PDWCCE
     C*** BEG DEL *** 11/07/97 ****
     C*** END ADD *** 3/11/97 ****
     C                   END
     C*
     C*
     C     SCEOB2        IFNE      *BLANK
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCEOB2        XXKEY
     C     TBKEY         CHAIN     PEB01                              25
     C  N25EBSTAT        COMP      'I'                                    25
     C     *IN25         CABEQ     '1'           STRT
     C                   END
     C*
     C*
     C     SCEOB3        IFNE      *BLANK
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCEOB3        XXKEY
     C     TBKEY         CHAIN     PEB01                              26
     C  N26EBSTAT        COMP      'I'                                    26
     C     *IN26         CABEQ     '1'           STRT
     C                   END
     C*
     C*** WRITE EOMB PRINT WORK RECORD TO BE PROCESSED LATER.
     C*** BEGIN CHG *** 07/26/95 ***********************************************
     C     *IN57         IFEQ      '0'                                          ## 5/01/89
     C                   EXSR      GETSEQ
     C*
     C                   MOVE      SCCO#         EOCO#
     C                   MOVE      SCSUB#        EOSUB#
     C                   MOVE      SCFND         EOFND
     C                   Z-ADD     SCYR          EOFYR
     C                   MOVE      SCCASE        EOCASE
     C                   MOVE      SCCO#         Q1CO#
     C                   MOVE      SCSUB#        Q1SUB#
     C                   MOVE      SCFND         Q1FND
     C                   MOVE      SCYR          Q1FYR
     C                   MOVE      SCCASE        Q1CASE
     C                   MOVE      QQSYS         Q1SYS
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EOTRDT
     C                   MOVE      SCFEIN        EOFEIN
     C                   MOVE      SCMOD         EOMOD
     C     SCTOTP        ADD       SCNDIS        EOAMT                          ADD 03/24/93
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EOFRDT
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EOTODT
     C                   MOVE      SCTRCD        EOTRCD
     C                   MOVE      SCPNAM        EOPNAM
     C                   MOVE      SCADR1        EOADR1
     C                   MOVE      SCADR2        EOADR2
     C                   MOVE      SCCTY         EOCTY
     C                   MOVE      SCST          EOST
     C                   MOVEL     SCZP1         EOZIP
     C                   MOVE      SCZIP2        EOZIP
     C                   MOVE      ' '           EOSTAT
     C                   MOVE      *BLANKS       EOCHK#
     C                   Z-ADD     C1EOB#        EOEOB#
     C                   MOVE      C1EOB#        Q1EOB#
     C**********           MOVE C1EOB#    Q2EOB#           DEL 01/29/98
     C                   MOVE      SCEOB1        EOEOB1
     C                   MOVE      SCEOB2        EOEOB2
     C                   MOVE      SCEOB3        EOEOB3
     C                   MOVE      *BLANKS       EORMK1                         REMARK CODES
     C                   MOVE      *BLANKS       EORMK2
     C                   MOVE      *BLANKS       EORMK3
     C                   MOVE      *BLANKS       EORMDS                         REMARK DESC
     C* ADD 7/21/95 *******************************************
     C     *IN76         IFEQ      '1'
     C                   MOVEL     SCRMK1        EORMK1                         REMARK CODES
     C                   MOVEL     SCRMK2        EORMK2
     C                   MOVEL     SCRMK3        EORMK3
     C                   MOVEL     SCRMDS        EORMDS                         REMARK DESC
     C                   ENDIF
     C* END ADD 7/21/95 *******************************************
     C                   MOVE      *BLANKS       EOCKCN                         REMARK CODE4
     C                   MOVE      QQSYS         EOSYS                          FOR USE WITH
LJD  C                   MOVE      'H'           EOHRFL
     C                   Z-ADD     SC#BLL        EO#BLL                         NUMBER OF BILLS
     C                   Z-ADD     SCOTHR        EOOTHR                                     89
     C                   MOVE      SCTOTC        EOAMTC                         AMOUNT CHARGED
     C                   Z-ADD     *ZEROS        EOWCCK                         WORKER COMP CHK#
     C                   MOVE      *BLANKS       EOFLG1
     C                   MOVE      *BLANKS       EOFLG2
     C                   MOVE      *BLANKS       EOFLG3
     C                   MOVE      *BLANKS       EOFLG4
     C                   MOVE      *BLANKS       EOFLG5
     C                   MOVE      *BLANKS       EOFLG7
     C                   MOVE      *BLANKS       EOFLG8
     C                   MOVE      *BLANKS       EOFLG9
     C*
     C*** BEG CHG *** 3/11/97 ***
     C**********           MOVE SCEMPL    EOEMPL
     C********** SCDFC     MULT 10000.01  EODFC
     C                   MOVE      DCEMPL        EOEMPL
     C                   MOVE      DCDFC         EODFC
     C*** END CHG *** 3/11/97 ***
     C                   MOVE      SCTAPE        EOTAPE
     C                   MOVE      *BLANKS       EOCLSF
     C                   MOVE      *BLANKS       EOAUDT
     C                   MOVE      *BLANKS       EOBTFL
     C                   MOVE      *BLANKS       EOBLFL
     C                   MOVE      *BLANKS       EONLFL
     C                   MOVE      *BLANKS       EODIFL
     C                   Z-ADD     *ZEROS        EOINV#
     C                   Z-ADD     SCNDIS        EONDIS                         ADD 03/16/93
     C                   MOVE      *BLANKS       EOSB01
     C                   MOVE      *BLANKS       EOSB02
     C                   MOVE      *BLANKS       EOSB03
     C                   MOVE      *BLANKS       EOSB04
     C                   MOVE      *BLANKS       EOSB05
     C                   MOVE      *BLANKS       EOSB06
     C                   MOVE      *BLANKS       EOSB07
     C                   MOVE      *BLANKS       EOSB08
     C                   MOVE      *BLANKS       EOSB09
     C                   MOVE      *BLANKS       EOSB10
     C                   Z-ADD     *ZEROS        EOBAMT
     C*** BEG DEL *** 03/11/97 ***
     C*** BEG ADD *** 02/03/98 ***
     C                   MOVE      SCLIC#        EOFLIC
     C*** END ADD *** 02/03/98 ***
     C*** END DEL *** 03/11/97 ***
     C                   MOVE      *BLANKS       EODIA1
     C                   MOVE      *BLANKS       EODIA2
     C                   MOVE      *BLANKS       EODIA3
     C                   MOVE      *BLANKS       EODIA4
     C*  GET DIAGNOSIS CODES FROM DIAG CODE WORK FILE
     C     DCKEY         CHAIN     PDC01                              92
     C     *IN92         IFEQ      '0'
     C     DCDIA1        IFNE      *BLANKS
     C                   MOVE      DCDIA1        EODIA1
     C                   END
     C     DCDIA2        IFNE      *BLANKS
     C                   MOVE      DCDIA2        EODIA2
     C                   END
     C     DCDIA3        IFNE      *BLANKS
     C                   MOVE      DCDIA3        EODIA3
     C                   END
     C     DCDIA4        IFNE      *BLANKS
     C                   MOVE      DCDIA4        EODIA4
     C                   END
     C                   END                                                    END 0 *IN92
     C*
     C*** GET ADJUDICATION STATE
     C     DCAST         IFNE      *BLANKS
     C                   MOVE      DCAST         EOAST
     C                   ELSE
     C                   Z-ADD     SCFEIN        XXFEIN
     C                   Z-ADD     SCMOD         XXMOD
     C     P1KEY         CHAIN     PP101                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      P1ST          EOAST
     C                   ELSE
     C                   MOVE      'FL'          EOAST
     C                   END
     C                   END
     C*
     C                   MOVE      DCZIPA        EOZIPA                         ADD 02/24/93
     C                   MOVE      DCCERT        EOCERT                         ADD 11/11/92
     C                   Z-ADD     DCEXT         EOEXT                          ADD 11/11/92
     C                   MOVE      DCBLTY        EOFLG6
     C*
     C*  NOW DELETE DIAG CODE WORK FILE RECORD
     C     DCKEY         DELETE    PDC01                              75
LJD  C                   MOVE      *BLANKS       EONLFL                         ## NO LST TM
LJD  C                   MOVE      SCTAPE        EOTAPE                         8/29/89 ADD
     C*
     C                   MOVE      Q1USER        EOUSER
     C                   MOVE      Q1PGM         EOPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C                   Z-ADD     DSTIME        EOTIME
     C                   MOVEL     Q1USER        EOIUSR
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EOIDT
     C*  EOB RECORD WILL NOW BE WRITTEN WITH DIAGNOSIS CODES IN IT,
     C                   WRITE     PEO01
     C                   ELSE
     C                   MOVE      SCEOB1        EOEOB1
     C                   MOVE      SCEOB2        EOEOB2
     C                   MOVE      SCEOB3        EOEOB3
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EOFRDT
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EOTODT
     C                   MOVE      SCTOTC        EOAMTC
     C                   Z-ADD     SCTOTP        EOAMT
     C                   UPDATE    PEO03
     C                   END
     C*** END CHG *** 07/26/95 *************************************************
     C*** BEGIN ADD *** 12/15/97 *************************************
     C*
     C*** DELETE THE BILL ENTRY MESSAGE FROM FDMGP
     C*
     C                   MOVE      SCCO#         QZCO#             3
     C                   MOVE      CASUB#        QZSUB             5
     C                   MOVE      CAFND         QZFND             3
     C                   MOVE      CAFYR         QZFYR             3
     C                   MOVE      CACASE        QZCASE            7
     C                   MOVE      XXDCS#        QZDCS#            5
     C                   MOVE      *BLANKS       QZEOB#            9
     C                   CALL      'F0074R'
     C                   PARM                    QZCO#
     C                   PARM                    QZSUB
     C                   PARM                    QZFND
     C                   PARM                    QZFYR
     C                   PARM                    QZCASE
     C                   PARM                    QZDCS#
     C                   PARM                    QZEOB#
     C                   PARM      'S'           QZDEL1            1
     C                   PARM      'P'           QZDEL2            1
     C                   PARM      ' '           QZDEL3            1
     C                   PARM      ' '           QZDEL4            1
     C                   PARM      ' '           QZDEL5            1
     C*** END ADD *** 12/15/97 ***************************************
     C*
     C*  WRITE RECORD TO EOB DAILY TRANSACTION FILE
     C*
     C                   MOVEL     XXSS#         EOSS#
     C                   Z-ADD     XXACDT        EOACDT
     C                   MOVEL     'A'           EOTRTP                            TRNS TYPE
     C                   WRITE     PEO02
     C*** ADD 7/21/95 ************************
     C                   MOVEL     SCSTS         X1STS
     C*** END ADD 7/21/95 ****************************
     C*
     C*** BEGIN CHG *** 07/26/95 ***********************************************
     C     *IN57         IFEQ      '0'
     C                   MOVE      SCCO#         E1CO#
     C                   MOVE      SCSUB#        E1SUB#
     C                   MOVE      SCFND         E1FND
     C                   Z-ADD     SCYR          E1FYR
     C                   MOVE      SCCASE        E1CASE
     C                   Z-ADD     C1EOB#        E1EOB#
     C*** DEL 7/21/95      MOVE *BLANKS   E1ACCT
     C*** BEG CHG *** 3/11/97 ***
     C***************      MOVELSCACCT    E1ACCT
     C                   MOVEL     DCACCT        E1ACCT
     C*** END CHG *** 3/11/97 ***
     C                   Z-ADD     *ZEROS        E1ENDT
     C                   Z-ADD     *ZEROS        E1BLDT
     C                   Z-ADD     *ZEROS        E1ORDT
     C                   Z-ADD     *ZEROS        E1CRDT
     C*** BEG DEL *** 3/11/97 ***
     C*******    SCDT9     MULT 10000.01  E1DT9
     C*******    SCCRDT    MULT 10000.01  E1CRDT
     C*** END DEL *** 3/11/97 ***
     C                   Z-ADD     *ZEROS        E1TRPH
     C                   Z-ADD     *ZEROS        E1TRPM
     C                   MOVE      *BLANKS       E1FLG1
     C*** BEGIN ADD *** 12/03/96 ***********************************************
     C**********           MOVE *BLANKS   E1FLG1           DEL 01/29/98
     C********** SCRFND    IFEQ 'Y'                        DEL 01/29/98
     C********** SCRECO    ANDEQ'Y'                        DEL 01/29/98
     C**********           MOVE 'B'       E1FLG1           DEL 01/29/98
     C**********           ELSE                            DEL 01/29/98EQ 'Y'
     C********** SCRECO    IFEQ 'Y'                        DEL 01/29/98
     C********** SCRFND    ANDEQ'N'                        DEL 01/29/98
     C**********           MOVE 'R'       E1FLG1           DEL 01/29/98
     C**********           END                             DEL 01/29/98Q 'Y'
     C**********           END                             DEL 01/29/98Q 'Y'
     C*** END ADD *** 12/03/96 *************************************************
     C                   MOVE      *BLANKS       E1FLG2
     C                   MOVE      *BLANKS       E1FLG3
     C                   MOVE      *BLANKS       E1FLG4
     C                   MOVE      *BLANKS       E1FLG5
     C                   Z-ADD     *ZEROS        E1DT1
     C                   Z-ADD     *ZEROS        E1DT2
     C                   Z-ADD     *ZEROS        E1DT3
     C                   Z-ADD     *ZEROS        E1DT4
     C                   Z-ADD     *ZEROS        E1DT5
     C                   Z-ADD     *ZEROS        E1AMT1
     C                   Z-ADD     *ZEROS        E1AMT2
     C                   Z-ADD     *ZEROS        E1AMT3
     C                   Z-ADD     *ZEROS        E1AMT4
     C                   Z-ADD     *ZEROS        E1AMT5
     C                   MOVE      *BLANKS       E110A1
     C                   MOVE      *BLANKS       E110A2
     C                   MOVE      *BLANKS       E110A3
     C                   MOVE      *BLANKS       E140A1
     C                   MOVE      *BLANKS       E120A2
     C*** ADD 7/25/95 *****************************
     C                   MOVEL     DC7A01        E17A01
     C                   MOVEL     DC7A02        E17A02
     C                   MOVEL     DC7A03        E17A03
     C                   MOVEL     DC7A04        E17A04
     C                   MOVEL     DCDT06        E1DT6
     C                   MOVEL     DCDT07        E1DT7
     C                   MOVEL     DCDT08        E1DT8
     C                   MOVEL     DCBLTY        E1FLG3
     C*
     C                   Z-ADD     1             X
     C     5             SUBST     DC10A1:X      X1NWCD                 65
     C                   Z-ADD     6             X
     C     3             SUBST     DC10A1:X      X1TYPE                 65
     C                   Z-ADD     9             X
     C     2             SUBST     DC10A1:X      X1DISH                 65
     C                   Z-ADD     7             X
     C     3             SUBST     DC10A2:X      X1TOB                  65
     C                   Z-ADD     10            X
     C*** BEGIN CHG *** 10/01/95 ***********************************************
     C********** 1         SUBSTDC10A1:X  X1PCM        65
     C     1             SUBST     DC10A2:X      X1PCM                  65
     C*** END CHG *** 10/01/95 *************************************************
     C*
     C                   Z-ADD     1             X
     C     1             SUBST     DC10A2:X      X2GRTC                 65
     C                   Z-ADD     2             X
     C     5             SUBST     DC10A2:X      X2GRTP                 65
     C                   Z-ADD     1             X
     C     3             SUBST     DC10A3:X      X2HTOS                 65
     C*
     C                   MOVEL     X120A2        E120A2                         ADD 07/27/95
     C                   MOVE      X120A1        E120A1                         ADD 07/27/95
     C*** END ADD 7/21/95 ****************************
     C                   MOVE      Q1USER        E1USER
     C                   MOVE      Q1PGM         E1PGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         E1DT
     C                   Z-ADD     DSTIME        E1TIME
     C                   MOVEL     Q1USER        E1IUSR
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         E1IDT
     C*** BEG ADD *** 03/11/97 ***
     C                   EXSR      E1INIT
     C*** END ADD *** 03/11/97 ***
     C                   WRITE     PE101
     C*** BEG ADD *** 03/11/97 ***
     C*** BEG ADD *** 11/07/97 ***
     C*** BEG DEL *** 11/30/97 ***
     C*** EDI INSTALLED ?
     C********             CALL 'SA003R'
     C********             PARM *BLANK    QQRTCD  1
     C********             PARM *BLANK    QQEDI   1
     C********
     C********   QQEDI     IFEQ 'Y'
     C********
     C****END*ADD *** 11/07/97 ***
     C********             MOVE EOCO#     QFCO#
     C********             MOVE EOSUB#    QFSUB#
     C********             MOVE EOFND     QFFND
     C********             MOVE EOFYR     QFFYR
     C********             MOVE EOCASE    QFCASE
     C********             MOVE EOEOB#    QFEOB#
     C********             CALL 'F0070R6'
     C********             PARM           QFCO#   3        COMPANY
     C********             PARM           QFSUB#  5        SUBSCRIBER
     C********             PARM           QFFND   3        FUND ID
     C********             PARM           QFFYR   3        FUND YR
     C********             PARM           QFCASE  7        CASE/CLAIM
     C********             PARM           QFEOB#  9        EOB #
     C********             PARM 'A'       QFACTF  1        ACTION FLAG
     C********             PARM *BLANKS   QFMGID  7        MSG ID
     C********             PARM *BLANK    QFRTCD  1
      ********
     C********             ENDIF                           ADD 11/07/97
     C*** END DEL *** 11/30/97 ***
     C*** BEG ADD *** 03/11/97 ***
      *
     C                   ELSE
     C                   Z-ADD     SCPRVP        E1AMT3
     C                   MOVE      X120A1        E120A1                         ADD 05/25/94
     C                   UPDATE    PE102
     C                   END
     C*** BEGIN  CHG *** 07/26/95 **********************************************
     C     *IN57         IFEQ      '0'
     C*** END CHG *** 07/26/95 *************************************************
     C********** *IN74     IFEQ '1'                        DEL 06/05/98
     C*
     C*  IF "H" SYSTEM ID, OUTPUT HBA RECORD
     C*
     C     *IN90         IFEQ      '1'
     C********             GOTO HBATAG                     DEL 06/05/98
     C********             ELSE                            DEL 06/05/98
     C********             GOTO NETTAG                     DEL 06/05/98
     C********             END                             DEL 06/05/98
     C*
     C********   HBATAG    TAG                             DEL 06/05/98
     C*** WRITE HBA TRANS FILE RECORD
     C*
     C                   MOVE      SCCO#         HBCO#                          CO #
     C                   MOVE      SCSUB#        HBSUB#                         SUB #
     C                   MOVE      SCFND         HBFND                          FUND #
     C                   Z-ADD     SCYR          HBFYR                          FUND YEAR
     C                   MOVE      SCCASE        HBCASE                         CASE #
     C                   MOVE      SCSS#         HBSS#                          SS #
     C                   Z-ADD     SCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HBACDT
     C                   MOVE      C1EOB#        HBEOB#                         EOB #
     C                   MOVE      '1'           HBLVL                          HBA LEVEL
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HBTRDT
     C                   MOVE      SCFEIN        HBFEIN                         PAYEE FED #
     C                   MOVE      SCMOD         HBMOD                          MODIFIER
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HBFRDT
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HBTODT
     C                   MOVE      SCTOTP        HBTALW                         REC ALLOW
     C                   MOVE      SCTOTC        HBTCHG                         TOTAL CHRGD
     C*
     C*** CHANGE NEXT 2 LINES *** CONNIE/A9C *** 8/25/89 *** WAS XXBAMT
     C*
     C                   MOVE      *ZEROS        HB1CAM                         CALC BILL AMT
     C                   MOVE      *ZEROS        HB1AAM                         ACTUAL BILL AMT
     C*
     C                   MOVE      *ZEROS        HB1PM1                         % TO CALC AMT
     C                   MOVE      *BLANKS       HBTRCD
     C                   MOVE      *ZEROS        HBHBAP
     C                   MOVE      *ZEROS        HBARDA
     C                   MOVE      *BLANKS       HBICD1
     C                   MOVE      *BLANKS       HBICD2
     C                   MOVE      *BLANKS       HBICD3
     C                   MOVE      *BLANKS       HBICD4
     C                   MOVE      *BLANKS       HBICD5
     C                   MOVE      SCPNAM        HBPNAM                         NAME
     C                   MOVE      *BLANKS       HBPCTL
     C                   MOVE      *BLANKS       HBATYP
     C                   MOVE      SCADR1        HBADR1                         ADDRESS 1
     C                   MOVE      SCADR2        HBADR2                         ADDRESS 2
     C                   MOVE      SCCTY         HBCTY                          CITY
     C                   MOVE      SCST          HBST                           STATE
     C                   MOVEL     SCZP1         HBZIP                          ZIP
     C                   MOVE      SCZIP2        HBZIP                          ZIP
     C                   MOVE      SCEOB1        HBEOB1                         EOB CODE 1
     C                   MOVE      SCEOB2        HBEOB2                         EOB CODE 2
     C                   MOVE      SCEOB3        HBEOB3                         EOB CODE 3
     C                   MOVE      ' '           HBSTAT                         HOLD STATUS ##
     C                   MOVE      *BLANKS       HBFLG1
     C                   MOVE      'H'           HBFLG2
     C                   MOVE      *BLANKS       HBFLG3
     C                   MOVE      *BLANKS       HBFLG4
     C                   MOVE      *BLANKS       HBFLG5
     C                   MOVE      *ZEROS        HBINV#
     C                   MOVE      *BLANKS       HBHRFL
     C                   MOVE      *BLANKS       HBBLFL
     C                   MOVE      Q1USER        HBUSER                         USER
     C                   MOVE      Q1USER        HBIUSR
     C                   MOVE      Q1PGM         HBPGM                          PROGRAM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HBDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HBIDT
     C                   Z-ADD     DSTIME        HBTIME                         TIME
     C                   WRITE     PHB01
     C*
     C                   END                                                    ADD 06/05/98
     C*
     C*** BEGIN CHG *** 09/21/99 **************************************
     C********   ##NTWK    IFEQ '1'                        ADD 06/05/98
     C     ##NTWK        IFEQ      'Y'
     C*** END CHG *** 09/21/99 ****************************************
     C*
     C********** NETTAG    TAG                             DEL 06/05/98
     C*
     C*  GET THE EFFECTIVE AND TERMINATION DATE FOR PROVIDER
     C** IF NO ASSOCIATED RECORD, DON'T CREATE A NETWORK DISC RECORD
     C*
     C*** BEG CHG *** 02/13/98 *****************************
     C* HOSPITAL PROVIDER
     C     PDTYPE        IFEQ      '4'
     C     HSKEY         SETGT     PS402
     C     NXTHOS        TAG
     C                   READP     PS402                                  60
     C     *IN60         IFEQ      '0'
     C     S4NWCD        ANDEQ     SCNWCD
     C*** BEGIN ADD *** 09/21/99 **************************************
     C     S4ENDT        IFEQ      0
     C                   Z-ADD     99999999      S4ENDT
     C                   ENDIF                                                  ENDT = 0
     C*** END ADD *** 09/21/99 ****************************************
     C     S4ENDT        IFGT      *ZERO
     C     CAFDAT        IFGT      S4ENDT
     C                   GOTO      NXTHOS
     C                   END
     C                   Z-ADD     S4BGDT        XXFROM
     C                   Z-ADD     S4ENDT        XXTO
     C                   ENDIF
     C                   ELSE                                                   *IN60 ON
     C                   GOTO      TERM
     C                   END                                                    *IN60 OFF
     C                   END                                                    PDTYPE=4
     C*
     C* PHYSICIAN PROVIDER
     C     PDTYPE        IFEQ      '1'
     C     PHKEY         SETGT     PS401
     C     NXTPHY        TAG
     C                   READP     PS401                                  60
     C     *IN60         IFEQ      '0'
     C     S4NWCD        ANDEQ     SCNWCD
     C*** BEGIN ADD *** 09/21/99 **************************************
     C     S4ENDT        IFEQ      0
     C                   Z-ADD     99999999      S4ENDT
     C                   ENDIF                                                  ENDT = 0
     C*** END ADD *** 09/21/99 ****************************************
     C     S4ENDT        IFGT      *ZERO
     C     CAFDAT        IFGT      S4ENDT
     C                   GOTO      NXTPHY
     C                   END
     C                   Z-ADD     S4BGDT        XXFROM
     C                   Z-ADD     S4ENDT        XXTO
     C                   ENDIF
     C                   ELSE                                                   *IN60 ON
     C                   GOTO      TERM
     C                   END                                                    *IN60 OFF
     C                   END                                                    PDTYPE=1
     C*
     C**HOSPITAL PROVIDER
     C********   PDTYPE    IFEQ '4'
     C********   HSKEY     SETGTPHS01
     C********   NXTHOS    TAG
     C********             READPPHS01                    60
     C********   *IN60     IFEQ '0'
     C********   HSNWCD    ANDEQSCNWCD
     C********   CAFDAT    IFGT HSTMDT
     C********             GOTO NXTHOS
     C********             END
     C********             Z-ADDHSCEDT    XXFROM
     C********             Z-ADDHSTMDT    XXTO
     C********             ELSE                            *IN60 ON
     C********             GOTO TERM
     C********             END                             *IN60 OFF
     C********             END                             PDTYPE=4
     C********
     C**PHYSICIAN PROVIDER
     C********   PDTYPE    IFEQ '1'
     C********   PHKEY     SETGTPPH01
     C********   NXTPHY    TAG
     C********             READPPPH01                    60
     C********   *IN60     IFEQ '0'
     C********   PHNWCD    ANDEQSCNWCD
     C********   CAFDAT    IFGT PHNXDT
     C********             GOTO NXTPHY
     C********             END
     C********             Z-ADDPHNEDT    XXFROM
     C********             Z-ADDPHNXDT    XXTO
     C********             ELSE                            *IN60 ON
     C********             GOTO TERM
     C********             END                             *IN60 OFF
     C********             END                             PDTYPE=1
     C*** BEG CHG *** 02/13/98 *****************************
     C*
     C*  COMPARE DATE RANGE -- IF FROM/TO SERVICE DATES ARE BOTH BEFORE
     C*    OR BOTH AFTER THE PROVIDER CONTRACT DATES, NO NETWORK DISC RCD
     C*    IS REQUIRED. BUT, IF ANY SERVICE DATES ARE IN RANGE, A BILL
     C*    WILL BE PRODUCED..
     C*
     C     EOFRDT        IFLT      XXFROM                                       BEFORE THE
     C     EOTODT        ANDLT     XXFROM                                       CONTRACT
     C*
     C                   GOTO      TERM
     C                   END                                                    EOFRDT LT
     C*
     C     EOFRDT        IFGT      XXTO                                         AFTER THE
     C     EOTODT        ANDGT     XXTO                                         CONTRACT
     C*
     C                   GOTO      TERM
     C                   END                                                    EOFRDT GT
     C*
     C*  IF CAPRTY NE *BLANK THEN WRITE NETWORK TRANSACTION RECORD
     C*  ELSE BYPASS IT (BECAUSE EOB IS BYPASSED - CMD 22)
     C*
     C     CAPRTY        IFNE      *BLANK
     C*
     C     PDPARN        IFEQ      'P'
     C*
     C*** NEXT CHECK IF FUND IS A NETWORK SUBSCRIBER
     C*
     C                   Z-ADD     1             W                 5 0
     C     'N'           LOOKUP    FSY(W)                                 50
     C     *IN50         IFEQ      '1'
     C*
     C*** WRITE NETWORK DISCOUNT RECORD IF PARTIC. PROVIDER & FUND
     C*
     C                   MOVE      SCCO#         TRCO#
     C                   MOVE      SCSUB#        TRSUB#
     C                   MOVE      SCFND         TRFND
     C                   Z-ADD     SCYR          TRFYR
     C                   MOVE      SCCASE        TRCASE
     C                   MOVE      SCFEIN        TRFEIN
     C                   MOVE      SCMOD         TRMOD
     C                   Z-ADD     C1EOB#        TREOB#
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TRFRDT
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TRTODT
     C                   MOVE      SCTRCD        TRTRCD
     C                   MOVE      *ZEROS        TRINV#
     C                   MOVE      'N'           TRSYS                          9/14/89
LJD  C                   MOVE      'H'           TRHRFL
LJD  C                   MOVE      *BLANKS       TRNLFL                         ## NO LST TM
     C*
     C                   MOVE      *ZEROS        TRBAMT
     C                   MOVE      ' '           TRSTAT
     C                   MOVE      ' '           TRBLFL                         NOT USED AT THIS TIM
     C                   MOVE      ' '           TRFLG1
     C                   MOVE      ' '           TRFLG3
     C                   MOVE      ' '           TRFLG4
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TRTRDT
     C                   MOVE      Q1USER        TRUSER
     C                   MOVE      Q1USER        TRIUSR
     C                   MOVE      Q1PGM         TRPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TRDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TRIDT
     C                   Z-ADD     DSTIME        TRTIME
     C                   WRITE     PTR01
     C*
     C                   END                                                       *IN50=1
     C*
     C                   END                                                    PDPARN=N
     C*
     C                   END                                                    CAPRTY=' '
     C*
     C                   END                                                    ##NTWK
     C*
     C                   END                                                    END *IN57
     C*** END CHG ***  07/26/95 ************************************************
     C*
     C                   Z-ADD     SCYR          XXFYR
     C                   MOVE      SCCASE        XXCASE
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C*** BEGIN CHG *** 07/26/95 ***********************************************
     C     *IN57         IFEQ      '0'
     C                   ADD       SCTOTP        CMAR01
     C                   ELSE
     C                   ADD       SCPRVP        CMAR01
     C                   END
     C*** END CHG *** 07/26/95 *************************************************
     C                   UPDATE    PCM01
     C                   END                                                    END 0 *IN60
     C*** BEGIN ADD *** 07/26/95 ***********************************************
     C     *IN57         IFEQ      '1'
     C                   MOVE      SCCO#         Q1CO#
     C                   MOVE      SCSUB#        Q1SUB#
     C                   MOVE      SCFND         Q1FND
     C                   MOVE      SCYR          Q1FYR
     C                   MOVE      SCCASE        Q1CASE
     C                   MOVE      EOEOB#        Q1EOB#
     C                   MOVE      *BLANKS       Q1QUE            10
     C                   MOVE      *BLANKS       Q1MODE            1
     C                   CALL      'N7128C'
     C                   PARM                    Q1CO#
     C                   PARM                    Q1SUB#
     C                   PARM                    Q1FND
     C                   PARM                    Q1FYR
     C                   PARM                    Q1CASE
     C                   PARM                    Q1EOB#
     C                   PARM                    Q1QUE
     C                   PARM                    Q1MODE
     C                   END
     C*** END ADD *** 07/26/95 *************************************************
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C     *INKW         IFEQ      '0'
     C     *INKC         ANDEQ     '0'
     C     *IN72         IFEQ      '1'
     C*** BEG DEL *** 10/07/99 ****************************************
     C**********           GOTO TMMT
     C*** END DEL *** 10/07/99 ****************************************
     C                   ELSE
     C     XXSKIP        IFEQ      '0'
     C     SCIPFL        ANDEQ     'Y'
     C********** SCRECO    ANDEQ'N'                        DEL 01/29/98
     C     *IN74         IFEQ      '1'
     C                   MOVE      'Y'           Q1NTFL
     C                   ELSE
     C                   MOVE      'N'           Q1NTFL
     C                   END
     C                   CALL      'F6000CI'
     C                   PARM                    Q1CO#             3
     C                   PARM                    Q1SUB#            5
     C                   PARM                    Q1FND             3
     C                   PARM                    Q1FYR             3
     C                   PARM                    Q1CASE            7
     C                   PARM                    Q1EOB#            9
     C                   PARM                    Q1NTFL            1
     C                   PARM      'N'           Q1CERT            1
     C                   PARM      'N'           Q1TM              1
     C                   PARM      'Y'           Q1CHK             1
     C                   PARM                    Q1SYS             1
     C*** BEGIN ADD *** 12/03/96 **************************************
     C**********           ELSE                            ELSE0XXSKIP8
     C**********                                           DEL 01/29/98
     C********** SCRECO    IFEQ 'Y'                        DEL 01/29/98
     C**********                                           DEL 01/29/98
     C********** SCRFND    IFEQ 'Y'                        DEL 01/29/98
     C**********                                           DEL 01/29/98
     C**CALL*RECON BREAKDOWN SCREEN                        DEL 01/29/98
     C**********                                           DEL 01/29/98
     C**********           MOVE *BLANKS   Q2CALL  1        DEL 01/29/98
     C**********           MOVE *BLANKS   Q2RTCD  1        DEL 01/29/98
     C**********           MOVE UPCO#     Q2CO#   3        DEL 01/29/98
     C**********           MOVE SCSUB#    Q2SUB#  5        DEL 01/29/98
     C**********           MOVE SCFND     Q2FND   3        DEL 01/29/98
     C**********           MOVE QQFYR     Q2FYR   3        DEL 01/29/98
     C**********           MOVE SCCASE    Q2CASE  7        DEL 01/29/98
     C**********           MOVE SCTOTP    Q2AMT   9        DEL 01/29/98
     C**********           MOVE SCSS#     Q2SS#   9        DEL 01/29/98
     C**********           MOVE SCACDT    Q2ACDT  8        DEL 01/29/98
     C**********           CALL 'F0087R'                   DEL 01/29/98
     C**********           PARM           QQPRDS           DEL 01/29/98
     C**********           PARM           Q2CALL           DEL 01/29/98
     C**********           PARM           Q2CO#            DEL 01/29/98
     C**********           PARM           Q2SUB#           DEL 01/29/98
     C**********           PARM           Q2FND            DEL 01/29/98
     C**********           PARM           Q2FYR            DEL 01/29/98
     C**********           PARM           Q2CASE           DEL 01/29/98
     C**********           PARM           Q2AMT            DEL 01/29/98
     C**********           PARM           Q2SS#            DEL 01/29/98
     C**********           PARM           Q2ACDT           DEL 01/29/98
     C**********           PARM           Q2EOB#  9        DEL 01/29/98
     C**********           PARM           Q2RTCD           DEL 01/29/98
     C**********           END                             END SCRFND9= Y
     C**********                                           DEL 01/29/98
     C**CALL*INTERACTIVE BATCH TO PROCESS RECON EOMB       DEL 01/29/98
     C**PASS*'N' IN Q1NTFL SO NO BILLING AMOUNT WILL BE CALCULATED29/98
     C**********                                           DEL 01/29/98
     C**********           CALL 'F6000CI'                  DEL 01/29/98
     C**********           PARM           Q1CO#   3        DEL 01/29/98
     C**********           PARM           Q1SUB#  5        DEL 01/29/98
     C**********           PARM           Q1FND   3        DEL 01/29/98
     C**********           PARM           Q1FYR   3        DEL 01/29/98
     C**********           PARM           Q1CASE  7        DEL 01/29/98
     C**********           PARM           Q1EOB#  9        DEL 01/29/98
     C**********           PARM 'N'       Q1NTFL  1        DEL 01/29/98
     C**********           PARM 'N'       Q1CERT  1        DEL 01/29/98
     C**********           PARM 'N'       Q1TM    1        DEL 01/29/98
     C**********           PARM 'Y'       Q1CHK   1        DEL 01/29/98
     C**********           PARM           Q1SYS   1        DEL 01/29/98
     C**********           END                             END SCRECO9= Y
     C*** END   ADD *** 12/03/96 **************************************
     C                   END
     C                   END
     C                   END
     C*
     C                   CLOSE     F0086F
     C*
     C*** CALL WC PAYMENT REQUEST PROGRAM ****************************
     C*** BEGIN CHG *** 07/27/95 ***********************************************
     C********** SCTOTP    IFNE 0
     C*** BEGIN CHG *** 12/03/96 **************************************
     C********** SCPAY     IFGT 0
     C*** BEGIN DEL *** 03/31/98 **************************************
     C********** SCTOTP    IFNE 0
     C**********           MOVE 'I'       QZCALL
     C**********           ELSE
     C**********           MOVE '2'       QZCALL
     C**********           END
     C*** END   DEL *** 03/31/98 **************************************
     C********** *INKW     ANDNE'1'
     C     *INKW         IFNE      '1'
     C*** END CHG *** 01/23/95 *************************************************
     C*** END CHG *** 07/27/95 *************************************************
     C     *INKC         ANDNE     '1'
     C     *IN71         ANDEQ     '1'
     C*** BEG ADD *** 02/25/97 ****
     C     F1FL04        ANDEQ     'Y'
     C*** END ADD *** 02/25/97 ****
     C*  Call F0006R1 to temporarily change UPFND to FNWCF# so that W0080R works
     C                   MOVE      SCCO#         QFCO#
     C                   MOVE      HLWCF#        QFFND
     C                   CALL      'F0006R1'
     C                   PARM                    QFCO#
     C                   PARM                    QFFND
     C                   PARM                    QFRTCD
     C*** CALL WC PAYMENT REQUEST PROGRAM ****************************
     C*** FILL QZ FIELDS
     C*** BEGIN ADD *** 03/31/98 **************************************
     C                   MOVE      'I'           QZCALL                         *DEL 12/03/96
     C*** END   ADD *** 03/31/98 **************************************
     C                   MOVE      SCFND         QZFND                          ADD 11/07/97
     C                   MOVE      SCPNAM        QZPNAM                         ADD 11/07/97
     C                   MOVE      SCTRCD        QZTRCD                         ADD 11/07/97
     C                   MOVE      SCYR          QZYR
     C                   MOVE      SCCASE        QZCASE
     C                   MOVE      SCFEIN        QZFEIN
     C                   MOVE      SCMOD         QZMOD
     C                   MOVE      SCFRDT        QZFRDT
     C                   MOVE      SCTODT        QZTODT
     C*** BEGIN ADD *** 07/26/95 ***********************************************
     C**********           MOVE SCTOTP    QZAMT
     C*** BEGIN CHG *** 12/03/96 ***********************************************
     C**********           MOVE SCPAY     QZAMT
     C                   MOVE      SCTOTP        QZAMT
     C*** END   CHG *** 12/03/96 ***********************************************
     C*** END ADD *** 07/26/95 *************************************************
     C                   MOVE      EOEOB#        QZEOB#
     C*** CALL REQUEST PAYMENT PROGRAM IN WC SYSTEM
     C                   MOVE      *BLANK        QXPRDS
     C                   MOVEL     PTL(2)        QXPRDS
     C*** EOMB CHECK MISMATCH TRACKING COPY STMT
     C/COPY SCPYCMF86
     C*** BEGIN ADD *** 02/12/99 **************************************
     C                   MOVE      E1CRDT        QZRCVD            7
     C*** END ADD *** 02/12/99 ****************************************
     C                   CALL      'W0080R'
     C                   PARM                    QXPRDS           40
     C                   PARM                    QZCALL            1
     C                   PARM                    QZFND             3            ADD 11/07/97
     C                   PARM                    QZYR              3            CHG 11/07/97
     C                   PARM                    QZCASE            7
     C                   PARM                    QZFEIN            9
     C                   PARM                    QZMOD             3
     C                   PARM                    QZFRDT            8
     C                   PARM                    QZTODT            8
     C                   PARM                    QZAMT             9
     C                   PARM                    QZEOB#            9
     C                   PARM                    QZPNAM           40            ADD 11/07/97
     C                   PARM                    QZTRCD            2            ADD 11/07/97
     C*** BEGIN ADD *** 02/12/99 **************************************
     C                   PARM                    @@KC              1
     C                   PARM                    QZRCVD            7
     C*** END ADD *** 02/12/99 ****************************************
     C*  Call F0006R1 to change back UPFND to Group
     C                   MOVE      SCCO#         QFCO#
     C                   MOVE      HLFND         QFFND
     C                   CALL      'F0006R1'
     C                   PARM                    QFCO#
     C                   PARM                    QFFND
     C                   PARM                    QFRTCD
     C                   END
     C*** END CALL TO WC PAYMENT REQUEST PROGRAM ****************************
     C*
     C*********************
     C*  END OF PROGRAM   *
     C*********************
     C                   RETURN
     C                   MOVE      '1'           *INLR
     C*******************************************************************
     C*  EDIT SUBROUTINE - EDIT ENTRY VALUES:                           *
     C*******************************************************************
     C     EDIT          BEGSR
     C                   MOVE      *BLANKS       ERR               1
     C*
     C*  THE CLAIM MASTER RECORD MUST BE ON FILE
     C*
     C                   Z-ADD     SCYR          XXFYR
     C                   MOVE      SCCASE        XXCASE
     C     CMKEY         CHAIN     PCM01                              20
     C     *IN20         IFEQ      '1'
     C                   MOVE      '1'           ERR
     C                   GOTO      XEDIT
     C                   ELSE
     C                   EXCEPT    ECM01
     C                   END
     C*
     C     XEDIT         ENDSR
     C*** BEG ADD *** 03/11/97 ***
     C*******************************************************************
     C*  E1INIT : NEW FILE FIELDS                                      IL.
     C*******************************************************************
     C     E1INIT        BEGSR
      * THE FOLLOWING ARE NEW FILE FIELDS
     C                   MOVE      DCADSC        E1ADSC
     C                   MOVE      DCADTY        E1ADTY
     C                   MOVE      DCADDI        E1ADDI
     C                   MOVE      DCADLT        E1ADLT
     C                   MOVE      DCADHR        E1ADHR
     C                   MOVE      DCDYCV        E1DYCV
     C                   MOVE      DCNOCV        E1NOCV
     C                   MOVE      DCOCD1        E1OCD1
     C                   MOVE      DCODT1        E1ODT1
     C                   MOVE      DCOCD2        E1OCD2
     C                   MOVE      DCODT2        E1ODT2
     C                   MOVE      DCOCD3        E1OCD3
     C                   MOVE      DCODT3        E1ODT3
     C                   MOVE      DCOCD4        E1OCD4
     C                   MOVE      DCODT4        E1ODT4
     C                   MOVE      DCOSCD        E1OSCD
     C                   MOVE      DCOSFD        E1OSFD
     C                   MOVE      DCOSTD        E1OSTD
     C                   MOVE      DCPATS        E1PATS
     C                   MOVE      DCVC01        E1VC01
     C                   MOVE      DCVA01        E1VA01
     C                   CLEAR                   E1VC02
     C                   CLEAR                   E1VA02
     C                   CLEAR                   E1VC03
     C                   CLEAR                   E1VA03
     C                   CLEAR                   E1VC04
     C                   CLEAR                   E1VA04
     C                   CLEAR                   E1VC05
     C                   CLEAR                   E1VA05
     C                   CLEAR                   E1VC06
     C                   CLEAR                   E1VA06
     C                   CLEAR                   E1VC07
     C                   CLEAR                   E1VA07
     C                   CLEAR                   E1VC08
     C                   CLEAR                   E1VA08
     C                   CLEAR                   E1VC09
     C                   CLEAR                   E1VA09
     C                   CLEAR                   E1VC10
     C                   CLEAR                   E1VA10
     C                   CLEAR                   E1VC11
     C                   CLEAR                   E1VA11
     C                   CLEAR                   E1VC12
     C                   CLEAR                   E1VA12
     C                   CLEAR                   E1SPCD
     C                   MOVE      DCCRDT        E1CRDT
     C                   MOVE      DCIWRI        E1IWRI
     C                   MOVE      DCACCT        E1ACCT
     C                   MOVE      DCPHTY        E1PHTY
     C                   MOVE      DCSFEI        E1SFEI
     C                   MOVE      DCSMOD        E1SMOD
     C                   MOVE      DCBFEI        E1BFEI
     C                   MOVE      DCBMOD        E1BMOD
     C                   MOVE      DCTFEI        E1TFEI
     C                   MOVE      DCTMOD        E1TMOD
     C                   MOVE      DCOFEI        E1OFEI
     C                   MOVE      DCOMOD        E1OMOD
     C                   CLEAR                   E1COC1
     C                   CLEAR                   E1COC2
     C                   CLEAR                   E1COC3
     C                   CLEAR                   E1COC4
     C                   CLEAR                   E1COC5
     C                   CLEAR                   E1COC6
     C                   CLEAR                   E1COC7
     C                   MOVE      DCMRC#        E1MRC#
     C                   CLEAR                   E1OAMT
     C                   MOVE      DCDIA5        E1DIA5
     C                   MOVE      DCDIA6        E1DIA6
     C                   MOVE      DCDIA7        E1DIA7
     C                   MOVE      DCDIA8        E1DIA8
     C                   MOVE      DCDIA9        E1DIA9
     C                   MOVE      DC7A05        E17A05
     C                   MOVE      DC7A06        E17A06
     C                   MOVE      DCDT13        E1DT13
     C                   MOVE      DCDT14        E1DT14
     C                   MOVE      DCDT15        E1DT15
     C                   MOVE      DCDT16        E1DT16
     C                   MOVE      DCDT17        E1DT17
     C                   MOVE      DCDT18        E1DT18
     C                   MOVE      DCPMRI        E1PMRI
     C                   CLEAR                   E1TOTN
     C                   MOVE      DCEMPL        EOEMPL
     C                   MOVE      DCDFC         EODFC
     C                   MOVE      DCLIC#        EOFLIC
     C                   MOVE      DCAMOD        E1AMOD
      *
     C                   MOVE      DCDT09        E1DT9
      * E1AMT4 = HOSPITAL DAILY ACCUM RATE
     C                   MOVE      *ZEROS        X1AMT4
     C                   MOVE      DC10A4        X1AMT4
     C     X1AMT4        IFEQ      *BLANKS
     C                   MOVE      *ZEROS        X1AMT4
     C                   ENDIF
      *
     C                   ENDSR
     C*** END ADD *** 03/11/97 ***
     C*******************************************************************
     C*  GETSEQ SUBROUTINE: GET THE SEQ # TO ATACH A RP RCD TO ITS DETAIL.
     C*******************************************************************
     C     GETSEQ        BEGSR
     C*
     C     COKEY         CHAIN     PC101                              60
     C     *IN60         IFEQ      '0'
     C     C1EOB#        IFEQ      *HIVAL
     C                   Z-ADD     1             C1EOB#
     C                   ELSE
     C                   ADD       1             C1EOB#
     C                   END
     C                   UPDATE    PC101
     C                   ELSE
     C                   Z-ADD     *HIVAL        C1EOB#
     C                   END
     C                   Z-ADD     0             XHOTHR            9 2
     C*
     C                   MOVEL     *BLANKS       XXTRCD
     C     CAKEY         CHAIN     PCA01                              60
     C*
     C     *IN60         DOWEQ     '0'
     C                   Z-ADD     C1EOB#        CAEOB#
     C                   Z-ADD     *ZEROS        CADCS#
     C                   MOVEL     SCTRCD        CATRCD
     C                   UPDATE    PCA01
     C*
     C*** BEGIN ADD *** 12/15/97 **************************************
     C     MGKY          CHAIN     PMG01                              62
     C     *IN62         IFEQ      *OFF
     C                   Z-ADD     C1EOB#        MGEOB#
     C                   UPDATE    PMG01
     C                   ENDIF                                                  62 = Off
     C*** END ADD *** 12/15/97 ****************************************
     C*
     C* DETERMINE IF RECORD IS IN DUPLICATE CLAIM FILE
     C*
     C     DUKEY         CHAIN     PDU01                              61
     C     *IN61         IFEQ      '0'
     C                   MOVE      'Y'           DUFLG1
     C     CAEOMB        IFEQ      '399'
     C                   MOVE      'Y'           DUFLG2
     C                   END
     C                   Z-ADD     C1EOB#        DUEOB#
     C                   UPDATE    PDU01
     C*
     C* DETERMINE IF THIS IS A SERVICE CODE OR CPT4 PROCEDURE CODE
     C*
     C                   MOVE      *BLANKS       XXCODE
     C                   MOVEL     CACPT4        XXCODE
     C*
     C* UPDATE ALL CORRESPONDING DUP RECORDS W/ FLAG TO PRINT ON REPORT
     C*
     C                   Z-ADD     0             DUPCNT            3 0
     C     CAPRTY        IFEQ      '1'
     C     DUKEY2        CHAIN     PDU02                              61
     C     *IN61         DOWEQ     '0'
     C                   MOVE      'Y'           DUFLG1
     C                   ADD       1             DUPCNT
     C                   UPDATE    PDU02
     C     DUKEY2        READE     PDU02                                  61
     C                   END                                                    END DOWEQ 61
     C                   ELSE
     C     CAPRTY        IFEQ      '4'
     C     DUKEY3        CHAIN     PDU03                              61
     C     *IN61         DOWEQ     '0'
     C                   MOVE      'Y'           DUFLG1
     C                   ADD       1             DUPCNT
     C                   UPDATE    PDU03
     C     DUKEY3        READE     PDU03                                  61
     C                   END                                                    END DOWEQ 61
     C                   END                                                    END CAPRTY = 4
     C                   END                                                    END CAPRTY = 1
     C*
     C* IF YOU ONLY HAVE ONE RECORD (I.E NO DUPS) - THEN DELETE THE ONE
     C*
     C     DUPCNT        IFLT      2
     C     DUKEY         DELETE    PDU01                              61
     C                   END                                                    END LT 2
     C*
     C                   END                                                    END DUP SECTION
     C*
     C*** BEGIN ADD *** 07/27/98 **************************************
     C*
     C*  ADDS PAYMENT PLAN DESCRIPTION TO HC10A1 IN FILE FDHCP FOR
     C*  EACH SEQUENCE ENTRY FOR THIS CASE.  IT IS PUT INTO POSITION
     C*  5-6 OF HC10A1.
     C*
     C     *IN78         IFEQ      *ON
     C     COST          ANDEQ     'FL'
     C     HCKY          CHAIN     PHC01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     SCPMNT        XX10A1
     C                   UPDATE    PHC01
     C                   ENDIF                                                  *IN60 = Off
     C                   ENDIF                                                  *IN78 = On
     C*
     C*** END ADD *** 07/27/98 ****************************************
     C*
     C     CAKEY         READE     PCA01                                  60
     C                   END
     C*
     C                   ENDSR
     C*** BEG ADD *** 08/02/98 ***********************
     C*
     C*** SUBROUTINE TO CHECK FOR TAPE SUBMISSION
     C*
     C     SRTAPE        BEGSR
     C*
     C**************       MOVELXXAST     QQSTT
     C                   CALL      'CHKSTR'                             99
     C                   PARM      QQAST         QQSTT             2
     C                   PARM                    QQSTRT            1
     C                   PARM                    QQFLRT            3
     C*
     C                   MOVEL     QQFLRT        XXSTAT
     C*** INIT
     C                   MOVE      'N'           SCTAPE
     C                   MOVE      '0'           *IN81
     C*** SET
     C     QQSTRT        IFEQ      '1'
     C     XXTAPE        ANDEQ     'Y'
     C*
     C     QQAST         IFEQ      'FL'
     C     QQSYS         ANDEQ     'P'
     C     QQAST         OREQ      'TX'
     C     QQSYS         ANDEQ     'P'
     C     QQAST         OREQ      'TX'
     C     QQSYS         ANDEQ     'H'
     C                   MOVE      'Y'           SCTAPE
     C                   MOVE      '1'           *IN81
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END ADD *** 08/02/98 ***********************
     C*******************************************************************
     C****
     OPPD01     E            EPD01
     OPCM01     E            ECM01
     OPDC01     E            EDC01
     OPEO03     E            PEO03E
**  TABLE UA
-- ADD --
- UPDATE -
**  TABLE ST
ALAKAZARCACOCTDEDCFLGAHIIDILINIAKSKYLAMEMDMAMIMNMSWY
MOMTNENVNHNJNMNYNCNDOHOKORPARISCSDTNTXUTVTVAWAWVWIPR
**  TABLE LT  - ALPHABET
ABCDEFGHIJKLMNOPQRSTUVWXYZ
**  PTL - CALLED PGM TITLES
         EOMB CODE TABLE SEARCH
           REQUEST WC PAYMENT
          TIME/MATERIALS ENTRY
**  QCMDEXC TABLE
DLTOVR FILE(FTABL)
OVRDBF FILE(FTABL) TOFILE(*LIBL/FTABL  )
