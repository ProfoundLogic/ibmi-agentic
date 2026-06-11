     H Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H*
     H/TITLE F0363RIL - Hospital UB Detail Information
     H*****************************************************************
     H*
     H* F0363RIL - Hospital UB Detail Information
     H*
     H* CREATED :
     H* 08/11/10  C:227  Project: IL001
     H* COPIED FROM F0363R
     H*****************************************************************
     H*  REVISIONS:
     H*
     H*  07/18/23 C:342 Project: NY060
     H*                 NY Compliance: CARC/RARC Update
     H*
     H*  10/13/15 C:307 Support: PHW37547
     H*                 Corrected error indicator
     H*
     H*  02/21/12  C:277  PROJECT: IL004
     H*                   ADD WINDOW FOR GEO ZIP SEARCH/ZIP CODE COUNTY
     H*                   REGION SEARCH
     H*
     H*  05/10/11 C:257 SUPPORT 76558
     H*                 Load blank to kkrate in KIDO.
     H*                 Load leading blanks in the key field for
     H*                 DRG code when reading FMFSPL1IL
     H*
     H*  07/15/10 C:251 SUPPORT 72899
     H*                 Allow subset on SMSA from provider file
     H*
     H*  11/10/09 C:062 Project 144MBP
     H*                 -DEFAULT FROM AND TO DATE
     H*                 -HCPC AND MOD 1/2 FIELDS NOT REQUIRED.
     H*
     H*  08/31/09 C:227 Project CT005
     H*                         CT 2009 FS
     H*
     H*  07/22/09 C:227 Project AR038
     H*                         Edit checks for AR state
     H*
     H*  05/20/09 C:198 Project FS343
     H*                         Re-compiled only as there is change in
     H*                         structure of FTOPP.
     H*
     F*  04/18/09 C:079 PROJECT TX051
     F*                         CORRECTED HOW UCR FLAG IS BEING SET
     F*
     F*  04/03/09 C:198 PROJECT MD066
     F*                         CHANGED THE ERROR MSG WHEN BOTH SCRVCD
     F*                         AND SCHCPC ARE BLANK.
     F*
     F*  03/18/09 C:198 PROJECT TX051
     F*                         ADDED 1 MORE OPTION IN WDW01, CHANGED
     F*                         CHECK ON SCHCPC AND CHANGED ON SCAPC.
     F*
     F*  03/06/09 C:189 PROJECT FS342
     F*                         OPPS DETAIL SCREEN NOT SHOWING PROPERLY
     F*
     F*  11/28/08 C:191 PROJECT MS017
     F*                         DRG UPDATE
     F*
     F*  08/14/08  C:198  PROJECT TX039 PRISS# 19
     F*                   OPTION PROC CODE INFO WHEN TAKEN ON F9 = CODE INFO
     F*                   SHOULD DISPLAY  PROC CODE INFO FROM FMPRPXX.
     F*
     F*  06/27/08  C:079  PROJECT SY156
     F*                   GETTING RECORD LOCK ON FAEUL.  FIX THIS.
     F*
     F*  06/23/08  C:189  PROJECT: TX039
     F*                   ADDED NEW SELECTION CRITERIA,IPPS PAYMENT
     F*                   INFORMATION.
     F*
     F*  06/12/08  C:198  PROJECT: TX039
     F*                   CHANGED FTOIP TO FTOPP
     F*
     F*  06/11/08  C:189  PROJECT: TX039
     F*                   CHANGED FTIIP TO FTPFP
     F*
     F*  06/10/08  C:198  PROJECT TX040
     F*                   HOME HEALTH CODE FILE INCORPORATED WHEN F10
     F*                   IS PRESSED ON HCPC CODE.
     F*
     F*  05/13/08  C:180  PROJECT MC318 - Remove unnecessary edit.
     F*
     F*  04/07/08  C:189  PROJECT: TX039
     F*                   ADDED CALL TO PROGRAMS FOR IPPS HOSPITAL
     F*                   IMPACTS AND DRG'S.
     F*
     F*  04/04/08  C:062  PROJECT TX040
     F*                   ADD HOME HEALTH PROCESSING
     F*
     F*  04/03/08  C:198  PROJECT: TX039
     F*                   ADDED CALL TO PROGRAMS FOR OPPS HOSPITAL
     F*                   IMPACTS AND APC'S.
     F*
     F*  03/21/08  C:180  PROJECT MC318
     F*
     F*  02/08/08  C:189  PROJECT: MC319
     F*                   DO NOT ALLOW INACTIVE CODES FOR PLACE OF SERVICE
     H*
     H*  10/13/07  C:079  Project: MS014
     H*                   On Code info use CPT only if not blank
     H*                   For MS HCPC/CPT field holds DRG also
     H*
     H*  08/21/07  C:194  Project: MS014
     H*                   Per Richie, removed edit for Revenue Code.
     H*
     H*  06/11/07  C:182  SUPPORT: MC144 57703
     H*                   F9=CODE INFO NOW HAS PROC CODE INFO
     H*
     H*  03/02/07  C:188  PROJECT: CA002
     H*                   837 CA Medical EDI
     H*
     H*  06/20/05  C:079  SUPPORT: 47606
     H*                   1. PASS SCCHRG AND SCPAID TO F0356R01
     H*
     H*  02/17/05  C:61   PROJECT MC266
     H*                   Add Procedure Code Modifier.
     H*
     H*  02/16/05  C:61   PROJECT MC264
     H*                   Add "TO Date of Service".
     H*
     H*  04/07/04  C:079  PROJECT AL051
     H*                   1. GET DESC FROM PROC CODE FILE IF BLANK
     H*                   2. ADD SEARCH WINDOW FOR REV CODE/HCPCS
     H*
     H*  06/20/03  C:062  PROJECT SY068
     H*                   COMPILE ONLY
     H*
     H*  04/25/03  C:079  PROJECT FSMNT
     H*                   FIX COMPARISON FOR SCFDAT VS EOCRDT
     H*
     H*  04/25/03  C:079  PROJECT FS255
     H*                   CHANGES TO F1=REPEAT
     H*
     H*  04/03/03  C:079  SUPPORT MC044 36611
     H*                   SCPOS CHANGED TO 3.0N
     H*
     H*  12/20/02  C:079  PROJECT: NW075
     H*                   CALL N0037R TO DETERMINE MANUAL FEE SCHEDULE
     H*
     H*  08/07/02  C:062  PROJECT FS244
     H*                   ZIP AREA IS NOW BEING ASSIGNED AT THE LINE
     H*
     H*  07/05/02  C:062  SUPPORT:FS246
     H*                   ALLOW DATE WINDOW TO POP UP EVEN IF NOT
     H*                   AN EDI BILL.
     H*
     H*  06/20/02  C:079  Project: FSMNT
     H*                   On an add set units = 1
     H*
     H*  05/21/02  C:079  Support: MC114 32491
     H*                   If DUUNTS=0 populate with 1.
     H*
     H*  05/16/02  C:079  Project: GA015
     H*                   If GA Rad or PT chain without rev code.
     H*
     H*  04/23/02  C:079  Support: MC048 32199
     H*                   Populate billed code fields and description
     H*
     H*  04/23/02  C:062  Proj: FSMNT #224
     H*                   COMPILE ONLY
     H*
     H*  04/19/02  C:079  SUPPORT: GA015
     H*                   Allow charged=0 if GA outp using "P" codes
     H*
     H*  04/08/02  C:062  PROJECT:FS236
     H*                   POPULATE CAPRTY WITH EOBTYP.
     H*
     H*  01/07/02  C:062  PROJECT:FS235
     H*                   DELETE ONLY EOB CODES IF CURRENT RECON LEVEL.
     H*
     H*  01/05/02  C:079  PROJECT:KSMNT
     H*                   Lift edit on hospital service code. Load
     H*                   valid description or "Unlisted"
     H*
     H*  10/31/01  C:079  SUPPORT:MC055 29571
     H*                   On an override don't force and EOMB code if
     H*                   percent >= 100.
     H*
     H*  05/31/01  C:062  PROJECT: MC114 27653
     H*                   DON'T EDIT FIELDS THAT ARE PROTECTED.
     H*
     H*  05/24/01  C:062  PROJECT: FS209
     H*                   LOAD CACODE WITH DURVCD/DURATE
     H*
     H*  05/02/01  C:079  SUPPORT: MC114 27315
     H*                   1. Make FTABL, FMSVL1, and FTPSL user
     H*                      controlled files and close them on return
     H*                      in case adj state changes on next entry.
     H*
     H*  03/26/01  C:062  PROJECT FSMNT
     H*                   CHANGE PROCESSING OF EOB NOTES/CODES
     H*
     H*  11/03/00  C:079  Support MC081 24815
     H*                   Initialize SCUNTS to 1 on an add
     H*
     H*  10/13/00  C:062  Support MC238
     H*                   ADD LOGIC TO DEAL WITH NON-SEARCHABLE FIELDS.
     H*
     H*  08/15/00  C:62   PROJECT: MC215
     H*                   REPLACE SFBPF REFERENCES WITH APIS.
     H*
     H*  05/25/00  C:062  SUPPORT: MC205
     H*                   PASS IN SCRVCD SO AS TO POSITION TO THAT CODE
     H*
     H*  04/11/00  C:117  PROJECT:FS211
     H*                   ADDITIONAL EDITS TO PBILLS PGMS FOR DATA
     H*                   SUBMISSION (Added CARERR & EDIERR subrs)
     H*
     H*  04/04/00 C:117 PROJECT MCMNT
     H*                 Changed function F15 to "Display Lines" rather
     H*                 than "Previous Lines".
     H*
     H*  04/02/00 C:117 PROJECT: MCMNT
     H*                 DISPLAY "EOB CODE NEEDED" MSG WHEN F12=CANCEL
     H*                 FROM F0356R01 (EOB CODE/NOTES)
     H*
     H*  04/02/00 C:117 PROJECT: MCMNT
     H*                 SUPPRESSED ZEROS IN RULE NUMBER
     H*                 AND ADDED EOB CODE TO SCREEN
     H*
     H*  02/25/00 C:117 SUPPORT: MC044/21153
     H*                 Added Adj. code to appear on F1=Repeat
     H*
     H*  02/24/00 C:79  PROJ: FSMNT
     H*                 Remove clearing of red/paid fields. This is
     H*                 done in FA713R.
     H*
     H*  01/11/00 C:62  Support: NW012
     H*                 -REMOVE LOGIC OF MANUAL FEE SCHEDULE.
     H*                 -DON'T CROSS-FOOT IF CSCD = '300', THIS IS FOR
     H*                  THE MANUAL FEE SCHEDULE OVERRIDE CALCULATION.
     H*                 -DON'T DELETE MESSAGE IF 'NW00001'.
     H*
     H*  11/12/99 C:79  MC152
     H*                 Delete/Change efficiency isssue.
     H*
     H*  08/27/99 C:56  MC152
     H*                 When Adjudication Code is override type and
     H*                 override percentage is zero, delete any
     H*                 adjudication/network eob codes or messages.
     H*  07/23/99 C:31  Project: NW010
     H*                 WC Summary File
     H*                 1. Add Manual fee schedule message and code
     H*
     H* 06/15/99 C:56  Project: MC152
     H*                Correct problem of not being able to operate
     H*                function keys after taking F1=Repeat.
     H*                                                               *
     H* 05/18/99 C:56  Project: MC152                                 *
     H*                Changed FDCALB to FDCALF                       *
     H*                                                               *
     H* 05/05/99 C:56  Support: MC025 #17939                          *
     H*                Add deletion of FDDAP records when line item   *
     H*                is deleted.                                    *
     H*                                                               *
     H* 04/19/99 C:56  Support: MC025  #17566                         *
     H*                If adjudication code is not override type,     *
     H*                clear PAID/MRA/OTH/OTH2/NTWK reductions fields.*
     H*                This will prevent extraneous information from  *
     H*                being written in these fields.                 *
     H*
     H* 04/01/99 C:56  Project: MC152
     H*                Allow zero charge amount for an override bill
     H*                type per request from TASB.
     H*
     H* 03/30/99 C:56  Project: MC152                                 *
     H*                Add call to EOB Codes/Notes program when an    *
     H*                override type adjudication code is entered.    *
     H*                If any reductions is entered or calculated,    *
     H*                require entry of EOB code.                     *
     H*                                                               *
     H* 02/23/99 C:56  Project: MC152                                 *
     H*                Add deletion of EOB Codes/Notes and Error      *
     H*                Messages to subroutine DLTPRE.                 *
     H*                                                               *
     H* 02/17/99 C:94 PROJ:SUP16661
     H*               SCRVCD NEEDS TO BE INITIALIZED TO BLANKS
     H*
     H* 02/16/99 C:94 PROJ:SUP16659
     H*               CHANGE DESCRIPTION SO THAT IT IS NOT REQUIRED.
     H*               IF NOTHING IS ENTERED, THEN DEFAULT TO
     H*               '* UNLISTED *' IN THE FILE.
     H*
     H* 09/21/98 C:79 PROJ:MC152
     H*               Load Revenue Code from screen into databas field
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      43      Add mode                                       **
     H*****************************************************************
     FF0363FIL  CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F*
     F*  Physical file definition (for maintaining record)
     FFDDUP     UF A E             DISK
     F                                     RENAME(PDU01:DBRCDF)
     F*
     F*  EOMB Transaction File
     FFAEOP     UF   E             DISK
     F                                     INFDS(DBFEO)
     F*
     F*  Fee Schedule Detail File
     FFDCALF    IF   E           K DISK
     F*
     F*  Fee Schedule Detail File
     FFDCAP     UF A E             DISK
     F                                     RENAME(PCA01:PCA02)
     FFDCAL12   IF   E           K DISK
     F                                     RENAME(PCA01:PCA03)
     FFDCAL10   IF   E           K DISK
     F                                     RENAME(PCA01:PCA04)
     F*
     F*  EOMB Detail EOB/Notes Transaction File
     FFDDEL     UF   E           K DISK
     FFDDEL3    IF   E           K DISK
     F                                     RENAME(PDE01:PDE03)
     F*
     F*  Fee Schedule Claims Status Code File
     FFTCSP     IF   E             DISK
     F                                     RENAME(PCS01:PCS02)
     F*
     F*  Place of Service Table File
     FFTPSL     IF   E           K DISK    USROPN
     F*  Provider Bills Control File
     FFMBEL     IF   E           K DISK
     F*
     F*  WCMC Control File
     FFMCXL     IF   E           K DISK
     F*
     F*  Bill Status Table File
     FFTBSL     IF   E           K DISK
     FFTBSP     IF   E             DISK
     F                                     RENAME(PBS01:PBS02)
     F*
     F*  EOMB Detail Amount Transaction File
     FFDDAL2    UF   E           K DISK
     F*
     F*  Logical over all Tabel Files
     FFTABL     IF   E           K DISK    USROPN
     F                                     RENAME(PTS01:PTS02)
     F                                     IGNORE(PAC01)
     F                                     IGNORE(PEB01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PSP01)
     F                                     IGNORE(PAT01)
     F                                     IGNORE(PFT01)
     F                                     IGNORE(PCC01)
     F                                     IGNORE(PPS01)
     F*
     F* Pre-Cert Master File
     FUMPCP     IF   E             DISK
     FUMPCL3    IF   E           K DISK
     F                                     RENAME(PPC01:PPC02)
     F*
     F* WCMC Claim Master File
     FFMCML1    IF   E           K DISK
     F                                     INFDS(DBFDB)
     FFDDUL     IF   E           K DISK
     F                                     INFDS(DBFDC)
     F* Service Code File
     FFMSVL1    IF   E           K DISK    USROPN
     F                                     INFDS(DBFSV)
     F* Procedure code file
     F************FMPRL   IF  E           K        DISK
     FFMFSL1IL  IF   E           K DISK
     F                                     RENAME(PFS01:PFS03)
     F                                     INFDS(DBFPR)
     FFMFSL2IL  IF   E           K DISK    USROPN
     FFMFSPIL   IF   E             DISK
     F                                     RENAME(PFS01:PFS02)
     FFMRCL     IF   E           K DISK
     F                                     INFDS(DBFRC)
     F*
     F* State FS Rules/Pgm Cross Reference File
     FFDRUL1    IF   E           K DISK
     FFDMGL2    UF   E           K DISK
     FFAESL5    UF   E           K DISK
     FFAEUL     UF   E           K DISK
     FFMP1L     IF   E           K DISK
     FFMGZL     IF   E           K DISK
     FSTSTL1    IF   E           K DISK
     F******FMSVL5TXIF  E           K        DISK
     F******      PSV01                             KRENAMETXSV05
     F************FMPRLTX IF  E           K        DISK
     F************PPR01                             KRENAMETXPR01
     FFTAPL     IF   E           K DISK
     F                                     INFDS(DSAPL)
     F*****FTOPL   IF  E           K        DISK
     F*****                                              KINFDS DSOPL
     F*****FTDGL   IF  E           K        DISK
     F*****                                         KINFDS DSDGL
     F*****FTPFL   IF  E           K        DISK
     F*****                                         KINFDS DSIIL
     F*****FTRVL1  IF  E           K        DISK
     F*****                                         KINFDS DSRVL
     F*****FTLBL1  IF  E           K        DISK
     F*****                                         KINFDS DSLBL
     F*** BEG ADD *** 02/21/12 ****************************************
     FFMF1L2IL  IF   E           K DISK
     F                                     RENAME(PF101:PF102)
     FFMF1L1IL  IF   E           K DISK
     F                                     RENAME(PF101:PF103)
     F                                     INFDS(DBF1R)
     FFMF1PIL   IF   E             DISK
     F*** END ADD *** 02/21/12 ****************************************
      /COPY SCOPYMT01D
     D*
     D DBFDB           DS
     D  DBRRNB               397    400B 0
     D DBFSV           DS
     D  SVRRN                397    400B 0
     D DBFPR           DS
     D  PRRRN                397    400B 0
     D DBFRC           DS
     D  RCRRN                397    400B 0
     D DBFDC           DS
     D  DBRRNC               397    400B 0
     D DSAPL           DS
     D  APRRN                397    400B 0
     D***** BEG Add ******* 02/21/12 **********************************
     D DBF1R           DS
     D  P1RRN                397    400B 0
     D***** END Add ******* 02/21/12 **********************************
     D*****DSOPL       DS
     D*****                               B 397 4000OPRRN
     D*****DSDGL       DS
     D*****                               B 397 4000DGRRN
     D*****DSIIL       DS
     D*****                               B 397 4000IIRRN
     D DBFEO           DS
     D  EORRN                397    400B 0
     D*****DSRVL       DS
     D*****                               B 397 4000RVRRN
     D*****DSLBL       DS
     D*****                               B 397 4000LBRRN
     D                 DS
     D  K1CMSV                 1     11
     D  K1RVCD                 1      3
     D  K1HCPC                 4      8
     D  K1UNTS                 9     11
     D***PY SCOPYMT01I
     D*
     D                 DS
     D  TMTOS                  1      5
     D  HLKEY                  1      2
     D  HDKEY                  3      5
     D TMSMSA          DS
     D  TMSVCD                 1      3
     D  TMCMSV                 4      8
     D*** BEG ADD 05/10/11 *****************************************************
     D                 DS
     D  #PRCPT                 1      5
     D  PRCPT1                 1      4
     D  PRCPT2                 1      3
     D  PRCPT3                 1      2
     D*** END ADD 05/10/11 *****************************************************
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Hospital (UB) Detail-
     D                                      Information')
     D UNLIST          C                   CONST('* UNLISTED *')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)
     D XXANES          C                   CONST('Anesthesia')
     D XXHCPC          C                   CONST('HCPCS')
     D XXPROF          C                   CONST('Professional')
     D XXOUTP          C                   CONST('Outpatient')
     D XXTRAU          C                   CONST('Trauma Admis - -
     D                                     MS-DRG')
     D XXSTAN          C                   CONST('Standard - MS-DRG')
     D*** BEG ADD *** 02/21/12 *****************************************
     D XXTRA1          C                   CONST('Trauma Admis - DRG')
     D XXSTA1          C                   CONST('Standard - DRG   ')

     I*** END ADD *** 02/21/12 *****************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          FDDUP
     C                   PARM                    @@RRN2            9 0          FAEOP
     C                   PARM                    @@RRN3            9 0          FDCAP
     C                   PARM                    @@NWCD            5
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C* KEY LIST FOR WCMC DETAIL FILE
     C*
     C     CAKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKDSEQ
     C*
     C     CAKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     CAKY3         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKEOB#
     C*
     C     CAKY4         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKLIN#
     C*
     C* KEY LIST FOR EOB/Notes File
     C*
     C     DEKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*
     C     DEKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C                   KFLD                    KKDSEQ
     C*
     C*  KEY LIST FOR PLACE OF SERVICE FILE
     C*
     C     PSKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKPOS
     C*
     C*  KEY LIST FOR TABLE FILE
     C*
     C     TBKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKKEY
     C*
     C*  Key List for WCMC Control File
     C*
     C     CXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C* Key List for Bill Status Tabel File
     C*
     C     BSKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTYPE
     C                   KFLD                    KKCODE
     C*
     C* Key List for EOMB Detail Amount Transaction File
     C*
     C     DAKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
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
     C* KEY LIST FOR PRE-CERT MASTER FILE
     C*
     C     PCKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C* KEY LIST FOR WCMC CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C* KEY LIST FOR EOMB PHYS/HCFA BILL DETAIL FILE
     C*
     C     DUKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*
     C* Key list for Service Codes File
     C*
     C     SVKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    K1CMSV
     C                   KFLD                    K1EFDT
     C*
     C     SVKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    K1CMSV
     C*
     C     SVKY3         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    K1RVCD
     C                   KFLD                    K1EFDT
      *
     C     SVKY4         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    K1RVCD
     C*
     C* Key list for State FS Rules/Pgm Cross Reference File
     C*
     C     RUKY1         KLIST
     C                   KFLD                    KKST
     C                   KFLD                    KKATYP
     C                   KFLD                    KKEFDT
     C*
     C     RUKY2         KLIST
     C                   KFLD                    KKST
     C                   KFLD                    KKATYP
     C*
     C* Key List for Provider Bills Control File (FMBEP)
     C*
     C     BEKY          KLIST
     C                   KFLD                    EOAST
     C                   KFLD                    EOSYS
     C                   KFLD                    EOBTYP
     C                   KFLD                    EOSTYP
     C*
     C* key list for geo zip file
     C*
     C     GZKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFRZP
     C                   KFLD                    HLFDAT
     C*
     C     GZKY2         KLIST
     C                   KFLD                    ##CO#
     C*
     C* key list for provider auxiliary file
     C*
     C     P1KY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C     OPKY1         KLIST
     C                   KFLD                    KKPROV            6
     C                   KFLD                    KPEFDT            8 0
     C*
     C     OPKY2         KLIST
     C                   KFLD                    KKPROV
     C*
     C     APKY1         KLIST
     C                   KFLD                    KAHCPC            5
     C                   KFLD                    KKEFDT
     C*
     C     APKY2         KLIST
     C                   KFLD                    KAHCPC
     C*
     C     IIKY1         KLIST
     C                   KFLD                    K1PROV            6
     C                   KFLD                    KPEFDT            8 0
     C*
     C     IIKY2         KLIST
     C                   KFLD                    K1PROV
     C*
     C     DGKY1         KLIST
     C                   KFLD                    K1HDRG            5 0
     C                   KFLD                    K1EFDT
     C*
     C     DGKY2         KLIST
     C                   KFLD                    K1HDRG
     C*
     *
     C     DTCYM1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*  FS BASE CONTROL FILE
     C*
     C* KEY LIST FOR PROVIDER BILL ERROR MESSAGE FILE
     C*
     C     MGKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*
     C*          Provider Network Contract File
     C*          ------------------------------------------------------
     C*  KEY LIST FOR EOMB CHANGE STATUS TRANSACTION FILE
     C*
     C     ESKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKBLST
     C                   KFLD                    KKRCN
     C*
     C     ESKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKBLST
     C     PRKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKRATE
     C                   KFLD                    KKFDAT
     C*
     C     PRKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKRATE
     C*** BEG ADD *** 08/11/10 ****************************************
     C     FSKY          KLIST
     C                   KFLD                    KKZIP
     C                   KFLD                    KKRATE
     C                   KFLD                    KKCAT
     C                   KFLD                    KKFDAT
     C*
     C     FSKY1         KLIST
     C                   KFLD                    KKZIP
     C                   KFLD                    KKRATE
     C                   KFLD                    KKCAT
     C*
     C     FSKY2         KLIST
     C                   KFLD                    KKRATE
     C                   KFLD                    KKFDAT
     C*
     C     FSKY3         KLIST
     C                   KFLD                    KKRATE
     C*
     C*** END ADD *** 08/11/10 ****************************************
     C     RCKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKZIPA
     C                   KFLD                    KKRATE
     C                   KFLD                    KKFDAT
     C*
     C     RCKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKZIPA
     C                   KFLD                    KKRATE
     C*** BEG ADD *** 02/21/12 ****************************************
     C*
     C     F1KY2         KLIST
     C                   KFLD                    KKRATE
     C                   KFLD                    KKFDAT
     C*
     C     F1KY3         KLIST
     C                   KFLD                    KKRATE
     C*
     C     F2KY          KLIST
     C                   KFLD                    KKRGN#
     C                   KFLD                    KKPRCD
     C                   KFLD                    KKCAT
     C                   KFLD                    KKFDAT
     C*
     C     F2KY1         KLIST
     C                   KFLD                    KKRGN#
     C                   KFLD                    KKPRCD
     C                   KFLD                    KKCAT
     C*
     C*** END ADD *** 02/21/12 ****************************************
     C*
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    CALIN#        XXLIN#
     C     *LIKE         DEFINE    CASUB#        KKSUB#
     C     *LIKE         DEFINE    CAFND         KKFND
     C     *LIKE         DEFINE    CAFYR         KKFYR
     C     *LIKE         DEFINE    CACASE        KKCASE
     C     *LIKE         DEFINE    CAFEIN        KKFEIN
     C     *LIKE         DEFINE    CAMOD         KKMOD
     C     *LIKE         DEFINE    CAEOB#        KKEOB#
     C     *LIKE         DEFINE    CADSEQ        KKDSEQ
     C     *LIKE         DEFINE    CSKEY         KKKEY
     C     *LIKE         DEFINE    EOACDT        HLACDT
     C     *LIKE         DEFINE    CAFDAT        HLFDAT
     C     *LIKE         DEFINE    CATDAT        HLTDAT
     C     *LIKE         DEFINE    PSPOS         KKPOS
     C     *LIKE         DEFINE    DURATE        KKHCPC
     C     *LIKE         DEFINE    SVEFDT        KKFDAT
     C     *LIKE         DEFINE    BSTYPE        KKTYPE
     C     *LIKE         DEFINE    BSCODE        KKCODE
     C     *LIKE         DEFINE    CADCS#        XXDCS#
     C     *LIKE         DEFINE    CADCS#        KKDCS#
     C     *LIKE         DEFINE    EOFRDT        HKFDAT
     C     *LIKE         DEFINE    EOCHRG        TLCHRG
     C     *LIKE         DEFINE    CALIN#        KKLIN#
     C     *LIKE         DEFINE    CAOT1C        HLOT1C
     C     *LIKE         DEFINE    CAOT2C        HLOT2C
     C     *LIKE         DEFINE    CARCN         KKRCN
     C     *LIKE         DEFINE    CATOTC        SMCHG
     C     *LIKE         DEFINE    CATOTD        SMMRA
     C     *LIKE         DEFINE    CANDIS        SMNDIS
     C     *LIKE         DEFINE    CAOTHR        SMOTHR
     C     *LIKE         DEFINE    CAOTH2        SMOTH2
     C     *LIKE         DEFINE    CATOTP        SMPAID
     C     *LIKE         DEFINE    CATOTC        DFCHRG
     C     *LIKE         DEFINE    CATOTD        DFMRA
     C     *LIKE         DEFINE    CANDIS        DFNDIS
     C     *LIKE         DEFINE    CAOTHR        DFOTHR
     C     *LIKE         DEFINE    CAOTH2        DFOTH2
     C     *LIKE         DEFINE    CATOTP        DFPAID
     C     *LIKE         DEFINE    PCCERT        HLCERT
     C     *LIKE         DEFINE    PCEXT         HLEXT
     C     *LIKE         DEFINE    CALIN#        ZZLIN#
     C     *LIKE         DEFINE    CADSEQ        ZZDSEQ
     C     *LIKE         DEFINE    DUUNTS        XXUNTS
     C     *LIKE         DEFINE    SVCMSV        KKCMSV
     C     *LIKE         DEFINE    SVEFDT        K1EFDT
     C     *LIKE         DEFINE    RUST          KKST
     C     *LIKE         DEFINE    RUATYP        KKATYP
     C     *LIKE         DEFINE    RUEFDT        KKEFDT
     C     *LIKE         DEFINE    EOCRDT        HLSYDT
     C     *LIKE         DEFINE    CAPAID        XXTOTL
     C**** Beg Chg *** 07/18/23 **************************************
     C**** *LIKE         DEFINE    DEEOB         XXEOB
     C     *LIKE         DEFINE    DEEOBCD       XXEOB
     C**** End Chg *** 07/18/23 **************************************
     C     *LIKE         DEFINE    CALIN#        ##LIN#
     C     *LIKE         DEFINE    CACHRG        ##CHRG
     C     *LIKE         DEFINE    CAPAID        ##PAID
     C     *LIKE         DEFINE    ESBLST        KKBLST
     C     *LIKE         DEFINE    EOCO#         KKCO#
     C     *LIKE         DEFINE    EOCRDT        HLCRDT
     C     *LIKE         DEFINE    EOCRDT        HLCSDT
     C     *LIKE         DEFINE    EOCRDT        HLRRDT
     C     *LIKE         DEFINE    EOCRDT        HLRSDT
     C     *LIKE         DEFINE    GZFRZP        KKFRZP
     C     *LIKE         DEFINE    GZEFDT        HLEFDT
     C     *LIKE         DEFINE    DURATE        KKRATE
     C     *LIKE         DEFINE    CAZIPA        KKZIPA
     C     *LIKE         DEFINE    FSZIP         @@ZIP
     C     *LIKE         DEFINE    FSCODE        KKPRCD
     C     *LIKE         DEFINE    FSCAT         KKCAT
     C     *LIKE         DEFINE    FSZIP         KKZIP
     C                   MOVE      *BLANKS       TMSVCD
     C*** BEG ADD *** 02/21/12 ****************************************
     C     *LIKE         DEFINE    F1RGN#        KKRGN#
     C*** END ADD *** 02/21/12 ****************************************
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
     C                   Z-ADD     SCFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFDAT
      *
     C                   Z-ADD     SCTDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTDAT
     C*
     C                   ENDSR
      *
      *
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C*
     C* DELETE THE FDCAP RECORD
     C*
     C     @@RRN3        CHAIN     PCA02                              99
     C     *IN99         IFEQ      *OFF
     C                   DELETE    PCA02
     C                   ENDIF
     C*
     C* DELETE EOB CODES/NOTES FOR THE LINE ITEM
     C*
     C     DEKY          CHAIN     PDE01                              99
     C     *IN99         DOWEQ     *OFF
     C                   DELETE    PDE01
     C     DEKY          READE     PDE01                                  99
     C                   ENDDO                                                  *IN99 = *OFF
     C*
     C* DELETE THE DETAIL TRANSACTION RECORDS FOR LINE ITEM
     C*
     C     DAKY          CHAIN     PDA01                              99
     C     *IN99         DOWEQ     *OFF
     C                   DELETE    PDA01
     C     DAKY          READE     PDA01                                  99
     C                   ENDDO
     C*
     C* DELETE ERROR MESSAGES FOR THE LINE ITEM
     C*
     C     MGKY          CHAIN     PMG01                              99
     C     *IN99         DOWEQ     *OFF
     C                   DELETE    PMG01
     C     MGKY          READE     PMG01                                  99
     C                   ENDDO                                                  *IN99 = *OFF
     C*
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*** BEG ADD *** 04/11/00 ****************************************
     C*================================================================
     C* CARERR - Date check subrountine
     C*================================================================
     C     CARERR        BEGSR
     C*
     C     CARTAG        TAG
     C*
     C     EORCN         IFEQ      0
     C                   SETON                                        19
     C                   SETOFF                                       20
     C                   Z-ADD     EOCRDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCRDT
     C                   Z-ADD     EOCSDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCSDT
     C                   EXFMT     SCR03
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C* DATE CARR RCVD CONNOT BE BLANK
     C*
     C     SCCRDT        IFEQ      *ZEROS
     C                   SETON                                        9010
     C                   GOTO      CARTAG
     C                   ENDIF
     C*
     C* DATE RECIEVED MUST BE VALID
     C*
     C     SCCRDT        IFNE      *ZEROS
     C                   Z-ADD     SCCRDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9021
     C                   GOTO      CARTAG
     C                   ELSE
     C                   MOVEL     SRDTO         SCCRDT
     C                   Z-ADD     SCCRDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLCRDT
     C                   ENDIF                                                  SRERR = '1'
     C*
     C                   ENDIF                                                  SCRRDT NE *ZEROS
     C*
     C* Received date cannot be less than DOI
     C*
     C     *IN90         IFEQ      *OFF
     C     HLCRDT        ANDNE     *ZEROS
     C     HLCRDT        ANDLT     EOACDT
     C                   SETON                                        9022
     C                   GOTO      CARTAG
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C* Received date cannot be less than EOMB TO DATE
     C*
     C     *IN90         IFEQ      *OFF
     C     HLCRDT        ANDNE     *ZEROS
     C     HLCRDT        ANDLT     EOTODT
     C     EUEDI         IFEQ      'Y'
     C                   SETON                                        9023
     C                   GOTO      CARTAG
     C                   ENDIF
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C* Received date cannot be greater than system date
     C*
     C     *IN90         IFEQ      *OFF
     C     HLCRDT        ANDNE     *ZEROS
     C     HLCRDT        ANDGT     HLSYDT
     C                   SETON                                        9025
     C                   GOTO      CARTAG
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C*  IF ADJ STATE IS KY, SENT DT CANNOT BE BLANK
     C*
     C     EOAST         IFEQ      'KY'
     C     SCCSDT        ANDEQ     *ZEROS
     C                   SETON                                        9008
     C                   GOTO      CARTAG
     C                   ENDIF
     C*
     C* Received date cannot be greater than sent date
     C*
     C     *IN90         IFEQ      *OFF
     C     SCCRDT        IFNE      *ZEROS
     C     SCCSDT        ANDNE     *ZEROS
     C*
     C                   Z-ADD     SCCSDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLCSDT
     C*
     C     HLCSDT        IFGT      HLCRDT
     C                   SETON                                        9026
     C                   GOTO      CARTAG
     C                   ENDIF                                                  HLCRDT GT HLRSDT
     C*
     C                   ENDIF                                                  SCCRDT NE *ZEROS
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCCSDT        ANDNE     *ZEROS
     C                   Z-ADD     SCCSDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9027
     C                   GOTO      CARTAG
     C                   ELSE
     C                   MOVEL     SRDTO         SCCSDT
     C*
     C                   Z-ADD     SCCSDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLCSDT
     C                   ENDIF                                                  SRERR = '1'
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C* Sent date cannot be less than DOI
     C*
     C     *IN90         IFEQ      *OFF
     C     HLCSDT        ANDNE     *ZEROS
     C     HLCSDT        ANDLT     EOACDT
     C                   SETON                                        9013
     C                   GOTO      CARTAG
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C* Sent date cannot be less than EOMB TO DATE
     C*
     C     *IN90         IFEQ      *OFF
     C     HLCSDT        ANDNE     *ZEROS
     C     HLCSDT        ANDLT     EOTODT
     C     EUEDI         IFEQ      'Y'
     C                   SETON                                        9012
     C                   GOTO      CARTAG
     C                   ENDIF
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C* Sent date cannot be greater than system date
     C*
     C     *IN90         IFEQ      *OFF
     C     HLCSDT        ANDNE     *ZEROS
     C     HLCSDT        ANDGT     HLSYDT
     C                   SETON                                        9011
     C                   GOTO      CARTAG
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C     @@RRN2        CHAIN     PEO01                              99
     C                   MOVE      SCCRDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EOCRDT
     C                   Z-ADD     SCCSDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EOCSDT
     C*
     C                   UPDATE    PEO01
     C*
     C                   ELSE                                                   *INKL=OFF
     C*
     C                   SETON                                        90
     C                   GOTO      CAREND
     C*
     C                   ENDIF                                                  *INKL=*OFF
     C*
     C                   ELSE                                                   RCN=0
     C*
     C                   SETON                                        20
     C                   SETOFF                                       19
     C                   Z-ADD     ESRRDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRRDT
     C                   Z-ADD     ESRSDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRSDT
     C                   EXFMT     SCR03
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C*  CARR RCVD DT CANNOT BE BLANK
     C*
     C     SCRRDT        IFEQ      *ZEROS
     C                   SETON                                        9010
     C                   GOTO      CARTAG
     C                   ENDIF
     C*
     C* DATE RECIEVED MUST BE VALID
     C*
     C     SCRRDT        IFNE      *ZEROS
     C                   Z-ADD     SCRRDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9021
     C                   GOTO      CARTAG
     C                   ELSE
     C                   MOVEL     SRDTO         SCRRDT
     C                   Z-ADD     SCRRDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLRRDT
     C                   ENDIF                                                  SRERR = '1'
     C*
     C                   ENDIF                                                  SCRRDT NE *ZEROS
     C*
     C* Received date cannot be less than DOI
     C*
     C     *IN90         IFEQ      *OFF
     C     HLRRDT        ANDNE     *ZEROS
     C     HLRRDT        ANDLT     EOACDT
     C                   SETON                                        9022
     C                   GOTO      CARTAG
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C* Received date cannot be less than EOMB TO DATE
     C*
     C     *IN90         IFEQ      *OFF
     C     HLRRDT        ANDNE     *ZEROS
     C     HLRRDT        ANDLT     EOTODT
     C     EUEDI         IFEQ      'Y'
     C                   SETON                                        9023
     C                   GOTO      CARTAG
     C                   ENDIF
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C* Received date cannot be greater than system date
     C*
     C     *IN90         IFEQ      *OFF
     C     HLRRDT        ANDNE     *ZEROS
     C     HLRRDT        ANDGT     HLSYDT
     C                   SETON                                        9025
     C                   GOTO      CARTAG
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C*  IF ADJ STATE IS KY, SENT DT CANNOT BE BLANK
     C*
     C     EOAST         IFEQ      'KY'
     C     SCCSDT        ANDEQ     *ZEROS
     C                   SETON                                        9008
     C                   GOTO      CARTAG
     C                   ENDIF
     C*
     C* Received date cannot be greater than sent date
     C*
     C     *IN90         IFEQ      *OFF
     C     SCRRDT        IFNE      *ZEROS
     C     SCRSDT        ANDNE     *ZEROS
     C*
     C                   Z-ADD     SCRSDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLRSDT
     C*
     C     HLRSDT        IFGT      HLRRDT
     C                   SETON                                        9026
     C                   GOTO      CARTAG
     C                   ENDIF                                                  HLCRDT GT HLRSDT
     C*
     C                   ENDIF                                                  SCCRDT NE *ZEROS
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCRSDT        ANDNE     *ZEROS
     C                   Z-ADD     SCRSDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9027
     C                   GOTO      CARTAG
     C                   ELSE
     C                   MOVEL     SRDTO         SCCSDT
     C*
     C                   Z-ADD     SCRSDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLRSDT
     C                   ENDIF                                                  SRERR = '1'
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C* Sent date cannot be less than DOI
     C*
     C     *IN90         IFEQ      *OFF
     C     HLRSDT        ANDNE     *ZEROS
     C     HLRSDT        ANDLT     EOACDT
     C                   SETON                                        9013
     C                   GOTO      CARTAG
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C* Sent date cannot be less than EOMB TO DATE
     C*
     C     *IN90         IFEQ      *OFF
     C     HLRSDT        ANDNE     *ZEROS
     C     HLRSDT        ANDLT     EOTODT
     C     EUEDI         IFEQ      'Y'
     C                   SETON                                        9012
     C                   GOTO      CARTAG
     C                   ENDIF
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C* Sent date cannot be greater than system date
     C*
     C     *IN90         IFEQ      *OFF
     C     HLRSDT        ANDNE     *ZEROS
     C     HLRSDT        ANDGT     HLSYDT
     C                   SETON                                        9011
     C                   GOTO      CARTAG
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C     ESKY1         SETGT     PES01
     C     ESKY2         READPE    PES01                                  60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      SCRRDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ESRRDT
     C                   MOVE      SCRSDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ESRSDT
     C*
     C                   UPDATE    PES01
     C*
     C                   ELSE                                                   *INKL=OFF
     C*
     C                   SETON                                        90
     C                   GOTO      CAREND
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  *INKL=*OFF
     C*
     C                   ENDIF                                                  RCN=0
     C*
     C     CAREND        ENDSR
     C*================================================================
     C*  EDIERR - EDI ERROR SR FOR FUTURE DATES
     C*================================================================
     C     EDIERR        BEGSR
     C*
     C     EDITAG        TAG
     C*
     C                   MOVEL     EUEDI         SCEDI
     C                   EXFMT     SCR02
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C     SCEDI         IFEQ      *BLANKS
     C                   SETON                                        9017
     C                   GOTO      EDITAG
     C                   ELSE
     C                   SETOFF                                       17
     C                   ENDIF                                                  SCEDI IFEQ BLANKS
     C*
     C     SCEDI         IFNE      *BLANKS
     C     SCEDI         ANDNE     'Y'
     C     SCEDI         ANDNE     'N'
     C                   SETON                                        9018
     C                   GOTO      EDITAG
     C                   ELSE
     C                   SETOFF                                       18
     C                   ENDIF                                                  SCEDI IFNE BLANKS
     C*
     C     *IN90         IFEQ      *OFF
     C     EUKY          CHAIN     PEU01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     SCEDI         EUEDI
     C                   UPDATE    PEU01
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  *IN90=*OFF
     C                   ENDIF                                                  *INKL=*OFF
     C*
     C     EDIEND        ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLSYDT
     C*
     C* Validate Claim Status (Adjudication) code
     C*
     C     SCCSCD        IFEQ      *BLANKS
     C     *IN46         ANDEQ     *OFF
     C                   SETON                                        9071
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCCSCD IFEQ BLANKS
     C*
     C                   MOVEL(P)  SCCSCD        KKKEY
     C     TBKY          CHAIN     PCS01                              99
     C     *IN99         IFEQ      *ON
     C     *IN46         ANDEQ     *OFF
     C                   SETON                                        9072
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C*
     C* Validate Place of Service
     C*
     C     SCPOS         IFEQ      *ZEROS
     C     *IN46         ANDEQ     *OFF
     C                   SETON                                        9076
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCPOS IFEQ BLANKS
     C*
     C                   MOVE(P)   SCPOS         KKPOS
     C     PSKY          CHAIN     PPS01                              99
     C     *IN99         IFEQ      *ON
     C     *IN46         ANDEQ     *OFF
     C                   SETON                                        9077
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C*
     C     *IN99         IFEQ      *OFF
     C     *IN46         ANDEQ     *OFF
     C     PSSTAT        ANDNE     'A'
     C                   SETON                                        9025
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C*
     C* Service Date must be entered
     C*
     C     SCFDAT        IFEQ      *ZEROS
     C     *IN46         ANDEQ     *OFF
     C                   SETON                                        9073
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCFDAT IFEQ ZEROS
     C*
     C* Validate Date of Service
     C*
     C                   Z-ADD     SCFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C     *IN46         ANDEQ     *OFF
     C                   SETON                                        9074
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     SRDTO         SCFDAT
     C                   ENDIF
     C*
     C* Date Of Service cannot be less than Accident Date
     C*
     C                   Z-ADD     SCFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLFDAT
     C*
     C     HLFDAT        IFLT      EOACDT
     C     *IN46         ANDEQ     *OFF
     C                   SETON                                        9075
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLFDAT IFLT HLACDT
     C*
      *
      * To date of service
      *
     C     SCTDAT        IFNE      *ZERO
     C                   Z-ADD     SCTDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTDAT
      *
     C     SRERR         IFEQ      *ON
     C                   SETON                                        9007
     C                   GOTO      CK1END
     C                   ENDIF
      *
     C                   Z-ADD     SCTDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLTDAT
      *
      * BASE THE EDIT OF PREVENTING FUTURE SERVICE DATES ON CONTROL
      * FLAG SFBPF/FBAL04 POSTION 8 (BLANK = EDIT, Y = ALLOW FUTURE DATES)
      *
     C     ##SDTS        IFNE      'Y'
      *
     C     HLTDAT        IFGT      HLSYDT
     C     *IN46         ANDEQ     *OFF
     C                   SETON                                        9029
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLTDAT GT HLSYDT
      *
     C                   ENDIF                                                  ##SDTS
      *
      * To Date Of Service cannot be less than from Date of Service
      *
     C     HLTDAT        IFNE      0
     C     HLTDAT        ANDLT     HLFDAT
     C     *IN46         ANDEQ     *OFF
     C                   SETON                                        9028
     C                   GOTO      CK1END
     C                   ENDIF
      *
     C                   ENDIF                                                  SCTDAT
     C*
     C* BASE THE EDIT OF PREVENTING FUTURE SERVICE DATES ON CONTROL
     C* FLAG SFBPF/FBAL04 POSTION 8 (BLANK = EDIT, Y = ALLOW FUTURE DATES)
     C*
     C                   MOVE      ' '           XXFSDF            1
     C     ##SDTS        IFNE      'Y'
     C*
     C     HLFDAT        IFGT      HLSYDT
     C     *IN46         ANDEQ     *OFF
     C                   SETON                                        9030
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLFDAT GT HLSYDT
     C*  Give an error window if the date is a future date and the
     C*  data submission flag is 'Y'.
     C*
     C                   ELSE                                                   XXFSDF='Y'
     C     EUKY          CHAIN(N)  PEU01                              60
     C     *IN60         IFEQ      *OFF
     C     EUEDI         IFEQ      'Y'
     C     HLFDAT        ANDGT     HLSYDT
     C     *IN46         ANDEQ     *OFF
     C                   EXSR      EDIERR
     C                   SETON                                        90
     C                   GOTO      CK1END
     C                   ENDIF                                                  EUEDI=Y
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  XXFSDF = *BLANKS
     C*
     C* Date of Service must be less than Carr Rcvd Dt
     C*     (If recon = 0)  FAEOP
     C*
     C* This is all conditioned on weather or not the EDI flag is on
     C*
     C     EORCN         IFEQ      0
     C     HLFDAT        ANDGT     EOCRDT
     C     *IN46         ANDEQ     *OFF
     C                   EXSR      CARERR
     C     EUEDI         IFEQ      'Y'
     C                   SETON                                        90
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF                                                  EORCN=0
     C*
     C*     (If recon > 0)  FAESP
     C*
     C     EORCN         IFGT      0
     C                   MOVEL     'R'           KKBLST
     C     ESKY1         SETGT     PES01
     C     ESKY2         READPE    PES01                                  60
     C     *IN60         IFEQ      *OFF
     C     HLFDAT        ANDGT     ESRRDT
     C     *IN46         ANDEQ     *OFF
     C                   EXSR      CARERR
     C     EUEDI         IFEQ      'Y'
     C                   SETON                                        90
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ELSE                                                   *IN60
     C     EORCN         SUB       1             KKRCN
     C     ESKY1         SETGT     PES01
     C     ESKY2         READPE    PES01                                  60
     C     *IN60         IFEQ      *OFF
     C     HLFDAT        ANDGT     ESRRDT
     C     *IN46         ANDEQ     *OFF
     C                   EXSR      CARERR
     C     EUEDI         IFEQ      'Y'
     C                   SETON                                        90
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  EORCN>0
     C                   MOVEL     *BLANKS       SCZIPA
     C                   MOVEL     *BLANKS       SCZIPB
     C                   Z-ADD     *ZEROS        HLEFDT
     C*
     C* Let's get the zip area forn this line.
     C*
     C     P1KY          CHAIN     PP101                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  P1ZIP         KKFRZP
     C*
     C     GZKY1         SETGT     PGZ01
     C     GZKY2         READPE    PGZ01                                  99
     C     *IN99         DOWEQ     *OFF
     C     GZSTAT        IFNE      'I'
     C     KKFRZP        ANDLE     GZTOZP
     C     HLEFDT        IFEQ      *ZEROS
     C     GZEFDT        ORGT      HLEFDT
     C     GZEFDT        ANDLE     HLFDAT
     C                   MOVEL     GZZIPA        SCZIPA
     C                   MOVEL     GZZIPA        SCZIPB
     C                   Z-ADD     GZEFDT        HLEFDT
     C                   ENDIF
     C                   ENDIF
     C     GZKY2         READPE    PGZ01                                  99
     C                   ENDDO
     C                   ENDIF
     C*
     C* Service Code must be entered
     C*
     C     SCRVCD        IFEQ      *ZEROS
     C     SCHCPC        ANDEQ     *BLANKS
     C     *IN46         ANDEQ     *OFF
     C                   SETON                                        9095
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCRVCD = *ZEROS
      * TX: Revenue code always required for Outpatient bills
      *
     C     SCRVCD        IFEQ      *ZEROS
     C     SCAST         ANDEQ     'TX'
     C     EUICN         IFEQ      'AO'
     C     EUICN         OREQ      'AS'
     C     EUICN         OREQ      'HO'
     C     EUICN         OREQ      'HS'
     C                   SETON                                        9092
     C                   GOTO      CK1END
     C                   ENDIF                                                              ROS
     C                   ENDIF                                                              ROS
      * Procedure Code Modifier
      *
     C     SCPMOD        IFNE      *BLANKS
     C     *IN46         ANDEQ     *OFF
     C                   MOVEL(P)  SCPMOD        KKKEY
     C     TBKY          CHAIN     PPM01                              60
      *
     C     *IN60         IFEQ      *ON
     C     PMSTAT        OREQ      'I'
     C                   SETON                                        9015
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN60
      *
     C                   ENDIF                                                  SCPMOD          NKS
      *
      * Procedure Code Modifier 2
      *
     C     SCMOD2        IFNE      *BLANKS
     C     *IN46         ANDEQ     *OFF
     C                   MOVEL(P)  SCMOD2        KKKEY
     C*** Beg Chg *** 10/13/15 ****************************************
     C*****TBKY          CHAIN     PPM01                              99
     C     TBKY          CHAIN     PPM01                              60
     C*** End Chg *** 10/13/15 ****************************************
      *
     C     *IN60         IFEQ      *ON
     C     PMSTAT        OREQ      'I'
     C                   SETON                                        9088
     C                   GOTO      CK1END
     C*** Beg Chg *** 10/13/15 ****************************************
     C*******            ENDIF                                                  *IN99
     C                   ENDIF                                                  *IN60
     C*** End Chg *** 10/13/15 ****************************************
      *
     C                   ENDIF                                                  SCMOD2          NKS
     C*
     C* Description is required
     C*
     C                   MOVE      SCRVCD        K1RVCD
     C     SCAST         IFEQ      'IL'
     C                   MOVE      *BLANKS       K1HCPC
     C                   ELSE
     C                   MOVE      SCHCPC        K1HCPC
     C                   ENDIF
     C                   MOVE      SCHCPC        KKRATE
     C                   MOVE      *ZEROS        K1UNTS
     C                   Z-ADD     SCFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         K1EFDT
     C                   MOVEL     SRDTO         KKFDAT
     C*
     C* Revenue code  valid and active
     C     SCRVCD        IFNE      0
     C     SVKY1         SETGT     PSV01
     C     SVKY2         READPE    PSV01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C     SVSTAT        ANDEQ     'A'
     C                   MOVEL(P)  SVDESC        SCCDDS
     C                   ELSE
     C                   SETON                                        9093
     C                   GOTO      CK1END
     C                   ENDIF                                                  IN99 = OFF
     C                   ENDIF                                                  SCRVCD <> 0
     C*
      *
     C********** @EDTRC    IFEQ 'Y'
     C********** SVKY3     SETGTTXSV05               99
      *
     C********** *IN99     IFEQ *ON
     C********** *HIVAL    SETGTTXSV05
     C**********           ENDIF
      *
     C********** SVKY4     REDPETXSV05                   99
     C*
     C********** *IN99     IFEQ *OFF
      *
     C********** *IN99     DOWEQ*OFF
     C********** SVSTAT    ANDNE'A'
     C********** SVKY4     REDPETXSV05                   99
     C**********           ENDDO
      *
     C********** SVEFDT    IFGT K1EFDT
     C********** *IN99     OREQ *ON
     C**********           MOVE *ON       *IN90
     C**********           MOVE *ON       *IN93
     C**********           GOTO CK1END
     C**********           ENDIF
      *
     C**********           ELSE
     C**********           MOVE *ON       *IN90
     C**********           MOVE *ON       *IN93
     C**********           GOTO CK1END
     C**********           ENDIF
     C*********            ENDIF
      *
      * HCPC Code - Valid and Active when entered.
      *
     C*** BEG ADD *** 02/21/12 ************************************************
     C     SCHCPC        IFNE      *BLANKS
     C     KKFDAT        IFLT      1120101
     C*** END ADD *** 02/21/12 ************************************************
     C*** BEG DEL *** 02/21/12 ************************************************
     C********** SCHCPC    IFNE *BLANKS
     C*** END DEL *** 02/21/12 ************************************************
     C     FSKY2         SETGT     PFS01
     C     FSKY3         READPE    PFS01                                  99
     C*
     C     *IN98         IFEQ      *OFF
     C     FSSTAT        ANDEQ     'A'
     C                   MOVEL(P)  FSDSC1        SCCDDS
     C                   ELSE
     C                   SETON                                        909435
     C                   GOTO      CK1END
     C                   ENDIF
     C*** BEG DEL *** 02/21/12 ************************************************
     C******************   ENDIF
     C*** END DEL *** 02/21/12 ************************************************
     C*** BEG ADD *** 02/21/12 ************************************************
     C                   ELSE
     C     F1KY2         SETGT     PF102
     C     F1KY3         READPE    PF102                                  99
     C*
     C     *IN98         IFEQ      *OFF
     C     F1STAT        ANDEQ     'A'
     C                   MOVEL(P)  F1DSC1        SCCDDS
     C                   ELSE
     C                   SETON                                        909435
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF                                                  SCHCPC <> blank
     C*** END ADD *** 02/21/12 ************************************************
     C*
     C* VERIFY THAT FEE SCHEDULE IS ON SYSTEM FOR SERVICE DATE
     C* AND VERIFY THAT CODE IS EFFECTIVE FOR FEE SCHEDULE.
     C*     ONLY IF THE CODE IS ON THE FILE
     C*
     C     SVKY1         SETGT     PSV01
     C     SVKY2         READPE    PSV01                                  99
     C     *IN99         IFEQ      *OFF                                         code is on the file
     C                   MOVE      SCAST         KKST
     C                   MOVE      'H'           KKATYP
     C                   Z-ADD     K1EFDT        KKEFDT
     C     RUKY1         SETGT     PRU01
     C     RUKY2         READPE    PRU01                                  99
     C     *IN99         IFEQ      *ON
     C     *IN46         ANDEQ     *OFF
     C                   SETON                                        9085
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C*
     C* Charges are required
     C*
     C*
     C* IF ADJUDICATION CODE IS OVERRIDE TYPE, ALLOW ZERO CHARGE AMOUNT
     C* OR IF GEORGIA OUTPATIENT BILL USING "P" CODES
     C*
     C     CSOVRD        IFNE      'Y'
     C*
     C     SCCHRG        IFEQ      *ZEROS
     C                   SETON                                        9080
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCCHRG IFEQ ZEROS
     C                   ENDIF                                                  CSOBRD NE Y
     C*
     C*
     C* TYPE CODE #1 REQUIRED IF CORRESPONDING AMOUNT IS ENTERED
     C*
     C     SCOTHR        IFNE      *ZEROS
     C     SCOT1C        ANDEQ     *BLANKS
     C                   SETON                                        9081
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCOTHER IFN *ZEROS
     C*
     C* VALIDATE TYPE CODE #1
     C*
     C     SCOT1C        IFNE      *BLANKS
     C                   MOVE      'O'           KKTYPE
     C                   MOVEL(P)  SCOT1C        KKCODE
     C     BSKY          CHAIN     PBS01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9082
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C                   ENDIF                                                  SCOT1C IFNE *BLANKS
     C*
     C* TYPE CODE #2 REQUIRED IF CORRESPONDING AMOUNT IS ENTERED
     C*
     C     SCOTH2        IFNE      *ZEROS
     C     SCOT2C        ANDEQ     *BLANKS
     C                   SETON                                        9083
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCOTHER IFN *ZEROS
     C*
     C* VALIDATE TYPE CODE #2
     C*
     C     SCOT2C        IFNE      *BLANKS
     C                   MOVE      'O'           KKTYPE
     C                   MOVEL(P)  SCOT2C        KKCODE
     C     BSKY          CHAIN     PBS01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9084
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C                   ENDIF                                                  SCOT2C IFNE *BLANKS
     C*
     C* CROSS-FOOT IF ADJUDICATION CODE IS OVERRIDE TYPE AND HAS
     C* NO OVERRIDE PRECENTAGE
     C*
     C                   MOVEL(P)  SCCSCD        KKKEY
     C     TBKY          CHAIN     PCS01                              99
     C*
     C* IF THE SUM OF THE REDUCTIONS AND PAID IS GREATER THAN THE
     C* CHARGED AMOUNT, THEN GIVE AN ERROR
     C*
     C     *IN99         IFEQ      *OFF
     C     CSOVRD        ANDEQ     'Y'
     C     CSOVRP        ANDEQ     *ZEROS
     C     SCCHRG        ANDNE     *ZEROS
     C     SCMRA         ADD       SCOTHR        XXTOTL
     C                   ADD       SCOTH2        XXTOTL
     C   47              ADD       SCNTWK        XXTOTL
     C                   ADD       SCPAID        XXTOTL
     C     XXTOTL        IFGT      SCCHRG
     C                   SETON                                        9058
     C                   GOTO      CK1END
     C                   ENDIF                                                  XXTOTL GT SCCHRG
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C* SCENARIO #1
     C*
     C* MOVE CHARGE AMOUNT INTO MRA AMOUNT IF ALL REDUCTIONS ARE
     C* *ZERO AND PAID AMOUNT IS ZERO
     C*
     C     *IN99         IFEQ      *OFF
     C     CSOVRD        ANDEQ     'Y'
     C     CSOVRP        ANDEQ     *ZEROS
     C     SCCHRG        ANDNE     *ZEROS
     C     SCMRA         ANDEQ     *ZEROS
     C     SCOTHR        ANDEQ     *ZEROS
     C     SCOTH2        ANDEQ     *ZEROS
     C     SCNTWK        ANDEQ     *ZEROS
     C     SCPAID        ANDEQ     *ZEROS
     C     SCCSCD        ANDNE     '300'
     C                   Z-ADD     SCCHRG        SCMRA
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C* SCENARIO #2
     C*
     C* IF CHARGE AMOUNT IS NOT ZERO, PAID AMOUNT IS NOT ZERO
     C* AND ALL REDUCTIONS AMOUNTS ARE ZERO, THEN PUT DIFFERENCE
     C* BETWEEN CHARGE AMOUNT AND PAID AMOUNT IN MRA REDUCTIONS
     C*
     C     *IN99         IFEQ      *OFF
     C     CSOVRD        ANDEQ     'Y'
     C     CSOVRP        ANDEQ     *ZEROS
     C     SCCHRG        ANDNE     *ZEROS
     C     SCPAID        ANDNE     *ZEROS
     C     SCMRA         ANDEQ     *ZEROS
     C     SCOTHR        ANDEQ     *ZEROS
     C     SCOTH2        ANDEQ     *ZEROS
     C     SCNTWK        ANDEQ     *ZEROS
     C     SCCHRG        SUB       SCPAID        SCMRA
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C* THE FOLLOWING MATH WILL CALCULATE THE PAID AMOUNT
     C* BASED ON THE CHARGED AMOUNT AND REDUCTION AMOUNTS ENTERED
     C* THIS WILL NOT CAUSE PROBLEMS WITH SCENARIOS #1 AND #2
     C*
     C     SCCHRG        IFNE      *ZEROS
     C     SCCSCD        ANDNE     '300'
     C     SCCHRG        SUB       SCMRA         SCPAID
     C                   SUB       SCOTHR        SCPAID
     C                   SUB       SCOTH2        SCPAID
     C                   SUB       SCNTWK        SCPAID
     C                   ENDIF                                                  SCCHRG NE *ZEROS
     C*
     C* SPECIAL CASE OF MFS = 'Y' FOR '300
     C*
     C     SCCHRG        IFNE      *ZEROS
     C     SCCSCD        ANDEQ     '300'
     C*
     C     SCMRA         IFNE      *ZEROS
     C     SCOTHR        ORNE      *ZEROS
     C     SCOTH2        ORNE      *ZEROS
     C     SCNTWK        ORNE      *ZEROS
     C*
     C     SCCHRG        SUB       SCMRA         SCPAID
     C                   SUB       SCOTHR        SCPAID
     C                   SUB       SCOTH2        SCPAID
     C                   SUB       SCNTWK        SCPAID
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C* IF OVERIDE TYPE AND CHARGE IS ZEROS AND PAID NE ZEROS, THEN
     C* PUT THE DIFFERENCE IN SCMRA
     C*
     C     SCCHRG        IFEQ      *ZEROS
     C     CSOVRD        ANDEQ     'Y'
     C     SCPAID        ANDNE     *ZEROS
     C     SCCHRG        SUB       SCPAID        SCMRA
     C                   Z-ADD     0             SCOTHR
     C                   Z-ADD     0             SCOTH2
     C                   Z-ADD     0             SCNTWK
     C                   ENDIF                                                  SCCHRG NE *ZEROS
     C*
     C     SCCHRG        IFEQ      *ZEROS
     C     CSOVRD        ANDEQ     'Y'
     C     SCPAID        ANDEQ     *ZEROS
     C     SCCHRG        SUB       SCMRA         SCPAID
     C                   SUB       SCOTHR        SCPAID
     C                   SUB       SCOTH2        SCPAID
     C                   SUB       SCNTWK        SCPAID
     C                   ENDIF                                                  SCCHRG = *ZEROS
     C*
     C* IF ADJUDICATION CODE IS AN OVERRIDE TYPE AND THE OVERRIDE
     C* PRECENTAGE IS GREATER THAN ZEROS, CLEAR ALL REDUCTIONS AMOUNT
     C* FIELDS AND PAID AMONTE FIELD.  THE ADJUDICATION PROGRAM
     C* REQUIRES THIS AMOUNTS TO BE ZEROS IN ORDER TO CALCULATE
     C* THE PERCENTAGE
     C*
     C     *IN99         IFEQ      *OFF
     C     CSOVRD        ANDEQ     'Y'
     C     CSOVRP        ANDGT     *ZEROS
     C                   Z-ADD     0             SCMRA
     C                   Z-ADD     0             SCOTHR
     C                   Z-ADD     0             SCOTH2
     C                   Z-ADD     0             SCNTWK
     C                   Z-ADD     0             SCPAID
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C* GIVE ERROR IF THE ADJUDICATION CODE IS AN OVERRIDE TYPE,
     C* OVERRIDE PERCENT IS ZERO, A REDUCTION IS APPLIED AND
     C* NO EOB CODE EXISTS FOR THE LINE ITEM
     C*
     C*
     C* 03/30/99 -  CALL EOB CODES/NOTES PROGRAM NOW INSTEAD OF GIVING
     C*             AN ERROR
     C                   MOVE      *BLANKS       XXEOB
     C     *IN99         IFEQ      *OFF
     C     CSOVRD        ANDEQ     'Y'
     C     CSOVRP        ANDLT     100
     C*
     C                   MOVEL(P)  '*YES'        GO                4
     C                   Z-ADD     EORCN         KKRCN
     C                   Z-ADD     SCSEQ#        KKDSEQ
     C     DEKY2         CHAIN     PDE03                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C     DEESRC        IFEQ      'U'
     C                   MOVEL     *BLANKS       GO
     C                   LEAVE
     C                   ENDIF
     C*
     C     DEKY2         READE     PDE03                                  99
     C                   ENDDO
     C*
     C     GO            IFEQ      '*YES'
     C                   Z-ADD     @@RRN2        ##RRN2
     C                   Z-ADD     @@RRN3        ##RRN3
     C*
     C* IF IN ADD MODE, NEED TO SEND IN SCREEN DSEQ NUMBER TO BE
     C* WRITTEN IN EOB CODES/NOTES FILE.  THIS IS DONE BECAUSE THIS
     C* DETAIL CHARGE IS NOT WRITTEN YET. ALSE SEND ZERO FOR THE
     C* RELATIVE RECORD NUMBER OF FDCAP (##RRN3).
     C*
     C     @@MODE        IFEQ      'A'
     C                   Z-ADD     SCSEQ#        ##DSEQ            5 0
     C                   Z-ADD     0             ##RRN3
     C                   Z-ADD     SCSEQ#        KKDSEQ
     C                   ELSE
     C                   Z-ADD     0             ##DSEQ
     C                   ENDIF                                                  @@MODE = 'A'
     C*
     C*** REMOVE ANY EOB CODES AND MESSAGES PRODUCED BY ADJUDICATION
     C*   OR NETWORK PROCESSING
     C*
     C     DEKY          CHAIN     PDE01                              99
     C     *IN99         DOWEQ     *OFF
     C     DEESRC        IFEQ      'A'
     C**** Beg Chg *** 07/18/23 **************************************
     C**** DEEOB         ANDNE     *BLANKS
     C     DEEOBCD       ANDNE     *BLANKS
     C**** End Chg *** 07/18/23 **************************************
     C     DEESRC        OREQ      'N'
     C**** Beg Chg *** 07/18/23 **************************************
     C**** DEEOB         ANDNE     *BLANKS
     C     DEEOBCD       ANDNE     *BLANKS
     C**** End Chg *** 07/18/23 **************************************
     C     DERCN         IFEQ      EORCN
     C                   DELETE    PDE01
     C                   ELSE
     C                   EXCEPT    RPDE01
     C                   ENDIF
     C                   ELSE
     C                   EXCEPT    RPDE01
     C                   ENDIF                                                  DEESRC = 'A'
     C     DEKY          READE     PDE01                                  99
     C                   ENDDO
     C*
     C     MGKY          CHAIN     PMG01                              99
     C     *IN99         DOWEQ     *OFF
     C     MGSRC         IFEQ      'S'
     C     MGSRC         OREQ      'N'
     C     MGMSG#        IFNE      'NW00001'
     C                   DELETE    PMG01
     C                   ELSE
     C*
     C                   UPDATE    PMG01
     C*
     C                   ENDIF
     C                   ELSE
     C                   EXCEPT    RPMG01
     C                   ENDIF                                                  MGSRC = 'S'
     C     MGKY          READE     PMG01                                  99
     C                   ENDDO
     C*
     C                   CALL      'F0356R01'
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      'A'           ##MODE
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM                    ##RRN2
     C                   PARM                    ##RRN3
     C                   PARM                    ##DSEQ
     C                   PARM      SCCHRG        ##CHRG
     C                   PARM      SCPAID        ##PAID
     C*
     C     ##KL          IFEQ      '1'
     C                   SETON                                        9014
     C                   SETOFF                                       69
     C                   ELSE
     C                   SETOFF                                       14
     C                   ENDIF
     C     SCMRA         IFNE      *ZEROS
     C     SCNTWK        ORNE      *ZEROS
     C     SCOTHR        ORNE      *ZEROS
     C     SCOTH2        ORNE      *ZEROS
     C*
     C                   Z-ADD     EORCN         KKRCN
     C                   Z-ADD     SCSEQ#        KKDSEQ
     C                   SETON                                        90
     C     DEKY2         CHAIN     PDE03                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C     DEESRC        IFEQ      'U'
     C                   SETOFF                                       90
     C                   LEAVE
     C                   ENDIF
     C*
     C     DEKY2         READE     PDE03                                  99
     C                   ENDDO
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ENDIF                                                  GO = *YES
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C*
     C* IF IN CHANGE MODE, THEN SET 'OUTFLG' TO END PROGRAM
     C*
     C     @@MODE        IFEQ      'C'
     C                   MOVE      '1'           OUTFLG
     C                   ENDIF
     C*
     C     CK1END        ENDSR                                                  SRERR IFEQ 1
      *
      *
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
      *
      *
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
      *
      *
     C*================================================================
     C* KADO - F01=REPEAT
     C*================================================================
     C     KADO          BEGSR
     C*
     C     SCLIN#        IFGT      0
     C     SCLIN#        SUB       1             KKLIN#
     C     CAKY4         CHAIN     PCA04                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     CADSEQ        KKDSEQ
     C     DUKY          CHAIN     PDU01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C* POSITION CURSOR TO DATE OF SERVICE
     C*
     C                   SETON                                        33
     C*
     C* LOAD REPEAT VALUES TO SCREEN
     C*
     C                   MOVEL     CACSCD        SCCSCD
     C                   MOVEL(P)  SCCSCD        KKKEY
     C     TBKY          CHAIN     PCS01                              99
     C  N99              MOVEL(P)  CSDESC        SCCSDS
     C*
     C                   Z-ADD     CAFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCFDAT
     C*
     C                   Z-ADD     CACHRG        SCCHRG
     C                   MOVEL     CACSCD        SCCSCD
     C                   MOVEL(P)  SCCSCD        KKKEY
     C     TBKY          CHAIN     PCS01                              99
     C  N99              MOVEL(P)  CSDESC        SCCSDS
     C*
     C*
     C                   MOVE      DUPOS         SCPOS
     C                   MOVE(P)   SCPOS         KKPOS
     C     PSKY          CHAIN     PPS01                              99
     C  N99              MOVEL(P)  PSDESC        SCPODS
     C*
     C                   Z-ADD     DURVCD        SCRVCD
     C                   MOVEL(P)  DURATE        SCHCPC
     C                   Z-ADD     1             SCUNTS
     C*
     C                   SETON                                        90
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  SCLIN# GT 0
     C                   ENDSR
     C*================================================================
     C* KBDO - F2=PROCESS
     C*================================================================
     C     KBDO          BEGSR
     C*
     C* IF USER HITS F2 IN ADD MODE ON LIN#1, THEN CALL SUBROUTINES
     C* ERRCK1 AND WRITER BEFOR ADJUDICATING
     C*
     C     @@MODE        IFEQ      'A'
     C     SCLIN#        ANDEQ     1
     C                   SETOFF                                       KB
B02  C     *IN90         DOUEQ     *OFF
     C                   MOVEA     '00000'       *IN(71)
     C                   MOVEA     '00000'       *IN(76)
     C                   MOVEA     '00000'       *IN(81)
     C                   MOVEA     '00000'       *IN(86)
B03  C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C     *INKE         CASEQ     *ON           KEDO
     C                   CAS                     OTHFKS
     C                   ENDCS
B04  C     *IN90         IFEQ      *OFF
     C     @@MODE        ANDNE     'D'
     C                   EXSR      ERRCK1
E04  C                   ENDIF
B04  C     *IN90         IFEQ      *OFF
B05  C     #OFSCR        IFEQ      1
     C                   EXSR      FINISH
X05  C                   ELSE
     C     @@MODE        CASEQ     'C'           UPDATR
     C     @@MODE        CASEQ     'D'           DSPLYR
     C                   CAS                     OTHERR
     C                   ENDCS
     C                   EXSR      XSCR02
E05  C                   ENDIF
E04  C                   ENDIF
E03  C                   ENDIF
     C   90              EXFMT     SCR01
E02  C                   ENDDO
     C                   ENDIF
     C*
     C                   SETON                                        LR
     C                   RETURN
     C     KBEND         ENDSR
     C*
     C*================================================================
     C* KGDO - F7=HEADER INFO
     C*================================================================
     C     KGDO          BEGSR
     C     BEKY          CHAIN     PBE01                              99
     C     *IN99         IFEQ      *OFF
     C     BEHESM        ANDNE     *BLANK
     C                   Z-ADD     @@RRN2        ##RRN
     C                   MOVEL(P)  BEHESM        PGMNAM           10
     C                   CALL      PGMNAM
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRN
     C                   ENDIF                                                  *IN99 = Off
     C                   SETON                                        90
     C     KGEND         ENDSR
     C*================================================================
     C* KIDO - F9=CODE INFO
     C*================================================================
     C     KIDO          BEGSR
     C                   MOVE      EOAST         ##ST              2
     C                   MOVE      SCRVCD        K1RVCD
     C                   MOVE      SCHCPC        K1HCPC
     C                   MOVE      *ZEROS        K1UNTS
     C                   Z-ADD     SCFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         K1EFDT
     C                   SETOFF                                       57
     C     EUICN         IFEQ      'IP'
     C     EUICN         OREQ      'IM'
     C     EUICN         OREQ      'IS'
     C                   SETON                                        57
     C                   ENDIF
     C*
     C                   MOVE      '1'           SCPCF
     C                   MOVE      *BLANK        SCSCF
     C                   MOVE      *BLANK        SCDRI
     C     WDW           TAG
     C*** Medicare APC file must be shown when hospital bill is outpatient
     C** *in41 for display of medicare APC file
     C                   EXFMT     WDW01
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C                   SETOFF                                         66
     C*
     C     SCPCF         IFEQ      '1'
     C                   SETON                                          66
     C                   ENDIF
     C*
     C     SCDRI         IFEQ      '1'
     C     *IN66         IFEQ      *ON
     C                   SETON                                          71
     C                   GOTO      WDW
     C                   ENDIF                                                  *IN80 EQ *ON
     C                   SETON                                          66
     C                   ENDIF                                                  SCSCF EQ '1'
     C*
     C     SCSCF         IFEQ      '1'
     C     *IN66         IFEQ      *ON
     C                   SETON                                          71
     C                   GOTO      WDW
     C                   ENDIF                                                  *IN80 EQ *ON
     C                   SETON                                          66
     C                   ENDIF                                                  SCSCF EQ '1'
     C*
     C                   SETOFF                                         66
     C*
     C                   SELECT
     C     SCSCF         WHENNE    '1'
     C     SCSCF         ANDNE     *BLANK
     C                   SETON                                          72
     C                   GOTO      WDW
     C*
     C     SCDRI         WHENNE    '1'
     C     SCDRI         ANDNE     *BLANK
     C                   SETON                                          45
     C                   GOTO      WDW
     C*
     C     SCPCF         WHENNE    '1'
     C     SCPCF         ANDNE     *BLANK
     C                   SETON                                          73
     C                   GOTO      WDW
     C*
     C                   ENDSL
     C*
     C                   SELECT
     C     SCSCF         WHENEQ    '1'
     C*
     C     SVKY1         SETGT     PSV01
     C     SVKY2         READPE    PSV01                                  99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRN
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     SVRRN         ##RRN
     C                   ENDIF                                                  *IN99 = On
     C                   CALL      'F2320R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRN
     C                   PARM                    ##ST
     C*
     C     SCPCF         WHENEQ    '1'
     C*
     C                   Z-ADD     CAFDAT        KKFDAT
     C                   MOVEL     DURATE        KKRATE
     C*
     C                   MOVEL(P)  EOFLIC        KKZIP
     C                   SELECT
     C     SCHCPC        WHENGE    '00100'
     C     SCHCPC        ANDLE     '01999'
     C                   MOVEL(P)  XXANES        KKCAT
     C     SCHCPC        WHENGE    'A0000'
     C     SCHCPC        ANDLE     'V9999'
     C                   MOVEL(P)  XXHCPC        KKCAT
     C                   OTHER
     C                   MOVEL(P)  XXPROF        KKCAT
     C                   ENDSL
     C     EUICN         IFEQ      'AO'
     C     EUICN         OREQ      'AS'
     C                   MOVEL(P)  'ASTC'        KKCAT
     C                   ENDIF
     C     EUICN         IFEQ      'HO'
     C     EUICN         OREQ      'HS'
     C                   MOVEL(P)  XXOUTP        KKCAT
     C                   ENDIF
     C*
     C*** BEG ADD *** 02/21/12 ************************************************
     C     KKFDAT        IFLT      1120101
     C*** END ADD *** 02/21/12 ************************************************
     C     FSKY          SETGT     PFS03
     C     FSKY1         READPE    PFS03                                  99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRN
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     PRRRN         ##RRN
     C                   ENDIF                                                  *IN99 = On
     C*
     C                   CALL      'F2544R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRN
     C*** BEG ADD *** 02/21/12 ************************************************
     C                   ELSE
     C*
     C                   SELECT
     C     SCHCPC        WHENGE    '00100'
     C     SCHCPC        ANDLE     '01999'
     C                   MOVEL(P)  XXANES        KKCAT
     C     SCHCPC        WHENGE    'A0000'
     C     SCHCPC        ANDLE     'V9999'
     C                   MOVEL(P)  XXHCPC        KKCAT
     C     SCHCPC        WHENGE    '    1'
     C     SCHCPC        ANDLE     '  989'
     C                   MOVEL(P)  XXSTA1        KKCAT
     C                   OTHER
     C                   MOVEL(P)  XXPROF        KKCAT
     C                   ENDSL
     C*
     C     EUICN         IFEQ      'AO'
     C     EUICN         OREQ      'AS'
     C                   MOVEL(P)  'ASTC'        KKCAT
     C                   ENDIF
     C     EUICN         IFEQ      'HO'
     C     EUICN         OREQ      'HS'
     C                   MOVEL(P)  XXOUTP        KKCAT
     C                   ENDIF
     C*
     C                   MOVE      EORGN#        KKRGN#
     C                   MOVE      SCHCPC        KKPRCD
     C     F2KY          SETGT     PF103
     C     F2KY1         READPE    PF103                                  99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRN
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     P1RRN         ##RRN
     C                   ENDIF                                                  *IN99 = On
     C*
     C                   CALL      'F2547R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRN
     C*
     C                   ENDIF
     C*** END ADD *** 02/21/12 ************************************************
     C*
     C     SCDRI         WHENEQ    '1'
     C*
     C                   Z-ADD     CAFDAT        KKFDAT
     C*** BEG CHG *** 05/10/11 ****************************************
     C******               MOVE *ALL'0'   KKRATE
     C                   MOVE      *BLANKS       KKRATE
     C*** END CHG *** 05/10/11 ****************************************
     C*** BEG CHG 05/10/11 *****************************************************
     C************         MOVE EUDRG#    KKRATE
     C                   MOVE      EUDRG#        #PRCPT
     C*
     C     PRCPT1        IFEQ      '0000'
     C                   MOVE      *BLANKS       PRCPT1
     C                   ELSE
     C     PRCPT2        IFEQ      '000'
     C                   MOVE      *BLANKS       PRCPT2
     C                   ELSE
     C     PRCPT3        IFEQ      '00'
     C                   MOVE      *BLANKS       PRCPT3
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   MOVE      #PRCPT        KKRATE
     C*** END CHG 05/10/11 *****************************************************
     C*
     C                   MOVEL(P)  EOFLIC        KKZIP
     C                   SELECT
     C     EOSPCD        WHENEQ    'TC'
     C                   MOVEL(P)  XXTRAU        KKCAT
     C                   OTHER
     C                   MOVEL(P)  XXSTAN        KKCAT
     C                   ENDSL
     C*
     C*** BEG ADD *** 02/21/12 ************************************************
     C     KKFDAT        IFLT      1120101
     C*** END ADD *** 02/21/12 ************************************************
     C     FSKY          SETGT     PFS03
     C     FSKY1         READPE    PFS03                                  99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRN
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     PRRRN         ##RRN
     C                   ENDIF                                                  *IN99 = On
     C*
     C                   CALL      'F2544R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRN
     C*** BEG ADD *** 02/21/12 ************************************************
     C                   ELSE
     C*
     C                   SELECT
     C     EOSPCD        WHENEQ    'TC'
     C                   MOVEL(P)  XXTRA1        KKCAT
     C                   OTHER
     C                   MOVEL(P)  XXSTA1        KKCAT
     C                   ENDSL
     C*
     C                   MOVE      EORGN#        KKRGN#
     C                   MOVE      SCHCPC        KKPRCD
     C     F1KY2         SETGT     PF102
     C     F1KY3         READPE    PF102                                  99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRN
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     P1RRN         ##RRN
     C                   ENDIF                                                  *IN99 = On
     C*
     C                   CALL      'F2547R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRN
     C*
     C                   ENDIF
     C*** END ADD *** 02/21/12 ************************************************
     C*
     C                   ENDSL
     C                   ENDIF
     C                   MOVEA     '0000'        *IN(61)
     C                   MOVEA     '00000'       *IN(71)
     C                   SETON                                        90
     C     KIEND         ENDSR
     C*================================================================
     C* KJDO - F10=SEARCH.
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C*
     C*  ADJUDICATION CODE SEARCH
     C*
     C     HFLD          WHENEQ    'SCCSCD'
     C     *IN44         ANDNE     *ON
     C*
     C                   CALL      'F2300R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C                   PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN
     C*
     C     ##KL          IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        9031      pos cursor
     C                   ENDIF                                                  ##KL IFEQ *ON
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PCS02                              99
     C                   MOVEL(P)  CSKEY         SCCSCD
     C                   MOVEL(P)  CSDESC        SCCSDS
     C                   SETON                                        9032      pos cursor
     C                   ENDIF                                                  ##RRN IFNE ZEROS
     C*
     C* PLACE OF SERVICE SEARCH
     C*
     C     HFLD          WHENEQ    'SCPOS'
     C                   MOVE      EOAST         ##AST
     C                   MOVE      *BLANKS       ##POS
     C                   MOVEL     SCPOS         HLPOS            15
     C                   CALL      'F2370R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##AST             2
     C                   PARM                    ##POS            15
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##POS         OREQ      *BLANKS
     C                   MOVEL(P)  HLPOS         SCPOS
     C                   SETON                                        9032      pos cursor
     C                   ELSE
     C                   MOVEL     ##POS         TPPOS             3
     C                   MOVE      TPPOS         SCPOS
     C                   MOVE(P)   SCPOS         KKPOS
     C     PSKY          CHAIN     PPS01                              99
     C  N99              MOVEL(P)  PSDESC        SCPODS
     C                   SETON                                        9033      pos cursor
     C                   ENDIF                                                  ##KC IFEQ 1
     C*
     C* REVENUE CODE SEARCH
     C*
     C     HFLD          WHENEQ    'SCRVCD'
     C                   MOVEL     SCRVCD        HLRVCD            3 0
     C                   MOVEL     SCHCPC        HLHCPC            5
     C                   MOVEL     SCCDDS        HLCDDS           40
     C                   MOVE      EOAST         ##ST              2
     C                   MOVE      *BLANKS       ##CMSV           15
     C                   MOVE      *BLANKS       ##CDSC           40
     C     SCRVCD        IFNE      *ZEROS
     C*
     C                   MOVEL(P)  SCRVCD        ##CMSV
     C*
     C                   ENDIF
     C     TMCMSV        IFNE      *BLANKS
     C                   MOVEL(P)  SCRVCD        TMSVCD
     C                   MOVEL(P)  TMSMSA        ##CMSV
     C                   ENDIF
     C                   CALL      'F2320R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##ST
     C                   PARM                    ##CMSV
     C                   PARM                    ##CDSC
     C*
     C     ##KL          IFEQ      '1'
     C     ##CMSV        OREQ      *BLANKS
     C                   MOVEL     HLRVCD        SCRVCD
     C                   MOVEL     HLHCPC        SCHCPC
     C                   MOVEL(P)  HLCDDS        SCCDDS
     C                   SETON                                        9034
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##CMSV        IFNE      *BLANKS
     C                   MOVEL     ##CMSV        SCRVCD
     C*******    5         SUBST##CMSV:4  SCHCPC
     C                   MOVEL(P)  ##CDSC        SCCDDS
     C                   SETON                                        9036
     C                   ENDIF                                                  ##CMSV NE *BLANKS
     C*
     C* HCPCS/CPT4 CODE SEARCH
     C*
     C*
     C     HFLD          WHENEQ    'SCHCPC'
     C*
     C*** BEG DEL *** 08/11/10 ****************************************
     C**********           MOVE EOAST     ##ST    2
     C**********           Z-ADDEORRN     ##RRN4  90
     C**********           MOVE *BLANKS   ##CMSV
     C********** TMCMSV    IFNE *BLANKS
     C**********           MOVE *BLANKS   TMSVCD
     C**********           MOVELTMSMSA    ##CMSV    P
     C**********           ENDIF
     C**********           CALL 'F2016R'
     C**********           PARM '1'       ##MODE
     C**********           PARM ' '       ##KC
     C**********           PARM ' '       ##KL
     C**********           PARM           ##CMSV
     C**********           PARM           ##CDSC
     C**********           PARM ' '       @@RTCD  1
     C**********           PARM           ##ST
     C**********           PARM           ##RRN4           *** FAEOP ***
     C*
     C********** ##CMSV    IFNE *BLANKS
     C*
     C* If selection was from the procedure code file don't pop rev cod
     C*
     C********** @@RTCD    IFNE 'C'
     C********** @@RTCD    ANDNE'H'
     C********** @@RTCD    ANDNE'1'
     C**********           MOVEL##CMSV    SCRVCD
     C********** 5         SUBST##CMSV:4  SCHCPC
     C**********           MOVEL##CDSC    SCCDDS    P
     C**********           SETON                     9036
     C**********           ENDIF
     C*
     C********** @@RTCD    IFEQ 'C'
     C********** 5         SUBST##CMSV:4  SCHCPC
     C**********           MOVEL##CDSC    SCCDDS    P
     C**********           SETON                     9036
     C**********           ELSE
     C********** @@RTCD    IFEQ 'H'
     C**********           MOVEL##CMSV    SCHCPC
     C**********           MOVEL##CDSC    SCCDDS    P
     C**********           SETON                     9036
     C**********           ENDIF                           @@RTCD = 'H'
     C**********           ENDIF                           @@RTCD = 'C'
     C**********           ELSE
     C**********           SETON                     9035
     C**********           ENDIF                           ##CMSV NE *BLANKS
     C*** END DEL *** 08/11/10 ****************************************
     C*** BEG ADD *** 08/11/10 ****************************************
     C                   MOVE      SCHCPC        HLHCPC
     C                   MOVEL(P)  SCCDDS        HLCDDS
     C                   MOVEL(P)  EOFLIC        @@ZIP
     C*** BEG ADD *** 02/21/12 *****************************
     C                   MOVEL     '1'           SCRGN
     C                   MOVEL     *BLANK        SCZIP
     C                   MOVEL     *BLANK        XXFLG             1
     C                   MOVEA     '000'         *IN(17)
     C*
     C     XXFLG         DOWEQ     *BLANK
     C     *INKL         ANDEQ     *OFF
     C*
     C                   EXFMT     WDW02
     C                   MOVEA     '000'         *IN(17)
     C*
     C     *INKL         IFEQ      *OFF
     C                   SELECT
     C*
     C     SCRGN         WHENEQ    '1'
     C     SCZIP         ANDEQ     '1'
     C                   MOVE      *ON           *IN17                          SELECTED TWO
     C*
     C     SCRGN         WHENNE    '1'
     C     SCRGN         ANDNE     ' '
     C                   MOVE      *ON           *IN18                          INVALID CODE
     C*
     C     SCZIP         WHENNE    '1'
     C     SCZIP         ANDNE     ' '
     C                   MOVE      *ON           *IN19                          INVALID CODE
     C*
     C     SCRGN         WHENEQ    '1'
     C*
     C                   CALL      'F2547R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      '2'           ##VIEW            1
     C                   PARM      EORGN#        ##RGN             3 0
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVEL     HLHCPC        SCHCPC
     C                   MOVEL(P)  HLCDDS        SCCDDS
     C                   SETON                                        9054      pos cursor
     C                   ELSE
     C     ##RRN         CHAIN     PF101                              99
     C  N99              MOVEL     F1CODE        SCHCPC
     C  N99              MOVEL     F1DSC1        SCCDDS
     C                   SETON                                        9097
     C                   ENDIF
     C                   MOVEL     '1'           XXFLG
     C                   MOVEA     '000'         *IN(17)
     C*
     C     SCZIP         WHENEQ    '1'
     C                   MOVE      *ZEROS        ##RRN
     C*
     C*** END ADD *** 02/21/12 *****************************
     C                   CALL      'F2544R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM      '2'           ##VIEW            1
     C                   PARM      @@ZIP         ##ZIP             3
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVEL     HLHCPC        SCHCPC
     C                   MOVEL(P)  HLCDDS        SCCDDS
     C                   SETON                                        9054      pos cursor
     C                   ELSE
     C     ##RRN         CHAIN     PFS02                              99
     C  N99              MOVEL     FSCODE        SCHCPC
     C  N99              MOVEL     FSDSC1        SCCDDS
     C                   SETON                                        9097
     C                   ENDIF
     C*** BEG ADD *** 02/21/12 *****************************
     C                   MOVEL     '000'         *IN(17)
     C                   MOVEL     '1'           XXFLG
     C*
     C                   ENDSL
     C*
     C                   ELSE
     C                   MOVE      *ON           *IN54
     C                   ENDIF
     C                   ENDDO
     C*** END ADD *** 02/21/12 *****************************
     C*** END ADD *** 08/11/10 ****************************************
      *
      *  Procedure Modifier
      *
     C     HFLD          WHENEQ    'SCPMOD'
     C                   MOVE      EOAST         #2AST
      *
     C                   CALL      'F2315R'
     C                   PARM      '2'           #2CALL            1
     C                   PARM      *BLANK        #2KC              1
     C                   PARM      *BLANK        #2KL              1
     C                   PARM                    #2AST             2
     C                   PARM      *BLANKS       #2KEY            15
      *
     C     #2KEY         IFEQ      *BLANKS
     C                   SETON                                        9005      pos cursor
      *
     C                   ELSE
      *
     C                   MOVEL     #2KEY         SCPMOD
     C                   SETON                                        9024      pos cursor
     C                   ENDIF
     C*
     C*
      *
      *  Procedure Modifier
      *
     C     HFLD          WHENEQ    'SCMOD2'
     C                   MOVE      EOAST         #2AST
      *
     C                   CALL      'F2315R'
     C                   PARM      '2'           #2CALL            1
     C                   PARM      *BLANK        #2KC              1
     C                   PARM      *BLANK        #2KL              1
     C                   PARM                    #2AST             2
     C                   PARM      *BLANKS       #2KEY            15
      *
     C     #2KEY         IFEQ      *BLANKS
     C                   SETON                                        9024      pos cursor
      *
     C                   ELSE
      *
     C                   MOVEL     #2KEY         SCMOD2
     C                   SETON                                        9006      pos cursor
     C                   ENDIF
     C*
     C* REDUCTIONS TYPE CODE SEARCHES
     C*
     C     HFLD          WHENEQ    'SCOT1C'
     C                   MOVEL(P)  SCOT1C        HLOT1C
     C                   CALL      'F2435R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      'O'           ##TYPE            1
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVEL(P)  HLOT1C        SCOT1C
     C                   SETON                                        9048
     C                   ENDIF                                                  ##KC = 1
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PBS02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  BSCODE        SCOT1C
     C                   SETON                                        9049
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRB NE *ZEROS
     C*
     C     HFLD          WHENEQ    'SCOT2C'
     C                   MOVEL(P)  SCOT2C        HLOT2C
     C                   CALL      'F2435R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      'O'           ##TYPE            1
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVEL(P)  HLOT2C        SCOT2C
     C                   SETON                                        9050
     C                   ENDIF                                                  ##KC = 1
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PBS02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  BSCODE        SCOT2C
     C                   SETON                                        9051
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C                   OTHER
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCFDAT'
     C                   SETOFF                                       37
     C                   SETON                                        383940
     C     HFLD          WHENEQ    'SCCDDS'
     C                   SETOFF                                       383940
     C                   SETON                                        37
     C     HFLD          WHENEQ    'SCUNTS'
     C                   SETOFF                                       373940
     C                   SETON                                        38
     C     HFLD          WHENEQ    'SCCHRG'
     C                   SETOFF                                       373840
     C                   SETON                                        39
     C     HFLD          WHENEQ    'SCPAID'
     C                   SETOFF                                       373839
     C                   SETON                                        40
     C     HFLD          WHENEQ    'SCMRA'
     C                   SETOFF                                       3940
     C                   SETON                                        3738
     C     HFLD          WHENEQ    'SCNTWK'
     C                   SETOFF                                       3740
     C                   SETON                                        3839
     C     HFLD          WHENEQ    'SCOTHR'
     C                   SETOFF                                       3738
     C                   SETON                                        3940
     C     HFLD          WHENEQ    'SCOTH2'
     C                   SETOFF                                       3839
     C                   SETON                                        3740
     C                   ENDSL
     C*
     C                   SETON                                        90
     C*
     C                   ENDSL
     C*
     C                   EXSR      DATSBR
     C                   SETON                                        70
     C*
     C                   ENDSR
     C*================================================================
     C* KKDO - F11=EOB/Notes
     C*================================================================
     C     KKDO          BEGSR
     C                   Z-ADD     0             ##RRN
     C                   Z-ADD     @@RRN2        ##RRN2            9 0          faeop
     C                   Z-ADD     @@RRN3        ##RRN3            9 0
     C*
     C     @@MODE        IFEQ      'C'
     C     @@MODE        OREQ      'A'
     C                   MOVE      ' '           ##CALL
     C                   ELSE
     C                   MOVE      '1'           ##CALL
     C                   ENDIF                                                  @@MODE = 'C'
     C*
     C                   CALL      'F0356R'
     C                   PARM                    ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##RRN
     C                   PARM                    ##RRN2
     C                   PARM                    ##RRN3
     C                   SETON                                        90
     C                   ENDSR
     C*================================================================
     C* KNDO - F14=DARTS
     C*================================================================
     C     KNDO          BEGSR
     C*
     C* CALL API TO ACCESS DARTS
     C*
     C                   MOVE      EOCO#         #1CO#
     C                   MOVE      EOSUB#        #1SUB#
     C                   MOVE      EOFND         #1FND
     C                   MOVE      EOFYR         #1FYR
     C                   MOVE      EOCASE        #1CASE                   90
     C*
     C                   CALL      'FA703R'
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM                    ##WCIN            1
     C                   PARM                    ##WCDK            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE
     C                   PARM      *BLANK        #1KEY            40
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* KPDO - F15=DISPLAY LINES
     C*================================================================
     C     KPDO          BEGSR
     C                   Z-ADD     @@RRN2        ##RRN
     C                   CALL      'F0354R'
     C                   PARM      '1'           ##CALL
     C                   PARM                    ##RRN                          faeop
     C                   PARM      SCLIN#        ##LIN#
     C                   SETON                                        90
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   CLEAR                   SCRVCD
     C                   Z-ADD     0             SCEOB#
     C                   Z-ADD     0             SCLIN#
     C                   Z-ADD     0             SCSEQ#
     C                   MOVE      *BLANKS       SCCSCD
     C                   MOVE      *BLANKS       SCCSDS
     C                   Z-ADD     0             SCFDAT
     C                   Z-ADD     0             SCTDAT
     C                   MOVE      *ZEROS        SCPOS
     C                   MOVE      *BLANKS       SCPODS
     C                   MOVE      *BLANKS       SCHCPC
     C                   MOVE      *BLANKS       SCPMOD
     C                   MOVE      *BLANKS       SCMOD2
     C                   MOVE      *BLANKS       SCCDDS
     C                   Z-ADD     0             SCCHRG
     C                   Z-ADD     0             SCPAID
     C                   Z-ADD     0             SCMRA
     C                   Z-ADD     0             SCNTWK
     C                   Z-ADD     0             SCOTHR
     C                   MOVE      *BLANKS       SCOT1C
     C                   Z-ADD     0             SCOTH2
     C                   MOVE      *BLANKS       SCOT2C
     C                   CLEAR                   SCEOB1
     C                   CLEAR                   SCEOB2
     C                   CLEAR                   SCEOB3
     C                   CLEAR                   SCEOB4
     C*
     C                   MOVE      EOAST         SCAST
     C                   Z-ADD     EOEOB#        SCEOB#
     C*
     C* SET LINE NUMBER
     C*
     C     @@MODE        IFEQ      'A'
     C                   Z-ADD     *HIVAL        KKLIN#
     C     CAKY4         SETGT     PCA04
     C     CAKY3         READPE    PCA04                                  99
     C     *IN99         IFEQ      *OFF
     C     CALIN#        ADD       1             SCLIN#
     C                   ELSE
     C                   Z-ADD     1             SCLIN#
     C                   ENDIF
     C                   Z-ADD     1             SCUNTS
     C*
     C* SET DATE ENTERED
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SCEDAT
     C*
     C* DEFAULT CLAIMS STATUS CODE FROM FAEOP
     C*
     C                   MOVEL     EOCSCD        SCCSCD
     C   44              MOVEL     '300'         SCCSCD
     C                   MOVEL(P)  SCCSCD        KKKEY
     C     TBKY          CHAIN     PCS01                              99
     C  N99              MOVEL(P)  CSDESC        SCCSDS
     C*
     C* FROM FAEHP
     C*
     C     @@MODE        IFEQ      'A'                                          ADD
     C*
     C* DETERMINE DETAIL CHARGE SEQUENCE NUMBER
     C*
     C                   Z-ADD     *HIVAL        KKDSEQ
     C*
     C     CAKY          SETGT     PCA01
     C     CAKY1         READPE    PCA01                                  99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     1             SCSEQ#
     C                   ELSE
     C     CADSEQ        ADD       1             SCSEQ#
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C*
     C                   ENDIF                                                  @@MODE IFEQ 'A'
     C*
     C*
     C                   ENDIF                                                  @@MODE = 'A'
     C     EUKY          CHAIN(N)  PEU01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL     *BLANKS       EUEDI
     C                   ELSE
     C     ##CLT#        IFEQ      '025'
     C     ##CLT#        OREQ      '144'
     C*
     C                   Z-ADD     EUSCPF        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCFDAT
     C*
     C                   Z-ADD     EUSCPT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTDAT
     C*
     C                   ENDIF
     C                   ENDIF
     C     @@MODE        IFNE      'A'
     C*
     C* FROM FDDUP
     C*
     C                   MOVE(P)   DUPOS         SCPOS
     C                   MOVE(P)   SCPOS         KKPOS
     C     PSKY          CHAIN     PPS01                              99
     C  N99              MOVEL(P)  PSDESC        SCPODS
     C*
     C                   Z-ADD     DURVCD        SCRVCD
     C                   MOVEL     DURATE        SCHCPC
     C                   Z-ADD     DUUNTS        SCUNTS
     C                   MOVEL     DUDESC        SCCDDS
     C                   MOVE      DUPMOD        SCPMOD
     C                   MOVE      DUMOD2        SCMOD2
     C*
     C* GET INFO FROM FDCAP
     C*
     C     @@RRN3        CHAIN(N)  PCA02                              99
     C     *IN99         IFEQ      *OFF
     C*
     C* DEFAULT CLAIM STATUS CODE TO 400 IF THIS IS A RECON AND
     C* TRYING TO CHANGE A LINE ITEM ALREADY IN HISTORY (46 = ON)
     C*
     C     *IN46         IFEQ      *ON
     C                   MOVEL     '400'         SCCSCD
     C                   ELSE
     C                   MOVEL     CACSCD        SCCSCD
     C                   ENDIF
     C*
     C                   MOVEL(P)  SCCSCD        KKKEY
     C     TBKY          CHAIN     PCS01                              99
     C  N99              MOVEL(P)  CSDESC        SCCSDS
     C*
     C                   Z-ADD     CAFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCFDAT
      *
     C                   Z-ADD     CATDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTDAT
      *
     C                   Z-ADD     CAIDT         SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEDAT
     C*
     C                   Z-ADD     CACHRG        SCCHRG
     C                   Z-ADD     CAOTHR        SCOTHR
     C                   Z-ADD     CAOTH2        SCOTH2
     C                   Z-ADD     CANDIS        SCNTWK
     C                   Z-ADD     CAMRA         SCMRA
     C                   Z-ADD     CAPAID        SCPAID
     C                   MOVE      CAOT1C        SCOT1C
     C                   MOVE      CAOT2C        SCOT2C
     C                   Z-ADD     CALIN#        SCLIN#
     C                   Z-ADD     CADSEQ        SCSEQ#
     C                   ENDIF                                                  *IN99 IFEQ *OFF
     C*
     C* DISPLAY EOB CODES FOR BILL
     C*
     C     @@MODE        IFNE      'A'
     C                   Z-ADD     0             XXSEQ#            2 0
     C     DEKY          CHAIN(N)  PDE01                              99
     C     *IN99         DOWEQ     *OFF
     C**** Beg Chg *** 07/18/23 **************************************
     C**** DEEOB         IFNE      *BLANKS
     C     DEEOBCD       IFNE      *BLANKS
     C**** End Chg *** 07/18/23 **************************************
     C                   ADD       1             XXSEQ#
     C**** Beg Del *** 07/18/23 **************************************
     C****               SELECT
     C**** XXSEQ#        WHENEQ    1
     C****               MOVEL     DEEOB         SCEOB1
     C**** XXSEQ#        WHENEQ    2
     C****               MOVEL     DEEOB         SCEOB2
     C**** XXSEQ#        WHENEQ    3
     C****               MOVEL     DEEOB         SCEOB3
     C**** XXSEQ#        WHENEQ    4
     C****               MOVEL     DEEOB         SCEOB4
     C****               ENDSL
     C**** End Del *** 07/18/23 **************************************
     C**** Beg Add *** 07/18/23 **************************************
     C                   SELECT
     C     XXSEQ#        WHENEQ    1
     C                   MOVEL     DEEOBCD       SCEOB1
     C     XXSEQ#        WHENEQ    2
     C                   MOVEL     DEEOBCD       SCEOB2
     C     XXSEQ#        WHENEQ    3
     C                   MOVEL     DEEOBCD       SCEOB3
     C     XXSEQ#        WHENEQ    4
     C                   MOVEL     DEEOBCD       SCEOB4
     C     XXSEQ#        WHENEQ    5
     C                   MOVEL     DEEOBCD       SCEOB5
     C     XXSEQ#        WHENEQ    6
     C                   MOVEL     DEEOBCD       SCEOB6
     C     XXSEQ#        WHENEQ    7
     C                   MOVEL     DEEOBCD       SCEOB7
     C     XXSEQ#        WHENEQ    8
     C                   MOVEL     DEEOBCD       SCEOB8
     C     XXSEQ#        WHENEQ    9
     C                   MOVEL     DEEOBCD       SCEOB9
     C     XXSEQ#        WHENEQ    10
     C                   MOVEL     DEEOBCD       SCEOB10
     C     XXSEQ#        WHENEQ    11
     C                   MOVEL     DEEOBCD       SCEOB11
     C     XXSEQ#        WHENEQ    12
     C                   MOVEL     DEEOBCD       SCEOB12
     C                   ENDSL
     C**** End Add *** 07/18/23 **************************************
     C                   ENDIF                                                  DEEOB NE *BLANKS
     C     DEKY          READE(N)  PDE01                                  99
     C                   ENDDO
     C                   MOVEL     CAZIPA        SCZIPA
     C                   MOVEL     CAZIPA        SCZIPB
     C                   ELSE
     C                   MOVEL     *BLANKS       SCZIPA
     C                   MOVEL     *BLANKS       SCZIPB
     C                   ENDIF                                                  @@MODE = 'C'
     C*
     C* DISPLAY RULE NUMBER
     C*
     C                   Z-ADD     CARULE        SCRULE
     C     SCRULE        IFNE      *ZEROS
     C                   SETON                                        56
     C                   ELSE
     C                   SETOFF                                       56
     C                   ENDIF                                                  SCRULE NE *ZEROS
     C*
     C                   ENDIF                                                  @@MODE IFNE 'A'
     C*
     C* DEFAULT UNITS TO 1 IF = 0
     C*
     C     SCUNTS        IFEQ      0
     C                   Z-ADD     1             SCUNTS
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C* DEFAULT UNITS TO 1 IF = 0
     C*
     C     SCUNTS        IFEQ      0
     C                   Z-ADD     1             SCUNTS
     C                   ENDIF
     C*
     C* LOAD FIELDS FOR FDDUP
     C*
     C                   MOVE(P)   SCPOS         DUPOS
     C                   Z-ADD     SCRVCD        DURVCD
     C                   MOVEL(P)  SCHCPC        DURATE
     C     SCCDDS        IFEQ      *BLANKS
     C                   MOVEL(P)  UNLIST        DUDESC
     C                   ELSE
     C                   MOVEL(P)  SCCDDS        DUDESC
     C                   ENDIF
     C     SCCDDS        IFEQ      *BLANKS
     C     SCCDDS        OREQ      UNLIST
     C     SCHCPC        IFNE      *BLANKS
     C*
     C                   Z-ADD     SCFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKFDAT
     C                   MOVE      SCHCPC        KKRATE
     C*
     C*** BEG CHG *** 08/11/10 ****************************************
     C*********  PRKY      SETGTPPR01
     C*********  PRKY1     REDPEPPR01                    99
     C     FSKY2         SETGT     PFS01
     C     FSKY3         READPE    PFS01                                  99
     C*** END CHG *** 08/11/10 ****************************************
     C*
     C     *IN99         IFEQ      *OFF
     C*** BEG CHG *** 08/11/10 ****************************************
     C*********  PRCPT4    ANDEQSCHCPC
     C*********  PRSTAT    ANDNE'I'
     C     FSCODE        ANDEQ     SCHCPC
     C     FSSTAT        ANDNE     'I'
     C*
     C*********            MOVELPRDSC1    DUDESC    P
     C                   MOVEL(P)  FSDSC1        DUDESC
     C*** END CHG *** 08/11/10 ****************************************
     C*
     C                   ENDIF                                                  99=OFF
     C*
     C                   ENDIF                                                  DURATE <> BLANK
     C*
     C                   ENDIF                                                  SCCDDS IFEQ
     C                   Z-ADD     DURVCD        DUBPRC
     C                   MOVEL(P)  DURATE        DUBRAT
     C                   MOVEL(P)  DUDESC        DUBPRD
     C                   Z-ADD     SCUNTS        DUUNTS
     C                   MOVE      SCPMOD        DUPMOD
     C                   MOVE      SCMOD2        DUMOD2
     C*
     C* LOAD FIELDS FOR FDCAP
     C*
     C                   Z-ADD     SCFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CAFDAT
     C                   MOVEL     SRDTO         CATDAT
      *
     C     SCTDAT        IFNE      *ZEROS
     C                   Z-ADD     SCTDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CATDAT
     C                   ENDIF
      *
     C                   MOVEL     SCHCPC        CACPT4
     C                   MOVEL     DURVCD        K1RVCD
     C                   MOVE      DURATE        K1HCPC
     C                   MOVE      *BLANKS       K1UNTS
     C                   MOVEL(P)  K1CMSV        CACODE
     C                   Z-ADD     SCRVCD        CASVCD
     C*
     C                   Z-ADD     SCUNTS        CAUNTS
     C*
     C                   Z-ADD     SCCHRG        CACHRG
     C                   MOVEL     EOSS#         CASS#
     C                   MOVEL     EOACDT        CAACDT
     C                   Z-ADD     SCOTHR        CAOTHR
     C                   Z-ADD     SCOTH2        CAOTH2
     C                   Z-ADD     SCNTWK        CANDIS
     C                   Z-ADD     SCMRA         CAMRA
     C                   MOVE      SCZIPA        CAZIPA
     C                   MOVE(P)   SCAST         CAAST
     C*
     C                   Z-ADD     SCPAID        CAPAID
     C                   MOVEL(P)  SCCSCD        CACSCD
     C                   MOVEL(P)  SCOT1C        CAOT1C
     C                   MOVEL(P)  SCOT2C        CAOT2C
     C     XXRCFL        IFEQ      'Y'
     C                   MOVEL     'Y'           CARCFL
     C                   ELSE
     C                   MOVEL     'S'           CARCFL
     C                   ENDIF
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
     C* INITIALIZE INDICATORS FOR ERROR CHECKING: THESE ARE INDICATORS
     C* BEYOND 71 - 89 RESERVED BY COPYBOOK. THIS SHOULD BE SET OFF
     C* IN 'PGMEND' SUBROUTINE AS WELL.
     C*
     C                   SETOFF                                       606162
     C                   SETOFF                                       636465
     C                   SETOFF                                       666758
     C                   SETOFF                                       7030
     C*
     C*** INITIALIZE POSITION CURSOR INDICATORS
     C*
     C                   SETOFF                                       313233
     C                   SETOFF                                       343536
     C                   SETOFF                                       373839
     C                   SETOFF                                       404142
     C                   SETOFF                                       484950
     C                   SETOFF                                       535455
     C*
     C*
     C     *INKA         CASEQ     *ON           KADO                           F1=Repeat
     C     *INKB         CASEQ     *ON           KBDO                           F02=Process
     C     *INKG         CASEQ     *ON           KGDO                           F07=Header info
     C     *INKI         CASEQ     *ON           KIDO                           F09=Code info
     C     *INKJ         CASEQ     *ON           KJDO                           F10=Search
     C     *INKK         CASEQ     *ON           KKDO                           F11=EOB/Notes
     C     *INKN         CASEQ     *ON           KNDO                           F14=DARTS
     C     *INKP         CASEQ     *ON           KPDO                           F15=Previous lines
     C                   ENDCS
     C*
     C* Close state specific files that are overridden
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C     @@MODE        IFEQ      'C'
     C     *INKL         ANDEQ     *OFF
     C                   EXSR      KBDO
     C                   ENDIF
     C*
     C                   SETOFF                                       606162
     C                   SETOFF                                       636465
     C                   SETOFF                                       666758
     C                   SETOFF                                       7030
     C*
     C                   CLOSE     FTABL
     C                   CLOSE     FMSVL1
     C                   CLOSE     FTPSL
     C                   CLOSE     FMFSL2IL
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   OPEN      FTABL
     C                   OPEN      FMSVL1
     C                   OPEN      FTPSL
     C                   OPEN      FMFSL2IL
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       ##CLT#            3
     C*
     C                   SETOFF                                       16
     C     ##CLT#        IFEQ      '025'
     C     ##CLT#        OREQ      '144'
     C                   SETON                                        16
     C                   ENDIF
     C*
     C** Get standard screen headings
     C*
     C     @@RRN2        CHAIN(N)  PEO01                              99
     C                   MOVE      EOCO#         #1CO#
     C                   MOVE      EOSUB#        #1SUB#
     C                   MOVE      EOFND         #1FND
     C                   MOVE      EOFYR         #1FYR
     C                   MOVE      EOCASE        #1CASE
     C                   MOVE      EOCO#         KKCO#
     C                   MOVE      EOSUB#        KKSUB#
     C                   MOVE      EOFND         KKFND
     C                   MOVE      EOFYR         KKFYR
     C                   MOVE      EOCASE        KKCASE
     C                   MOVE      EOEOB#        KKEOB#
     C                   MOVE      EORCN         KKRCN
     C     EOAST         IFEQ      'TX'
     C                   MOVEL(P)  EOAST         XXKEY            10
     C     XXKEY         CHAIN     STSTL1                             99
     C     1             SUBST     STBS02:2      @EDTRC            1
     C                   ELSE
     C                   MOVE      'N'           @EDTRC
     C                   ENDIF
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
     C                   CALL      'SA009R'
     C                   PARM      *ZEROS        ##EOBD            3 0          EOMB DELETE SECURITY
     C                   PARM      *BLANKS       ##EOBI            1            EOMB INTERACIVE PROC
     C                   PARM      *BLANKS       ##ARIN            1            AUDIT/RECON INSTALLE
     C                   PARM      *BLANKS       ##ACIS            1            AUTO CHECK ISSUANCE
     C                   PARM      *BLANKS       ##PAY0            1            PRINT WCMC EOB PAY>0
     C                   PARM      *BLANKS       ##SDTS            1            ALLOW FUTURE SERV DT
     C                   PARM      *BLANKS       ##MCIV            1            MED CASES INIT VIEW
     C                   PARM      *BLANKS       ##PBIV            1            PROV BILLS ININT VIE
     C                   PARM      *BLANKS       ##AINV            1            AUTO INVOICE FLAG
     C                   PARM      *BLANKS       ##WHPG            1            Work History Usage
     C                   PARM      *BLANKS       ##PRIV            1            PROVIDER INIT VIEW
     C                   PARM      *BLANKS       ##TDUP            1            ADJUDICATION DUP CRI
     C                   PARM      *BLANKS       ##NWSL            1            PROV NTWK SELECTION
     C                   PARM      *BLANKS       ##TP01            2            TAPE STATE 01
     C                   PARM      *BLANKS       ##TP02            2            TAPE STATE 02
     C                   PARM      *BLANKS       ##TP03            2            TAPE STATE 03
     C                   PARM      *BLANKS       ##TP04            2            TAPE STATE 04
     C                   PARM      *BLANKS       ##TP05            2            TAPE STATE 05
     C                   PARM      *BLANKS       ##TP06            2            TAPE STATE 06
     C                   PARM      *BLANKS       ##TP07            2            TAPE STATE 07
     C                   PARM      *BLANKS       ##TP08            2            TAPE STATE 08
     C                   PARM      *BLANKS       ##TP09            2            TAPE STATE 09
     C                   PARM      *BLANKS       ##TP10            2            TAPE STATE 10
     C*
     C* SET KEY FIELDS
     C*
     C                   Z-ADD     EOSUB#        KKSUB#
     C                   Z-ADD     EOFND         KKFND
     C                   Z-ADD     EOFYR         KKFYR
     C                   Z-ADD     EOCASE        KKCASE
     C                   Z-ADD     EOFEIN        KKFEIN
     C                   Z-ADD     EOMOD         KKMOD
     C                   Z-ADD     EOEOB#        KKEOB#
     C                   Z-ADD     EORCN         KKRCN
     C*
     C                   MOVE      *BLANKS       TMCMSV
     C     P1KY          CHAIN(N)  PP101                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  P1CNTY        TMCMSV
     C                   ENDIF
     C*
     C* IF IN DELETE MODE, WE MUST CHAIN TO FDCAP RECORD TO GET THE
     C* DETAIL SEQUECE NUMBER AND THE RELATIVE RECORD NUMBER FOR THE
     C* PHARMACY DETAIL RECORD.  ELSE USE THE PASSED PHARMACY DETAIL
     C* RELEATIVE RECORD NUMBER TO GET THE DETAIL SEQUENCE NUMBER
     C*
     C     @@MODE        IFEQ      'E'
     C     @@RRN3        CHAIN(N)  PCA02                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     CADSEQ        KKDSEQ
     C     DUKY          CHAIN     PDU01                              99
     C                   Z-ADD     DBRRNC        @@RRN
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ELSE
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C                   Z-ADD     DUDSEQ        KKDSEQ
     C                   ENDIF                                                  @@MODE = 'E'
     C*
     C* SET TOTAL CHARGE
     C*
     C                   EXSR      SMCHRG
     C*
     C** Get 'Amount & type code' headings
     C*
     C     CXKY          CHAIN     PCX01                              99
     C                   MOVEL     CXHMRA        SCHMR1
     C                   MOVEL     CXHNET        SCHNTW
     C                   MOVEL     CXHOT1        SCHO11
     C                   MOVEL     CXHOT1        SCHO12
     C                   MOVEL     CXHOT2        SCHO21
     C                   MOVEL     CXHOT2        SCHO22
     C*
     C* SET INDICATOR TO DISPLAY NEWORK REDUCTIONS FIELD IF
     C* NETWORK IS INSTALLED
     C*
     C     ##NTWK        IFEQ      'Y'
     C                   SETON                                        47
     C*
     C* DETERMINE IF THE NETWORK SELECTED HAS A MANUAL FEE SCHEDULE
     C*
     C     EOFSFL        IFEQ      'Y'
     C                   SETON                                        03
     C                   ELSE
     C                   SETOFF                                       03
     C                   ENDIF
     C                   ELSE
     C                   SETOFF                                       47
     C                   ENDIF                                                  ##NTWK = 'Y'
     C*
     C* DETERMINE IF ANY EOB/NOTES EXISTS FOR THIS BILL
     C*
     C     DEKY          CHAIN(N)  PDE01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        69
     C                   ELSE
     C                   SETOFF                                       69
     C                   ENDIF                                                  *IN99 IFEQ *OFF
     C*
     C* DISPLAY PROVIDER NETWORK IF NOT BLANKS
     C*
     C     EONTWK        IFNE      *BLANKS
     C                   MOVEL(P)  EONTWK        SCNWCD
     C                   SETON                                        68
     C                   ELSE
     C                   SETOFF                                       68
     C                   ENDIF                                                  EONTWK
     C*
     C     @@MODE        IFEQ      'A'
     C                   SETON                                        41
     C                   ENDIF
     C*
     C* DIAGNOSIS CODES 2, 3 AND 4 WILL BE DISPLAYED ONLY IF EXISTS
     C* FAEHP
     C*
     C* PROTECT ALL FIELDS EXCEPT AMOUNT FIELDS WHEN DOING A RECON
     C* IF THE CHARGE IS PART OF THE PREVIOUSLY COMPLETED BILL.
     C* ALL FIELDS ARE AVAILABLE IF ADDDED DURING THE RECON.
     C*
     C     @@RRN3        CHAIN     PCA02                              99
     C     EORCN         IFNE      *ZEROS
     C     @@MODE        ANDEQ     'C'
     C     CARCN         ANDNE     EORCN
     C                   SETON                                        46
     C                   ELSE
     C                   SETOFF                                       46
     C                   ENDIF                                                  EORCN NE *ZEROS
     C*
     C     @@MODE        COMP      'A'                                4343
     C*
     C* CALL PRR-CERT SELECTION PROGRAM IF IN ADD MODE AND PRE-CERT
     C* IS INSTALLED AND PRE-CERT RECORDS EXIST FOR THIS CASE
     C*
     C     @@MODE        IFEQ      'A'
     C     ##UR          ANDEQ     'Y'
     C     EORCN         ANDEQ     *ZEROS
     C                   MOVE      *BLANKS       HLCERT
     C                   Z-ADD     0             HLEXT
     C*
     C     PCKY          CHAIN     PPC02                              99
     C     *IN99         IFEQ      *OFF
     C     CMKY          CHAIN     PCM01                              99        GET FMCM RRN
     C                   CALL      'U0060R'
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      '2'           ##CALL
     C                   PARM      DBRRNB        ##RRN
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PPC01                              98
     C     *IN98         IFEQ      *OFF
     C     @@RRN2        CHAIN     PEO01                              98
     C     *IN98         IFEQ      *OFF
     C                   MOVEL(P)  PCCERT        EOCERT
     C                   Z-ADD     PCEXT         EOEXT
     C                   MOVEL     Q1USER        EOUSER
     C                   MOVEL     Q1PGM         EOPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        EOTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C                   UPDATE    PEO01
     C                   ENDIF                                                  *IN98 = *0FF
     C                   ENDIF                                                  *IN98 = *OFF
     C                   ENDIF                                                  ##KC EQ *BLANKS
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ENDIF                                                  @@MODE = 'A'
     C*
     C* DISPLAY EOB CODES AND TITLE IF NOT IN ADD MODE
     C*
     C     @@MODE        IFNE      'A'
     C                   SETON                                        59
     C                   ELSE
     C                   SETOFF                                       59
     C                   ENDIF                                                  @@MODE NE 'A'
     C*
     C* SET INDICATOR TO POSITION CURSOR TO PLACE OF SERVICE FIELD
     C* IF IN ADD MODE
     C*
     C     @@MODE        IFEQ      'A'
     C                   SETON                                        32
     C                   ELSE
     C                   SETOFF                                       32
     C                   ENDIF
     C     EOAST         IFEQ      'MS'
     C                   SETON                                        97
     C                   ELSE
     C                   SETOFF                                       97
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*================================================================
     C* SMCHRG - Sum up all charges for bill
     C*================================================================
     C     SMCHRG        BEGSR
     C*
     C* SUM UP TOTAL CHARGES FOR THIS BILL
     C*
     C                   Z-ADD     0             TLCHRG
     C     CAKY3         CHAIN     PCA03                              99
     C     *IN99         DOWEQ     *OFF
     C                   ADD       CACHRG        TLCHRG
     C     CAKY3         READE     PCA03                                  99
     C                   ENDDO
     C                   Z-ADD     TLCHRG        SCTOTC
     C                   ENDSR
     C*================================================================
     C* UFAEOP - Update FAEOP file
     C*================================================================
     C     UFAEOP        BEGSR
     C*
     C     @@RRN2        CHAIN     PEO01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     SCFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLFDAT
     C*
     C     EOFRDT        IFEQ      *ZEROS
     C                   Z-ADD     HLFDAT        EOFRDT
     C                   ENDIF
     C*
     C     EOFRDT        IFNE      *ZEROS
     C     HLFDAT        ANDLT     EOFRDT
     C                   Z-ADD     HLFDAT        EOFRDT
     C                   ENDIF
     C*
     C     EOTODT        IFEQ      *ZEROS
     C                   Z-ADD     HLFDAT        EOTODT
     C                   ENDIF
     C*
     C     EOTODT        IFNE      *ZEROS
     C     HLFDAT        ANDGT     EOTODT
     C                   Z-ADD     HLFDAT        EOTODT
     C                   ENDIF
     C*
     C                   MOVEL     Q1USER        EOUSER
     C                   MOVEL     Q1PGM         EOPGM
     C                   Z-ADD     DSTIME        EOTIME
     C                   MOVEL     SRDTO         EODT
     C*
     C                   UPDATE    PEO01
     C                   ENDIF                                                  *IN99 IFEQ
     C                   ENDSR
     C*================================================================
     C* UFDCAP - Update FDCAP record with any changes
     C*================================================================
     C     UFDCAP        BEGSR
     C     @@RRN3        CHAIN     PCA02                              99
     C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C*
     C* IF ADJUDICATION CODE IS 1, CLEAR CATOTA.  THIS WILL ALLOW THE
     C* OVERRIDE PROCESSING TO BE PERFORMED IN THE ADJUDICTION PROGRAM
     C*
     C                   MOVEL     Q1USER        CAUSER
     C                   MOVEL     Q1PGM         CAPGM
     C                   Z-ADD     DSTIME        CATIME
     C                   MOVEL     SRDTO         CADT
     C                   UPDATE    PCA02
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C                   TIME                    DSDTTM
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        DUUSER
     C                   MOVEL     Q1PGM         DUPGM
     C                   Z-ADD     DSTIME        DUTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DUDT
     C                   UPDATE    DBRCDF
     C*
     C* UPDATE FAEOP
     C*
     C                   EXSR      UFAEOP
     C*
     C* UPDATE FDCAP
     C*
     C                   EXSR      UFDCAP
     C*
     C* UPDATE TOTAL CHARGES FIELD
     C*
     C                   EXSR      SMCHRG
     C*
     C                   ENDIF
     C                   SETOFF                                       90
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C                   CLEAR                   PCA02
     C                   TIME                    DSDTTM
     C                   EXSR      MVSCDB
     C                   Z-ADD     ##CO#         DUCO#
     C                   Z-ADD     EOSUB#        DUSUB#
     C                   Z-ADD     EOFND         DUFND
     C                   Z-ADD     EOFYR         DUFYR
     C                   Z-ADD     EOCASE        DUCASE
     C                   Z-ADD     SCSEQ#        DUDSEQ
     C                   Z-ADD     EOEOB#        DUEOB#
     C                   MOVEL     EOSS#         DUSS#
     C                   Z-ADD     EOACDT        DUACDT
     C                   MOVE      'A'           DUSTAT
     C                   MOVEL     Q1USER        DUUSER
     C                   MOVEL     Q1USER        DUIUSR
     C                   MOVEL     Q1PGM         DUPGM
     C                   MOVEL     Q1PGM         DUIPGM
     C                   Z-ADD     DSTIME        DUTIME
     C                   Z-ADD     DSTIME        DUITIM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DUDT
     C                   MOVEL     SRDTO         DUIDT
     C                   WRITE     DBRCDF
     C*
     C                   Z-ADD     ##CO#         CACO#
     C                   Z-ADD     EOSUB#        CASUB#
     C                   Z-ADD     EOFND         CAFND
     C                   Z-ADD     EOFYR         CAFYR
     C                   Z-ADD     EOCASE        CACASE
     C                   MOVEL     EOSS#         CASS#
     C                   Z-ADD     EOACDT        CAACDT
     C                   MOVEL     Q1USER        CAUSER
     C                   MOVEL     Q1USER        CAIUSR
     C                   MOVEL     Q1PGM         CAPGM
     C                   MOVEL     Q1PGM         CAIPGM
     C                   Z-ADD     DSTIME        CATIME
     C                   MOVEL     SRDTO         CADT
     C                   MOVEL     SRDTO         CAIDT
     C                   MOVEL     SRDTO         CARDAT
     C                   Z-ADD     SCLIN#        CALIN#
     C                   Z-ADD     SCSEQ#        CADSEQ
     C                   Z-ADD     EOEOB#        CAEOB#
     C                   Z-ADD     EORCN         CARCN
     C                   Z-ADD     EOFEIN        CAFEIN
     C                   Z-ADD     EOMOD         CAMOD
     C                   MOVE      'S'           CARCFL
     C*
     C                   MOVEL(P)  '4'           CAPRTY
     C*
     C                   WRITE     PCA02
     C*
     C* UPDATE FAEOP
     C*
     C                   EXSR      UFAEOP
     C*
     C* UPDATE TOTAL CHARGES
     C*
     C                   EXSR      SMCHRG
     C*
     C                   FEOD      FDDUP
     C                   SETON                                        32
     C                   ENDSR
     C*================================================================
     C     CKAP          BEGSR
     C*================================================================
      *
     C                   Z-ADD     K1EFDT        KKEFDT
     C                   MOVEL     KKRATE        KAHCPC
      *
     C     APKY1         SETGT     PAP01                              99
      *
     C     *IN99         IFEQ      *ON
     C     *HIVAL        SETGT     PAP01
     C                   ENDIF
      *
     C     APKY2         READPE    PAP01                                  99
      *
     C     *IN99         IFEQ      *OFF
      *
     C     *IN99         DOWEQ     *OFF
     C     APSTAT        ANDNE     'A'
     C     APKY2         READPE    PAP01                                  99
     C                   ENDDO
      *
     C     APEFDT        IFGT      KKFDAT
     C     *IN99         OREQ      *ON
      *
     C     @EDTRC        IFEQ      'Y'
     C                   EXSR      CKUC
     C                   ELSE
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN94
     C                   GOTO      CKAPED
     C                   ENDIF                                                  @EDTRC = 'Y'
      *
     C                   ENDIF                                                  *IN99 = *ON
      *
     C                   ELSE                                                   *IN99 = *OFF
      *
     C     @EDTRC        IFEQ      'Y'
     C                   EXSR      CKUC
     C                   ELSE
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN94
     C                   GOTO      CKAPED
     C                   ENDIF                                                  @EDTRC = 'Y'
      *
     C                   ENDIF                                                  *IN99 = *OFF
      *
     C     CKAPED        ENDSR
      *
     C*================================================================
     C     CKUC          BEGSR
     C*================================================================
      *
     C                   MOVE(P)   SCZIPA        CAZIPA
     C                   MOVE(P)   CAZIPA        KKZIPA
     C                   MOVE      *BLANK        XXRCFL            1
      *
     C     RCKY          SETGT     PRC01                              99
      *
     C     *IN99         IFEQ      *ON
     C     *HIVAL        SETGT     PRC01
     C                   ENDIF
      *
     C     RCKY1         READPE    PRC01                                  99
      *
     C     *IN99         IFEQ      *OFF
      *
     C     *IN99         DOWEQ     *OFF
     C     RCSTAT        ANDNE     'A'
     C     RCKY1         READPE    PRC01                                  99
     C                   ENDDO
      *
     C     RCEFDT        IFGT      KKFDAT
     C     *IN99         OREQ      *ON
      *
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN94
     C                   GOTO      CKUCED
      *
     C                   ELSE                                                   *IN99 = *ON
      *
     C                   MOVE      'Y'           XXRCFL
      *
     C                   EXSR      UFDCAP
      *
     C                   ENDIF                                                  *IN99 = *ON
      *
     C                   EXSR      UFDCAP
      *
     C                   ELSE                                                   *IN99 = *OFF
      *
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN94
     C                   GOTO      CKUCED
      *
     C                   ENDIF                                                  *IN99 = *OFF
      *
     C     CKUCED        ENDSR
     OPMG01     E            RPMG01
     OPDE01     E            RPDE01
