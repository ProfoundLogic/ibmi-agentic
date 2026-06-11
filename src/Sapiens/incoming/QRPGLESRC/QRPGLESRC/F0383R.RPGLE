     H Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('STBNDDIR')
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE F0383R - Dental Detail Information
     H*****************************************************************
     H*
     H* F0383R - Dental Detail Information
     H*
     H* 06/21/99  C:117  Project: MC152
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  07/09/24  C:342  Project: FS688
     H*                   2024 USH&L Fee Schedule (Recompile)
     H*
     H*  07/18/23 C:342 Project: NY060
     H*                 NY Compliance: CARC/RARC Update
     H*
     H*  06/27/19 C:079  Project: 152MAP
     H*                  Relax edits if Pbills lite is installed
     H*
     H*  09/15/15  C:079  Support: MC090 37261
     H*                   Move ICD-10 description over two spaces
     H*
     H*  09/12/14  C:296  Support: 81089
     H*                   Add error message in screen for indicator 30.
     H*
     H*  07/11/14  C:280  Support: 86063
     H*                   Change KKICD9 to movel                       s.
     H*
     H*
     H*  05/20/14  C:079  Project:114MBB
     H*                   *** Remove project ***
     H*
     H*  05/14/14  C:296 Project: NC034
     H*                  Remove dental code check for EDI flag = Y.
     H*
     H*  02/04/14  C:290  PRISS: 17 Project SY336
     H*                   Corrected the logic for populating ICD10 codes.
     H*                   Includes Display file changes also.
     H*
     H*  11/09/13  C:296 Project: FS433
     H*                  Corrected the logic when hlcf <> 0
     H*
     H*  04/08/13 C:284 Project: 147MAT
    H*                 Do not update CARCFL if it is already 'Y'
    H*                 Change DDRCFL to CARCFL in KIDO subr.
     H*
     H*  02/27/13  C:284  Project:IL005
     H*                   Modify program to load data from FMF1PIL when
     H*                   EOAST = IL .
     H*
     H*  11/15/12 C:287 Project: MC367
     H*                 Disable function code F14 and F15 for GS access to Bill Entry
     H*
     H*  10/18/12  C:284  Project:114MHU
     H*                   Add condition to by pass edits on certain condition.
     H*
     H*  07/26/12  C:277  Project:TN016
     H*                   For F9=Code Info, selection for Medicare
     H*                   physician only
     H*
     H*  06/21/12  C:277  Project:114MFI
     H*                   Disable F9 when Adjudication state = HL
     H*
     H*  12/14/11  C:251  Project:114MBB
     H*                   Call API FA775R to calculate payment based on
     H*                   annual and lifetime maximums
     H*
     H*  08/19/11  C:079  Project:114MYS
     H*                   Only call FA770R for reconsiderations
     F*
     H*  06/22/11  C:198  Project:114MYS
     H*                   Added logic to verify if payment limit is reached
     H*
     F*  09/20/10  C:227  Project: IL001
     F*                   Made changes to use R & C for IL state.
     H*
     F*  06/26/09  C:198  Support: 68962
     F*                   Re-compiled only.
     F*
     F*  02/08/08  C:189  PROJECT: MC319
     F*                   DO NOT ALLOW INACTIVE CODES FOR PLACE OF SERVICE
     H*
     H*  04/22/07  C:079  PROJECT: FL067
     H*                   CHANGE OUT THE EOBR1 AND EOBR2 ARRAYS TO USE
     H*                   NEW D CODES
     H*
     H*  04/20/07  C:188  SUPPORT: MC025 58045
     H*                   CORRECT DUPLICATE ERROR INDICATOR
     H*
     H*  04/17/07  C:188  SUPPORT: MC114 56809
     H*                   CONDITION CHAIN TO FMRCP BASED ON STATE
     H*                   CONTROL FILE (STSTP) FLAG STBS02.
     H*
     H*  04/15/04  C:079  PROJECT FL046
     H*                   FLORIDA: EDIT EOBR CODES
     H*
     H*  05/30/03  C:079  PROJECT FSMNT
     H*                   SCPOS CONFICTING INDICATOR CHANGED
     H*
     H*  04/25/03  C:079  PROJECT FS255
     H*                   CHANGES TO F1=REPEAT
     H*
     H*  04/03/03  C:079  SUPPORT MC044 36611
     H*                   SCPOS CHANGED TO 3.0N
     H*
     H*  01/14/03  C:062  PROJECT FL027
     H*                   ADD EDIT ON CPT4 CODE IT MUST BE A DENTAL
     H*                   CODE IF THE EDI FLAG IS Y
     H*
     H*  01/07/03  C:062  PROJECT FS244
     H*                   USE EOFLIC TO GET THE GEO ZIP
     H*
     H*  12/20/02  C:079  PROJECT: NW075
     H*                   CALL N0037R TO DETERMINE MANUAL FEE SCHEDULE
     H*
     H*  08/23/02  C:079  SUPPORT: MC025 33965
     H*                   1. CAN'T USE IN15(RESERVED FOR COPYBOOK)
     H*                   2. ALSO MOVE XXRCFL INTO CARCFL TO UPDATE
     H*                   3. DON'T POPULATE XXRCFL FOR BILLED CODE.
     H*
     H*  08/07/02  C:062  PROJECT FS244
     H*                   ZIP AREA IS NOW BEING ASSIGNED AT THE LINE
     H*
     H*  07/05/02  C:062  SUPPORT:FS246
     H*                   ALLOW DATE WINDOW TO POP UP EVEN IF NOT
     H*                   AN EDI BILL.
     H*
     H*  04/08/02  C:062  PROJECT:FS236
     H*                   POPULATE CAPRTY WITH EOBTYP.
     H*
     H*  01/07/02  C:062  PROJECT:FS235
     H*                   DELETE ONLY EOB CODES IF CURRENT RECON LEVEL.
     H*
     H*  10/31/01  C:079  SUPPORT:MC055 29571
     H*                   On an override don't force and EOMB code if
     H*                   percent >= 100.
     H*
     H*  05/31/01  C:062  Project: MCMNT
     H*                   Correct conditions for CARERR window to be
     H*                   displayed. Use GT not GE.
     H*
     H*  05/31/01  C:062  PROJECT: MC114 27653
     H*                   DON'T EDIT FIELDS THAT ARE PROTECTED.
     H*
     H*  05/24/01  C:062  PROJECT: FS209
     H*                   LOAD CACODE WITH DDPRCD
     H*
     H*  05/24/01  C:062  PROJECT: MCMNT #191
     H*                   ADDING PROCESSING FOR ENTRY DATE SCREEN FLD.
     H*
     H*  05/02/01  C:079  SUPPORT: MC114 27315
     H*                   1. Make FTABL, FMPRL, and FTPSL user
     H*                      controlled files and close them on return
     H*                      in case adj state changes on next entry.
     H*
     H*  03/26/01  C:062  PROJECT FSMNT
     H*                   CHANGE PROCESSING OF EOB NOTES/CODES
     H*
     H*  02/14/01 C:117 PROJECT: FS220
     H*                 ADDING DIAGNOSIS CODES TO DENTAL PGMS
     H*
     H*  01/17/01 C:079 Project: PA008
     H*                 Update CARCFL and DDRCFL from XXRCFL.
     H*
     H*  01/11/01 C:062 Support MC111 25815
     H*                 COMPILE ONLY.
     H*
     H*  10/13/00 C:062 Support MC238
     H*                 ADD LOGIC TO DEAL WITH NON-SEARCHABLE FIELDS.
     H*
     H*  08/15/00 C:62  PROJECT: MC215
     H*                 REPLACE SFBPF REFERENCES WITH APIS.
     H*
     H*  06/19/00 C:117 PROJECT FS211
     H*                 Additional Edits to date fields
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
     H*
     H*  02/25/00 C:117 SUPPORT: MC044/21153
     H*                 Modified the Adj. code when doing an F1=Repeat
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
     H*  12/07/99 C:79  Project: FS144
     H*                 1. Don't clear CAMRAA. This causes problems
     H*                    calculating FSMRA
     H*
     H*  11/12/99 C:56  MC152
     H*                 Delete/Change efficiency isssue.
     H*
     H*  09/30/99 C:56  Support: MC152
     H*                 Add screen billed code to repeat processing
     H*
     H*  08/27/99 C:56  MC152
     H*                 When Adjudication Code is override type and
     H*                 override percentage is zero, delete any
     H*                 adjudication/network eob codes or messages.
     H*  07/23/99 C:31  Project: NW010
     H*                 WC Summary File
     H*                 1. Add Manual fee schedule message and code
     H*
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      29      Add mode                                       **
     H*****************************************************************
     H*
     FF0383F    CF   E             WORKSTN
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F*
     F*  Physical file definition (for maintaining record)
     FFDDDP     UF A E             DISK
     F                                     RENAME(PDD01:DBRCDF)
     F*
     F*  EOMB Transaction File
     FFAEOP     UF   E             DISK
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
     F*** BEGIN ADD *** 03/26/01 **************************************
     FFDDEL3    IF   E           K DISK
     F                                     RENAME(PDE01:PDE03)
     F*** END   ADD *** 03/26/01 **************************************
     F*
     F*  Fee Schedule Claims Status Code File
     FFTCSP     IF   E             DISK
     F                                     RENAME(PCS01:PCS02)
     F*  Place of Service Table File
     FFTPSL     IF   E           K DISK    USROPN
     F*
     F*
     F*  Provider Bills Control File
     FFMBEL     IF   E           K DISK
     F*
     F*  WCMC Control File
     FFMCXL     IF   E           K DISK
     F*
     F*  Bill Status Table File
     FFTBSL     IF   E           K DISK
     F*** BEGIN ADD *** 10/13/00 **************************************
     FFTBSP     IF   E             DISK
     F                                     RENAME(PBS01:PBS02)
     F*** END   ADD *** 10/13/00 **************************************
     F*
     F*  EOMB Detail Amount Transaction File
     F*** BEG CHG *** 11/12/99 ****************************************
     F*****FDDAL2  UF  E           K        DISK                      A
     FFDDAL2    UF   E           K DISK
     F*** END CHG *** 11/12/99 ****************************************
     F*
     F*** BEG DEL *** 11/12/99 ****************************************
     F**********FDDAL1  IF  E           K        DISK
     F**********  PDA01                             KRENAMEPDA02
     F*** END DEL *** 11/12/99 ****************************************
     F*
     F*  Logical over all Tabel Files
     FFTABL     IF   E           K DISK    USROPN
     F                                     RENAME(PTS01:PTS02)
     F                                     IGNORE(PAC01)
     F*** BEG DEL *** 04/15/04 ****************************************
     F**********  PEB01                             KIGNORE
     F*** END DEL *** 04/15/04 ****************************************
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
     F* Generic Table File
     FFTGTL     IF   E           K DISK
     FFTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PGT02)
     F*
     F* State FS Rules/Pgm Cross Reference File
     FFDRUL1    IF   E           K DISK
     F*** BEGIN DEL *** 08/15/00 **************************************
     F**********SSYLF   IF  E           K        DISK
     F**********  SPC01                             KIGNORE
     F**********  SWC01                             KIGNORE
     F**********  SRC01                             KIGNORE
     F**********  SDR01                             KIGNORE
     F**********  SBC01                             KIGNORE
     F**********  SCS01                             KIGNORE
     F**********  SGL01                             KIGNORE
     F**********  SP201                             KIGNORE
     F*** END   DEL *** 08/15/00 **************************************
     FFDMGL2    UF   E           K DISK
     F*
     F* EOMB Dental Detail Transaction File
     FFDDDL     IF   E           K DISK
     F*** BEG ADD *** 02/14/01 ****************************************
     F                                     INFDS(DBFDC)
     F*** END ADD *** 02/14/01 ****************************************
     F*** BEGIN ADD *** 08/07/02 **************************************
     FFMP1L     IF   E           K DISK
     FFMGZL     IF   E           K DISK
     F*** END   ADD *** 08/07/02 **************************************
     F*
     F* REASONABLE AND CUSTORMARY FILE
     F*
     FFMRCL     IF   E           K DISK
     F                                     INFDS(DBFRC)
     F*** BEG ADD *** 09/20/10 ****************************************
     FFMRCP     IF   E             DISK
     F                                     RENAME(PRC01:PRC02)
     F*** END ADD *** 09/20/10 ****************************************
     F*
     F*  Procedure Code File
     FFMPRL     IF   E           K DISK    USROPN
     F                                     INFDS(DBFPR)
     F*** BEGIN ADD *** 07/23/99 **************************************
     F*** BEGIN DEL *** 01/11/00 **************************************
     F**********FMPDL   IF  E           K        DISK
     F**********FMS4L3  IF  E           K        DISK
     F**********FMS4L2  IF  E           K        DISK
     F**********  PS401                             KRENAMEPS402
     F*** END   DEL *** 01/11/00 **************************************
     F*** END ADD *** 07/23/99 ****************************************
     F*** BEG ADD *** 06/19/00 ****************************************
     FFAEDL     UF   E           K DISK
     FFAESL5    UF   E           K DISK
     F*** END ADD *** 06/19/00 ****************************************
     F*** BEG ADD *** 02/14/01 ****************************************
     F*  Diagnosis Code Physical File
     FFMDIL     IF   E           K DISK
     F*** END ADD *** 02/14/01 ****************************************
     F*
     F*** BEG ADD *** 04/17/07 ****************************************
     F*  State Code Table File
     FSTSTL     IF   E           K DISK
     F*** END ADD *** 04/17/07 ****************************************
     F*** Beg Add *** 06/22/11 ****************************************
     FFMFNL     IF   E           K DISK
     FFTRVL3    IF   E           K DISK
     F                                     INFDS(DBRVU)
     FFTRVP     IF   E             DISK
     F                                     RENAME(PRV01:PRV02)
     F*** Beg Add *** 06/21/12 ***************************************
     FFmhlp     if   e             disk
     F                                     rename(phl01:phl02) Prefix(h2:2)
     F*
     Ffmhll1    if   e           k disk    rename(phl01:phl03) Prefix(h3:2)
     F*** End Add *** 06/21/12 ***************************************
     F*** Beg Add *** 02/27/13 *************************************************
     FFMFSPIL   IF   E             DISK
     F                                     RENAME(PFS01:PFS02)
     FFMFSL2IL  IF   E           K DISK    USROPN
     F****FMFSL1IL  IF   E           K DISK    USROPN
     F****                                RENAME(PFS01:PFS03)
     FFMF1L2IL  IF   E           K DISK
     FFMF1L1IL  IF   E           K DISK
     F                                     RENAME(PF101:PF103)
     FFMF1PIL   IF   E             DISK
     F                                     RENAME(PF101:PF102)
     F*** End Add *** 02/27/13 *************************************************
      *** Beg Add *** 02/04/14 ****************************************
     Ffmicd10l1 if   e           k disk
      *** End Add *** 02/04/14 ****************************************
      /COPY SCOPYMT01D
     D*** End Add *** 06/22/11 ****************************************
     d*** Beg Add *** 06/27/19 ***************************************
      /copy sprmodule
     d isPbillsLite    S               n
     d*** End Add *** 06/27/19 ***************************************
     D*** BEG ADD *** 04/15/04 ****************************************
     D*** BEG CHG *** 04/22/07 ****************************************
     D*****************   EOBR1   1  13  3  A
     D*****************   EOBR2   1   4  3  A
     D EOBR1           S              3    DIM(29) CTDATA PERRCD(1) ASCEND
     D EOBR2           S              3    DIM(15) CTDATA PERRCD(1) ASCEND
     D*** END CHG *** 04/22/07 ****************************************
     D*** END ADD *** 04/15/04 ****************************************
     D*
     D*** BEG DEL *** 11/12/99 ****************************************
     D*************PDA02
     D************* DACO#                           D2CO#
     D************* DASUB#                          D2SUB#
     D************* DAFND                           D2FND
     D************* DAFYR                           D2FYR
     D************* DACASE                          D2CASE
     D************* DASS#                           D2SS#
     D************* DAACDT                          D2ACDT
     D************* DAEOB#                          D2EOB#
     D************* DADSEQ                          D2DSEQ
     D************* DARCN#                          D2RCN#
     D************* DATRDT                          D2TRDT
     D************* DACHRG                          D2CHRG
     D************* DAMRA                           D2MRA
     D************* DAOTHR                          D2OTHR
     D************* DAOTH2                          D2OTH2
     D************* DANDIS                          D2NDIS
     D************* DAPAID                          D2PAID
     D************* DAOTY1                          D2OTY1
     D************* DAOTY2                          D2OTY2
     D************* DATRCD                          D2TRCD
     D************* DASTAT                          D2STAT
     D************* DAUSER                          D2USER
     D************* DAPGM                           D2PGM
     D************* DATIME                          D2TIME
     D************* DADT                            D2DT
     D************* DAIUSR                          D2IUSR
     D************* DAIPGM                          D2IPGM
     D************* DAITIM                          D2ITIM
     D************* DAIDT                           D2IDT
     D*** END DEL *** 11/12/99 ****************************************
     D DBFDB           DS
     D  DBRRNB               397    400B 0
     D DBFPR           DS
     D  PRRRN                397    400B 0
     D DBFDC           DS
     D  DBRRNC               397    400B 0
     D DBFRC           DS
     D  RCRRN                397    400B 0
     D*** Beg Add *** 06/22/11 *****************************
     D DBRVU           DS
     D  RVRRN                397    400B 0
     D*** End Add *** 06/22/11 *****************************
     D***PY SCOPYMT01I
     D*
     D                 DS
     D  TMTOS                  1      5
     D  HLKEY                  1      2
     D  HDKEY                  3      5
     D*
      *** Beg Add *** 02/04/14 ****************************************
     D kkicd10         s                   like(iccode)
      *** End Add *** 02/04/14 ****************************************
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Dental Detail Info-
     D                                     rmation')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)
     D*** Beg Add *** 06/21/12 ****************************************
     DRtvPrcCd         pr                  extpgm('F2386R')
     D inCall                         1
     D inKC                           1
     D inKl                           1
     D inRrn                          9  0
     D*
     D inCall          s              1
     D inKC            s              1
     D inKl            s              1
     D inRrn           s              9  0
     D*** Beg Add *** 07/26/12 ****************************************
     D inMode          s              1
     D xxflg           s              1
     D*** End Add *** 07/26/12 ****************************************
     D*** End Add *** 06/21/12 ****************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          FDDDP
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
     C*** BEGIN ADD *** 03/26/01 **************************************
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
     C*** END   ADD *** 03/26/01 **************************************
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
     C*** BEG DEL *** 11/12/99 ****************************************
     C********** DAKY1     KLIST
     C**********           KFLD           ##CO#
     C**********           KFLD           KKSUB#
     C**********           KFLD           KKFND
     C**********           KFLD           KKFYR
     C**********           KFLD           KKCASE
     C**********           KFLD           KKEOB#
     C**********           KFLD           KKDSEQ
     C**********           KFLD           KKRCN
     C*** END DEL *** 11/12/99 ****************************************
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
     C     DPKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*
     C* KEY LIST FOR GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY1
     C*
     C     GIKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##TBID
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
     C     BEKY          KLIST
     C                   KFLD                    EOAST
     C                   KFLD                    EOSYS
     C                   KFLD                    EOBTYP
     C                   KFLD                    EOSTYP
     C*** BEGIN ADD *** 08/07/02 **************************************
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
     C*** END   ADD *** 08/07/02 **************************************
     C*
     C*  FS BASE CONTROL FILE
     C*
     C*** BEGIN DEL *** 08/15/00 **************************************
     C********** FBKY      KLIST
     C**********           KFLD           XSKEY   2
     C*** END   DEL *** 08/15/00 **************************************
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
     C* Key list for Dental Bill Detail File
     C*
     C     DDKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*
     C     PRKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKPRCD            5
     C                   KFLD                    KKFDAT            7 0
     C*
     C     PRKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKPRCD
     C*
     C     RCKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKZIPA            3
     C                   KFLD                    KKCPT4            5
     C                   KFLD                    KKEFDT
     C*
     C     RCKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKZIPA
     C                   KFLD                    KKCPT4
     C*
     C*  KEY LIST FOR PLACE OF SERVICE
     C     PSKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKPOS
     C*** BEGIN DEL *** 01/11/00 **************************************
     C*** BEGIN ADD *** 07/23/99 **************************************
     C*
     C*          Provider Master File
     C*          ------------------------------------------------------
     C********** PDKY      KLIST
     C**********           KFLD           KKFEIN
     C**********           KFLD           KKMOD
     C*
     C*          Provider Network Contract File
     C*          ------------------------------------------------------
     C********** S4KY      KLIST
     C**********           KFLD           KKFEIN
     C**********           KFLD           KKMOD
     C**********           KFLD           KKNWCD
     C********** *LIKE     DEFN EONTWK    KKNWCD
     C*** END   DEL *** 01/11/00 **************************************
     C*** END ADD *** 07/23/99 ****************************************
     C*** BEG ADD *** 06/19/00 ****************************************
     C*
     C* KEY LIST FOR EOMB DENTAL HEADER FILE
     C*
     C     EDKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
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
     C*** END ADD *** 06/19/00 ****************************************
     C*** BEG ADD *** 02/14/01 ****************************************
     C*
     C*  Key List for Diagnosis Code File
     C*
     C     DIKY          KLIST
     C                   KFLD                    KKICD9
     C                   KFLD                    KKLOST
     C*
     C*** END ADD *** 02/14/01 ****************************************
     C*** BEG ADD *** 04/17/07 ****************************************
     C** Key List for State Code Table File
     C*
     C     STKY          KLIST
     C                   KFLD                    @@KEY
     C*** END ADD *** 04/17/07 ****************************************
     C*** Beg Add *** 06/22/11 ****************************************
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C     RVKY1         KLIST
     C                   KFLD                    KKPRCD
     C                   KFLD                    KKEFDT
     C*
     C     RVKY2         KLIST
     C                   KFLD                    KKPRCD
     C*
     C*** End Add *** 06/22/11 ****************************************
     C*** BEG ADD *** 02/27/13 ****************************************
     C* FMFSPIL KEY
     C     FSKY          KLIST
     C                   KFLD                    KKZIP             3
     C                   KFLD                    KKPRCD
     C                   KFLD                    KKCAT
     C                   KFLD                    KKFDAT
     C*
     C     FSKY1         KLIST
     C                   KFLD                    KKZIP
     C                   KFLD                    KKPRCD
     C                   KFLD                    KKCAT
     C*
     C     FSKY2         KLIST
     C                   KFLD                    KKPRCD
     C                   KFLD                    KKFDAT
     C*
     C     FSKY3         KLIST
     C                   KFLD                    KKPRCD
     C*
     C* KLIST FOR FMF1PIL
     C*
     C     F1KY          KLIST
     C                   KFLD                    KKPRCD
     C                   KFLD                    KKFDAT
     C*
     C     F1KY1         KLIST
     C                   KFLD                    KKPRCD
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
      *** Beg Add *** 02/04/14 ****************************************
     C*   KEY LIST FOR DIAGNOSIS CODE MASTER FILE
     C     fmicd10l1key  klist
     C                   kfld                    kkicd10
      *** End Add *** 02/04/14 ****************************************
     C*
     C*** END ADD *** 02/27/13 ****************************************
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
     C     *LIKE         DEFINE    CACHRG        SMCHG
     C     *LIKE         DEFINE    CAMRA         SMMRA
     C     *LIKE         DEFINE    CANDIS        SMNDIS
     C     *LIKE         DEFINE    CAOTHR        SMOTHR
     C     *LIKE         DEFINE    CAOTH2        SMOTH2
     C     *LIKE         DEFINE    CAPAID        SMPAID
     C     *LIKE         DEFINE    CACHRG        DFCHRG
     C     *LIKE         DEFINE    CAMRA         DFMRA
     C     *LIKE         DEFINE    CANDIS        DFNDIS
     C     *LIKE         DEFINE    CAOTHR        DFOTHR
     C     *LIKE         DEFINE    CAOTH2        DFOTH2
     C     *LIKE         DEFINE    CAPAID        DFPAID
     C     *LIKE         DEFINE    CALIN#        ZZLIN#
     C     *LIKE         DEFINE    CADSEQ        ZZDSEQ
     C     *LIKE         DEFINE    DDPART        KKPART
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C     *LIKE         DEFINE    GTKEY         KKKEY1
     C     *LIKE         DEFINE    RUST          KKST
     C     *LIKE         DEFINE    RUATYP        KKATYP
     C     *LIKE         DEFINE    RUEFDT        KKEFDT
     C     *LIKE         DEFINE    EOCRDT        HLSYDT
     C     *LIKE         DEFINE    CAPAID        XXTOTL
     C**** Beg Chg *** 07/18/23 **************************************
     C**** *LIKE         DEFINE    DEEOB         XXEOB
     C     *LIKE         DEFINE    DEEOBCD       XXEOB
     C**** End Chg *** 07/18/23 **************************************
     C     *LIKE         DEFINE    PSPOS         KKPOS
     C     *LIKE         DEFINE    CACPT4        HLCPT4
     C*** BEG ADD *** 04/04/00 ****************************************
     C     *LIKE         DEFINE    CALIN#        ##LIN#
     C*** END ADD *** 04/04/00 ****************************************
     C*** BEG ADD *** 06/19/00 ****************************************
     C     *LIKE         DEFINE    ESBLST        KKBLST
     C     *LIKE         DEFINE    EOCO#         KKCO#
     C     *LIKE         DEFINE    EOCRDT        HLCRDT
     C     *LIKE         DEFINE    EOCRDT        HLCSDT
     C     *LIKE         DEFINE    EOCRDT        HLRRDT
     C     *LIKE         DEFINE    EOCRDT        HLRSDT
     C*** END ADD *** 06/19/00 ****************************************
     C*** BEG ADD *** 02/14/01 ****************************************
     C     *LIKE         DEFINE    DIICD9        KKICD9
     C     *LIKE         DEFINE    DILOST        KKLOST
     C*** END ADD *** 02/14/01 ****************************************
     C*** BEGIN ADD *** 08/07/02 **************************************
     C     *LIKE         DEFINE    GZFRZP        KKFRZP
     C     *LIKE         DEFINE    GZEFDT        HLEFDT
     C*** END   ADD *** 08/07/02 **************************************
     C*** BEG ADD *** 04/17/07 ****************************************
     C     *LIKE         DEFINE    STKEY         @@KEY
     C*** END ADD *** 04/17/07 ****************************************
     C*** Beg Add *** 06/22/11 *****************************************
     C     *LIKE         DEFINE    STMFS         XXMFS
     C*** End Add *** 06/22/11 *****************************************
     C*** Beg Add *** 02/27/13 *************************************************
     C     *LIKE         DEFINE    FSCAT         KKCAT
     C     *LIKE         DEFINE    F1RGN#        KKRGN#
     C*** End Add *** 02/27/13 *************************************************
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
     C                   Z-ADD     SCFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFDAT
     C*
     C                   ENDSR
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
     C*
     C* DELETE EOB CODES/NOTES FOR THE LINE ITEM
     C*
     C     DEKY          CHAIN     PDE01                              99
     C     *IN99         DOWEQ     *OFF
     C                   DELETE    PDE01
     C     DEKY          READE     PDE01                                  99
     C                   ENDDO                                                  *IN99 = *OFF
     C*
     C*
     C* DELETE THE DETAIL AMOUNT TRANS RECORDS FOR LINE ITEM
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
     C*** BEG ADD *** 06/19/00 ****************************************
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
     C*** BEGIN ADD *** 07/05/02 **************************************
     C     EDEDI         IFEQ      'Y'
     C*** END   ADD *** 07/05/02 **************************************
     C                   SETON                                        9023
     C                   GOTO      CARTAG
     C*** BEGIN ADD *** 07/05/02 **************************************
     C                   ENDIF
     C*** END   ADD *** 07/05/02 **************************************
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
     C*** BEGIN ADD *** 07/05/02 **************************************
     C     EDEDI         IFEQ      'Y'
     C*** END   ADD *** 07/05/02 **************************************
     C                   SETON                                        9012
     C                   GOTO      CARTAG
     C*** BEGIN ADD *** 07/05/02 **************************************
     C                   ENDIF
     C*** END   ADD *** 07/05/02 **************************************
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
     C*** BEGIN CHG *** 07/05/02 **************************************
     C**********           MOVE EOCSDT    SRDTI
     C                   Z-ADD     SCCSDT        SRDTI
     C*** END   CHG *** 07/05/02 **************************************
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
     C*** BEGIN ADD *** 07/05/02 **************************************
     C     EDEDI         IFEQ      'Y'
     C*** END   ADD *** 07/05/02 **************************************
     C                   SETON                                        9023
     C                   GOTO      CARTAG
     C*** BEGIN ADD *** 07/05/02 **************************************
     C                   ENDIF
     C*** END   ADD *** 07/05/02 **************************************
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
     C*** BEGIN ADD *** 07/05/02 **************************************
     C     EDEDI         IFEQ      'Y'
     C*** END   ADD *** 07/05/02 **************************************
     C                   SETON                                        9012
     C                   GOTO      CARTAG
     C*** BEGIN ADD *** 07/05/02 **************************************
     C                   ENDIF
     C*** END   ADD *** 07/05/02 **************************************
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
     C                   MOVEL     EDEDI         SCEDI
     C                   EXFMT     SCR02
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C     SCEDI         IFEQ      *BLANKS
     C                   SETON                                        9017
     C                   GOTO      EDITAG
     C                   ENDIF                                                  SCEDI IFEQ BLANKS
     C*
     C     SCEDI         IFNE      *BLANKS
     C     SCEDI         ANDNE     'Y'
     C     SCEDI         ANDNE     'N'
     C                   SETON                                        9018
     C                   GOTO      EDITAG
     C                   ENDIF                                                  SCEDI IFNE BLANKS
     C*
     C     *IN90         IFEQ      *OFF
     C     EDKY          CHAIN     PED01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     SCEDI         EDEDI
     C                   UPDATE    PED01
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  *IN90=*OFF
     C                   ENDIF                                                  *INKL=*OFF
     C*
     C     EDIEND        ENDSR
     C*** END ADD *** 06/19/00 ****************************************
     C*** BEG ADD *** 04/15/04 ****************************************
     C*================================================================
     C*  EDITEB - EDIT FLORIDA EOB CODES
     C*================================================================
     C     EDITEB        BEGSR
     C*
     C                   MOVE      *BLANK        FNDEB1            1
     C                   MOVE      *BLANK        FNDEB2            1
     C*
     C                   SELECT
     C*
     C     SCPAID        WHENEQ    0
     C*
     C                   MOVE      'N'           FNDEB1
     C*
     C     DEKY          CHAIN(N)  PDE01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C**** Beg Chg *** 07/18/23 **************************************
     C****               MOVEL(P)  DEEOB         KKKEY
     C                   MOVEL(P)  DEEOBCD       KKKEY
     C**** End Chg *** 07/18/23 **************************************
     C     TBKY          CHAIN     PEB01                              99
     C     *IN99         IFEQ      *OFF
     C     DESTAT        ANDEQ     'A'
     C*
     C     EBXREF        LOOKUP    EOBR1                                  98
     C*
     C     *IN98         IFEQ      *ON
     C                   MOVE      'Y'           FNDEB1
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     DEKY          READE(N)  PDE01                                  99
     C*
     C                   ENDDO
     C*
     C     SCPAID        WHENNE    0
     C*
     C                   MOVE      'N'           FNDEB2
     C*
     C     DEKY          CHAIN(N)  PDE01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C**** Beg Chg *** 07/18/23 **************************************
     C****               MOVEL(P)  DEEOB         KKKEY
     C                   MOVEL(P)  DEEOBCD       KKKEY
     C**** End Chg *** 07/18/23 **************************************
     C     TBKY          CHAIN     PEB01                              99
     C     *IN99         IFEQ      *OFF
     C     DESTAT        ANDEQ     'A'
     C*
     C     EBXREF        LOOKUP    EOBR2                                  98
     C*
     C     *IN98         IFEQ      *ON
     C                   MOVE      'Y'           FNDEB2
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     DEKY          READE(N)  PDE01                                  99
     C*
     C                   ENDDO
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*** END ADD *** 04/15/04 ****************************************
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
    C*** Beg Add *** 06/27/19 ****************************************
     C                   if        not isPbillsLite
    C*** End Add *** 06/27/19 ****************************************
     C*
     C* Validate Claim Status (Adjudication) code
     C*
     C     SCCSCD        IFEQ      *BLANKS
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C                   SETON                                        9071
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCCSCD IFEQ BLANKS
     C*
     C                   MOVEL(P)  SCCSCD        KKKEY
     C     TBKY          CHAIN     PCS01                              99
     C     *IN99         IFEQ      *ON
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C                   SETON                                        9072
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C*
     C* Validate Date of Service
     C*
     C     SCFDAT        IFEQ      *ZEROS
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C                   SETON                                        9073
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCRXFD IFEQ ZEROS
     C*
     C                   Z-ADD     SCFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C                   SETON                                        9074
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     SRDTO         SCFDAT
     C                   ENDIF
     C*
     C* DATE OF SERVICE CANNOT BE LESS THAN ACCIDENT DATE
     C*
     C                   Z-ADD     SCFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLFDAT
     C*
     C     HLFDAT        IFLT      EOACDT
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C                   SETON                                        9075
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLFDAT IFLT HLACDT
     C*
     C* BASE THE EDIT OF PREVENTING FUTURE SERVICE DATES ON CONTROL
     C* FLAG SFBPF/FBAL04 POSTION 8 (BLANK = EDIT, Y = ALLOW FUTURE DATES)
     C*
     C*** BEGIN DEL *** 08/15/00 **************************************
     C**********           MOVE ' '       XXFSDF  1
     C**********           MOVEL'FB'      XSKEY
     C********** FBKY      CHAINSFB01                99
     C********** *IN99     IFEQ *OFF
     C********** 1         SUBSTFBAL04:8  XXFSDF
     C*** END   DEL *** 08/15/00 **************************************
     C*** BEGIN CHG *** 08/15/00 **************************************
     C********** XXFSDF    IFNE 'Y'
     C     ##SDTS        IFNE      'Y'
     C*** END   CHG *** 08/15/00 **************************************
     C*
     C     HLFDAT        IFGT      HLSYDT
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C                   SETON                                        9089
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLFDAT GT HLSYDT
     C*
     C*** BEG ADD *** 06/19/00 ****************************************
     C*  Give an error window if the date is a future date and the
     C*  data submission flag is 'Y'.
     C*
     C                   ELSE                                                   XXFSDF='Y'
     C     EDEDI         IFEQ      'Y'
     C     HLFDAT        ANDGT     HLSYDT
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C                   EXSR      EDIERR
     C                   SETON                                        90
     C                   GOTO      CK1END
     C                   ENDIF
     C*** END ADD *** 06/19/00 ****************************************
     C                   ENDIF                                                  XXFSDF = *BLANKS
     C*** BEGIN DEL *** 08/15/00 **************************************
     C**********           ENDIF                           *IN99 = *OFF
     C*** END   DEL *** 08/15/00 **************************************
     C*** BEG ADD *** 06/19/00 ****************************************
     C*
     C* Date of Service must be less than Carr Rcvd Dt
     C*     (If recon = 0)  FAEOP
     C*
     C* This is all conditioned on weather or not the EDI flag is on
     C*
     C*** BEGIN DEL *** 07/05/02 **************************************
     C********** EDEDI     IFEQ 'Y'
     C*** END   DEL *** 07/05/02 **************************************
     C     EORCN         IFEQ      0
     C     HLFDAT        ANDGT     EOCRDT
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C                   EXSR      CARERR
     C*** BEGIN ADD *** 07/05/02 **************************************
     C     EDEDI         IFEQ      'Y'
     C*** END   ADD *** 07/05/02 **************************************
     C                   SETON                                        90
     C                   GOTO      CK1END
     C*** BEGIN ADD *** 07/05/02 **************************************
     C                   ENDIF
     C*** END   ADD *** 07/05/02 **************************************
     C                   ENDIF                                                  EORCN=0
     C*
     C*     (If recon > 0)  FAESP
     C*
     C     EORCN         IFGT      0
     C                   MOVEL     'R'           KKBLST
     C     ESKY1         SETGT     PES01
     C     ESKY2         READPE    PES01                                  60
     C     *IN60         IFEQ      *OFF
     C*** BEGIN CHG *** 05/31/01 **************************************
     C********** HLFDAT    ANDGTESRRDT
     C     HLFDAT        IFGT      ESRRDT
     C*** END   CHG *** 05/31/01 **************************************
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C                   EXSR      CARERR
     C*** BEGIN ADD *** 07/05/02 **************************************
     C     EDEDI         IFEQ      'Y'
     C*** END   ADD *** 07/05/02 **************************************
     C                   SETON                                        90
     C                   GOTO      CK1END
     C*** BEGIN ADD *** 07/05/02 **************************************
     C                   ENDIF
     C*** END   ADD *** 07/05/02 **************************************
     C*** BEGIN ADD *** 05/31/01 **************************************
     C                   ENDIF
     C*** END   ADD *** 05/31/01 **************************************
     C                   ELSE                                                   *IN60
     C     EORCN         SUB       1             KKRCN
     C     ESKY1         SETGT     PES01
     C     ESKY2         READPE    PES01                                  60
     C     *IN60         IFEQ      *OFF
     C*** BEGIN CHG *** 05/31/01 **************************************
     C********** HLFDAT    ANDGEESRRDT
     C     HLFDAT        ANDGT     ESRRDT
     C*** END   CHG *** 05/31/01 **************************************
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C                   EXSR      CARERR
     C*** BEGIN ADD *** 07/05/02 **************************************
     C     EDEDI         IFEQ      'Y'
     C*** END   ADD *** 07/05/02 **************************************
     C                   SETON                                        90
     C                   GOTO      CK1END
     C*** BEGIN ADD *** 07/05/02 **************************************
     C                   ENDIF
     C*** END   ADD *** 07/05/02 **************************************
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  EORCN>0
     C*** BEGIN DEL *** 07/05/02 **************************************
     C**********           ENDIF                           EHEDI=Y
     C*** END   DEL *** 07/05/02 **************************************
     C*** END ADD *** 06/19/00 ****************************************
     C*** BEGIN ADD *** 08/07/02 **************************************
     C                   MOVEL     *BLANKS       SCZIPA
     C                   MOVEL     *BLANKS       SCZIPB
     C                   Z-ADD     *ZEROS        HLEFDT
     C*
     C* Let's get the zip area forn this line.
     C*
     C*** BEGIN ADD *** 01/07/03 **************************************
     C                   MOVEL(P)  EOFLIC        KKFRZP
     C     KKFRZP        IFEQ      *BLANKS
     C     KKFRZP        OREQ      '000'
     C*** END   ADD *** 01/07/03 **************************************
     C     P1KY          CHAIN     PP101                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  P1ZIP         KKFRZP
     C*** BEGIN ADD *** 01/07/03 **************************************
     C                   ENDIF
     C                   ENDIF
     C*
     C     KKFRZP        IFNE      *BLANKS
     C     KKFRZP        ANDNE     '000'
     C*** END   ADD *** 01/07/03 **************************************
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
     C*** END   ADD *** 08/07/02 **************************************
     C*
     C* VALIDATE PLACE OF SERVICE
     C*
     C*** BEG CHG *** 04/03/03 ****************************************
     C********** SCPOS     IFEQ *BLANKS
     C     SCPOS         IFEQ      *ZEROS
     C*** END CHG *** 04/03/03 ****************************************
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C*** BEG CHG *** 05/30/03 ****************************************
     C******************** SETON                     9076
     C                   SETON                                        9077
     C*** END CHG *** 05/30/03 ****************************************
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCPOS IFEQ BLANKS
     C*
     C                   MOVE(P)   SCPOS         KKPOS
     C     PSKY          CHAIN     PPS01                              99
     C     *IN99         IFEQ      *ON
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C                   SETON                                        9077
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C*
     C*** BEG ADD *** 02/08/08 ****************************************
     C*
     C     *IN99         IFEQ      *OFF
     C     *IN46         ANDEQ     *OFF
     C     PSSTAT        ANDNE     'A'
     C                   SETON                                        9025
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C*** END ADD *** 02/08/08 ****************************************
     C*
     C* VALIDATE TOOTH CODE
     C*
     C     SCPART        IFNE      *BLANKS
     C                   MOVE      'TOTH'        KKTBID
     C                   MOVEL(P)  SCPART        KKKEY1
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C*** BEG CHG *** 04/20/07 *****************************************
     C*****                SETON                     9067
     C                   SETON                                        9051
     C*** END CHG *** 04/20/07 *****************************************
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C*
     C                   ENDIF
     C*
     C* VERIFY THAT FEE SCHEDULE IS ON SYSTEM FOR SERVICE DATE
     C*
     C                   Z-ADD     HLFDAT        KKEFDT
     C                   MOVE      SCAST         KKST
     C                   MOVE      'D'           KKATYP
     C     RUKY1         SETGT     PRU01
     C     RUKY2         READPE    PRU01                                  99
     C     *IN99         IFEQ      *ON
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C                   SETON                                        9082
     C                   GOTO      CK1END
     C                   ENDIF
     C*** BEG ADD *** 02/14/01 ****************************************
     C*
     C* ONE DIAGNOSIS CODE MUST BE MARKED WITH AN 'X' IF ONE EXISTS
     C*
     C     *IN28         IFEQ      *ON
     C     SCDXR1        IFEQ      *BLANKS
     C     SCDXR2        ANDEQ     *BLANKS
     C     SCDXR3        ANDEQ     *BLANKS
     C     SCDXR4        ANDEQ     *BLANKS
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C                   SETON                                        9066
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
     C*
     C* VALIDATE DIAGNOSIS CODE CROSS REFERENCE FUND
     C*
     C     SCDXR1        IFNE      'X'
     C     SCDXR1        ANDNE     *BLANKS
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C                   SETON                                        9063
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C* VALIDATE DIAGNOSIS CODE CROSS REFERENCE FUND
     C*
     C     SCDXR2        IFNE      'X'
     C     SCDXR2        ANDNE     *BLANKS
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C                   SETON                                        9064
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C     SCDXR3        IFNE      'X'
     C     SCDXR3        ANDNE     *BLANKS
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C                   SETON                                        9085
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C     SCDXR4        IFNE      'X'
     C     SCDXR4        ANDNE     *BLANKS
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C                   SETON                                        9086
     C                   GOTO      CK1END
     C                   ENDIF
     C*** END ADD *** 02/14/01 ****************************************
     C*
     C* CHARGES ARE REQUIRED
     C*
     C* IF ADJUDICATION CODE IS OVERRIDE TYPE, ALLOW ZERO CHARGE AMOUNT
     C*
     C     CSOVRD        IFNE      'Y'
     C     SCCHRG        IFEQ      *ZEROS
     C                   SETON                                        9067
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCCHRG IFEQ ZEROS
     C                   ENDIF                                                  CSOBRD NE Y
     C*
     C* PROCEDURE CODE IS REQUIRED
     C*
     C                   MOVE      ' '           XXRCFL            1
     C*
     C     SCPRCD        IFEQ      *BLANKS
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C                   SETON                                        9083
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCPRCD IFEQ BLANKS
     C*** Beg Add *** 06/21/12 ****************************************
      /Free
         //If State is 'HL and specialty code <> ASC
         If Eoast = 'HL';

              Chain scprcd phl03;
                 If not %Found(Fmhll1);
                   *in84 = *on;
                   *in90 = *on;
                 Else;

                   If H3Stat = 'I';
                     *in57 = *on;
                     *in90 = *on;
                   Endif;
                   scDesc = %subst(h3Desc:1:25);
                   xxrcfl = 'C';
                 Endif;

         //VALIDATE BILLED CPT4 CODE
              If scBlcd <> *blanks;
                 Chain scblcd phl03;
                   If not %Found(Fmhll1);
                     *in17 = *on;
                     *in90 = *on;
                   Else;

                     If H3Stat = 'I';
                       *in16 = *on;
                       *in90 = *on;
                     Endif;
                   Endif;
              Else;
                scblcd = scprcd;
              Endif;
           Else;
      /End-Free
     C*** End Add *** 06/21/12 ****************************************
     C*
     C                   MOVEL(P)  SCPRCD        KKPRCD
     C                   Z-ADD     SCFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         KKFDAT
     C*
     C* VERIFY THAT THERE IS A FEE SCHEDULE ON THE SYSTEM FOR THE
     C* SERVICE DATE
     C*
     C                   MOVEL     SCAST         KKST
     C                   MOVE      'D'           KKATYP
     C                   Z-ADD     KKFDAT        KKEFDT
     C     RUKY1         SETGT     PRU01
     C     RUKY2         READPE    PRU01                                  99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9030
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C*** Bed Del *** 11/09/13 ****************************************
     C*** Bed Add *** 06/21/12 ****************************************
      /Free
        // Endif;
      /End-Free
     C*** End Add *** 06/21/12 ****************************************
     C*** End Del *** 11/09/13 ****************************************
     C*** BEGIN ADD *** 01/14/03 **************************************
     C*
     C* IF EDI THEN MUST BE DENTAL CODE
     C*
     C***** Beg Del ***** 05/14/14 ******************************
     C*****EDEDI         IFEQ      'Y'
     C*****SCPRCD        IFLT      'D0001'
     C*****SCPRCD        ORGT      'D9999'
     C*****              SETON                                        8790
     C*****              GOTO      CK1END
     C*****              ENDIF
     C*****              ENDIF
     C***** End Del ***** 05/14/14 ******************************
     C*** END   ADD *** 01/14/03 **************************************
     C*
     C* VALIDATE CPT4 CODE
     C*
     C*** BEG ADD *** 02/27/13 ****************************************
     C     EOAST         IFEQ      'IL'
     C     KKFDAT        IFLT      1120101
     C     FSKY2         SETGT     PFS01
     C     FSKY3         READPE    PFS01                                  99
     C  N99FSSTAT        IFEQ      'I'
     C                   SETON                                        9057
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     FSDSC1        SCDESC
     C                   ENDIF
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9084
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ELSE
     C*
     C     F1KY          SETGT     PF101
     C     F1KY1         READPE    PF101                                  99
     C  N99F1STAT        IFEQ      'I'
     C                   SETON                                        9057
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     F1DSC1        SCDESC
     C                   ENDIF
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9084
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*** END ADD *** 02/27/13 ****************************************
     C*** Beg Add *** 06/22/11 ****************************************
     C*** CHAIN IN  - FTRVP
     C     SCAST         IFEQ      'P1'
     C     XXMFS         ANDEQ     'Y'
     C*
     C     RVKY1         SETGT     PRV01
     C     RVKY2         READPE    PRV01                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C     RVSTAT        IFEQ      'I'
     C     RVKY2         READPE    PRV01                                  99
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9057
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL(P)  RVDESC        SCDESC
     C                   MOVEL     'M'           XXRCFL
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE                                                   AST = 'P1'
     C*** End Add *** 06/22/11 ****************************************
     C*** Beg Add *** 02/27/13 *************************************************
     C     EOAST         IFEQ      'IL'
     C     KKFDAT        IFLT      1120101
     C     FSKY2         SETGT     PFS01
     C     FSKY3         READPE    PFS01                                  99
     C                   ELSE
     C     F1KY          SETGT     PF101
     C     F1KY1         READPE    PF101                                  99
     C                   ENDIF
     C                   ELSE
     C*** End Add *** 02/27/13 *************************************************
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  99
     C*** Beg Add *** 02/27/13 *************************************************
     C                   ENDIF
     C*** End Add *** 02/27/13 *************************************************
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C* GO TO REASONABLE AND CUTORMARY FILE IF CPT4 EFFECTIVE DATE IS
     C* LESS THAN FEE SCHEDULE EFFECTIVE DATE
     C*
     C*** Beg Add *** 02/27/13 *************************************************
     C     EOAST         IFEQ      'IL'
     C     KKFDAT        IFLT      1120101
     C     FSEFDT        IFLT      RUEFDT
     C                   SETON                                        99
     C                   ENDIF                                                  FSEFDT < RUEFDT
     C                   ELSE
     C     F1EFDT        IFLT      RUEFDT
     C                   SETON                                        99
     C                   ENDIF                                                  F1EFDT < RUEFDT
     C                   ENDIF
     C                   ELSE
     C*** End Add *** 02/27/13 *************************************************
     C     PREFDT        IFLT      RUEFDT
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C                   SETON                                        99
     C                   ENDIF                                                  PREFDT < RUEFDT
     C*** Beg Add *** 02/27/13 *************************************************
     C                   ENDIF
     C*** End Add *** 02/27/13 *************************************************
     C*
     C  N99PRSTAT        IFEQ      'I'
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C                   SETON                                        9057
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99  = 'I'
     C*
     C     *IN99         IFEQ      *OFF
     C*** Beg Add *** 04/08/13 ****************************************
     C     CARCFL        ANDNE     'Y'
     C*** End Add *** 04/08/13 ****************************************
     C*** Beg Add *** 02/27/13 ****************************************
     C     EOAST         IFEQ      'IL'
     C     KKFDAT        IFLT      1120101
     C                   MOVEL(P)  FSDSC1        SCDESC
     C                   ELSE
     C                   MOVEL(P)  F1DSC1        SCDESC
     C                   ENDIF
     C                   ELSE
     C*** End Add *** 02/27/13 ****************************************
     C                   MOVEL(P)  PRDSC1        SCDESC
     C*** Beg Add *** 02/27/13 ****************************************
     C                   ENDIF
     C*** End Add *** 02/27/13 ****************************************
     C                   MOVE      'C'           XXRCFL
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C     *IN99         IFEQ      *ON
     C*** BEG ADD *** 04/17/07 ****************************************
     C* If State flag STBS02='N' then don't check R&C file
     C     ##RC          IFEQ      'N'
     C                   SETON                                        9084
     C                   GOTO      CK1END
     C                   ELSE
     C*** END ADD *** 04/17/07 ****************************************
     C*** BEG ADD *** 09/20/10 ****************************************
     C     SCAST         IFEQ      'IL'
     C                   MOVEL     EOFLIC        KKZIPA
     C                   ELSE
     C*** END ADD *** 09/20/10 ****************************************
     C                   MOVEL     SCZIPA        KKZIPA
     C*** BEG ADD *** 09/20/10 ****************************************
     C                   ENDIF
     C*** END ADD *** 09/20/10 ****************************************
     C                   MOVEL(P)  SCPRCD        KKCPT4
     C                   MOVEL     KKFDAT        KKEFDT
     C     RCKY1         SETGT     PRC01
     C     RCKY2         READPE    PRC01                                  99
     C*** BEG ADD *** 04/17/07 ****************************************
     C                   ENDIF
     C*** END ADD *** 04/17/07 ****************************************
     C*
     C     *IN99         IFEQ      *ON
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C                   SETON                                        9084
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C*
     C     *IN99         IFEQ      *OFF
     C     RCSTAT        ANDEQ     'I'
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C                   SETON                                        9057
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  RCDSC1        SCDESC
     C                   MOVE      'Y'           XXRCFL
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ENDIF
     C*
     C* VERIFY THAT THE PROCECURE CODE IS EFFECTIVE FOR THE FEE SCHEDULE
     C*
     C     XXRCFL        IFEQ      'C'
     C*** Beg Add *** 02/27/13 *************************************************
     C     EOAST         IFEQ      'IL'
     C     KKFDAT        IFLT      1120101
     C     FSEFDT        IFLT      RUEFDT
     C     *IN46         ANDEQ     *OFF
     C                   SETON                                        9042
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ELSE
     C     F1EFDT        IFLT      RUEFDT
     C     *IN46         ANDEQ     *OFF
     C                   SETON                                        9042
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     C*** End Add *** 02/27/13 *************************************************
     C     PREFDT        IFLT      RUEFDT
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C                   SETON                                        9042
     C                   GOTO      CK1END
     C                   ENDIF
     C*** Beg Add *** 02/27/13 *************************************************
     C                   ENDIF
     C*** End Add *** 02/27/13 *************************************************
     C                   ENDIF
     C*** End Add *** 06/22/11 ****************************************
     C                   ENDIF                                                  AST = 'P1'
     C*** End Add *** 06/22/11 ****************************************
     C*
     C*** VALIDATE BILLED PROCEDURE CODE
     C*
     C                   MOVEL(P)  SCBLCD        KKPRCD
     C*** BEG DEL *** 08/23/02 ****************************************
     C**********           Z-ADDSCFDAT    SRDTI
     C**********           MOVE *BLANKS   SRDTO
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     KKFDAT    P
     C**********
     C**VERIFY*THAT THERE IS A FEE SCHEDULE ON THE SYSTEM FOR THE
     C**SERVICE*DATE
     C**********
     C**********           MOVELSCAST     KKST
     C**********           MOVE 'D'       KKATYP
     C**********           Z-ADDKKFDAT    KKEFDT
     C********** RUKY1     SETGTPRU01
     C********** RUKY2     REDPEPRU01                    99
     C********** *IN99     IFEQ *ON
     C**********           SETON                     9030
     C**********           GOTO CK1END
     C**********           ENDIF                           *IN99 = *ON
     C*** END DEL *** 08/23/02 ****************************************
     C*
     C* VALIDATE BILLED CPT4 CODE
     C*
     C     SCBLCD        IFNE      *BLANKS
     C*
     C*** Beg Add *** 06/22/11 ****************************************
     C*** CHAIN IN  - FTRVP
     C     SCAST         IFEQ      'P1'
     C     XXMFS         ANDEQ     'Y'
     C*
     C     RVKY1         SETGT     PRV01
     C     RVKY2         READPE    PRV01                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C     RVSTAT        IFEQ      'I'
     C     RVKY2         READPE    PRV01                                  99
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9057
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL(P)  RVDESC        SCDESC
     C                   MOVEL     'M'           XXRCFL
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE                                                   AST = 'P1'
     C*** Beg Add *** 02/27/13 *************************************************
     C     EOAST         IFEQ      'IL'
     C     KKFDAT        IFLT      1120101
     C     FSKY2         SETGT     PFS01
     C     FSKY3         READPE    PFS01                                  99
     C                   ELSE
     C     F1KY          SETGT     PF101
     C     F1KY1         READPE    PF101                                  99
     C                   ENDIF
     C                   ELSE
     C*** End Add *** 02/27/13 *************************************************
     C*** End Add *** 06/22/11 ****************************************
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  99
     C*** Beg Add *** 02/27/13 *************************************************
     C                   ENDIF
     C*** End Add *** 02/27/13 *************************************************
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C* GO TO REASONABLE AND CUSTOMARY FILE IF CPT4 EFFECTIVE DATE IS
     C* LESS THAN FEE SCHEDULE EFFECTIVE DATE
     C*
     C*** Beg Add *** 02/27/13 *************************************************
     C     EOAST         IFEQ      'IL'
     C     KKFDAT        IFEQ      1120101
     C     FSEFDT        IFLT      RUEFDT
     C                   SETON                                        99
     C                   ENDIF                                                  FSEFDT < RUEFDT
     C                   ELSE
     C     F1EFDT        IFLT      RUEFDT
     C                   SETON                                        99
     C                   ENDIF                                                  F1EFDT < RUEFDT
     C                   ENDIF
     C                   ELSE
     C*** End Add *** 02/27/13 *************************************************
     C     PREFDT        IFLT      RUEFDT
     C                   SETON                                        99
     C                   ENDIF                                                  PREFDT < RUEFDT
     C*** Beg Add *** 02/27/13 *************************************************
     C                   ENDIF
     C*** End Add *** 02/27/13 *************************************************
     C*
     C  N99PRSTAT        IFEQ      'I'
     C*** BEG CHG *** 08/23/02 ****************************************
     C******************** SETON                     9015
     C                   SETON                                        9017
     C*** END CHG *** 08/23/02 ****************************************
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99  = 'I'
     C*
     C*** BEG DEL *** 08/23/02 ****************************************
     C********** *IN99     IFEQ *OFF
     C**********           MOVELPRDSC1    SCDESC    P
     C**********           MOVE 'C'       XXRCFL
     C**********           ENDIF                           *IN99 = *OFF
     C*** END DEL *** 08/23/02 ****************************************
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C     *IN99         IFEQ      *ON
     C*
     C*** BEG ADD *** 09/20/10 ****************************************
     C     SCAST         IFEQ      'IL'
     C                   MOVEL     EOFLIC        KKZIPA
     C                   ELSE
     C*** END ADD *** 09/20/10 ****************************************
     C*
     C                   MOVEL     SCZIPA        KKZIPA
     C*** BEG ADD *** 09/20/10 ****************************************
     C                   ENDIF
     C*** END ADD *** 09/20/10 ****************************************
     C                   MOVEL(P)  SCBLCD        KKCPT4
     C                   MOVEL     KKFDAT        KKEFDT
     C     RCKY1         SETGT     PRC01
     C     RCKY2         READPE    PRC01                                  99
     C*
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9016
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C*
     C     *IN99         IFEQ      *OFF
     C     RCSTAT        ANDEQ     'I'
     C*** BEG CHG *** 08/23/02 ****************************************
     C******************** SETON                     9015
     C                   SETON                                        9017
     C*** END CHG *** 08/23/02 ****************************************
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C*
     C*** BEG DEL *** 08/23/02 ****************************************
     C********** *IN99     IFEQ *OFF
     C**********           MOVELRCDSC1    SCDESC    P
     C**********           MOVE 'Y'       XXRCFL
     C**********           ENDIF                           *IN99 = *OFF
     C*** END DEL *** 08/23/02 ****************************************
     C*
     C                   ENDIF
     C*** Beg Add *** 06/22/11 ****************************************
     C                   ENDIF                                                  AST = 'P1'
     C*** End Add *** 06/22/11 ****************************************
     C*** BEGIN ADD *** 01/14/03 **************************************
     C                   ELSE
     C                   MOVEL(P)  SCPRCD        SCBLCD
     C*** END   ADD *** 01/14/03 **************************************
     C*
     C                   ENDIF
     C*** BEGIN ADD *** 01/14/03 **************************************
     C*** Bed Chg *** 11/09/13 ****************************************
      /Free
         Endif;
      /End-Free
     C*** End Chg *** 11/09/13 ****************************************
     C*
     C* IF EDI THEN MUST BE DENTAL CODE
     C*
     C***** Beg Del ***** 05/14/14 ******************************
     C*****EDEDI         IFEQ      'Y'
     C*****SCBLCD        IFLT      'D0001'
     C*****SCBLCD        ORGT      'D9999'
     C*****              SETON                                        8890
     C*****              GOTO      CK1END
     C*****              ENDIF
     C*****              ENDIF
     C***** End Del ***** 05/14/14 ******************************
     C*** END   ADD *** 01/14/03 **************************************
     C*** BEG DEL *** 08/23/02 ****************************************
     C**********
     C**VERIFY*THAT THE PROCECURE CODE IS EFFECTIVE FOR THE FEE SCHEDULE
     C**********
     C********** XXRCFL    IFEQ 'C'
     C********** PREFDT    IFLT RUEFDT
     C**********           SETON                     9042
     C**********           GOTO CK1END
     C**********           ENDIF
     C**********           ENDIF
     C*** END DEL *** 08/23/02 ****************************************
     C*
     C* TYPE CODE #1 REQUIRED IF CORRESPONDING AMOUNT IS ENTERED
     C*
     C     SCOTHR        IFNE      *ZEROS
     C     SCOT1C        ANDEQ     *BLANKS
     C                   SETON                                        9078
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
     C                   SETON                                        9079
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C                   ENDIF                                                  SCOT1C IFNE *BLANKS
     C*
     C* TYPE CODE #2 REQUIRED IF CORRESPONDING AMOUNT IS ENTERED
     C*
     C     SCOTH2        IFNE      *ZEROS
     C     SCOT2C        ANDEQ     *BLANKS
     C                   SETON                                        9080
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
     C                   SETON                                        9081
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
     C*** BEGIN ADD *** 01/11/00 **************************************
     C     SCCSCD        ANDNE     '300'
     C*** END   ADD *** 01/11/00 **************************************
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
     C*** BEGIN ADD *** 01/11/00 **************************************
     C     SCCSCD        ANDNE     '300'
     C*** END   ADD *** 01/11/00 **************************************
     C     SCCHRG        SUB       SCMRA         SCPAID
     C                   SUB       SCOTHR        SCPAID
     C                   SUB       SCOTH2        SCPAID
     C                   SUB       SCNTWK        SCPAID
     C                   ENDIF                                                  SCCHRG NE *ZEROS
     C*** BEGIN ADD *** 01/11/00 **************************************
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
     C*** END   ADD *** 01/11/00 **************************************
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
     C*** BEG ADD *** 10/31/01 ****************************************
     C     CSOVRP        ANDLT     100
     C*** END ADD *** 10/31/01 ****************************************
     C*** BEGIN DEL *** 03/26/01 **************************************
     C********** CSOVRP    ANDEQ*ZEROS
     C*** END   DEL *** 03/26/01 **************************************
     C*** BEG ADD *** 04/15/04 ****************************************
     C*
     C     SCAST         IFEQ      'FL'
     C     SCPAID        ANDLT     SCCHRG
     C                   EXSR      EDITEB
     C*
     C     FNDEB1        IFEQ      'N'
     C     FNDEB2        OREQ      'N'
     C*
     C     FNDEB1        IFEQ      'N'
     C                   EXFMT     EB1WDW
     C                   ENDIF
     C     FNDEB2        IFEQ      'N'
     C                   EXFMT     EB2WDW
     C                   ENDIF
     C*
     C                   Z-ADD     @@RRN2        ##RRN2
     C                   Z-ADD     @@RRN3        ##RRN3
     C*
     C     @@MODE        IFEQ      'A'
     C                   Z-ADD     SCSEQ#        ##DSEQ            5 0
     C                   Z-ADD     0             ##RRN3
     C                   Z-ADD     SCSEQ#        KKDSEQ
     C                   ELSE
     C                   Z-ADD     0             ##DSEQ
     C                   ENDIF                                                  @@MODE = 'A'
     C*
     C                   CALL      'F0356R01'
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      'A'           ##MODE
     C                   PARM      *ZEROS        ##RRN
     C                   PARM                    ##RRN2
     C                   PARM                    ##RRN3
     C                   PARM                    ##DSEQ
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*** END ADD *** 04/15/04 ****************************************
     C*** BEGIN ADD *** 03/26/01 **************************************
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
     C*** END   ADD *** 03/26/01 **************************************
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
     C*** BEG ADD *** 08/27/99 ****************************************
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
     C*** BEGIN ADD *** 01/07/02 **************************************
     C     DERCN         IFEQ      EORCN
     C*** END   ADD *** 01/07/02 **************************************
     C                   DELETE    PDE01
     C*** BEGIN ADD *** 01/07/02 **************************************
     C                   ELSE
     C                   EXCEPT    RPDE01
     C                   ENDIF
     C*** END   ADD *** 01/07/02 **************************************
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
     C*** BEGIN ADD *** 01/11/00 **************************************
     C     MGMSG#        IFNE      'NW00001'
     C*** END   ADD *** 01/11/00 **************************************
     C                   DELETE    PMG01
     C*** BEGIN ADD *** 01/11/00 **************************************
     C                   ELSE
     C*
     C                   UPDATE    PMG01
     C*
     C                   ENDIF
     C*** END   ADD *** 01/11/00 **************************************
     C                   ELSE
     C                   EXCEPT    RPMG01
     C                   ENDIF                                                  MGSRC = 'S'
     C     MGKY          READE     PMG01                                  99
     C                   ENDDO
     C*** END ADD *** 08/27/99 ****************************************
     C*
     C                   CALL      'F0356R01'
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      'A'           ##MODE
     C                   PARM      *ZEROS        ##RRN
     C                   PARM                    ##RRN2
     C                   PARM                    ##RRN3
     C                   PARM                    ##DSEQ
     C*
     C*** BEG ADD *** 04/02/00 ****************************************
     C     ##KL          IFEQ      '1'
     C                   SETON                                        14
     C                   SETON                                        90
     C                   SETOFF                                       69
     C                   ELSE
     C                   SETON                                        69
     C                   SETOFF                                       14
     C                   ENDIF
     C*** END ADD *** 04/02/00 ****************************************
     C     SCMRA         IFNE      *ZEROS
     C     SCNTWK        ORNE      *ZEROS
     C     SCOTHR        ORNE      *ZEROS
     C     SCOTH2        ORNE      *ZEROS
     C*
     C*** BEGIN CHG *** 03/26/01 **************************************
     C**********           MOVE *BLANKS   XXEOB
     C********** DEKY      CHAINPDE01               N99
     C********** *IN99     DOWEQ*OFF
     C*
     C********** DEEOB     IFNE *BLANKS
     C**********           MOVE DEEOB     XXEOB
     C**********           LEAVE
     C**********           ENDIF                           DEEOB NE *BLANKS
     C*
     C********** DEKY      READEPDE01               N    99
     C**********           ENDDO                           *IN99 = *OFF
     C*
     C********** XXEOB     IFEQ *BLANKS
     C**********           SETON                     90
     C**********           ENDIF                           XXEOB = *BLANKS
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
     C*** END   CHG *** 03/26/01 **************************************
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C*** BEGIN ADD *** 03/26/01 **************************************
     C                   ENDIF                                                  GO = *YES
     C*** END   ADD *** 03/26/01 **************************************
     C                   ENDIF                                                  *IN99 = *OFF
     C*** BEG DEL *** 02/24/00 ****************************************
     C*
     C**CLEAR*PAID AMOUNT AND REDUCTIONS FIELDS IF THE ADJUDICATION
     C**CODE*IS NOT OVERRIDE TYPE.  ADJUDICATION PROGRAMS AND NETWORK
     C**PROGRAMS WILL DETERMINE THE NECESSARY REDUCTIONS.
     C*********
     C*********  *IN99     IFEQ *OFF
     C*********  CSOVRD    ANDNE'Y'
     C*********            Z-ADD0         SCPAID
     C*********            Z-ADD0         SCMRA
     C*********            Z-ADD0         SCOTHR
     C*********            Z-ADD0         SCOTH2
     C*********            Z-ADD0         SCNTWK
     C*********            ENDIF
     C*** END DEL *** 02/24/00 ****************************************
    C*** Beg Add *** 06/27/19 ****************************************
     C                   Endif
    C*** End Add *** 06/27/19 ****************************************
     C*
     C* IF IN CHANGE MODE, THEN SET 'OUTFLG' TO END PROGRAM
     C*
     C     @@MODE        IFEQ      'C'
     C                   MOVE      '1'           OUTFLG
     C                   ENDIF
     C*
     C     CK1END        ENDSR                                                  SRERR IFEQ 1
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
     C* KADO - F01=REPEAT
     C*================================================================
     C     KADO          BEGSR
     C*
     C     SCLIN#        IFGT      0
     C     SCLIN#        SUB       1             KKLIN#
     C     CAKY4         CHAIN     PCA04                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     CADSEQ        KKDSEQ
     C     DDKY          CHAIN     PDD01                              99
     C     *IN99         IFEQ      *OFF
     C*** BEG ADD *** 04/25/03 ****************************************
     C*
     C* POSITION CURSOR TO DATE OF SERVICE
     C*
     C                   SETON                                        41
     C*** END ADD *** 04/25/03 ****************************************
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
     C                   MOVEL(P)  DDPART        SCPART
     C                   MOVEL(P)  DDPOS         SCPOS
     C                   MOVEL(P)  DDPRCD        SCPRCD
     C*** BEG ADD *** 09/30/99 ****************************************
     C*** BEG DEL *** 04/25/03 ****************************************
     C******************** MOVELDDBPRC    SCBLCD    P
     C*** END DEL *** 04/25/03 ****************************************
     C*** END ADD *** 09/30/99 ****************************************
     C*** BEG DEL *** 04/25/03 ****************************************
     C******************** MOVELDDPRDS    SCDESC    P
     C*** END DEL *** 04/25/03 ****************************************
     C*
     C                   MOVE(P)   SCPOS         KKPOS
     C     PSKY          CHAIN     PPS01                              99
     C  N99              MOVEL(P)  PSDESC        SCPODS
     C*
     C                   MOVEL(P)  'TOTH'        KKTBID
     C                   MOVEL(P)  SCPART        KKKEY1
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCTDES
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C*** BEG ADD *** 02/16/01 ****************************************
     C   28              MOVEL     DDDXR1        SCDXR1
     C   61              MOVEL     DDDXR2        SCDXR2
     C   62              MOVEL     DDDXR3        SCDXR3
     C   45              MOVEL     DDDXR4        SCDXR4
     C   52              MOVE      'X'           SCDXR1
     C*** END ADD *** 02/16/01 ****************************************
     C*
     C                   Z-ADD     CACHRG        SCCHRG
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
     C*** BEG DEL *** 11/12/99 ****************************************
     C*  CALL FA713R WHICH WILL CALL APPROPRIATE ADJUDICATION PROGRAMS
     C********************
     C********************
     C******************** Z-ADD@@RRN2    ##RRN
     C******************** CALL 'FA713R'
     C******************** PARM ' '       ##KC
     C******************** PARM ' '       ##KL
     C******************** PARM           ##RRN
     C********************
     C**WRITE/UPDATE*EOMB*DETAIL AMOUNT TRANSACTION FILE
     C********************
     C***********CAKY3**** CHAINPCA03                99
     C************IN99**** DOWEQ*OFF
     C******************** Z-ADD0         SMCHG
     C******************** Z-ADD0         SMMRA
     C******************** Z-ADD0         SMNDIS
     C******************** Z-ADD0         SMOTHR
     C******************** Z-ADD0         SMOTH2
     C******************** Z-ADD0         SMPAID
     C******************** Z-ADDCADSEQ    KKDSEQ
     C***********DAKY***** CHAINPDA02                98
     C************IN98**** DOWEQ*OFF
     C******************** ADD  D2CHRG    SMCHG
     C******************** ADD  D2MRA     SMMRA
     C******************** ADD  D2NDIS    SMNDIS
     C******************** ADD  D2OTHR    SMOTHR
     C******************** ADD  D2OTH2    SMOTH2
     C******************** ADD  D2PAID    SMPAID
     C***********DAKY***** READEPDA02                    98
     C******************** ENDDO                           *IN98 = *OFF
     C********************
     C**COMPARE*THE*DETAIL CHARGE AMOUNTS WITH SUM OF DETAIL AMOUNTS
     C**IF*THERE*IS*A*DIFFERENCE, THEN WRITE OR UPDATE DETAIL AMONT REC
     C********************
     C***********CACHRG*** SUB  SMCHG     DFCHRG
     C***********CAMRA**** SUB  SMMRA     DFMRA
     C***********CANDIS*** SUB  SMNDIS    DFNDIS
     C***********CAOTHR*** SUB  SMOTHR    DFOTHR
     C***********CAOTH2*** SUB  SMOTH2    DFOTH2
     C***********CAPAID*** SUB  SMPAID    DFPAID
     C********************
     C***********DFCHRG*** IFNE *ZEROS
     C***********DFMRA**** ORNE *ZEROS
     C***********DFNDIS*** ORNE *ZEROS
     C***********DFOTHR*** ORNE *ZEROS
     C***********DFOTH2*** ORNE *ZEROS
     C***********DFPAID*** ORNE *ZEROS
     C********************
     C**UPDATE*IF*THERE*IS A FDDAP FOR THE CURRENT RECON
     C**ELSE*WRITE*NEW*FDDAP RECORD TO REFLECT CHANGES
     C********************
     C***********DAKY1**** CHAINPDA01                97
     C************IN97**** IFEQ *OFF
     C******************** Z-ADDCACHRG    DACHRG
     C******************** Z-ADDCAMRA     DAMRA
     C******************** Z-ADDCANDIS    DANDIS
     C******************** Z-ADDCAOTHR    DAOTHR
     C******************** Z-ADDCAOTH2    DAOTH2
     C******************** Z-ADDCAPAID    DAPAID
     C******************** TIME           DSDTTM
     C******************** MOVELQ1PGM     DAPGM     P
     C******************** MOVELQ1USER    DAUSER    P
     C******************** Z-ADDDSTIME    DATIME
     C******************** Z-ADDDSDATE    SRDTI
     C******************** MOVE *BLANKS   SRDTO
     C******************** MOVE *BLANKS   SRERR
     C******************** CALL 'SRCVTDAT'DTCYMD
     C******************** MOVELSRDTO     DADT
     C******************** UPDATPDA01
     C********************
     C******************** ELSE
     C********************
     C******************** CLEARPDA01
     C******************** Z-ADD##CO#     DACO#
     C******************** Z-ADDKKSUB#    DASUB#
     C******************** Z-ADDKKFND     DAFND
     C******************** Z-ADDKKFYR     DAFYR
     C******************** Z-ADDKKCASE    DACASE
     C******************** MOVELEOSS#     DASS#
     C******************** Z-ADDEOACDT    DAACDT
     C******************** Z-ADDKKEOB#    DAEOB#
     C******************** Z-ADDKKDSEQ    DADSEQ
     C******************** Z-ADDKKRCN     DARCN#
     C******************** TIME           DSDTTM
     C******************** Z-ADDDSDATE    SRDTI
     C******************** MOVE *BLANKS   SRDTO
     C******************** MOVE *BLANKS   SRERR
     C******************** CALL 'SRCVTDAT'DTCYMD
     C******************** MOVELSRDTO     DATRDT
     C******************** Z-ADDDFCHRG    DACHRG
     C******************** Z-ADDDFMRA     DAMRA
     C******************** Z-ADDDFNDIS    DANDIS
     C******************** Z-ADDDFOTHR    DAOTHR
     C******************** Z-ADDDFOTH2    DAOTH2
     C******************** Z-ADDDFPAID    DAPAID
     C******************** MOVE 'A'       DASTAT
     C******************** MOVELQ1PGM     DAPGM     P
     C******************** MOVELQ1PGM     DAIPGM    P
     C******************** MOVELQ1USER    DAUSER    P
     C******************** MOVELQ1USER    DAIUSR    P
     C******************** Z-ADDDSTIME    DATIME
     C******************** Z-ADDDSTIME    DAITIM
     C******************** MOVELSRDTO     DADT
     C******************** MOVELSRDTO     DAIDT
     C******************** WRITEPDA01
     C******************** ENDIF                           *IN97 = *OFF
     C********************
     C******************** ENDIF                           DFCHRG NE *ZEROS
     C***********CAKY3**** READEPCA03                    99
     C******************** ENDDO                           *IN99 = *OFF
     C*** END DEL *** 11/12/99 ****************************************
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
     C*** Beg Add *** 02/27/13 *************************************************
     C     CARCFL        IFEQ      'C'
     C     SCAST         ANDEQ     'IL'
    C*
     C     SCAST         IFEQ      'IL'
     C                   MOVEL     EOFLIC        KKZIPA
     C                   ELSE
     C                   MOVEL     SCZIPA        KKZIPA
     C                   ENDIF
     C                   MOVEL(P)  SCPRCD        KKCPT4
     C                   MOVEL     KKFDAT        KKEFDT
     C     RCKY1         SETGT     PRC01
     C     RCKY2         READPE    PRC01                                  99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRN
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     RCRRN         ##RRN
     C                   ENDIF                                                  *IN99 = On
     C                   CALL      'F2547R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRN
    C*
     C                   ELSE
     C*** End Add *** 02/27/13 *************************************************
     C                   Z-ADD     SCFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         KKFDAT
     C*** BEG CHG *** 01/17/01 ****************************************
     C********** CARCFL    IFEQ 'C'
     C*** Beg Chg *** 04/08/13 ****************************************
     C**** DDRCFL        IFEQ      'C'
     C     CARCFL        IFEQ      'C'
     C*** End Chg *** 04/08/13 ****************************************
     C*** END CHG *** 01/17/01 ****************************************
     C                   MOVE      EOAST         ##ST              2
     C                   MOVEL(P)  SCPRCD        KKPRCD
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRN
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     PRRRN         ##RRN
     C                   ENDIF                                                  *IN99 = On
     C                   CALL      'F2305R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRN
     C                   PARM                    ##ST
     C*** Beg Add *** 02/27/13 *************************************************
     C                   ENDIF                                                  SCAST = 'IL'
     C*** End Add *** 02/27/13 *************************************************
     C                   ENDIF                                                  RCFP = 'C'
     C     CARCFL        IFEQ      'Y'
     C*** BEG ADD *** 09/20/10 ****************************************
     C     SCAST         IFEQ      'IL'
     C                   MOVEL     EOFLIC        KKZIPA
     C                   ELSE
     C*** END ADD *** 09/20/10 ****************************************
     C                   MOVEL     SCZIPA        KKZIPA
     C*** BEG ADD *** 09/20/10 ****************************************
     C                   ENDIF
     C*** END ADD *** 09/20/10 ****************************************
     C                   MOVEL(P)  SCPRCD        KKCPT4
     C                   MOVEL     KKFDAT        KKEFDT
     C     RCKY1         SETGT     PRC01
     C     RCKY2         READPE    PRC01                                  99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRN
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     RCRRN         ##RRN
     C                   ENDIF                                                  *IN99 = On
     C                   CALL      'F2310R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRN
     C                   PARM                    ##ST
     C                   ENDIF                                                  RCFP = 'Y'
     C*** Beg Add *** 06/22/11 ****************************************
     C     SCAST         IFEQ      'P1'
     C     XXMFS         ANDEQ     'Y'
     C     RVKY1         SETGT     PRV01
     C     RVKY2         READPE    PRV01                                  99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRN
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     RVRRN         ##RRN
     C                   ENDIF                                                  *IN99 = On
     C                   CALL      'F2366R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRN
     C*
     C                   ENDIF                                                  AST = 'P1'
     C*** End Add *** 06/22/11 ****************************************
     C                   SETON                                        90
     C     KIEND         ENDSR
     C*================================================================
     C* KJDO - F10=SEARCH.
     C*================================================================
     C     KJDO          BEGSR
     C*** BEGIN DEL *** 10/13/00 **************************************
     C*
     C*** INITIALIZE POSITION CURSOR INDICATORS
     C*
     C**********           SETOF                     313233
     C**********           SETOF                     343536
     C**********           SETOF                     373839
     C**********           SETOF                     404142
     C**********           SETOF                     484950
     C**********           SETOF                     535455
     C*** END   DEL *** 10/13/00 **************************************
     C*
     C                   SELECT
     C*
     C*  ADJUDICATION CODE SEARCH
     C*
     C     HFLD          WHENEQ    'SCCSCD'
     C*** BEGIN ADD *** 07/23/99 **************************************
     C     *IN44         ANDNE     *ON
     C*** END ADD *** 07/23/99 ****************************************
     C*
     C                   CALL      'F2300R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C                   PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C     ##KL          IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        9040      pos cursor
     C                   ENDIF                                                  ##KL IFEQ *ON
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PCS02                              99
     C                   MOVEL(P)  CSKEY         SCCSCD
     C                   MOVEL(P)  CSDESC        SCCSDS
     C                   SETON                                        9041      pos cursor
     C                   ENDIF                                                  ##RRN IFNE ZEROS
     C*
     C*  TOOTH CODE SEARCH
     C*
     C     HFLD          WHENEQ    'SCPART'
     C                   Z-ADD     *ZERO         ##RRN
     C                   MOVEL     SCPART        HLPART           15
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM      'TOTH'        ##TBID
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVEL(P)  HLPART        SCPART
     C                   SETON                                        9039      pos cursor
     C                   ELSE
     C     ##RRN         CHAIN     PGT02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTKEY         SCPART
     C                   MOVEL(P)  GTDESC        SCTDES
     C                   SETON                                        9038      pos cursor
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF
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
     C                   SETON                                        9031      pos cursor
     C                   ELSE
 CCC C                   MOVEL     ##POS         TPPOS             3
 AAA C                   MOVE      TPPOS         SCPOS
 CCC C                   MOVE(P)   SCPOS         KKPOS
     C     PSKY          CHAIN     PPS01                              99
     C  N99              MOVEL(P)  PSDESC        SCPODS
     C                   SETON                                        9032      pos cursor
     C                   ENDIF                                                  ##KC IFEQ 1
     C*
     C* PROCEDURE CODE SEARCH
     C*
     C     HFLD          WHENEQ    'SCPRCD'
     C*
     C                   MOVE      SCPRCD        HLCPT4
     C                   MOVEL(P)  SCDESC        HLDESC           40
     C*** Beg Add *** 06/21/12 ****************************************
      /Free

         If Eoast = 'HL';
           inCall= '2';
           inRrn = 0;
           inKc = *blanks;
           inKl = *blanks;

           Callp RtvPrcCd(inCall:inKc:inKl:inRrn);
             If inRrn <> 0;
                Chain inRrn phl02;
                  If %found(Fmhlp);
                    scPrcd = H2Code;
                    scDesc = %subst(H2Desc:1 :25);
                    *in34 = *on;
                    *in90 = *on;
                  Else;
                    ScPrcd = HlCpt4;
                    scDesc = HlDesc;
                    *in54 = *on;
                    *in90 = *on;
                  Endif;

             Else;
               scPrcd = HlCpt4;
               scDesc = HlDesc;
               *in54 = *on;
               *in90 = *on;
             Endif;

         Else;
      /End-Free
     C*** End Add *** 06/21/12 ****************************************
     C                   MOVE      *BLANKS       ##PROC            5
     C                   MOVE      *BLANKS       ##PROD           50
     C                   Z-ADD     0             ##RRN1            9 0
     C                   MOVE      SCAST         ##AST
     C*** BEG ADD *** 09/20/10 ****************************************
     C     SCAST         IFEQ      'IL'
     C*** Beg Chg *** 02/27/13 ****************************************
     C*******            CALL      'F2310R'
     C*******            PARM      '2'           ##CALL            1
     C*******            PARM      ' '           ##KC
     C*******            PARM      ' '           ##KL
     C*******            PARM                    ##RRN1
     C                   CALL      'F2547R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN1
     C                   PARM      '2'           ##VIEW            1
     C                   PARM      EORGN#        ##RGN             3 0
     C*** End Chg *** 02/27/13 ****************************************
     C     ##KL          IFEQ      '1'
     C     ##RRN1        OREQ      0
     C                   MOVEL     HLCPT4        SCPRCD
     C                   MOVEL(P)  HLDESC        SCDESC
     C                   SETON                                        9054      pos cursor
     C                   ELSE
     C*** Beg Chg *** 02/27/13 ****************************************
     C**** ##RRN1        CHAIN     PRC02                              99
     C**** *IN99         IFEQ      *OFF
     C****               MOVEL     RCCPT4        SCPRCD
     C****               MOVEL(P)  RCDSC1        SCDESC
     C****               SETON                                        9034
     C****               ENDIF                                                  *IN99 = *OFF
     C     ##RRN1        CHAIN     PF102                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     F1CODE        SCPRCD
     C                   MOVEL     F1DSC1        SCDESC
     C                   SETON                                        9034
     C                   ENDIF                                                  *IN99 = *OFF
     C*** End Chg *** 02/27/13 ****************************************
     C                   ENDIF                                                  ##KL = 1
     C                   ELSE                                                   SCAST = 'IL
     C*** END ADD *** 09/20/10 ****************************************
     C*** Beg Add *** 06/22/11 ****************************************
     C     SCAST         IFEQ      'P1'
     C     XXMFS         ANDEQ     'Y'
     C                   CALL      'F2366R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN1
     C     ##KL          IFEQ      '1'
     C     ##RRN1        OREQ      0
     C                   MOVEL     HLCPT4        SCPRCD
     C                   MOVEL(P)  HLDESC        SCDESC
     C                   SETON                                        9054      pos cursor
     C                   ENDIF                                                  ##KL = 1
     C     ##RRN1        IFNE      *ZEROS
     C     ##RRN1        CHAIN     PRV02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  RVPRCD        SCPRCD
     C                   MOVEL(P)  RVDESC        SCDESC
     C                   SETON                                        9034
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRB NE *ZEROS
     C                   ELSE                                                   SCAST = 'P1
     C*** End Add *** 06/22/11 ****************************************
     C                   CALL      'F2013R'
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN1
     C                   PARM                    ##PROC
     C                   PARM                    ##PROD
     C                   PARM      ' '           ##RTCD
     C                   PARM                    ##AST
     C*
     C     ##RTCD        IFEQ      '1'
     C     ##PROD        OREQ      *BLANKS
     C                   MOVEL     HLCPT4        SCPRCD
     C                   MOVEL(P)  HLDESC        SCDESC
     C                   SETON                                        9054      pos cursor
     C                   ELSE
     C                   MOVEL     ##PROC        SCPRCD
     C                   MOVEL(P)  ##PROD        SCDESC
     C                   SETON                                        9034
     C                   ENDIF                                                  ##RTCD = 1
     C*** Beg Add *** 06/22/11 ****************************************
     C                   ENDIF                                                  SCAST = 'P1'
     C*** End Add *** 06/22/11 ****************************************
     C*** BEG ADD *** 09/20/10 ****************************************
     C                   ENDIF                                                  SCAST = 'IL'
     C*** END ADD *** 09/20/10 ****************************************
     C*** BEGIN ADD *** 10/13/00 **************************************
     C*
     C*** Beg Add *** 06/21/12 ****************************************
      /Free
         Endif;
      /End-Free
     C*** End Add *** 06/21/12 ****************************************
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
     C                   SETON                                        90
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C                   OTHER
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCFDAT'
     C                   SETOFF                                       35
     C                   SETON                                        363743
     C     HFLD          WHENEQ    'SCBLCD'
     C                   SETOFF                                       363743
     C                   SETON                                        35
     C*** BEG ADD *** 02/14/01 ****************************************
     C     HFLD          WHENEQ    'SCDXR1'
     C*                    SETOF                     919394
     C                   SETON                                        92
     C     HFLD          WHENEQ    'SCDXR2'
     C*                    SETOF                     919294
     C                   SETON                                        93
     C     HFLD          WHENEQ    'SCDXR3'
     C*                    SETOF                     919293
     C                   SETON                                        94
     C     HFLD          WHENEQ    'SCDXR4'
     C*                    SETOF                     9394
     C                   SETON                                        9192
     C*** END ADD *** 02/14/01 ****************************************
     C     HFLD          WHENEQ    'SCCHRG'
     C                   SETOFF                                       353743
     C                   SETON                                        36
     C     HFLD          WHENEQ    'SCPAID'
     C                   SETOFF                                       353643
     C                   SETON                                        37
     C     HFLD          WHENEQ    'SCMRA'
     C                   SETOFF                                       353637
     C                   SETON                                        43
     C     HFLD          WHENEQ    'SCNTWK'
     C                   SETOFF                                       3743
     C                   SETON                                        3536
     C     HFLD          WHENEQ    'SCOTHR'
     C                   SETOFF                                       3543
     C                   SETON                                        3637
     C     HFLD          WHENEQ    'SCOTH2'
     C                   SETOFF                                       3536
     C                   SETON                                        3743
     C                   ENDSL
     C*
     C                   SETON                                        90
     C*** END   ADD *** 10/13/00 **************************************
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
     C     @@MODE        IFEQ      'C'
     C     @@MODE        OREQ      'A'
     C                   MOVE      ' '           ##CALL
     C                   ELSE
     C                   MOVE      '1'           ##CALL
     C                   ENDIF                                                  @@MODE = 'C'
     C                   CALL      'F0356R'
     C                   PARM                    ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
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
     C* KPDO - F14=DISPLAY LINES
     C*================================================================
     C     KPDO          BEGSR
     C                   Z-ADD     @@RRN2        ##RRN
     C                   CALL      'F0354R'
     C                   PARM      '1'           ##CALL
     C                   PARM                    ##RRN                          faeop
     C*** BEG ADD *** 04/04/00 ****************************************
     C                   PARM      SCLIN#        ##LIN#
     C*** END ADD *** 04/04/00 ****************************************
     C                   SETON                                        90
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   Z-ADD     0             SCEOB#
     C                   Z-ADD     0             SCLIN#
     C                   Z-ADD     0             SCSEQ#
     C                   MOVE      *BLANKS       SCPART
     C                   MOVE      *BLANKS       SCTDES
     C                   MOVE      *BLANKS       SCCSCD
     C                   MOVE      *BLANKS       SCCSDS
     C                   Z-ADD     0             SCFDAT
     C*** BEG CHG *** 04/03/03 ****************************************
     C******************** MOVE *BLANKS   SCPOS
     C                   MOVE      *ZEROS        SCPOS
     C*** END CHG *** 04/03/03 ****************************************
     C                   MOVE      *BLANKS       SCPODS
     C                   MOVE      *BLANKS       SCDESC
     C                   MOVE      *BLANKS       SCPRCD
     C                   MOVE      *BLANKS       SCBLCD
     C*** BEG ADD *** 02/14/01 ****************************************
     C                   MOVE      *BLANKS       SCDXR1
     C                   MOVE      *BLANKS       SCDIA1
     C                   MOVE      *BLANKS       SCD1DS
     C                   MOVE      *BLANKS       SCDXR2
     C                   MOVE      *BLANKS       SCDIA2
     C                   MOVE      *BLANKS       SCD2DS
     C                   MOVE      *BLANKS       SCDXR3
     C                   MOVE      *BLANKS       SCDIA3
     C                   MOVE      *BLANKS       SCD3DS
     C                   MOVE      *BLANKS       SCDXR4
     C                   MOVE      *BLANKS       SCDIA4
     C                   MOVE      *BLANKS       SCD4DS
     C*** END ADD *** 02/14/01 ****************************************
     C                   Z-ADD     0             SCCHRG
     C                   Z-ADD     0             SCPAID
     C                   Z-ADD     0             SCMRA
     C                   Z-ADD     0             SCNTWK
     C                   Z-ADD     0             SCOTHR
     C                   MOVE      *BLANKS       SCOT1C
     C                   Z-ADD     0             SCOTH2
     C                   MOVE      *BLANKS       SCOT2C
     C*** BEGIN ADD *** 05/24/01 **************************************
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SCEDAT
     C*** END   ADD *** 05/24/01 **************************************
     C                   CLEAR                   SCEOB1
     C                   CLEAR                   SCEOB2
     C                   CLEAR                   SCEOB3
     C                   CLEAR                   SCEOB4
     C*
     C                   MOVE      EOAST         SCAST
     C*** BEGIN DEL *** 08/07/02 **************************************
     C**********           MOVE EOZIPA    SCZIPA    P
     C*** END   DEL *** 08/07/02 **************************************
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
     C*
     C* SET DATE ENTERED
     C*
     C                   TIME                    DSDTTM
     C*
     C* DEFAULT CLAIMS STATUS CODE FROM FAEOP
     C*
     C                   MOVEL     EOCSCD        SCCSCD
     C*** BEGIN ADD *** 07/23/99 **************************************
     C   44              MOVEL     '300'         SCCSCD
     C*** END ADD *** 07/23/99 ****************************************
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
     C*** BEG ADD *** 02/14/01 ****************************************
     C*
     C* DEFAULT 1ST DIAGNOSIS CROSS REFERENCE CODE IF ONLY ONE
     C* DIAGNOSIS CODE IS ENTERED.
     C*
     C   52              MOVE      'X'           SCDXR1
     C*
     C     EDKY          CHAIN(N)  PED01                              99
     C     *IN99         IFEQ      *OFF
      *** Beg Add *** 02/04/14 ****************************************
      /free
       If EDicdind <> 'Y';
       //If ICD9 code is selected
      /end-free
      *** END Add *** 02/04/14 ****************************************
     C                   MOVEL(P)  EDDIA1        SCDIA1
     C                   MOVEL(P)  EDDIA2        SCDIA2
     C                   MOVEL(P)  EDDIA3        SCDIA3
     C                   MOVEL(P)  EDDIA4        SCDIA4
      *** Beg Add *** 02/04/14 ****************************************
      /free
       Else;
       //If ICD10 code is selected
          SCDIA1 = EDICD1001;
          SCDIA2 = EDICD1002;
          SCDIA3 = EDICD1003;
          SCDIA4 = EDICD1004;
       Endif;
      /end-free
      *** END Add *** 02/04/14 ****************************************
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C*
     C* GET DIAGNOSIS CODES DESCRIPTIONS
     C*
      *** Beg Add *** 02/04/14 ****************************************
      /free
       If EDicdind <> 'Y';
      /end-free
      *** END Add *** 02/04/14 ****************************************
     C                   MOVE      'D'           KKLOST
     C*
     C     SCDIA1        IFNE      *BLANKS
     C*
     C*** Beg Chg *** 07/11/14 ****************************************
     C*
     C***********        MOVE(P)   SCDIA1        KKICD9
     C                   MOVEL(P)  SCDIA1        KKICD9
     C*
     C*** End Chg *** 07/11/14 ****************************************
     C*
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCD1DS
     C                   ENDIF                                                  SCDIA1 IFNE *BLANKS
     C*
     C     SCDIA2        IFNE      *BLANKS
     C*
     C*** Beg Chg *** 07/11/14 ****************************************
     C*
     C***********        MOVE(P)   SCDIA2        KKICD9
     C                   MOVEL(P)  SCDIA2        KKICD9
     C*
     C*** End Chg *** 07/11/14 ****************************************
     C*
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCD2DS
     C                   ENDIF                                                  SCDIA2 IFNE *BLANKS
     C*
     C     SCDIA3        IFNE      *BLANKS
     C*
     C*** Beg Chg *** 07/11/14 ****************************************
     C*
     C***********        MOVE(P)   SCDIA3        KKICD9
     C                   MOVEL(P)  SCDIA3        KKICD9
     C*
     C*** End Chg *** 07/11/14 ****************************************
     C*
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCD3DS
     C                   ENDIF                                                  SCDIA3 IFNE *BLANKS
     C*
     C     SCDIA4        IFNE      *BLANKS
     C*
     C*** Beg Chg *** 07/11/14 ****************************************
     C*
     C***********        MOVE(P)   SCDIA4        KKICD9
     C                   MOVEL(P)  SCDIA4        KKICD9
     C*
     C*** End Chg *** 07/11/14 ****************************************
     C*
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCD4DS
     C                   ENDIF                                                  SCDIA4 IFNE *BLANKS
      *** Beg Add *** 02/04/14 ****************************************
      /free
       Else;
          If scDia1 <> *blanks;
             kkICD10 = scDia1;
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   SCD1DS = ICdsc1;
                Endif;
          Endif;
          If scDia2 <> *blanks;
             kkICD10 = scDia2;
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   SCD2DS = ICdsc1;
                Endif;
          Endif;
          If scDia3 <> *blanks;
             kkICD10 = scDia3;
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   SCD3DS = ICdsc1;
                Endif;
          Endif;
          If scDia4 <> *blanks;
             kkICD10 = scDia4;
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   SCD4DS = ICdsc1;
                Endif;
          Endif;
       Endif;
      /end-free
      *** END Add *** 02/04/14 ****************************************
     C*
     C*** END ADD *** 02/14/01 ****************************************
     C                   ENDIF                                                  @@MODE IFEQ 'A'
     C*
     C                   MOVEL(P)  SCCSCD        KKKEY
     C     TBKY          CHAIN     PCS01                              99
     C   99              MOVEL(P)  CSDESC        SCCSDS
     C*
     C     @@MODE        IFNE      'A'
     C*
     C* FROM FDDDP
     C*
     C                   MOVEL(P)  SCCSCD        KKKEY
     C     TBKY          CHAIN     PCS01                              99
     C  N99              MOVEL(P)  CSDESC        SCCSDS
     C*
     C                   MOVEL(P)  DDPART        SCPART
     C                   MOVEL(P)  DDPOS         SCPOS
     C                   MOVEL(P)  DDPRCD        SCPRCD
     C                   MOVEL(P)  DDPRDS        SCDESC
     C                   MOVEL     DDBPRC        SCBLCD
     C*** BEG ADD *** 02/14/01 ****************************************
     C                   MOVE      DDDXR1        SCDXR1
     C                   MOVE      DDDXR2        SCDXR2
     C                   MOVE      DDDXR3        SCDXR3
     C                   MOVE      DDDXR4        SCDXR4
     C*** END ADD *** 02/14/01 ****************************************
     C*
     C                   MOVE(P)   SCPOS         KKPOS
     C     PSKY          CHAIN     PPS01                              99
     C  N99              MOVEL(P)  PSDESC        SCPODS
     C*
     C                   MOVEL(P)  'TOTH'        KKTBID
     C                   MOVEL(P)  SCPART        KKKEY1
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCTDES
     C                   ENDIF                                                  *IN99 = *OFF
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
     C                   Z-ADD     CAFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCFDAT
     C*** BEGIN ADD *** 05/24/01 **************************************
     C                   Z-ADD     CAIDT         SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEDAT
     C*** END   ADD *** 05/24/01 **************************************
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
     C*** BEG ADD *** 02/14/01 ****************************************
     C*
     C* FROM FAEDP
     C*
     C     EDKY          CHAIN(N)  PED01                              99
     C     *IN99         IFEQ      *OFF
      *** Beg Add *** 02/04/14 ****************************************
      /free
       If EDicdind <> 'Y';
       //If ICD9 code is selected
      /end-free
      *** END Add *** 02/04/14 ****************************************
     C                   MOVEL(P)  EDDIA1        SCDIA1
     C                   MOVEL(P)  EDDIA2        SCDIA2
     C                   MOVEL(P)  EDDIA3        SCDIA3
     C                   MOVEL(P)  EDDIA4        SCDIA4
      *** Beg Add *** 02/04/14 ****************************************
      /free
       Else;
       //If ICD10 code is selected
          SCDIA1 = EDICD1001;
          SCDIA2 = EDICD1002;
          SCDIA3 = EDICD1003;
          SCDIA4 = EDICD1004;
       Endif;
      /end-free
      *** END Add *** 02/04/14 ****************************************
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C*
     C* GET DIAGNOSIS CODES DESCRIPTIONS
     C*
      *** Beg Add *** 02/04/14 ****************************************
      /free
       If EDicdind <> 'Y';
      /end-free
      *** END Add *** 02/04/14 ****************************************
     C                   MOVE      'D'           KKLOST
     C*
     C     SCDIA1        IFNE      *BLANKS
     C*
     C*** Beg Chg *** 07/11/14 ****************************************
     C*
     C***********        MOVE(P)   SCDIA1        KKICD9
     C                   MOVEL(P)  SCDIA1        KKICD9
     C*
     C*** End Chg *** 07/11/14 ****************************************
     C*
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCD1DS
     C                   ENDIF                                                  SCDIA1 IFNE *BLANKS
     C*
     C     SCDIA2        IFNE      *BLANKS
     C*
     C*** Beg Chg *** 07/11/14 ****************************************
     C*
     C***********        MOVE(P)   SCDIA2        KKICD9
     C                   MOVEL(P)  SCDIA2        KKICD9
     C*
     C*** End Chg *** 07/11/14 ****************************************
     C*
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCD2DS
     C                   ENDIF                                                  SCDIA2 IFNE *BLANKS
     C*
     C     SCDIA3        IFNE      *BLANKS
     C*
     C*** Beg Chg *** 07/11/14 ****************************************
     C*
     C***********        MOVE(P)   SCDIA3        KKICD9
     C                   MOVEL(P)  SCDIA3        KKICD9
     C*
     C*** End Chg *** 07/11/14 ****************************************
     C*
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCD3DS
     C                   ENDIF                                                  SCDIA3 IFNE *BLANKS
     C*
     C     SCDIA4        IFNE      *BLANKS
     C*
     C*** Beg Chg *** 07/11/14 ****************************************
     C*
     C***********        MOVE(P)   SCDIA4        KKICD9
     C                   MOVEL(P)  SCDIA4        KKICD9
     C*
     C*** End Chg *** 07/11/14 ****************************************
     C*
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL(P)  DIDSC1        SCD4DS
     C                   ENDIF                                                  SCDIA4 IFNE *BLANKS
      *** Beg Add *** 02/04/14 ****************************************
      /free
       Else;
          If scDia1 <> *blanks;
             kkICD10 = scDia1;
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   SCD1DS = ICdsc1;
                Endif;
          Endif;
          If scDia2 <> *blanks;
             kkICD10 = scDia2;
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   SCD2DS = ICdsc1;
                Endif;
          Endif;
          If scDia3 <> *blanks;
             kkICD10 = scDia3;
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   SCD3DS = ICdsc1;
                Endif;
          Endif;
          If scDia4 <> *blanks;
             kkICD10 = scDia4;
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   SCD4DS = ICdsc1;
                Endif;
          Endif;
       Endif;
      /end-free
      *** END Add *** 02/04/14 ****************************************
     C*
     C*** END ADD *** 02/14/01 ****************************************
     C*
     C* DISPLAY EOB CODES FOR BILL
     C*
     C     @@MODE        IFNE      'A'
     C                   Z-ADD     0             XXSEQ#            2 0
     C     DEKY          CHAIN(N)  PDE01                              99
     C     *IN99         DOWEQ     *OFF
     C**** Beg Chg *** 07/18/23 **************************************
     C**** DEEOB         ANDNE     *BLANKS
     C     DEEOBCD       ANDNE     *BLANKS
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
     C     DEKY          READE(N)  PDE01                                  99
     C                   ENDDO
     C*** BEGIN ADD *** 08/07/02 **************************************
     C                   MOVEL     CAZIPA        SCZIPA
     C                   MOVEL     CAZIPA        SCZIPB
     C                   ELSE
     C                   MOVEL     *BLANKS       SCZIPA
     C                   MOVEL     *BLANKS       SCZIPB
     C*** END   ADD *** 08/07/02 **************************************
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
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C* LOAD FIELDS FOR FDDDP
     C*
     C                   MOVEL(P)  SCPART        DDPART
     C                   MOVEL(P)  SCPOS         DDPOS
     C                   MOVEL     SCPRCD        DDPRCD
     C*** BEG ADD *** 02/14/01 ****************************************
     C                   MOVE      SCDXR1        DDDXR1
     C                   MOVE      SCDXR2        DDDXR2
     C                   MOVE      SCDXR3        DDDXR3
     C                   MOVE      SCDXR4        DDDXR4
     C*** END ADD *** 02/14/01 ****************************************
     C*
     C     SCDESC        IFNE      *BLANKS
     C                   MOVEL(P)  SCDESC        DDPRDS
     C                   ELSE
     C                   MOVEL(P)  SCPRCD        KKPRCD
     C                   MOVEL(P)  SCFDAT        KKFDAT
     C*** BEG ADD *** 02/27/13 ****************************************
     C     EOAST         IFEQ      'IL'
     C     KKFDAT        IFLT      1120101
     C     FSKY2         CHAIN     PFS01                              99
     C                   ELSE
     C     F1KY          CHAIN     PF101                              99
     C                   ENDIF
     C                   ELSE
     C*** END ADD *** 02/27/13 ****************************************
     C     PRKY          CHAIN     PPR01                              99
     C*** BEG ADD *** 02/27/13 ****************************************
     C                   ENDIF
     C*** END ADD *** 02/27/13 ****************************************
     C     *IN99         IFEQ      *OFF
     C*** Beg Add *** 02/27/13 *************************************************
     C     EOAST         IFEQ      'IL'
     C     KKFDAT        IFLT      1120101
     C                   MOVEL(P)  FSDSC1        DDPRDS
     C                   ELSE
     C                   MOVEL(P)  F1DSC1        DDPRDS
     C                   ENDIF
     C                   ELSE
     C*** End Add *** 02/27/13 *************************************************
     C                   MOVEL(P)  PRDSC1        DDPRDS
     C*** Beg Add *** 02/27/13 *************************************************
     C                   ENDIF
     C*** End Add *** 02/27/13 *************************************************
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ENDIF                                                  SCDESC = *BLANKS
     C*
     C     SCBLCD        IFNE      *BLANKS
     C                   MOVEL     SCBLCD        DDBPRC
     C                   ELSE
     C                   MOVEL     SCPRCD        DDBPRC
     C*
     C                   ENDIF                                                  SCDESC = *BLANKS
     C*
     C* LOAD FIELDS FOR FDCAP
     C*
     C                   Z-ADD     SCFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CAFDAT
     C                   MOVEL     SRDTO         CATDAT
     C*
     C*** BEGIN ADD *** 05/24/01 **************************************
     C                   MOVEL(P)  DDPRCD        CACODE
     C*** END   ADD *** 05/24/01 **************************************
     C                   MOVEL     SCPRCD        CACPT4
     C                   MOVEL     SCBLCD        CABPRC
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
     C*** BEG ADD *** 02/14/01 ****************************************
     C     SCDXR1        IFEQ      'X'
     C                   MOVEL(P)  SCDIA1        CAICD9
     C                   ENDIF
     C     SCDXR2        IFEQ      'X'
     C                   MOVEL(P)  SCDIA2        CAICD9
     C                   ENDIF
     C     SCDXR3        IFEQ      'X'
     C                   MOVEL(P)  SCDIA3        CAICD9
     C                   ENDIF
     C     SCDXR4        IFEQ      'X'
     C                   MOVEL(P)  SCDIA4        CAICD9
     C                   ENDIF
     C*** END ADD *** 02/14/01 ****************************************
     C*
     C                   Z-ADD     SCPAID        CAPAID
     C                   MOVEL(P)  SCCSCD        CACSCD
     C                   MOVEL(P)  SCOT1C        CAOT1C
     C                   MOVEL(P)  SCOT2C        CAOT2C
     C*** BEG ADD *** 08/23/02 ****************************************
     C                   MOVE      XXRCFL        CARCFL
     C*** END ADD *** 08/23/02 ****************************************
     C*
     C* LOAD FIELDS FOR FDDAP
     C*
     C                   Z-ADD     SCMRA         DAMRA
     C                   Z-ADD     SCNTWK        DANDIS
     C                   Z-ADD     SCOTHR        DAOTHR
     C                   Z-ADD     SCOTH2        DAOTH2
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
     C* BEYOND 71 - 89 RESERVED BY COPYBOOK. THESE INDICATORS SHOULD
     C* ALSO BE SET OFF IN 'PGMEND' SUBROUTINE AS WELL.
     C*
     C*** BEG DEL *** 02/14/01 ****************************************
     C***************      SETOF                     606162
     C*** BEG DEL *** 02/14/01 ****************************************
     C                   SETOFF                                       636465
     C                   SETOFF                                       666758
     C                   SETOFF                                       7030
     C*** BEGIN ADD *** 10/13/00 **************************************
     C*
     C*** INITIALIZE POSITION CURSOR INDICATORS
     C*
     C                   SETOFF                                       313233
     C                   SETOFF                                       343536
     C                   SETOFF                                       373839
     C                   SETOFF                                       404142
     C                   SETOFF                                       4344
     C                   SETOFF                                       484950
     C                   SETOFF                                       535455
     C*** END   ADD *** 10/13/00 **************************************
     C*
     C*** Beg Chg *** MC367 *** 11/15/12 ******************************
     C***  *INKA         CASEQ     *ON           KADO                           F1=Repeat
     C***  *INKB         CASEQ     *ON           KBDO                           F02=Process
     C***  *INKG         CASEQ     *ON           KGDO                           F07=Header info
     C***  *INKI         CASEQ     *ON           KIDO                           F09=Code info
     C***  *INKJ         CASEQ     *ON           KJDO                           F10=Search
     C***  *INKK         CASEQ     *ON           KKDO                           F11=EOB/Notes
     C***  *INKN         CASEQ     *ON           KNDO                           F14=DARTS
     C***  *INKP         CASEQ     *ON           KPDO                           F15=Previous lines
     C***                ENDCS
      /Free
         Select;
          When *INKA = *On;  //F1=Repeat
           Exsr KADO;
          When *INKB = *On;  //F02=Process
           Exsr KBDO;
          When *INKG = *On;  //F07=Header info
           Exsr KGDO;
          When *INKI = *On;  //F09=Code info
           Exsr KIDO;
          When *INKJ = *On;  //F10=Search
           Exsr KJDO;
          When *INKK = *On;  //F11=EOB/Notes
           Exsr KKDO;
      /If Defined(WEB)
          When *INKN = *On;  //F14=DARTS
           Exsr KNDO;
          When *INKP = *On;  //F15=Previous lines
           Exsr KPDO;
      /EndIf
         EndSl;
      /End-Free
     C*** End Chg *** MC367 *** 11/15/12 ******************************
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C*** Beg Add *** 06/22/11 ****************************************
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       ##CLT#            3
     C     ##CLT#        IFEQ      '114'
     C                   MOVEL(P)  SCCSCD        KKKEY
     C     FNKY          CHAIN     PFN01                              99
     C     *IN99         IFEQ      *OFF
     C     FNNWCG        IFEQ      'Y'
     C     TBKY          CHAIN     PCS01                              99
     C     *IN99         IFEQ      *OFF
     C     CSOVRD        IFEQ      'Y'
     C     CSOVRP        ANDEQ     0
     C*** BEG ADD *** 08/19/11 ****************************************
     C     EORCN         ANDNE     CARCN
     C*** END ADD *** 08/19/11 ****************************************
     C*
     C                   CALL      'FA770R'
     C                   PARM                    @@RRN3
     C                   PARM      'D'           @1MODE            1
     C*
     C                   ENDIF                                                  CSOVRD = 'Y'
     C                   ENDIF                                                  FNNWCG = 'Y'
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  CLT = '114'
     C*** End Add *** 06/22/11 ****************************************
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
     C* Close state specific files that are overridden
     C*** BEG ADD *** 05/02/01 ****************************************
     C                   CLOSE     FTABL
     C                   CLOSE     FMPRL
     C                   CLOSE     FTPSL
     C*** END ADD *** 05/02/01 ****************************************
     C*** Beg Add *** 02/27/13 *************************************************
     C                   CLOSE     FMFSL2IL
     C*** End Add *** 02/27/13 *************************************************
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C*** BEG ADD *** 05/02/01 ****************************************
     C                   OPEN      FTABL
     C                   OPEN      FMPRL
     C                   OPEN      FTPSL
     C*** END ADD *** 05/02/01 ****************************************
     C*** Beg Add *** 02/27/13 ************************************************
     C                   OPEN      FMFSL2IL
     C*** End Add *** 02/27/13 ************************************************
     C*
     C** Get standard screen headings
     C*
     C     @@RRN2        CHAIN(N)  PEO01                              99
     C                   MOVE      EOCO#         #1CO#
     C                   MOVE      EOSUB#        #1SUB#
     C                   MOVE      EOFND         #1FND
     C                   MOVE      EOFYR         #1FYR
     C                   MOVE      EOCASE        #1CASE
     C*** BEG ADD *** 06/19/00 ****************************************
     C                   MOVE      EOCO#         KKCO#
     C                   MOVE      EOSUB#        KKSUB#
     C                   MOVE      EOFND         KKFND
     C                   MOVE      EOFYR         KKFYR
     C                   MOVE      EOCASE        KKCASE
     C                   MOVE      EOEOB#        KKEOB#
     C                   MOVE      EORCN         KKRCN
     C     EDKY          CHAIN(N)  PED01                              60
     C*** END ADD *** 06/19/00 ****************************************
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
     C*** BEGIN ADD *** 08/15/00 **************************************
     C*
     C*** BEG ADD *** 12/08/00 ****************************************
     C*  Get flag to determine if we use FMRCP file for Adj State
     C*
     C                   MOVEL     *BLANKS       ##RC              1
     C                   MOVEL     EOAST         @@KEY
     C*
     C     STKY          CHAIN     PST01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     STBS02        ##RC
     C*** Beg Add *** 06/22/11 ****************************************
     C                   MOVEL     STMFS         XXMFS
     C*** End Add *** 06/22/11 ****************************************
     C*
     C                   ENDIF                                                  *IN99
     C*** END ADD *** 12/08/00 ****************************************
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
     C*** END   ADD *** 08/15/00 **************************************
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
     C* IF IN DELETE MODE, WE MUST CHAIN TO FDCAP RECORD TO GET THE
     C* DETAIL SEQUECE NUMBER AND THE RELATIVE RECORD NUMBER FOR THE
     C* PHARMACY DETAIL RECORD.  ELSE USE THE PASSED PHARMACY DETAIL
     C* RELEATIVE RECORD NUMBER TO GET THE DETAIL SEQUENCE NUMBER
     C*
     C     @@MODE        IFEQ      'E'
     C     @@RRN3        CHAIN(N)  PCA02                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     CADSEQ        KKDSEQ
     C     DDKY          CHAIN     PDD01                              99
     C                   Z-ADD     DBRRNC        @@RRN
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ELSE
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C                   Z-ADD     DDDSEQ        KKDSEQ
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
     C*** BEG ADD *** 12/20/02 ****************************************
     C*
     C* DETERMINE IF THE NETWORK SELECTED HAS A MANUAL FEE SCHEDULE
     C*
     C     EOFSFL        IFEQ      'Y'
     C                   SETON                                        03
     C                   ELSE
     C                   SETOFF                                       03
     C                   ENDIF
     C*** END ADD *** 12/20/02 ****************************************
     C*** BEGIN DEL *** 01/11/00 **************************************
     C*** BEGIN ADD *** 07/23/99 **************************************
     C**********           SETOF                     44
     C********** PDKY      CHAINPPD01                99
     C********** PDPARN    IFEQ 'P'
     C**********           MOVE EONTWK    KKNWCD    P
     C********** PDTYPE    IFEQ '4'
     C********** S4KY      CHAINPS402                99
     C********** *IN99     IFEQ *OFF
     C********** S4FSFL    ANDEQ'Y'
     C**********           SETON                     44
     C**********           ENDIF                           *IN99 = Off
     C**********           ELSE                            TYPE = '4'
     C********** PDTYPE    IFEQ '1'
     C********** S4KY      CHAINPS401                99
     C********** *IN99     IFEQ '0'
     C********** S4FSFL    ANDEQ'Y'
     C**********           SETON                     44
     C**********           ENDIF                           *IN99 = Off
     C**********           ENDIF                           TYPE = '1'
     C**********           ENDIF                           TYPE = '4'
     C**********           ENDIF                           PARN = 'P'
     C*** END ADD *** 07/23/99 ****************************************
     C*** END   DEL *** 01/11/00 **************************************
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
     C*** BEG ADD *** 02/14/01 ****************************************
     C*
     C* DIAGNOSIS CODES 1, 2, 3, AND 4 WILL BE DISPLAYED ONLY IF EXISTS
     C* FAEDP
     C*
     C     EDKY          CHAIN(N)  PED01                              99
     C     *IN99         IFEQ      *OFF
     C*
      *** Beg Add *** 02/04/14 ****************************************
      /free
       If EDicdind <> 'Y';
      /end-free
      *** END Add *** 02/04/14 ****************************************
     C     EDDIA1        IFNE      *BLANKS
     C                   SETON                                        28
     C                   ELSE
     C                   SETOFF                                       28
     C                   ENDIF                                                  EDDIA1 NE *BLANKS
     C*
     C     EDDIA2        IFNE      *BLANKS
     C                   SETON                                        61
     C                   ELSE
     C                   SETOFF                                       61
     C                   ENDIF                                                  EDDIA2 NE *BLANKS
     C*
     C     EDDIA3        IFNE      *BLANKS
     C                   SETON                                        62
     C                   ELSE
     C                   SETOFF                                       62
     C                   ENDIF                                                  EDDIA3 NE *BLANKS
     C*
     C     EDDIA4        IFNE      *BLANKS
     C                   SETON                                        45
     C                   ELSE
     C                   SETOFF                                       45
     C                   ENDIF                                                  EDDIA4 NE *BLANKS
      *** Beg Add *** 02/04/14 ****************************************
      /free
       Else;
          If EDicd1001 <> *blanks;
             *in28 = *on;
          Else;
             *in28 = *off;
          Endif;

          If EDicd1002 <> *blanks;
             *in61 = *on;
          Else;
             *in61 = *off;
          Endif;

          If EDicd1003 <> *blanks;
             *in62 = *on;
          Else;
             *in62 = *off;
          Endif;

          If EDicd1004 <> *blanks;
             *in45 = *on;
          Else;
             *in45 = *off;
          Endif;
       Endif;
      /end-free
      *** END Add *** 02/04/14 ****************************************
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C* DEFAULT 'X' TO FIRST DIAGNOSIS CODE IF IT IS THE ONLY
     C* DIAGNOSIS CODE
     C*
      *** Beg Add *** 02/04/14 ****************************************
      /free
       If EDicdind <> 'Y';
      /end-free
      *** END Add *** 02/04/14 ****************************************
     C     @@MODE        IFEQ      'A'
     C     EDDIA1        ANDNE     *BLANKS
     C     EDDIA2        ANDEQ     *BLANKS
     C     EDDIA3        ANDEQ     *BLANKS
     C     EDDIA4        ANDEQ     *BLANKS
     C                   SETON                                        52
     C                   ELSE
     C                   SETOFF                                       52
     C                   ENDIF                                                  @@MODE = 'A'
      *** Beg Add *** 02/04/14 ****************************************
      /free
       Else;
          If @@mode = 'A' AND EDicd1001 <> *Blanks AND EDicd1002 = *Blanks
             AND EDicd1003 = *Blanks AND EDicd1004 =*Blanks;
             *IN52 = *on;
          Else;
             *IN52 = *off;
          Endif;
       Endif;
      /end-free
      *** End Add *** 02/04/14 ****************************************
     C*** END ADD *** 02/14/01 ****************************************
     C*
     C     @@MODE        IFEQ      'A'
     C                   SETON                                        41
     C                   ENDIF
     C*
     C     @@MODE        COMP      'A'                                2929
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
     C* CALL PRR-CERT SELECTION PROGRAM IF IN ADD MODE AND PRE-CERT
     C* IS INSTALLED AND PRE-CERT RECORDS EXIST FOR THIS CASE
     C*
     C     @@MODE        IFEQ      'A'
     C     ##UR          ANDEQ     'Y'
     C     EOCERT        ANDEQ     *BLANKS
     C     EORCN         ANDEQ     *ZEROS
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
       //*** Beg Add *** 06/27/19 *************************************
      /Free
         isPbillsLite = isModEnabled('Pbills Lite');
      /End-Free
       //*** End Add *** 06/27/19 *************************************
     C*
     C* DISPLAY EOB CODES AND TITLE IF NOT IN ADD MODE
     C*
     C     @@MODE        IFNE      'A'
     C                   SETON                                        59
     C                   ELSE
     C                   SETOFF                                       59
     C                   ENDIF                                                  @@MODE NE 'A'
     C*** Beg Add *** 06/21/12 ****************************************
      /Free
         If Eoast = 'HL';
           *in29 = *off;
         Endif;
      /End-Free
     C*** End Add *** 06/21/12 ****************************************
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
     C*                    Z-ADDSCFDAT    SRDTI
     C*                    MOVE *BLANKS   SRDTO
     C*                    MOVE *BLANKS   SRERR
     C*                    CALL 'SRCVTDAT'DTCYMD
     C*                    MOVELSRDTO     HLFDAT
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
     C*** BEG DEL *** 12/07/99 ****************************************
     C* IF ADJUDICATION CODE IS NE 1, CLEAR CAMRAA. THIS WILL ALLOW THE
     C* OVERRIDE PROCESSING TO BE PERFORMED IN THE ADJUDICTION PROGRAM
     C*
     C********** SCCSCD    IFNE '1'
     C**********           Z-ADD0         CAMRAA
     C**********           ENDIF                           SCCSCD = 1
     C*** END DEL *** 12/07/99 ****************************************
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
     C*** BEG CHG *** 01/17/01 ****************************************
     C                   MOVE      XXRCFL        DDRCFL
     C*** END CHG *** 01/17/01 ****************************************
     C                   MOVEL     Q1USER        DDUSER
     C                   MOVEL     Q1PGM         DDPGM
     C                   Z-ADD     DSTIME        DDTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DDDT
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
     C*** BEG ADD *** 01/17/01 ****************************************
     C                   MOVE      XXRCFL        DDRCFL
     C*** END ADD *** 01/17/01 ****************************************
     C*** END ADD *** 01/17/01 ****************************************
     C                   Z-ADD     ##CO#         DDCO#
     C                   Z-ADD     EOSUB#        DDSUB#
     C                   Z-ADD     EOFND         DDFND
     C                   Z-ADD     EOFYR         DDFYR
     C                   Z-ADD     EOCASE        DDCASE
     C                   Z-ADD     SCSEQ#        DDDSEQ
     C                   Z-ADD     EOEOB#        DDEOB#
     C                   MOVEL     EOSS#         DDSS#
     C                   Z-ADD     EOACDT        DDACDT
     C                   MOVE      'A'           DDSTAT
     C                   MOVEL     Q1USER        DDUSER
     C                   MOVEL     Q1USER        DDIUSR
     C                   MOVEL     Q1PGM         DDPGM
     C                   MOVEL     Q1PGM         DDIPGM
     C                   Z-ADD     DSTIME        DDTIME
     C                   Z-ADD     DSTIME        DDITIM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DDDT
     C                   MOVEL     SRDTO         DDIDT
     C                   WRITE     DBRCDF
     C                   Z-ADD     ##CO#         CACO#
     C                   Z-ADD     EOSUB#        CASUB#
     C                   Z-ADD     EOFND         CAFND
     C                   Z-ADD     EOFYR         CAFYR
     C                   Z-ADD     EOCASE        CACASE
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
     C*** BEG CHG *** 01/17/01 ****************************************
     C******************** MOVE 'C'       CARCFL
     C                   MOVE      XXRCFL        CARCFL
     C*** END CHG *** 01/17/01 ****************************************
     C*
     C*** BEGIN CHG *** 04/08/02 **************************************
     C********** EOBTYP    IFEQ 'N'
     C**********           MOVE '1'       CAPRTY
     C**********           ENDIF
     C*
     C********** EOBTYP    IFEQ 'H'
     C**********           MOVE '4'       CAPRTY
     C**********           ENDIF
     C                   MOVEL(P)  EOBTYP        CAPRTY
     C*** END   CHG *** 04/08/02 **************************************
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
     C                   FEOD      FDDDP
     C                   SETON                                        41
     C                   ENDSR
     OPMG01     E            RPMG01
     OPDE01     E            RPDE01
**  (EOBR1)
D10
D21
D22
D23
D24
D25
D26
D30
D40
D41
D42
D43
D44
D45
D46
D47
D48
D49
D50
D51
D52
D53
D54
D60
D61
D62
D63
D64
D65
**  (EOBR2)
D71
D72
D73
D74
D75
D80
D81
D82
D83
D84
D90
D91
D93
D94
D95
