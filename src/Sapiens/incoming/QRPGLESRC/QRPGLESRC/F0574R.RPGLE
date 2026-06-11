     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*****************************************************************
     H* PROGRAM: F0574R                                               *
     H* DESCRIPTION:  CASE DETAIL FILE - DETAIL INQUIRY               *
     H*****************************************************************
     H** REVISIONS:                                                  **
     H*                                                              **
     H*  06/09/20  C:212 Project : SY420                             **
     H*                  Mask SSN                                    **
     H*                                                              **
     H*  08/15/00 C:62  PROJECT: MC215                               **
     H*                 REPLACE SFBPF REFERENCES WITH APIS.          **
     H*                                                              **
     H*  08/31/99  C:56    Project: MC208                            **
     H*                1.  If Provider Bills is installed, then      **
     H*                    use appropriate detail file to write the  **
     H*                    "Place of Service" field.                 **
     H*                                                              **
     H*                2.  Replace F0020C/F0030C/F0050R with call    **
     H*                    to new Code Search                        **
     H*
     H*  05/17/99  C:56    Support: MC025 #17870                     **
     H*                    Make changes so that this program works   **
     H*                    with Provider Bills & BILL ENTRY          **
     H*                                                              **
     H*  04/08/99  C:56    Support: MC095 #17568                     **
     H*              1.    Add QQCO#/QQSUB#/QQFND as entry PARMS     **
     H*              2.    Replace use of UPCO#/UPSUB#/UPFND with    **
     H*                    new parms                                 **
     H*                                                              **
     H*  12/16/98  C:79    Added three fields to the display SCR02:  **
     H*                    1. Facility fee = SV92A1                  **
     H*                    2. Prof comp    = SV92A2                  **
     H*                    3. Tech comp    = SV92A3                  **
     H*                                                              **
     H*  12/10/92  C:20    ADDED OTHER MRA 2 TO SCREEN AND CHANGED   **
     H*                    ASC MRA TO OTHER MRA 1 FOR AL             **
     H*  02/01/93  C:20    CHANGED USE OF P1ST TO XXAST(CAAST)       **
     H*                    THIS IS BECAUSE OF ABILITY IN F0070R TO   **
     H*                    TEMP CHANGE ADJ STATE                     **
     H*  04/22/93  C:20    PROJ: FS098                               **
     H*                    CHANGED PROGRAM TO USE THREE SCREENS ONE  **
     H*                    FOR PHYSICIAN, HOSPITAL, PHARMACY         **
     H*                                                               *
     H*  07/20/93 C:20   PROJ: AL006                                  *
     H*                  1. ADD IN USE OF PR92N5 FOR DIA AMOUNT       *
     H*                  2. CHANGED HEADINGS IN FDCFP FILE FOR NEW    *
     H*                     SPLTY CODE AMOUNTS                        *
     H*                                                               *
     H*  12/01/93 C:20   PROJ: FSMNT                                  *
     H*                  1. If CACPT4 = 'FSMRA', don't display        *
     H*                     Total Charge field                        *
     H*                                                               *
     H*  12/30/93 C:20   PROJ: AL013                                  *
     H*                  1. FIX TO PROPERLY DISPLAY PROF/PT FLAG      *
     H*                                                               *
     H*  04/19/94 C:20   REQ/PROJ: 128/FS127                          *
     H*                  1. MODIFICATION TO USE CARCFL TO DETERMINE   *
     H*                     SCREEN TO DISPLAY, NOT CAPRTY.  THE       *
     H*                     FOLLOWING WILL BE THE DISPLAY KEY:        *
     H*                     CARCFL        DISPLAY TYPE                *
     H*                       C           CPT4 CHARGE                 *
     H*                       Y           R&C CHARGE                  *
     H*                       X           PHARMACY CHARGE             *
     H*                       S           SERVICE CODE CHARGE         *
     H*                       N           FDHCP NOT IN FMSVPxx        *
     H*                  2. SET ON INDICATOR TO DISPLAY SC92N1 FIELD  *
     H*                     FOR MA CPT4 CODES(ASC GROUP)              *
     H*                                                               *
     H*  06/16/94 C:05   Project: MC112                               *
     H*                  1. Added Type of Service Code                *
     H*                                                               *
     H*  06/23/94 C:20   PROJ: KY001                                  *
     H*                  1. SET ON INDICATOR SO CHI AMOUNT WILL BE    *
     H*                     DISPLAYED                                 *
     H*                                                               *
     H*  11/28/94 C:05   PROJ: PA001                                  *
     H*                  1. Added F6 (from SCR01/SCR02/SCR03) to      *
     H*                     call Code Maint in INQUIRY mode           *
     H*                                                               *
     H*  03/20/95 C:20   REQ/PROJ: 147/AL022                          *
     H*                  1. REMOVE DISPLAY OF DDS FIELD FROM SCREEN   *
     H*                     FOR AL CODES.                             *
     H*                                                               *
     H*  10/16/96 C:32  REQ/PROJ: OK005                               *
     H*                 1. CHG CRITERIA FOR PROCESSING 'FSMRA'        *
     H*                    FSMRA REC WAS CHGD TO HAVE A ZERO IN CATOTC*
     H*                    AND A NEGATIVE PAID AMT IN CATOTD (DENIED) *
     H*                  CROSS FOOT TOTL = 0 -(- CATOTD) = +REC PAYMNT*
     H*                                                               *
     H*  03/25/97 C:43   REQ/PROJ: SUPPORT MC068, SEQ=7031            *
     H*                  1. ADD RECORD TO COMPILE TIME TABLE=TYPE     *
     H*                  2. DEFENSE COMPILE TIME TABLE INDEX VALUE    *
     H*                                                               *
     H*  08/29/97 C:59   REQ/PROJ: OH009
     H*                  1. DEFUALT R&C TO '430' IF GEOZIP NOT FOUND  *
     H*                  2. BYPASSED RETURN CODE EQUAL TO 'P'         *
     H*                     FIELD WAS BEING USED BEFORE ALL UPDATES TO*
     H*                     BILL ENTRY AND WAS BEING RE-SET CORRECTLY *
     H*                     NOW IT HOLDS A 'P' REGARDLESS OF EDITS    *
     H*                     CAUSING OPTION 5 (DETAIL INQUIRY) NOT TO  *
     H*                     WORK CORRECTLY.                           *
     H*
     H*  09/12/97 C:59  Project: MC048/8323
     H*                 CHANGE THE R&C CODES TO PAY ACCORDING TO
     H*                 PHYSICAL ADDRESS, NOT MAILING ADDRESS
     H*
     H*  04/06/98 C:79  Project: OK006
     H*                 USE THE VALUE FROM CACPT4 TO SEARCH FOR SERVICE
     H*                 CODE DESCRIPTION
XXX  H*****************************************************************
     H** INDICATORS
     H** 23   PR1A1 = A (PROF COMP IS 'ANE')
     H*****************************************************************
     FF0574F    CF   E             WORKSTN USROPN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     F*** BEG DEL *** 04/08/99 ****************************************
     F*****SMUPL   IF  E           K        DISK
     F*** END DEL *** 04/08/99 ****************************************
     F*** BEGIN CHG *** 04/22/93 **************************************
     F**********FDCALB  UF  E           K        DISK
     FFDCALF    UF   E           K DISK
     F*** BEG ADD *** 05/17/99 ****************************************
     F                                     INFDS(DBFDSA)
     F*** END ADD *** 05/17/99 ****************************************
     F*** END CHG *** 04/22/93 ****************************************
     FFMPDL     IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     F*** BEGIN DEL *** 04/22/93 **************************************
     F**********FMFNL   IF  E           K        DISK
     F*** END DEL *** 04/22/93 ****************************************
     FFMSVL1    IF   E           K DISK    USROPN
     F*** BEG ADD *** 08/31/99 ****************************************
     F                                     INFDS(DBFSV)
     F*** END ADD *** 08/31/99 ****************************************
     F*
     FFMPRL     IF   E           K DISK    USROPN
     F*** BEG ADD *** 08/31/99 ****************************************
     F                                     INFDS(DBFPR)
     F*** END ADD *** 08/31/99 ****************************************
     F*** BEGIN ADD *** 09/12/97 ***************************************
      *   ADDED FILE FMP1P BACK TO BE USED TO DETERMINE GEOZIP FOR
      *   R&C SEARCH
     F*** BEGIN DEL 02/01/93 ******************************************
     FFMP1L     IF   E           K DISK
     F*** END DEL *** 02/01/93 ****************************************
     F*** END ADD ***** 09/12/97 ***************************************
     F*** BEGIN DEL *** 04/22/93 **************************************
     F**********FMSML   IF  E           K        DISK
     F*** END DEL *** 04/22/93 ****************************************
     F*
     FFTABL     IF   E           K DISK    USROPN
     F                                     RENAME(PRC01:PRC0W)
     FFMRCL     IF   E           K DISK
     F*** BEG ADD *** 08/31/99 ****************************************
     F                                     INFDS(DBFRC)
     F*** END ADD *** 08/31/99 ****************************************
     FFDHCL     IF   E           K DISK
     FFMGZL     IF   E           K DISK
     FFDRUL1    IF   E           K DISK
     F*** BEGIN ADD *** 04/22/93 **************************************
     FFMRXL     IF   E           K DISK
     F*** BEG ADD *** 08/31/99 ****************************************
     F                                     INFDS(DBFRX)
     F*** END ADD *** 08/31/99 ****************************************
     FFDCFL     IF   E           K DISK
     F*** BEGIN DEL *** 08/15/00 **************************************
     F**********SSYLF   IF  E           K        DISK
     F**********  SCS01                             KRENAMESCS0W
     F**********  SRC01                             KRENAMESRC0W
     F*** END   DEL *** 08/15/00 **************************************
     F*** END ADD *** 04/22/93 ****************************************
     F*** BEG ADD *** 05/17/99 ****************************************
     FFMCXL     IF   E           K DISK
     FFDDEL     IF   E           K DISK
     FFAEOLG    IF   E           K DISK
     F                                     INFDS(DBFDSB)
     F*** END ADD *** 05/17/99 ****************************************
     F*** BEG ADD *** 08/31/99 ****************************************
     FFDDHL     IF   E           K DISK
     FFDDUL     IF   E           K DISK
     FFDDDL     IF   E           K DISK
     FFDDPL     IF   E           K DISK
     F*** END ADD *** 08/31/99 ****************************************
     F*

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************

     D*** BEG CHG *** 03/25/97
     D*****************   TYPE    1   5 20               SERV TYPE DESCRIPTIONS
     D TYPE            S             20    DIM(6) CTDATA PERRCD(1)              SERV TYPE DESCRIPTIO
     D*** END CHG *** 03/25/97
     D QCA             S             55    DIM(6) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     D*** BEGIN ADD *** 04/22/93 **************************************
     D PTYP            S              2    DIM(6) CTDATA PERRCD(1)
     D PTYD            S             20    DIM(6) ALT(PTYP)
     D FORM            S              3    DIM(64) CTDATA PERRCD(1)
     D FORD            S             20    DIM(64) ALT(FORM)
     D*** BEGIN DEL *** 08/15/00 **************************************
     D**********SCS0W
     D**********    CSKEY                           CSKEYW
     D**********SRC0W
     D**********    RCKEY                           RCKEYX
     D*** END   DEL *** 08/15/00 **************************************
     D*** BEG ADD *** 08/31/99 ****************************************
     D DBFPR           DS
     D  PRRRN                397    400B 0
     D DBFRC           DS
     D  RCRRN                397    400B 0
     D DBFSV           DS
     D  SVRRN                397    400B 0
     D DBFRX           DS
     D  RXRRN                397    400B 0
     D*** END ADD *** 08/31/99 ****************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*** BEGIN ADD *** 04/28/92 **************************************
     D                 DS
     D  XXCMSV                 1     11
     D  XXRVCD                 1      3  0
     D  XXHCPC                 4      8
     D  XXUNTS                 9     11  0
     D*** END ADD *** 04/28/92 ****************************************
     D*+++ BGN ADD LJQ  6-13-91 +++++++++++++++++++++++++++++++++++++*
     D*** QCMDEXC CMD STRING MAPPED
     D                 DS
     D  @@CMD                  1     55
     D  @@ST                  38     39
     D*+++ END ADD LJQ  6-13-91 +++++++++++++++++++++++++++++++++++++*
     D*** BEGIN ADD *** 04/28/92 **************************************
     D*** QCMDEXC CMD STRING MAPPED
     D                 DS
     D  @XCMD                  1     55
     D  @XST                  40     41
     D*** END ADD *** 04/28/92 ****************************************
     D*** BEG ADD *** 08/31/99 ****************************************
     D                 DS
     D  K1CMSV                 1     11
     D  K1RVCD                 1      3
     D  K1HCPC                 4      8
     D  K1UNTS                 9     11
     D*** END ADD *** 08/31/99 ****************************************
     D*** BEG ADD *** 05/17/99 ****************************************
     D DBFDSA          DS
     D  DBRRNA               397    400B 0
     D DBFDSB          DS
     D  DBRRNB               397    400B 0
     D*** END ADD *** 05/17/99 ****************************************
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*** END ADD *** 04/22/93 ****************************************
     IPRC0W
     I              RCKEY                       RCKEYW

     C*
     C*   KEY LIST FOR THE CASE DETAIL FILE
     C*
     C     CAKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQDSEQ
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    SCCO#
     C*
     C*
     C*  USER PARAMETER FILE
     C*
     C*** BEG DEL *** 04/08/99 ****************************************
     C********** UPKEY     KLIST
     C**********           KFLD           Q1USER
     C*** END DEL *** 04/08/99 ****************************************
     C*** BEGIN DEL *** 04/22/93 **************************************
     C**********
     C****KEY LIST FOR THE FUND
     C**********
     C********** FNKEY     KLIST
     C**********           KFLD           UPCO#
     C**********           KFLD           UPSUB#
     C**********           KFLD           UPFND
     C**********
     C****KEY LIST FOR THE SUBSCRIBER
     C**********
     C********** SMKEY     KLIST
     C**********           KFLD           UPCO#
     C**********           KFLD           UPSUB#
     F*** END DEL *** 04/22/93 ****************************************
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    QQFYR
     C                   KFLD                    SCCASE
     C*
     C*  CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    SCSS#
     C*
     C*  PART CODE FILE
     C*
     C     PAKY          KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    TABKEY           10
     C*
     C*  TABLE FILE (EOMB)
     C*
     C     TBKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    XXKEY            10
     C*
     C*  SERVICE CODE FILE
     C*
     C     SVKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    XXCMSV
     C                   KFLD                    XXFDAT                         07/16/92 KRR
     C*
     C*  HOSPITAL DETAIL CHARGE FILE
     C*
     C     HCKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CADSEQ
     C*
     C*  PROCEDURE CODE FILE
     C*
     C     PRKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCCPT4
     C                   KFLD                    XXFDAT
     C*
     C*
     C*  REASONABLE / CUSTOMARY FILE
     C*
     C     RCKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    XXZIPA                         02/07/92 ADD
     C                   KFLD                    SCCPT4
     C                   KFLD                    XXFDAT
     C*
     C*  PAYEE DEMOGRAPHICS FILE
     C*
     C     PDKEY         KLIST
     C                   KFLD                    SCFEIN
     C                   KFLD                    SCMOD
     C*
     C*  DIAGNOSIS CODE FILE
     C*
     C*
     C*   KEY LIST FOR THE EOB FILE
     C*
     C     EOKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXEOB#
     C*
     C*  PROVIDER AUXILIARY FILE
     C*
     C     P1KEY         KLIST
     C                   KFLD                    SCFEIN
     C                   KFLD                    SCMOD
     C*
     C*** Geographical Zip Area FILE
     C     GZKY          KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    XXFRZP
     C*
     C*  State FS Rules/Pgm Cross-Reference File
     C     RUKY1         KLIST
     C*********************KFLD           P1ST             Adjudication State
     C                   KFLD                    XXAST             2            Adjudication State
     C                   KFLD                    XXATYP                         Adjudication Type
     C                   KFLD                    XXEFDT                         Service Date
     F*** BEGIN ADD *** 04/22/93 **************************************
     C*
     C* NDC DRUG CODE FILE
     C*
     C     RXKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCNDC
     C*
     C*  FS BASE CONTROL FILE
     C*
     C*** BEGIN DEL *** 08/15/00 **************************************
     C********** FBKY      KLIST
     C**********           KFLD           XSKEY   2
     C*** END   DEL *** 08/15/00 **************************************
     C*
     C*  STATE CPT FIELD HEADINGS
     C*
     C     CFKEY         KLIST
     C                   KFLD                    CAAST
     F*** END ADD *** 04/22/93 ****************************************
     C*
     C*** BEG ADD *** 05/17/99 ****************************************
     C*
     C* KEY LIST FOR WCMC CONTROL FILE
     C*
     C     CXKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C* KEY LIST FOR EOB CODES/NOTES FILE
     C*
     C     DEKY          KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    SCEOB#
     C                   KFLD                    QQDSEQ
     C*
     C* KEY LIST FOR EOMB FILE
     C*
     C     EOKY          KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    SCEOB#
     C*
     C*** END ADD *** 05/17/99 ****************************************
     C*
     C*** BEG ADD *** 08/31/99 ****************************************
     C*
     C* KEY LIST FOR PROVIDER BILLS DETAIL FILES
     C*
     C     DUKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*
     C     DHKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*
     C     DDKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*
     C     PRKY          KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    KKPRCD
     C                   KFLD                    KKFDAT
     C*
     C     PRKY1         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    KKPRCD
     C*
     C     RCKY1         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    KKZIPA
     C                   KFLD                    KKCPT4
     C                   KFLD                    KKEFDT
     C*
     C     RCKY2         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    KKZIPA
     C                   KFLD                    KKCPT4
     C*
     C*
     C     SVKY1         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    K1CMSV
     C                   KFLD                    K1EFDT
     C*
     C     SVKY2         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    K1CMSV
     C*
     C     DPKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*** END ADD *** 08/31/99 ****************************************
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
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
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQDSEQ
     C                   PARM                    QQRTCD            1
     C*** BEG ADD *** 04/08/99 ****************************************
     C                   PARM                    QQCO#             3 0
     C                   PARM                    QQSUB#            5 0
     C                   PARM                    QQFND             3 0
     C*** END ADD *** 04/08/99 ****************************************
     C*
     C* INITIALIZE WORK FIELDS
     C     *LIKE         DEFINE    CMFYR         QQFYR
     C     *LIKE         DEFINE    CMCASE        QQCASE
     C     *LIKE         DEFINE    CADSEQ        QQDSEQ
     C     *LIKE         DEFINE    CAEOB#        XXEOB#
     C     *LIKE         DEFINE    RUATYP        XXATYP
     C     *LIKE         DEFINE    RUEFDT        XXFDAT
     C     *LIKE         DEFINE    RUEFDT        XXWDAT
     C     *LIKE         DEFINE    RUEFDT        XXEFDT
     C     *LIKE         DEFINE    GZFRZP        XXFRZP
     C     *LIKE         DEFINE    GZZIPA        XXZIPA
     C                   MOVE      ' '           QXCALL            1
     C*** BEG ADD *** 05/17/99 ****************************************
     C     *LIKE         DEFINE    CACO#         KKCO#
     C     *LIKE         DEFINE    CASUB#        KKSUB#
     C     *LIKE         DEFINE    SCFND         KKFND
     C*** END ADD *** 05/17/99 ****************************************
     C*** BEG ADD *** 08/31/99 ****************************************
     C     *LIKE         DEFINE    CAFYR         KKFYR
     C     *LIKE         DEFINE    CACASE        KKCASE
     C     *LIKE         DEFINE    CAEOB#        KKEOB#
     C     *LIKE         DEFINE    CADSEQ        KKDSEQ
     C     *LIKE         DEFINE    PRCPT4        KKPRCD
     C     *LIKE         DEFINE    PREFDT        KKFDAT
     C     *LIKE         DEFINE    RCZIPA        KKZIPA
     C     *LIKE         DEFINE    RCCPT4        KKCPT4
     C     *LIKE         DEFINE    PREFDT        KKEFDT
     C     *LIKE         DEFINE    SVEFDT        K1EFDT
     C*** END ADD *** 08/31/99 ****************************************
     C*
     C*** BEG ADD *** 05/17/99 ****************************************
     C*
     C**  GET SYSTEM INFO
     C*
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
     C                   PARM                    ##RHAB            1            REHAB INST
     C                   PARM                    ##MSS             1            MS SUPPORT
     C                   PARM                    ##FSCH            1            FEE SCHEDULE
     C                   PARM                    ##PRVB            1            PROVIDER BILL
     C                   PARM                    ##NPWC            1            NON-PDS WC  LS
     C*
     C*** GET REDUCTIONS HEADINGS FROM WCMC CONTROL FILE
     C*
     C     ##PRVB        IFNE      'Y'
     C                   MOVE      *OFF          *IN90
     C                   ELSE
     C*
     C                   MOVE      *ON           *IN90
     C                   CLEAR                   SCHMRR
     C                   CLEAR                   SCHOT1
     C                   CLEAR                   SCHOT2
     C                   CLEAR                   SCHNET
     C                   Z-ADD     QQCO#         KKCO#
     C                   Z-ADD     QQSUB#        KKSUB#
     C                   Z-ADD     QQFND         KKFND
     C     CXKY          CHAIN     PCX01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  CXHMRA        SCHMRR
     C                   MOVEL(P)  CXHOT1        SCHOT1
     C                   MOVEL(P)  CXHOT2        SCHOT2
     C                   MOVEL(P)  CXHNET        SCHNET
     C                   ENDIF                                                  *IN60 = *OFF
     C                   ENDIF                                                  ##PRVB
     C*** END ADD *** 05/17/99 ****************************************
     C*
     C*** BEGIN DEL *** 08/29/97 **************************************
     C** INCORRECT PROGRAMMING IN F0070R IS CAUSING A 'P' IN THIS FIELD
     C********   QQRTCD    CABEQ'P'       TERM             PRE-OPEN FILES
     C*** END DEL ***** 08/29/97 **************************************
     C*** BEGIN ADD *** 04/08/93 **************************************
     C                   MOVE      '0'           *IN29
     C                   MOVE      '0'           *IN30
     C                   MOVE      '0'           *IN31
     C                   MOVE      '0'           *IN32
     C     QQCALL        IFEQ      '2'
     C                   MOVE      '1'           *IN31
     C                   ELSE
     C     QQCALL        IFEQ      '3'
     C                   MOVE      '1'           *IN32
     C                   ELSE
     C     QQCALL        IFEQ      '4'
     C                   MOVE      '1'           *IN29
     C                   ELSE
     C                   MOVE      '1'           *IN30
     C                   END
     C                   END
     C                   END
     C*** BEGIN DEL *** 08/15/00 **************************************
     C**********           MOVEL'FB'      XSKEY
     C********** FBKY      CHAINSFB01                60
     C********** *IN60     IFEQ '0'
     C*** END   DEL *** 08/15/00 **************************************
     C*** BEGIN CHG *** 08/15/00 **************************************
     C********** FBFL06    IFEQ 'Y'
     C     ##NTWK        IFEQ      'Y'
     C*** END   CHG *** 08/15/00 **************************************
     C                   MOVE      '1'           *IN84
     C                   ELSE
     C                   MOVE      '0'           *IN84
     C                   END
     C*** BEGIN DEL *** 08/15/00 **************************************
     C**********           ELSE
     C**********           MOVE '0'       *IN84
     C**********           END
     C*** END   DEL *** 08/15/00 **************************************
     C*** END ADD *** 04/08/93 ****************************************
     C*
     C                   OPEN      F0574F
     C                   MOVE      '1'           *IN99
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C                   MOVE      QQCASE        SCCASE
     C                   MOVE      QQFYR         SCYR
     C                   MOVE      QQDSEQ        SCSEQ#
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C*** BEG DEL *** 04/08/99 ****************************************
     C********** UPKEY     CHAINPUP01                60
     C********** *IN60     IFEQ '1'
     C**********           MOVE *ZERO     UPCO#
     C**********           MOVE *ZERO     UPSUB#
     C**********           MOVE *ZERO     UPFND
     C**********           END
     C**********           MOVE UPSUB#    SCSUB#
     C**********           MOVE UPFND     SCFND
     C*** END DEL *** 04/08/99 ****************************************
     C*
     C*** BEG ADD *** 04/08/99 ****************************************
     C                   MOVE      QQCO#         SCCO#             3 0          NO SHOWN ON SCREEN
     C                   MOVE      QQSUB#        SCSUB#
     C                   MOVE      QQFND         SCFND
     C*** END ADD *** 04/08/99 ****************************************
     C*
     C     COKEY         CHAIN     PCO01                              60
     C*** BEGIN DEL *** 04/22/93 **************************************
     C**********           MOVELCONAME    SCCONM
     C**********
     C****GET FUND NAME
     C**********
     C********** FNKEY     CHAINPFN01                60
     C**********           MOVELFNNAME    SCNAME
     C**********
     C****GET SUBSCRIBER NAME
     C**********
     C********** SMKEY     CHAINPSM01                60
     C**********           MOVELSMNAME    SCSNAM
     C*** END DEL *** 04/22/93 ****************************************
     C*
     C*   GET CLAIMENTS SS# AND NAME
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C                   MOVE      CMSS#         SCSS#
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C*
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CDLSN         QQLAST           56
     C                   MOVEL     CDFSN         QQFRST           15
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        SCCNAM
     C                   ELSE
     C                   MOVEL     *BLANKS       SCCNAM
     C                   END
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
       scSS# = MaskData('SSN':'CMSS#':CMSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C*
     C*   CHAIN OUT TO THE CASE DETAIL FILE AND GET CHOOSEN RECORD
     C*
     C     CAKEY         CHAIN     PCA01                              50
     C     *IN50         IFEQ      '0'
     C                   EXCEPT    RLSCAE
     C                   MOVE      '0'           *IN33
     C                   MOVE      '0'           *IN34
     C                   MOVE      '0'           *IN35
     C*
     C                   MOVEL     CACSCD        SCCSCD
     C*** BEGIN ADD *** 04/22/93 **************************************
     C     *IN30         IFEQ      '1'
     C                   Z-ADD     CAEOB#        SCEOB#
     C                   ELSE
     C                   Z-ADD     CALTR#        SCLTR#
     C                   END
     C*** END ADD *** 04/22/93 ****************************************
     C                   Z-ADD     CAFEIN        SCFEIN
     C                   Z-ADD     CAMOD         SCMOD
     C                   MOVE      *BLANKS       XXAST                          ADD 02/01/93
     C                   MOVEL     CAAST         XXAST                          ADD 02/01/93
     C                   MOVEL     CAAST         SCAST                          ADD 04/23/93
     C                   MOVEL     CAZIPA        SCZIPA                         ADD 04/23/93
     C*
     C*
     C*** BEGIN DEL *** 02/01/93 **************************************
     C* GET STATE FIRST
     C********** P1KEY     CHAINPP101                55
     C********** P1ST      IFEQ *BLANKS
     C**********           MOVE 'FL'      P1ST
     C**********           END
     C*** END DEL *** 02/01/93 ****************************************
     C*
     C*** CLOSE FILE IF OPENED
     C     STOPN         IFEQ      '1'
     C                   CLOSE     FMPRL
     C                   CLOSE     FTABL
     C                   CLOSE     FMSVL1
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
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           STOVR             1
     C                   END
     C*
     C*** OVERRIDE FMPRL FILE TO SELECTED STATE CODE
     C     STOVR         IFEQ      ' '
     C                   Z-ADD     55            LENGTH
     C                   MOVEL     QCA(2)        @@CMD
     C*********************MOVELP1ST      @@ST             DEL 02/01/93TATE
     C                   MOVEL     XXAST         @@ST                           ADD 02/01/93TATE
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD
     C                   PARM                    LENGTH           15 5
     C                   Z-ADD     55            LENGTH
     C                   MOVEL     QCA(4)        @@CMD
     C*********************MOVELP1ST      @@ST             DEL 02/01/93
     C                   MOVEL     XXAST         @@ST                           ADD 02/01/93
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD
     C                   PARM                    LENGTH           15 5
     C                   Z-ADD     55            LENGTH
     C                   MOVEL     QCA(6)        @XCMD
     C*********************MOVELP1ST      @@ST             DEL 02/01/93
     C                   MOVEL     XXAST         @XST                           ADD 02/01/93
     C                   CALL      'QCMDEXC'
     C                   PARM                    @XCMD
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           STOVR             1
     C                   END
     C*
     C*** OPEN FILE FOR PROCESSING
     C     STOPN         IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FMPRL
     C                   OPEN      FTABL
     C                   OPEN      FMSVL1
     C                   MOVE      '1'           STOPN
     C                   END
     C*** CHECK TO SEE IF CLAIM STATUS IS AN OVERRIDE TYPE
     C                   MOVEL     *BLANKS       XXKEY
     C                   MOVEL     CACSCD        XXKEY
     C     TBKEY         CHAIN     PCS01                              60
     C     CSOVRD        COMP      'Y'                                    21
     C*
     C*          CAPRTY    COMP '4'                      20
     C*
     C     100.0001      MULT      CAFDAT        SCFDAT
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCFDAT
     C     100.0001      MULT      CATDAT        SCTDAT
     C                   Z-ADD     CATDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTDAT
     C*** BEG ADD *** 08/31/99 ****************************************
     C     *IN90         IFEQ      *OFF
     C*** END ADD *** 08/31/99 ****************************************
     C                   MOVE      CAIPOP        SCIPOP
     C*** BEG ADD *** 08/31/99 ****************************************
     C                   ELSE
     C                   MOVE      SCCO#         KKCO#
     C                   MOVE      SCSUB#        KKSUB#
     C                   MOVE      SCFND         KKFND
     C                   MOVE      SCYR          KKFYR
     C                   MOVE      SCCASE        KKCASE
     C                   MOVE      SCEOB#        KKEOB#
     C                   MOVE      SCSEQ#        KKDSEQ
     C                   CLEAR                   SCIPOP
     C     EOKY          CHAIN     PEO01                              60
     C     *IN60         IFEQ      *OFF
     C                   SELECT
     C     EOBTYP        WHENEQ    'N'
     C     DHKY          CHAIN     PDH01                              60
     C  N60              MOVE      DHPOS         SCIPOP
     C     EOBTYP        WHENEQ    'H'
     C     DUKY          CHAIN     PDU01                              60
     C  N60              MOVE      DUPOS         SCIPOP
     C     EOBTYP        WHENEQ    'D'
     C     DDKY          CHAIN     PDD01                              60
     C  N60              MOVE      DDPOS         SCIPOP
     C                   ENDSL
     C                   ENDIF                                                  *IN60 = *OFF
     C                   ENDIF                                                  *IN90 = *OFF
     C*** END ADD *** 08/31/99 ****************************************
     C                   MOVEL     *BLANKS       SCPRDE
     C*
     C*** GET PROVIDER NAME
     C     PDKEY         CHAIN     PPD01                              60
     C                   MOVEL     PDNAME        SCPRDE
     C*
     C                   MOVEL     CATRCD        TABKEY
     C*
     C                   Z-ADD     CASVCD        XXRVCD            3 0
     C*** BEGIN CHG *** 08/29/97 **************************************
     C** ALLOWS USER TO INPUT BOTH CPT AND REVENUE WITH DESC FOR REV
     C********             MOVE CACPT4    XXHCPC  5
     C*** BEG CHG ***** 04/07/98 **************************************
     C******************** MOVE *BLANKS   XXHCPC  5
     C                   MOVE      CACPT4        XXHCPC            5
     C*** END CHG ***** 04/07/98 **************************************
     C*** END CHG ***** 08/29/97 **************************************
     C                   Z-ADD     *ZEROS        XXUNTS            3 0          ADD 05/31/92
     C                   MOVE      XXCMSV        SCCMSV
     C*
     C*** BEGIN CHG *** 04/19/94 ***********************************************
     C********** CAPRTY    COMP '4'                      20
     C     CARCFL        IFEQ      'S'
     C     CARCFL        OREQ      'N'
     C                   MOVE      '1'           *IN20
     C                   ELSE                                                   ELSE CARCFL EQ 'N,S'
     C                   MOVE      '0'           *IN20
     C                   END                                                    END CARCFL EQ 'N,S'
     C*** END ADD *** 04/19/94 *************************************************
     C     *IN20         IFEQ      '1'                                          HOSPITAL TYPE
     C                   MOVE      '1'           *IN34                          ADD 04/22/93
     C*
     C*** GET SERVICE CODE DESCRIPTION
     C*
     C                   MOVEL     *BLANKS       SCDESC
     C                   Z-ADD     SCFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDAT
     C     SVKEY         SETGT     PSV01
     C     NXTSV1        TAG
     C                   READP     PSV01                                  60
     C     *IN60         IFEQ      '0'
     C     SVCO#         ANDEQ     SCCO#
     C     SVCMSV        ANDEQ     XXCMSV                                       CHG 04/28/92
     C     SVSTAT        IFNE      'I'
     C                   MOVEL     SVDESC        SCDESC
     C                   MOVE      SVTYPE        SCTYPE
     C*
     C*** GET SERVICE TYPE DESCRIPTION FROM TABLE
     C                   MOVE      SVTYPE        X                 1 0
     C                   MOVE      *BLANKS       SCTYDS
     C*** BEG ADD *** 03/25/97 ***
     C     X             IFGT      0
     C     X             ANDLE     6
     C*** END ADD *** 03/25/97 ***
     C                   MOVEL     TYPE(X)       SCTYDS
     C*** BEG ADD *** 03/25/97 ***
     C                   ELSE
     C                   MOVEL(P)  'NO TABLE'    SCTYDS
     C                   ENDIF
     C*** END ADD *** 03/25/97 ***
     C*
     C                   Z-ADD     SVALOW        SCSMRA
     C                   Z-ADD     SVINPC        SCINPC
     C                   Z-ADD     SVOTPC        SCOTPC
      *** BEG ADD *** 12/23/98 ****************************************
     C                   Z-ADD     SV92A1        SCFACF
     C                   Z-ADD     SV92A2        SCPROF
     C                   Z-ADD     SV92A3        SCTECH
      *** END ADD *** 12/23/98 ****************************************
     C                   ELSE
     C                   GOTO      NXTSV1
     C                   END
     C*** BEGIN ADD *** 04/23/93 **************************************
     C     HCKY          CHAIN     PHC01                              55
     C     *IN55         IFEQ      '0'
     C                   Z-ADD     HC6N1         SCPZIP
     C                   MOVEL     HCEO10        SCTOS                          ADD 06/16/94
     C                   END
     C*** END ADD *** 04/23/93 ****************************************
     C                   ELSE
     C                   MOVEL     'UNLISTED'    SCDESC
     C                   MOVE      *BLANKS       SCTYDS
     C                   MOVE      *BLANKS       SCTYPE
     C                   Z-ADD     *ZEROS        SCSMRA
      *** BEG ADD *** 12/16/98 ****************************************
     C                   Z-ADD     *ZEROS        SCFACF
     C                   Z-ADD     *ZEROS        SCPROF
     C                   Z-ADD     *ZEROS        SCTECH
      *** END ADD *** 12/16/98 ****************************************
     C     HCKY          CHAIN     PHC01                              55
     C     *IN55         IFEQ      '0'
     C                   MOVE      HCDESC        SCDESC
     C                   Z-ADD     HC6N1         SCPZIP
     C                   MOVEL     HCEO10        SCTOS                          ADD 06/16/94
     C                   END
     C                   END
     C*
     C                   ELSE                                                   NON-HOSP TYPE
     C     CARCFL        IFNE      'X'                                          ADD 04/22/93
     C                   MOVE      '1'           *IN33                          ADD 04/22/93
     C*** BEGIN ADD *** 04/22/93 **************************************
     C                   MOVEA     '000000'      *IN(71)
     C                   MOVE      '1'           *IN77
     C                   MOVE      '0'           *IN78                          ADD 07/20/93
     C                   MOVE      '0'           *IN64
     C     CAAST         IFEQ      'AL'
     C                   MOVE      '1'           *IN71
     C                   MOVE      '1'           *IN72
     C**********           MOVE '1'       *IN73            DEL 01/03/94
     C**********           MOVE '1'       *IN78            DEL 03/20/95
     C**********           MOVE '1'       *IN64            DEL 01/03/94
     C                   ELSE
     C     CAAST         IFEQ      'FL'
     C                   MOVE      '1'           *IN71
     C                   ELSE
     C     CAAST         IFEQ      'GA'
     C                   MOVE      '1'           *IN71
     C                   MOVE      '0'           *IN77
     C*** BEGIN ADD *** 06/23/94 ***********************************************
     C                   ELSE
     C     CAAST         IFEQ      'KY'
     C                   MOVE      '1'           *IN71
     C*** END ADD *** 06/23/94 *************************************************
     C                   ELSE
     C     CAAST         IFEQ      'LA'
     C                   MOVE      '1'           *IN71
     C**********           MOVE '1'       *IN72
     C                   MOVE      '1'           *IN74
     C                   MOVE      '1'           *IN75
     C**********           MOVE '1'       *IN76
     C*** BEGIN ADD *** 04/19/94 ***********************************************
     C                   ELSE
     C     CAAST         IFEQ      'MA'
     C                   MOVE      '1'           *IN71
     C*** END ADD *** 04/19/94 *************************************************
     C                   ELSE
     C     CAAST         IFEQ      'NY'
     C                   MOVE      '1'           *IN71
     C                   MOVE      '1'           *IN72
     C                   MOVE      '1'           *IN73
     C                   MOVE      '1'           *IN64
     C                   END                                                    END = NY
     C*** BEGIN ADD *** 04/19/94 ***********************************************
     C                   END                                                    END = MA
     C*** END ADD *** 04/19/94 *************************************************
     C                   END                                                    END = LA
     C*** BEGIN ADD *** 06/23/94 ***********************************************
     C                   END                                                    END = KY
     C*** END ADD *** 06/23/94 *************************************************
     C                   END                                                    END = GA
     C                   END                                                    END = FL
     C                   END                                                    END = AL
     C     CFKEY         CHAIN     PCF01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CFHMRA        SCHMRA
     C                   MOVEL     CF92H1        SC92H1
     C                   MOVEL     CF92H2        SC92H2
     C                   MOVEL     CF92H3        SC92H3
     C                   MOVEL     CF92H4        SC92H4                         ADD 07/20/93
     C                   MOVEL     CF92H5        SC92H5                         ADD 07/20/93
     C                   MOVEL     CF1H3         SC1H3
     C                   MOVEL     CF1H4         SC1H4
     C                   MOVEL     CF1H5         SC1H5
     C                   ELSE
     C                   MOVE      *BLANKS       SCHMRA
     C                   MOVE      *BLANKS       SC92H1
     C                   MOVE      *BLANKS       SC92H2
     C                   MOVE      *BLANKS       SC92H3
     C                   MOVE      *BLANKS       SC92H4                         ADD 07/20/93
     C                   MOVE      *BLANKS       SC92H5                         ADD 07/20/93
     C                   MOVE      *BLANKS       SC1H3
     C                   MOVE      *BLANKS       SC1H4
     C                   MOVE      *BLANKS       SC1H5
     C                   END
     C*** END ADD *** 04/22/93 ****************************************
     C                   MOVEL     CACPT4        SCCPT4
     C*** BEGIN ADD *** 04/23/93 **************************************
     C     HCKY          CHAIN     PHC01                              55
     C     *IN55         IFEQ      '0'
     C                   Z-ADD     HC6N1         SCPZIP
     C                   MOVEL     HCEO10        SCTOS                          ADD 06/16/94
     C                   END
     C*** END ADD *** 04/23/93 ****************************************
     C*
     C*** GET PROCEDURE CODE INFORMATION !!!
     C*
     C                   MOVEL     *BLANKS       PRDSC1
     C                   MOVEL     *BLANKS       PRDSC2
     C                   MOVE      *ZEROS        PRMRA
     C                   MOVE      *ZEROS        PRANAM
     C                   Z-ADD     *ZEROS        PRPROF
     C                   MOVEL     *BLANK        PRRPT
     C                   MOVEL     *BLANK        PRSURG
     C                   MOVEL     *BLANK        PRSUPP
     C                   MOVEL     *BLANK        PRFSR
     C*
     C                   MOVEL     *BLANK        SCDESC
     C                   MOVEL     *BLANK        SCDSC2
     C                   Z-ADD     *ZERO         SCMRA
     C                   Z-ADD     *ZERO         SCANEA
     C                   Z-ADD     *ZERO         SCPRCO
     C                   MOVE      '0'           *IN23
     C                   MOVE      *BLANKS       SCPRCA
     C                   MOVEL     *BLANK        SCBRFL
     C                   MOVEL     *BLANK        SCASFL
     C                   MOVEL     *BLANK        SCSIFL
     C                   MOVEL     *BLANK        SCFSFL
     C                   Z-ADD     *ZEROS        SC92N1
     C                   Z-ADD     *ZEROS        SC92N2                         ADD 12/10/92
     C                   Z-ADD     *ZEROS        SC92N4                         ADD 12/10/92
     C                   Z-ADD     *ZEROS        SC92N5                         ADD 07/20/93
     C*
     C*
     C                   MOVE      *BLANKS       XXFRZP
     C                   MOVE      *BLANKS       XXZIPA
     C*
     C* Now that the Provider is established, get its Geographic Zip Area
     C* Get Provider's Zip Code
     C* 1st - establish the first 3-bytes of the zip code
     C*********            MOVELPDZIP     XXFRZP           DEL 09/12/97
     C*** BEGIN ADD *** 09/12/97 **************************************
      *   USING PHYSICAL ADDRESS INSTEAD OF MAILING TO PAY
     C     P1KEY         CHAIN     PP101                              60
     C     *IN60         IFEQ      '0'
     C* Now that the Provider is established, get its Geographic Zip Area
     C* 1st - establish the first 3-bytes of the zip code
     C                   MOVEL     P1ZIP         XXFRZP
     C                   END                                                    END 0 *IN60 - PPD01
     C*** END ADD ***** 09/12/97 **************************************
     C*
     C* 2nd - Get the corresponding Geographic Zip Area record:
     C     GZKY          SETGT     PGZ01
     C                   READP     PGZ01                                  60
     C     *IN60         IFEQ      '0'
     C     XXFRZP        ANDLE     GZTOZP
     C                   MOVE      GZZIPA        XXZIPA
     C*** BEGIN ADD *** 08/29/97 **************************************
     C                   ELSE
     C                   MOVEL     '430'         XXZIPA
     C*** END ADD *** 08/29/97 ****************************************
     C                   END                                                    END 0 *IN60
     C*
     C                   Z-ADD     SCFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDAT
     C*
     C*** DATE OF PR MUST BE FOR RULES
     C                   Z-ADD     SCFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C*** Determine what type of Provider we are working with (Specialty Code)
     C                   MOVE      'P'           XXATYP                         Default Health Prov
     C     PDSPCD        IFEQ      'ASC'
     C***********P1ST      ANDEQ'FL'                       DEL 02/01/93
     C     XXAST         ANDEQ     'FL'                                         ADD 02/01/93
     C                   MOVE      'A'           XXATYP                         Ambulatory Surg Ctr
     C                   END                                                    END EQ 'ASC'
     C*** BEGIN ADD *** 03/20/95 ***********************************************
     C     CARCFL        IFEQ      'D'
     C                   MOVE      'D'           XXATYP
     C                   END
     C*** END ADD *** 03/20/95 *************************************************
     C     RUKY1         SETGT     PRU01
     C                   READP     PRU01                                  60
     C     *IN60         IFEQ      '0'
     C***********RUST      ANDEQP1ST                       DEL 02/01/93
     C     RUST          ANDEQ     XXAST                                        ADD 02/01/93
     C     RUATYP        ANDEQ     XXATYP
     C                   MOVE      RUEFDT        XXWDAT
     C                   ELSE                                                   ELS 0 *IN60 - PRU01
     C                   MOVE      9999999       XXWDAT
     C                   END                                                    END 0 *IN60 - PRU01
     C     PRKEY         SETGT     PPR01
     C     NXTPR1        TAG
     C                   READP     PPR01                                  60
     C     *IN60         IFEQ      '1'
     C     PRCO#         ORNE      SCCO#
     C     PRCPT4        ORNE      SCCPT4
     C     PREFDT        ORLT      XXWDAT
     C*
     C*** If State's Procedure Code is not valid - Check for R&C Code
     C                   MOVE      XXFDAT        XXWDAT
     C     RCKEY         SETGT     PRC01
     C     NXTRC1        TAG
     C                   READP     PRC01                                  60
     C     *IN60         IFEQ      '0'
     C     RCCO#         ANDEQ     SCCO#
     C     RCZIPA        ANDEQ     XXZIPA
     C     RCCPT4        ANDEQ     SCCPT4
     C     RCSTAT        IFNE      'I'
     C                   MOVEL     RCDSC1        SCDESC
     C                   MOVEL     RCDSC2        SCDSC2
     C                   Z-ADD     RCMRA         SCMRA
     C                   Z-ADD     RCANAM        SCANEA
     C                   Z-ADD     RCPROF        SCPRCO
     C                   MOVE      '0'           *IN23
     C                   MOVE      *BLANKS       SCPRCA
     C                   MOVEL     RCRPT         SCBRFL
     C                   MOVEL     RCSURG        SCASFL
     C                   MOVEL     RCSUPP        SCSIFL
     C                   MOVEL     RCFSR         SCFSFL
     C                   Z-ADD     *ZEROS        SC92N1
     C                   Z-ADD     *ZEROS        SC92N2                         ADD 12/10/92
     C                   ELSE                                                   ELSE NE 'I' - RCSTAT
     C                   GOTO      NXTRC1
     C                   END                                                    END NE 'I' - RCSTAT
     C                   END                                                    END 0 *IN60 - PRC01
     C                   ELSE                                                   ELSE 1 *IN60 - PPR01
     C     PRSTAT        IFNE      'I'
     C                   MOVEL     PRDSC1        SCDESC
     C                   MOVEL     PRDSC2        SCDSC2
     C                   Z-ADD     PRMRA         SCMRA
     C                   MOVE      PRFOLU        SCFOLU                         ADD 04/22/93
     C     PR1A1         IFEQ      'A'
     C                   MOVE      '1'           *IN23
     C                   MOVEL     'ANE'         SCPRCA
     C                   Z-ADD     *ZEROS        SCPRCO
     C                   ELSE                                                   ELS PR1A1 = 'A'
     C                   MOVE      '0'           *IN23
     C*** BEGIN ADD *** 12/30/93 **************************************
     C                   MOVEL     PR1A1         SC1A1
     C*** END ADD *** 12/30/93 ****************************************
     C                   MOVE      *BLANKS       SCPRCA
     C                   Z-ADD     PRPROF        SCPRCO
     C                   END                                                    END PR1A1 = 'A'
     C                   MOVE      PR1A2         SC1A2                          ADD 02/16/93
     C                   Z-ADD     PRANAM        SCANEA
     C                   MOVEL     PRRPT         SCBRFL
     C                   MOVEL     PRSURG        SCASFL
     C                   MOVEL     PRSUPP        SCSIFL
     C                   MOVEL     PRFSR         SCFSFL
     C                   Z-ADD     PR92N1        SC92N1
     C                   Z-ADD     PR92N2        SC92N2                         ADD 12/10/92
     C                   Z-ADD     PR92N3        SC92N3                         ADD 04/22/93
     C                   Z-ADD     PR92N4        SC92N4                         ADD 05/07/93
     C                   Z-ADD     PR92N5        SC92N5                         ADD 07/20/93
     C                   MOVE      PR1A3         SC1A3                          ADD 04/22/93
     C                   MOVE      PR1A4         SC1A4                          ADD 04/22/93
     C                   MOVE      PR1A5         SC1A5                          ADD 04/22/93
     C                   ELSE                                                   ELSE NE 'I' - PRSTAT
     C                   GOTO      NXTPR1
     C                   END                                                    END NE 'I' - PRSTAT
     C                   END                                                    END 1 *IN60 - PPR01
     C*
     C*
     C*** BEGIN ADD *** 04/22/93 **************************************
     C                   ELSE                                                   EQ CARCFL=X
     C                   MOVE      '1'           *IN35
     C*** BEG ADD *** 08/31/99 ****************************************
     C                   MOVE      *BLANKS       SCNDC
     C     ##PRVB        IFNE      'Y'
     C*** END ADD *** 08/31/99 ****************************************
     C     HCKY          CHAIN     PHC01                              55
     C     *IN55         IFEQ      '0'
     C                   Z-ADD     HC6N1         SCPZIP
     C                   MOVEL     HCNDC         SCNDC
     C                   MOVEL     HCRX#         SCRX#
     C                   Z-ADD     HCRXMQ        SCRXMQ
     C*** BEG ADD *** 08/31/99 ****************************************
     C                   ENDIF                                                  *IN55 = 0
     C                   ELSE
     C     DPKY          CHAIN     PDP01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     DPNDC         SCNDC
     C                   MOVEL     DPRX#         SCRX#
     C                   ENDIF                                                  *IN60 = *OFF
     C*
     C                   ENDIF
     C*** END ADD *** 08/31/99 ****************************************
     C     RXKEY         CHAIN     PRX01                              56
     C     *IN56         IFEQ      '0'
     C                   MOVEL     RXNAME        SCNAME
     C                   MOVEL     RXSTRN        SCSTRN
     C                   Z-ADD     RXTOTU        SCTOTU
     C                   Z-ADD     RXPSZE        SCPSZE
     C                   MOVEL     RXUNTD        SCUNTD
     C                   Z-ADD     RXAWPD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCAWPD
     C                   Z-ADD     RXAWP         SCAWP
     C                   Z-ADD     RX1AWP        SC1AWP
     C                   MOVEL     RXPTYP        SCPTYP
     C                   Z-ADD     1             Q                 5 0
     C     SCPTYP        LOOKUP    PTYP(Q)                                57
     C     *IN57         IFEQ      '1'
     C                   MOVEL     PTYD(Q)       SCTDES
     C                   ELSE
     C                   MOVE      *BLANKS       SCTDES
     C                   END
     C                   MOVEL     RXFORM        SCFORM
     C                   Z-ADD     1             Y                 5 0
     C     SCFORM        LOOKUP    FORM(Y)                                58
     C     *IN58         IFEQ      '1'
     C                   MOVEL     FORD(Y)       SCFDES
     C                   ELSE
     C                   MOVE      *BLANKS       SCFDES
     C                   END
     C                   MOVEL     RXMEAS        SCMEAS
     C     SCMEAS        IFEQ      'EA'
     C                   MOVEL     'Each'        SCMDES
     C                   ELSE
     C     SCMEAS        IFEQ      'GM'
     C                   MOVEL     'Gram'        SCMDES
     C                   ELSE
     C     SCMEAS        IFEQ      'ML'
     C     'Millilit'    CAT       'er'          SCMDES
     C                   END
     C                   END
     C                   END
     C                   END
     C*** BEG ADD *** 08/31/99 ****************************************
     C**********           ELSE
     C**********           MOVE *BLANKS   SCNDC
     C**********           END
     C*** END ADD *** 08/31/99 ****************************************
     C                   END
     C*** END ADD *** 04/22/93 ****************************************
     C*
     C                   END
     C                   MOVEL     CAPMOD        SCPMOD
     C     CAPMOD        COMP      *BLANKS                            2222
     C****** MUST GET PMOD DESC IF IT EXISTS
     C                   MOVEL     *BLANKS       XXKEY
     C                   MOVEL     CAPMOD        XXKEY
     C                   MOVEL     *BLANK        PMDESC
     C     TBKEY         CHAIN     PPM01                              60
     C                   MOVEL     PMDESC        SCPMDE
     C*
     C*
     C                   Z-ADD     CAANMN        SCANMN
     C                   Z-ADD     CAUNTS        SCUNTS
     C*** BEGIN CHG *** 12/01/93 ************************************
     C**********           Z-ADDCATOTC    SCTOTC
     C*** BEGIN DEL *** 10/16/96 *************************************
     C********** CACPT4    IFNE 'FSMRA'
     C**********           Z-ADDCATOTC    SCTOTC
     C**********           ELSE                            ELSE NE FSMRA
     C**********           Z-ADD*ZEROS    SCTOTC
     C**********           END                             END NE FSMRA
     C*** END   DEL *** 10/16/96 *************************************
     C*** END CHG ***** 12/01/93 ************************************
     C*** BEG CHG *** 05/17/99 ****************************************
     C     ##PRVB        IFNE      'Y'
     C                   Z-ADD     CATOTC        SCTOTC
     C                   Z-ADD     CATOTP        SCTOTP
     C                   Z-ADD     CATOTD        SCMRAR
     C                   ELSE
     C                   Z-ADD     CACHRG        SCTOTC
     C                   Z-ADD     CAPAID        SCTOTP
     C                   Z-ADD     CAMRA         SCMRAR
     C                   Z-ADD     CAOTH2        SCOTH2
     C                   ENDIF                                                  ##PRVB
     C*** END CHG *** 05/17/99 ****************************************
     C*** BEGIN ADD *** 04/22/93 **************************************
     C                   Z-ADD     CAOTHR        SCOTHR
     C                   Z-ADD     CANDIS        SCNTWK
     C*** END ADD *** 04/22/93 ****************************************
     C     *IN30         IFEQ      '1'                                          ADD 04/22/93
     C*** BEG CHG *** 05/17/99 ****************************************
     C     ##PRVB        IFNE      'Y'
     C                   MOVE      CAEOMB        SCEOMB
     C                   ELSE
     C                   CLEAR                   SCEOMB
     C     DEKY          CHAIN     PDE01                              60
     C     *IN60         DOWEQ     *OFF
     C     DEEOB         IFNE      *BLANKS
     C                   MOVEL(P)  DEEOB         SCEOMB
     C                   LEAVE
     C                   ENDIF                                                  EOEOB <> *BLANKS
     C     DEKY          READE     PDE01                                  60
     C                   ENDDO                                                  *IN60 = *OFF
     C                   ENDIF                                                  ##PRVB
     C*** END CHG *** 05/17/99 ****************************************
     C                   END                                                    ADD 04/22/93
     C*
     C                   MOVEL     *BLANKS       XXKEY
     C*** BEG CHG *** 05/17/99 ****************************************
     C     ##PRVB        IFNE      'Y'
     C                   MOVEL     CAEOMB        XXKEY
     C                   ELSE
     C                   MOVEL     SCEOMB        XXKEY
     C                   ENDIF                                                  ##PRVB
     C*** END CHG *** 05/17/99 ****************************************
     C                   MOVEL     *BLANK        EBDESC
     C     TBKEY         CHAIN     PEB01                              55
     C                   MOVEL     EBDESC        SCEBDS
     C*
     C                   MOVE      CAPART        SCPART
     C*
     C                   MOVEL     CAPART        TABKEY
     C                   MOVEL     *BLANK        PADESC
     C     PAKY          CHAIN     PPA01                              55
     C                   MOVEL     PADESC        SCPTDS
     C*
     C                   MOVEL     *BLANKS       XXKEY
     C                   MOVEL     CAP1          XXKEY
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C*** BEGIN CHG 04/22/93 ******************************************
     C     *IN33         IFEQ      '1'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C     *IN34         IFEQ      '1'
     C                   MOVEL     'SCR02'       Q2RECF
     C                   END
     C     *IN35         IFEQ      '1'
     C                   MOVEL     'SCR03'       Q2RECF
     C                   END
     C*** END ADD *** 04/22/93 ****************************************
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C*
     C*** BEGIN ADD *** 04/22/93 **************************************
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
     C     Q2RECF        IFEQ      'SCR03'
     C                   EXFMT     SCR03
     C                   END
     C*** END ADD *** 04/22/93 ****************************************
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C*
     C     *IN11         IFEQ      '1'
     C*
     C**********************************************
     C*   F3 = Exit
     C**********************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C*** BEGIN ADD *** 11/20/94 ********************************************
     C**********************************************
     C*   F6 = Code Inquiry
     C**********************************************
     C     *INKF         IFEQ      '1'
     C**************************
     C*   SCR01 - CPT4 Inquiry *
     C**************************
     C     Q2RECF        IFEQ      'SCR01'
     C*** BEG CHG *** 08/31/99 ****************************************
     C**********           CALL 'F0020C'
     C**********           PARM           QQPRDS
     C**********           PARM '1'       QXCALL
     C**********           PARM           CAAST
     C*
     C                   Z-ADD     SCFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         KKFDAT
     C                   MOVE      EOAST         ##ST              2
     C     CARCFL        IFEQ      'C'
     C                   MOVEL(P)  SCCPT4        KKPRCD
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  60
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     0             ##RRN
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     PRRRN         ##RRN
     C                   ENDIF                                                  *IN99 = On
     C                   CALL      'F2305C01'
     C                   PARM      ' '           @@KC              1
     C                   PARM      ' '           @@KL              1
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRN
     C                   PARM                    ##ST
     C                   ENDIF                                                  RCFP = 'C'
     C     CARCFL        IFEQ      'Y'
     C                   MOVEL     SCZIPA        KKZIPA
     C                   MOVEL(P)  SCCPT4        KKCPT4
     C                   MOVEL     KKFDAT        KKEFDT
     C     RCKY1         SETGT     PRC01
     C     RCKY2         READPE    PRC01                                  60
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     0             ##RRN
     C                   ELSE                                                   *IN60 = On
     C                   Z-ADD     RCRRN         ##RRN
     C                   ENDIF                                                  *IN60 = On
     C                   CALL      'F2310R01'
     C                   PARM      ' '           @@KC
     C                   PARM      ' '           @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRN
     C                   PARM                    ##ST
     C                   ENDIF                                                  RCFP = 'Y'
     C     @@KL          IFEQ      '1'
     C                   GOTO      STRT
     C                   ENDIF                                                  @@KL = '1'
     C*** END CHG *** 08/31/99 ****************************************
     C                   END                                                    END SCR01
     C**************************
     C*   SCR02 - Serv Inquiry *
     C**************************
     C     Q2RECF        IFEQ      'SCR02'
     C*** BEG CHG *** 08/31/99 ****************************************
     C**********           CALL 'F0030C'
     C**********           PARM           QQPRDS
     C**********           PARM '1'       QXCALL
     C**********           PARM           CAAST
     C                   MOVE      EOAST         ##ST              2
     C                   MOVEL     XXCMSV        K1CMSV
     C                   Z-ADD     SCFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         K1EFDT
     C     SVKY1         SETGT     PSV01
     C     SVKY2         READPE    PSV01                                  60
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     0             ##RRN
     C                   ELSE                                                   *IN60 = On
     C                   Z-ADD     SVRRN         ##RRN
     C                   ENDIF                                                  *IN60 = On
     C                   CALL      'F2320C01'
     C                   PARM      ' '           @@KC
     C                   PARM      ' '           @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRN
     C                   PARM                    ##ST
     C*** END CHG *** 08/31/99 ****************************************
     C                   END                                                    END SCR02
     C**************************
     C*   SCR03 - Rx Inquiry   *
     C**************************
     C     Q2RECF        IFEQ      'SCR03'
     C*** BEG CHG *** 08/31/99 ****************************************
     C**********           CALL 'F0050R'
     C**********           PARM           QQPRDS
     C**********           PARM '1'       QXCALL
     C     RXKEY         CHAIN     PRX01                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     RXRRN         ##RRN
     C                   ELSE
     C                   Z-ADD     0             ##RRN
     C                   ENDIF                                                  *IN60 = *OFF
     C                   CALL      'F2350R01'
     C                   PARM      ' '           @@KC
     C                   PARM      ' '           @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRN
     C     @@KL          IFEQ      '1'
     C                   GOTO      STRT
     C                   ENDIF                                                  @@KL = '1'
     C*** END CHG *** 08/31/99 ****************************************
     C                   END                                                    END SCR03
     C****
     C                   END                                                    END *INKF
     C*** END ADD ***** 11/20/94 ********************************************
     C*
     C*** BEG ADD *** 05/17/99 ****************************************
     C*
     C*************************************
     C*   F11 = EOB Codes/Notes           *
     C*************************************
     C*
     C     *INKK         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR02'
     C     Q2RECF        OREQ      'SCR03'
     C*
     C* GET FAEOP RELATIVE RECORD NUMBER
     C*
     C     EOKY          CHAIN     PEO01                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     DBRRNB        ##RRN2
     C                   ELSE
     C                   Z-ADD     0             ##RRN2
     C                   ENDIF
     C*
     C                   CALL      'F0356R'
     C                   PARM      '1'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C                   PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM                    ##RRN2            9 0          faeop
     C                   PARM      DBRRNA        ##RRN3            9 0          fdcap
     C                   GOTO      STRT
     C                   ENDIF                                                  Q2RECF = SCR01
     C                   ENDIF                                                  *INKL = '1'
     C*** END ADD *** 05/17/99 ****************************************
     C*
     C**********************************************
     C*   F12 = Previous
     C**********************************************
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C*
     C                   END
     C*
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C*
     C   99              CLOSE     F0574F
     C   99              MOVE      '0'           *IN99
     C                   MOVE      *BLANKS       QQCALL
     C*
     C                   RETURN
     C                   MOVE      '1'           *INLR
     C*
     OPCA01     E            RLSCAE
** TYPE - SERVICE TYPE DESCRIPTIONS
Hospital Room Charge
Misc. Charge
Sur/Ane Charge
Lab/X-ray Charge
Other Charge
Hospital Outpatient Services
**  QCMDEXC TABLE
DLTOVR FILE(FMPRL)
OVRDBF FILE(FMPRL) TOFILE(*LIBL/FMPRL  )
DLTOVR FILE(FTABL)
OVRDBF FILE(FTABL) TOFILE(*LIBL/FTABL  )
DLTOVR FILE(FMSVL1)
OVRDBF FILE(FMSVL1) TOFILE(*LIBL/FMSVL1  )
**  PRODUCT TYPE AND DESCRIPTION
01RX-Only/Trade
02RX-Only/Generic
03Surgical
05RX-Only/Generic
07OTC/Trade
08OTC/Generic
**  FORM TYPE AND DESCRIPTION
ACCAccessories
AERAerosol
AMPAmpuls
BALBalm
BANBandage
CAPCapsules
CRECream
DEVDevice
DIADiaphragm
DREDressing
DRUDrum
ELIElixir
ENSEnseals
EXPExpectorant
EXTExtract
FLAFlakes
GAUGauze
GELGEL
GRAGranular
INHInhalent
INLInhaler
INJInjection
INTIntravenous
KITKit
LEALeave
LIQLiquid
LOTLotion
LOZLozenge
NEBNebulizer
NDLNeddle
OILOil
OINOintment
PACPacket
PADPad
PASPaste
PIPPipette
POWPowder
PROProphylactics
PULPulvules
RUBRub
SALSalve
SETSet
SOASoap
SOLSolution
SPGSponge
SPRSpray
STIStick
SUPSuppository
SUSSuspension
SWASwab
SRNSyringe
SYRSyrup
TABTablets
TAMTampon
TAPTape
TESTest
THEThermometer
TINTincture
TONTonic
TROTroches
TUBTube
VAPVaporizer
VIAVial
WAFWafer
