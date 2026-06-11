     H Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H BNDDIR('STBNDDIR')
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE F0353RLA - Louisiana Physician (HFCA) Detail Information
     H*****************************************************************
     H*
     H* F0353RLA - Louisiana Physician (HFCA) Detail Information
     H*
     H*   PROGRAMMER: C:296
     H*   CREATED:   05/20/14      PROJECT: MC378
     H*                            COPIED FROM F0353FFL
     H*****************************************************************
     H*
     H** CALLED FROM:
     H*    F0350R99 - PBILLS CONTROLLING PROGRAM
     H*    F0390R   - WORK WITH PBILLS DETAIL
     H*    F0350R   - WORK WITH PBILLS
     H*  CALLS TO:
     H*    N0037R   - SELECT NETWORK CONTRACTS
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  07/18/25  C:342 Project MC451
     H*                  NCCI Med Data Call Enhancements
     H*                  Added STBNDDIR
     H*
     H*  07/18/23 C:342 Project: NY060
     H*                 NY Compliance: CARC/RARC Update
     H*
     H*  09/05/16 C:309   Support: PHW43225
     H*                   Recompile onlye (screen change)
     H*
     H*  09/15/15  C:079  Support: MC090 37261
     H*                   Move ICD-10 description over two spaces
     H*
     H*  05/19/15  C:079  Support: MC129 34743
     H*                   On a recon do not update FDDHP
     H*
     H*  07/11/14  C:280  Support: 86063
     H*                   Change KKICD9 to movel
     H*
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      29      Add mode                                       **
     H**      12      County code blank/invalid                      **
     H*****************************************************************
     H*
     FF0353FLA  CF   E             WORKSTN
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F*
     F*  Physical file definition (for maintaining record)
     FFDDHP     UF A E             DISK
     F                                     RENAME(PDH01:DBRCDF)
     F*
     F*  EOMB Transaction File
     FFAEOP     UF   E             DISK
     F*
     F*  Type of Service File
     FFTTSP     IF   E             DISK
     F*
     F*  Fee Schedule Detail File
     F*** BEG CHG *** 05/18/99 ****************************************
     F**********FDCALB  IF  E           K        DISK
     FFDCALF    IF   E           K DISK
     F*** END CHG *** 05/18/99 ****************************************
     F*
     F*  Fee Schedule Detail File
     FFDCAP     UF A E             DISK
     F                                     RENAME(PCA01:PCA02)
     FFDCAL12   IF   E           K DISK
     F                                     RENAME(PCA01:PCA03)
     FFDCAL10   IF   E           K DISK
     F                                     RENAME(PCA01:PCA04)
     F*** BEGIN ADD *** 08/08/07 **************************************
     FFDCAL28   IF   E           K DISK
     F                                     RENAME(PCA01:PCA05)
     F*** END ADD****** 08/08/07 **************************************
     F*
     F*  EOMB Detail EOB/Notes Transaction File
     F*** BEG CHG *** 02/23/99 ****************************************
     F**********FDDEL   IF  E           K        DISK
     FFDDEL     UF   E           K DISK
     F*** END CHG *** 02/23/99 ****************************************
     F*** BEGIN ADD *** 03/26/01 **************************************
     FFDDEL3    IF   E           K DISK
     F                                     RENAME(PDE01:PDE03)
     F*** END   ADD *** 03/26/01 **************************************
     F*
     F*  Fee Schedule Claims Status Code File
     FFTCSP     IF   E             DISK
     F                                     RENAME(PCS01:PCS02)
     F*
     F*  Place of Service Table File
     FFTPSL     IF   E           K DISK    USROPN
     F*
     F*  Procedure Code File
     FFMPRL     IF   E           K DISK    USROPN
     F                                     INFDS(DBFPR)
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
     F*  EOMB Phys/HCFA Header File
     FFAEHL     UF   E           K DISK
     F*
     F*  Diagnosis Code Physical File
     FFMDIL     IF   E           K DISK
     F*
     F*  Logical over all Tabel Files
     FFTABL     IF   E           K DISK    USROPN
     F                                     RENAME(PTS01:PTS02)
     F                                     IGNORE(PAC01)
     F*** BEG DEL *** 09/16/99 ****************************************
     F**********  PEB01                             KIGNORE
     F*** END DEL *** 09/16/99 ****************************************
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PSP01)
     F                                     IGNORE(PAT01)
     F                                     IGNORE(PFT01)
     F*** BEG DEL *** 09/28/04 ****************************************
     F*********** PCC01                             KIGNORE
     F*** END DEL *** 09/28/04 ****************************************
     F                                     IGNORE(PPS01)
     F*** BEG ADD *** 09/28/04 ****************************************
     FFTCCP     IF   E             DISK
     F                                     RENAME(PCC01:PCC02)
     FFAAGLTX   IF   E           K DISK
     F                                     INFDS(DBFDD)
     F*** END ADD *** 09/28/04 ****************************************
     F*
     F* Pre-Cert Master File
     FUMPCP     IF   E             DISK
     FUMPCL3    IF   E           K DISK
     F                                     RENAME(PPC01:PPC02)
     F*
     F* WCMC Claim Master File
     FFMCML1    IF   E           K DISK
     F                                     INFDS(DBFDB)
     FFDDHL     IF   E           K DISK
     F*** BEG ADD *** 02/23/99 ****************************************
     F                                     INFDS(DBFDC)
     F*** END ADD *** 02/23/99 ****************************************
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
     F* REASONABLE AND CUSTORMARY FILE
     F*
     FFMRCL     IF   E           K DISK
     F                                     INFDS(DBFRC)
     F*** BEG ADD *** 02/23/99 ****************************************
     FFDMGL2    UF   E           K DISK
     F*** END ADD *** 02/23/99 ****************************************
     F*** BEGIN ADD *** 07/23/99 **************************************
     F*** BEGIN DEL *** 01/11/00 **************************************
     F**********FMPDL   IF  E           K        DISK
     F**********FMS4L3  IF  E           K        DISK
     F**********FMS4L2  IF  E           K        DISK
     F**********  PS401                             KRENAMEPS402
     F*** END   DEL *** 01/11/00 **************************************
     F*** END ADD *** 07/23/99 ****************************************
     F*** BEG ADD *** 09/16/99 ****************************************
     FFDDEP     O    E             DISK
     F                                     RENAME(PDE01:PDE02)
     F*** END ADD *** 09/16/99 ****************************************
     F*** BEG ADD *** 04/11/00 ****************************************
     FFAESL5    UF   E           K DISK
     F*** END ADD *** 04/11/00 ****************************************
     F*** BEG ADD *** 12/08/00 ****************************************
     F*  State Code Table File
     FSTSTL     IF   E           K DISK
     F*** END ADD *** 12/08/00 ****************************************
     F*** BEGIN ADD *** 08/27/01 **************************************
     FFMVCLTX   IF   E           K DISK
     F*** END   ADD *** 08/27/01 **************************************
     F*** BEGIN ADD *** 08/07/02 **************************************
     F*** BEG CHG *** 09/28/04 ****************************************
     F***** FMP1L   IF  E           K        DISK
     FFMP1L     UF   E           K DISK
     F*** END CHG *** 09/28/04 ****************************************
     FFMGZL     IF   E           K DISK
     F*** END   ADD *** 08/07/02 **************************************
     F*
     F*  Error log file
     F*** BEG ADD *** 09/19/02 ****************************************
     FFMZRL     IF   E           K DISK    USROPN
     F*** BEG ADD *** 10/31/05 ****************************************
     F*
     F*  WCMC NDC Drug Code File
     FFMRXP     IF   E             DISK
     F                                     RENAME(PRX01:PRX02)
     FFMRXL     IF   E           K DISK
      *** Beg Add *** 02/04/14 ****************************************
     Ffmicd10l1 if   e           k disk
      *** End Add *** 02/04/14 ****************************************
      /COPY SCOPYMT01D
     D*** END ADD *** 10/31/05 ****************************************
     D*
     D QCA             S             56    DIM(4) CTDATA PERRCD(1)
     D*** END ADD *** 09/19/02 ****************************************
     D*** BEG ADD *** 04/15/04 ****************************************
     D*** BEG CHG *** 04/22/07 ****************************************
     D******************  EOBR1   1  13  3  A
     D******************  EOBR2   1   4  3  A
     D EOBR1           S              3    DIM(29) CTDATA PERRCD(1) ASCEND
     D EOBR2           S              3    DIM(15) CTDATA PERRCD(1) ASCEND
     D*** END ADD *** 08/08/07 ****************************************
     D*** BEG DEL *** 11/12/99 ****************************************
     D**********PDA02
     D**********    DACO#                           D2CO#
     D**********    DASUB#                          D2SUB#
     D**********    DAFND                           D2FND
     D**********    DAFYR                           D2FYR
     D**********    DACASE                          D2CASE
     D**********    DASS#                           D2SS#
     D**********    DAACDT                          D2ACDT
     D**********    DAEOB#                          D2EOB#
     D**********    DADSEQ                          D2DSEQ
     D**********    DARCN#                          D2RCN#
     D**********    DATRDT                          D2TRDT
     D**********    DACHRG                          D2CHRG
     D**********    DAMRA                           D2MRA
     D**********    DAOTHR                          D2OTHR
     D**********    DAOTH2                          D2OTH2
     D**********    DANDIS                          D2NDIS
     D**********    DAPAID                          D2PAID
     D**********    DAOTY1                          D2OTY1
     D**********    DAOTY2                          D2OTY2
     D**********    DATRCD                          D2TRCD
     D**********    DASTAT                          D2STAT
     D**********    DAUSER                          D2USER
     D**********    DAPGM                           D2PGM
     D**********    DATIME                          D2TIME
     D**********    DADT                            D2DT
     D**********    DAIUSR                          D2IUSR
     D**********    DAIPGM                          D2IPGM
     D**********    DAITIM                          D2ITIM
     D**********    DAIDT                           D2IDT
     D*** BEG DEL *** 11/12/99 ****************************************
     D DBFDB           DS
     D  DBRRNB               397    400B 0
     D DBFPR           DS
     D  PRRRN                397    400B 0
     D DBFRC           DS
     D  RCRRN                397    400B 0
     D*** BEG ADD *** 02/23/99 ****************************************
     D DBFDC           DS
     D  DBRRNC               397    400B 0
     D DBFDD           DS
     D  DBRRND               397    400B 0
     D*** END ADD *** 02/23/99 ****************************************
     D***PY SCOPYMT01I
     D*
     D                 DS
     D  TMTOS                  1      5
     D  HLKEY                  1      2
     D  HDKEY                  3      5
     D                 DS
     D  QQCMD                  1     56
     D  QQST                  38     39
     D*** BEG ADD *** 09/08/04 ****************************************
     D                 DS
     D  P1CNTY                 1     15
     D  YYCNTY                 6      9
      *** Beg Add *** 02/04/14 ****************************************
     D kkicd10         s                   like(iccode)
     DXFL              S              1A
      *** End Add *** 02/04/14 ****************************************
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Physician (HCFA) Det-
     D                                     ail Information')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)
     D*** Beg Add *** 07/18/25 ***************************************
     D invalidPos      s              2  0
      /copy sPrModule
     D*** End Add *** 07/18/25 ***************************************

     I*** END CHG *** 04/22/07 ****************************************
     I*** END ADD *** 04/15/04 ****************************************
     I*
     I*** BEG ADD *** 08/08/07 ****************************************
     IPCA05
     I              CAMSEQ                      CAMSQ#

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          FDDHP
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
     C*
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
     C*** BEG ADD *** 08/08/07 ****************************************
     C     CAKYM1        KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKMSEQ            5 0
     C*
     C     CAKYM2        KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*** END ADD *** 08/08/07 ****************************************
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
     C*  KEY LIST FOR PROCEDURE CODE FILE
     C*
     C     PRKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKPRCD
     C                   KFLD                    KKFDAT
     C*
     C     PRKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKPRCD
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
     C*** BEG DEL *** 11/12/99 ****************************************
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
     C*  Key List for Diagnosis Code File
     C*
     C     DIKY          KLIST
     C                   KFLD                    KKICD9
     C                   KFLD                    KKLOST
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
     C     DHKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
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
     C*** BEGIN DEL *** 08/15/00 **************************************
     C********** FBKY      KLIST
     C**********           KFLD           XSKEY   2
     C*** END   DEL *** 08/15/00 **************************************
     C*
     C* key list for reasonable and customary file
     C*
     C     RCKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKZIPA
     C                   KFLD                    KKCPT4
     C                   KFLD                    KKEFDT
     C*
     C     RCKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKZIPA
     C                   KFLD                    KKCPT4
     C*
     C*** BEG ADD *** 02/23/99 ****************************************
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
     C*** END ADD *** 02/23/99 ****************************************
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
     C*** END ADD *** 07/23/99 ****************************************
     C*** END   DEL *** 01/11/00 **************************************
     C*** BEG ADD *** 04/11/00 ****************************************
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
     C*** END ADD *** 04/11/00 ****************************************
     C*** BEG ADD *** 12/08/00 ****************************************
     C** Key List for State Code Table File
     C*
     C     STKY          KLIST
     C                   KFLD                    @@KEY
     C*** END ADD *** 12/08/00 ****************************************
     C*** BEGIN ADD *** 08/27/01 **************************************
     C*
     C*  KEY LIST FOR VALID STATE CODE FILE
     C*
     C     VCKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKPRCD
     C                   KFLD                    KKFDAT
     C*
     C     VCKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKPRCD
     C*** END   ADD *** 08/27/01 **************************************
     C*** BEG ADD *** 09/19/02 ****************************************
     C     ZRKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFZCD
     C                   KFLD                    KKEFDT
     C*** END ADD *** 09/19/02 ****************************************
     C*** BEG ADD *** 09/28/04 ****************************************
     C     CCKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY
     C*
     C     AGKY          KLIST
     C                   KFLD                    KKGRP
     C                   KFLD                    KKRGN
     C                   KFLD                    KKEFDT
     C*
     C     AGKY1         KLIST
     C                   KFLD                    KKGRP
     C                   KFLD                    KKRGN
     C*** END ADD *** 09/28/04 ****************************************
     C*** BEG ADD *** 10/31/05 ****************************************
     C*  KEY LIST FOR PROCEDURE CODE FILE
     C*
     C     RXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKNDC
     C*** END ADD *** 10/31/05 ****************************************
      *** Beg Add *** 02/04/14 ****************************************
     C*   KEY LIST FOR DIAGNOSIS CODE MASTER FILE
     C     fmicd10l1key  klist
     C                   kfld                    kkicd10
      *** End Add *** 02/04/14 ****************************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    DHTOS         HLTOS
     C     *LIKE         DEFINE    DHPRCD        HLCPT4
     C     *LIKE         DEFINE    DHMOD1        HLMOD
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
     C     *LIKE         DEFINE    PSPOS         KKPOS
     C     *LIKE         DEFINE    PRCPT4        KKPRCD
     C     *LIKE         DEFINE    PREFDT        KKFDAT
     C     *LIKE         DEFINE    BSTYPE        KKTYPE
     C     *LIKE         DEFINE    BSCODE        KKCODE
     C     *LIKE         DEFINE    DIICD9        KKICD9
     C     *LIKE         DEFINE    DILOST        KKLOST
     C*** BEG ADD *** 09/19/02 ****************************************
     C     *LIKE         DEFINE    ZRFZCD        KKFZCD
     C*** END ADD *** 09/19/02 ****************************************
     C     *LIKE         DEFINE    CADCS#        XXDCS#
     C     *LIKE         DEFINE    CADCS#        KKDCS#
     C     *LIKE         DEFINE    EOFRDT        HKFDAT
     C     *LIKE         DEFINE    EOCHRG        TLCHRG
     C     *LIKE         DEFINE    CALIN#        KKLIN#
     C     *LIKE         DEFINE    CAOT1C        HLOT1C
     C     *LIKE         DEFINE    CAOT2C        HLOT2C
     C     *LIKE         DEFINE    CARCN         KKRCN
     C********** *LIKE     DEFN CATOTC    SMCHG
     C     *LIKE         DEFINE    CACHRG        SMCHG
     C********** *LIKE     DEFN CATOTD    SMMRA
     C     *LIKE         DEFINE    CAMRA         SMMRA
     C     *LIKE         DEFINE    CANDIS        SMNDIS
     C     *LIKE         DEFINE    CAOTHR        SMOTHR
     C     *LIKE         DEFINE    CAOTH2        SMOTH2
     C********** *LIKE     DEFN CATOTP    SMPAID
     C     *LIKE         DEFINE    CAPAID        SMPAID
     C********** *LIKE     DEFN CATOTC    DFCHRG
     C     *LIKE         DEFINE    CACHRG        DFCHRG
     C********** *LIKE     DEFN CATOTD    DFMRA
     C     *LIKE         DEFINE    CAMRA         DFMRA
     C     *LIKE         DEFINE    CANDIS        DFNDIS
     C     *LIKE         DEFINE    CAOTHR        DFOTHR
     C     *LIKE         DEFINE    CAOTH2        DFOTH2
     C     *LIKE         DEFINE    CAPAID        DFPAID
     C     *LIKE         DEFINE    CALIN#        ZZLIN#
     C     *LIKE         DEFINE    CADSEQ        ZZDSEQ
     C     *LIKE         DEFINE    DHUNTS        XXUNTS
     C     *LIKE         DEFINE    RUST          KKST
     C     *LIKE         DEFINE    RUATYP        KKATYP
     C     *LIKE         DEFINE    RUEFDT        KKEFDT
     C     *LIKE         DEFINE    EOCRDT        HLSYDT
     C     *LIKE         DEFINE    CAPAID        XXTOTL
     C     *LIKE         DEFINE    RCZIPA        KKZIPA
     C     *LIKE         DEFINE    RCCPT4        KKCPT4
     C*** Beg Chg *** 07/18/23 **************************************
     C**** *LIKE         DEFINE    DEEOB         XXEOB
     C     *LIKE         DEFINE    DEEOBCD       XXEOB
     C*** End Chg *** 07/18/23 **************************************
     C*** BEG ADD *** 04/04/00 ****************************************
     C     *LIKE         DEFINE    CALIN#        ##LIN#
     C*** END ADD *** 04/04/00 ****************************************
     C*** BEG ADD *** 06/20/05 ****************************************
     C     *LIKE         DEFINE    CACHRG        ##CHRG
     C     *LIKE         DEFINE    CAPAID        ##PAID
     C*** END ADD *** 06/20/05 ****************************************
     C*** BEG ADD *** 12/08/00 ****************************************
     C     *LIKE         DEFINE    STKEY         @@KEY
     C*** END ADD *** 12/08/00 ****************************************
     C*** BEG ADD *** 04/11/00 ****************************************
     C     *LIKE         DEFINE    ESBLST        KKBLST
     C     *LIKE         DEFINE    EOCO#         KKCO#
     C*** BEG ADD *** 09/28/04 ****************************************
     C     *LIKE         DEFINE    AGRGN         KKRGN
     C     *LIKE         DEFINE    AGGRP         KKGRP
     C*** END ADD *** 09/28/04 ****************************************
     C     *LIKE         DEFINE    EOCRDT        HLCRDT
     C     *LIKE         DEFINE    EOCRDT        HLCSDT
     C     *LIKE         DEFINE    EOCRDT        HLRRDT
     C     *LIKE         DEFINE    EOCRDT        HLRSDT
     C*** END ADD *** 04/11/00 ****************************************
     C*** BEG ADD *** 08/08/00 ****************************************
     C     *LIKE         DEFINE    PR1A2         SV1A2
     C*** END ADD *** 08/08/00 ****************************************
     C*** BEG ADD *** 11/08/01 ****************************************
     C     *LIKE         DEFINE    CACHRG        SVCHRG
     C     *LIKE         DEFINE    CALIN#        SVLIN#
     C     *LIKE         DEFINE    CADSEQ        SVDSEQ
     C     *LIKE         DEFINE    CADSEQ        SVRCN
     C*** END ADD *** 11/08/01 ****************************************
     C*** BEGIN ADD *** 03/26/01 **************************************
     C     *LIKE         DEFINE    DENOT1        XXNOT1
     C     *LIKE         DEFINE    DENOT2        XXNOT2
     C*** END   ADD *** 03/26/01 **************************************
     C*** BEGIN ADD ****** 05/04/11 ************************************
     C     *LIKE         DEFINE    DENOT3        XXNOT3
     C     *LIKE         DEFINE    DENOT4        XXNOT4
     C*** END   ADD ****** 05/04/11 ************************************
     C*** BEGIN ADD *** 07/16/02 **************************************
     C     *LIKE         DEFINE    CADSEQ        HDDSEQ
     C*** END   ADD *** 07/16/02 **************************************
     C*** BEGIN ADD *** 08/08/07 **************************************
     C     *LIKE         DEFINE    CAMSEQ        HDMSEQ
     C*** END   ADD *** 08/08/07 **************************************
     C*** BEGIN ADD *** 08/07/02 **************************************
     C     *LIKE         DEFINE    GZFRZP        KKFRZP
     C     *LIKE         DEFINE    GZEFDT        HLEFDT
     C*** END   ADD *** 08/07/02 **************************************
     C*** BEGIN ADD *** 10/31/05 **************************************
     C     *LIKE         DEFINE    DHNDC         KKNDC
     C*** END   ADD *** 10/31/05 **************************************
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
     C*** BEG ADD *** 02/23/99 ****************************************
     C*
     C* DELETE EOB CODES/NOTES FOR THE LINE ITEM
     C*
     C     DEKY          CHAIN     PDE01                              99
     C     *IN99         DOWEQ     *OFF
     C                   DELETE    PDE01
     C     DEKY          READE     PDE01                                  99
     C                   ENDDO                                                  *IN99 = *OFF
     C*
     C*** BEG ADD *** 05/05/99 ****************************************
     C*
     C* DELETE THE DETAIL TRANSACTION RECORDS FOR LINE ITEM
     C*
     C     DAKY          CHAIN     PDA01                              99
     C     *IN99         DOWEQ     *OFF
     C                   DELETE    PDA01
     C     DAKY          READE     PDA01                                  99
     C                   ENDDO
     C*** END ADD *** 05/05/99 ****************************************
     C*
     C* DELETE ERROR MESSAGES FOR THE LINE ITEM
     C*
     C     MGKY          CHAIN     PMG01                              99
     C     *IN99         DOWEQ     *OFF
     C                   DELETE    PMG01
     C     MGKY          READE     PMG01                                  99
     C                   ENDDO                                                  *IN99 = *OFF
     C*** END ADD *** 02/23/99 ****************************************
     C*** BEGIN ADD *** 08/21/02 **************************************
     C                   EXSR      UFAEOP
     C*** END   ADD *** 08/21/02 **************************************
     C*
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
     C*** BEGIN ADD *** 11/11/05 **************************************
     C     ##SDTS        ANDNE     'Y'
     C*** END   ADD *** 11/11/05 **************************************
     C*** BEGIN ADD *** 07/05/02 **************************************
     C     EHEDI         IFEQ      'Y'
     C*** END   ADD *** 07/05/02 **************************************
     C*** BEG ADD *** 04/15/04 ****************************************
     C     EHEDI         OREQ      'E'
     C*** END ADD *** 04/15/04 ****************************************
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
     C*******BEG ADD ******03/11/10 ***************************
     C* SET OFF IN26 SETUP ON IF FROM NDC EDIT
     C     *IN26         IFEQ      '1'
     C     XFL           ANDEQ     '1'
     C                   SETOFF                                         26
     C                   ENDIF                                                  HLCRDT GT HLRSDT
     C*******END ADD ******03/11/10 ***************************
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
     C*** BEGIN ADD *** 11/11/05 **************************************
     C     ##SDTS        ANDNE     'Y'
     C*** END   ADD *** 11/11/05 **************************************
     C*** BEGIN ADD *** 07/05/02 **************************************
     C     EHEDI         IFEQ      'Y'
     C*** END   ADD *** 07/05/02 **************************************
     C*** BEG ADD *** 04/15/04 ****************************************
     C     EHEDI         OREQ      'E'
     C*** END ADD *** 04/15/04 ****************************************
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
     C*** BEGIN ADD *** 11/11/05 **************************************
     C     ##SDTS        ANDNE     'Y'
     C*** END   ADD *** 11/11/05 **************************************
     C*** BEGIN ADD *** 07/05/02 **************************************
     C     EHEDI         IFEQ      'Y'
     C*** END   ADD *** 07/05/02 **************************************
     C*** BEG ADD *** 04/15/04 ****************************************
     C     EHEDI         OREQ      'E'
     C*** END ADD *** 04/15/04 ****************************************
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
     C*** BEGIN ADD *** 11/11/05 **************************************
     C     ##SDTS        ANDNE     'Y'
     C*** END   ADD *** 11/11/05 **************************************
     C*** BEGIN ADD *** 07/05/02 **************************************
     C     EHEDI         IFEQ      'Y'
     C*** END   ADD *** 07/05/02 **************************************
     C*** BEG ADD *** 04/15/04 ****************************************
     C     EHEDI         OREQ      'E'
     C*** END ADD *** 04/15/04 ****************************************
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
     C                   MOVEL     EHEDI         SCEDI
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
     C     EHKY          CHAIN     PEH01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     SCEDI         EHEDI
     C                   UPDATE    PEH01
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  *IN90=*OFF
     C                   ENDIF                                                  *INKL=*OFF
     C*
     C     EDIEND        ENDSR
     C*** END ADD *** 04/11/00 ****************************************
     C*** BEG ADD *** 08/02/00 ****************************************
     C*================================================================
     C*  EDICPT - EDI ERROR SR FOR USER DEFINED CPT CODES
     C*================================================================
     C     EDICPT        BEGSR
     C*
     C     CPTTAG        TAG
     C*
     C                   MOVEL     EHEDI         SCEDI
     C                   EXFMT     SCR04
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C     SCEDI         IFEQ      *BLANKS
     C                   SETON                                        9017
     C                   GOTO      CPTTAG
     C                   ENDIF                                                  SCEDI IFEQ BLANKS
     C*
     C     SCEDI         IFNE      *BLANKS
     C     SCEDI         ANDNE     'Y'
     C     SCEDI         ANDNE     'N'
     C                   SETON                                        9018
     C                   GOTO      CPTTAG
     C                   ENDIF                                                  SCEDI IFNE BLANKS
     C*
     C     *IN90         IFEQ      *OFF
     C     EHKY          CHAIN     PEH01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     SCEDI         EHEDI
     C                   UPDATE    PEH01
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  *IN90=*OFF
     C                   ENDIF                                                  *INKL=*OFF
     C*
     C     CPTEND        ENDSR
     C*** END ADD *** 08/02/00 ****************************************
     C*** BEG ADD *** 12/03/99 ****************************************
     C*================================================================
     C     EDITAL        BEGSR
     C*================================================================
     C*          ALABAMA STATE SPECIFIC EDITS
     C*
     C                   SELECT
     C*
     C* IF THIS IS A SURGERY CODE, BODY PART S1-S5 IS REQUIRED
     C     SCPRCD        WHENGE    '10000'
     C     SCPRCD        ANDLE     '69999'
     C     SCAREA        ANDNE     'S1'
     C     SCAREA        ANDNE     'S2'
     C     SCAREA        ANDNE     'S3'
     C     SCAREA        ANDNE     'S4'
     C     SCAREA        ANDNE     'S5'
     C*
     C                   SETON                                        9067
     C*
     C*** IF THIS IS A ANES CODE, BODY PART A1-A4 REQUIRED
     C     SCPRCD        WHENGE    '00100'
     C     SCPRCD        ANDLE     '01999'
     C     SCAREA        ANDNE     'A1'
     C     SCAREA        ANDNE     'A2'
     C     SCAREA        ANDNE     'A3'
     C     SCAREA        ANDNE     'A4'
     C*
     C                   SETON                                        9067
     C*
     C     SCPRCD        WHENEQ    '97752'
     C     SCAREA        ANDNE     '01'
     C     SCAREA        ANDNE     '02'
     C     SCAREA        ANDNE     '03'
     C     SCAREA        ANDNE     '04'
     C     SCAREA        ANDNE     '05'
     C     SCAREA        ANDNE     '06'
     C     SCAREA        ANDNE     '7A'
     C     SCAREA        ANDNE     '7B'
     C     SCAREA        ANDNE     '7C'
     C     SCAREA        ANDNE     '7D'
     C     SCAREA        ANDNE     '7E'
     C     SCAREA        ANDNE     '7F'
     C     SCAREA        ANDNE     '7G'
     C*
     C                   SETON                                        9067
     C*
     C                   ENDSL                                                  SCPRCD GE 10000
     C*
     C                   ENDSR
     C*** END ADD *** 12/03/99 ****************************************
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
     C*** Beg Chg *** 07/18/23 **************************************
     C*****              MOVEL(P)  DEEOB         KKKEY
     C                   MOVEL(P)  DEEOBCD       KKKEY
     C*** End Chg *** 07/18/23 **************************************
     C     TBKY          CHAIN     PEB01                              99
     C     *IN99         IFEQ      *OFF
     C     DESTAT        ANDEQ     'A'
     C     DEESRC        ANDNE     'A'
     C*
     C*******BEG ADD ******03/11/10 ***************************
     C                   SETOFF                                           98
     C*******END ADD ******03/11/10 ***************************
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
     C*** Beg Chg *** 07/18/23 **************************************
     C****               MOVEL(P)  DEEOB         KKKEY
     C                   MOVEL(P)  DEEOBCD       KKKEY
     C*** End Chg *** 07/18/23 **************************************
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
     C*** BEG ADD *** 11/23/99 ****************************************
     C*================================================================
     C     EDITFL        BEGSR
     C*================================================================
     C*          FLORIDA STATE SPECIFIC EDITS
     C*
     C* BODY AREA IS REQUIRED IF PHYS THERAPY CODE AND DATE < 9/1/97.
     C*
     C                   SELECT
     C*
     C     SCPRCD        WHENGE    '97010'
     C     SCPRCD        ANDLE     '97752'
     C     HLFDAT        ANDLT     0970901
     C     SCAREA        ANDNE     '2 '
     C     SCAREA        ANDNE     '3 '
     C     SCAREA        ANDNE     '4 '
     C     SCAREA        ANDNE     '5 '
     C     SCAREA        ANDNE     '6 '
     C*
     C                   SETON                                        9067
     C*
     C*** If this is an Anesthesia Code with mod -47, Body Part A1-A6 required
     C     SCMOD1        WHENEQ    '47'
     C     SCMOD2        OREQ      '47'
     C     SCMOD3        OREQ      '47'
     C     SCMOD4        OREQ      '47'
     C*
     C     SCAREA        IFNE      'A1'
     C     SCAREA        ANDNE     'A2'
     C     SCAREA        ANDNE     'A3'
     C     SCAREA        ANDNE     'A4'
     C     SCAREA        ANDNE     'A5'
     C*** BEG CHG *** 04/28/00 ****************************************
     C********** SCAREA    ANDNE'A5'
     C     SCAREA        ANDNE     'A6'
     C*** END CHG *** 04/28/00 ****************************************
     C*
     C                   SETON                                        9067
     C*
     C                   ENDIF
     C*
     C                   ENDSL                                                  SCPRCD GE 97010
     C*
     C                   ENDSR
     C*** END ADD *** 11/23/99 ****************************************
     C*** BEG ADD *** 12/03/99 ****************************************
     C*================================================================
     C     EDITLA        BEGSR
     C*================================================================
     C*          LOUISIANNA STATE SPECIFIC EDITS
     C*
     C                   SELECT
     C*
     C* IF THIS IS A THERMOGRAPHY CODE, BODY PART T1-T7 REQUIRED
     C     SCPRCD        WHENGE    '93760'
     C     SCPRCD        ANDLE     '93762'
     C     SCAREA        ANDNE     'T1'
     C     SCAREA        ANDNE     'T2'
     C     SCAREA        ANDNE     'T3'
     C     SCAREA        ANDNE     'T4'
     C     SCAREA        ANDNE     'T5'
     C     SCAREA        ANDNE     'T6'
     C     SCAREA        ANDNE     'T7'
     C*
     C                   SETON                                        9067
     C*
     C*** IF THIS IS A PHYS THERAPY CODE, BODY PART P1-P6 REQUIRED
     C     SCPRCD        WHENGE    'OT010'
     C     SCPRCD        ANDLE     'OT999'
     C     SCAREA        ANDNE     'P1'
     C     SCAREA        ANDNE     'P2'
     C     SCAREA        ANDNE     'P3'
     C     SCAREA        ANDNE     'P4'
     C     SCAREA        ANDNE     'P5'
     C     SCAREA        ANDNE     'P6'
     C*
     C                   SETON                                        9067
     C*
     C     SCPRCD        WHENGE    'PT010'
     C     SCPRCD        ANDLE     'PT999'
     C     SCAREA        ANDNE     'P1'
     C     SCAREA        ANDNE     'P2'
     C     SCAREA        ANDNE     'P3'
     C     SCAREA        ANDNE     'P4'
     C     SCAREA        ANDNE     'P5'
     C     SCAREA        ANDNE     'P6'
     C*
     C                   SETON                                        9067
     C*
     C     SCPRCD        WHENGE    '97010'
     C     SCPRCD        ANDLE     '97799'
     C     SCAREA        ANDNE     'P1'
     C     SCAREA        ANDNE     'P2'
     C     SCAREA        ANDNE     'P3'
     C     SCAREA        ANDNE     'P4'
     C     SCAREA        ANDNE     'P5'
     C     SCAREA        ANDNE     'P6'
     C*
     C                   SETON                                        9067
     C*
     C*** IF THIS IS A SURGERY CODE, BODY PART S1-S5 IS REQUIRED
     C     SCPRCD        WHENGE    '10000'
     C     SCPRCD        ANDLE     '69999'
     C*** BEG DEL *** 09/06/00 ****************************************
     C********** SCAREA    ANDNE'A '
     C********** SCAREA    ANDNE'B1'
     C********** SCAREA    ANDNE'B2'
     C********** SCAREA    ANDNE'O '
     C*** END DEL *** 09/06/00 ****************************************
     C*** BEG ADD *** 09/06/00 ****************************************
     C     SCAREA        ANDNE     'S1'
     C     SCAREA        ANDNE     'S2'
     C     SCAREA        ANDNE     'S3'
     C     SCAREA        ANDNE     'S4'
     C     SCAREA        ANDNE     'S5'
     C*** END ADD *** 09/06/00 ****************************************
     C*** BEG ADD *** 04/18/01 ****************************************
     C     SCAREA        ANDNE     'A '
     C     SCAREA        ANDNE     'B1'
     C     SCAREA        ANDNE     'B2'
     C     SCAREA        ANDNE     'O '
     C*** END ADD *** 04/18/01 ****************************************
     C*
     C                   SETON                                        9067
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C     EDITNE        BEGSR
     C*================================================================
     C*          NEBRASKA STATE SPECIFIC EDITS
     C*
     C                   SELECT
     C*
     C* IF THIS IS A SURGERY CODE, BODY PART S1-S5 REQUIRED
     C     SCPRCD        WHENGE    '10000'
     C     SCPRCD        ANDLE     '69999'
     C     SCAREA        ANDNE     'S1'
     C     SCAREA        ANDNE     'S2'
     C     SCAREA        ANDNE     'S3'
     C     SCAREA        ANDNE     'S4'
     C     SCAREA        ANDNE     'S5'
     C*
     C                   SETON                                        9067
     C*
     C                   ENDSL                                                  SCPRCD GE 10000
     C*
     C                   ENDSR
     C*================================================================
     C     EDITMS        BEGSR
     C*================================================================
     C*          MISSISSIPPI STATE SPECIFIC EDITS
     C*
     C                   SELECT
     C*
     C* IF THIS IS A SURGERY CODE, BODY PART A, O, B1, OR B2 REQUIRED
     C     SCPRCD        WHENGE    '10000'
     C     SCPRCD        ANDLE     '69999'
     C     SCAREA        ANDNE     'A '
     C     SCAREA        ANDNE     'B1'
     C     SCAREA        ANDNE     'B2'
     C     SCAREA        ANDNE     'O '
     C*
     C                   SETON                                        9067
     C*
     C*** IF THIS IS A PHYS THERAPY CODE, BODY PART P1-P6 REQUIRED
     C     SCPRCD        WHENGE    '97010'
     C     SCPRCD        ANDLE     '97799'
     C     SCPRCD        ORGE      '95900'
     C     SCPRCD        ANDLE     '95904'
     C     SCPRCD        OREQ      '95935'
     C*
     C     SCAREA        IFNE      'P1'
     C     SCAREA        ANDNE     'P2'
     C     SCAREA        ANDNE     'P3'
     C     SCAREA        ANDNE     'P4'
     C     SCAREA        ANDNE     'P5'
     C     SCAREA        ANDNE     'P6'
     C*
     C                   SETON                                        9067
     C*
     C                   ENDIF                                                  SCAREA NE P1
     C*
     C                   ENDSL                                                  SCPRCD GE 10000
     C*
     C                   ENDSR
     C*** BEG ADD *** 08/30/00 ****************************************
     C*================================================================
     C     EDITNC        BEGSR
     C*================================================================
     C*          NORTH CAROLINA STATE SPECIFIC EDITS
     C*
     C                   SELECT
     C*
     C* IF THIS IS A SURGERY CODE, BODY PARTS S1-S5 ARE REQUIRED
     C     SCPRCD        WHENGE    '10000'
     C     SCPRCD        ANDLE     '69999'
     C     SCAREA        ANDNE     'S1'
     C     SCAREA        ANDNE     'S2'
     C     SCAREA        ANDNE     'S3'
     C     SCAREA        ANDNE     'S4'
     C     SCAREA        ANDNE     'S5'
     C*
     C                   SETON                                        9067
     C*
     C                   ENDSL                                                  SCPRCD GE 10000
     C*
     C                   ENDSR
     C*** END ADD *** 08/30/00 ****************************************
     C*================================================================
     C     EDITNM        BEGSR
     C*================================================================
     C*          NEW MEXICO STATE SPECIFIC EDITS
     C*
     C                   SELECT
     C*
     C* IF THIS IS A SURGERY CODE, BODY PART M, S OR B REQUIRED
     C     SCPRCD        WHENGE    '10000'
     C     SCPRCD        ANDLE     '69999'
     C     SCAREA        ANDNE     'M '
     C     SCAREA        ANDNE     'B '
     C     SCAREA        ANDNE     'S '
     C*
     C                   SETON                                        9067
     C*
     C                   ENDSL                                                  SCPRCD GE 10000
     C*
     C                   ENDSR
     C*================================================================
     C     EDITNY        BEGSR
     C*================================================================
     C*          NEW YORK STATE SPECIFIC EDITS
     C*
     C                   SELECT
     C*
     C* IF THIS IS A RADIOLOGY CODE, BODY PART C OR R REQUIRED
     C     SCPRCD        WHENGE    '70010'
     C     SCPRCD        ANDLE     '79999'
     C     SCAREA        ANDNE     'C '
     C     SCAREA        ANDNE     'R '
     C*
     C                   SETON                                        9067
     C*
     C                   ENDSL                                                  SCPRCD GE 10000
     C*
     C                   ENDSR
     C*================================================================
     C     EDITOH        BEGSR
     C*================================================================
     C*          OHIO STATE SPECIFIC EDITS
     C*
     C                   SELECT
     C*
     C* IF THIS IS CODE 97010C-97999, BODY PART 1-6, 7A-7F IS REQUIRE  D
     C     SCPRCD        WHENGE    '97010'
     C     SCPRCD        ANDLE     '97999'
     C     SCAREA        ANDNE     '01'
     C     SCAREA        ANDNE     '02'
     C     SCAREA        ANDNE     '03'
     C     SCAREA        ANDNE     '04'
     C     SCAREA        ANDNE     '05'
     C     SCAREA        ANDNE     '06'
     C     SCAREA        ANDNE     '07'
     C     SCAREA        ANDNE     '08'
     C     SCAREA        ANDNE     '09'
     C     SCAREA        ANDNE     '10'
     C*
     C                   SETON                                        9067
     C*
     C                   ENDSL                                                  SCPRCD GE 10000
     C*
     C                   ENDSR
     C*** BEG ADD *** 02/13/03 ****************************************
     C*================================================================
     C     EDITPA        BEGSR
     C*================================================================
     C*          PENNSYLVANIA STATE SPECIFIC EDITS
     C*
     C* VALIDATE BILLED PROCEDURE CODES
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C     SCPRCD        IFGE      'A0001'
     C     SCPRCD        ANDLE     'C9999'
     C     SCPRCD        ORGE      'E0001'
     C     SCPRCD        ANDLE     'Z9999'
     C*
     C     SCMOD1        IFEQ      *BLANKS
     C     SCMOD2        ANDEQ     *BLANKS
     C     SCMOD3        ANDEQ     *BLANKS
     C     SCMOD4        ANDEQ     *BLANKS
     C*
     C                   SETON                                        9004
     C*
     C                   ENDIF                                                  SCMOD1 IFE BLAN
     C*
     C                   ENDIF                                                  SCCPT4 IFGE
     C*
     C                   ENDIF                                                  *IN90 = *OFF
     C*
     C                   ENDSR
     C*** END ADD *** 02/13/03 ****************************************
     C*================================================================
     C     EDITTX        BEGSR
     C*================================================================
     C*          TEXAS STATE SPECIFIC EDITS
     C*
     C*** BEGIN DEL *** 08/27/01 **************************************
     C*** BEG ADD *** 07/17/01 ****************************************
     C**N99***** XXRCFL    IFEQ 'Y'
     C********** EHEDI     ANDEQ'Y'
     C********** PR1AG     OREQ 'Y'
     C********** EHEDI     ANDEQ'Y'
     C**********           EXSR EDICPT
     C**********           SETON                     90
     C**********           GOTO ENDTX
     C**********           ENDIF                           *IN99  = 'I'
     C*** END ADD *** 07/17/01 ****************************************
     C*** END   DEL *** 08/27/01 **************************************
     C*** BEGIN ADD *** 08/27/01 **************************************
     C*
     C* VALIDATE BILLED PROCEDURE CODES
     C*
     C     *IN90         IFEQ      *OFF
     C*** BEG ADD *** 07/21/03 ****************************************
     C*** BEG CHG *** 02/06/04 ****************************************
     C********** SCFDAT    ANDLT1030801
     C     HLFDAT        ANDLT     1030801
     C*** END CHG *** 02/06/04 ****************************************
     C*** END ADD *** 07/21/03 ****************************************
     C*
     C                   MOVEL(P)  SCBLCD        KKPRCD
     C*
     C     KKPRCD        IFEQ      *BLANKS
     C                   MOVEL(P)  SCPRCD        KKPRCD
     C                   ENDIF
     C*
     C                   Z-ADD     SCFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKFDAT
     C*
     C     VCKY1         SETGT     PVC01
     C     VCKY2         READPE    PVC01                                  99
     C*
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9002
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C*** END   ADD *** 08/27/01 **************************************
     C*
     C* BODY AREA IS REQUIRED IF SURGERY CODE
     C*
     C                   SELECT
     C*
     C     SCPRCD        WHENGE    '10040'
     C     SCPRCD        ANDLE     '69979'
     C     SCAREA        ANDNE     'O '
     C     SCAREA        ANDNE     'O1'
     C     SCAREA        ANDNE     'O2'
     C*** BEG ADD *** 07/21/03 ****************************************
     C     SCFDAT        ANDLT     1030801
     C*** END ADD *** 07/21/03 ****************************************
     C*
     C                   SETON                                        9067
     C*
     C     SCPRCD        WHENEQ    '97752'
     C     SCAREA        ANDNE     '01'
     C     SCAREA        ANDNE     '02'
     C     SCAREA        ANDNE     '03'
     C     SCAREA        ANDNE     '04'
     C     SCAREA        ANDNE     '05'
     C     SCAREA        ANDNE     '06'
     C     SCAREA        ANDNE     '7A'
     C     SCAREA        ANDNE     '7B'
     C     SCAREA        ANDNE     '7C'
     C     SCAREA        ANDNE     '7D'
     C     SCAREA        ANDNE     '7E'
     C     SCAREA        ANDNE     '7F'
     C     SCAREA        ANDNE     '7G'
     C*
     C                   SETON                                        9067
     C*
     C                   ENDSL                                                  SCPRCD GE 10040
     C*** BEGIN ADD *** 08/27/01 **************************************
     C                   ENDIF
     C*** END   ADD *** 08/27/01 **************************************
     C*
     C*** BEGIN DEL *** 08/27/01 **************************************
     C*** BEG ADD *** 07/17/01 ****************************************
     C********** ENDTX     TAG
     C*** END ADD *** 07/17/01 ****************************************
     C*** END   DEL *** 08/27/01 **************************************
     C*** BEG ADD *** 09/28/04 ****************************************
     C     HLFDAT        IFGE      1040901
     C     SCPRCD        ANDGE     '10000'
     C     SCPRCD        ANDLE     '69999'
     C     SCMOD1        IFEQ      'SG'
     C     SCMOD2        OREQ      'SG'
     C     SCMOD3        OREQ      'SG'
     C     SCMOD4        OREQ      'SG'
     C     SCCNTY        IFEQ      *BLANKS
     C                   SETON                                        9012
     C                   ENDIF
     C                   MOVEL     'TX'          XXCNTY            5
     C                   MOVE      SCCNTY        XXCNTY
     C                   MOVEL(P)  XXCNTY        KKKEY
     C     CCKY          CHAIN     PCC01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9012
     C                   ELSE
     C     P1KY          CHAIN     PP101                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  SCCNTY        YYCNTY
     C                   UPDATE    PP101
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*** END ADD *** 09/28/04 ****************************************
     C*
     C                   ENDSR
     C*** END ADD *** 12/03/99 ****************************************
     C*================================================================
     C     EDITWV        BEGSR
     C*================================================================
     C*          WEST VIRGINIA STATE SPECIFIC EDITS
     C*
     C     SCFDAT        IFGE      1010501
     C*
     C     SCPRCD        IFEQ      'V5030'
     C     SCPRCD        OREQ      'V5040'
     C     SCPRCD        OREQ      'V5050'
     C     SCPRCD        OREQ      'V5055'
     C     SCPRCD        OREQ      'V5060'
     C     SCPRCD        OREQ      'V5065'
     C     SCPRCD        OREQ      'V5080'
     C     SCPRCD        OREQ      'V5170'
     C     SCPRCD        OREQ      'V5260'
     C     SCPRCD        OREQ      'V5265'
     C     SCPRCD        OREQ      'V5270'
     C     SCPRCD        OREQ      'V5336'
     C*
     C     SCMOD1        IFNE      'RT'
     C     SCMOD1        ANDNE     'LT'
     C*
     C     SCMOD2        ANDNE     'RT'
     C     SCMOD2        ANDNE     'LT'
     C*
     C     SCMOD3        ANDNE     'RT'
     C     SCMOD3        ANDNE     'LT'
     C*
     C     SCMOD4        ANDNE     'RT'
     C     SCMOD4        ANDNE     'LT'
     C*
     C                   SETON                                        9021
     C*
     C                   ENDIF                                                  SCMOD1 IFNE
     C*
     C                   ENDIF                                                  SCPRCD IFEQ
     C*
     C                   ENDIF                                                  SCFDAT IFGE
     C*
     C                   ENDSR
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
     C*** BEG ADD *** 01/18/99 ****************************************
     C     EOBTYP        IFEQ      'V'
     C     CSOVRD        ANDNE     'Y'
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C*
     C                   SETON                                        9096
     C                   GOTO      CK1END
     C*
     C                   ENDIF                                                  EOBTYP=V    ON
     C*** END ADD *** 01/18/99 ****************************************
     C*
     C* Validate Date of Service
     C*
     C     SCFDAT        IFEQ      *ZEROS
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C                   SETON                                        9073
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCFDAT IFEQ ZEROS
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
     C* DATE OF SERVICE MUST BE LESS THAN ACCIDENT DATE
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
     C                   SETON                                        9028
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLFDAT GT HLSYDT
     C*** BEG DEL *** 11/11/05 ****************************************
     C*** BEG ADD *** 04/11/00 ****************************************
     C*  Give an error window if the date is a future date and the
     C*  data submission flag is 'Y'.
     C*
     C******************** ELSE                            XXFSDF='Y'
     C********** EHEDI     IFEQ 'Y'
     C********** HLFDAT    ANDGTHLSYDT
     C*** BEGIN ADD *** 05/31/01 **************************************
     C********** *IN46     ANDEQ*OFF
     C*** END   ADD *** 05/31/01 **************************************
     C*** BEG ADD *** 04/15/04 ****************************************
     C********** EHEDI     OREQ 'E'
     C********** HLFDAT    ANDGTHLSYDT
     C********** *IN46     ANDEQ*OFF
     C*** END ADD *** 04/15/04 ****************************************
     C******************** EXSR EDIERR
     C******************** SETON                     90
     C******************** GOTO CK1END
     C******************** ENDIF
     C*** END ADD *** 04/11/00 ****************************************
     C*** END DEL *** 11/11/05 ****************************************
     C*
     C                   ENDIF                                                  XXFSDF = *BLANKS
     C*** BEGIN DEL *** 08/15/00 **************************************
     C**********           ENDIF                           *IN99 = *OFF
     C*** END   DEL *** 08/15/00 **************************************
     C*** BEG ADD *** 04/11/00 ****************************************
     C*
     C* Date of Service must be less than Carr Rcvd Dt
     C*     (If recon = 0)  FAEOP
     C*
     C* This is all conditioned on weather or not the EDI flag is on
     C*
     C*** BEGIN DEL *** 07/05/02 **************************************
     C********** EHEDI     IFEQ 'Y'
     C*** END   DEL *** 07/05/02 **************************************
     C     EORCN         IFEQ      0
     C     HLFDAT        ANDGT     EOCRDT
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C                   EXSR      CARERR
     C*** BEGIN ADD *** 11/11/05 **************************************
     C     ##SDTS        IFNE      'Y'
     C*** END   ADD *** 11/11/05 **************************************
     C*** BEGIN ADD *** 07/05/02 **************************************
     C     EHEDI         IFEQ      'Y'
     C*** END   ADD *** 07/05/02 **************************************
     C*** BEG ADD *** 04/15/04 ****************************************
     C     EHEDI         OREQ      'E'
     C*** END ADD *** 04/15/04 ****************************************
     C                   SETON                                        90
     C                   GOTO      CK1END
     C*** BEGIN ADD *** 07/05/02 **************************************
     C                   ENDIF
     C*** END   ADD *** 07/05/02 **************************************
     C*** BEGIN ADD *** 11/11/05 **************************************
     C                   ENDIF
     C*** END   ADD *** 11/11/05 **************************************
     C                   ENDIF                                                  EORCN=0
     C*
     C*     (If recon > 0)  FAESP
     C*
     C     EORCN         IFGT      0
     C                   MOVEL     'R'           KKBLST
     C     ESKY1         SETGT     PES01
     C     ESKY2         READPE    PES01                                  60
     C     *IN60         IFEQ      *OFF
     C*** BEG CHG *** 10/13/00 ****************************************
     C********** HLFDAT    ANDGTESRRDT
     C     HLFDAT        IFGT      ESRRDT
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C*** END CHG *** 10/13/00 ****************************************
     C                   EXSR      CARERR
     C*** BEGIN ADD *** 11/11/05 **************************************
     C     ##SDTS        IFNE      'Y'
     C*** END   ADD *** 11/11/05 **************************************
     C*** BEGIN ADD *** 07/05/02 **************************************
     C     EHEDI         IFEQ      'Y'
     C*** END   ADD *** 07/05/02 **************************************
     C*** BEG ADD *** 04/15/04 ****************************************
     C     EHEDI         OREQ      'E'
     C*** END ADD *** 04/15/04 ****************************************
     C                   SETON                                        90
     C                   GOTO      CK1END
     C*** BEGIN ADD *** 07/05/02 **************************************
     C                   ENDIF
     C*** END   ADD *** 07/05/02 **************************************
     C*** BEGIN ADD *** 11/11/05 **************************************
     C                   ENDIF
     C*** END   ADD *** 11/11/05 **************************************
     C*** BEG ADD *** 10/13/00 ****************************************
     C                   ENDIF                                                  HLFDAT ANDGT
     C*** END ADD *** 10/13/00 ****************************************
     C                   ELSE                                                   *IN60
     C     EORCN         SUB       1             KKRCN
     C     ESKY1         SETGT     PES01
     C     ESKY2         READPE    PES01                                  60
     C     *IN60         IFEQ      *OFF
     C*** BEG CHG *** 10/13/00 ****************************************
     C********** HLFDAT    ANDGEESRRDT
     C     HLFDAT        ANDGT     ESRRDT
     C*** END CHG *** 10/13/00 ****************************************
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C                   EXSR      CARERR
     C*** BEGIN ADD *** 11/11/05 **************************************
     C     ##SDTS        IFNE      'Y'
     C*** END   ADD *** 11/11/05 **************************************
     C*** BEGIN ADD *** 07/05/02 **************************************
     C     EHEDI         IFEQ      'Y'
     C*** END   ADD *** 07/05/02 **************************************
     C*** BEG ADD *** 04/15/04 ****************************************
     C     EHEDI         OREQ      'E'
     C*** END ADD *** 04/15/04 ****************************************
     C                   SETON                                        90
     C                   GOTO      CK1END
     C*** BEGIN ADD *** 07/05/02 **************************************
     C                   ENDIF
     C*** END   ADD *** 07/05/02 **************************************
     C*** BEGIN ADD *** 11/11/05 **************************************
     C                   ENDIF
     C*** END   ADD *** 11/11/05 **************************************
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  EORCN>0
     C*** BEGIN DEL *** 07/05/02 **************************************
     C**********           ENDIF                           EHEDI=Y
     C*** END   DEL *** 07/05/02 **************************************
     C*** END ADD *** 04/11/00 ****************************************
     C*** BEGIN ADD *** 08/07/02 **************************************
     C                   MOVEL     *BLANKS       SCZIPA
     C                   MOVEL     *BLANKS       SCZIPB
     C                   MOVEL     *ZEROS        HLEFDT
     C*
     C* Let's get the zip area forn this line.
     C*
     C*** BEGIN ADD *** 01/07/03 **************************************
     C                   MOVEL(P)  EOFLIC        KKFRZP
     C     KKFRZP        IFEQ      *BLANKS
     C     KKFRZP        OREQ      '000'
     C*** END   ADD *** 01/07/03 **************************************
     C     P1KY          CHAIN(N)  PP101                              99
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
     C                   SETON                                        9076
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
     C* CHARGES ARE REQUIRED
     C*
     C*** BEG ADD *** 04/01/99 ****************************************
     C*
     C* IF ADJUDICATION CODE IS OVERRIDE TYPE, ALLOW ZERO CHARGE AMOUNT
     C*
     C     CSOVRD        IFNE      'Y'
     C*** END ADD *** 04/01/99 ****************************************
     C     SCCHRG        IFEQ      *ZEROS
     C                   SETON                                        9089
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCCHRG IFEQ ZEROS
     C*** BEG ADD *** 04/01/99 ****************************************
     C                   ENDIF                                                  CSOBRD NE Y
     C*** END ADD *** 04/01/99 ****************************************
     C*
     C* PROCEDURE CODE IS REQUIRED
     C*
     C*** BEGIN CHG *** 03/16/09 **************************************
     C**********           MOVE ' '       XXRCFL  1
     C                   MOVE      'C'           XXRCFL            1
     C*** END   CHG *** 03/16/09 **************************************
     C*
     C     SCPRCD        IFEQ      *BLANKS
     C*** BEGIN ADD *** 05/31/01 **************************************
     C*****     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C     SCNDC         ANDEQ     *blanks
     C                   SETON                                        9083
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCPRCD IFEQ BLANKS
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
     C                   MOVE      'P'           KKATYP
     C                   Z-ADD     KKFDAT        KKEFDT
     C     RUKY1         SETGT     PRU01
     C     RUKY2         READPE    PRU01                                  99
     C     *IN99         IFEQ      *ON
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C                   SETON                                        9030
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C*
     C* VALIDATE CPT4 CODE
     C*
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C*** BEGIN ADD *** 01/16/01 **************************************
     C     SCAST         ANDNE     'UC'
     C*** END   ADD *** 01/16/01 **************************************
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C*
     C*** BEG ADD *** 08/08/00 ****************************************
     C                   MOVE      PR1A2         SV1A2
     C*** END ADD *** 08/08/00 ****************************************
     C*
     C* GO TO REASONABLE AND CUTORMARY FILE IF CPT4 EFFECTIVE DATE IS
     C* LESS THAN FEE SCHEDULE EFFECTIVE DATE OR CODE IS INACTIVE
     C*
     C     PREFDT        IFLT      RUEFDT
     C                   SETON                                        99
     C                   ENDIF                                                  PREFDT < RUEFDT
     C*
     C  N99PRSTAT        IFEQ      'I'
     C*** BEG CHG *** 12/08/00 ****************************************
     C******************** SETON                     9057
     C******************** GOTO CK1END
     C                   SETON                                        99
     C*** END CHG *** 12/08/00 ****************************************
     C                   ENDIF                                                  *IN99  = 'I'
     C*
     C*** BEG ADD *** 08/02/00 ****************************************
     C  N99PR1AG         IFEQ      'Y'
     C     EHEDI         ANDEQ     'Y'
     C*** BEG ADD *** 04/15/04 ****************************************
     C     EHEDI         OREQ      'E'
     C     PR1AG         ANDEQ     'Y'
     C*** END ADD *** 04/15/04 ****************************************
     C                   EXSR      EDICPT
     C                   SETON                                        90
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99  = 'I'
     C*** END ADD *** 08/02/00 ****************************************
     C*
     C     *IN99         IFEQ      *OFF
     C*** Beg Add *** 04/08/13 ****************************************
     C     CARCFL        ANDNE     'Y'
     C*** End Add *** 04/08/13 ****************************************
     C                   MOVEL(P)  PRDSC1        SCCDDS
     C                   MOVE      'C'           XXRCFL
     C                   ENDIF                                                  *IN99 = *OFF
     C*** BEGIN ADD *** 03/11/10 **************************************
     C*
     C* VALLIDATE NDC CODE AND COMPOUNDED DRUGS
     C*
     C*****              MOVE      '0'           XFL               1
     C*****EOREVI        IFGE      'E'
     C*****SCNDC         IFNE      *BLANKS
     C*****SCPRCD        ANDNE     'DSPNS'
     C*****SCPRCD        ANDNE     'COMPD'
     C*****              SETON                                        9026
     C*****              MOVE      '1'           XFL               1
     C*****              GOTO      CK1END
     C*****              ENDIF                                                   SCNDC
     C*****              ENDIF                                                   REVI GE E
     C*** END   ADD *** 03/11/10 **************************************
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C     *IN99         IFEQ      *ON
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C*** BEGIN ADD *** 01/16/01 **************************************
     C     SCAST         OREQ      'UC'
     C*** END   ADD *** 01/16/01 **************************************
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C*
     C*** BEG ADD *** 12/08/00 ****************************************
     C* If State flag STBS02='N' then don't check R&C file
     C     ##RC          IFEQ      'N'
     C                   SETON                                        9084
     C                   GOTO      CK1END
     C*
     C                   ELSE
     C*** END ADD *** 12/08/00 ****************************************
     C*
      /Free
         If SCPRCD <> *Blanks ;
      /End-Free
     C                   MOVEL     SCZIPA        KKZIPA
     C                   MOVEL(P)  SCPRCD        KKCPT4
     C                   MOVEL     KKFDAT        KKEFDT
     C     RCKY1         SETGT     PRC01
     C     RCKY2         READPE    PRC01                                  99
     C*
     C     *IN99         IFEQ      *ON
     C*** BEG ADD *** 12/20/01 ****************************************
     C*
     C* VALIDATE CPT4 CODE AGAINST UC
     C*
     C     SCAST         IFEQ      'UC'
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  98
     C*
     C     *IN98         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        SCCDDS
     C                   MOVE      'C'           XXRCFL
     C*** BEG ADD *** 06/03/02 ****************************************
     C                   MOVE      PR1A2         SV1A2
     C*** END ADD *** 06/03/02 ****************************************
     C*
     C                   ELSE
     C                   SETON                                        9084
     C                   GOTO      CK1END
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*** END ADD *** 12/20/01 ****************************************
     C                   SETON                                        9084
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C*
     C*** BEG ADD *** 12/20/01 ****************************************
     C                   ENDIF                                                  SCAST=UC
     C*** END ADD *** 12/20/01 ****************************************
     C*
     C     *IN99         IFEQ      *OFF
     C     RCSTAT        ANDEQ     'I'
     C*** BEG ADD *** 12/20/01 ****************************************
     C*
     C* VALIDATE CPT4 CODE AGAINST UC
     C*
     C     SCAST         IFEQ      'UC'
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  98
     C*
     C     *IN98         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        SCCDDS
     C                   MOVE      'C'           XXRCFL
     C*** BEG ADD *** 06/03/02 ****************************************
     C                   MOVE      PR1A2         SV1A2
     C*** END ADD *** 06/03/02 ****************************************
     C*
     C                   ELSE
     C                   SETON                                        9057
     C                   GOTO      CK1END
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*** END ADD *** 12/20/01 ****************************************
     C                   SETON                                        9057
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C*
     C*** BEG ADD *** 12/20/01 ****************************************
     C                   ENDIF                                                  SCAST=UC
     C*** END ADD *** 12/20/01 ****************************************
      /Free
         Endif ;
      /End-Free
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  RCDSC1        SCCDDS
     C                   MOVE      'Y'           XXRCFL
     C*** BEG ADD *** 06/03/02 ****************************************
     C                   MOVE      RC1A2         SV1A2
     C*** END ADD *** 06/03/02 ****************************************
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C*** BEG ADD *** 12/08/00 ****************************************
     C                   ENDIF                                                  ##RC IFEQ ' '
     C*** END ADD *** 12/08/00 ****************************************
     C*
     C                   ENDIF
     C*
     C* VERIFY THAT THE PROCECURE CODE IS EFFECTIVE FOR THE FEE SCHEDULE
     C*
      /Free
         If SCPRCD <> *Blanks ;
      /End-Free
     C*
     C     XXRCFL        IFEQ      'C'
     C     PREFDT        IFLT      RUEFDT
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C                   SETON                                        9042
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
     C*
      /Free
         Endif ;
      /End-Free
     C*
     C*** BEG ADD *** 02/18/00 ****************************************
     C*
     C* VALIDATE CPT4 BILLED CODE WITHOUT USING EFFECTIVE DATE
     C*
      /Free
         If SCPRCD <> *Blanks ;
      /End-Free
     C*
     C     SCBLCD        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  SCBLCD        KKPRCD
     C*
     C*** BEGIN CHG *** 04/26/04 **************************************
     C********** PRKY1     CHAINPPR01                99
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  99
     C*** END   CHG *** 04/26/04 **************************************
     C*
     C  N99PRSTAT        IFEQ      'I'
     C*** BEG CHG *** 12/08/00 ****************************************
     C******************** SETON                     9057
     C******************** GOTO CK1END
     C                   SETON                                        99
     C*** END CHG *** 12/08/00 ****************************************
     C*** BEGIN ADD *** 04/26/04 **************************************
     C                   ELSE
     C                   MOVEL     PRDSC1        XXBPRD           25
     C*** END   ADD *** 04/26/04 **************************************
     C                   ENDIF                                                  *IN99  = 'I'
     C*
     C     *IN99         IFEQ      *ON
     C*** BEG ADD *** 12/08/00 ****************************************
     C* If State flag STBS02='N' then don't check R&C file
     C     ##RC          IFEQ      'N'
     C*** BEG CHG *** 05/02/01 ****************************************
     C******************** SETON                     9084
     C                   SETON                                        9016
     C*** END CHG *** 05/02/01 ****************************************
     C                   GOTO      CK1END
     C*
     C                   ELSE
     C*** END ADD *** 12/08/00 ****************************************
     C*
     C                   MOVEL     SCZIPA        KKZIPA
     C*** BEG CHG *** 04/18/01 ****************************************
     C******************** MOVELSCPRCD    KKCPT4    P
     C                   MOVEL(P)  SCBLCD        KKCPT4
     C*** END CHG *** 04/18/01 ****************************************
     C                   MOVEL     KKFDAT        KKEFDT
     C*
     C     RCKY2         CHAIN     PRC01                              99
     C*
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9016
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C*
     C     *IN99         IFEQ      *OFF
     C     RCSTAT        ANDEQ     'I'
     C                   SETON                                        9015
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C*
     C*** BEG ADD *** 12/08/00 ****************************************
     C                   ENDIF                                                  ##RC = 'N'
     C*** END ADD *** 12/08/00 ****************************************
     C*
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C*
     C                   ENDIF                                                  SCBLCD IFEQ BLANKS
      /Free
         Endif ;
      /End-Free
     C*** END ADD *** 02/18/00 ****************************************
     C********** XXRCFL    IFEQ 'Y'
     C********** RCEFDT    IFLT RUEFDT
     C**********           SETON                     9042
     C**********           GOTO CK1END
     C**********           ENDIF
     C**********           ENDIF
     C*** BEG ADD *** 06/15/99 ****************************************
     C*** BEG DEL *** 08/18/99 ****************************************
     C*** VALIDATE BILLED PROCEDURE CODE
     C***********
     C***********          MOVELSCBLCD    KKPRCD    P
     C***********          Z-ADDSCFDAT    SRDTI
     C***********          MOVE *BLANKS   SRDTO
     C***********          CALL 'SRCVTDAT'DTCYMD
     C***********          MOVELSRDTO     KKFDAT    P
     C***********
     C**VERIFY*THAT THERE IS A FEE SCHEDULE ON THE SYSTEM FOR THE
     C**SERVICE*DATE
     C***********
     C***********          MOVELSCAST     KKST
     C***********          MOVE 'P'       KKATYP
     C***********          Z-ADDKKFDAT    KKEFDT
     C***********RUKY1     SETGTPRU01
     C***********RUKY2     REDPEPRU01                    99
     C************IN99     IFEQ *ON
     C***********          SETON                     9030
     C***********          GOTO CK1END
     C***********          ENDIF                           *IN99 = *ON
     C***********
     C**VALIDATE*CPT4 CODE
     C***********
     C***********SCBLCD    IFNE *BLANKS
     C***********
     C***********PRKY      SETGTPPR01
     C***********PRKY1     REDPEPPR01                    99
     C***********
     C************IN99     IFEQ *OFF
     C***********
     C**GO*TO*REASONABLE AND CUTORMARY FILE IF CPT4 EFFECTIVE DATE IS
     C**LESS*THAN FEE SCHEDULE EFFECTIVE DATE
     C***********
     C***********PREFDT    IFLT RUEFDT
     C***********          SETON                     99
     C***********          ENDIF                           PREFDT < RUEFDT
     C***********
     C**N99******PRSTAT    IFEQ 'I'
     C***********          SETON                     9015
     C***********          GOTO CK1END
     C***********          ENDIF                           *IN99  = 'I'
     C***********
     C************IN99     IFEQ *OFF
     C***********          MOVELPRDSC1    SCCDDS    P
     C***********          MOVE 'C'       XXRCFL
     C***********          ENDIF                           *IN99 = *OFF
     C***********
     C***********          ENDIF                           *IN99 = *OFF
     C***********
     C************IN99     IFEQ *ON
     C***********
     C***********          MOVELSCZIPA    KKZIPA
     C***********          MOVELSCBLCD    KKCPT4    P
     C***********          MOVELKKFDAT    KKEFDT
     C***********RCKY1     SETGTPRC01
     C***********RCKY2     REDPEPRC01                    99
     C***********
     C************IN99     IFEQ *ON
     C***********          SETON                     9016
     C***********          GOTO CK1END
     C***********          ENDIF                           *IN99 = *ON
     C***********
     C************IN99     IFEQ *OFF
     C***********RCSTAT    ANDEQ'I'
     C***********          SETON                     9015
     C***********          GOTO CK1END
     C***********          ENDIF                           *IN99 IFEQ *ON
     C***********
     C************IN99     IFEQ *OFF
     C***********          MOVELRCDSC1    SCCDDS    P
     C***********          MOVE 'Y'       XXRCFL
     C***********          ENDIF                           *IN99 = *OFF
     C***********
     C***********          ENDIF
     C***********
     C***********          ENDIF
     C***********
     C**VERIFY*THAT THE PROCECURE CODE IS EFFECTIVE FOR THE FEE SCHEDULE
     C***********
     C***********XXRCFL    IFEQ 'C'
     C***********PREFDT    IFLT RUEFDT
     C***********          SETON                     9042
     C***********          GOTO CK1END
     C***********          ENDIF
     C***********          ENDIF
     C***********
     C*** BEG DEL *** 08/18/99 ****************************************
     C*** END ADD *** 06/15/99 ****************************************
     C*
     C* VALIDATE THE PROCEDURE CODE MODIFIERS IF ENTETED (1-4)
     C*
     C     SCMOD1        IFNE      *BLANKS
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C                   MOVEL(P)  SCMOD1        KKKEY
     C     TBKY          CHAIN     PPM01                              99
     C*** BEG ADD *** 08/26/99 ****************************************
     C     *IN99         IFEQ      *ON
     C     PMSTAT        OREQ      'I'
     C*** END ADD *** 08/26/99 ****************************************
     C                   SETON                                        9085
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C                   ENDIF                                                  SCMOD1 IFNE *BLANKS
     C*
     C     SCMOD2        IFNE      *BLANKS
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C                   MOVEL(P)  SCMOD2        KKKEY
     C     TBKY          CHAIN     PPM01                              99
     C     *IN99         IFEQ      *ON
     C*** BEG ADD *** 08/26/99 ****************************************
     C     PMSTAT        OREQ      'I'
     C*** END ADD *** 08/26/99 ****************************************
     C                   SETON                                        9086
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C                   ENDIF                                                  SCMOD2 IFNE *BLANKS
     C*
     C     SCMOD3        IFNE      *BLANKS
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C                   MOVEL(P)  SCMOD3        KKKEY
     C     TBKY          CHAIN     PPM01                              99
     C     *IN99         IFEQ      *ON
     C*** BEG ADD *** 08/26/99 ****************************************
     C     PMSTAT        OREQ      'I'
     C*** END ADD *** 08/26/99 ****************************************
     C                   SETON                                        9087
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C                   ENDIF                                                  SCMOD3 IFNE *BLANKS
     C*
     C     SCMOD4        IFNE      *BLANKS
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C                   MOVEL(P)  SCMOD4        KKKEY
     C     TBKY          CHAIN     PPM01                              99
     C     *IN99         IFEQ      *ON
     C*** BEG ADD *** 08/26/99 ****************************************
     C     PMSTAT        OREQ      'I'
     C*** END ADD *** 08/26/99 ****************************************
     C                   SETON                                        9088
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C                   ENDIF                                                  SCMOD4 IFNE *BLANKS
     C*
     C* ONE DIAGNOSIS CODE MUST BE MARKED WITH AN 'X'
     C*
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
     C*
     C* VALIDATE DIAGNOSIS CODE CROSS REFERENCE FUND
     C*
     C     SCDXR1        IFNE      'X'
     C     SCDXR1        ANDNE     *BLANKS
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C                   SETON                                        9079
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
     C                   SETON                                        9080
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C     SCDXR3        IFNE      'X'
     C     SCDXR3        ANDNE     *BLANKS
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C                   SETON                                        9081
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C     SCDXR4        IFNE      'X'
     C     SCDXR4        ANDNE     *BLANKS
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C                   SETON                                        9082
     C                   GOTO      CK1END
     C                   ENDIF
     C*** BEGIN ADD *** 10/31/05 **************************************
     C*
     C* VALIDATE NATIONAL DRUG CODE (NDC)
     C*
     C     SCNDC         IFEQ      *BLANKS
     C                   MOVEL     *BLANKS       SCNDCD
     C                   ENDIF
     C*** BEGIN ADD *** 03/11/10 **************************************
     C     EOREVI        IFGE      'E'
     C     SCPRCD        IFEQ      'COMPD'
     C     SCNDC         ANDNE     *BLANKS
     C                   SETON                                        9098
     C                   GOTO      CK1END
     C                   ENDIF                                                   NDC
     C                   ENDIF                                                   PROCEDURE
     C*** END   ADD *** 03/11/10 **************************************
     C*
     C     SCPRCD        IFEQ      '96370'
     C     SCNDC         ANDEQ     *BLANKS
     C*** BEGIN ADD *** 06/22/06 **************************************
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
     C                   MOVE      'N'           FNDEB4            1
     C*
     C     DEKY          CHAIN(N)  PDE01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*** Beg Chg *** 07/18/23 **************************************
     C****               MOVEL(P)  DEEOB         KKKEY
     C                   MOVEL(P)  DEEOBCD       KKKEY
     C*** End Chg *** 07/18/23 **************************************
     C     TBKY          CHAIN     PEB01                              99
     C     *IN99         IFEQ      *OFF
     C     DESTAT        ANDEQ     'A'
     C*** BEG CHG ***** 05/30/07 **************************************
     C*
     C********** EBXREF    ANDEQ'S15'
     C     EBXREF        ANDEQ     'D62'
     C*
     C*** END CHG ***** 05/30/07 **************************************
     C*
     C                   MOVE      'Y'           FNDEB4
     C                   LEAVE
     C*
     C                   ENDIF
     C*
     C     DEKY          READE(N)  PDE01                                  99
     C*
     C                   ENDDO
     C*
     C     FNDEB4        IFEQ      'N'
     C*
     C                   EXFMT     EB4WDW
     C*
     C     *INKL         IFEQ      *ON
     C*** END   ADD *** 06/22/06 **************************************
     C                   SETON                                        9006
     C                   GOTO      CK1END
     C*** BEGIN ADD *** 06/22/06 **************************************
     C                   ELSE                                                   *INKL=*ON
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
     C                   SETON                                        90
     C                   GOTO      CK1END
     C                   ENDIF                                                  *INLK=*ON
     C                   ENDIF                                                  FNDEB4    IFEQ 'N'
     C*** END   ADD *** 06/22/06 **************************************
     C                   ENDIF                                                  SCPRCD IFEQ 96370
     C*
     C     SCNDC         IFNE      *BLANKS
     C     *IN46         ANDEQ     *OFF
     C*
     C                   MOVEL(P)  SCNDC         KKNDC
     C     RXKY          CHAIN     PRX01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9001
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C*
     C     RXSTAT        IFEQ      'I'
     C                   SETON                                        9001
     C                   GOTO      CK1END
     C                   ENDIF                                                  RXSTAT = 'I'
     C                   ENDIF                                                  SCNDC IFNE BLANKS
     C*
     C*** END   ADD *** 10/31/05 **************************************
     C*
     C* VALIDATE TYPE OF SERVICE CODE
     C*
     C     SCTOS         IFNE      *BLANKS
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C                   MOVEL(P)  EOAST         HLKEY             2
     C                   MOVEL(P)  SCTOS         HDKEY             3
     C*                    MOVE HDKEY     HLKEY
     C                   MOVEL(P)  TMTOS         KKKEY
     C     TBKY          CHAIN     PTS02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9078
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ ON
     C                   ENDIF                                                  SCTOS IFNE BLANKS
     C*
     C* TYPE CODE #1 REQUIRED IF CORRESPONDING AMOUNT IS ENTERED
     C*
     C     SCOTHR        IFNE      *ZEROS
     C     SCOT1C        ANDEQ     *BLANKS
     C                   SETON                                        9061
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
     C                   SETON                                        9063
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C                   ENDIF                                                  SCOT1C IFNE *BLANKS
     C*
     C* TYPE CODE #2 REQUIRED IF CORRESPONDING AMOUNT IS ENTERED
     C*
     C     SCOTH2        IFNE      *ZEROS
     C     SCOT2C        ANDEQ     *BLANKS
     C                   SETON                                        9062
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
     C                   SETON                                        9064
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C                   ENDIF                                                  SCOT2C IFNE *BLANKS
       //*** Beg add ***** 07/18/25 ************************************
       If isModEnabled('NCCI Med Data Call');
          // %check return non zero if non numeric value including blank
          invalidPos = %check('0123456789': %trim(scnpir));
          If invalidPos <> 0 or scnpir = *blanks;
             msnpir = 'MC50007';
             ernpir = *on;
             *in90 = *on;
             leavesr;
          Endif;
       Endif;
       //*** End add ***** 07/18/25 ************************************
     C*** BEG DEL *** 12/03/99 ****************************************
     C**********
     C**BODY*AREA IS REQUIRED IF SURGERY CODE
     C**********
     C********** SCAST     IFEQ 'TX'
     C********** SCPRCD    ANDGE'10040'
     C********** SCPRCD    ANDLE'69979'
     C********** SCAREA    ANDNE'O '
     C********** SCAREA    ANDNE'O1'
     C********** SCAREA    ANDNE'O2'
     C**********           SETON                     9067
     C**********           GOTO CK1END
     C**********           ENDIF                           SCPRCD GE 10040
     C*** END DEL *** 12/03/99 ****************************************
     C*
     C* VALIDATE BODY PART CODE IF ENTERED
     C*
     C     SCAREA        IFNE      *BLANKS
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C                   MOVEL(P)  SCAREA        KKKEY
     C     TBKY          CHAIN     PPA01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9040
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C                   ENDIF                                                  SCAREA IFNE *BLANKS
     C*** BEG ADD *** 12/03/99 ****************************************
     C* Now perform all state specific edits before cross footing
     C                   SELECT
     C*
     C*
     C     SCAST         WHENEQ    'AL'
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C*
     C                   EXSR      EDITAL
     C   90              GOTO      CK1END
     C*
     C     SCAST         WHENEQ    'FL'
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C*
     C                   EXSR      EDITFL
     C   90              GOTO      CK1END
     C*
     C     SCAST         WHENEQ    'LA'
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C*
     C                   EXSR      EDITLA
     C   90              GOTO      CK1END
     C*
     C     SCAST         WHENEQ    'NE'
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C*
     C                   EXSR      EDITNE
     C   90              GOTO      CK1END
     C*
     C*** BEG ADD *** 08/30/00 ****************************************
     C     SCAST         WHENEQ    'NC'
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C*
     C                   EXSR      EDITNC
     C   90              GOTO      CK1END
     C*
     C*** END ADD *** 08/30/00 ****************************************
     C     SCAST         WHENEQ    'NM'
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C*
     C                   EXSR      EDITNM
     C   90              GOTO      CK1END
     C*
     C     SCAST         WHENEQ    'NY'
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C*
     C                   EXSR      EDITNY
     C   90              GOTO      CK1END
     C*
     C     SCAST         WHENEQ    'OH'
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C*
     C                   EXSR      EDITOH
     C   90              GOTO      CK1END
     C*
     C     SCAST         WHENEQ    'TX'
     C*** BEGIN ADD *** 05/31/01 **************************************
     C     *IN46         ANDEQ     *OFF
     C*** END   ADD *** 05/31/01 **************************************
     C*
     C                   EXSR      EDITTX
     C   90              GOTO      CK1END
     C*
     C*** BEG ADD *** 08/30/01 ****************************************
     C     SCAST         WHENEQ    'WV'
     C     *IN46         ANDEQ     *OFF
     C*
     C                   EXSR      EDITWV
     C   90              GOTO      CK1END
     C*** END ADD *** 08/30/01 ****************************************
     C*
     C*** BEG ADD *** 02/13/03 ****************************************
     C     SCAST         WHENEQ    'PA'
     C     *IN46         ANDEQ     *OFF
     C*
     C                   EXSR      EDITPA
     C   90              GOTO      CK1END
     C*** END ADD *** 02/13/03 ****************************************
     C*
     C                   ENDSL                                                              *BLANKS
     C*** END ADD *** 12/03/99 ****************************************
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
     C*** BEG ADD *** 04/01/99 ****************************************
     C     SCCHRG        ANDNE     *ZEROS
     C*** END ADD *** 04/01/99 ****************************************
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
     C*** BEG ADD *** 04/01/99 ****************************************
     C     SCCHRG        IFNE      *ZEROS
     C*** END ADD *** 04/01/99 ****************************************
     C*** BEGIN ADD *** 01/11/00 **************************************
     C     SCCSCD        ANDNE     '300'
     C*** END   ADD *** 01/11/00 **************************************
     C     SCCHRG        SUB       SCMRA         SCPAID
     C                   SUB       SCOTHR        SCPAID
     C                   SUB       SCOTH2        SCPAID
     C                   SUB       SCNTWK        SCPAID
     C*** BEG ADD *** 04/01/99 ****************************************
     C                   ENDIF                                                  SCCHRG NE *ZEROS
     C*** END ADD *** 04/01/99 ****************************************
     C*** BEG ADD *** 11/11/02 ****************************************
     C     SCCHRG        IFNE      *ZEROS
     C     SCCSCD        ANDNE     '300'
     C     EOCRNE        ANDEQ     'Y'
     C     CSOVRD        ANDNE     'Y'
     C     SCMRA         ANDEQ     *ZEROS
     C     SCOTHR        ANDEQ     *ZEROS
     C     SCOTH2        ANDEQ     *ZEROS
     C     SCNTWK        ANDEQ     *ZEROS
     C                   Z-ADD     0             SCPAID
     C                   ENDIF                                                  SCCHRG NE *ZEROS
     C*** END ADD *** 11/11/02 ****************************************
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
     C*** END ADD *** 04/01/99 ****************************************
     C*
     C* IF ADJUDICATION CODE IS AN OVERRIDE TYPE AND THE OVERRIDE
     C* PRECENTAGE IS GREATER THAN ZEROS, CLEAR ALL REDUCTIONS AMOUNT
     C* FIELDS AND PAID AMOUNT FIELD.  THE ADJUDICATION PROGRAM
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
     C* 03/30/99 -  CALL EOB CODES/NOTES PROGRAM NOW INSTEAD OF GIVING
     C*             AN ERROR
     C*
     C                   MOVE      *BLANKS       XXEOB
     C     *IN99         IFEQ      *OFF
     C     CSOVRD        ANDEQ     'Y'
     C*
     C*** BEG ADD *** 10/31/01 ****************************************
     C     CSOVRP        ANDLT     100
     C*** END ADD *** 10/31/01 ****************************************
     C*** BEG ADD *** 04/15/04 ****************************************
     C*
     C     SCAST         IFEQ      'FL'
     C     SCPAID        ANDLT     SCCHRG
     C*
     C     @@MODE        IFEQ      'A'
     C                   Z-ADD     SCSEQ#        ##DSEQ            5 0
     C                   Z-ADD     0             ##RRN3
     C                   Z-ADD     SCSEQ#        KKDSEQ
     C                   ELSE
     C                   Z-ADD     0             ##DSEQ
     C                   ENDIF                                                  @@MODE = 'A'
     C*
     C                   EXSR      EDITEB
     C*
     C                   ENDIF
     C*
     C*** END ADD *** 04/15/04 ****************************************
     C*
     C*** BEGIN DEL *** 03/26/01 **************************************
     C********** CSOVRP    ANDEQ*ZEROS
     C*** END   DEL *** 03/26/01 **************************************
     C********** @@MODE    ANDEQ'C'                       DEL 03/30/99
     C*** BEG DEL *** 03/30/99 ****************************************
     C********** SCMRA     IFNE *ZEROS
     C********** SCNTWK    ORNE *ZEROS
     C********** SCOTHR    ORNE *ZEROS
     C********** SCOTH2    ORNE *ZEROS
     C*** BEG ADD *** 03/31/99 ****************************************
     C********** @@MODE    IFEQ 'A'
     C**********           Z-ADDSCSEQ#    KKDSEQ
     C**********           ENDIF                           @@MODE = A
     C*** END ADD *** 03/31/99 ****************************************
     C********** DEKY      CHAINPDE01               N99
     C********** *IN99     DOWEQ*OFF
     C**********
     C********** DEEOB     IFNE *BLANKS
     C**********           MOVE DEEOB     XXEOB
     C**********           LEAVE
     C**********           ENDIF                           DEEOB NE *BLANKS
     C**********
     C********** DEKY      READEPDE01               N    99
     C**********           ENDDO                           *IN99 = *OFF
     C**********
     C*** END DEL *** 03/30/99 ****************************************
     C*
     C*** BEG DEL *** 03/30/99 ****************************************
     C********** XXEOB     IFEQ *BLANKS
     C**********           SETON                     9065
     C**********           GOTO CK1END
     C**********           ENDIF                           XXEOB = *BLANKS
     C*** END DEL *** 03/30/99 ****************************************
     C**********
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
     C*
     C*** BEG ADD *** 04/15/04 ****************************************
     C     FNDEB1        OREQ      'N'
     C     FNDEB2        OREQ      'N'
     C*** END ADD *** 04/15/04 ****************************************
     C*
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
     C*** Beg Chg *** 07/18/23 **************************************
     C**** DEEOB         ANDNE     *BLANKS
     C     DEEOBCD       ANDNE     *BLANKS
     C*** End Chg *** 07/18/23 **************************************
     C     DEESRC        OREQ      'N'
     C*** Beg Chg *** 07/18/23 **************************************
     C**** DEEOB         ANDNE     *BLANKS
     C     DEEOBCD       ANDNE     *BLANKS
     C*** End Chg *** 07/18/23 **************************************
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
     C*** BEG ADD *** 04/05/02 ****************************************
     C     MGSRC         OREQ      'C'
     C*** END ADD *** 04/05/02 ****************************************
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
     C*** BEG ADD *** 04/15/04 ****************************************
     C     FNDEB1        IFEQ      'N'
     C                   EXFMT     EB1WDW
     C                   ENDIF
     C     FNDEB2        IFEQ      'N'
     C                   EXFMT     EB2WDW
     C                   ENDIF
     C*** END ADD *** 04/15/04 ****************************************
     C*
     C                   CALL      'F0356R01'
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      'A'           ##MODE
     C                   PARM      *ZEROS        ##RRN
     C                   PARM                    ##RRN2
     C                   PARM                    ##RRN3
     C                   PARM                    ##DSEQ
     C*** BEG ADD *** 06/20/05 ****************************************
     C                   PARM      SCCHRG        ##CHRG
     C                   PARM      SCPAID        ##PAID
     C*** END ADD *** 06/20/05 ****************************************
     C*
     C*** BEG ADD *** 04/02/00 ****************************************
     C     ##KL          IFEQ      '1'
     C                   SETON                                        9014
     C                   SETOFF                                       69
     C                   ELSE
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
     C*** Beg Chg *** 07/18/23 **************************************
     C****               MOVEL(P)  DEEOB         XXEOB
     C                   MOVEL(P)  DEEOBCD       XXEOB
     C*** End Chg *** 07/18/23 **************************************
     C                   MOVEL(P)  DENOT1        XXNOT1
     C                   MOVEL(P)  DENOT2        XXNOT2
     C*** BEGIN ADD ****** 05/04/11 ************************************
     C                   MOVEL(P)  DENOT3        XXNOT3
     C                   MOVEL(P)  DENOT4        XXNOT4
     C*** END   ADD ****** 05/04/11 ************************************
     C                   SETOFF                                       90
     C                   LEAVE
     C                   ENDIF
     C*
     C     DEKY2         READE     PDE03                                  99
     C                   ENDDO
     C*** END   CHG *** 03/26/01 **************************************
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C*** BEG DEL *** 03/30/99 ****************************************
     C**********           ENDIF                           XXEOB = *BLANKS
     C*** END DEL *** 03/30/99 ****************************************
     C*
     C*** END ADD *** 03/30/99 ****************************************
     C*
     C*** BEG DEL *** 03/30/99 ****************************************
     C**********           ENDIF                           SCMRA = NE *ZEROS
     C*** END DEL *** 03/30/99 ****************************************
     C*** BEGIN ADD *** 03/26/01 **************************************
     C                   ENDIF                                                  GO = *YES
     C*** END   ADD *** 03/26/01 **************************************
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C*** BEG DEL *** 02/24/00 ****************************************
     C****BEG*ADD *** 04/19/99 ****************************************
     C*********
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
     C****END*ADD *** 04/19/99 ****************************************
     C*** END DEL *** 02/24/00 ****************************************
     C*** BEG ADD *** 08/10/04 ****************************************
     C*
     C     SCAST         IFEQ      'FL'
     C     SCPRCD        ANDNE     SCBLCD
     C     SCBLCD        ANDNE     *BLANKS
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
     C                   MOVE      'N'           FNDEB3            1
     C*
     C     DEKY          CHAIN(N)  PDE01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*** Beg Chg *** 07/18/23 **************************************
     C****               MOVEL(P)  DEEOB         KKKEY
     C                   MOVEL(P)  DEEOBCD       KKKEY
     C*** End Chg *** 07/18/23 **************************************
     C     TBKY          CHAIN     PEB01                              99
     C*** BEG DEL *** 04/22/07 ****************************************
     C*********
     C*********  *IN99     IFEQ *OFF
     C*********  DESTAT    ANDEQ'A'
     C*********  EBXREF    ANDEQ'S11'
     C*** END DEL *** 04/22/07 ****************************************
     C*** BEG ADD *** 04/22/07 ****************************************
     C     *IN99         IFEQ      *OFF
     C     DESTAT        ANDEQ     'A'
     C     EBXREF        ANDEQ     'D62'
     C*
     C     *IN99         OREQ      *OFF
     C     DESTAT        ANDEQ     'A'
     C     EBXREF        ANDEQ     'D80'
     C*** END ADD *** 04/22/07 ****************************************
     C*
     C                   MOVE      'Y'           FNDEB3
     C                   LEAVE
     C*
     C                   ENDIF
     C*
     C     DEKY          READE(N)  PDE01                                  99
     C*
     C                   ENDDO
     C*
     C     FNDEB3        IFEQ      'N'
     C*
     C                   EXFMT     EB3WDW
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
     C*** END ADD *** 08/10/04 ****************************************
     C*
     C* IF IN CHANGE MODE, THEN SET 'OUTFLG' TO END PROGRAM
     C*
     C     CALEND        TAG
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
     C     DHKY          CHAIN     PDH01                              99
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
     C                   Z-ADD     CAFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCFDAT
     C*
     C**********           Z-ADDCATOTC    SCCHRG
     C                   Z-ADD     CACHRG        SCCHRG
     C*** BEG ADD *** 02/25/00 ****************************************
     C                   MOVEL     CACSCD        SCCSCD
     C                   MOVEL(P)  SCCSCD        KKKEY
     C     TBKY          CHAIN     PCS01                              99
     C  N99              MOVEL(P)  CSDESC        SCCSDS
     C*
     C*** END ADD *** 02/25/00 ****************************************
     C*
     C                   MOVE      DHPOS         SCPOS
     C                   MOVE(P)   SCPOS         KKPOS
     C     PSKY          CHAIN     PPS01                              99
     C  N99              MOVEL(P)  PSDESC        SCPODS
     C*
     C                   MOVEL     DHPRCD        SCPRCD
     C*** BEG ADD *** 03/10/05 ****************************************
     C                   MOVEL     DHAREA        SCAREA
     C*** END ADD *** 03/10/05 ****************************************
     C*** BEG DEL *** 04/25/03 ****************************************
     C******************** MOVELSCPRCD    KKPRCD    P
     C******************** Z-ADDSCFDAT    SRDTI
     C******************** MOVE *BLANKS   SRDTO
     C******************** CALL 'SRCVTDAT'DTCYMD
     C******************** MOVELSRDTO     KKFDAT    P
     C***********PRKY***** SETGTPPR01
     C***********PRKY1**** REDPEPPR01                    99
     C************IN99**** IFEQ *OFF
     C******************** MOVELPRDSC1    SCCDDS
     C******************** ENDIF                           *IN99 IFEQ *ON
     C********************
     C******************** MOVELDHMOD1    SCMOD1
     C******************** MOVELDHMOD2    SCMOD2
     C******************** MOVELDHMOD3    SCMOD3
     C******************** MOVELDHMOD4    SCMOD4
     C********************
     C******************** MOVELDHTOS     SCTOS
     C******************** MOVELEOAST     HLTOS     P
     C******************** MOVELDHTOS     H1TOS
     C******************** MOVE H1TOS     HLTOS
     C******************** MOVELHLTOS     KKKEY     P
     C***********TBKY***** CHAINPTS02                99
     C**N99*************** MOVELTSDESC    SCTODS    P
     C*** END DEL *** 04/25/03 ****************************************
     C*
     C                   MOVE      DHDXR1        SCDXR1
     C                   MOVE      DHDXR2        SCDXR2
     C                   MOVE      DHDXR3        SCDXR3
     C                   MOVE      DHDXR4        SCDXR4
     C*** BEG CHG *** 04/25/03 ****************************************
     C******************** Z-ADDDHUNTS    SCUNTS
     C                   Z-ADD     1             SCUNTS
     C*** END CHG *** 04/25/03 ****************************************
     C*
     C*** BEG DEL *** 04/25/03 ****************************************
     C******************** MOVELDHAREA    SCAREA
     C******************** MOVELSCAREA    KKKEY     P
     C***********TBKY***** CHAINPPA01                99
     C**N99*************** MOVELPADESC    SCARDS    P
     C*** END DEL *** 04/25/03 ****************************************
     C*** BEG DEL *** 03/07/00 ****************************************
     C*** BEG ADD *** 09/30/99 ****************************************
     C******************** MOVELDHBPRC    SCBLCD    P
     C*** END ADD *** 09/30/99 ****************************************
     C*** END DEL *** 03/07/00 ****************************************
     C*** BEG CHG *** 06/15/99 ****************************************
     C**********           EXFMTSCR01
     C**********           SETON                     90
     C                   SETON                                        90
     C*** END CHG *** 06/15/99 ****************************************
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
     C**********           EXFMTSCR01
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
     C*  CALL FA713R WHICH WILL CALL APPROPRIATE ADJUDICATION PROGRAMS
     C*
     C*** BEG DEL *** 11/12/99 ****************************************
     C**********           Z-ADD@@RRN2    ##RRN
     C**********           CALL 'FA713R'
     C**********           PARM ' '       ##KC
     C**********           PARM ' '       ##KL
     C**********           PARM           ##RRN
     C**********
     C**WRITE/UPDATE EOMB DETAIL AMOUNT TRANSACTION FILE
     C**********
     C********** CAKY3     CHAINPCA03                99
     C********** *IN99     DOWEQ*OFF
     C**********           Z-ADD0         SMCHG
     C**********           Z-ADD0         SMMRA
     C**********           Z-ADD0         SMNDIS
     C**********           Z-ADD0         SMOTHR
     C**********           Z-ADD0         SMOTH2
     C**********           Z-ADD0         SMPAID
     C**********           Z-ADDCADSEQ    KKDSEQ
     C********** DAKY      CHAINPDA02                98
     C********** *IN98     DOWEQ*OFF
     C**********           ADD  D2CHRG    SMCHG
     C**********           ADD  D2MRA     SMMRA
     C**********           ADD  D2NDIS    SMNDIS
     C**********           ADD  D2OTHR    SMOTHR
     C**********           ADD  D2OTH2    SMOTH2
     C**********           ADD  D2PAID    SMPAID
     C********** DAKY      READEPDA02                    98
     C**********           ENDDO                           *IN98 = *OFF
     C**********
     C**COMPARE*THE DETAIL CHARGE AMOUNTS WITH SUM OF DETAIL AMOUNTS
     C**IF*THERE IS A DIFFERENCE, THEN WRITE OR UPDATE DETAIL AMONT REC
     C**********
     C********** CATOTC    SUB  SMCHG     DFCHRG
     C********** CACHRG    SUB  SMCHG     DFCHRG
     C********** CATOTD    SUB  SMMRA     DFMRA
     C********** CAMRA     SUB  SMMRA     DFMRA
     C********** CANDIS    SUB  SMNDIS    DFNDIS
     C********** CAOTHR    SUB  SMOTHR    DFOTHR
     C********** CAOTH2    SUB  SMOTH2    DFOTH2
     C********** CATOTP    SUB  SMPAID    DFPAID
     C********** CAPAID    SUB  SMPAID    DFPAID
     C**********
     C********** DFCHRG    IFNE *ZEROS
     C********** DFMRA     ORNE *ZEROS
     C********** DFNDIS    ORNE *ZEROS
     C********** DFOTHR    ORNE *ZEROS
     C********** DFOTH2    ORNE *ZEROS
     C********** DFPAID    ORNE *ZEROS
     C**********
     C**UPDATE*IF THERE IS A FDDAP FOR THE CURRENT RECON
     C**ELSE*WRITE NEW FDDAP RECORD TO REFLECT CHANGES
     C**********
     C********** DAKY1     CHAINPDA01                97
     C********** *IN97     IFEQ *OFF
     C**********           Z-ADDCATOTC    DACHRG
     C**********           Z-ADDCACHRG    DACHRG
     C**********           Z-ADDCATOTD    DAMRA
     C**********           Z-ADDCAMRA     DAMRA
     C**********           Z-ADDCANDIS    DANDIS
     C**********           Z-ADDCAOTHR    DAOTHR
     C**********           Z-ADDCAOTH2    DAOTH2
     C**********           Z-ADDCATOTP    DAPAID
     C**********           Z-ADDCAPAID    DAPAID
     C**********           TIME           DSDTTM
     C**********           MOVELQ1PGM     DAPGM     P
     C**********           MOVELQ1USER    DAUSER    P
     C**********           Z-ADDDSTIME    DATIME
     C**********           Z-ADDDSDATE    SRDTI
     C**********           MOVE *BLANKS   SRDTO
     C**********           MOVE *BLANKS   SRERR
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     DADT
     C**********           UPDATPDA01
     C**********
     C**********           ELSE
     C**********
     C**********           CLEARPDA01
     C**********           Z-ADD##CO#     DACO#
     C**********           Z-ADDKKSUB#    DASUB#
     C**********           Z-ADDKKFND     DAFND
     C**********           Z-ADDKKFYR     DAFYR
     C**********           Z-ADDKKCASE    DACASE
     C**********           MOVELEOSS#     DASS#
     C**********           Z-ADDEOACDT    DAACDT
     C**********           Z-ADDKKEOB#    DAEOB#
     C**********           Z-ADDKKDSEQ    DADSEQ
     C**********           Z-ADDKKRCN     DARCN#
     C**********           TIME           DSDTTM
     C**********           Z-ADDDSDATE    SRDTI
     C**********           MOVE *BLANKS   SRDTO
     C**********           MOVE *BLANKS   SRERR
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     DATRDT
     C**********           Z-ADDDFCHRG    DACHRG
     C**********           Z-ADDDFMRA     DAMRA
     C**********           Z-ADDDFNDIS    DANDIS
     C**********           Z-ADDDFOTHR    DAOTHR
     C**********           Z-ADDDFOTH2    DAOTH2
     C**********           Z-ADDDFPAID    DAPAID
     C**********           MOVE 'A'       DASTAT
     C**********           MOVELQ1PGM     DAPGM     P
     C**********           MOVELQ1PGM     DAIPGM    P
     C**********           MOVELQ1USER    DAUSER    P
     C**********           MOVELQ1USER    DAIUSR    P
     C**********           Z-ADDDSTIME    DATIME
     C**********           Z-ADDDSTIME    DAITIM
     C**********           MOVELSRDTO     DADT
     C**********           MOVELSRDTO     DAIDT
     C**********           WRITEPDA01
     C**********           ENDIF                           *IN97 = *OFF
     C**********
     C**********           ENDIF                           DFCHRG NE *ZEROS
     C********** CAKY3     READEPCA03                    99
     C**********           ENDDO                           *IN99 = *OFF
     C**********
     C*** END DEL *** 11/12/99 ****************************************
     C*
     C                   SETON                                        LR
     C*
     C                   RETURN
     C*
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
     C*
     C*================================================================
     C* KIDO - F9=CODE INFO
     C*================================================================
     C     KIDO          BEGSR
     C                   Z-ADD     SCFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         KKFDAT
     C     CARCFL        IFEQ      'C'
     C                   MOVE      EOAST         ##ST              2
     C                   MOVEL(P)  SCPRCD        KKPRCD
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRN
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     PRRRN         ##RRN
     C                   ENDIF                                                  *IN99 = On
     C*** BEG ADD *** 09/28/04 ****************************************
     C     SCAST         IFEQ      'TX'
     C     KKFDAT        ANDGE     1040901
     C     SCPRCD        ANDGE     '10000'
     C     SCPRCD        ANDLE     '69999'
     C     SCCNTY        ANDNE     *BLANKS
     C     SCRGN#        ANDNE     *BLANKS
     C*
     C                   MOVEL     'TX'          XXCNTY
     C                   MOVE      SCCNTY        XXCNTY
     C                   MOVEL(P)  XXCNTY        KKKEY
     C     CCKY          CHAIN     PCC01                              99
     C*
     C                   MOVEL(P)  CC10A1        KKRGN
     C                   MOVE(P)   PR1AI         KKGRP
     C*
     C     AGKY          SETGT     PAG01
     C     AGKY1         READPE    PAG01                                  99
     C*
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             #1RRN             9 0
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     DBRRND        #1RRN
     C                   ENDIF                                                  *IN99 = On
     C*
     C                   CALL      'F2522R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    #1RRN
     C                   PARM                    ##ST
     C*
     C                   ENDIF                                                  SCAST = TX
     C*** END ADD *** 09/28/04 ****************************************
     C                   CALL      'F2305R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRN
     C                   PARM                    ##ST
     C                   ENDIF                                                  RCFP = 'C'
     C     CARCFL        IFEQ      'Y'
     C                   MOVEL     SCZIPA        KKZIPA
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
     C**********           SETOF                     4041
     C**********           SETOF                     484950
     C**********           SETOF                     535455
     C*** END   DEL *** 10/13/00 **************************************
     C*
     C                   SELECT
     C*
     C*** BEG ADD *** 09/28/04 ****************************************
     C*
     C*  COUNTY CODE SEARCH
     C*
     C     HFLD          WHENEQ    'SCCNTY'
     C     *IN24         ANDNE     *ON
     C*
     C                   CALL      'F2360R'
     C                   PARM      '2'           ##CALL
     C                   PARM      '0'           ##KC
     C                   PARM      '0'           ##KL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      EOAST         ##ST
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PCC02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C     CCSTAT        OREQ      'I'
     C*
     C     3             SUBST(P)  CCKEY:3       SCCNTY
     C                   SETON                                        9095
     C*
     C                   ELSE
     C                   SETON                                        9011
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C                   SETON                                        9011
     C*
     C                   ENDIF
     C*** END ADD *** 09/28/04 ****************************************
     C*
     C*  ADJUDICATION CODE SEARCH
     C*
     C     HFLD          WHENEQ    'SCCSCD'
     C*** BEGIN ADD *** 07/23/99 **************************************
     C     *IN24         ANDNE     *ON
     C*** END ADD *** 07/23/99 ****************************************
     C*
     C                   CALL      'F2300R'
     C                   PARM      '2'           ##CALL
     C                   PARM      '0'           ##KC
     C                   PARM      '0'           ##KL
     C                   PARM      *ZEROS        ##RRN
     C*
     C     ##KL          IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*** BEGIN CHG *** 10/13/00 **************************************
     C**********           SETON                     9040  pos cursor
     C                   SETON                                        9095      pos cursor
     C*** END   CHG *** 10/13/00 **************************************
     C                   ENDIF                                                  ##KL IFEQ *ON
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PCS02                              99
     C                   MOVEL(P)  CSKEY         SCCSCD
     C                   MOVEL(P)  CSDESC        SCCSDS
     C                   SETON                                        9041      pos cursor
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
     C*  TYPE OF SERVICE SEARCH
     C*
     C     HFLD          WHENEQ    'SCTOS'
     C                   MOVE      EOAST         ##AST
     C                   MOVEL     SCTOS         HLTOS
     C                   Z-ADD     0             ##RRN             9 0
     C                   CALL      'F2365R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM                    ##AST
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVEL     HLTOS         SCTOS
     C                   SETON                                        9032      pos cursor
     C                   ELSE
     C     ##RRN         CHAIN     PTS01                              99
     C  N99              MOVEL     TSKEY         XXTOS             3
     C  N99              MOVE      XXTOS         X1TOS             1
     C  N99              MOVEL(P)  X1TOS         SCTOS
     C  N99              MOVEL(P)  TSDESC        SCTODS
     C                   SETON                                        9054      pos cursor
     C                   ENDIF
     C*
     C* PROCEDURE CODE SEARCH
     C*
     C     HFLD          WHENEQ    'SCPRCD'
     C**********           MOVE EOAST     ##AST
     C**********           MOVE *BLANKS   ##CPT4
     C**********           MOVE *BLANKS   ##DESC
     C**********           MOVE SCPRCD    HLCPT4
     C**********           MOVELSCCDDS    HLDESC 40 P
     C**********           CALL 'F2305R'
     C**********           PARM '2'       ##CALL
     C**********           PARM ' '       ##KC
     C**********           PARM ' '       ##KL
     C**********           PARM           ##AST
     C**********           PARM           ##CPT4 15
     C**********           PARM           ##DESC 40
     C**********
     C********** ##KC      IFEQ '1'
     C********** ##KL      OREQ '1'
     C********** ##CPT4    OREQ *BLANKS
     C**********           MOVE HLCPT4    SCPRCD
     C**********           MOVELHLDESC    SCCDDS    P
     C**********           SETON                     9054  pos cursor
     C**********           ELSE
     C**********           MOVEL##CPT4    SCPRCD    P
     C**********           MOVEL##DESC    SCCDDS    P
     C**********           SETON                     9034  pos cursor
     C**********           ENDIF
     C*
     C                   MOVE      SCPRCD        HLCPT4
     C                   MOVEL(P)  SCCDDS        HLDESC           40
     C                   MOVE      *BLANKS       ##PROC            5
     C                   MOVE      *BLANKS       ##PROD           50
     C                   Z-ADD     0             ##RRN1            9 0
     C                   MOVE      SCAST         ##AST
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
     C                   MOVEL(P)  HLDESC        SCCDDS
     C                   SETON                                        9054      pos cursor
     C                   ELSE
     C                   MOVEL     ##PROC        SCPRCD
     C                   MOVEL(P)  ##PROD        SCCDDS
     C*** BEGIN CHG *** 10/13/00 **************************************
     C**********           SETON                     9034
     C                   SETON                                        9097
     C*** END   CHG *** 10/13/00 **************************************
     C                   ENDIF                                                  ##RTCD = 1
     C*** BEGIN ADD *** 08/27/01 **************************************
     C*
     C* BILLED PROCEDURE CODE SEARCH
     C*
     C     HFLD          WHENEQ    'SCBLCD'
     C     EOAST         ANDEQ     'TX'
     C                   CALL      'F2532R'
     C                   PARM      '2'           ##CALL
     C                   PARM      '0'           ##KC
     C                   PARM      '0'           ##KL
     C                   PARM      EOAST         ##AST
     C                   PARM      *BLANKS       ##CPT4           15
     C                   PARM      *BLANKS       ##DESC           40
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##CPT4        OREQ      *BLANKS
     C                   SETON                                        9790      pos cursor
     C                   ELSE
     C                   MOVEL(P)  ##CPT4        SCBLCD
     C                   SETON                                        9034      pos cursor
     C                   ENDIF
     C*
     C*** END   ADD *** 08/27/01 **************************************
     C*
     C* PROCEDURE MODIFIER SEARCH (for modifier #1)
     C*
     C     HFLD          WHENEQ    'SCMOD1'
     C                   MOVE      EOAST         ##AST
     C                   MOVE      *BLANKS       ##KEY
     C                   MOVE(P)   SCMOD1        HLMOD
     C                   CALL      'F2315R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##AST
     C                   PARM                    ##KEY            15
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##KEY         OREQ      *BLANKS
     C                   MOVE      HLMOD         SCMOD1
     C                   SETON                                        9034      pos cursor
     C                   ELSE
     C                   MOVEL     ##KEY         SCMOD1
     C                   SETON                                        9035      pos cursor
     C                   ENDIF
     C*
     C* PROCEDURE MODIFIER SEARCH (for modifier #2)
     C*
     C     HFLD          WHENEQ    'SCMOD2'
     C                   MOVE      EOAST         ##AST
     C                   MOVE      *BLANKS       ##KEY
     C                   MOVE(P)   SCMOD2        HLMOD
     C                   CALL      'F2315R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##AST
     C                   PARM                    ##KEY            15
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##KEY         OREQ      *BLANKS
     C                   MOVE      HLMOD         SCMOD2
     C                   SETON                                        9035      pos cursor
     C                   ELSE
     C                   MOVEL     ##KEY         SCMOD2
     C                   SETON                                        9036      pos cursor
     C                   ENDIF
     C*
     C* PROCEDURE MODIFIER SEARCH (for modifier #3)
     C*
     C     HFLD          WHENEQ    'SCMOD3'
     C                   MOVE      EOAST         ##AST
     C                   MOVE      *BLANKS       ##KEY
     C                   MOVE(P)   SCMOD3        HLMOD
     C                   CALL      'F2315R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##AST
     C                   PARM                    ##KEY            15
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##KEY         OREQ      *BLANKS
     C                   MOVE      HLMOD         SCMOD3
     C                   SETON                                        9036      pos cursor
     C                   ELSE
     C                   MOVEL     ##KEY         SCMOD3
     C                   SETON                                        9037      pos cursor
     C                   ENDIF
     C*
     C* PROCEDURE MODIFIER SEARCH (for modifier #4)
     C*
     C     HFLD          WHENEQ    'SCMOD4'
     C                   MOVE      EOAST         ##AST
     C                   MOVE      *BLANKS       ##KEY
     C                   MOVE(P)   SCMOD4        HLMOD
     C                   CALL      'F2315R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##AST
     C                   PARM                    ##KEY            15
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##KEY         OREQ      *BLANKS
     C                   MOVE      HLMOD         SCMOD4
     C                   SETON                                        9037      pos cursor
     C                   ELSE
     C                   MOVEL     ##KEY         SCMOD4
     C  N52              SETON                                        9038      pos cursor
     C   52              SETON                                        9053
     C                   ENDIF
     E*** BEG ADD *** 10/31/05 ****************************************
     C*
     C*  NDC CODE SEARCH
     C     HFLD          WHENEQ    'SCNDC'
     C                   MOVEL     SCNDC         HLNDC            11
     C                   CALL      'F2350R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      *ZEROS        ##RRN
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVEL(P)  HLNDC         SCNDC
     C                   SETON                                        9005      pos cursor
     C                   ELSE
     C     ##RRN         CHAIN     PRX02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  RXNDC         SCNDC
     C                   MOVEL(P)  RXNAME        SCNDCD
     C                   SETON                                        9033      pos cursor
     C                   ENDIF
     C                   ENDIF
     E*** END ADD *** 10/31/05 ****************************************
     C*
     C*  BODY PART SEARCH
     C     HFLD          WHENEQ    'SCAREA'
     C                   MOVE      EOAST         ##AST
     C                   MOVE      *BLANKS       ##KEY
     C                   MOVEL     SCAREA        HLAREA           15
     C                   CALL      'F2325R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##AST
     C                   PARM                    ##KEY
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##KEY         OREQ      *BLANKS
     C                   MOVEL(P)  HLAREA        SCAREA
     C                   SETON                                        9039      pos cursor
     C                   ELSE
     C                   MOVEL(P)  ##KEY         SCAREA
     C                   MOVEL(P)  ##KEY         KKKEY
     C     TBKY          CHAIN     PPA01                              99
     C  N99              MOVEL(P)  PADESC        SCARDS
     C                   SETON                                        9055      pos cursor
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
     C*** BEGIN CHG *** 10/13/00 **************************************
     C**********           SETON                     9051
     C                   SETON                                        90
     C*** END   CHG *** 10/13/00 **************************************
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*** BEGIN ADD *** 10/13/00 **************************************
     C*** Beg Add *** 07/18/25 ***************************************
     C     HFLD          WHENEQ    'SCNPIR'
     C                   EVAL      MSNPIR = 'MCR0676'
     C                   EVAL      ERNPIR = *ON
     C                   EVAL      *IN90  = *ON
     C*** End Add *** 07/18/25 ***************************************
     C                   OTHER
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCFDAT'
     C                   SETOFF                                       91
     C                   SETON                                        929394
     C     HFLD          WHENEQ    'SCBLCD'
     C                   SETOFF                                       929394
     C                   SETON                                        91
     C     HFLD          WHENEQ    'SCDXR1'
     C                   SETOFF                                       919394
     C                   SETON                                        92
     C     HFLD          WHENEQ    'SCDXR2'
     C                   SETOFF                                       919294
     C                   SETON                                        93
     C     HFLD          WHENEQ    'SCDXR3'
     C                   SETOFF                                       919293
     C                   SETON                                        94
     C     HFLD          WHENEQ    'SCDXR4'
     C                   SETOFF                                       9394
     C                   SETON                                        9192
     C     HFLD          WHENEQ    'SCCHRG'
     C                   SETOFF                                       9194
     C                   SETON                                        9293
     C     HFLD          WHENEQ    'SCPAID'
     C                   SETOFF                                       9192
     C                   SETON                                        9394
     C     HFLD          WHENEQ    'SCMRA'
     C                   SETOFF                                       9293
     C                   SETON                                        9194
     C     HFLD          WHENEQ    'SCNTWK'
     C                   SETOFF                                       94
     C                   SETON                                        919293
     C     HFLD          WHENEQ    'SCOTHR'
     C                   SETOFF                                       92
     C                   SETON                                        919394
     C     HFLD          WHENEQ    'SCOTH2'
     C                   SETOFF                                       93
     C                   SETON                                        919294
     C     HFLD          WHENEQ    'SCUNTS'
     C                   SETON                                        919293    94
     C                   SETON                                        94
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
     C*
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
     C* KPDO - F15=DISPLAY LINES
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
     C*** BEG DEL *** 05/12/09 ****************************************
     C*** BEGIN ADD *** 09/19/03 **************************************
     C*================================================================
     C* KQDO - F16=ADL INFO
     C*================================================================
     C***        KQDO      BEGSR
     C*
     C***                  CALL 'F8362R'
     C***                  PARM EHLIC#    ##LIC# 12
     C***                  PARM *BLANKS   ##RTCD  1
     C*
     C***                  SETON                     90
     C*
     C***                  ENDSR
     C*================================================================
     C* KYDO - F25=MORE KEYS
     C*================================================================
     C***        KYDO      BEGSR
     C*
     C***        *IN07     IFEQ *OFF
     C***                  SETON                     07
     C***                  ELSE
     C***                  SETOF                     07
     C***                  ENDIF
     C*
     C***                  SETON                     90
     C*
     C***                  ENDSR
     C*** END   ADD *** 09/19/03 **************************************
     C*** END DEL *** 05/12/09 ****************************************
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   Z-ADD     0             SCEOB#
     C                   Z-ADD     0             SCLIN#
     C                   Z-ADD     0             SCSEQ#
     C**********           Z-ADD0         SCTOTC
     C                   MOVE      *BLANKS       SCCSCD
     C                   MOVE      *BLANKS       SCCSDS
     C                   Z-ADD     0             SCFDAT
     C*** BEG CHG *** 04/03/03 ****************************************
     C******************** MOVE *BLANKS   SCPOS
     C                   MOVE      *ZEROS        SCPOS
     C*** END CHG *** 04/03/03 ****************************************
     C                   MOVE      *BLANKS       SCPODS
     C                   MOVE      *BLANKS       SCTOS
     C                   MOVE      *BLANKS       SCTODS
     C                   MOVE      *BLANKS       SCPRCD
     C                   MOVE      *BLANKS       SCCDDS
     C*** BEG ADD *** 06/15/99 ****************************************
     C                   MOVE      *BLANKS       SCBLCD
     C*** END ADD *** 06/15/99 ****************************************
     C                   MOVE      *BLANKS       SCMOD1
     C                   MOVE      *BLANKS       SCMOD2
     C                   MOVE      *BLANKS       SCMOD3
     C                   MOVE      *BLANKS       SCMOD4
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
     C                   Z-ADD     0             SCCHRG
     C                   Z-ADD     0             SCPAID
     C                   Z-ADD     0             SCMRA
     C                   Z-ADD     0             SCNTWK
     C                   Z-ADD     0             SCOTHR
     C                   MOVE      *BLANKS       SCOT1C
     C                   Z-ADD     0             SCOTH2
     C                   MOVE      *BLANKS       SCOT2C
     C*** BEG CHG *** 11/03/00 ****************************************
     C******************** Z-ADD0         SCUNTS
     C                   Z-ADD     1             SCUNTS
     C*** END CHG *** 11/03/00 ****************************************
     C                   MOVE      *BLANKS       SCAREA
     C                   MOVE      *BLANKS       SCARDS
     C*** BEG ADD *** 10/31/05 ****************************************
     C                   MOVE      *BLANKS       SCNDC
     C                   MOVE      *BLANKS       SCNDCD
     C*** END ADD *** 10/31/05 ****************************************
     C                   CLEAR                   SCEOB1
     C                   CLEAR                   SCEOB2
     C                   CLEAR                   SCEOB3
     C                   CLEAR                   SCEOB4
     C*** BEGIN ADD *** 11/28/07 **************************************
     C                   CLEAR                   SCNPIR
     C*** END   ADD *** 11/28/07 **************************************
     C*
     C                   MOVE      EOAST         SCAST
     C**********           MOVE EOZIPA    SCZIPA    P
     C                   Z-ADD     EOEOB#        SCEOB#
     C*** BEG ADD *** 09/28/04 ****************************************
     C                   MOVE      *BLANKS       SCCNTY
     C*** END ADD *** 09/28/04 ****************************************
     C*
     C* SET LINE NUMBER
     C*
     C     @@MODE        IFEQ      'A'
     C*****                ADD  1         XXLIN#
     C*****                Z-ADDXXLIN#    SCLIN#
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
     C                   Z-ADD     DSDATE        SCEDAT
     C*
     C* DEFAULT CLAIMS STATUS CODE FROM FAEOP
     C*
     C                   MOVEL     EOCSCD        SCCSCD
     C*** BEGIN ADD *** 07/23/99 **************************************
     C   24              MOVEL     '300'         SCCSCD
     C*** END ADD *** 07/23/99 ****************************************
     C*
     C* DEFAULT 1ST DIAGNOSIS CROSS REFERENCE CODE IF ONLY ONE
     C* DIAGNOSIS CODE IS ENTERED.
     C*
     C   52              MOVE      'X'           SCDXR1
     C                   ENDIF                                                  @@MODE = 'A'
     C*
     C* FROM FAEHP
     C*
     C     EHKY          CHAIN(N)  PEH01                              99
     C     *IN99         IFEQ      *OFF
      *** Beg Add *** 02/04/14 ****************************************
      /free
       If EHicdind <> 'Y';
       //If ICD9 code is selected
      /end-free
      *** END Add *** 02/04/14 ****************************************
     C                   MOVEL(P)  EHDIA1        SCDIA1
     C                   MOVEL(P)  EHDIA2        SCDIA2
     C                   MOVEL(P)  EHDIA3        SCDIA3
     C                   MOVEL(P)  EHDIA4        SCDIA4
      *** Beg Add *** 02/04/14 ****************************************
      /free
       Else;
       //If ICD10 code is selected
          SCDIA1 = EHICD1001;
          SCDIA2 = EHICD1002;
          SCDIA3 = EHICD1003;
          SCDIA4 = EHICD1004;
       Endif;
      /end-free
      *** END Add *** 02/04/14 ****************************************
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C*
     C* GET DIAGNOSIS CODES DESCRIPTIONS
     C*
      *** Beg Add *** 02/04/14 ****************************************
      /free
       If EHicdind <> 'Y';
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
     C                   MOVEL(P)  SCCSCD        KKKEY
     C     TBKY          CHAIN     PCS01                              99
     C  N99              MOVEL(P)  CSDESC        SCCSDS
     C*
     C*** BEG ADD *** 09/19/02 ****************************************
     C                   MOVE      *BLANKS       SCRGN#
     C*** END ADD *** 09/19/02 ****************************************
     C*
     C     @@MODE        IFNE      'A'
     C*
     C*** BEG ADD *** 09/19/02 ****************************************
     C     *IN51         IFEQ      *OFF
     C*
     C                   MOVE      *BLANKS       SCRGN#
     C*
     C                   Z-ADD     CAFDAT        KKEFDT
     C                   MOVEL     EOFLIC        KKFZCD
     C*
     C     ZRKY          SETGT     PZR01                              99
     C                   READP     PZR01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C     KKFZCD        ANDGE     ZRFZCD
     C     KKFZCD        ANDLE     ZRTZCD
     C                   MOVE(P)   ZRRGN#        SCRGN#
     C*
     C                   ELSE
     C*** BEG CHG *** 07/10/03 ****************************************
     C******************** MOVE *BLANKS   SCRGN#
     C*
     C* IF ZIP CODE IS NOT IN THE REGION FILE SET THE STATE DEFAULTS
     C*
     C                   SELECT
     C     SCAST         WHENEQ    'PA'
     C                   MOVE(P)   3             SCRGN#
     C     SCAST         WHENEQ    'TX'
     C                   MOVE(P)   99            SCRGN#
     C*** BEG ADD *** 12/12/03 ****************************************
     C     SCAST         WHENEQ    'FL'
     C                   MOVE(P)   99            SCRGN#
     C*** END ADD *** 12/12/03 ****************************************
     C*** BEG ADD *** 12/19/04 ****************************************
     C     SCAST         WHENEQ    'NY'
     C                   MOVE(P)   1             SCRGN#
     C*** END ADD *** 12/19/04 ****************************************
     C                   OTHER
     C                   MOVE      *BLANKS       SCRGN#
     C                   ENDSL
     C*** END CHG *** 07/10/03 ****************************************
     C*
     C                   ENDIF
     C*** BEG ADD *** 12/19/04 ****************************************
     C*
     C* FOR NY PARTICIPANTS IN TUPP SET REGION = 4 (HIGHEST)
     C*
     C     SCAST         IFEQ      'NY'
     C     5             SUBST(P)  P1CNTY:1      XXSMSA            5
     C     XXSMSA        IFEQ      'TUPP '
     C                   MOVE(P)   4             SCRGN#
     C                   ENDIF
     C                   ENDIF
     C*** END ADD *** 12/19/04 ****************************************
     C*** BEG ADD *** 12/12/03 ****************************************
     C     SCAST         IFEQ      'FL'
     C     SCRGN#        ANDEQ     '  099'
     C                   MOVE      '01/02'       SCRGN#
     C                   ENDIF
     C*** END ADD *** 12/12/03 ****************************************
     C*
     C                   ENDIF                                                  *IN51 =
     C*** END ADD *** 09/19/02 ****************************************
     C*
     C* FROM FDDHP
     C*
     C                   MOVE(P)   DHPOS         SCPOS
     C                   MOVE(P)   SCPOS         KKPOS
     C     PSKY          CHAIN     PPS01                              99
     C  N99              MOVEL(P)  PSDESC        SCPODS
     C*
     C                   MOVEL(P)  DHTOS         SCTOS
     C                   MOVEL(P)  EOAST         HLTOS
     C                   MOVEL     DHTOS         H1TOS             1
     C                   MOVE      H1TOS         HLTOS
     C                   MOVEL(P)  HLTOS         KKKEY
     C*****                MOVELSCTOS     KKKEY
     C     TBKY          CHAIN     PTS02                              99
     C  N99              MOVEL(P)  TSDESC        SCTODS
     C*
     C                   MOVEL(P)  DHPRCD        SCPRCD
     C                   MOVEL(P)  DHPRDS        SCCDDS
     C                   MOVEL(P)  DHMOD1        SCMOD1
     C                   MOVEL(P)  DHMOD2        SCMOD2
     C                   MOVEL(P)  DHMOD3        SCMOD3
     C                   MOVEL(P)  DHMOD4        SCMOD4
     C                   MOVE      DHDXR1        SCDXR1
     C                   MOVE      DHDXR2        SCDXR2
     C                   MOVE      DHDXR3        SCDXR3
     C                   MOVE      DHDXR4        SCDXR4
     C                   Z-ADD     DHUNTS        SCUNTS
     C*
     C                   MOVEL     DHAREA        SCAREA
     C                   MOVEL(P)  SCAREA        KKKEY
     C     TBKY          CHAIN     PPA01                              99
     C  N99              MOVEL(P)  PADESC        SCARDS
     C*** BEGIN ADD *** 11/28/07 **************************************
     C                   MOVEL(P)  DHNPIR        SCNPIR
     C*** END   ADD *** 11/28/07 **************************************
     C*
     C*** BEG ADD *** 11/08/01 ****************************************
     C                   EXSR      NXTSEQ
     C                   Z-ADD     ZZDSEQ        SVDSEQ
     C                   Z-ADD     ZZLIN#        SVLIN#
     C*** END ADD *** 11/08/01 ****************************************
     C*** BEG ADD *** 10/31/05 ****************************************
     C                   MOVEL(P)  DHNDC         SCNDC
     C                   MOVEL(P)  DHNDCD        SCNDCD
     C*** END ADD *** 10/31/05 ****************************************
     C*
     C* GET INFO FROM FDCAP
     C*
     C     @@RRN3        CHAIN(N)  PCA02                              99
     C     *IN99         IFEQ      *OFF
     C*** BEG ADD *** 11/08/01 ****************************************
     C*
     C* FIND OUT IF MULTI-LINE OR NOT
     C*
     C*
     C* Save recon
     C*
     C                   Z-ADD     CARCN         SVRCN
     C*
     C*** END ADD *** 11/08/01 ****************************************
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
     C*
     C*** BEG ADD *** 06/12/99 ****************************************
     C                   MOVEL     DHBPRC        SCBLCD
     C*** END ADD *** 06/12/99 ****************************************
     C                   Z-ADD     CAIDT         SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEDAT
     C*
     C**********           Z-ADDCATOTC    SCCHRG
     C                   Z-ADD     CACHRG        SCCHRG
     C                   Z-ADD     CAOTHR        SCOTHR
     C                   Z-ADD     CAOTH2        SCOTH2
     C                   Z-ADD     CANDIS        SCNTWK
     C**********           Z-ADDCATOTD    SCMRA
     C                   Z-ADD     CAMRA         SCMRA
     C**********           Z-ADDCATOTP    SCPAID
     C                   Z-ADD     CAPAID        SCPAID
     C                   MOVE      CAOT1C        SCOT1C
     C                   MOVE      CAOT2C        SCOT2C
     C                   Z-ADD     CALIN#        SCLIN#
     C                   Z-ADD     CADSEQ        SCSEQ#
     C                   ENDIF                                                  *IN99 IFEQ *OFF
     C*
     C* GET PROCEDURE CODE DESCRIPTION
     C*
     C*****                MOVELSCPRCD    KKPRCD    P
     C*****                Z-ADDSCFDAT    SRDTI
     C*****                MOVE *BLANKS   SRDTO
     C*****                CALL 'SRCVTDAT'DTCYMD
     C*****                MOVELSRDTO     KKFDAT    P
     C*****      PRKY      SETGTPPR01
     C*****      PRKY1     REDPEPPR01                    99
     C*****      *IN99     IFEQ *OFF
     C*****                MOVELPRDSC1    SCCDDS
     C*****                ENDIF                           *IN99 IFEQ *ON
     C*
     C* DISPLAY EOB CODES FOR BILL
     C*
     C     @@MODE        IFNE      'A'
     C                   Z-ADD     0             XXSEQ#            2 0
     C     DEKY          CHAIN(N)  PDE01                              99
     C     *IN99         DOWEQ     *OFF
     C*** Beg Chg *** 07/18/23 **************************************
     C**** DEEOB         IFNE      *BLANKS
     C     DEEOBCD       IFNE      *BLANKS
     C*** End Chg *** 07/18/23 **************************************
     C                   ADD       1             XXSEQ#
     C*** Beg Del *** 07/18/23 **************************************
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
     C*** End Del *** 07/18/23 **************************************
     C*** Beg Add *** 07/18/23 **************************************
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
     C*** End Add *** 07/18/23 **************************************
     C                   ENDIF                                                  DEEOB NE *BLANKS
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
     C*** BEG ADD *** 09/28/04 ****************************************
     C     P1KY          CHAIN(N)  PP101                              99
     C     *IN99         IFEQ      *OFF
     C     3             SUBST(P)  P1CNTY:6      SCCNTY
     C                   ENDIF
     C*** END ADD *** 09/28/04 ****************************************
     C*** BEG ADD *** 04/17/08 ****************************************
     C*
     C* CHECK TO SEE IF LINES SPLIT OR NOT
     C*
     C                   CALL      'SA014R'
1    C                   PARM      EOAST         ##ST              2
2    C                   PARM      EOBTYP        ##BTYP            1
3    C                   PARM      CAFDAT        ##FDAT            7 0
4    C                   PARM      *BLANK        ##SPLT            1
     C*** END ADD *** 04/17/08 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C* LOAD FIELDS FOR FDDHP
     C*
 CCC C                   MOVE(P)   SCPOS         DHPOS
     C                   MOVEL(P)  SCTOS         DHTOS
     C                   MOVEL(P)  SCPRCD        DHPRCD
     C                   MOVEL(P)  SCCDDS        DHPRDS
     C                   MOVEL(P)  SCMOD1        DHMOD1
     C                   MOVEL(P)  SCMOD2        DHMOD2
     C                   MOVEL(P)  SCMOD3        DHMOD3
     C                   MOVEL(P)  SCMOD4        DHMOD4
     C                   MOVE      SCDXR1        DHDXR1
     C                   MOVE      SCDXR2        DHDXR2
     C                   MOVE      SCDXR3        DHDXR3
     C                   MOVE      SCDXR4        DHDXR4
     C*** BEG ADD *** 10/31/05 ****************************************
     C                   MOVEL(P)  SCNDC         DHNDC
     C                   MOVEL(P)  SCNDCD        DHNDCD
     C*** END ADD *** 10/31/05 ****************************************
     C*** BEG ADD *** 04/06/99 ****************************************
     C                   MOVE      XXRCFL        DHRCFL
     C*** END ADD *** 04/06/99 ****************************************
     C*** BEG ADD *** 05/19/99 ****************************************
     C                   MOVE      EHPPLN        DHPPLN
     C*** END ADD *** 05/19/99 ****************************************
     C*** BEG ADD *** 02/18/00 ****************************************
     C* If billed code is blank then load paid code to billed code
     C     SCBLCD        IFEQ      *BLANKS
     C                   MOVEL(P)  SCPRCD        DHBPRC
     C*** BEGIN ADD *** 04/26/04 **************************************
     C                   MOVEL(P)  SCCDDS        DHBPRD
     C*** END   ADD *** 04/26/04 **************************************
     C*
     C                   ELSE
     C                   MOVEL(P)  SCBLCD        DHBPRC
     C*** BEGIN ADD *** 04/26/04 **************************************
     C                   MOVEL(P)  XXBPRD        DHBPRD
     C*** END   ADD *** 04/26/04 **************************************
     C*
     C                   ENDIF                                                  SCBLCD = BLANKS
     C*** END ADD *** 02/18/00 ****************************************
     C*** BEG ADD *** 04/17/08 ****************************************
     C                   Z-ADD     SCFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDAT            7 0
     C*** END ADD *** 04/17/08 ****************************************
     C*** BEG ADD *** 03/23/99 ****************************************
     C*
     C*** Beg Add *** 05/19/15 ***************************************
     C* Do not adjust units if this is a true reconsideration
     C     svRCN         IfEq      EOrcn
     C*
     C* If anes or equipment code then retain screen units else set=1
     C     SCPRCD        IFGE      '00100'
     C     SCPRCD        ANDLE     '01999'
     C     SCPRCD        ORGE      'D0001'
     C     SCPRCD        ANDLE     'D9999'
     C*** BEG CHG *** 08/08/00 ****************************************
     C********** PR1A2     OREQ 'Y'
     C     SV1A2         OREQ      'Y'
     C*** END CHG *** 08/08/00 ****************************************
     C*** BEG ADD *** 06/08/99 ****************************************
     C* Include anesthesia by a surgeon for Oklahoma
     C     SCAREA        ANDNE     'AN'
     C     SCAST         ANDNE     'OK'
     C*
     C*** BEG CHG *** 08/08/00 ****************************************
     C********** PR1A2     OREQ 'Y'
     C     SV1A2         OREQ      'Y'
     C*** END CHG *** 08/08/00 ****************************************
     C     SCAREA        ANDEQ     'AN'
     C     SCAST         ANDEQ     'OK'
     C*** END ADD *** 06/08/99 ****************************************
     C*** BEG ADD *** 01/04/00 ****************************************
     C* Include mileage for ambulance travel, etc
     C*
     C*** BEG CHG *** 08/08/00 ****************************************
     C********** PR1A2     OREQ 'M'
     C     SV1A2         OREQ      'M'
     C*** END CHG *** 08/08/00 ****************************************
     C*** END ADD *** 01/04/00 ****************************************
     C*** BEG ADD *** 08/08/00 ****************************************
     C* Include rental for HCPCS codes
     C*
     C     SV1A2         OREQ      'R'
     C*** END ADD *** 08/08/00 ****************************************
     C*** BEG ADD *** 01/15/01 ****************************************
     C* Include hourly fees
     C*
     C     SV1A2         OREQ      'H'
     C*** END ADD *** 01/15/01 ****************************************
     C*
     C*** BEG ADD *** 08/31/01 ****************************************
     C* Include HCPCS codes that are not mileage,rental, or hourly but
     C* still don't want to break out because they are purchased in
     C* large quantities.
     C*
     C     SV1A2         OREQ      'U'
     C*** END ADD *** 08/31/01 ****************************************
      **** BEGIN ADD *** 04/17/08 *************************************
     C     ##SPLT        OREQ      'N'
      **** END ADD   *** 04/17/08 *************************************
     C*
     C                   Z-ADD     SCUNTS        DHUNTS
     C*
     C                   ELSE
     C                   Z-ADD     1             DHUNTS
     C*
     C                   ENDIF                                                  SCPRCD GE 00100
     C*
     C*** Beg Add *** 05/19/15 ***************************************
     C                   EndIf
     C*** End Add *** 05/19/15 ***************************************
     C*** END ADD *** 03/23/99 ****************************************
     C*** BEG ADD *** 09/19/01 ****************************************
     C*
     C* UNITS CANNOT BE = 0, DEFAULT TO 1.
     C*
     C     DHUNTS        IFEQ      0
     C                   Z-ADD     1             DHUNTS
     C                   ENDIF
     C*** END ADD *** 09/19/01 ****************************************
     C*
     C                   MOVEL(P)  SCAREA        DHAREA
     C*** BEGIN ADD *** 11/28/07 **************************************
     C                   MOVEL(P)  SCNPIR        DHNPIR
     C*** END   ADD *** 11/28/07 **************************************
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
     C*** BEG DEL *** 02/18/00 ****************************************
     C*** BEG ADD *** 06/15/99 ****************************************
     C******************** MOVELSCBLCD    DHBPRC
     C*** END ADD *** 06/15/99 ****************************************
     C*** END DEL *** 02/18/00 ****************************************
     C                   MOVEL     SCPRCD        CACPT4
     C*** BEGIN ADD *** 05/24/01 **************************************
     C                   MOVEL(P)  DHPRCD        CACODE
     C*** END   ADD *** 05/24/01 **************************************
     C*
     C                   SELECT
     C     SCMOD1        WHENNE    *BLANKS
     C                   MOVE      SCMOD1        CAPMOD
     C     SCMOD2        WHENNE    *BLANKS
     C                   MOVE      SCMOD2        CAPMOD
     C     SCMOD3        WHENNE    *BLANKS
     C                   MOVE      SCMOD3        CAPMOD
     C     SCMOD4        WHENNE    *BLANKS
     C                   MOVE      SCMOD4        CAPMOD
     C*** BEG ADD *** 08/04/99 ****************************************
     C                   OTHER
     C                   MOVE      *BLANKS       CAPMOD
     C*** END ADD *** 08/04/99 ****************************************
     C                   ENDSL
     C*
     C                   Z-ADD     SCUNTS        CAUNTS
     C*
     C********** SCPRCD    IFGE '00100'
     C********** SCPRCD    ANDLE'01999'
     C**********           MOVE CAUNTS    CAANMN
     C**********           ENDIF
     C*
     C                   Z-ADD     SCCHRG        CACHRG
     C*
     C* This code will load the correct charged amount if multiple
     C* units were entered for this line item.
     C*
     C*** BEG ADD *** 11/08/01 ****************************************
     C     @@MODE        IFEQ      'C'
     C     SCUNTS        ANDGT     1
     C     SV1A2         ANDEQ     *BLANK
     C     SVRCN         ANDEQ     EORCN
     C*** BEG ADD *** 04/17/08 ****************************************
     C     ##SPLT        ANDNE     'N'
     C*** END ADD *** 04/17/08 ****************************************
     C*
     C     @@MODE        OREQ      'C'
     C     SCUNTS        ANDGT     1
     C     SV1A2         ANDEQ     'T'
     C     SVRCN         ANDEQ     EORCN
     C*** BEG ADD *** 04/17/08 ****************************************
     C     ##SPLT        ANDNE     'N'
     C*** END ADD *** 04/17/08 ****************************************
     C*
     C                   Z-ADD     SVCHRG        CACHRG
     C*
     C                   ENDIF
     C*** END ADD *** 11/08/01 ****************************************
     C*
     C                   MOVEL     EOSS#         CASS#
     C                   MOVEL     EOACDT        CAACDT
     C                   Z-ADD     SCOTHR        CAOTHR
     C                   Z-ADD     SCOTH2        CAOTH2
     C                   Z-ADD     SCNTWK        CANDIS
     C**********           Z-ADDSCCHRG    CACHRG
     C                   Z-ADD     SCMRA         CAMRA
     C                   MOVE      SCZIPA        CAZIPA
     C                   MOVE(P)   SCAST         CAAST
     C                   MOVEL(P)  SCAREA        CAPART
     C*
     C                   Z-ADD     SCPAID        CAPAID
     C                   MOVEL(P)  SCCSCD        CACSCD
     C                   MOVEL(P)  SCOT1C        CAOT1C
     C                   MOVEL(P)  SCOT2C        CAOT2C
     C*
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
     C*
     C*
     C                   ENDSR
     C*
     C*** BEG ADD *** 11/08/01 ****************************************
     C*================================================================
     C* NXTSEQ - Find next available sequence numbers
     C*================================================================
     C*
     C     NXTSEQ        BEGSR
     C*
     C                   Z-ADD     *HIVAL        KKLIN#
     C*
     C     CAKY4         SETGT     PCA04
     C     CAKY3         READPE    PCA04                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C     CALIN#        ADD       1             ZZLIN#
     C*
     C                   ELSE
     C                   Z-ADD     1             ZZLIN#
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C*** BEGIN ADD *** 07/16/02 **************************************
     C                   Z-ADD     KKDSEQ        HDDSEQ
     C*** END   ADD *** 07/16/02 **************************************
     C                   Z-ADD     *HIVAL        KKDSEQ
     C*
     C     CAKY          SETGT     PCA01
     C     CAKY1         READPE    PCA01                                  99
     C*
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     1             ZZDSEQ
     C*
     C                   ELSE
     C     CADSEQ        ADD       1             ZZDSEQ
     C*
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C*** BEGIN ADD *** 07/16/02 **************************************
     C                   Z-ADD     HDDSEQ        KKDSEQ
     C*** END   ADD *** 07/16/02 **************************************
     C*
     C                   ENDSR
     C*** END ADD *** 11/08/01 ****************************************
     C*
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
     C* BEYOND 71 - 89 RESERVED BY COPYBOOK. THESE SHOULD BE SET OFF
     C* IN THE 'PGMEND' SUBROUTINE ALSO.
     C*
     C                   SETOFF                                       606162
     C                   SETOFF                                       636465
     C                   SETOFF                                       666758
     C                   SETOFF                                       703042
     C*** BEGIN CHG *** 10/31/05 **************************************
     C*****************    SETOF                     5728
     C                   SETOFF                                       572806
     C                   SETOFF                                       01
     C*** END   CHG *** 10/31/05 **************************************
     C*** BEGIN ADD *** 10/13/00 **************************************
     C*
     C*** INITIALIZE POSITION CURSOR INDICATORS
     C*
     C                   SETOFF                                       313233
     C                   SETOFF                                       343536
     C                   SETOFF                                       373839
     C                   SETOFF                                       4041
     C                   SETOFF                                       484950
     C                   SETOFF                                       535455
     C                   SETOFF                                       919293
     C                   SETOFF                                       949511
     C*** END   ADD *** 10/13/00 **************************************
     C*** BEGIN ADD *** 10/31/05 **************************************
     C                   SETOFF                                       05
     C*** END   ADD *** 10/31/05 **************************************
       //*** Beg add ***** 07/18/25 ************************************
       ernpir = *off;
       msnpir = *blanks;
       //*** End add ***** 07/18/25 ************************************
     C*
     C*** Beg Chg *** MC367 *** 11/19/12 ******************************
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
     C*** End Chg *** MC367 *** 11/19/12 ******************************
     C*** BEG DEL *** 05/12/09 ****************************************
     C*** BEGIN ADD *** 09/19/03 **************************************
     C***        *INKQ     CASEQ*ON       KQDO             F16=ADL Info
     C***        *INKY     CASEQ*ON       KYDO             F24=More Keys
     C*** END   ADD *** 09/19/03 **************************************
     C*** END DEL *** 05/12/09 ****************************************
     C*** MC367          ENDCS
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
     C                   SETOFF                                       703042
     C*** BEGIN CHG *** 10/31/05 **************************************
     C*****************    SETOF                     5728
     C                   SETOFF                                       572806
     C                   SETOFF                                       01
     C*** END   CHG *** 10/31/05 **************************************
     C*** BEG ADD *** 05/02/01 ****************************************
     C*
     C* Close state specific files that are overridden
     C*** BEG ADD *** 05/02/01 ****************************************
     C*** BEG ADD *** 09/19/02 ****************************************
     C*
     C* Delete overrides,
     C*
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*
     C                   CLOSE     FMZRL                                99
     C*
     C*** END ADD *** 09/19/02 ****************************************
     C                   CLOSE     FTABL
     C                   CLOSE     FMPRL
     C                   CLOSE     FTPSL
     C*** END ADD *** 05/02/01 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*** BEG ADD *** 05/02/01 ****************************************
     C                   OPEN      FTABL
     C                   OPEN      FMPRL
     C                   OPEN      FTPSL
     C*** END ADD *** 05/02/01 ****************************************
     C*
     C*** BEGIN ADD *** 08/15/00 ***********************************************
     C                   CALL      'SA009R'
     C                   PARM      *ZEROS        ##EOBD            3 0          EOMB DELETE SECURITY
     C                   PARM      *BLANKS       ##EOBI            1            EOMB INTERACIVE PROC
     C                   PARM      *BLANKS       ##ARIN            1            AUDIT/RECON INSTALLE
     C                   PARM      *BLANKS       ##ACIS            1            AUTO CHECK ISSUANCE
     C                   PARM      *BLANKS       ##PAY0            1            PRINT WCMC EOB PAY>0
     C                   PARM      *BLANKS       ##SDTS            1            ALLOW FUTURE SERV DT
     C*** END   ADD *** 08/15/00 ***********************************************
     C*
     C** Get standard screen headings
     C*
     C     @@RRN2        CHAIN(N)  PEO01                              99
     C                   MOVE      EOCO#         #1CO#
     C                   MOVE      EOSUB#        #1SUB#
     C                   MOVE      EOFND         #1FND
     C                   MOVE      EOFYR         #1FYR
     C                   MOVE      EOCASE        #1CASE
     C*** BEGIN DEL *** 08/07/02 **************************************
     C**********           MOVELEOZIPA    SCZIPA
     C*** END   DEL *** 08/07/02 **************************************
     C*** BEG ADD *** 04/11/00 ****************************************
     C                   MOVE      EOCO#         KKCO#
     C                   MOVE      EOSUB#        KKSUB#
     C                   MOVE      EOFND         KKFND
     C                   MOVE      EOFYR         KKFYR
     C                   MOVE      EOCASE        KKCASE
     C                   MOVE      EOEOB#        KKEOB#
     C                   MOVE      EORCN         KKRCN
     C*** END ADD *** 04/11/00 ****************************************
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
     C*****                PARM           #1L2
     C                   MOVEL     #1L1          SCHD3
     C*****                MOVEL#1L2      SCHD4
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
     C*
     C                   ENDIF                                                  *IN99
     C*** END ADD *** 12/08/00 ****************************************
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
     C*** BEGIN DEL *** 08/07/02 **************************************
     C**********           MOVELEOZIPA    SCZIPA
     C*** END   DEL *** 08/07/02 **************************************
     C**********           MOVE *HIVAL    KKLIN#
     C*** BEG DEL *** 02/23/99 ****************************************
     C********** @@RRN     CHAINDBRCDF              N99
     C**********           Z-ADDDHDSEQ    KKDSEQ
     C*** END DEL *** 02/23/99 ****************************************
     C*
     C*
     C*** BED ADD *** 02/23/99 ****************************************
     C*
     C* IF IN DELETE MODE, WE MUST CHAIN TO FDCAP RECORD TO GET THE
     C* DETAIL SEQUECE NUMBER AND THE RELATIVE RECORD NUMBER FOR THE
     C* PHYSICIAN DETAIL RECORD.  ELSE USE THE PASSED PHYSICIAN DETAIL
     C* RELEATIVE RECORD NUMBER TO GET THE DETAIL SEQUENCE NUMBER
     C*
     C     @@MODE        IFEQ      'E'
     C     @@RRN3        CHAIN(N)  PCA02                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     CADSEQ        KKDSEQ
     C     DHKY          CHAIN     PDH01                              99
     C                   Z-ADD     DBRRNC        @@RRN
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ELSE
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C                   Z-ADD     DHDSEQ        KKDSEQ
     C                   ENDIF                                                  @@MODE = 'E'
     C*** END ADD *** 02/23/99 ****************************************
     C*
     C* GET LAST LINE NUMBER USED FOR THE BILL
     C*
     C********** CAKY4     SETGTPCA04
     C********** CAKY3     REDPEPCA04                    99
     C********** *IN99     IFEQ *OFF
     C**********           Z-ADDCALIN#    XXLIN#
     C**********           ELSE
     C**********           Z-ADD0         XXLIN#
     C**********           ENDIF
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
     C**********           SETOF                     24
     C********** PDKY      CHAINPPD01                99
     C********** PDPARN    IFEQ 'P'
     C**********           MOVE EONTWK    KKNWCD    P
     C********** PDTYPE    IFEQ '4'
     C********** S4KY      CHAINPS402                99
     C********** *IN99     IFEQ *OFF
     C********** S4FSFL    ANDEQ'Y'
     C**********           SETON                     24
     C**********           ENDIF                           *IN99 = Off
     C**********           ELSE                            TYPE = '4'
     C********** PDTYPE    IFEQ '1'
     C********** S4KY      CHAINPS401                99
     C********** *IN99     IFEQ '0'
     C********** S4FSFL    ANDEQ'Y'
     C**********           SETON                     24
     C**********           ENDIF                           *IN99 = Off
     C**********           ENDIF                           TYPE = '1'
     C**********           ENDIF                           TYPE = '4'
     C**********           ENDIF                           PARN = 'P'
     C*** END ADD *** 07/23/99 ****************************************
     C*** END   ADD *** 01/11/00 **************************************
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
     C*** BEG ADD *** 09/28/04 ****************************************
     C*
     C* POS CURSOR TO ADJUDICATION CODE IN CHANGE MODE
     C*
     C     @@MODE        IFEQ      'C'
     C                   SETON                                        95
     C                   ENDIF
     C*** END ADD *** 09/28/04 ****************************************
     C*
     C* DIAGNOSIS CODES 2, 3 AND 4 WILL BE DISPLAYED ONLY IF EXISTS
     C* FAEHP
     C*
     C     EHKY          CHAIN(N)  PEH01                              99
     C     *IN99         IFEQ      *OFF
     C*
      *** Beg Add *** 02/04/14 ****************************************
      /free
       If EHicdind <> 'Y';
      /end-free
      *** END Add *** 02/04/14 ****************************************
     C     EHDIA2        IFNE      *BLANKS
     C                   SETON                                        43
     C                   ELSE
     C                   SETOFF                                       43
     C                   ENDIF                                                  EHDIA2 NE *BLANKS
     C*
     C     EHDIA3        IFNE      *BLANKS
     C                   SETON                                        44
     C                   ELSE
     C                   SETOFF                                       44
     C                   ENDIF                                                  EHDIA3 NE *BLANKS
     C*
     C     EHDIA4        IFNE      *BLANKS
     C                   SETON                                        45
     C                   ELSE
     C                   SETOFF                                       45
     C                   ENDIF                                                  EHDIA4 NE *BLANKS
      *** Beg Add *** 02/04/14 ****************************************
      /free
       Else;
          If EHicd1002 <> *blanks;
             *in43 = *on;
          Else;
             *in43 = *off;
          Endif;

          If EHicd1003 <> *blanks;
             *in44 = *on;
          Else;
             *in44 = *off;
          Endif;

          If EHicd1004 <> *blanks;
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
       If EHicdind <> 'Y';
      /end-free
      *** END Add *** 02/04/14 ****************************************
     C     @@MODE        IFEQ      'A'
     C     EHDIA1        ANDNE     *BLANKS
     C     EHDIA2        ANDEQ     *BLANKS
     C     EHDIA3        ANDEQ     *BLANKS
     C     EHDIA4        ANDEQ     *BLANKS
     C                   SETON                                        52
     C                   ELSE
     C                   SETOFF                                       52
     C                   ENDIF                                                  @@MODE = 'A'
      *** Beg Add *** 02/04/14 ****************************************
      /free
       Else;
          If @@mode = 'A' AND EHicd1001 <> *Blanks AND EHicd1002 = *Blanks
             AND EHicd1003 = *Blanks AND EHicd1004 =*Blanks;
             *IN52 = *on;
          Else;
             *IN52 = *off;
          Endif;
       Endif;
      /end-free
      *** End Add *** 02/04/14 ****************************************
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
     C********** ##KC      IFNE '1'
     C********** ##KL      ANDNE'1'
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
     C*** BEG ADD *** 09/19/02 ****************************************
     C*
     C* PERFORM FILE OVERRIDES
     C*
     C                   SETOFF                                       51
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL(P)  QCA(1)        QQCMD
     C                   MOVEL     EOAST         QQST
     C                   CALL      'QCMDEXC'
     C                   PARM                    QQCMD
     C                   PARM                    LENGTH           15 5
     C*
     C                   OPEN      FMZRL                                51
     C*
     C*** END ADD *** 09/19/02 ****************************************
     C*** BEG ADD *** 12/19/04 ****************************************
     C*
     C* GET PROV AUX INFO
     C*
     C     P1KY          CHAIN(N)  PP101                              99
     C*** END ADD *** 12/19/04 ****************************************
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
     C**********           ADD  CATOTC    TLCHRG
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
     C*** BEGIN ADD *** 08/21/02 **************************************
     C     @@MODE        IFNE      'E'
     C*** END   ADD *** 08/21/02 **************************************
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
     C*** BEGIN ADD *** 08/21/02 **************************************
     C*
     C* IF THE LINE WAS NOT OVERIDDEN THEN CLEAR EOCRDO
     C* WHICH WILL ALLOW BILL TO GO INTO CODEREVIEW.
     C*
     C                   MOVEL(P)  SCCSCD        KKKEY
     C     TBKY          CHAIN     PCS01                              99
     C     *IN99         IFEQ      *ON
     C     CSOVRD        ORNE      'Y'
     C                   MOVEL(P)  *BLANKS       EOCRDO
     C                   ENDIF
     C*
     C                   ELSE
     C                   MOVEL(P)  *BLANKS       EOCRDO
     C                   ENDIF
     C*
     C                   TIME                    DSDTTM
     C*** END   ADD *** 08/21/02 **************************************
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
     C* IF ADJUDICATION CODE IS NE 1, CLEAR CAMRAA. THIS WILL ALLOW THE
     C* OVERRIDE PROCESSING TO BE PERFORMED IN THE ADJUDICTION PROGRAM
     C*
     C*** COMMENTED OUT DUE TO PROBS W/ FSMRA *** 02/02/00 ************
     C********** SCCSCD    IFNE '1'
     C**********           Z-ADD0         CATOTA
     C**********           Z-ADD0         CAMRAA
     C**********           ENDIF                           SCCSCD = 1
     C*** COMMENTED OUT DUE TO PROBS W/ FSMRA *** 02/02/00 ************
     C*
     C                   MOVEL     XXRCFL        CARCFL
     C                   MOVEL     Q1USER        CAUSER
     C                   MOVEL     Q1PGM         CAPGM
     C                   Z-ADD     DSTIME        CATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C*
     C*** BEG ADD *** 11/11/02 ****************************************
     C     CSOVRD        IFNE      'Y'
     C     CAMRA         ANDEQ     0
     C     CANDIS        ANDEQ     0
     C     CAOTHR        ANDEQ     0
     C     CAOTH2        ANDEQ     0
     C     EOCRNE        ANDEQ     'Y'
     C*
     C                   Z-ADD     0             CAPAID
     C*
     C                   ENDIF
     C*** END ADD *** 11/11/02 ****************************************
     C*
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
     C*
     C                   EXSR      MVSCDB
     C*
     C* This code will calculate units and charged amounts if multiple
     C* units were entered for this line item.
     C*
     C*** BEG ADD *** 11/08/01 ****************************************
     C*
     C     SCUNTS        IFGT      1
     C     SV1A2         ANDEQ     *BLANK
     C     SVRCN         ANDEQ     EORCN
     C*** BEG ADD *** 04/17/08 ****************************************
     C     ##SPLT        ANDNE     'N'
     C*** END ADD *** 04/17/08 ****************************************
     C*
     C     SCUNTS        ORGT      1
     C     SV1A2         ANDEQ     'T'
     C     SVRCN         ANDEQ     EORCN
     C*** BEG ADD *** 04/17/08 ****************************************
     C     ##SPLT        ANDNE     'N'
     C*** END ADD *** 04/17/08 ****************************************
     C*
     C     SCCHRG        DIV       SCUNTS        SVCHRG
     C     SCCHRG        SUB       SVCHRG        SCCHRG
     C                   Z-ADD     1             CAUNTS
     C                   Z-ADD     1             DHUNTS
     C*
     C*
     C                   ENDIF
     C*
     C*** END ADD *** 11/08/01 ****************************************
     C*
     C                   MOVEL     Q1USER        DHUSER
     C                   MOVEL     Q1PGM         DHPGM
     C                   Z-ADD     DSTIME        DHTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DHDT
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
     C**********           SETON                     90
     C*
     C* This code will write multiple line items if multiple
     C* units were entered for this line item.
     C*
     C*** BEG ADD *** 11/08/01 ****************************************
     C*
     C     SVRCN         IFEQ      EORCN
     C     SCUNTS        ANDGT     1
     C     SV1A2         ANDEQ     *BLANK
      **** BEGIN ADD *** 04/17/08 *************************************
     C     ##SPLT        ANDNE     'N'
      **** END ADD   *** 04/17/08 *************************************
     C*
     C     SVRCN         OREQ      EORCN
     C     SCUNTS        ANDGT     1
     C     SV1A2         ANDEQ     'T'
      **** BEGIN ADD *** 04/17/08 *************************************
     C     ##SPLT        ANDNE     'N'
      **** END ADD   *** 04/17/08 *************************************
     C*
     C                   SUB       1             SCUNTS
     C*
     C* Load next available line# and sequence#
     C*
     C                   Z-ADD     SVLIN#        SCLIN#
     C                   Z-ADD     SVDSEQ        SCSEQ#
     C*
     C                   EXSR      WRITER
     C*
     C                   ENDIF
     C*
     C*** END ADD *** 11/08/01 ****************************************
     C*
     C                   SETOFF                                       90
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
      *
     C*** BEG ADD *** 08/08/07 ****************************************
     C                   MOVE      *HIVAL        KKMSEQ
     C*** BEG DEL *** 09/14/11 ****************************************
     C***************           @@MODE    IFEQ 'C'
     C****************                    Z-ADDCAMSEQ    HDMSEQ
     C***************                     ELSE
     C*** END DEL *** 09/14/11 ****************************************
     C     CAKYM1        SETGT     PCA05
     C     CAKYM2        READPE    PCA05                                  99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     1             HDMSEQ
     C                   ELSE
     C     CAMSQ#        ADD       1             HDMSEQ
     C                   ENDIF
     C*** BEG DEL *** 09/14/11 ****************************************
     C*****************    ENDIF
     C*** END DEL *** 09/14/11 ****************************************
     C*** END ADD *** 08/08/07 ****************************************
      *
     C*** BEG ADD *** 04/17/08 ****************************************
     C                   Z-ADD     SCFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDAT            7 0
     C*** END ADD *** 04/17/08 ****************************************
      *
     C     SCPRCD        IFGE      '00100'
     C     SCPRCD        ANDLE     '01999'
     C     SCPRCD        ORGE      'D0001'
     C     SCPRCD        ANDLE     'D9999'
     C     SCUNTS        ORLE      1
     C*** BEG ADD *** 03/23/99 ****************************************
     C*** BEG CHG *** 08/08/00 ****************************************
     C********** PR1A2     OREQ 'Y'
     C     SV1A2         OREQ      'Y'
     C*** END CHG *** 08/08/00 ****************************************
     C*** END ADD *** 03/23/99 ****************************************
     C*** BEG ADD *** 06/08/99 ****************************************
     C* Include anesthesia by a surgeon for Oklahoma
     C     SCAREA        ANDNE     'AN'
     C     SCAST         ANDNE     'OK'
     C*
     C*** BEG CHG *** 08/08/00 ****************************************
     C********** PR1A2     OREQ 'Y'
     C     SV1A2         OREQ      'Y'
     C*** END CHG *** 08/08/00 ****************************************
     C     SCAREA        ANDEQ     'AN'
     C     SCAST         ANDEQ     'OK'
     C*** END ADD *** 06/08/99 ****************************************
     C********** SCUNTS    ANDLE1
     C*** BEG ADD *** 01/04/00 ****************************************
     C* Include mileage for ambulance travel, ect.
     C*
     C*** BEG CHG *** 08/08/00 ****************************************
     C********** PR1A2     OREQ 'M'
     C     SV1A2         OREQ      'M'
     C*** END CHG *** 08/08/00 ****************************************
     C*** END ADD *** 01/04/00 ****************************************
     C*** BEG ADD *** 08/08/00 ****************************************
     C* Include rental for HCPCS codes
     C*
     C     SV1A2         OREQ      'R'
     C*** END ADD *** 08/08/00 ****************************************
     C*** BEG ADD *** 01/15/01 ****************************************
     C* Include hourly fees
     C*
     C     SV1A2         OREQ      'H'
     C*** END ADD *** 01/15/01 ****************************************
     C*** BEG ADD *** 08/31/01 ****************************************
     C* Include HCPCS codes that are not mileage,rental, or hourly but
     C* still don't want to break out because they are purchased in
     C* large quantities.
     C*
     C     SV1A2         OREQ      'U'
      *
      **** BEGIN ADD *** 04/17/08 *************************************
     C     ##SPLT        OREQ      'N'
      **** END ADD   *** 04/17/08 *************************************
     C*** END ADD *** 08/31/01 ****************************************
     C*
     C                   CLEAR                   DBRCDF
     C                   CLEAR                   PCA02
     C                   TIME                    DSDTTM
     C                   EXSR      MVSCDB
     C                   Z-ADD     ##CO#         DHCO#
     C                   Z-ADD     EOSUB#        DHSUB#
     C                   Z-ADD     EOFND         DHFND
     C                   Z-ADD     EOFYR         DHFYR
     C                   Z-ADD     EOCASE        DHCASE
     C                   Z-ADD     SCSEQ#        DHDSEQ
     C                   Z-ADD     EOEOB#        DHEOB#
     C                   MOVEL     EOSS#         DHSS#
     C                   Z-ADD     EOACDT        DHACDT
     C                   MOVE      'A'           DHSTAT
     C                   MOVEL     Q1USER        DHUSER
     C                   MOVEL     Q1USER        DHIUSR
     C                   MOVEL     Q1PGM         DHPGM
     C                   MOVEL     Q1PGM         DHIPGM
     C                   Z-ADD     DSTIME        DHTIME
     C                   Z-ADD     DSTIME        DHITIM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DHDT
     C                   MOVEL     SRDTO         DHIDT
     C                   WRITE     DBRCDF
     C*
     C                   Z-ADD     ##CO#         CACO#
     C                   Z-ADD     EOSUB#        CASUB#
     C                   Z-ADD     EOFND         CAFND
     C                   Z-ADD     EOFYR         CAFYR
     C                   Z-ADD     EOCASE        CACASE
     C                   MOVEL     Q1USER        CAUSER
     C                   MOVEL     Q1USER        CAIUSR
     C                   MOVEL     Q1PGM         CAPGM
     C                   MOVEL     Q1PGM         CAIPGM
     C*** BEG ADD *** 02/04/00 ****************************************
     C                   Z-ADD     DSTIME        CAITIM
     C*** END ADD *** 02/04/00 ****************************************
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
     C                   MOVE      XXRCFL        CARCFL
     C*
     C*** BEGIN CHG *** 04/08/02 **************************************
     C********** EOBTYP    IFEQ 'N'
     C**********           MOVE '1'       CAPRTY
     C**********           ENDIF
     C*
     C********** EOBTYP    IFEQ 'H'
     C**********           MOVE '4'       CAPRTY
     C**********           ENDIF
     C                   MOVEL(P)  '1'           CAPRTY
     C*** END   CHG *** 04/08/02 **************************************
     C*
     C*** BEG ADD ***** 08/08/07 **************************************
     C                   Z-ADD     HDMSEQ        CAMSEQ
     C*** END ADD ***** 08/08/07 **************************************
     C                   WRITE     PCA02
     C*
     C                   ELSE
     C*
     C* WRITE MULTIPLE RECORDS BASED ON SCUNTS
     C*
     C     SCCHRG        DIV       SCUNTS        XXCHRG            9 2
     C                   MVR                     REMAIN            9 2
     C                   Z-ADD     SCUNTS        XXUNTS
     C*
     C*** BEG ADD *** 09/16/99 ****************************************
     C     CSOVRD        IFEQ      'Y'
     C     CSOVRP        ANDEQ     *ZEROS
     C     SCUNTS        ANDGT     1
     C     SCMRA         DIV       SCUNTS        XXMRA             9 2
     C                   MVR                     XRMRA             9 2
     C     SCOTHR        DIV       SCUNTS        XXOTHR            9 2
     C                   MVR                     XROTHR            9 2
     C     SCOTH2        DIV       SCUNTS        XXOTH2            9 2
     C                   MVR                     XROTH2            9 2
     C     SCNTWK        DIV       SCUNTS        XXNTWK            9 2
     C                   MVR                     XRNTWK            9 2
     C                   ENDIF                                                  CSOVRD = 'Y'
     C*** END ADD *** 09/16/99 ****************************************
     C                   CLEAR                   DBRCDF
     C                   CLEAR                   PCA02
     C                   TIME                    DSDTTM
     C                   EXSR      MVSCDB
     C                   Z-ADD     ##CO#         DHCO#
     C                   Z-ADD     EOSUB#        DHSUB#
     C                   Z-ADD     EOFND         DHFND
     C                   Z-ADD     EOFYR         DHFYR
     C                   Z-ADD     EOCASE        DHCASE
     C                   Z-ADD     SCSEQ#        DHDSEQ
     C                   Z-ADD     EOEOB#        DHEOB#
     C                   MOVEL     EOSS#         DHSS#
     C                   Z-ADD     EOACDT        DHACDT
     C                   MOVE      'A'           DHSTAT
     C                   MOVEL     Q1USER        DHUSER
     C                   MOVEL     Q1USER        DHIUSR
     C                   MOVEL     Q1PGM         DHPGM
     C                   MOVEL     Q1PGM         DHIPGM
     C                   Z-ADD     DSTIME        DHTIME
     C                   Z-ADD     DSTIME        DHITIM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DHDT
     C                   MOVEL     SRDTO         DHIDT
     C                   WRITE     DBRCDF
     C*
     C                   Z-ADD     XXCHRG        CACHRG
     C*** BEG ADD *** 04/13/99 ****************************************
     C                   ADD       REMAIN        CACHRG
     C*** END ADD *** 04/13/99 ****************************************
     C*** BEG ADD *** 09/16/99 ****************************************
     C     CSOVRD        IFEQ      'Y'
     C     CSOVRP        ANDEQ     *ZEROS
     C     SCUNTS        ANDGT     1
     C     XXMRA         ADD       XRMRA         CAMRA
     C     XXOTHR        ADD       XROTHR        CAOTHR
     C     XXOTH2        ADD       XROTH2        CAOTH2
     C     XXNTWK        ADD       XRNTWK        CANDIS
     C     CACHRG        SUB       CAMRA         CAPAID
     C                   SUB       CAOTHR        CAPAID
     C                   SUB       CAOTH2        CAPAID
     C                   SUB       CANDIS        CAPAID
     C                   ENDIF                                                  CSOBRD = 'Y'
     C*** END ADD *** 09/16/99 ****************************************
     C*** BEG CHG *** 01/10/00 ****************************************
     C**********           Z-ADD0         CAUNTS
     C                   Z-ADD     1             CAUNTS
     C*** END CHG *** 01/10/00 ****************************************
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
     C*** BEG ADD *** 02/04/00 ****************************************
     C                   Z-ADD     DSTIME        CAITIM
     C*** END ADD *** 02/04/00 ****************************************
     C                   MOVEL     SRDTO         CADT
     C                   MOVEL     SRDTO         CAIDT
     C*** BEG ADD *** 01/10/00 ****************************************
     C                   MOVEL     SRDTO         CARDAT
     C*** END ADD *** 01/10/00 ****************************************
     C                   Z-ADD     SCLIN#        CALIN#
     C                   Z-ADD     SCSEQ#        CADSEQ
     C                   Z-ADD     EOEOB#        CAEOB#
     C                   Z-ADD     EORCN         CARCN
     C                   Z-ADD     EOFEIN        CAFEIN
     C                   Z-ADD     EOMOD         CAMOD
     C                   MOVE      XXRCFL        CARCFL
     C*
     C*** BEGIN CHG *** 04/08/02 **************************************
     C********** EOBTYP    IFEQ 'N'
     C**********           MOVE '1'       CAPRTY
     C**********           ENDIF
     C*
     C********** EOBTYP    IFEQ 'H'
     C**********           MOVE '4'       CAPRTY
     C**********           ENDIF
     C                   MOVEL(P)  '1'           CAPRTY
     C*** END   CHG *** 04/08/02 **************************************
     C*** BEG ADD *** 11/11/02 ****************************************
     C     CSOVRD        IFNE      'Y'
     C     CAMRA         ANDEQ     0
     C     CANDIS        ANDEQ     0
     C     CAOTHR        ANDEQ     0
     C     CAOTH2        ANDEQ     0
     C     EOCRNE        ANDEQ     'Y'
     C*
     C                   Z-ADD     0             CAPAID
     C*
     C                   ENDIF
     C*** END ADD *** 11/11/02 ****************************************
     C*
     C*** BEG ADD ***** 08/08/07 **************************************
     C                   Z-ADD     HDMSEQ        CAMSEQ
     C*** END ADD ***** 08/08/07 **************************************
     C                   WRITE     PCA02
     C                   SUB       1             XXUNTS
     C     XXUNTS        DOWGT     0
     C*
     C*** BEG DEL *** 11/08/01 ****************************************
     C**********
     C**********           Z-ADD*HIVAL    KKLIN#
     C********** CAKY4     SETGTPCA04
     C********** CAKY3     REDPEPCA04                    99
     C********** *IN99     IFEQ *OFF
     C********** CALIN#    ADD  1         ZZLIN#
     C**********           ELSE
     C**********           Z-ADD1         ZZLIN#
     C**********           ENDIF                           *IN99 = *OFF
     C**********
     C**********           Z-ADD*HIVAL    KKDSEQ
     C********** CAKY      SETGTPCA01
     C********** CAKY1     REDPEPCA01                    99
     C********** *IN99     IFEQ *ON
     C**********           Z-ADD1         ZZDSEQ
     C**********           ELSE
     C********** CADSEQ    ADD  1         ZZDSEQ
     C**********           ENDIF                           *IN99 IFEQ *ON
     C*** END DEL *** 11/08/01 ****************************************
     C*
     C*** BEG ADD *** 11/08/01 ****************************************
     C                   EXSR      NXTSEQ
     C*** END ADD *** 11/08/01 ****************************************
     C*
     C                   CLEAR                   DBRCDF
     C                   CLEAR                   PCA02
     C                   TIME                    DSDTTM
     C                   EXSR      MVSCDB
     C                   Z-ADD     ##CO#         DHCO#
     C                   Z-ADD     EOSUB#        DHSUB#
     C                   Z-ADD     EOFND         DHFND
     C                   Z-ADD     EOFYR         DHFYR
     C                   Z-ADD     EOCASE        DHCASE
     C                   Z-ADD     ZZDSEQ        DHDSEQ
     C                   Z-ADD     EOEOB#        DHEOB#
     C                   MOVEL     EOSS#         DHSS#
     C                   Z-ADD     EOACDT        DHACDT
     C                   MOVE      'A'           DHSTAT
     C                   MOVEL     Q1USER        DHUSER
     C                   MOVEL     Q1USER        DHIUSR
     C                   MOVEL     Q1PGM         DHPGM
     C                   MOVEL     Q1PGM         DHIPGM
     C                   Z-ADD     DSTIME        DHTIME
     C                   Z-ADD     DSTIME        DHITIM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DHDT
     C                   MOVEL     SRDTO         DHIDT
     C                   WRITE     DBRCDF
     C*
     C                   Z-ADD     XXCHRG        CACHRG
     C*** BEG ADD *** 09/16/99 ****************************************
     C     CSOVRD        IFEQ      'Y'
     C     CSOVRP        ANDEQ     *ZEROS
     C     SCUNTS        ANDGT     1
     C                   Z-ADD     XXMRA         CAMRA
     C                   Z-ADD     XXOTHR        CAOTHR
     C                   Z-ADD     XXOTH2        CAOTH2
     C                   Z-ADD     XXNTWK        CANDIS
     C     CACHRG        SUB       CAMRA         CAPAID
     C                   SUB       CAOTHR        CAPAID
     C                   SUB       CAOTH2        CAPAID
     C                   SUB       CANDIS        CAPAID
     C                   ENDIF                                                  CSOVRD = 'Y'
     C*** END ADD *** 09/16/99 ****************************************
     C*** BEG CHG *** 01/10/00 ****************************************
     C**********           Z-ADD0         CAUNTS
     C                   Z-ADD     1             CAUNTS
     C*** END CHG *** 01/10/00 ****************************************
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
     C*** BEG ADD *** 02/04/00 ****************************************
     C                   Z-ADD     DSTIME        CAITIM
     C*** END ADD *** 02/04/00 ****************************************
     C                   MOVEL     SRDTO         CADT
     C                   MOVEL     SRDTO         CAIDT
     C*** BEG ADD *** 01/10/00 ****************************************
     C                   MOVEL     SRDTO         CARDAT
     C*** END ADD *** 01/10/00 ****************************************
     C                   Z-ADD     ZZLIN#        CALIN#
     C                   Z-ADD     ZZDSEQ        CADSEQ
     C                   Z-ADD     EOEOB#        CAEOB#
     C                   Z-ADD     EORCN         CARCN
     C                   Z-ADD     EOFEIN        CAFEIN
     C                   Z-ADD     EOMOD         CAMOD
     C                   MOVE      XXRCFL        CARCFL
     C*
     C*** BEGIN CHG *** 04/08/02 **************************************
     C********** EOBTYP    IFEQ 'N'
     C**********           MOVE '1'       CAPRTY
     C**********           ENDIF
     C*
     C********** EOBTYP    IFEQ 'H'
     C**********           MOVE '4'       CAPRTY
     C**********           ENDIF
     C                   MOVEL(P)  '1'           CAPRTY
     C*** END   CHG *** 04/08/02 **************************************
     C*
     C*** BEG ADD *** 11/11/02 ****************************************
     C     CSOVRD        IFNE      'Y'
     C     CAMRA         ANDEQ     0
     C     CANDIS        ANDEQ     0
     C     CAOTHR        ANDEQ     0
     C     CAOTH2        ANDEQ     0
     C     EOCRNE        ANDEQ     'Y'
     C*
     C                   Z-ADD     0             CAPAID
     C*
     C                   ENDIF
     C*** END ADD *** 11/11/02 ****************************************
     C*
     C*** BEG ADD ***** 08/08/07 **************************************
     C                   Z-ADD     HDMSEQ        CAMSEQ
     C*** END ADD ***** 08/08/07 **************************************
     C                   WRITE     PCA02
     C*** BEG ADD *** 09/16/99 ****************************************
     C* WRITE SAME EOB CODE RECORD FOR LINE ITEMS WHEN SCUNTS > 1
     C*
     C     CSOVRD        IFEQ      'Y'
     C     CSOVRP        ANDEQ     *ZEROS
     C*** BEGIN CHG ***************************************************
     C********** XXEOB     ANDNE*BLANKS
     C     XXEOB         IFNE      *BLANKS
     C     XXNOT1        ORNE      *BLANKS
     C*** END   CHG *** 03/26/01 **************************************
     C                   CLEAR                   PDE02
     C                   Z-ADD     ##CO#         DECO#
     C                   Z-ADD     EOSUB#        DESUB#
     C                   Z-ADD     EOFND         DEFND
     C                   Z-ADD     EOFYR         DEFYR
     C                   Z-ADD     EOCASE        DECASE
     C                   MOVEL(P)  EOSS#         DESS#
     C                   Z-ADD     EOACDT        DEACDT
     C                   Z-ADD     EOEOB#        DEEOB#
     C                   Z-ADD     ZZDSEQ        DEDSEQ
     C                   Z-ADD     1             DESEQ#
     C*** Beg Add *** 07/18/23 **************************************
     C****               MOVEL(P)  XXEOB         DEEOB
     C                   MOVEL(P)  XXEOB         DEEOBCD
     C*** End Add *** 07/18/23 **************************************
     C*** BEGIN ADD *** 03/26/01 **************************************
     C     XXNOT1        IFEQ      *BLANKS
     C*** END   ADD *** 03/26/01 **************************************
     C                   MOVEL(P)  XXEOB         KKKEY
     C     TBKY          CHAIN     PEB01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  EBDESC        DENOT1
     C*** BEGIN ADD ****** 05/04/11 ************************************
     C                   MOVEL(P)  EBDSC2        DENOT2
     C                   MOVEL(P)  EBDSC3        DENOT3
     C                   MOVEL(P)  EBDSC4        DENOT4
     C*** END   ADD ****** 05/04/11 ************************************
     C                   ELSE
     C                   MOVE      *BLANKS       DENOT1
     C*** BEGIN ADD ****** 05/04/11 ************************************
     C                   MOVE      *BLANKS       DENOT2
     C                   MOVE      *BLANKS       DENOT3
     C                   MOVE      *BLANKS       DENOT4
     C*** END   ADD ****** 05/04/11 ************************************
     C                   ENDIF                                                  *IN99 = *OFF
     C*** BEGIN ADD *** 03/26/01 **************************************
     C                   ELSE
     C                   MOVEL(P)  XXNOT1        DENOT1
     C                   MOVEL(P)  XXNOT2        DENOT2
     C*** BEGIN ADD ****** 05/04/11 ************************************
     C                   MOVEL(P)  XXNOT3        DENOT3
     C                   MOVEL(P)  XXNOT4        DENOT4
     C*** END   ADD ****** 05/04/11 ************************************
     C                   ENDIF
     C*** END   ADD *** 03/26/01 **************************************
     C                   MOVE      'A'           DESTAT
     C                   MOVEL(P)  Q1USER        DEUSER
     C                   MOVEL(P)  Q1USER        DEIUSR
     C                   MOVEL(P)  Q1PGM         DEPGM
     C                   MOVEL(P)  Q1PGM         DEIPGM
     C                   Z-ADD     DSTIME        DETIME
     C                   Z-ADD     DSTIME        DEITIM
     C                   MOVEL     SRDTO         DEDT
     C                   MOVEL     SRDTO         DEIDT
     C                   MOVE      'U'           DEESRC
     C                   Z-ADD     EORCN         DERCN
     C                   WRITE     PDE02
     C*** BEGIN ADD *** 03/26/01 **************************************
     C                   ENDIF
     C*** END   ADD *** 03/26/01 **************************************
     C                   ENDIF                                                  CSOVRD = 'Y'
     C*** END ADD *** 09/16/99 ****************************************
     C                   SUB       1             XXUNTS
     C*
     C                   ENDDO
     C*
     C                   ENDIF                                                  SCPRCD GE 00100
     C*
     C* UPDATE FAEOP
     C*
     C                   EXSR      UFAEOP
     C*
     C* UPDATE TOTAL CHARGES
     C*
     C                   EXSR      SMCHRG
     C*
     C                   FEOD      FDDHP
     C                   SETON                                        41
     C                   ENDSR
     OPMG01     E            RPMG01
     OPDE01     E            RPDE01
**  (QCMDEX)
OVRDBF FILE(FMZRL) TOFILE(*LIBL/FMZRL  )
OVRDBF FILE(FMZRP) TOFILE(*LIBL/FMZRP  )
DLTOVR FILE(FMZRL)
DLTOVR FILE(FMZRP)
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
