     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**sy266_ovr*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H DEBUG DATEDIT(*MDY/)
     H*****************************************************************
     H* (c) 2010 StoneRiver, Inc. All rights reserved.
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE F0473R - Pharmacy Detail Information EDI
     H*****************************************************************
     H*
     H* F0473R - Pharmacy Detail Information EDI
     H* 03/08/00  C:53  Project: KY010
     H*                 REMOVE EDITS ON PROTECTD FIELDS FOR KY
     H*                 EDI CHANGE PROCESS.
     H*****************************************************************
     H** REVISIONS:
     H*
     H* 09/01/16 C:309 Project : FL091 Phase 2
     H*                Added a new field for NDC Code Secondary
     H*
     H* 03/11/10 C:227 PROJECT FL083
     H*                  Changed check for NDC for FL rev E
     H*
     H* 10/04/08 C:180 SUPPORT 65159
     H*                  Fourth position of FL Phy lic can be blank
     H*
     H*  09/27/07  C:189  SUPP 59718
     H*                   'PAYMENT TYPE'  Field added for Edit
     H*
     H*  09/25/07  C:180  SUPP 59201
     H*                   Validate SCLIC# based on following formats:
     H*
     H*                   AANNNNNNNNNNN
     H*                   (OR)
     H*                   AAANNNNNNNNNN
     H*                   (OR)
     H*                   AAAANNNNNNNNN
     H*
     H*                   Left justify and space fill to end of field
     H*                   Key alpha prefix and numeric digits of
     H*                   license number
     H*                   Must be valid value
     H*                   DO NOT zero pad numeric portion
     H*
     H*  08/31/07 C:079   SUPPORT: 59756
     H*                   REMOVE EDITING ON EOB CODES
     H*
     H*  08/30/07 C:194   SUPPORT: 59756
     H*                   REMOVE EDIT ON FIELDS DAYS SUPPLY, NEW
     H*                   REFILL, NDC, DAW IF FDDPP/DPRXPT = S
     H*
     H*  08/16/07 C:194   SUPPORT: 59404
     H*                   EOB CODE DESCRIPTION CUT OFF
     H*
     H*  08/04/07 C:079   Project: FL067
     H*                   Take out edit checking for FS on system
     H*
     H*  05/03/07 C:182   Support: 58104
     H*                   Remove Edits on Filled Date as now this is a
     H*                   protected field.
     H*
     H*  03/14/07 C:182   Project: FL067
     H*                   1. Made 'SCHCPC' required when payemnt
     H*                      type = 'S'(Supplies)
     H*                   2. Made 'SCPRIN' required when payemnt
     H*                      type = 'S'(Supplies)
     H*                   3. Changed Fld Heading for 'Cert' to be
     H*                      ' DAW'
     H*
     H*  10/18/06  C:181  Project: 10MJD
     H*                   ENABLE SEARCH & EDIT ON NDC CODE.
     H*
     H*  04/21/06  C:079  SUPPORT: 52278
     H*                   1. DO NOT UPDATE CERTAIN FDCAP FIELDS FOR EDI
     H*                      CORRECTION
     H*
     H*  04/07/06  C:163  Project: 48MWQ
     H*                   FL LIC# DB FOR RX BILLS
     H*
     H*  04/08/02  C:062  PROJECT:FS236
     H*                   POPULATE CAPRTY WITH EOBTYP.
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
     H*  11/12/99 C:56  MC152
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
     H*                                                               *
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
     H*                Messages to subroutin DLTPRE.                  *
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      29      Add mode                                       **
     H*****************************************************************
     H*
     FF0473FFL  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFDDPP     UF A E             DISK
     F                                     RENAME(PDP01:DBRCDF)
     F*
     F*  EOMB Transaction File
     FFAEOP     UF   E             DISK
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
     F*
     F*  EOMB Detail EOB/Notes Transaction File
     F*** BEG CHG *** 02/23/99 ****************************************
     F**********FDDEL   IF  E           K        DISK
     FFDDEL     UF   E           K DISK
     F*** END CHG *** 02/23/99 ****************************************
     F*** BEG ADD *** 10/18/16 ****************************************
     F*FDDDL   IF  E           K        DISK
     FFDDEP     O    E             DISK
     F                                     RENAME(PDE01:PDE02)
     F*** END ADD *** 10/18/16 ****************************************
     F*
     F*  Fee Schedule Claims Status Code File
     FFTCSP     IF   E             DISK
     F                                     RENAME(PCS01:PCS02)
     F*
     F*  WCMC NDC Drug Code File
     FFMRXP     IF   E             DISK
     F                                     RENAME(PRX01:PRX02)
     FFMRXL     IF   E           K DISK
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
     F*  WCMC EOMB Pharmacy Bill Header File
     F*****FAEPL   IF  E           K        DISK
     F*
     F*  Logical over all Tabel Files
     FFTABL     IF   E           K DISK
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
     FFDDPL     IF   E           K DISK
     F*** BEG ADD *** 02/23/99 ****************************************
     F                                     INFDS(DBFDC)
     F*** END ADD *** 02/23/99 ****************************************
     F* Generic Table File
     FFTGTL     IF   E           K DISK
     F*** BEGIN ADD *** 12/23/05 **************************************
     F* Generic Table File
     FFTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PGT02)
     F*** END   ADD *** 12/23/05 **************************************
     F*
     F* State FS Rules/Pgm Cross Reference File
     F*** BEG DEL *** 08/04/07 ****************************************
     F**** FDRUL1  IF  E           K        DISK
     F*** END DEL *** 08/04/07 ****************************************
     F*** BEG DEL ***** 05/03/07 **************************************
     F***** SSYLF   IF  E           K        DISK
     F*****       SPC01                             KIGNORE
     F*****       SWC01                             KIGNORE
     F*****       SRC01                             KIGNORE
     F*****       SDR01                             KIGNORE
     F*****       SBC01                             KIGNORE
     F*****       SCS01                             KIGNORE
     F*****       SGL01                             KIGNORE
     F*****       SP201                             KIGNORE
     F*** END DEL ***** 05/03/07 **************************************
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
     F*** BEG ADD *** 04/07/06 ****************************************
     F*
     F*  State Code Table File
     FSTSTL     IF   E           K DISK
     F*
     F*  FL Provider Lic# Database
     FFMPLLFL   IF   E           K DISK
     F*
     F*** END ADD *** 04/07/06 ****************************************
     F*** BEG ADD *** 03/14/07 ****************************************
     FFMPRL     IF   E           K DISK
     F*** END ADD *** 03/14/07 ****************************************
      /COPY SCOPYMT01D
     D*** BEG ADD *** 09/25/07 ****************************************
     D @A              S              1    DIM(26) CTDATA PERRCD(26)
     D @N              S              1    DIM(10) CTDATA PERRCD(10)
     D LL              S              1    DIM(11)
     D*** END ADD *** 09/25/07 ****************************************
     D*
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
     D*** BEG ADD *** 02/23/99 ****************************************
     D DBFDC           DS
     D  DBRRNC               397    400B 0
     D*** END ADD *** 02/23/99 ****************************************
     D*** BEGIN ADD *** 12/23/05 **************************************
     D*
     D* LICENSE NUMBER DATA STRUCTURE
     D*
     D                 DS
     D  SCLIC#                 1     13
     D  SCLIC1                 1      1
     D  SCLIC2                 2      2
     D  SCLIC3                 3      3
     D  SCLIC4                 4      4
     D*** BEG ADD *** 09/25/07 ****************************************
     D  SCLICA                 3     13
     D  SCLICB                 4     13
     D  SCLICC                 5     13
     D*** END ADD *** 09/25/07 ****************************************
     D*****¢¢¢¢ BEGIN DELETE ****** 09/25/07 **************************
     D****¢¢¢                                 5   9 SCLIC5
     D****¢¢¢                                 4   6 SCLIC6
     D****¢¢¢                                 3   9 SCLIC7
     D*** END   ADD *** 12/23/05 **************************************
     D***PY SCOPYMT01I
     D*
     D                 DS
     D  TMTOS                  1      5
     D  HLKEY                  1      2
     D  HDKEY                  3      5
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Pharmacy Detail Info-
     D                                     rmation')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)
     D*** BEG ADD *** 10/18/06 ****************************************
     D NT1             C                   CONST('Paid Code ')
     D NT3             C                   CONST(' Was Changed For EDI-
     D                                      Correction')
     D*** BEG ADD *** 03/11/10 ****************************************
     D XXNDC           C                   CONST('COMPD000000')
     D*** END ADD *** 03/11/10 ****************************************
     D*****************************************************************
     D                 DS
     D  XXNOT1                 1     55
     D  XXNT1                  1     10
     D  XXNT2                 11     21
     D  XXNT3                 22     55

     I*** END ADD *** 10/18/06 ****************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          FDDPP
     C                   PARM                    @@RRN2            9 0          FAEOP
     C                   PARM                    @@RRN3            9 0          FDCAP
     C**************       PARM           @@NWCD  5        DEL 03/08/00
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
     C*  KEY LIST FOR TABLE FILE
     C*
     C     TBKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKKEY
     C*
     C*  KEY LIST FOR PROCEDURE CODE FILE
     C*
     C     RXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKNDC
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
     C* KEY LIST FOR EOMB PHARMACY HEADER FILE
     C*
     C********** EPKY      KLIST
     C**********           KFLD           ##CO#
     C**********           KFLD           KKSUB#
     C**********           KFLD           KKFND
     C**********           KFLD           KKFYR
     C**********           KFLD           KKCASE
     C**********           KFLD           KKEOB#
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
     C*** BEGIN ADD *** 12/23/05 **************************************
     C     GTKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID
     C*
     C* FL Provider Lic#
     C*
     C     PLKY          KLIST
     C*** BEGIN CHG *** 04/07/06 **************************************
     C******************** KFLD           KKLIC#  9
     C                   KFLD                    KKLIC#           13
     C*** END   CHG *** 04/07/06 **************************************
     C*** END   ADD *** 12/23/05 **************************************
     C*** BEG DEL *** 08/04/07 ****************************************
     C********
     C**Key*list for State FS Rules/Pgm Cross Reference File
     C********
     C********   RUKY1     KLIST
     C********             KFLD           KKST
     C********             KFLD           KKATYP
     C********             KFLD           KKEFDT
     C********
     C********   RUKY2     KLIST
     C********             KFLD           KKST
     C********             KFLD           KKATYP
     C*** END DEL *** 08/04/07 ****************************************
     C*
     C     BEKY          KLIST
     C                   KFLD                    EOAST
     C                   KFLD                    EOSYS
     C                   KFLD                    EOBTYP
     C                   KFLD                    EOSTYP
     C*
     C*  FS BASE CONTROL FILE
     C*
     C     FBKY          KLIST
     C                   KFLD                    XSKEY             2
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
     C*** BEG ADD *** 04/07/06 ****************************************
     C*
     C** Key List for State Code Table File
     C*
     C     STKY          KLIST
     C                   KFLD                    ##KEY
     C*** END ADD *** 04/07/06 ****************************************
     C*** BEG ADD *** 10/18/06 ****************************************
     C*
     C*  Key List for Detail File
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
     C     DDKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C                   KFLD                    KKSEQ#
     C*** END ADD *** 10/18/06 ****************************************
     C*
     C*** BEG ADD *** 03/14/07 ****************************************
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
     C*** END ADD *** 03/14/07 ****************************************
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
     C     *LIKE         DEFINE    DPNDC         HLNDC
     C     *LIKE         DEFINE    DPNDC         KKNDC
     C     *LIKE         DEFINE    RXMEAS        HLRXMM
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY1
     C*** BEG DEL *** 08/04/07 ****************************************
     C*********
     C*********  *LIKE     DEFN RUST      KKST
     C*********  *LIKE     DEFN RUATYP    KKATYP
     C*********  *LIKE     DEFN RUEFDT    KKEFDT
     C*** END DEL *** 08/04/07 ****************************************
     C     *LIKE         DEFINE    EOCRDT        HLSYDT
     C     *LIKE         DEFINE    CAPAID        XXTOTL
     C     *LIKE         DEFINE    DEEOB         XXEOB
     C*** BEGIN ADD *** 04/07/06 **************************************
     C     *LIKE         DEFINE    STKEY         ##KEY
     C*** END   ADD *** 04/07/06 **************************************
     C*** BEG ADD *** 10/18/06 ****************************************
     C     *LIKE         DEFINE    EOCO#         KKCO#
     C     *LIKE         DEFINE    CADSEQ        KKSEQ#
     C*** END ADD *** 10/18/06 ****************************************
     C*** BEG   ADD *** 03/14/07 **************************************
     C     *LIKE         DEFINE    PRCPT4        KKPRCD
     C     *LIKE         DEFINE    PREFDT        KKFDAT
     C*** END   ADD *** 03/14/07 **************************************
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
     C                   Z-ADD     SCRXFD        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCRXFD
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
     C* DELETE THE DETAIL AMOUNT TRANS RECORDS FOR LINE ITEM
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
     C*
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
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLSYDT
     C*
    C***** Beg Add *** 09/01/16 **************************************
     C                   Eval      msNDC2  = *Blanks
     C                   Eval      ms1NDC2 = *Blanks
     C                   Eval      ms2NDC2 = *Blanks
     C                   Eval      ms3NDC2 = *Blanks
     C                   Eval      msDesc2 = *Blanks
     C                   Eval      erNDC2  = *Off
     C                   Eval      er1NDC2 = *Off
     C                   Eval      er2NDC2 = *Off
     C                   Eval      er3NDC2 = *Off
     C                   Eval      erDesc2 = *Off
    C***** End Add *** 09/01/16 **************************************

     C*** BEG DEL *** 03/08/00 *****************************
     C**Validate Claim Status (Adjudication) code
     C*******
     C*******    SCCSCD    IFEQ *BLANKS
     C*******              SETON                     9071
     C*******              GOTO CK1END
     C*******              ENDIF                           SCCSCD IFEQ BLANKS
     C*******
     C*******              MOVELSCCSCD    KKKEY     P
     C*******    TBKY      CHAINPCS01                99
     C*******    *IN99     IFEQ *ON
     C*******              SETON                     9072
     C*******              GOTO CK1END
     C*******              ENDIF                           *IN99 IFEQ *ON
     C*** END DEL *** 03/08/00 *****************************
     C*** BEG DEL *** 05/03/07 *****************************
     C*
     C* Validate Date of Service
     C*
     C********   SCRXFD    IFEQ *ZEROS
     C********             SETON                     9073
     C********             GOTO CK1END
     C********             ENDIF                           SCRXFD IFEQ ZEROS
     C********
     C********             Z-ADDSCRXFD    SRDTI
     C********             MOVE *BLANKS   SRDTO
     C********             MOVE *BLANKS   SRERR
     C********             CALL 'SRCVTDAT'DATCHK
     C********   SRERR     IFEQ '1'
     C********             SETON                     9074
     C********             GOTO CK1END
     C********             ELSE
     C********             MOVELSRDTO     SCRXFD
     C********             ENDIF
     C********
     C* DATE OF SERVICE CANNOT BE LESS THAN ACCIDENT DATE
     C********
     C********             Z-ADDSCRXFD    SRDTI
     C********             MOVE *BLANKS   SRDTO
     C********             MOVE *BLANKS   SRERR
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     HLFDAT
     C********
     C********   HLFDAT    IFLT EOACDT
     C********             SETON                     9075
     C********             GOTO CK1END
     C********             ENDIF                           HLFDAT IFLT HLACDT
     C********
     C* BASE THE EDIT OF PREVENTING FUTURE SERVICE DATES ON CONTROL
     C* FLAG SFBPF/FBAL04 POSTION 8 (BLANK = EDIT, Y = ALLOW FUTURE DATES)
     C*
     C********             MOVE ' '       XXFSDF  1
     C********             MOVEL'FB'      XSKEY
     C********   FBKY      CHAINSFB01                99
     C********   *IN99     IFEQ *OFF
     C********   1         SUBSTFBAL04:8  XXFSDF
     C********   XXFSDF    IFNE 'Y'
     C********
     C********   HLFDAT    IFGT HLSYDT
     C********             SETON                     9089
     C********             GOTO CK1END
     C********             ENDIF                           HLFDAT GT HLSYDT
     C********
     C********             ENDIF                           XXFSDF = *BLANKS
     C********             ENDIF                           *IN99 = *OFF
     C*** END DEL *** 05/03/07 *****************************
     C*** BEGIN ADD *** 12/23/05 **************************************
     C*
     C* Validate RX#
     C*
     C*
    C*** BEGIN DEL *** 08/16/07 **************************************
     C*********  SCRX#     IFEQ *BLANKS
     C*********  *IN46     ANDEQ*OFF
     C*********            SETON                     9004
     C*********            GOTO CK1END
     C*********            ENDIF                           SCRXDY IFEQ ZEROS
    C*** END   DEL *** 08/16/07 **************************************
     C*
     C* Validate Number of Days Supply
     C*
    C*** BEGIN ADD *** 08/30/07 **************************************
    C*** BEGIN CHG *** 09/27/07 **************************************
     C********   DPRXPT    IFEQ 'D'
     C     SCRXPT        IFEQ      'D'
    C*** END   CHG *** 09/27/07 **************************************
    C*** END   ADD *** 08/30/07 **************************************
     C     SCRXDY        IFEQ      *ZEROS
     C*** BEGIN ADD *** 09/27/07 **************************************
     C     SCRXPT        ANDEQ     'D'
     C*** END   ADD *** 09/27/07 **************************************
     C                   SETON                                        9091
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCRXDY IFEQ ZEROS
    C*** BEGIN ADD *** 08/30/07 **************************************
     C                   ENDIF                                                  DPRXPT IFEQ ZEROS
    C*** END   ADD *** 08/16/07 **************************************
     C*
     C* Validate Prescription, New or Refill Flag
     C*
    C*** BEGIN ADD *** 08/30/07 **************************************
    C*** BEGIN CHG *** 09/27/07 **************************************
     C**************    DPRXPT    IFEQ 'D'
     C     SCRXPT        IFEQ      'D'
    C*** END   CHG *** 09/27/07 **************************************
    C*** END   ADD *** 08/30/07 **************************************
     C     SCNWRF        IFEQ      *BLANKS
     C*** BEGIN ADD *** 09/27/07 **************************************
     C     SCRXPT        ANDEQ     'D'
     C*** END   ADD *** 09/27/07 **************************************
     C                   SETON                                        9092
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCNWRF IFEQ BLANKS
     C*
     C     SCNWRF        IFNE      *BLANK
     C     *IN46         ANDEQ     *OFF
     C                   MOVEL(P)  SCNWRF        KKKEY1
     C                   MOVEL(P)  'FLFI'        KKTBID
     C     GTKY          CHAIN     PGT01                              99
     C   99              SETON                                        9093
     C   99              GOTO      CK1END
     C                   ENDIF                                                  SCNWRF IFNE BLANK
     C*
    C*** BEGIN ADD *** 08/30/07 **************************************
     C                   ELSE                                                               BLANK
     C*
     C                   MOVEL(P)  SCNWRF        KKKEY1
     C                   MOVEL(P)  'FLFI'        KKTBID
     C     GTKY          CHAIN     PGT01                              99
     C*
     C                   ENDIF                                                  DPRXPT IFEQ ZEROS
    C*** END   ADD *** 08/16/07 **************************************
     C*
     C* Validate Certification
     C*
    C*** BEGIN ADD *** 08/30/07 **************************************
    C*** BEGIN CHG *** 09/27/07 **************************************
     C***********   DPRXPT    IFEQ 'D'
     C     SCRXPT        IFEQ      'D'
    C*** END   CHG *** 09/27/07 **************************************
    C*** END   ADD *** 08/30/07 **************************************
     C     SCCERT        IFEQ      *BLANK
     C*** BEGIN ADD *** 09/27/07 **************************************
     C     SCRXPT        ANDEQ     'D'
     C*** END   ADD *** 09/27/07 **************************************
     C                   SETON                                        9094
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCCERT IFEQ ZEROS
     C*
     C     SCCERT        IFNE      *BLANK
     C     *IN46         ANDEQ     *OFF
     C                   MOVEL(P)  SCCERT        KKKEY1
     C                   MOVEL(P)  'FLCT'        KKTBID
     C     GTKY          CHAIN     PGT01                              99
     C   99              SETON                                        9095
     C   99              GOTO      CK1END
     C                   ENDIF                                                  SCCERT IFNE BLANK
     C*
    C*** BEGIN ADD *** 08/30/07 **************************************
     C                   ELSE
     C*
     C                   MOVEL(P)  SCCERT        KKKEY1
     C                   MOVEL(P)  'FLCT'        KKTBID
     C     GTKY          CHAIN     PGT01                              99
     C*
     C                   ENDIF                                                  DPRXPT
    C*** END   ADD *** 08/30/07 **************************************
     C*
     C*** END   ADD *** 12/23/05 **************************************
     C*
     C*** BEG DEL *** 03/08/00 *****************************
     C**VALIDATE NDC DRUG CODE (REQUIRED)
     C*******
     C*******    SCNDC     IFEQ *BLANKS
     C*******              SETON                     9083
     C*******              GOTO CK1END
     C*******              ENDIF                           SCNDC  IFEQ BLANKS
     C*******
     C*******              MOVELSCNDC     KKNDC     P
     C*******    RXKY      CHAINPRX01                99
     C*******    *IN99     IFEQ *ON
     C*******              SETON                     9084
     C*******              GOTO CK1END
     C*******              ENDIF                           *IN99 IFEQ *ON
     C*******
     C*******    RXSTAT    IFEQ 'I'
     C*******              SETON                     9076
     C*******              GOTO CK1END
     C*******              ENDIF                           RXSTAT = 'I'
     C*** END DEL *** 03/08/00 *****************************
     C*** BEG ADD *** 10/18/06 *****************************
     C*VALIDATE NDC DRUG CODE (REQUIRED)
     C*
    C*** BEGIN ADD *** 09/27/07 **************************************
     C     SCRXPT        IFEQ      'S'
     C     SCNDC         ANDNE     ' '
     C*
     C                   SETON                                        9061
     C                   GOTO      CK1END
     C                   ENDIF
    C*** END   ADD *** 09/27/07 **************************************
    C*** BEGIN ADD *** 08/30/07 **************************************
    C*** BEGIN CHG *** 09/27/07 **************************************
     C***************   DPRXPT    IFEQ 'D'
     C     SCRXPT        IFEQ      'D'
    C*** END   CHG *** 09/27/07 **************************************
    C*** END   ADD *** 08/30/07 **************************************
     C     SCNDC         IFEQ      *BLANKS
     C                   SETON                                        9083
     C                   GOTO      CK1END
     C                   ENDIF                                                   SCNDC IFEQ BLANKS
     C*
     C                   MOVEL(P)  SCNDC         KKNDC
     C*** BEG ADD *** 03/11/10 ****************************************
     C     EOREVI        IFNE      'E'
     C     SCNDC         ANDNE     XXNDC
     C*** END ADD *** 03/11/10 ****************************************
     C     RXKY          CHAIN     PRX01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9084
     C                   GOTO      CK1END
     C                   ENDIF                                                   *IN99 IFEQ *ON
     C*** BEG ADD *** 03/11/10 ****************************************
     C                   ENDIF                                                  EOREVI NE 'E'
     C*** END ADD *** 03/11/10 ****************************************
     C*
    C*** BEGIN ADD *** 08/30/07 **************************************
     C                   ELSE
     C*
     C                   MOVEL(P)  SCNDC         KKNDC
     C*** BEG ADD *** 03/11/10 ****************************************
     C     EOREVI        IFNE      'E'
     C     SCNDC         ANDNE     XXNDC
     C*** END ADD *** 03/11/10 ****************************************
     C     RXKY          CHAIN     PRX01                              99
     C*** BEG ADD *** 03/11/10 ****************************************
     C                   ENDIF                                                  EOREVI NE 'E'
     C*** END ADD *** 03/11/10 ****************************************
     C*
     C                   ENDIF                                                   DPRXPT
    C*** END   ADD *** 08/30/07 **************************************
     C*** BEG ADD *** 03/11/10 ****************************************
     C     EOREVI        IFNE      'E'
     C     SCNDC         ANDNE     XXNDC
     C*** END ADD *** 03/11/10 ****************************************
     C     RXSTAT        IFEQ      'I'
     C                   SETON                                        9076
     C                   GOTO      CK1END
     C                   ENDIF                                                    RXSTAT = 'I'
     C*** BEG ADD *** 03/11/10 ****************************************
     C                   ENDIF                                                  EOREVI NE 'E'
     C*** END ADD *** 03/11/10 ****************************************
     C*** END ADD *** 10/18/06 *****************************
     C*** BEGIN ADD *** 12/23/05 **************************************

    C***** Beg Add *** 09/01/16 **************************************
     C*
     C     SCRXPT        IFEQ      'S'
     C     SCNDC2        ANDNE     ' '
     C*
     C                   SETON                                        90
     C                   Eval      ms3NDC2 = 'MCR0801'
     C                   Eval      er3NDC2 = *On
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C     SCRXPT        IFEQ      'D'
     C*    SCNDC2        IFEQ      *BLANKS
     C*                  SETON                                        90
     C*                  Eval      msNDC2 = 'MCR0503'
     C*                  Eval      erNDC2 = *On
     C*                  GOTO      CK1END
     C*                  ENDIF                                                   SCNDC2 IFEQ BLANKS
     C*
     C     SCNDC2        IFNE      *BLANKS
     C                   MOVEL(P)  SCNDC2        KKNDC
     C     EOREVI        IFNE      'E'
     C     SCNDC2        ANDNE     XXNDC
     C     RXKY          CHAIN     PRX01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        90
     C                   Eval      ms1NDC2= 'MCR0759'
     C                   Eval      er1NDC2= *On
     C                   GOTO      CK1END
     C                   ENDIF                                                   *IN99 IFEQ *ON
     C                   ENDIF                                                   EOREVI NE 'E'
     C                   ENDIF                                                   IF SCNDC2 <> ' '
     C                   ELSE
     C                   MOVEL(P)  SCNDC2        KKNDC
     C     EOREVI        IFNE      'E'
     C     SCNDC2        ANDNE     XXNDC
     C     RXKY          CHAIN     PRX01                              99
     C                   ENDIF                                                   EOREVI NE 'E'
     C                   ENDIF                                                   SCRXPT
     C*
     C     EOREVI        IFNE      'E'
     C     SCNDC2        ANDNE     XXNDC
     C     RXSTAT        IFEQ      'I'
     C                   SETON                                        90
     C                   Eval      ms2NDC2 = 'MCR2387'
     C                   Eval      er2NDC2 = *On
     C                   GOTO      CK1END
     C                   ENDIF                                                    RXSTAT = 'I'
     C                   ENDIF                                                  EOREVI NE 'E'
    C***** End Add *** 09/01/16 **************************************

     C* Validate Prescribing physicians license number
     C*
     C     SCLIC#        IFEQ      *BLANKS
     C                   SETON                                        9096
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C     SCLIC#        IFNE      *BLANKS
     C*** BEGIN ADD *** 04/07/06 **************************************
     C     XXCLT#        IFEQ      '048'
     C*
     C*  Check to see if the Provider DB installed
     C*
     C                   MOVEL     *BLANKS       ##KEY
     C                   MOVEL     'FL'          ##KEY
     C     STKY          CHAIN     PST01                              99
     C     *IN99         IFEQ      *OFF
     C     STBS01        ANDEQ     'Y'
     C*
     C* edit against the Prov Lic# database
     C*
     C                   MOVEL     SCLIC#        KKLIC#
     C     PLKY          CHAIN     PPL01                              99
     C     *IN99         IFEQ      *ON
     C     SCFLG7        IFNE      'Y'
     C     *IN90         DOUEQ     *OFF
     C                   SETOFF                                       90
     C                   MOVEL     *BLANKS       SCFLG7
     C                   EXFMT     WDW01
     C     *INKL         IFEQ      *OFF
     C     SCFLG7        IFNE      'Y'
     C     SCFLG7        ANDNE     'N'
     C                   SETON                                        3290
     C                   ENDIF                                                  SCFLG7<>'Y'
     C                   ENDIF                                                  *INKL=*OFF
     C                   ENDDO                                                  *IN90 DOUEQ*OFF
     C     *INKL         IFEQ      *ON
     C                   MOVEL     *BLANKS       SCFLG7
     C                   SETOFF                                       41
     C                   SETON                                        45
     C                   SETON                                        90
     C                   SETOFF                                       KL
     C                   GOTO      CK1END
     C                   ELSE
     C     SCFLG7        IFEQ      'N'
     C                   SETON                                        90
     C                   MOVEL     *BLANKS       SCFLG7
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     'Y'           SCFLG7
     C                   ENDIF                                                  SCFLG7<>'Y'
     C                   ELSE
     C                   MOVEL     *BLANKS       SCFLG7
     C                   ENDIF                                                  *IN99
     C                   MOVEL     *ON           UPDATE            1
     C*
     C                   ENDIF                                                  STBS01='Y'
     C*
     C                   ELSE                                                   XXCLT#='048'
     C*
     C*** END   ADD *** 04/07/06 **************************************
     C*
      *** BEG DELETE ***** 09/25/07 *¢*¢*¢*¢*¢*¢*¢*¢*¢*¢*¢*¢*¢*¢*¢*¢*¢*
     C* FIRST TWO CHARACTERS OF LICENSE NUMBER MUST BE ALPHA
     C*
     C****¢¢¢    SCLIC1    IFLT 'A'
     C****¢¢¢    SCLIC1    ORGT 'Z'
     C****¢¢¢              SETON                     9097
     C****¢¢¢              GOTO CK1END
     C****¢¢¢              ENDIF                           SCLIC1 < 'A'
     C****¢¢¢
     C****¢¢¢    SCLIC2    IFLT 'A'
     C****¢¢¢    SCLIC2    ORGT 'Z'
     C****¢¢¢              SETON                     9097
     C****¢¢¢              GOTO CK1END
     C****¢¢¢              ENDIF
     C****¢¢¢
     C* 3rd position can be alpha or numeric
     C****¢¢¢              TESTN          SCLIC3     60
     C****¢¢¢
     C****¢¢¢    *IN60     IFEQ '0'
     C****¢¢¢
     C****¢¢¢    SCLIC3    IFLT 'A'
     C****¢¢¢    SCLIC3    ORGT 'Z'
     C****¢¢¢
     C****¢¢¢              SETON                     9006
     C****¢¢¢              GOTO CK1END
     C****¢¢¢
     C****¢¢¢              ENDIF                           SCLIC3 IFLT
     C****¢¢¢
     C****¢¢¢              ENDIF                           *IN60 = 0
     C* 4TH position can be alpha or numeric
     C****¢¢¢
     C****¢¢¢              TESTN          SCLIC4     60
     C****¢¢¢
     C****¢¢¢    *IN60     IFEQ '0'
     C****¢¢¢
     C****¢¢¢    SCLIC4    IFLT 'A'
     C****¢¢¢    SCLIC4    ORGT 'Z'
     C****¢¢¢
     C****¢¢¢              SETON                     9007
     C****¢¢¢              GOTO CK1END
     C****¢¢¢
     C****¢¢¢              ENDIF                           SCLIC4 IFLT
     C****¢¢¢
     C****¢¢¢              ENDIF                           *IN60 = 0
     C****¢¢¢
     C****¢¢¢              SELEC
     C****¢¢¢    SCLIC3    WHGE 'A'
     C****¢¢¢    SCLIC3    ANDLE'Z'
     C****¢¢¢    SCLIC4    ANDGE'A'
     C****¢¢¢    SCLIC4    ANDLE'Z'
     C****¢¢¢              TESTN          SCLIC5     60
     C****¢¢¢    *IN60     IFEQ '0'
     C****¢¢¢              SETON                     9005
     C****¢¢¢              GOTO CK1END
     C****¢¢¢              END
     C****¢¢¢    SCLIC3    WHGE 'A'
     C****¢¢¢    SCLIC3    ANDLE'Z'
     C****¢¢¢    SCLIC4    ANDGE'0'
     C****¢¢¢    SCLIC4    ANDLE'9'
     C****¢¢¢              TESTN          SCLIC6     60
     C****¢¢¢    *IN60     IFEQ '0'
     C****¢¢¢              SETON                     9016
     C****¢¢¢              GOTO CK1END
     C****¢¢¢              END
     C****¢¢¢
     C****¢¢¢    SCLIC3    WHGE '0'
     C****¢¢¢    SCLIC3    ANDLE'9'
     C****¢¢¢    SCLIC4    ANDGE'0'
     C****¢¢¢    SCLIC4    ANDLE'9'
     C****¢¢¢              TESTN          SCLIC7     60
     C****¢¢¢
     C****¢¢¢    SCLIC3    WHGE '0'
     C****¢¢¢    SCLIC3    ANDLE'9'
     C****¢¢¢    SCLIC4    ANDGE'A'
     C****¢¢¢    SCLIC4    ANDLE'Z'
     C****¢¢¢              TESTN          SCLIC5     60
     C****¢¢¢
     C****¢¢¢              OTHER
     C****¢¢¢              TESTN          SCLIC5     60
     C****¢¢¢
     C****¢¢¢              ENDSL
     C****¢¢¢    *IN60     IFEQ '0'
     C****¢¢¢              SETON                     9015
     C****¢¢¢              GOTO CK1END
     C****¢¢¢              END
     C*** BEGIN ADD *** 04/07/06 **************************************
     C****** BEG ADD ******* 09/25/07 *****¢¢¢¢*****¢¢¢¢*****¢¢¢¢*****
     C                   MOVE      *ZEROS        LCNT              5 0
     C*
     C* FIRST TWO CHARACTERS OF LICENSE NUMBER MUST BE ALPHA
     C*
     C     SCLIC1        LOOKUP    @A                                     60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        9097
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C                   ADD       1             LCNT
      *
     C     SCLIC2        LOOKUP    @A                                     60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        9097
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C                   ADD       1             LCNT              5 0
      *
     C* 3rd position can be alpha or numeric
      *
     C     SCLIC3        LOOKUP    @A                                     60
     C     *IN60         IFEQ      *ON
     C                   ADD       1             LCNT              5 0
     C                   ELSE
     C                   MOVE      *IN40         SV40              1
     C     SCLIC3        LOOKUP    @N                                     40
     C     *IN40         IFEQ      *OFF
     C                   SETON                                        9015
     C                   GOTO      CK1END
     C                   ENDIF
     C                   MOVE      SV40          *IN40
     C                   ENDIF
      *
     C*
     C* 4TH position can be alpha or numeric
     C*
     C***** BEGIN ADD *****  10/04/08 *********************************
     C     SCLIC4        IFGT      *BLANKS
     C***** END ADD *******  10/04/08 *********************************
      *
     C     SCLIC4        LOOKUP    @A                                     60
     C     *IN60         IFEQ      *ON
      *
     C     LCNT          IFEQ      3
     C                   ADD       1             LCNT              5 0
     C                   ELSE
     C                   SETON                                        9015
     C                   GOTO      CK1END
     C                   ENDIF
      *
     C                   ELSE
     C                   MOVE      *IN40         SV40              1
     C     SCLIC4        LOOKUP    @N                                     40
     C     *IN40         IFEQ      *OFF
     C                   SETON                                        9015
     C                   GOTO      CK1END
     C                   ENDIF
     C                   MOVE      SV40          *IN40
     C                   ENDIF
     C***** BEGIN ADD *****  10/04/08 *********************************
     C                   ENDIF
     C***** END ADD *******  10/04/08 *********************************
      *
     C                   CLEAR                   LL
      *
     C                   SELECT
     C     LCNT          WHENEQ    2
     C                   MOVEA     SCLICA        LL
     C     LCNT          WHENEQ    3
     C                   MOVEA     SCLICB        LL
     C     LCNT          WHENEQ    4
     C                   MOVEA     SCLICC        LL
     C                   ENDSL
      *
     C     13            SUB       LCNT          MAX               3 0
     C                   Z-ADD     MAX           IX                3 0
      * Find 1st non-blank
     C                   DO        MAX
     C     LL(IX)        IFNE      *BLANKS
     C                   LEAVE
     C                   ENDIF
     C                   SUB       1             IX
     C                   ENDDO
      * Check numeric part
      *
      *
     C     LL(1)         IFGT      *ZERO
      *
     C                   DO        IX            IY                3 0
     C     LL(IY)        LOOKUP    @N                                     60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        9016
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
      *
     C                   ELSE
     C                   SETON                                        9016
     C                   ENDIF
      *
     C     *IN90         IFEQ      *ON
     C                   GOTO      CK1END
     C                   ENDIF
     C****** END ADD ******* 09/25/07 *****¢¢¢¢*****¢¢¢¢*****¢¢¢¢*****
     C*
     C                   ENDIF                                                  XXCLT#='048'
     C*** END   ADD *** 04/07/06 **************************************
     C*
     C                   ENDIF                                                  SCLIC# <> *BLANKS
     C*** END   ADD *** 12/23/05 **************************************
     C*****************************************************************
     C*
     C*** BEGIN ADD *** 09/27/07 **************************************
     C*
     C* Validate Rx Payment Type
     C*
     C     SCRXPT        IFEQ      *BLANK
     C                   SETON                                        9002
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCRXPT=BLANKS
     C*
     C     SCRXPT        IFNE      *BLANK
     C                   MOVEL(P)  SCRXPT        KKKEY1
     C                   MOVEL(P)  'FLPT'        KKTBID
     C     GTKY          CHAIN     PGT01                              99
     C   99              SETON                                        9030
     C   99              GOTO      CK1END
     C                   ENDIF                                                  SCRXPT<>BLANK
     C*
     C*
     C*** END   ADD *** 09/27/07 **************************************
     C* Validate HCPCS CODE
    C*** BEGIN ADD *** 09/27/07 **************************************
     C     SCRXPT        IFEQ      'D'
     C     SCHCPC        ANDNE     ' '
     C*
     C                   SETON                                        9032
     C                   GOTO      CK1END
     C                   ENDIF
    C*** END   ADD *** 09/27/07 **************************************
     C*
    C*** BEG   CHG *** 09/27/07 **************************************
     C***********    DPRXPT    IFEQ 'S'
     C     SCRXPT        IFEQ      'S'
    C*** END   CHG *** 09/27/07 **************************************
     C*
     C     SCHCPC        IFEQ      ' '
     C*
     C                   SETON                                        9034
     C                   GOTO      CK1END
     C*
     C                   ELSE                                                               BLANKS
     C*
     C                   MOVEL(P)  SCHCPC        KKPRCD
     C                   Z-ADD     SCRXFD        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         KKFDAT
     C*
     C* VALIDATE CPT4 CODE
     C*
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     SCHCPC        IFGE      'A0001'
     C     SCHCPC        ANDLE     'Z9999'
     C*
     C                   ELSE
     C*
     C                   SETON                                        9035
     C                   GOTO      CK1END
     C*
     C                   ENDIF                                                  SCHCPC>='A0001'
     C*
     C                   ELSE
     C*
     C                   SETON                                        9035
     C                   GOTO      CK1END
     C*
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDIF                                                  SCHCPC = ' '
     C*
     C                   ELSE
     C*
     C     SCHCPC        IFNE      ' '
     C*
     C                   SETON                                        9032
     C                   GOTO      CK1END
     C*
     C                   ENDIF                                                  SCHCHC <>' '
     C*
     C                   ENDIF                                                  SCRXPT = 'S'
     C*                    ENDIF                           SCHCPC IFEQ BLANKS
     C*
     C*          SCHCPC    IFLT 'A0001'
     C*          SCHCPC    ORGT 'Z9999'
     C*
     C*                    ELSE
     C*
     C*                    SETON                     9035
     C*                    GOTO CK1END
     C*
     C*                    ENDIF                           SCHCPC      K
     C*
     C*                    ELSE
     C*
     C*          SCHCPC    IFNE *BLANK
     C*
     C*                    SETON                     9040
     C*                    GOTO CK1END
     C*                    ENDIF                           SCHCPC IFNE BLANKS
     C*
     C*                    ENDIF                           DPRXPT = 'S'
     C*
     C* Validate Purchace/Rent Flag
     C*
    C*** BEG   CHG *** 09/27/07 **************************************
     C************   DPRXPT    IFEQ 'S'
     C     SCRXPT        IFEQ      'S'
    C*** END   CHG *** 09/27/07 **************************************
     C*
     C     SCPRIN        IFEQ      ' '
     C*
     C                   SETON                                        9037
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCPRIN IFEQ BLANKS
     C*
     C     SCPRIN        IFNE      *BLANK
     C*
     C                   MOVEL(P)  SCPRIN        KKKEY1
     C                   MOVEL(P)  'FLPR'        KKTBID
     C     GTKY          CHAIN     PGT01                              99
     C   99              SETON                                        9038
     C   99              GOTO      CK1END
     C                   ENDIF                                                  SCPRIN<>BLANK
     C*
     C                   ELSE
     C*
     C     SCPRIN        IFNE      *BLANK
     C*
     C                   SETON                                        9054
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCPRIN IFNE BLANKS
     C*
     C                   ENDIF                                                  DPRXPT = 'S'
     C*
     C*** BEG DEL *** 08/04/07 ****************************************
     C**************************************************************** BLANKS
     C********
     C**VERIFY THAT FEE SCHEDULE IS ON SYSTEM FOR SERVICE DATE
     C********
     C********             Z-ADDHLFDAT    KKEFDT
     C********             MOVE SCAST     KKST
     C********             MOVE 'X'       KKATYP
     C********   RUKY1     SETGTPRU01
     C********   RUKY2     REDPEPRU01                    99
     C********   *IN99     IFEQ *ON
     C********             SETON                     9082
     C********             GOTO CK1END
     C********             ENDIF
     C*** END DEL *** 08/04/07 ****************************************
     C*
     C* CHARGES ARE REQUIRED
     C*
     C*** BEG ADD *** 04/01/99 ****************************************
     C*
     C*** BEG DEL *** 03/08/00 *****************************
     C* IF ADJUDICATION CODE IS OVERRIDE TYPE, ALLOW ZERO CHARGE AMOUNT
     C*
     C     CSOVRD        IFNE      'Y'
     C*** END ADD *** 04/01/99 ****************************************
     C     SCCHRG        IFEQ      *ZEROS
     C                   SETON                                        9077
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCCHRG IFEQ ZEROS
     C*** BEG ADD *** 04/01/99 ****************************************
     C                   ENDIF                                                  CSOBRD NE Y
     C*** END ADD *** 04/01/99 ****************************************
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
     C*** BEGIN ADD *** 01/11/00 **************************************
     C* ZZZ
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
     C*** BEG ADD *** 04/01/99 ****************************************
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
     C     CSOVRP        ANDEQ     *ZEROS
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
     C*** END DEL *** 03/30/99 ****************************************
     C*
     C*** BEG DEL *** 03/30/99 ****************************************
     C********** XXEOB     IFEQ *BLANKS
     C**********           SETON                     9065
     C**********           GOTO CK1END
     C**********           ENDIF                           XXEOB = *BLANKS
     C*** END DEL *** 03/30/99 ****************************************
     C*
     C*** BEG ADD *** 03/30/99 ****************************************
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
     C*** BEG DEL *** 08/31/07 ****************************************
     C**********
     C********** DEKY      CHAINPDE01                99
     C********** *IN99     DOWEQ*OFF
     C********** DEESRC    IFEQ 'A'
     C********** DEEOB     ANDNE*BLANKS
     C********** DEESRC    OREQ 'N'
     C********** DEEOB     ANDNE*BLANKS
     C**********           DELETPDE01
     C**********           ELSE
     C**********           EXCPTRPDE01
     C**********           ENDIF                           DEESRC = 'A'
     C********** DEKY      READEPDE01                    99
     C**********           ENDDO
     C**********
     C********** MGKY      CHAINPMG01                99
     C********** *IN99     DOWEQ*OFF
     C********** MGSRC     IFEQ 'S'
     C********** MGSRC     OREQ 'N'
     C****BEGIN*ADD *** 01/11/00 **************************************
     C********** MGMSG#    IFNE 'NW00001'
     C****END***ADD *** 01/11/00 **************************************
     C**********           DELETPMG01
     C****BEGIN*ADD *** 01/11/00 **************************************
     C**********           ELSE
     C**********
     C**********           UPDATPMG01
     C**********
     C**********           ENDIF
     C****END***ADD *** 01/11/00 **************************************
     C**********           ELSE
     C**********           EXCPTRPMG01
     C**********           ENDIF                           MGSRC = 'S'
     C********** MGKY      READEPMG01                    99
     C**********           ENDDO
     C**********
     C*** END DEL *** 08/31/07 ****************************************
     C*** END ADD *** 08/27/99 ****************************************
     C*** BEG DEL *** 08/30/07 ****************************************
     C*
     C**********           CALL 'F0356R01'
     C**********           PARM ' '       ##KC
     C**********           PARM ' '       ##KL
     C**********           PARM 'A'       ##MODE
     C**********           PARM *ZEROS    ##RRN
     C**********           PARM           ##RRN2
     C**********           PARM           ##RRN3
     C**********           PARM           ##DSEQ
     C*** END DEL *** 08/30/07 ****************************************
     C*** BEG DEL *** 08/31/07 ****************************************
     C************
     C***********SCMRA     IFNE *ZEROS
     C***********SCNTWK    ORNE *ZEROS
     C***********SCOTHR    ORNE *ZEROS
     C***********SCOTH2    ORNE *ZEROS
     C************
     C************         MOVE *BLANKS   XXEOB
     C***********DEKY      CHAINPDE01               N99
     C************IN99     DOWEQ*OFF
     C************
     C***********DEEOB     IFNE *BLANKS
     C************         MOVE DEEOB     XXEOB
     C************         LEAVE
     C************         ENDIF                           DEEOB NE *BLANKS
     C************
     C***********DEKY      READEPDE01               N    99
     C************         ENDDO                           *IN99 = *OFF
     C************
     C***********XXEOB     IFEQ *BLANKS
     C************         SETON                     90
     C************         ENDIF                           XXEOB = *BLANKS
     C************         ENDIF                           *IN99 = *OFF
     C************
     C*** END DEL *** 08/31/07 ****************************************
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
     C*
     C* IF IN CHANGE MODE, THEN SET 'OUTFLG' TO END PROGRAM
     C*
     C     @@MODE        IFEQ      'C'
     C                   MOVE      '1'           OUTFLG
     C**********           SETOF                     90
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
     C     DPKY          CHAIN     PDP01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C* LOAD REPEAT VALUES TO SCREEN
     C*
     C                   Z-ADD     CAFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRXFD
     C*
     C                   Z-ADD     CACHRG        SCCHRG
     C*** BEG ADD *** 02/25/00 ****************************************
     C                   MOVEL     CACSCD        SCCSCD
     C                   MOVEL(P)  SCCSCD        KKKEY
     C     TBKY          CHAIN     PCS01                              99
     C  N99              MOVEL(P)  CSDESC        SCCSDS
     C*
     C                   MOVEL     DPNDC         SCNDC
     C*
     C                   MOVEL(P)  SCNDC         KKNDC
     C     RXKY          CHAIN     PRX01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     RXNAME        SCDESC
     C                   ENDIF                                                  *IN99 IFEQ *ON

     C***** Beg Add *** 09/01/16 ****************************************
     C                   Eval      SCNDC2 = DPNDC2
     C*
     C                   MOVEL(P)  SCNDC2        KKNDC
     C     RXKY          CHAIN     PRX01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     RXNAME        SCDESC2
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C***** End Add *** 09/01/16 ****************************************

     C                   Z-ADD     DPUNTS        SCUNTS
     C*** END ADD *** 02/25/00 ****************************************
     C*
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
    C*** BEGIN ADD *** 09/27/07 **************************************
     C                   MOVE      *OFF          *IN(61)
    C*** END   ADD *** 09/27/07 **************************************
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
     C***CALL*FA713R*WHICH WILL CALL APPROPRIATE ADJUDICATION PROGRAMS
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
     C***********CATOTC*** SUB  SMCHG     DFCHRG
     C***********CACHRG*** SUB  SMCHG     DFCHRG
     C***********CATOTD*** SUB  SMMRA     DFMRA
     C***********CAMRA**** SUB  SMMRA     DFMRA
     C***********CANDIS*** SUB  SMNDIS    DFNDIS
     C***********CAOTHR*** SUB  SMOTHR    DFOTHR
     C***********CAOTH2*** SUB  SMOTH2    DFOTH2
     C***********CATOTP*** SUB  SMPAID    DFPAID
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
     C******************** Z-ADDCATOTC    DACHRG
     C******************** Z-ADDCACHRG    DACHRG
     C******************** Z-ADDCATOTD    DAMRA
     C******************** Z-ADDCAMRA     DAMRA
     C******************** Z-ADDCANDIS    DANDIS
     C******************** Z-ADDCAOTHR    DAOTHR
     C******************** Z-ADDCAOTH2    DAOTH2
     C******************** Z-ADDCATOTP    DAPAID
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
     C                   MOVE      EOAST         ##ST              2
     C                   MOVEL(P)  SCNDC         KKNDC
     C     RXKY          CHAIN     PRX01                              99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRN
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     PRRRN         ##RRN
     C                   ENDIF                                                  *IN99 = On
     C*** BEG CHG *** 10/29/99 ****************************************
     C***********CARCFL    IFEQ 'C'
     C     CARCFL        IFEQ      'X'
     C*** BEG CHG *** 10/29/99 ****************************************
     C                   CALL      'F2350R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRN
     C******               PARM           ##ST
     C                   ENDIF                                                  RCFP = 'C'
     C     CARCFL        IFEQ      'Y'
     C                   CALL      'F2350R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRN
     C******               PARM           ##ST
     C                   ENDIF                                                  RCFP = 'Y'
     C                   SETON                                        90
     C     KIEND         ENDSR
     C*================================================================
     C* KJDO - F10=SEARCH.
     C*================================================================
     C     KJDO          BEGSR
     C*
     C*** INITIALIZE POSITION CURSOR INDICATORS
     C*
     C                   SETOFF                                       2728
     C                   SETOFF                                       313233
     C                   SETOFF                                       343536
     C                   SETOFF                                       373839
     C                   SETOFF                                       404142
     C                   SETOFF                                       484950
     C                   SETOFF                                       535455
     C*** BEG ADD *** 12/23/05 ****************************************
     C                   SETOFF                                       4345
     C*** END ADD *** 12/23/05 ****************************************
     C*** BEGIN ADD *** 09/27/07 **************************************
     C                   SETOFF                                       01
     C*** END   ADD *** 09/27/07 **************************************

    C***** Beg Add *** 09/01/16 **************************************
     C                   Eval      IndFoNDC2  = *Off
     C                   Eval      IndFoDesc2 = *Off
    C***** End Add *** 09/01/16 **************************************

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
     C* NDC CODE SEARCH
     C*
     C     HFLD          WHENEQ    'SCNDC'
     C                   MOVEL(P)  SCNDC         HLNDC
     C                   MOVEL(P)  SCRXMM        HLRXMM
     C                   MOVEL(P)  SCRXMD        HLRXMD           40
     C                   Z-ADD     0             ##RRN
     C                   CALL      'F2350R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVEL(P)  HLNDC         SCNDC
     C                   MOVEL(P)  HLRXMM        SCRXMM
     C                   MOVEL(P)  HLRXMD        SCRXMD
     C*** BEGIN CHG *** 09/27/07 **************************************
     C**********           SETON                     9054   pos cursor
     C                   SETON                                        9001
     C*** END   CHG *** 09/27/07 **************************************
     C                   ELSE
     C     ##RRN         CHAIN     PRX02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  RXNDC         SCNDC
     C                   MOVEL(P)  RXMEAS        SCRXMM
     C                   MOVEL(P)  RXNAME        SCDESC
     C                   MOVEL(P)  'MEAS'        KKTBID
     C                   MOVEL(P)  SCRXMM        KKKEY1
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCRXMD
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   MOVEL(P)  'MEAS'        KKTBID
     C                   MOVEL(P)  SCRXMM        KKKEY1
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCRXMD
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C*** BEG CHG *** 12/23/05 ****************************************
     C******************** SETON                     9053
     C                   SETON                                        9045
     C*** END CHG *** 12/23/05 ****************************************
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##KC = '1'
     C*
    C***** Beg Add *** 09/01/16 **************************************
     C*
     C* NDC Code Secondary search
     C*
     C     HFLD          WHENEQ    'SCNDC2'
     C                   MOVEL(P)  SCNDC2        HLNDC
     C                   MOVEL(P)  SCRXMM        HLRXMM
     C                   MOVEL(P)  SCRXMD        HLRXMD           40
     C                   Z-ADD     0             ##RRN
     C                   CALL      'F2350R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVEL(P)  HLNDC         SCNDC2
     C                   MOVEL(P)  HLRXMM        SCRXMM
     C                   MOVEL(P)  HLRXMD        SCRXMD
     C                   SETON                                        90
     C                   Eval      IndFoNDC2  = *On
     C                   ELSE
     C     ##RRN         CHAIN     PRX02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  RXNDC         SCNDC2
     C*                  MOVEL(P)  RXMEAS        SCRXMM
     C                   MOVEL(P)  RXNAME        SCDESC2
     C*                  MOVEL(P)  'MEAS'        KKTBID
     C*                  MOVEL(P)  SCRXMM        KKKEY1
     C*    GTKY          CHAIN     PGT01                              99
     C*    *IN99         IFEQ      *OFF
     C*                  MOVEL(P)  GTDESC        SCRXMD
     C*                  ENDIF                                                  *IN99 = *OFF
     C*
     C*                  MOVEL(P)  'MEAS'        KKTBID
     C*                  MOVEL(P)  SCRXMM        KKKEY1
     C*    GTKY          CHAIN     PGT01                              99
     C*    *IN99         IFEQ      *OFF
     C*                  MOVEL(P)  GTDESC        SCRXMD
     C*                  ENDIF                                                  *IN99 = *OFF
     C*
     C                   SETON                                        90
     C                   Eval      IndFoDesc2 = *On
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##KC = '1'
    C***** End Add *** 09/01/16 **************************************

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
     C*** BEGIN ADD *** 04/07/06 **************************************
     C     HFLD          WHENEQ    'SCLIC#'
     C     XXCLT#        IFEQ      '048'
     C*
     C                   SETOFF                                       41
     C                   MOVEL     *BLANKS       ##KEY
     C                   MOVEL     'FL'          ##KEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      *OFF
     C     STBS01        ANDEQ     'Y'
     C                   MOVEL(P)  SCLIC#        HLLIC#           13
     C                   CALL      'F2395R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##LIC#           13
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C                   SETON                                        9045
     C                   SETOFF                                       43
     C                   MOVEL(P)  HLLIC#        SCLIC#
     C                   ELSE
     C     ##LIC#        IFEQ      *BLANKS
     C                   MOVEL(P)  HLLIC#        SCLIC#
     C                   SETON                                        9045
     C                   ELSE
     C                   MOVEL(P)  ##LIC#        SCLIC#
     C                   SETON                                        90
     C                   SETOFF                                       45
     C                   ENDIF                                                  ##LIC#=*BLAN
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE                                                   *IN60=*OFF
     C                   SETON                                        9045
     C                   ENDIF                                                  *IN60=*OFF
     C                   ELSE
     C                   SETON                                        9028
     C                   ENDIF                                                  XXCLT#='048'
     C*** END   ADD *** 04/07/06 **************************************
     C*** BEGIN ADD *** 09/27/07 **************************************
     C     HFLD          WHENEQ    'SCRXDY'
     C                   SETON                                        9027
     C*** END   ADD *** 09/27/07 **************************************
     C*** BEGIN ADD *** 12/23/05 **************************************
     C     HFLD          WHENEQ    'SCNWRF'
     C*
     C                   MOVEL(P)  'FLFI'        KKTBID
     C*
     C     GTKY2         CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      0             ##RRN
     C                   PARM                    KKTBID
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SCNWRF
     C                   SETON                                        9043
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C                   SETON                                        9042
     C*
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C                   ENDIF                                                  99 = OFF
     C*
     C     HFLD          WHENEQ    'SCCERT'
     C*
     C                   MOVEL(P)  'FLCT'        KKTBID
     C*
     C     GTKY2         CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      0             ##RRN
     C                   PARM                    KKTBID
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SCCERT
     C                   SETON                                        9045
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C                   SETON                                        9043
     C*
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C                   ENDIF                                                  99 = OFF
     C*
     C*** END   ADD *** 12/23/05 **************************************
     C*
     C*** BEGIN ADD *** 03/14/07 **************************************
     C*
     C*** BEGIN ADD *** 09/27/07 **************************************
     C     HFLD          WHENEQ    'SCRXPT'
     C*
     C                   MOVEL(P)  'FLPT'        KKTBID
     C*
     C     GTKY2         CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      0             ##RRN
     C                   PARM                    KKTBID
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SCRXPT
     C                   SETON                                        9057
     C*
     C*
     C*          SCRXPT    IFEQ 'S'
     C*                    SETON                     17
     C*                    ENDIF
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE                                                   ##RRN<>0
     C                   SETON                                        9001
     C*
     C                   ENDIF                                                  ##RRN<>0
     C*
     C                   ENDIF                                                  99 = OFF
     C*
     C*** END   ADD *** 09/27/07 **************************************
     C* HCPCS SEARCH
     C*
     C     HFLD          WHENEQ    'SCHCPC'
     C*
     C     SCRXPT        IFEQ      'S'
     C*
     C                   MOVE      SCHCPC        HLCPT4            5
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
     C                   PARM                    ##AST             2
     C*
     C     ##RTCD        IFEQ      '1'
     C     ##PROD        OREQ      *BLANKS
     C                   MOVEL     HLCPT4        SCHCPC
     C                   SETON                                        9057        pos cursor
     C                   ELSE
     C                   MOVEL     ##PROC        SCHCPC
     C                   SETON                                        9052
     C                   ENDIF                                                  ##RTCD = 1
     C*
     C                   ELSE
     C*
     C                   SETON                                        9032
     C*
     C                   ENDIF
     C*
     C*
     C* Purchase/Rental Search
     C*
     C     HFLD          WHENEQ    'SCPRIN'
     C*
     C     SCRXPT        IFEQ      'S'
     C*
     C*                    SETOF                     22
     C                   MOVEL(P)  'FLPR'        KKTBID
     C*
     C     GTKY2         CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      0             ##RRN
     C                   PARM                    KKTBID
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SCPRIN
     C*
     C                   SETON                                        9053
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE                                                   ##RRN<>0
     C                   SETON                                        9052
     C*
     C                   ENDIF                                                  ##RRN<>0
     C*
     C                   ENDIF                                                  99 = OFF
     C*
     C                   ELSE
     C*
     C                   SETON                                        9039
     C*
     C                   ENDIF
     C*
     C*
     C*** END   ADD *** 03/14/07 **************************************
     C                   ENDSL
     C*
     C                   EXSR      DATSBR
     C                   SETON                                        70
     C*** BEG ADD *** 04/21/06 **************************************
     C                   SETON                                        90
     C*** END ADD *** 04/21/06 **************************************
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
     C* KPDO - F14=PREVIOUS LINES
     C*================================================================
     C     KPDO          BEGSR
     C                   Z-ADD     @@RRN2        ##RRN
     C                   CALL      'F0354R'
     C                   PARM      '1'           ##CALL
     C                   PARM                    ##RRN                          faeop
     C                   SETON                                        90
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C*** BEGIN ADD *** 09/27/07 **************************************
     C                   MOVEL     *BLANKS       SCRXPT
     C************         MOVEL*BLANKS   SCRXPD
     C*** END   ADD *** 09/27/07 **************************************
     C                   Z-ADD     0             SCEOB#
     C                   Z-ADD     0             SCLIN#
     C                   Z-ADD     0             SCSEQ#
     C                   MOVE      *BLANKS       SCRX#
     C                   MOVE      *BLANKS       SCCSCD
     C                   MOVE      *BLANKS       SCCSDS
     C                   Z-ADD     0             SCRXFD
     C                   MOVE      *BLANKS       SCNDC
     C                   MOVE      *BLANKS       SCDESC
    C***** Beg Add *** 09/01/16 **************************************
     C                   MOVEL(P)  *BLANKS       SCNDC2
     C                   MOVE      *BLANKS       SCDESC2
    C***** End Add *** 09/01/16 **************************************
     C                   MOVE      *BLANKS       SCRXMM
     C                   MOVE      *BLANKS       SCRXMD
     C                   Z-ADD     0             SCCHRG
     C                   Z-ADD     0             SCPAID
     C                   Z-ADD     0             SCMRA
     C                   Z-ADD     0             SCNTWK
     C                   Z-ADD     0             SCOTHR
     C                   MOVE      *BLANKS       SCOT1C
     C                   Z-ADD     0             SCOTH2
     C                   MOVE      *BLANKS       SCOT2C
     C                   Z-ADD     0             SCUNTS
     C*** BEG ADD ***** 03/14/07 **************************************
     C                   MOVEL     *BLANKS       SCHCPC
     C                   MOVEL     *BLANKS       SCPRIN
     C*** END ADD ***** 03/14/07 **************************************
     C                   CLEAR                   SCEOB1
     C                   CLEAR                   SCEOB2
     C                   CLEAR                   SCEOB3
     C                   CLEAR                   SCEOB4
     C*
     C                   MOVE      EOAST         SCAST
     C                   MOVE(P)   EOZIPA        SCZIPA
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
     C                   Z-ADD     DSDATE        SCEDAT
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
     C                   ENDIF                                                  @@MODE IFEQ 'A'
     C*
     C                   MOVEL(P)  SCCSCD        KKKEY
     C     TBKY          CHAIN     PCS01                              99
     C  N99              MOVEL(P)  CSDESC        SCCSDS
     C*
     C     @@MODE        IFNE      'A'
     C*
     C* FROM FDDPP
     C*
     C                   MOVEL(P)  DPRX#         SCRX#
     C                   MOVEL(P)  DPRXMM        SCRXMM
     C                   MOVEL(P)  DPNDC         SCNDC
     C                   MOVEL(P)  DPDESC        SCDESC
    C***** Beg Add *** 09/01/16 **************************************
     C                   MOVEL(P)  DPNDC2        SCNDC2
     C                   MOVEL(P)  DPDESC2       SCDESC2
    C***** End Add *** 09/01/16 **************************************
     C                   Z-ADD     DPUNTS        SCUNTS
     C*** BEGIN ADD *** 12/23/05 **************************************
     C                   Z-ADD     DPRXDY        SCRXDY
     C                   MOVEL     DPNWRF        SCNWRF
     C                   MOVEL     DPCERT        SCCERT
     C                   MOVEL(P)  DPLIC#        SCLIC#
     C*** END   ADD *** 12/23/05 **************************************
     C*** BEG ADD *** 09/27/07 ****************************************
     C                   MOVEL     DPRXPT        SCRXPT
     C*** END ADD *** 09/27/07 ****************************************
     C*
     C*
     C*** BEGIN ADD *** 03/14/07 **************************************
     C*
     C                   MOVEL     DPPRIN        SCPRIN
     C                   MOVEL     DPHCPC        SCHCPC
     C*
     C*** END   ADD *** 03/14/07 **************************************
     C*
     C*
     C                   MOVEL(P)  'MEAS'        KKTBID
     C                   MOVEL(P)  SCRXMM        KKKEY1
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCRXMD
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
     C                   MOVEL     SRDTO         SCRXFD
     C*
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
     C* GET NDCE CODE DESCRIPTION
     C*
     C*****                MOVELSCNDC     KKNDC     P
     C*****      RXKY      CHAINPRX01                99
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
     C     DEEOB         ANDNE     *BLANKS
     C                   ADD       1             XXSEQ#
     C                   SELECT
     C     XXSEQ#        WHENEQ    1
     C                   MOVEL     DEEOB         SCEOB1
     C     XXSEQ#        WHENEQ    2
     C                   MOVEL     DEEOB         SCEOB2
     C     XXSEQ#        WHENEQ    3
     C                   MOVEL     DEEOB         SCEOB3
     C     XXSEQ#        WHENEQ    4
     C                   MOVEL     DEEOB         SCEOB4
     C                   ENDSL
     C     DEKY          READE(N)  PDE01                                  99
     C                   ENDDO
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
     C* LOAD FIELDS FOR FDDPP
     C*
     C                   Z-ADD     SCRXFD        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DPRXFD
     C*
     C*** BEG ADD *** 10/18/06 ****************************************
     C*
     C* IF THE NDC PAID CODE IS CHANGED, THEN WRITE AN EOB NOTE.
     C*
     C     SCNDC         IFNE      DPNDC
     C*
     C                   EXSR      WRTDEP
     C*
     C                   ENDIF                                                  SCNDC
     C*
     C*** END ADD *** 10/18/06 ****************************************
     C*
     C                   MOVEL(P)  SCNDC         DPNDC
     C                   MOVEL(P)  SCRX#         DPRX#
     C                   MOVEL(P)  SCRXMM        DPRXMM
     C                   Z-ADD     SCUNTS        DPUNTS
     C                   Z-ADD     DPUNTS        DPRXMQ
     C*
     C     SCDESC        IFNE      *BLANKS
     C                   MOVEL(P)  SCDESC        DPDESC
     C                   ELSE
     C                   MOVEL(P)  SCNDC         KKNDC
     C     RXKY          CHAIN     PRX01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  RXNAME        DPDESC
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ENDIF                                                  SCDESC = *BLANKS
     C*** BEGIN ADD *** 12/23/05 **************************************

    C***** Beg Add *** 09/01/16 **************************************
     C                   MOVEL(P)  SCNDC2        DPNDC2
     C*
     C     SCDESC2       IFNE      *BLANKS
     C                   MOVEL(P)  SCDESC2       DPDESC2
     C                   ELSE
     C                   MOVEL(P)  SCNDC2        KKNDC
     C     RXKY          CHAIN     PRX01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  RXNAME        DPDESC2
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ENDIF                                                  SCDESC = *BLANKS
    C***** End Add *** 09/01/16 **************************************

     C                   Z-ADD     SCRXDY        DPRXDY
     C                   MOVEL     SCNWRF        DPNWRF
     C                   MOVEL     SCCERT        DPCERT
     C                   MOVEL(P)  SCLIC#        DPLIC#
     C*** END   ADD *** 12/23/05 **************************************
     C*** BEGIN ADD *** 09/27/07 **************************************
     C                   MOVEL     SCRXPT        DPRXPT
     C*** END   ADD *** 09/27/07 **************************************
     C*
     C*** BEGIN ADD *** 03/14/07 **************************************
     C*
     C                   MOVEL     SCPRIN        DPPRIN
     C                   MOVEL     SCHCPC        DPHCPC
     C*
     C*** END   ADD *** 03/14/07 **************************************
     C*
     C*
     C* LOAD FIELDS FOR FDCAP
     C*
     C                   MOVEL     SRDTO         CAFDAT
     C                   MOVEL     SRDTO         CATDAT
     C*
     C                   MOVEL     SCNDC         CACPT4
     C*
     C                   Z-ADD     SCUNTS        CAUNTS
     C*
     C*****      SCPRCD    IFGE '00100'
     C*****      SCPRCD    ANDLE'01999'
     C*****                MOVE CAUNTS    CAANMN
     C*****                ENDIF
     C*
     C*** BEG DEL *** 04/21/06 ****************************************
     C********************
     C******************** Z-ADDSCCHRG    CACHRG
     C******************** MOVELEOSS#     CASS#
     C******************** MOVELEOACDT    CAACDT
     C******************** Z-ADDSCOTHR    CAOTHR
     C******************** Z-ADDSCOTH2    CAOTH2
     C******************** Z-ADDSCNTWK    CANDIS
     C******************** Z-ADDSCCHRG    CACHRG
     C******************** Z-ADDSCMRA     CAMRA
     C******************** MOVE SCZIPA    CAZIPA
     C******************** MOVE SCAST     CAAST     P
     C********************
     C******************** Z-ADDSCPAID    CAPAID
     C******************** MOVELSCCSCD    CACSCD    P
     C******************** MOVELSCOT1C    CAOT1C    P
     C******************** MOVELSCOT2C    CAOT2C    P
     C*** END DEL *** 04/21/06 ****************************************
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
     C                   SETOFF                                       606162
     C                   SETOFF                                       636465
     C                   SETOFF                                       666758
     C                   SETOFF                                       7030
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
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C** Get standard screen headings
     C*
     C     @@RRN2        CHAIN(N)  PEO01                              99
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
     C*** BEG DEL *** 02/23/99 ****************************************
     C********** @@RRN     CHAINDBRCDF              N99
     C**********           Z-ADDDPDSEQ    KKDSEQ
     C*** END DEL *** 02/23/99 ****************************************
     C*
     C*
     C*** BED ADD *** 02/23/99 ****************************************
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
     C     DPKY          CHAIN     PDP01                              99
     C                   Z-ADD     DBRRNC        @@RRN
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ELSE
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C                   Z-ADD     DPDSEQ        KKDSEQ
     C                   ENDIF                                                  @@MODE = 'E'
     C*** END ADD *** 02/23/99 ****************************************
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
     C*
     C* DISPLAY EOB CODES AND TITLE IF NOT IN ADD MODE
     C*
     C     @@MODE        IFNE      'A'
     C                   SETON                                        59
     C                   ELSE
     C                   SETOFF                                       59
     C                   ENDIF                                                  @@MODE NE 'A'
     C*
     C*** BEGIN ADD *** 04/07/06 **************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
     C*
     C*** END   ADD *** 04/07/06 **************************************
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
     C                   Z-ADD     SCRXFD        SRDTI
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
     C*** BEG ADD *** 04/07/06 ****************************************
     C     UPDATE        IFEQ      *ON
     C                   MOVEL(P)  SCFLG7        EOFLG7
     C                   MOVEL     *OFF          UPDATE
     C                   ENDIF
     C*** END ADD *** 04/07/06 ****************************************
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
     C     SCCSCD        IFNE      '1'
     C                   Z-ADD     0             CAMRAA
     C                   ENDIF                                                  SCCSCD = 1
     C*
     C                   MOVEL     Q1USER        CAUSER
     C                   MOVEL     Q1PGM         CAPGM
     C                   Z-ADD     DSTIME        CATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
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
     C                   MOVEL     Q1USER        DPUSER
     C                   MOVEL     Q1PGM         DPPGM
     C                   Z-ADD     DSTIME        DPTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DPDT
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
     C                   Z-ADD     ##CO#         DPCO#
     C                   Z-ADD     EOSUB#        DPSUB#
     C                   Z-ADD     EOFND         DPFND
     C                   Z-ADD     EOFYR         DPFYR
     C                   Z-ADD     EOCASE        DPCASE
     C                   Z-ADD     SCSEQ#        DPDSEQ
     C                   Z-ADD     EOEOB#        DPEOB#
     C                   MOVEL     EOSS#         DPSS#
     C                   Z-ADD     EOACDT        DPACDT
     C                   MOVE      'A'           DPSTAT
     C                   MOVEL     Q1USER        DPUSER
     C                   MOVEL     Q1USER        DPIUSR
     C                   MOVEL     Q1PGM         DPPGM
     C                   MOVEL     Q1PGM         DPIPGM
     C                   Z-ADD     DSTIME        DPTIME
     C                   Z-ADD     DSTIME        DPITIM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DPDT
     C                   MOVEL     SRDTO         DPIDT
     C                   WRITE     DBRCDF
     C*
     C                   Z-ADD     ##CO#         CACO#
     C                   Z-ADD     EOSUB#        CASUB#
     C                   Z-ADD     EOFND         CAFND
     C                   Z-ADD     EOFYR         CAFYR
     C                   Z-ADD     EOCASE        CACASE
     C**********           MOVELSRDTO     CATDAT
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
     C**********           Z-ADDEOEOB#    CADCS#
     C                   Z-ADD     EORCN         CARCN
     C                   Z-ADD     EOFEIN        CAFEIN
     C                   Z-ADD     EOMOD         CAMOD
     C                   MOVE      'X'           CARCFL
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
     C*****                ELSE
     C*****
     C* WRITE MULTIPLE RECORDS BASED ON SCUNTS
     C*****
     C*****      SCCHRG    DIV  SCUNTS    XXCHRG  92
     C*****                Z-ADDSCUNTS    XXUNTS
     C*****                CLEARDBRCDF
     C*****                CLEARPCA02
     C*****                TIME           DSDTTM
     C*****                EXSR MVSCDB
     C*****                Z-ADD##CO#     DPCO#
     C*****                Z-ADDEOSUB#    DPSUB#
     C*****                Z-ADDEOFND     DPFND
     C*****                Z-ADDEOFYR     DPFYR
     C*****                Z-ADDEOCASE    DPCASE
     C*****                Z-ADDSCSEQ#    DPDSEQ
     C*****                Z-ADDEOEOB#    DPEOB#
     C*****                MOVELEOSS#     DPSS#
     C*****                Z-ADDEOACDT    DPACDT
     C*****                MOVELQ1USER    DPUSER
     C*****                MOVELQ1USER    DPIUSR
     C*****                MOVELQ1PGM     DPPGM
     C*****                MOVELQ1PGM     DPIPGM
     C*****                Z-ADDDSTIME    DPTIME
     C*****                Z-ADDDSTIME    DPITIM
     C*****
     C*****                Z-ADDDSDATE    SRDTI
     C*****                MOVE *BLANKS   SRDTO
     C*****                MOVE *BLANKS   SRERR
     C*****                CALL 'SRCVTDAT'DTCYMD
     C*****                MOVELSRDTO     DPDT
     C*****                MOVELSRDTO     DPIDT
     C*****                WRITEDBRCDF
     C*****
     C**********           Z-ADDXXCHRG    CATOTC
     C*****                Z-ADDXXCHRG    CACHRG
     C*****                Z-ADD0         CAUNTS
     C*****                Z-ADD##CO#     CACO#
     C*****                Z-ADDEOSUB#    CASUB#
     C*****                Z-ADDEOFND     CAFND
     C*****                Z-ADDEOFYR     CAFYR
     C*****                Z-ADDEOCASE    CACASE
     C*****                MOVELQ1USER    CAUSER
     C*****                MOVELQ1USER    CAIUSR
     C*****                MOVELQ1PGM     CAPGM
     C*****                MOVELQ1PGM     CAIPGM
     C*****                Z-ADDDSTIME    CATIME
     C*****                MOVELSRDTO     CADT
     C*****                MOVELSRDTO     CAIDT
     C*****                Z-ADDSCLIN#    CALIN#
     C*****                Z-ADDSCSEQ#    CADSEQ
     C*****                Z-ADDEOEOB#    CAEOB#
     C*****                Z-ADDEORCN     CARCN
     C*****                Z-ADDEOFEIN    CAFEIN
     C*****                Z-ADDEOMOD     CAMOD
     C*****                MOVE 'C'       CARCFL
     C*****
     C*****      EOBTYP    IFEQ 'N'
     C*****                MOVE '1'       CAPRTY
     C*****                ENDIF
     C*****
     C*****      EOBTYP    IFEQ 'H'
     C*****                MOVE '4'       CAPRTY
     C*****                ENDIF
     C*****
     C*****                WRITEPCA02
     C*****                SUB  1         XXUNTS
     C*****      XXUNTS    DOWGT0
     C*****
     C*****                Z-ADD*HIVAL    KKLIN#
     C*****      CAKY4     SETGTPCA04
     C*****      CAKY3     REDPEPCA04                    99
     C*****      *IN99     IFEQ *OFF
     C*****      CALIN#    ADD  1         ZZLIN#
     C*****                ELSE
     C*****                Z-ADD1         ZZLIN#
     C*****                ENDIF                           *IN99 = *OFF
     C*****
     C*****                Z-ADD*HIVAL    KKDSEQ
     C*****      CAKY      SETGTPCA01
     C*****      CAKY1     REDPEPCA01                    99
     C*****      *IN99     IFEQ *ON
     C*****                Z-ADD1         ZZDSEQ
     C*****                ELSE
     C*****      CADSEQ    ADD  1         ZZDSEQ
     C*****                ENDIF                           *IN99 IFEQ *ON
     C*****
     C*****                CLEARDBRCDF
     C*****                CLEARPCA02
     C*****                TIME           DSDTTM
     C*****                EXSR MVSCDB
     C*****                Z-ADD##CO#     DPCO#
     C*****                Z-ADDEOSUB#    DPSUB#
     C*****                Z-ADDEOFND     DPFND
     C*****                Z-ADDEOFYR     DPFYR
     C*****                Z-ADDEOCASE    DPCASE
     C*****                Z-ADDZZDSEQ    DPDSEQ
     C*****                Z-ADDEOEOB#    DPEOB#
     C*****                MOVELEOSS#     DPSS#
     C*****                Z-ADDEOACDT    DPACDT
     C*****                MOVELQ1USER    DPUSER
     C*****                MOVELQ1USER    DPIUSR
     C*****                MOVELQ1PGM     DPPGM
     C*****                MOVELQ1PGM     DPIPGM
     C*****                Z-ADDDSTIME    DPTIME
     C*****                Z-ADDDSTIME    DPITIM
     C*****
     C*****                Z-ADDDSDATE    SRDTI
     C*****                MOVE *BLANKS   SRDTO
     C*****                MOVE *BLANKS   SRERR
     C*****                CALL 'SRCVTDAT'DTCYMD
     C*****                MOVELSRDTO     DPDT
     C*****                MOVELSRDTO     DPIDT
     C*****                WRITEDBRCDF
     C*****
     C**********           Z-ADDXXCHRG    CATOTC
     C*****                Z-ADDXXCHRG    CACHRG
     C*****                Z-ADD0         CAUNTS
     C*****                Z-ADD##CO#     CACO#
     C*****                Z-ADDEOSUB#    CASUB#
     C*****                Z-ADDEOFND     CAFND
     C*****                Z-ADDEOFYR     CAFYR
     C*****                Z-ADDEOCASE    CACASE
     C*****                MOVELQ1USER    CAUSER
     C*****                MOVELQ1USER    CAIUSR
     C*****                MOVELQ1PGM     CAPGM
     C*****                MOVELQ1PGM     CAIPGM
     C*****                Z-ADDDSTIME    CATIME
     C*****                MOVELSRDTO     CADT
     C*****                MOVELSRDTO     CAIDT
     C*****                Z-ADDZZLIN#    CALIN#
     C*****                Z-ADDZZDSEQ    CADSEQ
     C*****                Z-ADDEOEOB#    CAEOB#
     C*****                Z-ADDEORCN     CARCN
     C*****                Z-ADDEOFEIN    CAFEIN
     C*****                Z-ADDEOMOD     CAMOD
     C*****                MOVE 'C'       CARCFL
     C*****
     C*****      EOBTYP    IFEQ 'N'
     C*****                MOVE '1'       CAPRTY
     C*****                ENDIF
     C*****
     C*****      EOBTYP    IFEQ 'H'
     C*****                MOVE '4'       CAPRTY
     C*****                ENDIF
     C*****
     C*****                WRITEPCA02
     C*****                SUB  1         XXUNTS
     C*****
     C*****                ENDDO
     C*****
     C*****                ENDIF                           SCPRCD GE 00100
     C*
     C* UPDATE FAEOP
     C*
     C                   EXSR      UFAEOP
     C*
     C* UPDATE TOTAL CHARGES
     C*
     C                   EXSR      SMCHRG
     C*
     C                   FEOD      FDDPP
     C                   SETON                                        41
     C                   ENDSR
     C*
     C*** BEG ADD *** 10/18/06 ****************************************
     C*================================================================
     C* WRTDEP - Write EOB note indicating change on NDC paid code
     C*================================================================
     C     WRTDEP        BEGSR
     C*
     C                   MOVE      *HIVAL        KKSEQ#
     C*
     C     DDKY          SETGT     PDE01
     C     DDKY2         READPE    PDE01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     DESEQ#        ADD       1             XXSEQ#
     C*
     C                   ELSE
     C*
     C                   Z-ADD     1             XXSEQ#
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     EOCO#         DECO#
     C                   Z-ADD     EOSUB#        DESUB#
     C                   Z-ADD     EOFND         DEFND
     C                   Z-ADD     EOFYR         DEFYR
     C                   Z-ADD     EOCASE        DECASE
     C                   Z-ADD     EOEOB#        DEEOB#
     C                   Z-ADD     CADSEQ        DEDSEQ
     C                   Z-ADD     XXSEQ#        DESEQ#
     C                   MOVEL     *BLANKS       DEEOB
     C                   MOVEL(P)  NT1           XXNT1
     C                   MOVEL(P)  DPNDC         XXNT2
     C                   MOVEL(P)  NT3           XXNT3
     C                   MOVEL(P)  XXNOT1        DENOT1
     C                   MOVEL(P)  *BLANKS       DENOT2
     C                   MOVEL     'A'           DESTAT
     C                   MOVEL(P)  'N'           DEPRNT
     C                   Z-ADD     EORCN         DERCN
     C                   MOVEL(P)  EOSS#         DESS#
     C                   Z-ADD     EOACDT        DEACDT
     C                   MOVE      'U'           DEESRC                         user entered
     C                   MOVEL     Q1USER        DEUSER
     C                   MOVEL     Q1USER        DEIUSR
     C                   MOVEL     Q1PGM         DEPGM
     C                   MOVEL     Q1PGM         DEIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     EODT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DEDT
     C*
     C                   MOVEL     SRDTO         DEIDT
     C*
     C                   MOVEL     DSTIME        DETIME
     C                   MOVEL     DSTIME        DEITIM
     C*
     C                   WRITE     PDE02
     C*
     C                   ENDSR
     C*** END ADD *** 10/18/06 ****************************************
     C*
     O*** BEG DEL *** 08/31/07 ****************************************
     O***** PMG01   E                RPMG01
     O***** PDE01   E                RPDE01
     O*** END DEL *** 08/31/07 ****************************************
**
ABCDEFGHIJKLMNOPQRSTUVWXYZ
**
0123456789
