     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**sy266_ovr*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE F0463RKY - Hospital UB Detail Information FOR KY (CHG)
     H*****************************************************************
     H*
     H* F0463RKY - Hospital UB Detail Information
     H*
     H* 03/09/00  C:52   Project: KY010
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  04/03/03  C:079  SUPPORT MC044 36611
     H*                   SCPOS CHANGED TO 3.0N
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
     FF0463FKY  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFDDUP     UF A E             DISK
     F                                     RENAME(PDU01:DBRCDF)
     F*
     F*  EOMB Transaction File
     FFAEOP     UF   E             DISK
     F*
     F*  Type of Service File
     F**********FTTSP   IF  E                    DISK
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
     F*
     F*  Fee Schedule Claims Status Code File
     FFTCSP     IF   E             DISK
     F                                     RENAME(PCS01:PCS02)
     F*
     F*  Place of Service Table File
     FFTPSL     IF   E           K DISK
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
     F*** BEG DEL *** 11/12/99 ****************************************
     F**********FDDAL1  IF  E           K        DISK
     F**********  PDA01                             KRENAMEPDA02
     F*** END DEL *** 11/12/99 ****************************************
     F*
     F*  EOMB Phys/HCFA Header File
     F**********FAEHL   IF  E           K        DISK
     F*
     F*  Diagnosis Code Physical File
     F**********FMDIL   IF  E           K        DISK
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
     FFDDUL     IF   E           K DISK
     F*** BEG ADD *** 02/23/99 ****************************************
     F                                     INFDS(DBFDC)
     F*** END ADD *** 02/23/99 ****************************************
     F* Service Code File
     FFMSVL1    IF   E           K DISK
     F                                     INFDS(DBFSV)
     F*
     F* State FS Rules/Pgm Cross Reference File
     FFDRUL1    IF   E           K DISK
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SWC01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
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
      /COPY SCOPYMT01D
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
     D DBFSV           DS
     D  SVRRN                397    400B 0
     D*** BEG ADD *** 02/23/99 ****************************************
     D DBFDC           DS
     D  DBRRNC               397    400B 0
     D*** END ADD *** 02/23/99 ****************************************
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
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Hospital (UB) Detail-
     D                                      Information')
     D*** BEG ADD *** 02/16/99 ***************************************
     D UNLIST          C                   CONST('* UNLISTED *')
     D*** END ADD *** 02/16/99 ***************************************
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)

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
     C*************        PARM           @@NWCD  5        DEL 03/09/00
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
     C********** DIKY      KLIST
     C**********           KFLD           KKICD9
     C**********           KFLD           KKLOST
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
     C     *LIKE         DEFINE    DEEOB         XXEOB
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
     C* Validate Claim Status (Adjudication) code
     C*
     C     SCCSCD        IFEQ      *BLANKS
     C                   SETON                                        9071
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCCSCD IFEQ BLANKS
     C*
     C                   MOVEL(P)  SCCSCD        KKKEY
     C     TBKY          CHAIN     PCS01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9072
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C*
     C* Validate Place of Service
     C*
     C*** BEG CHG *** 04/03/03 ****************************************
     C********** SCPOS     IFEQ *BLANKS
     C     SCPOS         IFEQ      *ZEROS
     C*** END CHG *** 04/03/03 ****************************************
     C                   SETON                                        9076
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCPOS IFEQ BLANKS
     C*
     C                   MOVE(P)   SCPOS         KKPOS
     C     PSKY          CHAIN     PPS01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9077
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C*
     C* Service Date must be entered
     C*
     C     SCFDAT        IFEQ      *ZEROS
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
     C                   SETON                                        9075
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLFDAT IFLT HLACDT
     C*
     C* BASE THE EDIT OF PREVENTING FUTURE SERVICE DATES ON CONTROL
     C* FLAG SFBPF/FBAL04 POSTION 8 (BLANK = EDIT, Y = ALLOW FUTURE DATES)
     C*
     C                   MOVE      ' '           XXFSDF            1
     C                   MOVEL     'FB'          XSKEY
     C     FBKY          CHAIN     SFB01                              99
     C     *IN99         IFEQ      *OFF
     C     1             SUBST     FBAL04:8      XXFSDF
     C     XXFSDF        IFNE      'Y'
     C*
     C     HLFDAT        IFGT      HLSYDT
     C                   SETON                                        9030
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLFDAT GT HLSYDT
     C*
     C                   ENDIF                                                  XXFSDF = *BLANKS
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C* Service Code must be entered
     C*
     C     SCRVCD        IFEQ      *ZEROS
     C     SCHCPC        ANDEQ     *BLANKS
     C                   SETON                                        9078
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCRVCD = *ZEROS
     C*
     C* Description is required
     C*
     C                   MOVE      SCRVCD        K1RVCD
     C                   MOVE      SCHCPC        K1HCPC
     C                   MOVE      *ZEROS        K1UNTS
     C                   Z-ADD     SCFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         K1EFDT
     C     SVKY1         SETGT     PSV01
     C     SVKY2         READPE    PSV01                                  99
     C     *IN99         IFEQ      *ON
     C*** BEG DEL *** 02/16/99 ***************************************
     C********** SCCDDS    ANDEQ*BLANKS
     C**********           SETON                     9079
     C**********           GOTO CK1END
     C*** END DEL *** 02/16/99 ***************************************
     C                   ELSE
     C     SVSTAT        IFEQ      'I'
     C     SCRVCD        IFNE      *ZEROS
     C                   SETON                                        9087
     C                   ENDIF
     C     SCHCPC        IFNE      *BLANKS
     C                   SETON                                        9089
     C                   ENDIF
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL(P)  SVDESC        SCCDDS
     C                   ENDIF                                                  SVSTAT = 'I'
     C                   ENDIF                                                  *IN99 = *OFF
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
     C                   SETON                                        9085
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C********** SVEFDT    IFLT RUEFDT
     C**********           SETON                     9086
     C**********           GOTO CK1END
     C**********           ENDIF                           SVEFDT LT RUEFDT
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C*
     C* Charges are required
     C*
     C*** BEG ADD *** 04/01/99 ****************************************
     C*
     C* IF ADJUDICATION CODE IS OVERRIDE TYPE, ALLOW ZERO CHARGE AMOUNT
     C*
     C     CSOVRD        IFNE      'Y'
     C*** END ADD *** 04/01/99 ****************************************
     C     SCCHRG        IFEQ      *ZEROS
     C                   SETON                                        9080
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCCHRG IFEQ ZEROS
     C*** BEG ADD *** 04/01/99 ****************************************
     C                   ENDIF                                                  CSOBRD NE Y
     C*** END ADD *** 04/01/99 ****************************************
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
     C********** *IN46     IFEQ *ON
     C********** SCCSCD    IFEQ '400'
     C********** SCCSCD    OREQ '2'
     C                   MOVEL(P)  SCCSCD        KKKEY
     C     TBKY          CHAIN     PCS01                              99
     C*****      *IN99     IFEQ *OFF
     C*****      CSOVRD    ANDEQ'Y'
     C*****      CSOVRP    ANDEQ*ZEROS
     C*****      SCMRA     ANDNE*ZEROS
     C*****                Z-ADD0         HLPAID 112
     C*****      SCCHRG    SUB  SCMRA     HLPAID
     C*****                SUB  SCNTWK    HLPAID
     C*****                SUB  SCOTHR    HLPAID
     C*****                SUB  SCOTH2    HLPAID
     C*****      HLPAID    IFNE SCPAID
     C*****                SETON                     47
     C*****                GOTO CK1END
     C*****                ENDIF                           HLPAID NE SCPAID
     C*****                ENDIF                           *IN99 = *OFF
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
     C********** @@MODE    ANDEQ'C'                        DEL 03/30/99
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
     C*
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
     C*
     C     DEKY          CHAIN     PDE01                              99
     C     *IN99         DOWEQ     *OFF
     C     DEESRC        IFEQ      'A'
     C     DEEOB         ANDNE     *BLANKS
     C     DEESRC        OREQ      'N'
     C     DEEOB         ANDNE     *BLANKS
     C                   DELETE    PDE01
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
     C*** BEG CHG *** 11/12/99 ****************************************
     C******************** PARM *ZEROS    ##RRN
     C                   PARM      *ZEROS        ##RRN             9 0
     C*** END CHG *** 11/12/99 ****************************************
     C                   PARM                    ##RRN2
     C                   PARM                    ##RRN3
     C                   PARM                    ##DSEQ
     C*
     C     SCMRA         IFNE      *ZEROS
     C     SCNTWK        ORNE      *ZEROS
     C     SCOTHR        ORNE      *ZEROS
     C     SCOTH2        ORNE      *ZEROS
     C*
     C                   MOVE      *BLANKS       XXEOB
     C     DEKY          CHAIN(N)  PDE01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C     DEEOB         IFNE      *BLANKS
     C                   MOVE      DEEOB         XXEOB
     C                   LEAVE
     C                   ENDIF                                                  DEEOB NE *BLANKS
     C*
     C     DEKY          READE(N)  PDE01                                  99
     C                   ENDDO                                                  *IN99 = *OFF
     C*
     C     XXEOB         IFEQ      *BLANKS
     C                   SETON                                        90
     C                   ENDIF                                                  XXEOB = *BLANKS
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C*** BEG DEL *** 03/30/99 ****************************************
     C**********           ENDIF                           XXEOB = *BLANKS
     C*** END DEL *** 03/30/99 ****************************************
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
     C     DUKY          CHAIN     PDU01                              99
     C     *IN99         IFEQ      *OFF
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
     C*** BEG ADD *** 02/25/00 ****************************************
     C                   MOVEL     CACSCD        SCCSCD
     C                   MOVEL(P)  SCCSCD        KKKEY
     C     TBKY          CHAIN     PCS01                              99
     C  N99              MOVEL(P)  CSDESC        SCCSDS
     C*
     C*** END ADD *** 02/25/00 ****************************************
     C*
     C                   MOVE      DUPOS         SCPOS
     C                   MOVE(P)   SCPOS         KKPOS
     C     PSKY          CHAIN     PPS01                              99
     C  N99              MOVEL(P)  PSDESC        SCPODS
     C*
     C                   Z-ADD     DURVCD        SCRVCD
     C                   MOVEL(P)  DURATE        SCHCPC
     C                   Z-ADD     DUUNTS        SCUNTS
     C                   MOVEL(P)  DUDESC        SCCDDS
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
     C******************** Z-ADD@@RRN2    ##RRN   90
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
     C                   MOVE      EOAST         ##ST              2
     C                   MOVE      SCRVCD        K1RVCD
     C                   MOVE      SCHCPC        K1HCPC
     C                   MOVE      *ZEROS        K1UNTS
     C                   Z-ADD     SCFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         K1EFDT
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
     C                   SETON                                        90
     C     KIEND         ENDSR
     C*================================================================
     C* KJDO - F10=SEARCH.
     C*================================================================
     C     KJDO          BEGSR
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
     C*****                MOVELSCUNTS    HLUNTS  30
     C                   MOVEL     SCCDDS        HLCDDS           40
     C                   MOVE      EOAST         ##ST              2
     C                   MOVE      *BLANKS       ##CMSV           15
     C                   MOVE      *BLANKS       ##CDSC           40
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
     C**********           MOVELHLUNTS    SCUNTS
     C                   MOVEL(P)  HLCDDS        SCCDDS
     C                   SETON                                        9034
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##CMSV        IFNE      *BLANKS
     C                   MOVEL     ##CMSV        SCRVCD
     C     5             SUBST     ##CMSV:4      SCHCPC
     C*****      3         SUBST##CMSV:9  ZZUNTS  3
     C*****                MOVE ZZUNTS    SCUNTS
     C                   MOVEL(P)  ##CDSC        SCCDDS
     C                   SETON                                        9036
     C                   ENDIF                                                  ##CMSV NE *BLANKS
     C*
     C* HCPCS/CPT4 CODE SEARCH
     C*
     C     HFLD          WHENEQ    'SCHCPC'
     C                   MOVEL     SCRVCD        HLRVCD
     C                   MOVEL     SCHCPC        HLHCPC
     C*****                MOVELSCUNTS    HLUNTS
     C                   MOVEL     SCCDDS        HLCDDS
     C                   MOVE      EOAST         ##ST
     C                   MOVE      *BLANKS       ##CMSV
     C                   MOVE      *BLANKS       ##CDSC
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
     C**********           MOVELHLUNTS    SCUNTS
     C                   MOVEL(P)  HLCDDS        SCCDDS
     C                   SETON                                        9035
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##CMSV        IFNE      *BLANKS
     C                   MOVEL     ##CMSV        SCRVCD
     C     5             SUBST     ##CMSV:4      SCHCPC
     C*****      3         SUBST##CMSV:9  ZZUNTS
     C*****                MOVE ZZUNTS    SCUNTS
     C                   MOVEL(P)  ##CDSC        SCCDDS
     C                   SETON                                        9036
     C                   ENDIF                                                  ##CMSV NE *BLANKS
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
     C* KPDO - F15=PREVIOUS LINES
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
     C*** BEG ADD *** 02/17/99 ***************************************
     C                   CLEAR                   SCRVCD
     C*** END ADD *** 02/17/99 ***************************************
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
     C                   MOVE      *BLANKS       SCHCPC
     C                   MOVE      *BLANKS       SCCDDS
     C                   Z-ADD     0             SCCHRG
     C                   Z-ADD     0             SCPAID
     C                   Z-ADD     0             SCMRA
     C                   Z-ADD     0             SCNTWK
     C                   Z-ADD     0             SCOTHR
     C                   MOVE      *BLANKS       SCOT1C
     C                   Z-ADD     0             SCOTH2
     C                   MOVE      *BLANKS       SCOT2C
     C                   Z-ADD     0             SCUNTS
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
     C* DEFAULT UNITS TO 1
     C*
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
     C*** BEGIN ADD *** 07/23/99 **************************************
     C   44              MOVEL     '300'         SCCSCD
     C*** END ADD *** 07/23/99 ****************************************
     C                   MOVEL(P)  SCCSCD        KKKEY
     C     TBKY          CHAIN     PCS01                              99
     C  N99              MOVEL(P)  CSDESC        SCCSDS
     C*
     C* FROM FAEHP
     C*
     C********** EHKY      CHAINPEH01                99
     C********** *IN99     IFEQ *OFF
     C**********           MOVELEHDIA1    SCDIA1    P
     C**********           MOVELEHDIA2    SCDIA2    P
     C**********           MOVELEHDIA3    SCDIA3    P
     C**********           MOVELEHDIA4    SCDIA4    P
     C**********           ENDIF                           *IN99 IFEQ *ON
     C*
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
     C     @@MODE        IFNE      'A'
     C*
     C* FROM FDDUP
     C*
     C                   MOVE(P)   DUPOS         SCPOS
     C                   MOVE(P)   SCPOS         KKPOS
     C     PSKY          CHAIN     PPS01                              99
     C  N99              MOVEL(P)  PSDESC        SCPODS
     C*
     C**********           MOVELDUPRCD    SCPRCD    P
     C                   Z-ADD     DURVCD        SCRVCD
     C                   MOVEL     DURATE        SCHCPC
     C                   Z-ADD     DUUNTS        SCUNTS
     C                   MOVEL     DUDESC        SCCDDS
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
     C* DISPLAY EOB CODES FOR BILL
     C*
     C     @@MODE        IFNE      'A'
     C                   Z-ADD     0             XXSEQ#            2 0
     C     DEKY          CHAIN(N)  PDE01                              99
     C     *IN99         DOWEQ     *OFF
     C     DEEOB         IFEQ      *BLANKS
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
     C                   ENDIF                                                  DEEOB NE *BLANKS
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
     C* LOAD FIELDS FOR FDDUP
     C*
     C                   MOVE(P)   SCPOS         DUPOS
     C                   Z-ADD     SCRVCD        DURVCD
     C                   MOVEL(P)  SCHCPC        DURATE
     C*** BEG CHG *** 02/16/99 ***************************************
     C     SCCDDS        IFEQ      *BLANKS
     C                   MOVEL(P)  UNLIST        DUDESC
     C                   ELSE
     C                   MOVEL(P)  SCCDDS        DUDESC
     C                   ENDIF
     C**********           MOVELSCCDDS    DUDESC    P
     C*** END CHG *** 02/16/99 ***************************************
     C                   Z-ADD     SCUNTS        DUUNTS
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
     C                   MOVEL     SCHCPC        CACPT4
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     SCRVCD        CASVCD
      *** END ADD *** 09/21/98 ****************************************
     C*
     C                   Z-ADD     SCUNTS        CAUNTS
     C*
     C********** SCPRCD    IFGE '00100'
     C********** SCPRCD    ANDLE'01999'
     C**********           MOVE CAUNTS    CAANMN
     C**********           ENDIF
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
     C**********           MOVELSCAREA    CAPART    P
     C*
     C                   Z-ADD     SCPAID        CAPAID
     C                   MOVEL(P)  SCCSCD        CACSCD
     C                   MOVEL(P)  SCOT1C        CAOT1C
     C                   MOVEL(P)  SCOT2C        CAOT2C
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
     C**********           MOVE *HIVAL    KKLIN#
     C*** BEG DEL *** 02/23/99 ****************************************
     C********** @@RRN     CHAINDBRCDF              N99
     C**********           Z-ADDDUDSEQ    KKDSEQ
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
     C     DUKY          CHAIN     PDU01                              99
     C                   Z-ADD     DBRRNC        @@RRN
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ELSE
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C                   Z-ADD     DUDSEQ        KKDSEQ
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
     C* DIAGNOSIS CODES 2, 3 AND 4 WILL BE DISPLAYED ONLY IF EXISTS
     C* FAEHP
     C*
     C********** EHKY      CHAINPEH01                99
     C********** *IN99     IFEQ *OFF
     C**********
     C********** EHDIA2    IFNE *BLANKS
     C**********           SETON                     43
     C**********           ELSE
     C**********           SETOF                     43
     C**********           ENDIF                           EHDIA2 NE *BLANKS
     C**********
     C********** EHDIA3    IFNE *BLANKS
     C**********           SETON                     44
     C**********           ELSE
     C**********           SETOF                     44
     C**********           ENDIF                           EHDIA3 NE *BLANKS
     C**********
     C********** EHDIA4    IFNE *BLANKS
     C**********           SETON                     45
     C**********           ELSE
     C**********           SETOF                     45
     C**********           ENDIF                           EHDIA4 NE *BLANKS
     C**********
     C**********           ENDIF                           *IN99 = *OFF
     C**********
     C* DEFAULT 'X' TO FIRST DIAGNOSIS CODE IF IT IS THE ONLY
     C* DIAGNOSIS CODE
     C**********
     C********** @@MODE    IFEQ 'A'
     C********** EHDIA1    ANDNE*BLANKS
     C********** EHDIA2    ANDEQ*BLANKS
     C********** EHDIA3    ANDEQ*BLANKS
     C********** EHDIA4    ANDEQ*BLANKS
     C**********           SETON                     52
     C**********           ELSE
     C**********           SETOF                     52
     C**********           ENDIF                           @@MODE = 'A'
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
     C*
     C* SET INDICATOR TO POSITION CURSOR TO PLACE OF SERVICE FIELD
     C* IF IN ADD MODE
     C*
     C     @@MODE        IFEQ      'A'
     C                   SETON                                        32
     C                   ELSE
     C                   SETOFF                                       32
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
     C********** SCCSCD    IFNE '1'
     C**********           Z-ADD0         CATOTA
     C**********           ENDIF                           SCCSCD = 1
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
     C**********           SETON                     90
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
     C                   MOVE      'S'           CARCFL
     C*
     C*** BEGIN CHG *** 04/08/02 **************************************
     C********** EOBTYP    IFEQ 'N'
     C**********           MOVE '1'       CAPRTY
     C**********           ENDIF
     C*
     C********** EOBTYP    IFEQ 'H'
     C**********           MOVE '4'       CAPRTY
     C**********           ENDIF
     C                   MOVEL(P)  '4'           CAPRTY
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
     C                   FEOD      FDDUP
     C                   SETON                                        32
     C                   ENDSR
     OPMG01     E            RPMG01
     OPDE01     E            RPDE01
