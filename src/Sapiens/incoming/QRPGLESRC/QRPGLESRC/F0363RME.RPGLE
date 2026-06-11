     H*****************************************************************
     H*Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H*****************************************************************
     H/TITLE F0363RME - Hospital UB Detail Information
     H*****************************************************************
     H*
     H* F0363RME - Hospital UB Detail Information
     H*
     H*  Created: 06/14/13   Programmer C:277   PROJECT: ME005
     H*****************************************************************
     H*  REVISIONS:
     H*
     H*  07/18/23 C:342 Project: NY060
     H*                 NY Compliance: CARC/RARC Update
     H*
     H*  10/13/15 C:307 Support: PHW37547
     H*                 Corrected error indicator
     H*
     H*****************************************************************
     FF0363FME  CF   E             WORKSTN
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
     F                                     INFDS(DBFEO)
     F*
     F*  Type of Service File
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
     F*  EOMB Phys/HCFA Header File
     F*
     F*  Diagnosis Code Physical File
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
     FFMPRL     IF   E           K DISK
     F                                     INFDS(DBFPR)
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
     FFMSVL5TX  IF   E           K DISK
     F                                     RENAME(PSV01:TXSV05)
     FFMPRLTX   IF   E           K DISK
     F                                     RENAME(PPR01:TXPR01)
     FFTAPL1ME  IF   E           K DISK
     F                                     INFDS(DSAPL)
     FFTOPL     IF   E           K DISK
     F                                     INFDS(DSOPL)
     FFTDGL1ME  IF   E           K DISK
     F                                     INFDS(DSDGL)
     FFTPFL     IF   E           K DISK
     F                                     INFDS(DSIIL)
     FFTRVL1    IF   E           K DISK
     F                                     INFDS(DSRVL)
     FFTLBL1    IF   E           K DISK
     F                                     INFDS(DSLBL)
     FFTAPPME   IF   E             DISK
     F                                     RENAME(PAP01:PAP0P)
     FFMRCP     IF   E             DISK
     F                                     RENAME(PRC01:PRC0P)
     FFMASP     IF   E             DISK
     FFMASL1    IF   E           K DISK
     F                                     INFDS(DBFAS)
     F                                     RENAME(PAS01:PAS02)
     FFMFNL     IF   E           K DISK
     FFMAML2    IF   E           K DISK
     FFMDFL1    IF   E           K DISK
     F                                     INFDS(DBDRG)
     FFMDEL2    IF   E           K DISK
     F                                     RENAME(PDE01:PDM02)
     FFTLBL2    IF   E           K DISK
     F                                     RENAME(PLB01:PLB02)
     FFTRVL3    IF   E           K DISK
     F                                     RENAME(PRV01:PRV03)
     FFTRTL2    IF   E           K DISK
     FFMHPP     IF   E             DISK
     FFMHPL1    IF   E           K DISK
     F                                     RENAME(PHP01:PHP02)
     Ffdcal24   IF   E           K DISK
     F                                     RENAME(pca01:pca05)
     FFMMGL1    IF   E           K DISK
     F                                     INFDS(DBFCMG)
     F                                     RENAME(PMG01:PCMG02)
     F                                     PREFIX(C)
     FFMIRL1    IF   E           K DISK
     F                                     INFDS(DBIRP)
     FFmhlp     if   e             disk
     F                                     rename(phl01:phl02) Prefix(h2:2)
     F*
     FFmhll1    if   e           k disk    rename(phl01:phl03) Prefix(h3:2)
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
     D DSOPL           DS
     D  OPRRN                397    400B 0
     D DSDGL           DS
     D  DGRRN                397    400B 0
     D DSIIL           DS
     D  IIRRN                397    400B 0
     D DBFEO           DS
     D  EORRN                397    400B 0
     D DSRVL           DS
     D  RVRRN                397    400B 0
     D DSLBL           DS
     D  LBRRN                397    400B 0
     D DBDRG           DS
     D  DRRRN                397    400B 0
     D                 DS
     D  K1CMSV                 1     11
     D  K1RVCD                 1      3
     D  K1HCPC                 4      8
     D  K1UNTS                 9     11
     D*
     D                 DS
     D  TMTOS                  1      5
     D  HLKEY                  1      2
     D  HDKEY                  3      5
     D TMSMSA          DS
     D  TMSVCD                 1      3
     D  TMCMSV                 4      8
     D                 DS
     D  EUBLTY                 1      3
     D  XXBLTY                 1      2
     D*
     D DBFAS           DS
     D  ASRRN                397    400B 0
     D*
     D DBFCMG          DS
     D  CMGRRN               397    400B 0
     D DBIRP           DS
     D  IRRRN                397    400B 0
     D*
     D xxchar          S              1
     D Edtcde          S              1    Inz('Y')
     D cnt             S              1  0
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Hospital (UB) Detail-
     D                                      Information')
     D UNLIST          C                   CONST('* UNLISTED *')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)
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
     D xxHcpc          s                   like(scHcpc)
     C*
     C/COPY SCOPYMT01C
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
     C     CAKY5         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
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
      *
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
     C*
     C*  FS BASE CONTROL FILE
     C*
     C*
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
     C*          Provider Master File
     C*          ------------------------------------------------------
     C*
     C*          Provider Network Contract File
     C*          ------------------------------------------------------
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
     C     PRKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKRATE
     C                   KFLD                    KKFDAT
     C*
     C     PRKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKRATE
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
     C     RVKY          KLIST
     C                   KFLD                    KKPRCD
     C*
     C     ASKY          KLIST
     C                   KFLD                    KKHCPC
     C                   KFLD                    KKEFDT
     C*
     C     ASKY1         KLIST
     C                   KFLD                    KKHCPC
     C*
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C     RTKY          KLIST
     C                   KFLD                    ###ST
     C                   KFLD                    ##TYP
     C                   KFLD                    ##EFDT            7 0
     C*
     C     RTKY1         KLIST
     C                   KFLD                    ###ST
     C                   KFLD                    ##TYP
     C     CMGKY         KLIST
     C                   KFLD                    KKHCPC
     C                   KFLD                    KKTIER
     C                   KFLD                    KKEFDT
     C*
     C     CMGKY1        KLIST
     C                   KFLD                    KKHCPC
     C                   KFLD                    KKTIER
     C*
     C     IRKY1         KLIST
     C                   KFLD                    K1PROV
     C                   KFLD                    KPEFDT
     C*
     C     IRKY2         KLIST
     C                   KFLD                    K1PROV
     C*
     C     DGKY          KLIST
     C                   KFLD                    KKDRG
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
     C     *LIKE         DEFINE    RVPRCD        KKPRCD
     C     *LIKE         DEFINE    STMFS         XXMFS
     C     *LIKE         DEFINE    STMPPO        XXMPPO
     C     *LIKE         DEFINE    CASRC         XXSRC
     C     *LIKE         DEFINE    RTMREL        XXMREL
     C     *LIKE         DEFINE    RTEFDT        XXEFDT
     C     *LIKE         DEFINE    RTTYPE        ##TYP
     C     *LIKE         DEFINE    RTST          ###ST
     C     *LIKE         DEFINE    CMGTIER       KKTIER
     C     *LIKE         DEFINE    DGDRG         KKDRG
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
      *
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
     C* Revenue Code must be entered
     C*
     C*
     C* CA: HCPC code must be entered
     C*
     C     EOAST         IFEQ      'CA'
     C     SCHCPC        ANDEQ     *BLANKS
     C     *IN46         ANDEQ     *OFF
     C                   SETON                                        9078
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C*
     C* AR: HCPC code must be entered
     C*
     C     EOAST         IFEQ      'AR'
     C     SCHCPC        ANDEQ     *BLANKS
     C     EUICN         IFEQ      'AO'
     C     EUICN         OREQ      'AS'
     C     EUICN         OREQ      'HO'
     C     EUICN         OREQ      'HS'
     C                   SETON                                        907835
     C                   GOTO      CK1END
     C                   ENDIF                                                              ROS
     C                   ENDIF
     C*
     C*
     C* AR: HCPC code must be entered
     C*
     C     EOAST         IFEQ      'CT'
     C     SCHCPC        ANDEQ     *BLANKS
     C     EUICN         IFEQ      'AO'
     C     EUICN         OREQ      'AS'
     C     EUICN         OREQ      'HO'
     C     EUICN         OREQ      'HS'
     C                   SETON                                        907835
     C                   GOTO      CK1END
     C                   ENDIF                                                              ROS
     C                   ENDIF
     C*
     C*
     C* TX: HCPC or Revenue code must be entered
     C*
     C     EOAST         IFEQ      'TX'
     C     SCHCPC        ANDEQ     *BLANKS
     C     SCRVCD        ANDEQ     *ZEROS
     C                   SETON                                        9095
     C                   GOTO      CK1END
     C                   ENDIF
     C*
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
      *
     C*
     C* Service Code must be entered if CA and EUICN = IM-IS
     C*
     C     SCRVCD        IFEQ      *ZEROS
     C     SCAST         ANDEQ     'CA'
     C     EUICN         ANDGE     'IM'
     C     EUICN         ANDLE     'IS'
     C                   SETON                                        9091
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCRVCD = *ZEROS
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
     C                   MOVE      SCHCPC        K1HCPC
     C                   MOVE      *ZEROS        K1UNTS
     C                   Z-ADD     SCFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         K1EFDT
     C*
     C* Revenue code  valid and active
      *
     C     @EDTRC        IFEQ      'Y'
     C     SVKY3         SETGT     TXSV05                             99
      *
     C     *IN99         IFEQ      *ON
     C     *HIVAL        SETGT     TXSV05
     C                   ENDIF
      *
     C     SVKY4         READPE    TXSV05                                 99
     C*
     C     *IN99         IFEQ      *OFF
      *
     C     *IN99         DOWEQ     *OFF
     C     SVSTAT        ANDNE     'A'
     C     SVKY4         READPE    TXSV05                                 99
     C                   ENDDO
      *
     C     SVEFDT        IFGT      K1EFDT
     C     *IN99         OREQ      *ON
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN93
     C                   GOTO      CK1END
     C                   ENDIF
      *
     C                   ELSE
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN93
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
      * Check COMMON SERVICE CODE for outpatient bills.
      *
      *
      *
      * HCPC Code - Valid and Active when entered.
      *
      /Free
       If ##clt# = '114' and sccscd = '2'
        and (eoast  = 'P1' or eoast = 'P2')
        and hlfdat < 1100101;
           Edtcde = 'N';
       Else;
           Edtcde = 'Y';
       Endif;

       If Edtcde = 'Y';
      /End-Free
      /Free
         //If State is 'HL
         If Eoast = 'HL';

            Chain scHcpc phl03;
               If not %Found(Fmhll1);
                 *in94 = *on;
                 *in90 = *on;
               Else;

                 If H3Stat = 'I';
                   *in89 = *on;
                   *in90 = *on;
                 Endif;

               scCdds = %subst(H3Desc:1:25);
               Endif;
         Else;

      /End-Free
     C*
     C                   SELECT
     C                   When      (XXBLTY='21' OR XXBLTY= '18')
     C                             AND XXMFS ='Y' AND SCRVCD = 22
     C*
     C                   IF        SCHCPC  <> *BLANKS
     C                   EXSR      CKSNF
     C   90              GOTO      CK1END
     C                   ENDIF
     C*
     C                   When      euicn = 'IR' and scrvcd = 24
     C                             and xxmfs ='Y' and xxblty = '11'
     C*
     C                   if        schcpc = *blanks
     C                   SETON                                        907835
     C   90              goto      ck1end
     C                   else
     C                   exsr      ckcmg
     C   90              goto      ck1end
     C                   endif
     C*
     C     EODSTY        WHENEQ    'DR'
     C     EUICN         ANDEQ     'AO'
     C     EODSTY        OREQ      'DR'
     C     EUICN         ANDEQ     'AS'
     C     EODSTY        OREQ      'DR'
     C     EUICN         ANDEQ     'HO'
     C     EODSTY        OREQ      'DR'
     C     EUICN         ANDEQ     'HS'
     C     EODSTY        OREQ      'DS'
     C     EUICN         ANDEQ     'AO'
     C     EODSTY        OREQ      'DS'
     C     EUICN         ANDEQ     'AS'
     C     EODSTY        OREQ      'DS'
     C     EUICN         ANDEQ     'HO'
     C     EODSTY        OREQ      'DS'
     C     EUICN         ANDEQ     'HS'
     C     XXMFS         OREQ      'Y'
     C*          XXMPPO    OREQ 'Y'
     C*
     C     XXMPPO        IFEQ      'Y'
     C     XXBLTY        ANDEQ     '83'
     C     EOAST         ANDEQ     'NJ'
     C     EOSPCD        OREQ      'ASC'
     C*
     C     SCHCPC        IFNE      *BLANKS
     C                   EXSR      CKASC
     C                   ENDIF
     C*
     C                   ELSE
     C     SCHCPC        IFNE      *BLANKS
     C                   EXSR      CKAPC
     C                   ENDIF
     C                   ENDIF
     C     EODSTY        WHENEQ    'DR'
     C     EUICN         ANDEQ     'IM'
     C     EODSTY        OREQ      'DR'
     C     EUICN         ANDEQ     'IP'
     C     EODSTY        OREQ      'DR'
     C     EUICN         ANDEQ     'IR'
     C     EODSTY        OREQ      'DR'
     C     EUICN         ANDEQ     'IS'
     C     EODSTY        OREQ      'DS'
     C     EUICN         ANDEQ     'IM'
     C     EODSTY        OREQ      'DS'
     C     EUICN         ANDEQ     'IP'
     C     EODSTY        OREQ      'DS'
     C     EUICN         ANDEQ     'IR'
     C     EODSTY        OREQ      'DS'
     C     EUICN         ANDEQ     'IS'
     C     XXMFS         OREQ      'Y'
     C     SCHCPC        IFNE      *BLANKS
     C                   EXSR      CKRC
     C                   ENDIF
     C                   OTHER
     C     EOAST         IFEQ      'TX'
     C     SCHCPC        ANDNE     *BLANKS
     C                   MOVE      SCHCPC        KKRATE
     C                   MOVE      K1EFDT        KKFDAT
     C     PRKY          SETGT     TXPR01                             99
      *
     C     *IN99         IFEQ      *ON
     C     *HIVAL        SETGT     TXPR01
     C                   ENDIF
      *
     C     PRKY1         READPE    TXPR01                                 99
     C*
     C     *IN99         IFEQ      *OFF
      *
     C     *IN99         DOWEQ     *OFF
     C     PRSTAT        ANDNE     'A'
     C     PRKY1         READPE    TXPR01                                 99
     C                   ENDDO
      *
     C     PREFDT        IFGT      KKFDAT
     C     *IN99         OREQ      *ON
     C     EUICN         IFEQ      'AO'
     C     EUICN         OREQ      'AS'
     C     EUICN         OREQ      'HO'
     C     EUICN         OREQ      'HS'
     C                   EXSR      CKAP
     C   90              GOTO      CK1END
     C                   ELSE
     C     @EDTRC        IFEQ      'Y'
     C                   EXSR      CKUC
     C   90              GOTO      CK1END
     C                   ENDIF                                                  @EDTRC = 'Y'
     C                   ENDIF                                                  BILL IS OUTP
     C                   ENDIF
      *
     C                   ELSE
     C     EUICN         IFEQ      'AO'
     C     EUICN         OREQ      'AS'
     C     EUICN         OREQ      'HO'
     C     EUICN         OREQ      'HS'
     C                   EXSR      CKAP
     C   90              GOTO      CK1END
     C                   ELSE
     C     @EDTRC        IFEQ      'Y'
     C                   EXSR      CKUC
     C   90              GOTO      CK1END
     C                   ENDIF                                                  @EDTRC = 'Y'
     C                   ENDIF                                                  BILL IS OUTP
     C                   ENDIF
      *
     C                   ENDIF
     C                   ENDSL                                                  EODSTY = DR/DS
     C     EOAST         IFEQ      'GA'
     C*
     C     SCHCPC        IFGE      '70010'
     C     SCHCPC        ANDLE     '79999'
     C*
     C     SCHCPC        ORGE      '97001'
     C     SCHCPC        ANDLE     '97799'
     C*
     C     SCHCPC        ORGE      '98925'
     C     SCHCPC        ANDLE     '98943'
     C*
     C                   MOVE      *ZEROS        K1RVCD
     C*
     C                   ENDIF                                                  SCHCPC = 97001
     C*
     C                   ENDIF                                                  EOAST = GA
     C                   EXSR      CKSRC
     C*
     C     SVKY1         SETGT     PSV01
     C     SVKY2         READPE    PSV01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  SVDESC        SCCDDS
     C                   ENDIF                                                  IN99 = OFF
     C*
     C** hard code revenue code description for SNF bills
     C                   if        (xxblty = '18'or xxblty = '21') and
     C                             xxmfs = 'Y' and scrvcd = 1
     C                   eval      sccdds = 'Total charge for all services'
     C                   endif
     C*
     C                   if        EOAST = 'ME' and SCHCPC <> *blanks
     C                             and (EUICN ='IM' or EUICN = 'IS' or
     C                             EUICN ='IP' or EUICN = 'IR')
     C                   eval      kkdrg = %Int(schcpc)
     C     dgky          chain     PDG01
     C                   if        %found(ftdgl1me)
     C                   movel     DGdesc        sccdds
     C                   else
     C                   movel     *blanks       sccdds
     C                   endif
     C*
     C                   endif
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
      /Free
         Endif;
         Endif;
      /End-free
     C*
     C*
     C* Charges are required
     C*
     C                   IF        (XXBLTY = '18' or XXBLTY = '21' )
     C                             and  scrvcd = 22 and xxmfs = 'Y'
     C                             and scchrg <> *zeros
     C                   SETON                                        9004
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C* IF ADJUDICATION CODE IS OVERRIDE TYPE, ALLOW ZERO CHARGE AMOUNT
     C* OR IF GEORGIA OUTPATIENT BILL USING "P" CODES
     C*
     C     CSOVRD        IFNE      'Y'
     C     SCAST         IFEQ      'GA'
     C     SCHCPC        ANDGE     'P0000'
     C     SCHCPC        ANDLE     'P9999'
     C     SCPOS         ANDEQ     22
     C*
     C     SCAST         OREQ      'GA'
     C     SCHCPC        ANDGE     'P0000'
     C     SCHCPC        ANDLE     'P9999'
     C     SCPOS         ANDEQ     24
     C*
     C                   ELSE
     C*
     C                   IF        (XXBLTY = '18' or XXBLTY = '21' )
     C                             and  scrvcd = 22 and xxmfs = 'Y'
     C                   ELSE
     C     SCCHRG        IFEQ      *ZEROS
     C                   SETON                                        9080
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCCHRG IFEQ ZEROS
     C                   ENDIF                                                  xxblty = 18/21
     C                   ENDIF                                                  SCAST  EQ GA
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
     C     ##CLT#        OREQ      '133'
     C     SCCSCD        ANDEQ     'DRG'
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
     C     ##CLT#        IFNE      '133'
     C     SCCSCD        ANDNE     'DRG'
     C     SCCHRG        IFEQ      *ZEROS
     C     CSOVRD        ANDEQ     'Y'
     C     SCPAID        ANDNE     *ZEROS
     C     SCCHRG        SUB       SCPAID        SCMRA
     C                   Z-ADD     0             SCOTHR
     C                   Z-ADD     0             SCOTH2
     C                   Z-ADD     0             SCNTWK
     C                   ENDIF                                                  SCCHRG NE *ZEROS
     C                   ENDIF
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
     C*
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
     C*
     C*** REMOVE ANY EOB CODES AND MESSAGES PRODUCED BY ADJUDICATION
     C*
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
     C*
     C*
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
     C*
     C                   ENDIF                                                  GO = *YES
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C**PROGRAMS WILL DETERMINE THE NECESSARY REDUCTIONS.
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
     C*  CALL FA713R WHICH WILL CALL APPROPRIATE ADJUDICATION PROGRAMS
     C**WRITE/UPDATE*EOMB*DETAIL AMOUNT TRANSACTION FILE
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
     C*
     C* Chain without rev code for GA PT and Rad codes
     C*
     C     SCAST         IFEQ      'GA'
     C     SCHCPC        ANDGE     '70010'
     C     SCHCPC        ANDLE     '79999'
     C*
     C     SCAST         OREQ      'GA'
     C     SCHCPC        ANDGE     '97001'
     C     SCHCPC        ANDLE     '97799'
     C*
     C     SCAST         OREQ      'GA'
     C     SCHCPC        ANDGE     '98925'
     C     SCHCPC        ANDLE     '98943'
     C*
     C     SCAST         OREQ      'MS'
     C     SCHCPC        ANDGE     '10000'
     C     SCHCPC        ANDLE     '99999'
     C*
     C                   MOVE      *ZEROS        K1RVCD
     C*
     C                   ENDIF
     C                   MOVE      '1'           SCPCF
     C                   MOVE      *BLANK        SCSCF
     C                   MOVE      *BLANK        SCDRI
     C                   MOVE      *BLANK        SCRVU
     C                   MOVE      *BLANK        SCLAB
     C                   MOVE      *BLANK        SCIMP
     C                   MOVE      *BLANK        SCAPC
     C                   MOVE      *BLANK        SCDRG
     C                   MOVE      *BLANK        SCPPS
     C                   MOVE      *BLANK        SCPPI
     C                   MOVE      *BLANK        SCRCF
     C                   MOVE      *BLANK        SCASCF
     C                   MOVE      *BLANK        SCASPF
     C                   eval      SCSNF = *blanks
     C                   if        xxmfs = 'Y' and (xxblty ='18' or
     C                             xxblty ='21') and scrvcd = 22
     C                   eval      *in79 = *on
     C                   goto      wdw
     C                   else
     C                   eval      *in79 = *off
     C                   endif
     C*** if FAEOP/EODSTY = 'DR/DS'
     C*** display only 2 options in case of outpatients
     C     EODSTY        IFEQ      'DR'
     C     EODSTY        OREQ      'DS'
     C     XXMFS         OREQ      'Y'
     C     XXMPPO        OREQ      'Y'
     C                   EXSR      PRCWDW
     C                   ELSE
     C     WDW           TAG
     C     EOAST         IFEQ      'TX'
     C                   SETON                                        67
     C                   ELSE
     C                   SETOFF                                       67
     C                   ENDIF
     C     EOAST         IFEQ      'AR'
     C                   SETON                                        57
     C                   ELSE
     C                   SETOFF                                       57
     C                   ENDIF
     C*
     C*** Medicare APC file must be shown when hospital bill is outpatient
     C*
     C** *in41 for display of medicare APC file
     C     EUICN         IFEQ      'AO'
     C     EUICN         OREQ      'AS'
     C     EUICN         OREQ      'HO'
     C     EUICN         OREQ      'HS'
     C                   SETON                                        41
     C                   ELSE
     C                   SETOFF                                       41
     C                   ENDIF                                                  EUICN = outpat
     C                   EXFMT     WDW01
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C*
     C*
     C*
     C*
     C*
     C*
     C*
     C*
     C                   SETOFF                                         66
     C*
     C     SCPCF         IFEQ      '1'
     C                   SETON                                          66
     C                   ENDIF
     C*
     C*
     C   41
     CAN 67SCAPC         IFEQ      '1'
     C     *IN66         IFEQ      *ON
     C                   SETON                                          71
     C                   GOTO      WDW
     C                   ENDIF                                                  *IN80 EQ *ON
     C                   SETON                                          66
     C                   ENDIF                                                  SCIMP EQ '1'
     C*
     C   67SCRCF         IFEQ      '1'
     C     *IN66         IFEQ      *ON
     C                   SETON                                          44
     C                   GOTO      WDW
     C                   ENDIF                                                  *IN80 EQ *ON
     C                   SETON                                          66
     C                   ENDIF                                                  SCRCF EQ '1'
     C*
     C     SCSCF         IFEQ      '1'
     C     *IN66         IFEQ      *ON
     C                   SETON                                          71
     C                   GOTO      WDW
     C                   ENDIF                                                  *IN80 EQ *ON
     C                   SETON                                          66
     C                   ENDIF                                                  SCSCF EQ '1'
     C*
     C     SCDRI         IFEQ      '1'
     C     *IN66         IFEQ      *ON
     C                   SETON                                          71
     C                   GOTO      WDW
     C                   ENDIF                                                  *IN80 EQ *ON
     C                   SETON                                          66
     C                   ENDIF                                                  SCSCF EQ '1'
     C     SCRVU         IFEQ      '1'
     C     *IN66         IFEQ      *ON
     C                   SETON                                          71
     C                   GOTO      WDW
     C                   ENDIF                                                  *IN80 EQ *ON
     C                   SETON                                          66
     C                   ENDIF                                                  SCSCF EQ '1'
     C     SCLAB         IFEQ      '1'
     C     *IN66         IFEQ      *ON
     C                   SETON                                          71
     C                   GOTO      WDW
     C                   ENDIF                                                  *IN80 EQ *ON
     C                   SETON                                          66
     C                   ENDIF                                                  SCSCF EQ '1'
     C*
     C     *IN67         IFEQ      '1'
     C     SCIMP         IFEQ      '1'
     C     *IN66         IFEQ      *ON
     C                   SETON                                          71
     C                   GOTO      WDW
     C                   ENDIF                                                  *IN80 EQ *ON
     C                   SETON                                          66
     C                   ENDIF                                                  SCIMP EQ '1'
     C*
     C                   ENDIF                                                  *IN67 = '1'
     C*
     C     *IN67         IFEQ      '1'
     C     SCPPS         IFEQ      '1'
     C     *IN66         IFEQ      *ON
     C                   SETON                                          71
     C                   GOTO      WDW
     C                   ENDIF                                                  *IN80 EQ *ON
     C                   SETON                                          66
     C                   ENDIF                                                  SCIMP EQ '1'
     C*
     C     SCDRG         IFEQ      '1'
     C     *IN66         IFEQ      *ON
     C                   SETON                                          71
     C                   GOTO      WDW
     C                   ENDIF                                                  *IN80 EQ *ON
     C                   SETON                                          66
     C                   ENDIF                                                  SCIMP EQ '1'
     C                   ENDIF                                                  *IN67 = '1'
     C*
     C*
     C     SCPPI         IFEQ      '1'
     C     *IN66         IFEQ      *ON
     C                   SETON                                          71
     C                   GOTO      WDW
     C                   ENDIF                                                  *IN80 EQ *ON
     C                   SETON                                          66
     C                   ENDIF                                                  SCIMP EQ '1'
     C*
     C     SCSNF         IFEQ      '1'
     C     *IN66         IFEQ      *ON
     C                   SETON                                          71
     C                   GOTO      WDW
     C                   ENDIF                                                  *IN66 EQ *ON
     C                   SETON                                          66
     C                   ENDIF                                                  SCSNF EQ '1'
     C*
     C                   SELECT
     C     SCRCF         WHENNE    '1'
     C     SCRCF         ANDNE     *BLANK
     C   67              SETON                                          46
     C   67              GOTO      WDW
     C*
     C     SCSCF         WHENNE    '1'
     C     SCSCF         ANDNE     *BLANK
     C                   SETON                                          72
     C                   GOTO      WDW
     C*
     C     SCDRI         WHENNE    '1'
     C     SCDRI         ANDNE     *BLANK
     C                   SETON                                          45
     C                   GOTO      WDW
     C     SCRVU         WHENNE    '1'
     C     SCRVU         ANDNE     *BLANK
     C                   SETON                                          72
     C                   GOTO      WDW
     C     SCLAB         WHENNE    '1'
     C     SCLAB         ANDNE     *BLANK
     C                   SETON                                          72
     C                   GOTO      WDW
     C     SCPCF         WHENNE    '1'
     C     SCPCF         ANDNE     *BLANK
     C                   SETON                                          73
     C                   GOTO      WDW
     C*
     C     SCIMP         WHENNE    '1'
     C     SCIMP         ANDNE     *BLANK
     C   67              SETON                                          61
     C                   GOTO      WDW
     C*
     C     SCAPC         WHENNE    '1'
     C     SCAPC         ANDNE     *BLANK
     C   67
     CAN 41              SETON                                          62
     C                   GOTO      WDW
     C*
     C*
     C     SCPPS         WHENNE    '1'
     C     SCPPS         ANDNE     *BLANK
     C   67              SETON                                          63
     C                   GOTO      WDW
     C*
     C     SCDRG         WHENNE    '1'
     C     SCDRG         ANDNE     *BLANK
     C   67              SETON                                          64
     C                   GOTO      WDW
     C*
     C*
     C     SCPPI         WHENNE    '1'
     C     SCPPI         ANDNE     *BLANK
     C   67              SETON                                          65
     C                   GOTO      WDW
     C     SCSNF         WHENNE    '1'
     C     SCSNF         ANDNE     *BLANK
     C                   if        xxmfs = 'Y' and (xxblty ='18' or
     C                             xxblty ='21') and scrvcd = 22
     C                   SETON                                          77
     C                   GOTO      WDW
     C                   ENDIF
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
     C     SCPCF         WHENEQ    '1'
     C*
     C                   Z-ADD     CAFDAT        KKFDAT
     C                   MOVEL     DURATE        KKRATE
     C*
     C     CAFDAT        IFLT      1080301
     C     EOBTYP        IFNE      'H'
     C     DURATE        IFGE      '00100'
     C     DURATE        ANDLE     '01999'
     C*
     C     DURATE        ORGE      '80000'
     C     DURATE        ANDLE     '89999'
     C*
     C                   MOVE      SCZIPA        CAZIPA
     C                   MOVE(P)   CAZIPA        KKZIPA
     C*
     C     RCKY          SETGT     PRC01
     C     RCKY1         READPE    PRC01                                  99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRN
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     RCRRN         ##RRN
     C                   ENDIF                                                  *IN99 = On
     C*
     C                   CALL      'F2310R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRN
     C                   PARM                    ##ST
     C*
     C                   ELSE
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRN
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     PRRRN         ##RRN
     C                   ENDIF                                                  *IN99 = On
     C*
     C                   CALL      'F2305R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRN
     C                   PARM                    ##ST
     C*
     C                   ENDIF
     C                   ENDIF                                                  EOBTYP <> 'H'
     C                   ELSE
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRN
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     PRRRN         ##RRN
     C                   ENDIF                                                  *IN99 = On
     C*
     C                   CALL      'F2305R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRN
     C                   PARM                    ##ST
     C*
     C                   ENDIF
     C*
     C     SCRCF         WHENEQ    '1'
     C*
     C     *IN67         IFEQ      *ON
     C*
     C                   Z-ADD     CAFDAT        KKFDAT
     C                   MOVEL     DURATE        KKRATE
     C*
     C                   MOVE      SCZIPA        CAZIPA
     C                   MOVE(P)   CAZIPA        KKZIPA
     C*
     C     RCKY          SETGT     PRC01
     C     RCKY1         READPE    PRC01                                  99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRN
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     RCRRN         ##RRN
     C                   ENDIF                                                  *IN99 = On
     C*
     C                   CALL      'F2310R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRN
     C                   PARM                    ##ST
     C*
     C                   ENDIF                                                  *IN67 & *IN47
     C     SCIMP         WHENEQ    '1'
     C*
     C     *IN67         IFEQ      *ON
     C                   Z-ADD     CAFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYM1
     C                   MOVEL     SRDTO         KPEFDT
     C     EUKY          CHAIN(N)  PEU01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     EUMCAR        KKPROV
     C                   ENDIF
     C     OPKY1         SETGT     POP01
     C     OPKY2         READPE    POP01                                  99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRN
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     OPRRN         ##RRN
     C                   ENDIF                                                  *IN99 = On
     C                   CALL      'F2357R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRN
     C                   ENDIF                                                  *IN67 = *ON
     C*
     C     SCAPC         WHENEQ    '1'
     C*
     C     *IN67         IFEQ      *ON
     C     *IN41         ANDEQ     *ON
     C                   Z-ADD     CAFDAT        KKEFDT
     C                   MOVEL     DURATE        KAHCPC
     C*
     C     APKY1         SETGT     PAP01
     C     APKY2         READPE    PAP01                                  99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRN
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     APRRN         ##RRN
     C                   ENDIF                                                  *IN99 = On
     C                   CALL      'F2553R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRN
     C                   ENDIF                                                  *IN67 = *ON
     C     SCPPS         WHENEQ    '1'
     C     *IN67         IFEQ      *ON
     C*
     C                   Z-ADD     CAFDAT        K1EFDT
     C     EUKY          CHAIN(N)  PEU01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     EUMCAR        K1PROV
     C                   ENDIF
     C*
     C                   Z-ADD     EUADDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM1
     C                   MOVEL     SRDTO         KPEFDT
     C*
     C     IIKY1         SETGT     PTP01
     C     IIKY2         READPE    PTP01                                  99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRN
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     IIRRN         ##RRN
     C                   ENDIF                                                  *IN99 = On
     C                   CALL      'F2356R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRN
     C                   ENDIF                                                  *IN67 = *ON
     C*
     C     SCDRG         WHENEQ    '1'
     C     *IN67         IFEQ      *ON
     C*
     C                   Z-ADD     CAFDAT        K1EFDT
     C                   Z-ADD     EUDRG#        K1HDRG
     C*
     C     DGKY1         SETGT     PDG01
     C     DGKY2         READPE    PDG01                                  99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRN
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     DGRRN         ##RRN
     C                   ENDIF                                                  *IN99 = On
     C                   CALL      'F2552R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRN
     C                   ENDIF                                                  *IN67 = *ON
     C     SCPPI         WHENEQ    '1'
     C     *IN67         IFEQ      *ON
     C*
     C     EUKY          CHAIN(N)  PEU01                              60
     C     *IN60         IFEQ      *OFF
     C                   CALL      'F0364R'
     C                   PARM                    ##CO#
     C                   PARM                    KKSUB#
     C                   PARM                    KKFND
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
     C                   PARM                    KKEOB#
     C                   PARM                    SCCONM
     C                   ENDIF                                                  *IN67 = *ON
     C                   ENDIF                                                  *IN67 = *ON
     C     SCDRI         WHENEQ    '1'
     C     EUKY          CHAIN(N)  PEU01                              60
     C     *IN60         IFEQ      *OFF
     C                   CALL      'F0366R'
     C                   PARM                    ##CO#
     C                   PARM                    KKSUB#
     C                   PARM                    KKFND
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
     C                   PARM                    KKEOB#
     C                   ENDIF
     C     SCRVU         WHENEQ    '1'
     C                   MOVEL     SCHCPC        KKPRCD
     C     RVKY          CHAIN     PRV01                              99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRN
     C                   ELSE
     C                   Z-ADD     RVRRN         ##RRN
     C                   ENDIF
     C                   CALL      'F2366R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRN
     C*
     C     SCLAB         WHENEQ    '1'
     C                   MOVEL     SCHCPC        KKPRCD
     C     RVKY          CHAIN     PLB01                              99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRN
     C                   ELSE
     C                   Z-ADD     LBRRN         ##RRN
     C                   ENDIF
     C                   CALL      'F2368R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRN
     C     SCSNF         WHENEQ    '1'
     C     EUKY          CHAIN(N)  PEU01                              60
     C*
     C                   CALL      'F0368R'
     C                   PARM                    ##CO#
     C                   PARM                    KKSUB#
     C                   PARM                    KKFND
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
     C                   PARM                    KKEOB#
     C                   PARM                    SCCONM
     C*
     C                   ENDSL
     C                   ENDIF
     C                   MOVEA     '0000'        *IN(61)
     C                   MOVEA     '00000'       *IN(71)
     C                   ENDIF
     C                   SETON                                        90
     C     KIEND         ENDSR
     C*================================================================
     C* KJDO - F10=SEARCH.
     C*================================================================
     C     KJDO          BEGSR
     C*
     C*
     C*** INITIALIZE POSITION CURSOR INDICATORS
     C*
     C*
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
     C     5             SUBST     ##CMSV:4      SCHCPC
     C                   MOVEL(P)  ##CDSC        SCCDDS
     C                   SETON                                        9036
     C                   ENDIF                                                  ##CMSV NE *BLANKS
     C*
     C* HCPCS/CPT4 CODE SEARCH
     C*
     C*
     C     HFLD          WHENEQ    'SCHCPC'
     C*
      /Free

         If Eoast = 'HL' ;
           inCall= '2';
           inRrn = 0;
           inKc = *blanks;
           inKl = *blanks;
           xxHcpc = scHcpc;

           Callp RtvPrcCd(inCall:inKc:inKl:inRrn);
             If inRrn <> 0;
                Chain inRrn phl02;
                  If %found(Fmhlp);
                    scHcpc = H2Code;
                    scCdds = %subst(H2Desc:1:25);
                    *in36 = *on;
                    *in90 = *on;
                  Else;
                    scHcpc = xxHcpc;
                    *in24 = *on;
                    *in90 = *on;
                  Endif;

             Else;
               scHcpc = xxHcpc;
               *in24 = *on;
               *in90 = *on;
             Endif;

         Else;

      /End-Free
     C*
     C                   MOVE      EOAST         ##ST              2
     C                   Z-ADD     EORRN         ##RRN4            9 0
     C                   MOVE      *BLANKS       ##CMSV
     C     TMCMSV        IFNE      *BLANKS
     C                   MOVE      *BLANKS       TMSVCD
     C                   MOVEL(P)  TMSMSA        ##CMSV
     C                   ENDIF
     C*
     C*** help will be only from APC file if o/p and EODSTY = 'DR/'DS'
     C*
     C                   SELECT
     C*
     C                   when      xxmfs = 'Y' and (xxblty ='18' or
     C                             xxblty ='21') and scrvcd = 22
     C*
     C                             or (xxmfs = 'Y' and euicn = 'IR'
     C                                 and scrvcd = 24 and xxblty = '11')
     C*
     C                   CALL      'F2016R'
     C                   PARM      '1'           ##MODE
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##CMSV
     C                   PARM                    ##CDSC
     C                   PARM      ' '           @@RTCD            1
     C                   PARM                    ##ST
     C                   PARM                    ##RRN4                         *** FAEOP ***
     C*
     C                   if        ##cmsv = *blanks
     C                   seton                                        9035
     C                   else
     C                   movel     ##cmsv        schcpc
     C                   movel     ##cdsc        sccdds
     C                   seton                                        9036
     C                   endif                                                  ##CMSV =  *BLANKS
     C*
     C     EODSTY        WHENEQ    'DR'
     C     EUICN         ANDEQ     'AO'
     C     EODSTY        OREQ      'DR'
     C     EUICN         ANDEQ     'AS'
     C     EODSTY        OREQ      'DR'
     C     EUICN         ANDEQ     'HO'
     C     EODSTY        OREQ      'DR'
     C     EUICN         ANDEQ     'HS'
     C     EODSTY        OREQ      'DS'
     C     EUICN         ANDEQ     'AO'
     C     EODSTY        OREQ      'DS'
     C     EUICN         ANDEQ     'AS'
     C     EODSTY        OREQ      'DS'
     C     EUICN         ANDEQ     'HO'
     C     EODSTY        OREQ      'DS'
     C     EUICN         ANDEQ     'HS'
     C     XXMFS         OREQ      'Y'
     C*          XXMPPO    OREQ 'Y'
     C*
     C     XXMPPO        IFEQ      'Y'
     C     XXBLTY        ANDEQ     '83'
     C     EOAST         ANDEQ     'NJ'
     C     EOSPCD        OREQ      'ASC'
     C** Help from Medicare ASC file
     C                   CALL      'F2358R'
     C                   PARM      '2'           ##CALL
     C                   PARM      '0'           ##KC
     C                   PARM      '0'           ##KL
     C                   PARM      *ZEROS        ##RRN
     C*
     C     ##RRN         IFEQ      *ZEROS
     C     *INKL         ANDEQ     *OFF
     C                   MOVEL     *ON           *INKL
     C                   ENDIF
     C*
     C     ##RRN         IFNE      *ZERO
     C     ##RRN         CHAIN     FMASP                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     ASHCPC        SCHCPC
     C                   MOVEL     ASDESC        SCCDDS
     C                   SETON                                        90
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     C*
     C                   CALL      'F2352R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      *ZEROS        ##RRN
      *
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PAP0P                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     APHCPC        SCHCPC
     C                   MOVEL     APDESC        SCCDDS
     C                   SETON                                        90
     C                   ENDIF
     C*
     C                   ENDIF                                                  ##RRN <> 0
     C                   ENDIF
     C*
     C     EODSTY        WHENEQ    'DR'
     C     EUICN         ANDEQ     'IM'
     C     EODSTY        OREQ      'DR'
     C     EUICN         ANDEQ     'IP'
     C     EODSTY        OREQ      'DR'
     C     EUICN         ANDEQ     'IR'
     C     EODSTY        OREQ      'DR'
     C     EUICN         ANDEQ     'IS'
     C     EODSTY        OREQ      'DS'
     C     EUICN         ANDEQ     'IM'
     C     EODSTY        OREQ      'DS'
     C     EUICN         ANDEQ     'IP'
     C     EODSTY        OREQ      'DS'
     C     EUICN         ANDEQ     'IR'
     C     EODSTY        OREQ      'DS'
     C     EUICN         ANDEQ     'IS'
     C*
     C                   CALL      'F2310R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      *ZEROS        ##RRN
      *
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PRC0P                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     RCCPT4        SCHCPC
     C                   MOVEL     RCDSC1        SCCDDS
     C                   SETON                                        90
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   OTHER                                                  EODSTY = DR/DS
     C                   CALL      'F2016R'
     C                   PARM      '1'           ##MODE
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##CMSV
     C                   PARM                    ##CDSC
     C                   PARM      ' '           @@RTCD            1
     C                   PARM                    ##ST
     C                   PARM                    ##RRN4                         *** FAEOP ***
     C*
     C     ##CMSV        IFNE      *BLANKS
     C*
     C* If selection was from the procedure code file don't pop rev cod
     C*
     C     @@RTCD        IFNE      'C'
     C     @@RTCD        ANDNE     'H'
     C     @@RTCD        ANDNE     '1'
     C                   MOVEL     ##CMSV        SCRVCD
     C     5             SUBST     ##CMSV:4      SCHCPC
     C                   MOVEL(P)  ##CDSC        SCCDDS
     C                   SETON                                        9036
     C                   ENDIF
     C*
     C     @@RTCD        IFEQ      'C'
     C     5             SUBST     ##CMSV:4      SCHCPC
     C                   MOVEL(P)  ##CDSC        SCCDDS
     C                   SETON                                        9036
     C                   ELSE
     C     @@RTCD        IFEQ      'H'
     C                   MOVEL     ##CMSV        SCHCPC
     C                   MOVEL(P)  ##CDSC        SCCDDS
     C                   SETON                                        9036
     C                   ENDIF                                                  @@RTCD = 'H'
     C                   ENDIF                                                  @@RTCD = 'C'
     C                   ELSE
     C                   SETON                                        9035
     C                   ENDIF                                                  ##CMSV NE *BLANKS
     C                   ENDSL                                                  EODSTY = DR/DS
      /Free

         Endif;

      /End-Free
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
     C*
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
     C** Disable F9 code info for rev code 001 line and SNF bills
     C                   if        (xxblty = '18'or xxblty = '21') and
     C                             xxmfs = 'Y' and scrvcd = 1
     C                   eval      *in96 = *on
     C                   else
     C                   eval      *in96 = *off
     C                   endif
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
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C     PRCPT4        ANDEQ     SCHCPC
     C     PRSTAT        ANDNE     'I'
     C*
     C                   MOVEL(P)  PRDSC1        DUDESC
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
     C*
     C*
     C                   MOVEL     SCHCPC        CACPT4
     C                   MOVEL     DURVCD        K1RVCD
     C                   MOVE      DURATE        K1HCPC
     C                   MOVE      *BLANKS       K1UNTS
     C                   MOVEL(P)  K1CMSV        CACODE
     C                   Z-ADD     SCRVCD        CASVCD
     C*
     C                   Z-ADD     SCUNTS        CAUNTS
     C*
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
     C                   Select
     C                   when      xxrcfl = 'Y'
     C                   eval      carcfl = 'Y'
     C                   when      xxrcfl = 'M'
     C                   eval      carcfl = 'M'
     C                   other
     C                   eval      carcfl = 'S'
     C                   endsl
     C                   MOVEL     XXSRC         CASRC
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
     C*** INITIALIZE POSITION CURSOR INDICATORS
     C*
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
     C     ##CLT#        IFEQ      '114'
     C                   MOVEL(P)  SCCSCD        KKKEY
     C     FNKY          CHAIN     PFN01                              99
     C     *IN99         IFEQ      *OFF
     C     FNNWCG        IFEQ      'Y'
     C     TBKY          CHAIN     PCS01                              99
     C     *IN99         IFEQ      *OFF
     C     CSOVRD        IFEQ      'Y'
     C     CSOVRP        ANDEQ     0
     C     EORCN         ANDNE     CARCN
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
     C                   MOVEL(P)  EOAST         XXST             10
     C     XXST          CHAIN     STSTL1                             99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     STMFS         XXMFS
     C                   MOVEL     STMPPO        XXMPPO
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
     C                   MOVEL     EOAST         ###ST
     C                   MOVEL     'M'           ##TYP
     C     RTKY          SETGT     PRT01
     C     RTKY1         READPE    PRT01                                  99
     C     *IN99         DOWEQ     *OFF
     C     RTSTAT        IFEQ      'I'
     C     RTKY1         READPE    PRT01                                  99
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     RTMREL        XXMREL
     C     RTMREL        IFEQ      '1'
     C                   MOVE      RTEFDT        XXEFDT
     C                   ENDIF
     C                   ENDIF
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
     C* GET LAST LINE NUMBER USED FOR THE BILL
     C*
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
     C* DEFAULT 'X' TO FIRST DIAGNOSIS CODE IF IT IS THE ONLY
     C* DIAGNOSIS CODE
     C*
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
     C                   MOVE      *BLANKS       TMSVCD
      /Free
         If Eoast = 'HL';
           *in43 = *off;
         Endif;
      /End-Free
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
     C     CKAPC         BEGSR
     C*================================================================
      *
     C                   Z-ADD     K1EFDT        KKEFDT
     C                   MOVEL     SCHCPC        KAHCPC
      *
     C     APKY1         SETGT     PAP01                              99
     C     APKY2         READPE    PAP01                                  99
      *
     C     *IN99         DOWEQ     *OFF
     C     APSTAT        ANDNE     'A'
     C     APKY2         READPE    PAP01                                  99
     C                   ENDDO
      *
     C     *IN99         IFEQ      *ON
      *
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN94
      *
     C                   ELSE
      *
     C                   MOVEL     APDESC        SCCDDS
      *
     C                   ENDIF                                                  *IN99 = *OFF
      *
     C     CKAPCE        ENDSR
      *
     C*================================================================
     C     CKASC         BEGSR
     C*================================================================
      *
     C                   Z-ADD     K1EFDT        KKEFDT
     C                   MOVEL     SCHCPC        KKHCPC
      *
     C     ASKY          SETGT     PAS02                              99
     C     ASKY1         READPE    PAS02                                  99
      *
     C     *IN99         DOWEQ     *OFF
     C     ASSTAT        ANDNE     'A'
     C     ASKY1         READPE    PAS02                                  99
     C                   ENDDO
      *
     C     *IN99         IFEQ      *ON
      *
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN94
      *
     C                   ELSE
      *
     C                   MOVEL     ASDESC        SCCDDS
      *
     C                   ENDIF                                                  *IN99 = *OFF
      *
     C     CKASCE        ENDSR
      *
     C*================================================================
     C     CKRC          BEGSR
     C*================================================================
      *
     C                   MOVE      SCHCPC        KKRATE
     C                   MOVE      SCZIPA        KKZIPA
     C                   Z-ADD     K1EFDT        KKFDAT
      *
     C     RCKY          SETGT     PRC01                              99
     C     RCKY1         READPE    PRC01                                  99
      *
     C     *IN99         DOWEQ     *OFF
     C     RCSTAT        ANDNE     'A'
     C     RCKY1         READPE    PRC01                                  99
     C                   ENDDO
      *
     C     *IN99         IFEQ      *ON
      *
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN94
      *
     C                   ELSE
      *
     C                   MOVEL     RCDSC1        SCCDDS
      *
     C                   ENDIF                                                  *IN99 = *OFF
      *
     C     CKRCE         ENDSR
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
     C*================================================================
     C     PRCWDW        BEGSR
     C*================================================================
     C*
     C*
     C     EUICN         IFEQ      'AO'
     C     EUICN         OREQ      'AS'
     C     EUICN         OREQ      'HO'
     C     EUICN         OREQ      'HS'
     C                   SETON                                        51
     C                   ELSE
     C                   SETOFF                                       51
     C                   ENDIF
     C                   eval      scirf  = *blanks
     C                   eval      *in71 = *off
     C                   If        euicn = 'IR' and xxmfs = 'Y'
     C                             and xxblty = '11'
     C                   seton                                        71
     C                   setoff                                       9651
     C                   eval      scirf  = '1'
     C                   else
     C                   setoff                                       71
     C*
     C     XXBLTY        IFEQ      '83'
     C     EOAST         ANDEQ     'NJ'
     C     XXMPPO        ANDEQ     'Y'
     C     EOSPCD        OREQ      'ASC'
     C                   SETON                                        96
     C                   MOVEL     '1'           SCASCF
     C                   ELSE
     C                   SETOFF                                       96
     C*
     C     *IN51         IFEQ      *ON
     C                   MOVEL     '1'           SCAPCF
     C                   ELSE
     C                   MOVEL     '1'           SCPFPF
     C                   ENDIF
     C                   ENDIF
     C                   endif                                                  euicn = 'IR'
     C                   MOVEL     ' '           SCASPF
     C                   MOVEL     ' '           SCOPPF
     C                   MOVEL     ' '           SCIPYF
     C                   MOVEL     ' '           SCIPRV
     C*
     C     *INKL         DOWEQ     *OFF
     C*
     C     EUICN         IFEQ      'AO'
     C     EUICN         OREQ      'AS'
     C     EUICN         OREQ      'HO'
     C     EUICN         OREQ      'HS'
     C                   SETON                                        51
     C                   ELSE
     C                   SETOFF                                       51
     C                   ENDIF
     C                   If        euicn = 'IR' and xxmfs = 'Y'
     C                             and xxblty = '11'
     C                   seton                                        71
     C                   setoff                                       9651
     C                   else
     C                   setoff                                       71
     C*
     C     XXBLTY        IFEQ      '83'
     C     EOAST         ANDEQ     'NJ'
     C     XXMPPO        ANDEQ     'Y'
     C     EOSPCD        OREQ      'ASC'
     C                   SETON                                        96
     C                   ELSE
     C                   SETOFF                                       96
     C                   ENDIF
     C                   endif                                                  euicn = 'IR'
     C*
     C                   EXFMT     WDW02
     C*
     C                   SETOFF                                       42
     C*
     C   51
     CANN96SCAPCF        IFEQ      '1'
     C                   SETON                                          42
     C                   ENDIF
     C*
     C  N51
     CANN96SCPFPF        IFEQ      '1'
     C                   SETON                                          42
     C                   ENDIF
     C*
     C   96SCASCF        IFEQ      '1'
     C                   SETON                                          42
     C                   ENDIF
     C                   SELECT
     C     SCOPPF        WHENEQ    '1'
     C     *IN51         ANDEQ     *ON
     C     *IN96         ANDEQ     *OFF
     C     *IN42         ANDEQ     *ON
     C                   SETON                                        52
     C                   SETOFF                                       5354
     C     SCIPYF        WHENEQ    '1'
     C     *IN51         ANDEQ     *OFF
     C     *IN96         ANDEQ     *OFF
     C     *IN42         ANDEQ     *ON
     C                   SETON                                        53
     C                   SETOFF                                       5254
     C     SCAPCF        WHENNE    '1'
     C     *IN51         ANDEQ     *ON
     C     *IN96         ANDEQ     *OFF
     C     SCAPCF        ANDNE     *BLANKS
     C                   SETON                                        54
     C                   SETOFF                                       5253
     C     SCOPPF        WHENNE    '1'
     C     *IN51         ANDEQ     *ON
     C     *IN96         ANDEQ     *OFF
     C     SCOPPF        ANDNE     *BLANKS
     C                   SETON                                        5253
     C                   SETOFF                                         54
     C     SCPFPF        WHENNE    '1'
     C     *IN51         ANDEQ     *OFF
     C     *IN96         ANDEQ     *OFF
     C     SCPFPF        ANDNE     *BLANKS
     C                   SETON                                        5354
     C                   SETOFF                                         52
     C     SCIPYF        WHENNE    '1'
     C     *IN51         ANDEQ     *OFF
     C     *IN96         ANDEQ     *OFF
     C     SCIPYF        ANDNE     *BLANKS
     C                   SETON                                        5254
     C                   SETOFF                                         53
     C*
     C     SCAPCF        WHENEQ    '1'
     C     *IN51         ANDEQ     *ON
     C     *IN96         ANDEQ     *OFF
     C     *INKL         ANDEQ     *OFF
     C                   Z-ADD     CAFDAT        KKEFDT
     C                   MOVEL     DURATE        KAHCPC
     C*
     C     APKY1         SETGT     PAP01
     C     APKY2         READPE    PAP01                                  99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRN
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     APRRN         ##RRN
     C                   ENDIF                                                  *IN99 = On
     C                   CALL      'F2553R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRN
     C                   MOVEL     ' '           SCAPCF
     C*
     C     SCOPPF        WHENEQ    '1'
     C     *IN51         ANDEQ     *ON
     C     *IN96         ANDEQ     *OFF
     C     *INKL         ANDEQ     *OFF
     C                   Z-ADD     CAFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYM1
     C                   MOVEL     SRDTO         KPEFDT
     C     EUKY          CHAIN(N)  PEU01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     EUMCAR        KKPROV
     C                   ENDIF
     C     OPKY1         SETGT     POP01
     C     OPKY2         READPE    POP01                                  99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRN
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     OPRRN         ##RRN
     C                   ENDIF                                                  *IN99 = On
     C                   CALL      'F2357R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRN
     C                   MOVEL     ' '           SCOPPF
     C*
     C     SCPFPF        WHENEQ    '1'
     C     *IN51         ANDEQ     *OFF
     C     *IN96         ANDEQ     *OFF
     C     *INKL         ANDEQ     *OFF
     C                   Z-ADD     CAFDAT        K1EFDT
     C     EUKY          CHAIN(N)  PEU01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     EUMCAR        K1PROV
     C                   ENDIF
     C*
     C                   Z-ADD     EUADDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM1
     C                   MOVEL     SRDTO         KPEFDT
     C*
     C     IIKY1         SETGT     PTP01
     C     IIKY2         READPE    PTP01                                  99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRN
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     IIRRN         ##RRN
     C                   ENDIF                                                  *IN99 = On
     C                   CALL      'F2356R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRN
     C                   MOVEL     ' '           SCPFPF
     C*
     C     SCIPYF        WHENEQ    '1'
     C     *IN51         ANDEQ     *OFF
     C     *IN96         ANDEQ     *OFF
     C     *INKL         ANDEQ     *OFF
     C     EUKY          CHAIN(N)  PEU01                              60
     C     *IN60         IFEQ      *OFF
     C                   CALL      'F0364R'
     C                   PARM                    ##CO#
     C                   PARM                    KKSUB#
     C                   PARM                    KKFND
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
     C                   PARM                    KKEOB#
     C                   PARM                    SCCONM
     C                   ENDIF                                                  *IN60 = *OFF
     C                   MOVEL     ' '           SCIPYF
     C*
     C     SCASPF        WHENEQ    '1'
     C     *IN96         ANDEQ     *ON
     C     *IN42         ANDEQ     *ON
     C                   SETON                                        52
     C                   SETOFF                                       5354
     C*
     C     SCASCF        WHENNE    '1'
     C     *IN96         ANDEQ     *ON
     C     SCASCF        ANDNE     *BLANKS
     C                   SETON                                        54
     C                   SETOFF                                       5253
     C*
     C     SCASPF        WHENNE    '1'
     C     *IN96         ANDEQ     *ON
     C     SCASPF        ANDNE     *BLANKS
     C                   SETON                                        5253
     C                   SETOFF                                         54
     C*
     C     SCASCF        WHENEQ    '1'
     C     *IN96         ANDEQ     *ON
     C     *INKL         ANDEQ     *OFF
     C                   Z-ADD     CAFDAT        KKEFDT
     C                   MOVEL     DURATE        KKHCPC
     C*
     C     APKY1         SETGT     PAP01
     C     APKY2         READPE    PAP01                                  99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRN
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     APRRN         ##RRN
     C                   ENDIF                                                  *IN99 = On
     C*
     C                   CALL      'F2553R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRN
     C*
     C                   MOVEL     ' '           SCASCF
     C*
     C     SCASPF        WHENEQ    '1'
     C     *IN96         ANDEQ     *ON
     C     *INKL         ANDEQ     *OFF
     C                   CALL      'F0365R'
     C                   PARM                    ##CO#
     C                   PARM                    KKSUB#
     C                   PARM                    KKFND
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
     C                   PARM                    KKEOB#
     C                   PARM                    SCCONM
     C                   PARM      CADSEQ        #1DSEQ            5 0
     C*
     C                   MOVEL     ' '           SCASPF
     C*
     C     SCIPRV        WHENEQ    '1'
     C     *IN71         ANDEQ     *ON
     C     *INKL         ANDEQ     *OFF
     C*
     C                   Z-ADD     CAFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYM1
     C                   MOVEL     SRDTO         KPEFDT
     C*
     C     EUKY          CHAIN(N)  PEU01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     EUMCAR        K1PROV
     C                   ENDIF
     C*
     C*
     C     IRKY1         SETGT     PIR01
     C     IRKY2         READPE    PIR01                                  99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRN
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     IRRRN         ##RRN
     C                   ENDIF                                                  *IN99 = On
     C                   CALL      'F2383R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRN
     C                   MOVEL     ' '           SCIPRV
     C*
     C     SCIRF         WHENEQ    '1'
     C     *IN71         ANDEQ     *ON
     C     *INKL         ANDEQ     *OFF
     C                   CALL      'F0369R'
     C                   PARM                    ##CO#
     C                   PARM                    KKSUB#
     C                   PARM                    KKFND
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
     C                   PARM                    KKEOB#
     C                   PARM                    SCCONM
     C                   PARM      CADSEQ        #1DSEQ            5 0
     C*
     C                   MOVEL     ' '           SCIRF
     C*
     C                   ENDSL
     C*
     C                   ENDDO
     C                   eval      *in71 = *off
     C*
     C                   ENDSR
     C*================================================================
     C     CKSRC         BEGSR
     C*================================================================
      *
     C     XXMFS         IFEQ      'Y'
     C     XXMREL        IFEQ      '1'
     C                   MOVE      XXEFDT        KKEFDT
     C                   ELSE
     C                   Z-ADD     K1EFDT        KKEFDT
     C                   ENDIF                                                          XXMREL = '1'
     C*
     C                   MOVEL(P)  SCHCPC        KKHCPC
     C*
     C                   SETON                                        99
     C*
     C     EOSPCD        IFEQ      'ASC'
     C*
     C*** CHAIN IN NEXT FILE - FMASP
     C*
     C     ASKY          SETGT     PAS02
     C     ASKY1         READPE    PAS02                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C     ASSTAT        IFEQ      'I'
     C     ASKY1         READPE    PAS02                                  99
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     'S'           XXSRC
     C                   MOVEL(P)  ASDESC        SCCDDS
     C                   MOVEL     'M'           XXRCFL
     C                   ENDIF
     C*
     C                   ENDIF                                                  EOSPCD = 'ASC'
     C*
     C     *IN99         IFEQ      *ON
     C     EOSPCD        ANDNE     'ASC'
     C*
     C     ASKY          SETGT     PAM01
     C     ASKY1         READPE    PAM01                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C     AMSTAT        IFEQ      'I'
     C     ASKY1         READPE    PAM01                                  99
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     'A'           XXSRC
     C                   MOVEL(P)  AMDESC        SCCDDS
     C                   MOVEL     'M'           XXRCFL
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN99 = *ON C'
     C*
     C     *IN99         IFEQ      *ON
     C     EOSPCD        ANDNE     'ASC'
     C*
     C*** CHAIN IN NEXT FILE - FMDFP
     C*
     C     ASKY          SETGT     PDF01
     C     ASKY1         READPE    PDF01                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C     DFSTAT        IFEQ      'I'
     C     ASKY1         READPE    PDF01                                  99
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     'F'           XXSRC
     C                   MOVEL(P)  DFDESC        SCCDDS
     C                   MOVEL     'M'           XXRCFL
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN99 = *ON C'
     C*
     C     *IN99         IFEQ      *ON
     C     EOSPCD        ANDNE     'ASC'
     C*
     C*** CHAIN IN NEXT FILE - FMDEP
     C*
     C     ASKY          SETGT     PDM02
     C     ASKY1         READPE    PDM02                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C     DESTAT        IFEQ      'I'
     C     ASKY1         READPE    PDM02                                  99
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     'D'           XXSRC
     C                   MOVEL(P)  DEDESC        SCCDDS
     C                   MOVEL     'M'           XXRCFL
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN99 = *ON C'
     C*
     C     *IN99         IFEQ      *ON
     C     EOSPCD        ANDNE     'ASC'
     C*
     C*** CHAIN IN NEXT FILE - FTLBP
     C*
     C     ASKY          SETGT     PLB02
     C     ASKY1         READPE    PLB02                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C     LBSTAT        IFEQ      'I'
     C     ASKY1         READPE    PLB02                                  99
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     'L'           XXSRC
     C                   MOVEL(P)  LBDESC        SCCDDS
     C                   MOVEL     'M'           XXRCFL
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN99 = *ON C'
     C*
     C     *IN99         IFEQ      *ON
     C     EOSPCD        ANDNE     'ASC'
     C*** CHAIN IN NEXT FILE - FTRVP
     C*** CHAIN IN NEXT FILE - FTRVP
     C*** CHAIN IN NEXT FILE - FTRVP
     C     ASKY          SETGT     PRV03
     C     ASKY1         READPE    PRV03                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C     RVSTAT        IFEQ      'I'
     C     ASKY1         READPE    PRV03                                  99
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     'R'           XXSRC
     C                   MOVEL(P)  RVDESC        SCCDDS
     C                   MOVEL     'M'           XXRCFL
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN99 = *ON C'
     C*
     C*
     C                   ENDIF                                                  XXMFS = Y
      *
     C     CKSRCE        ENDSR
      *
     C*================================================================
     C     CKSNF         BEGSR
     C*================================================================
      *
     C                   eval      *in51 = *off
     C                   eval      cnt = 0
      *
     C                   if        @@mode = 'A'
     C     caky5         setll     pca05
     C     caky5         reade     pca05
     C                   dow       not %eof
     C                   eval      kkhcpc = cacpt4
     C     asky1         chain     php02
     C                   if        %found and casvcd = 22
     C                   eval      cnt =  cnt + 1
     C                   endif
     C
     C     caky5         reade     pca05
     C                   enddo
     C                   endif
     C*
     C                   if        cnt < 4
     C*
     C                   Z-ADD     K1EFDT        KKEFDT
     C                   MOVEL     SCHCPC        KKHCPC
      *
     C     ASKY          SETGT     PHP02                              99
     C     ASKY1         READPE    PHP02                                  99
      *
     C     *IN99         DOWEQ     *OFF
     C     HPSTAT        ANDNE     'A'
     C     ASKY1         READPE    PHP02                                  99
     C                   ENDDO
      *
     C     *IN99         IFEQ      *ON
      *
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN94
      *
     C                   ELSE
      *
     C                   MOVEL     HPDSC1        SCCDDS
     C                   MOVEL     'H'           XXSRC
     C                   MOVEL     'M'           XXRCFL
      *
     C                   ENDIF                                                  *IN99 = *OFF
     C                   else
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN51

     C                   endif
      *
     C     CKSNFE        ENDSR
      *
     C*================================================================
     C     CKCMG         BEGSR
     C*================================================================
     C*
     C                   Z-ADD     K1EFDT        KKEFDT
     C                   MOVEL     SCHCPC        KKHCPC
     C*
     C* Get Tier (first char of CMG code from file FDDUP)
     C*
     C                   eval      xxchar = *blanks
     C                   eval      xxchar = %subst(schcpc:1:1)
     C                   select
     C                   when      xxchar = 'A'
     C                   eval      kktier = '0'
     C*
     C                   when      xxchar = 'B'
     C                   eval      kktier = '1'
     C*
     C                   when      xxchar = 'C'
     C                   eval      kktier = '2'
     C*
     C                   when      xxchar = 'D'
     C                   eval      kktier = '3'
     C*
     C                   endsl
     C*
     C*
     C     CMGKY         SETGT     PCMG02                             99
     C     CMGKY1        READPE    PCMG02                                 99
     C*
     C     *IN99         DOWEQ     *OFF
     C     CMGSTAT       ANDNE     'A'
     C     CMGKY1        READPE    PCMG02                                 99
     C                   ENDDO
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN94
     C*
     C                   ELSE
     C*
     C                   MOVEL     CMGDESC       SCCDDS
     C                   MOVEL     'C'           XXSRC
     C                   MOVEL     'M'           XXRCFL
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C     CKCMGE        ENDSR
      *
     OPMG01     E            RPMG01
     OPDE01     E            RPDE01
