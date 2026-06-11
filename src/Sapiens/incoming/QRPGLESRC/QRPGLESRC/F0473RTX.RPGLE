     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*****************************************************************
     H/TITLE F0473RTX - Pharmacy Detail Information EDI
     H*****************************************************************
     H*
     H* F0473RTX - Pharmacy Detail Information EDI
     H*
     H* 04/30/08  C:191 Project: TX045
     H*                 COPIED FROM F0473R
     H*****************************************************************
     H** REVISIONS:
     H*
     H* 08/17/11  C:251    Project: TX075
     H*                    Allow entry for description
     H*
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      29      Add mode                                       **
     H*****************************************************************
     H*
     FF0473FTX  CF   E             WORKSTN
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
     FFDDAL2    UF   E           K DISK
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
     F                                     INFDS(DBFDC)
     F* Generic Table File
     FFTGTL     IF   E           K DISK
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
     FFDMGL2    UF   E           K DISK
     D*
     D DBFDB           DS
     D  DBRRNB               397    400B 0
     D DBFPR           DS
     D  PRRRN                397    400B 0
     D DBFDC           DS
     D  DBRRNC               397    400B 0
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
     D*
     D NT0             C                   CONST('Field was changed fo-
     D                                     r EDI correction:')
     D NT1             C                   CONST('Original Value:')
     D FL1const        C                   CONST('Billed NDC code')
     D                 DS
     D  XXNOT1                 1     70
     D  XXNT1                  1     38
     D  XXNT2                 39     70
     D                 DS
     D  XXNOT2                 1     70
     D  XXNT21                 1     16
     D  XXNT22                17     70
     C*
     C/COPY SCOPYMT01C
     C*
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
     C     APIPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM      EOSUB#        #2SUB#            5 0
     C                   PARM      EOFND         #2FND             3 0
     C                   PARM      EOFYR         #2FYR             3 0
     C                   PARM      EOCASE        #2CASE            7 0
     C                   PARM      EOEOB#        #2EOB#            9 0
     C                   PARM      EORCN         #2RCN             3 0
     C                   PARM      DPDSEQ        #2DSEQ            5 0
     C                   PARM      'D'           #2CALL            1
     C                   PARM                    #2OVAL           70
     C                   PARM                    #2NVAL           70
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
     C*
     C*  FS BASE CONTROL FILE
     C*
     C     FBKY          KLIST
     C                   KFLD                    XSKEY             2
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
     C     *LIKE         DEFINE    RUST          KKST
     C     *LIKE         DEFINE    RUATYP        KKATYP
     C     *LIKE         DEFINE    RUEFDT        KKEFDT
     C     *LIKE         DEFINE    EOCRDT        HLSYDT
     C     *LIKE         DEFINE    CAPAID        XXTOTL
     C     *LIKE         DEFINE    DEEOB         XXEOB
     C     *LIKE         DEFINE    DPBNDC        XOBNDC
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
     C* DELETE EOB CODES/NOTES FOR THE LINE ITEM
     C*
     C     DEKY          CHAIN     PDE01                              99
     C     *IN99         DOWEQ     *OFF
     C                   DELETE    PDE01
     C     DEKY          READE     PDE01                                  99
     C                   ENDDO                                                  *IN99 = *OFF
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
     C*
     C* Validate Date of Service
     C*
     C     SCRXFD        IFEQ      *ZEROS
     C                   SETON                                        9073
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCRXFD IFEQ ZEROS
     C*
     C                   Z-ADD     SCRXFD        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9074
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     SRDTO         SCRXFD
     C                   ENDIF
     C*
     C* DATE OF SERVICE CANNOT BE LESS THAN ACCIDENT DATE
     C*
     C                   Z-ADD     SCRXFD        SRDTI
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
     C* VALIDATE NDC DRUG CODE (REQUIRED)
     C*
     C     SCNDC         ifeq      *BLANKS
     C                   SETON                                        9083
     C                   GOTO      ck1END
     C                   endif                                              S
     C*
     C                   MOVEL(p)  SCNDC         KKNDC
     C     RXKY          CHAIN     PRX01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9084
     C                   GOTO      CK1END
     C*** Beg Add *** 08/17/11 ************************************************
     C                   ELSE
     C                   MOVEL(P)  RXNAME        SCDESC
     C*** End Add *** 08/17/11 ************************************************
     C                   ENDIF
     C*
     C     RXSTAT        IFEQ      'I'
     C                   SETON                                        9076
     C                   GOTO      CK1END
     C                   ENDIF
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
     C                   SETON                                        9089
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLFDAT GT HLSYDT
     C*
     C                   ENDIF                                                  XXFSDF = *BLANKS
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C* VERIFY THAT FEE SCHEDULE IS ON SYSTEM FOR SERVICE DATE
     C*
     C                   Z-ADD     HLFDAT        KKEFDT
     C                   MOVE      SCAST         KKST
     C                   MOVE      'X'           KKATYP
     C     RUKY1         SETGT     PRU01
     C     RUKY2         READPE    PRU01                                  99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9082
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C* CHARGES ARE REQUIRED
     C*
     C* IF ADJUDICATION CODE IS OVERRIDE TYPE, ALLOW ZERO CHARGE AMOUNT
     C*
     C     CSOVRD        IFNE      'Y'
     C     SCCHRG        IFEQ      *ZEROS
     C                   SETON                                        9077
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCCHRG IFEQ ZEROS
     C                   ENDIF                                                  CSOBRD NE Y
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
     C* Validate Billed NDC Code
     C     SCBNDC        IFNE      *BLANKS
     C                   MOVEL(P)  SCBNDC        KKNDC
     C     RXKY          CHAIN     PRX01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     RXNAME        SCDESC
     C                   ELSE
     C                   SETON                                        9086
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *OFF
     C                   ENDIF                                                  SCBNDC=' '
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
     C* CALL EOB CODES/NOTES PROGRAM NOW INSTEAD OF GIVING
     C*             AN ERROR
     C****               MOVE      *BLANKS       XXEOB
     C**** *IN99         IFEQ      *OFF
     C**** CSOVRD        ANDEQ     'Y'
     C**** CSOVRP        ANDEQ     *ZEROS
     C*
     C****               Z-ADD     @@RRN2        ##RRN2
     C****               Z-ADD     @@RRN3        ##RRN3
     C*
     C* IF IN ADD MODE, NEED TO SEND IN SCREEN DSEQ NUMBER TO BE
     C* WRITTEN IN EOB CODES/NOTES FILE.  THIS IS DONE BECAUSE THIS
     C* DETAIL CHARGE IS NOT WRITTEN YET. ALSE SEND ZERO FOR THE
     C* RELATIVE RECORD NUMBER OF FDCAP (##RRN3).
     C*
     C**** @@MODE        IFEQ      'A'
     C****               Z-ADD     SCSEQ#        ##DSEQ            5 0
     C****               Z-ADD     0             ##RRN3
     C****               Z-ADD     SCSEQ#        KKDSEQ
     C****               ELSE
     C****               Z-ADD     0             ##DSEQ
     C****               ENDIF                                                  @@MODE = 'A'
     C*
     C*** REMOVE ANY EOB CODES AND MESSAGES PRODUCED BY ADJUDICATION
     C*   OR NETWORK PROCESSING
     C*
     C**** DEKY          CHAIN     PDE01                              99
     C**** *IN99         DOWEQ     *OFF
     C**** DEESRC        IFEQ      'A'
     C**** DEEOB         ANDNE     *BLANKS
     C**** DEESRC        OREQ      'N'
     C**** DEEOB         ANDNE     *BLANKS
     C****               DELETE    PDE01
     C****               ELSE
     C****               EXCEPT    RPDE01
     C****               ENDIF                                                  DEESRC = 'A'
     C**** DEKY          READE     PDE01                                  99
     C****               ENDDO
     C*
     C**** MGKY          CHAIN     PMG01                              99
     C**** *IN99         DOWEQ     *OFF
     C**** MGSRC         IFEQ      'S'
     C**** MGSRC         OREQ      'N'
     C**** MGMSG#        IFNE      'NW00001'
     C****               DELETE    PMG01
     C****               ELSE
     C*
     C****               UPDATE    PMG01
     C*
     C****               ENDIF
     C****               ELSE
     C****               EXCEPT    RPMG01
     C****               ENDIF                                                  MGSRC = 'S'
     C**** MGKY          READE     PMG01                                  99
     C****               ENDDO
     C*
     C****               CALL      'F0356R01'
     C****               PARM      ' '           ##KC
     C****               PARM      ' '           ##KL
     C****               PARM      'A'           ##MODE
     C****               PARM      *ZEROS        ##RRN
     C****               PARM                    ##RRN2
     C****               PARM                    ##RRN3
     C****               PARM                    ##DSEQ
     C*
     C**** SCMRA         IFNE      *ZEROS
     C**** SCNTWK        ORNE      *ZEROS
     C**** SCOTHR        ORNE      *ZEROS
     C**** SCOTH2        ORNE      *ZEROS
     C*
     C****               MOVE      *BLANKS       XXEOB
     C**** DEKY          CHAIN(N)  PDE01                              99
     C**** *IN99         DOWEQ     *OFF
     C*
     C**** DEEOB         IFNE      *BLANKS
     C****               MOVE      DEEOB         XXEOB
     C****               LEAVE
     C****               ENDIF                                                  DEEOB NE *BLANKS
     C*
     C**** DEKY          READE(N)  PDE01                                  99
     C****               ENDDO                                                  *IN99 = *OFF
     C*
     C**** XXEOB         IFEQ      *BLANKS
     C****               SETON                                        90
     C****               ENDIF                                                  XXEOB = *BLANKS
     C****               ENDIF                                                  *IN99 = *OFF
     C*
     C*
     C****               ENDIF                                                  *IN99 = *OFF
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
     C                   Z-ADD     DPUNTS        SCUNTS
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
     C                   MOVEL(P)  SCNDC         KKNDC
     C     RXKY          CHAIN     PRX01                              99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             ##RRN
     C                   ELSE                                                   *IN99 = On
     C                   Z-ADD     PRRRN         ##RRN
     C                   ENDIF                                                  *IN99 = On
     C     CARCFL        IFEQ      'X'
     C                   CALL      'F2350R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRN
     C                   ENDIF                                                  RCFP = 'C'
     C     CARCFL        IFEQ      'Y'
     C                   CALL      'F2350R01'
     C                   PARM                    @@KC
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRN
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
     C     *IN44         ANDNE     *ON
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
     C     HFLD          WHENEQ    'SCRXFD'
     C                   SETON                                        9032
     C     HFLD          WHENEQ    'SCRX#'
     C                   SETON                                        9033
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
     C                   SETON                                        9054      pos cursor
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
     C                   SETON                                        9053
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##KC = '1'
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
     C*Billed NDC CODE SEARCH
     C*
     C     HFLD          WHENEQ    'SCBNDC'
     C                   MOVEL(P)  SCBNDC        HLNDC
     C                   MOVEL(P)  RXNAME        HLDESC           40
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
     C                   MOVEL(P)  HLNDC         SCBNDC
     C                   MOVEL(P)  HLDESC        SCDSND
     C                   SETON                                        9031
     C                   ELSE
     C     ##RRN         CHAIN     PRX02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  RXNDC         SCBNDC
     C                   MOVEL(P)  RXNAME        SCDSND
     C                   SETON                                        9041
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##KC = '1'
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
     C                   Z-ADD     0             SCEOB#
     C                   Z-ADD     0             SCLIN#
     C                   Z-ADD     0             SCSEQ#
     C                   MOVE      *BLANKS       SCRX#
     C                   MOVE      *BLANKS       SCCSCD
     C                   MOVE      *BLANKS       SCCSDS
     C                   Z-ADD     0             SCRXFD
     C                   MOVE      *BLANKS       SCNDC
     C                   MOVE      *BLANKS       SCDESC
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
     C                   CLEAR                   SCEOB1
     C                   CLEAR                   SCEOB2
     C                   CLEAR                   SCEOB3
     C                   CLEAR                   SCEOB4
     C                   CLEAR                   SCBNDC
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
     C   44              MOVEL     '300'         SCCSCD
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
     C                   Z-ADD     DPUNTS        SCUNTS
     C*
     C                   MOVEL(P)  DPBNDC        SCBNDC
     C                   MOVEL(P)  DPBNDC        XOBNDC
     C                   MOVEL     *BLANKS       SCDSND
     C                   MOVEL(P)  SCBNDC        KKNDC
     C     RXKY          CHAIN     PRX01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  RXNAME        SCDSND
     C                   ENDIF                                                  *IN99 = *OFF
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
     C                   MOVEL(P)  SCNDC         DPNDC
     C                   MOVEL(P)  SCBNDC        DPBNDC
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
     C* CALL FA758R WITH THE CALL PARAMETER SET TO 'H'
     C                   EXSR      SRWREP
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
     C                   MOVE      'X'           CARCFL
     C                   MOVEL(P)  EOBTYP        CAPRTY
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
     C                   FEOD      FDDPP
     C* CALL FA758R WITH THE CALL PARAMETER SET TO 'H'
     C                   EXSR      SRWREP
     C                   SETON                                        41
     C                   ENDSR
     C*================================================================
     C* SRWREP - Write header note record
     C*================================================================
     C     SRWREP        BEGSR
     C* IF FIELDS CHANGED THEN CALL API
     C*
     C     DPBNDC        IFNE      XOBNDC
     C* CHANGED FIELD DESCRIPTION
     C                   MOVEL(P)  NT0           XXNT1
     C                   MOVEL(P)  FL1const      XXNT2
     C                   MOVEL(P)  XXNOT1        #2OVAL
     C* ACTUAL ORIGINAL VALUE
     C                   MOVEL(P)  NT1           XXNT21
     C                   MOVEL(P)  XOBNDC        XXNT22
     C                   MOVEL(P)  XXNOT2        #2NVAL
     C* CALL FA758R WITH THE CALL PARAMETER SET TO 'D'
     C                   CALL      'FA758R'      APIPL
     C                   ENDIF                                                  DPBNDC
     C*
     C                   ENDSR
     OPMG01     E            RPMG01
     OPDE01     E            RPDE01
