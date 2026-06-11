     H Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.')
     H*****************************************************************
     H/TITLE N9600R - PROCESS DETAIL CHARGES TO CORRECT DISCOUNT PGM
     H*****************************************************************
     H* THIS PGM IS CALLED BY F0070R, IT WILL READ THE DETAIL CHARGES *
     H* FOR THIS CLAIM/PROVIDER/DCS# AND DECIDE WHICH DISCOUNT PROGRAM*
     H* EACH OF THE SINGLE LINE ITEM SHOULD BE PROCESSED THRU.        *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    BILL ENTRY                                                 *
     H*                                                               *
     H*  PROGRAMMER: C:20               PROJECT: 98/NW001             *
     H*  CREATED: 03/19/94                                            *
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  07/18/23 C:342 Project: NY060
     H*                 NY Compliance: CARC/RARC Update
     H*
     H*  04/30/19 C:062 Project: 152WAB
     H*                 Apply Optacomp Mods.
     H*
     H*  03/18/15 C:212 Project 114MRC
     H*                 Add processing for DX type Network disc. code
     H*
     H*  04/30/13 C:277 Project 147MAS
     H*                 Add call of prohram N96DVR for CSI only
     H*
     H*  08/12/10 C:198 Project 114MWS
     H*                 Incorporated Medicare PPO processing
     H*
     H*  06/28/07 C:190 PROJECT: 10MKI
     H*                 ID Bills which are eligible for discount
     H*                 11/30/07 C:47 - If bill gets a 398 eob note,
     H*                 remove any ajudication notes.
     H*
     H*  06/20/07 C:193 PROJECT: 114MOE
     H*                 CALL N96DNR TO CALCULATE NOVACARE NET
     H*                 WORK DISCOUNT
     H*
     H*  04/19/06 C:163 PROJECT: 114WMF
     H*                 BACK TO BASE - NETWORK DISCOUNT (114MDE)
     H*
     H*  01/30/05 C:79  Support: MC010 49338
     H*                 Skip line item if Pre-Authorized amount paid
     H*
     H*  01/30/04 C:79  Support: MC010 41022
     H*                 Skip line item if CCI denied it.
     H*
     H*  01/14/00 C:62  Project: NW012
     H*                 CHANGE TO LINE ITEM LEVEL FOR MFS.
     H*
     H*  11/10/99 C:31  Project: NW011
     H*                 Discount Calculation changes
     H*                 1. Change FDCAP logic to EOB# instead of DCS#
     H*
     H*
     H*  10/27/99 C:62  Project: MC044 LOG#93
     H*                 ADD LOGIC TO PROCESS ONLY THOSE RECORDS FOR THE
     H*                 CURRENT RECON.
     H*
     H*  03/30/99 C:31  Project: NW011
     H*                 Discount Calculation changes
     H*                 1. Program created from F9500R
     H*
     H*  03/11/99 C:31  Support: 16841
     H*                 Network adjudication issue
     H*                 1. Add test to insure network discount does
     H*                    not take place for records that already
     H*                    have a network discount applied.
     H*                 2. Add test to insure change hasn't occured.
     H*
     H*  06/16/98 C:31  Support: 12221
     H*                 Network adjudication issue
     H*                 1. Change program to support old and new fee
     H*                    schedule processing.
     H*
     H*  06/09/98  C:56   PROJECT: MC010 #11300
     H*                   Process FDCAP records using DCS# due to new
     H*                   fee schedule processing.
     H*
     H*  03/09/98 C:62  Project: MC159
     H*                 Provider/Network Rewrite
     H*                 1. If S4ENDT = *ZEROS, then make it 9999999.
     H*
     H*  02/18/98 C:62  Project: MC159
     H*                 ** CHANGED NMPHL & NMHSL WITH FMS4L2/L3.
     H*                                                               *
     H*  02/02/98 C:62  DEMO                                          *
     H*                 1. CHANGE QQF DATES TO 6A MOVE TO 6.0 FIELD   *
     H*                    AND CONVERT TO 7.0 CYMD.                   *
     H*                                                               *
     H*  01/05/98 C:59  REQ/PROJ: MC145                               *
     H*                 1. PROGRAM WAS NOT FINDING A MATCH IN THE     *
     H*                    DETAIL CHARGE LISTING FILE BECAUSE IT      *
     H*                    WAS CONVERTING THE CODE FROM AN 8 RATHER   *
     H*                    THAN A SIX.    CHANGED TO USE A 6.         *
     H*                                                               *
     H*  04/06/95    C:20    REQ/PROJ: NWMNT                          *
     H*                      1. MODIFY PROGRAM TO MAKE SURE FEIN/MOD  *
     H*                         IS THE SAME WHEN COMPARING CONTRACT   *
     H*                         INFORNATION.                          *
     H*                                                               *
     H*  03/07/95    C:20    REQ/PROJ: NW007                          *
     H*                      1. PGM HAS BEEN UPDATED TO ACCEPT NEW    *
     H*                         PARMS:                                *
     H*                         SYS  1                                *
     H*                         FL1  1                                *
     H*                         FL2  1                                *
     H*                         FL3  1                                *
     H*                         FL4  1                                *
     H*                         FL5  1                                *
     H*                         DS1 10                                *
     H*                         DS2 10                                *
     H*                         DS3 10                                *
     H*                         DS4 10                                *
     H*                         DS5 10                                *
     H*                      2. USE SYSTEM TYPE TO DETERMINE NMPHP OR *
     H*                         NMHSP FILE FOR DISCOUNT INFORMATION   *
     H*                                                               *
     H*  11/22/94 C:20  REQ/PROJ: NWMNT                               *
     H*                 1. ADDED CODE TO NOT PROCESS A CHARGE FOR     *
     H*                    DISCOUNTING IF THE CLAIM STATUS CODE IS AN *
     H*                    OVERRIDE TYPE.                             *
     H*                                                               *
     H*  09/13/94 C:20  REQ/PROJ: NWMNT                               *
     H*                 1. ADDED SERVICE DATE PARMS.  THIS WILL LET   *
     H*                    THIS PROGRAM TO ONLY PROCESS THE SANE      *
     H*                    DETAIL CHARGE RECORDS THAT THE ADJ PGM HAS *
     H*                    JUST PROCESSED.                            *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*****************************************************************
     FNMBDLD    IF   E           K DISK
     FFMS4L2    IF   E           K DISK
     FFMS4L3    IF   E           K DISK
     F                                     RENAME(PS401:PS402)
     FFAEOL4    IF   E           K DISK
     F                                     INFDS(EOFDS)
     F*
     FFDCAL7    IF   E           K DISK    USROPN
     FFDCALL    UF   E           K DISK    USROPN
     F                                     RENAME(PCA01:PCA02)
     FFTABL     IF   E           K DISK
      * Adjudication message file logicals
     FFDMGP     O    E           K DISK
      *
     FFDMGL6    IF   E           K DISK
     F                                     RENAME(PMG01:PMG02)
      *
     FFDMGL5    IF   E           K DISK
     F                                     RENAME(PMG01:PMG03)
     FFDDEP     O    E             DISK
     FFDDEL     IF   E           K DISK
     F                                     RENAME(PDE01:PDE02)
     F**** Beg Chg *** 07/18/23 **************************************
     F**** FDDEL1    IF   E           K DISK
     FFDDEL6    IF   E           K DISK
     F**** End Chg *** 07/18/23 **************************************
     F                                     RENAME(PDE01:PDE03)
     FFDDEL4    UF   E           K DISK
     F                                     RENAME(PDE01:PDE04)
     FFDFRL0    IF   E           K DISK
     FFAEUL     IF   E           K DISK

     D QQF             S              6    DIM(50)                              FROM DATES
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXPGM                  1      8
     D  XDTYPE                 4      5
     D XXNOT1          C                   CONST('NETWORK DISCOUNT AP-
     D                                     PLIED')
     D EOFDS           DS
     D  EORRN                397    400B 0
     D*** Beg Add *** 04/30/13 ******************************************
     D N96DVR          pr                  ExtPgm('N96DVR')
     D  inCO#                         3
     D  inSUB#                        5
     D  inFND                         3
     D  inFYR                         3
     D  inCASE                        7
     D  inFEIN                        9
     D  inMOD                         3
     D  inFDAT                        8
     D  inDSEQ                        5
     D  inCODE                        3
     D  inCEDT                        8
     D  inSYS                         1
     D  inFL1                         1
     D  inFL2                         1
     D  inFL3                         1
     D  inFL4                         1
     D  inFL5                         1
     D  inDS1                        10
     D  inDS2                        10
     D  inDS3                        10
     D  inDS4                        10
     D  inDS5                        10
     D  inRTCD                        1
     D  inEOB#                        9  0
     D  inRCN                         3  0
     D  outCal                        1
     D
     D  inCO#          s              3
     D  inSUB#         s              5
     D  inFND          s              3
     D  inFYR          s              3
     D  inCASE         s              7
     D  inFEIN         s              9
     D  inMOD          s              3
     D  inFDAT         s              8
     D  inDSEQ         s              5
     D  inCODE         s              3
     D  inCEDT         s              8
     D  inSYS          s              1
     D  inFL1          s              1
     D  inFL2          s              1
     D  inFL3          s              1
     D  inFL4          s              1
     D  inFL5          s              1
     D  inDS1          s             10
     D  inDS2          s             10
     D  inDS3          s             10
     D  inDS4          s             10
     D  inDS5          s             10
     D  inRTCD         s              1
     D  inEOB#         s              9  0
     D  inRCN          s              3  0
     D  outCal         s              1
     D*** End Add *** 04/30/13 ******************************************

     C*
     C*  CASE DETAIL RECORDS
     C*          PCA01 - OLD FEE SCHEDULE PROCESS
     C     CAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXYMD
     C*
     C*          PCA02 - NEW FEE SCHEDULE PROCESS
     C     CAKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C                   KFLD                    XXEOB#
     C*
     C*          PCA02 - NEW FEE SCHEDULE PROCESS
     C     CAKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C                   KFLD                    XXEOB#
     C                   KFLD                    CADSEQ
     C     EOKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C*
     C*
     C*   KEY LIST FOR THE HOSPITAL PROVIDERS
     C*
     C*
     C*   KEY LIST FOR THE DOCTOR PROVIDERS
     C*
      *
      * Key List for Bill Entry Error Message File
     C     MGKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    CARCN
     C                   KFLD                    CADSEQ
      *
     C     MGKY1         KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    CARCN
     C                   KFLD                    CADSEQ
     C                   KFLD                    KKMSEQ
      *
     C     MGKY2         KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    CADSEQ
     C                   KFLD                    KKMSG#
     C                   KFLD                    CARCN
     C*
     C*   KEY LIST FOR PROVIDER NETWORK CONTRACT FILE.
     C*
     C     S4KEY         KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C                   KFLD                    XXNWCD
     C                   KFLD                    CAFDAT
     C*
     C*  BILLING CODE TABLE FILE
     C*
     C     BDKEY         KLIST
     C                   KFLD                    XXCODE            3
     C                   KFLD                    CAFDAT
     C*
     C*
     C     DEKY1         KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    CADSEQ
     C                   KFLD                    KKSEQ#
     C*
     C     DEKY2         KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    CADSEQ
     C*
     C     DEKY3         KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    CADSEQ
     C                   KFLD                    KKEOB
     C*
     C     DEKY4         KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    CADSEQ
     C                   KFLD                    KKESRC
     C*
     C*  CLAIM STATUS CODE TABLE FILE
     C*
     C     TCSKY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    TABKEY           10
     C*
     C     FRKY          KLIST
     C                   KFLD                    EOAST
     C                   KFLD                    CARULE
     C*
     C     *LIKE         DEFINE    CAFDAT        XXYMD
     C     *LIKE         DEFINE    MGMSG#        KKMSG#
     C     *LIKE         DEFINE    MGMSEQ        KKMSEQ
     C     *LIKE         DEFINE    CANDIS        XXNDIS
     C     *LIKE         DEFINE    EOCO#         ##CO#
     C     *LIKE         DEFINE    EOSUB#        ##SUB#
     C     *LIKE         DEFINE    EOFND         ##FND
     C     *LIKE         DEFINE    EOFYR         ##FYR
     C     *LIKE         DEFINE    EOCASE        ##CASE
     C     *LIKE         DEFINE    EOEOB#        ##EOB#
     C     *LIKE         DEFINE    BDCODE        ##CODE
     C     *LIKE         DEFINE    S4NWCD        ##NWCD
     C     *LIKE         DEFINE    BDEFDT        ##EFDT
     C     *LIKE         DEFINE    DESEQ#        KKSEQ#
     C     *LIKE         DEFINE    DESEQ#        XXSEQ#
     C**** Beg Chg *** 07/18/23 **************************************
     C**** *LIKE         DEFINE    DEEOB         KKEOB
     C     *LIKE         DEFINE    DEEOBCD       KKEOB
     C**** End Chg *** 07/18/23 **************************************
     C     *LIKE         DEFINE    DEESRC        KKESRC
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFUND            3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM                    QQEOB#            9
     C                   PARM                    QQEDAT            8
     C                   PARM                    QQNWCD            5
     C                   PARM                    QQSYS             1
     C                   PARM                    QQFL1             1
     C                   PARM                    QQFL2             1
     C                   PARM                    QQFL3             1
     C                   PARM                    QQFL4             1
     C                   PARM                    QQFL5             1
     C                   PARM                    QQDS1            10
     C                   PARM                    QQDS2            10
     C                   PARM                    QQDS3            10
     C                   PARM                    QQDS4            10
     C                   PARM                    QQDS5            10
     C                   PARM                    QQFDT           150
     C                   PARM                    QQFDT2          150
     C                   PARM                    QQRTCD            1
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
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
     C*
     C*
     C     QQRTCD        COMP      'N'                                    86
     C   86              OPEN      FDCALL
     C  N86              OPEN      FDCAL7
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQSUB#        XXSUB#            5 0
     C                   MOVE      QQFUND        XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQCASE        XXCASE            7 0
     C                   MOVE      QQFEIN        XXFEIN            9 0
     C                   MOVE      QQMOD         XXMOD             3 0
     C                   MOVE      QQEOB#        XXEOB#            9 0
     C                   MOVE      QQNWCD        XXNWCD            5
     C                   MOVE      QQSYS         XXSYS             1
     C                   MOVEA     QQFDT         QQF(1)
     C                   MOVEA     QQFDT2        QQF(26)
     C     *IN86         IFEQ      *ON
     C                   CLEAR                   QQF
     C                   MOVE      010101        QQF(1)
     C                   ENDIF                                                  *IN86 = On
     C*
     C                   DO        50            S                 3 0
     C                   MOVE      QQF(S)        XXDATE            6 0
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXYMD
     C*
     C     QQF(S)        IFNE      *ZERO
     C     QQF(S)        ANDNE     *BLANKS
     C*
     C*  GET THE CASE DETAIL RECORDS
     C*
     C  N86CAKY1         CHAIN     PCA01                              61
     C   86CAKY2         CHAIN(N)  PCA02                              61
     C  N61EOKY          CHAIN     PEO01                              61
     C                   MOVE      *BLANK        XXINCH            1
     C                   MOVE      *BLANK        XXINCP            1
     C*
     C* CHECK TO SEE IF PRUDENT BUY
     C*
     C     *IN61         IFEQ      *OFF
     C     XXCLT#        ANDEQ     '114'
     C*
     C                   CLEAR                   QFL1
     C*
     C                   CALL      'FA734R'
     C                   PARM      EOCO#         ##CO#
     C                   PARM      EOSUB#        ##SUB#
     C                   PARM      EOFND         ##FND
     C                   PARM      EOFYR         ##FYR
     C                   PARM      EOCASE        ##CASE
     C                   PARM      EOEOB#        ##EOB#
     C                   PARM      *BLANKS       ##NWCD
     C                   PARM      *BLANKS       ##CODE
     C                   PARM      *ZEROS        ##EFDT
     C                   PARM      *BLANKS       ##RTCD            1
     C*
     C                   ENDIF                                                  XXCLT#=114
     C*
     C     *IN61         DOWEQ     '0'
     C     CARCN         IFNE      EORCN
     C*
     C                   GOTO      NXTDET
     C*
     C                   ENDIF
     C*
     C* SKIP LINE ITEM IF IT WAS DENIED BY CORRECT CODING
     C*
     C     CAOTH2        IFGT      0
     C     CAOT2C        ANDEQ     'CCI'
     C                   GOTO      NXTDET
     C                   ENDIF
     C     EODSTY        IFEQ      'DR'
     C     XXCLT#        ANDEQ     '114'
     C     CAOTH2        ANDGT     0
     C     CAPAID        ANDEQ     0
     C     CAOT2C        ANDEQ     'CCI'
     C                   GOTO      NXTDET
     C                   ENDIF
     C*
     C     EODSTY        IFEQ      'DR'
     C     XXCLT#        ANDEQ     '114'
     C     CAOTH2        ANDGT     0
     C     CAPAID        ANDEQ     0
     C     CAOT2C        ANDEQ     'CRW'
     C                   GOTO      NXTDET
     C                   ENDIF
     C*
     C     FRKY          CHAIN     PFR01                              99
     C*
     C     EODSTY        IFEQ      'DR'
     C     XXCLT#        ANDEQ     '114'
     C     CAMRA         ANDGT     0
     C     CAPAID        ANDEQ     0
     C     FRDIS         ANDEQ     'Y'
     C                   GOTO      NXTDET
     C                   ENDIF
     C*
     C* SKIP LINE ITEM IF PRE-AUTHORIZED AMOUNT WAS PAID
     C*
     C     CAIPOP        IFEQ      'P'
     C                   GOTO      NXTDET
     C                   ENDIF
     C*
     C* GET THE CLAIM STATUS RECORD. SEE IF OVERRIDE FLAG IS 'Y'.
     C* ONLY PROCESS DISCOUNTING IF NOT AN OVERRIDE TYPE CLAIM STATUS CODE
     C*
     C                   MOVE      *BLANK        TABKEY
     C                   MOVEL     CACSCD        TABKEY
     C     TCSKY         CHAIN     PCS01                              60
     C     *IN60         IFEQ      '0'
     C     CSOVRD        IFNE      'Y'
     C*
     C     CAEOB#        IFEQ      XXEOB#
     C     CAFEIN        ANDEQ     XXFEIN                                       ADD 09/13/94
     C     CAMOD         ANDEQ     XXMOD                                        ADD 09/13/94
     C*
     C*  IF A HOSPITAL PROVIDER GET THE DISCOUNT CODE FOR THIS EFF DT RANGE
     C*
     C     XXSYS         IFEQ      'H'
     C*
     C* PRUDENT BUY DISCOUNT
     C*
B01  C     ##RTCD        IFNE      *BLANKS
     C     XXCLT#        ANDEQ     '114'
     C*
     C                   MOVE      XXCO#         QCO#
     C                   MOVE      XXSUB#        QSUB#
     C                   MOVE      XXFND         QFND
     C                   MOVE      XXFYR         QFYR
     C                   MOVE      XXCASE        QCASE
     C                   MOVE      XXFEIN        QFEIN
     C                   MOVE      XXMOD         QMOD
     C                   MOVE      CADSEQ        QDSEQ
     C*
     C                   Z-ADD     EOFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QFDAT
     C*
     C                   MOVEL(P)  ##EFDT        QCEDT
     C*
B02  C     CANDIS        IFNE      0
     C     CACHRG        SUB       CAMRA         XXWRK9            9 2
     C                   SUB       CANDIS        XXWRK9
     C                   SUB       CAOTHR        XXWRK9
     C                   SUB       CAOTH2        XXWRK9
B03  C     XXWRK9        IFNE      CAPAID
     C                   Z-ADD     0             XXNDIS
E03  C                   ENDIF                                                  WRK9 <> PAID
E02  C                   ENDIF                                                  NDIS <> 0
     C*
B04  C     XXNDIS        IFEQ      0
     C*
     C* IS THIS A MADIGAN DISCOUNT
     C*
     C     ##RTCD        IFEQ      'M'
     C*
     C                   MOVE      XXCO#         QCO#
     C                   MOVE      XXSUB#        QSUB#
     C                   MOVE      XXFND         QFND
     C                   MOVE      XXFYR         QFYR
     C                   MOVE      XXCASE        QCASE
     C                   MOVE      XXFEIN        QFEIN
     C                   MOVE      XXMOD         QMOD
     C                   MOVE      XXCODE        QCODE
     C*
     C                   Z-ADD     S4BGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QCEDT
     C*
     C                   MOVE      CADSEQ        QDSEQ
     C                   MOVE      XXSYS         QSYS
     C                   MOVEL     XXNWCD        QDS1
     C*
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QFDAT
     C*
     C                   CALL      'N96DKR'
     C                   PARM                    QCO#              3
     C                   PARM                    QSUB#             5
     C                   PARM                    QFND              3
     C                   PARM                    QFYR              3
     C                   PARM                    QCASE             7
     C                   PARM                    QFEIN             9
     C                   PARM                    QMOD              3
     C                   PARM                    QFDAT             8
     C                   PARM                    QDSEQ             5
     C                   PARM      ##CODE        QCODE             3
     C                   PARM                    QCEDT             8
     C                   PARM      EOSYS         QSYS              1
     C                   PARM      *BLANKS       QFL1              1
     C                   PARM      *BLANKS       QFL2              1
     C                   PARM      *BLANKS       QFL3              1
     C                   PARM      *BLANKS       QFL4              1
     C                   PARM      *BLANKS       QFL5              1
     C                   PARM      ##NWCD        QDS1             10
     C                   PARM      *BLANKS       QDS2             10
     C                   PARM      *BLANKS       QDS3             10
     C                   PARM      *BLANKS       QDS4             10
     C                   PARM      *BLANKS       QDS5             10
     C                   PARM      *BLANKS       QRTCD             1
     C*
     C                   ELSE
     C                   CALL      'N96DHR'
     C                   PARM                    QCO#              3
     C                   PARM                    QSUB#             5
     C                   PARM                    QFND              3
     C                   PARM                    QFYR              3
     C                   PARM                    QCASE             7
     C                   PARM                    QFEIN             9
     C                   PARM                    QMOD              3
     C                   PARM                    QFDAT             8
     C                   PARM                    QDSEQ             5
     C                   PARM      ##CODE        QCODE             3
     C                   PARM                    QCEDT             8
     C                   PARM      EOSYS         QSYS              1
     C                   PARM      ##RTCD        QFL1              1
     C                   PARM      *BLANKS       QFL2              1
     C                   PARM      *BLANKS       QFL3              1
     C                   PARM      *BLANKS       QFL4              1
     C                   PARM      *BLANKS       QFL5              1
     C                   PARM      ##NWCD        QDS1             10
     C                   PARM      *BLANKS       QDS2             10
     C                   PARM      *BLANKS       QDS3             10
     C                   PARM      *BLANKS       QDS4             10
     C                   PARM      *BLANKS       QDS5             10
     C                   PARM      *BLANKS       QRTCD             1
     C                   ENDIF
E04  C                   ENDIF                                                  NDIS = 0
     C*
B05  C   86S4FSFL        IFEQ      'Y'
     C*
     C* Write error message only if it does not already exist
     C*
     C                   MOVEL(P)  'NW00001'     KKMSG#
     C*
     C     MGKY2         CHAIN     PMG03                              99
B06  C     *IN99         IFEQ      *ON
     C*
     C* Find the next available message sequence number
     C*
     C                   MOVE      *HIVAL        KKMSEQ
     C*
     C     MGKY1         SETGT     PMG02
     C     MGKY          READPE(N) PMG02                                  99
B07  C     *IN99         IFEQ      *OFF
     C                   ADD       1             MGMSEQ
S07  C                   ELSE
     C                   Z-ADD     1             MGMSEQ
E07  C                   ENDIF                                                  *IN99=OFF
     C*
     C                   Z-ADD     CACO#         MGCO#
     C                   Z-ADD     CASUB#        MGSUB#
     C                   Z-ADD     CAFND         MGFND
     C                   Z-ADD     CAFYR         MGFYR
     C                   Z-ADD     CACASE        MGCASE
     C                   Z-ADD     CADCS#        MGDCS#
     C                   Z-ADD     CAEOB#        MGEOB#
     C                   Z-ADD     CADSEQ        MGDSEQ
     C                   MOVEL(P)  'N'           MGSRC
     C                   MOVEL     *BLANKS       MGCSI
     C                   MOVEL(P)  'NW00001'     MGMSG#
     C                   Z-ADD     CARCN         MGRCN
     C                   MOVE      'A'           MGSTAT
     C                   MOVE      Q1USER        MGUSER
     C                   MOVE      Q1PGM         MGPGM
     C                   MOVE      Q1PGM         MGIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        MGTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MGDT
     C*
     C                   MOVE      Q1USER        MGIUSR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MGDT
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MGIDT
     C*
     C                   Z-ADD     MGTIME        MGITIM
     C*
     C                   WRITE     PMG01
E06  C                   ENDIF
     C*
     C* UPDATE FDCAP
     C*
     C     CAKY4         CHAIN     PCA02                              99
B08  C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  '300'         CACSCD
     C                   Z-ADD     *ZEROS        CAMRA
     C                   Z-ADD     *ZEROS        CAOTHR
     C                   Z-ADD     *ZEROS        CAOTH2
     C                   Z-ADD     *ZEROS        CAPAID
     C                   Z-ADD     *ZEROS        CANDIS
     C                   UPDATE    PCA02
E08  C                   ENDIF
E05  C                   ENDIF
     C*
S01  C                   ELSE
     C*
     C* IF PRUDENT BUY BILL DON'T PROCESS
     C*
     C                   MOVE      'N'           XXINCH                         IN CONTRACT HOS
     C*
     C     S4KEY         SETGT     PS401
     C     NXTHOS        TAG
     C                   READP     PS401                                  60
     C     *IN60         IFEQ      '0'
     C     S4NWCD        ANDEQ     XXNWCD
     C     S4FEIN        ANDEQ     XXFEIN
     C     S4MOD         ANDEQ     XXMOD
     C     S4ENDT        IFEQ      0
     C                   Z-ADD     99999999      S4ENDT
     C                   ENDIF                                                  ENDT = 0
     C* THE SERVICE DATE MUST BE BETWEEN THE PROVIDERS CONTRACT DATES
     C*
     C     CAFDAT        IFGT      S4ENDT
     C                   GOTO      NXTHOS
     C                   END
     C     S4STAT        IFEQ      'I'
     C*
     C                   GOTO      NXTHOS
     C*
     C                   ENDIF
     C*
     C* IF THE SERVICE DATE IS WITHIN THE CONTRACT DATES MOVE DISCOUNT CODE
     C*
     C                   MOVE      S4DISC        XXCODE
     C                   MOVE      'Y'           XXINCH                         IN CONTRACT HOS
     C*
     C* GET THE DISCOUNT TYPE FROM THS DISCOUNT CODE TABLE
     C*
     C     BDKEY         SETGT     PBD01
     C     NXTBD         TAG
     C                   READP     PBD01                                  60
     C*
     C*** VERIFY DISCOUNT CODE AND STATUS
     C*
     C     *IN60         IFEQ      '0'
     C     BDCODE        IFNE      XXCODE
     C     BDSTAT        OREQ      'I'
     C*
     C                   GOTO      NXTBD
     C*
     C                   ELSE                                                   ELSE BDCODE/BDSTAT
     C*
     C                   MOVE      BDTYPE        XXTYPE            2
     C*
     C* CALL THE DISCOUNT PROGRAM TO DISCOUNT THIS LINE ITEM
     C*
     C                   MOVEL     'N96XXR'      XXPGM
     C                   MOVE      XXTYPE        XDTYPE
     C                   MOVE      XXCO#         QCO#              3
     C                   MOVE      XXSUB#        QSUB#             5
     C                   MOVE      XXFND         QFND              3
     C                   MOVE      XXFYR         QFYR              3
     C                   MOVE      XXCASE        QCASE             7
     C                   MOVE      XXFEIN        QFEIN             9
     C                   MOVE      XXMOD         QMOD              3
     C                   MOVE      XXCODE        QCODE             3
     C*
     C                   Z-ADD     S4BGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QCEDT             8
     C*
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QFDAT             8
     C*
     C                   MOVE      CADSEQ        QDSEQ             5
     C                   MOVE      XXSYS         QSYS              1
     C                   MOVE      *BLANKS       QFL1              1
     C                   MOVE      *BLANKS       QFL2              1
     C                   MOVE      *BLANKS       QFL3              1
     C                   MOVE      *BLANKS       QFL4              1
     C                   MOVE      *BLANKS       QFL5              1
     C                   MOVEL     XXNWCD        QDS1             10
     C                   MOVE      *BLANKS       QDS2             10
     C                   MOVE      *BLANKS       QDS3             10
     C                   MOVE      *BLANKS       QDS4             10
     C                   MOVE      *BLANKS       QDS5             10
     C                   MOVE      ' '           QRTCD             1
     C*
     C     CANDIS        IFNE      0
     C     CACHRG        SUB       CAMRA         XXWRK9            9 2
     C                   SUB       CANDIS        XXWRK9
     C                   SUB       CAOTHR        XXWRK9
     C                   SUB       CAOTH2        XXWRK9
     C     XXWRK9        IFNE      CAPAID
     C                   Z-ADD     0             XXNDIS
     C                   ENDIF                                                  WRK9 <> PAID
     C                   ENDIF                                                  NDIS <> 0
     C     XXNDIS        IFEQ      0
     C***** Beg Add *** 03/18/15 *******************************************
     C     XXPGM         ANDNE     'N96DXR'
     C***** End Add *** 03/18/15 *******************************************
     C                   CALL      XXPGM
     C                   PARM                    QCO#
     C                   PARM                    QSUB#
     C                   PARM                    QFND
     C                   PARM                    QFYR
     C                   PARM                    QCASE
     C                   PARM                    QFEIN
     C                   PARM                    QMOD
     C                   PARM                    QFDAT
     C                   PARM                    QDSEQ
     C                   PARM                    QCODE
     C                   PARM                    QCEDT
     C                   PARM                    QSYS
     C                   PARM                    QFL1
     C                   PARM                    QFL2
     C                   PARM                    QFL3
     C                   PARM                    QFL4
     C                   PARM                    QFL5
     C                   PARM                    QDS1
     C                   PARM                    QDS2
     C                   PARM                    QDS3
     C                   PARM                    QDS4
     C                   PARM                    QDS5
     C                   PARM                    QRTCD
     C                   ENDIF                                                  NDIS = 0
     C*
     C                   END                                                    END BDCODE/BDSTAT
     C                   END                                                    END *IN60 EQ '0'
     C   86S4FSFL        IFEQ      'Y'
     C*
     C* Write error message only if it does not already exist
     C*
     C                   MOVEL(P)  'NW00001'     KKMSG#
     C*
     C     MGKY2         CHAIN     PMG03                              99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C* Find the next available message sequence number
     C*
     C                   MOVE      *HIVAL        KKMSEQ
     C*
     C     MGKY1         SETGT     PMG02
     C     MGKY          READPE(N) PMG02                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   ADD       1             MGMSEQ
     C*
     C                   ELSE
     C*
     C                   Z-ADD     1             MGMSEQ
     C*
     C                   ENDIF                                                  *IN99=OFF
     C*
     C                   Z-ADD     CACO#         MGCO#
     C                   Z-ADD     CASUB#        MGSUB#
     C                   Z-ADD     CAFND         MGFND
     C                   Z-ADD     CAFYR         MGFYR
     C                   Z-ADD     CACASE        MGCASE
     C                   Z-ADD     CADCS#        MGDCS#
     C                   Z-ADD     CAEOB#        MGEOB#
     C                   Z-ADD     CADSEQ        MGDSEQ
     C                   MOVEL(P)  'N'           MGSRC
     C                   MOVEL     *BLANKS       MGCSI
     C                   MOVEL(P)  'NW00001'     MGMSG#
     C                   Z-ADD     CARCN         MGRCN
     C                   MOVE      'A'           MGSTAT
     C                   MOVE      Q1USER        MGUSER
     C                   MOVE      Q1PGM         MGPGM
     C                   MOVE      Q1PGM         MGIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        MGTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MGDT
     C*
     C                   MOVE      Q1USER        MGIUSR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MGDT
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MGIDT
     C*
     C                   Z-ADD     MGTIME        MGITIM
     C*
     C                   WRITE     PMG01
     C*
     C                   ENDIF
     C*
     C* UPDATE FDCAP
     C*
     C     CAKY4         CHAIN     PCA02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  '300'         CACSCD
     C                   Z-ADD     *ZEROS        CAMRA
     C                   Z-ADD     *ZEROS        CAOTHR
     C                   Z-ADD     *ZEROS        CAOTH2
     C                   Z-ADD     *ZEROS        CAPAID
     C                   Z-ADD     *ZEROS        CANDIS
     C*
     C                   UPDATE    PCA02
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ELSE                                                   ELSE *IN60 EQ '0'
     C*
     C                   GOTO      NXTDET
     C*
     C                   END                                                    END *IN60 EQ '0'
E01  C                   ENDIF
     C*
     C                   ELSE                                                   ELSE XXSYS EQ 'H'
     C*
     C     XXSYS         IFEQ      'P'
     C*
     C                   MOVE      'N'           XXINCP                         IN CONTRACT NON
     C     S4KEY         SETGT     PS402
     C     NXTPHY        TAG
     C                   READP     PS402                                  60
     C     *IN60         IFEQ      '0'
     C     S4NWCD        ANDEQ     XXNWCD
     C     S4FEIN        ANDEQ     XXFEIN
     C     S4MOD         ANDEQ     XXMOD
     C     S4ENDT        IFEQ      0
     C                   Z-ADD     99999999      S4ENDT
     C                   ENDIF                                                  ENDT = 0
     C* THE SERVICE DATE MUST BE BETWEEN THE PROVIDERS CONTRACT DATES
     C*
     C     CAFDAT        IFGT      S4ENDT
     C                   GOTO      NXTPHY
     C                   END
     C     S4STAT        IFEQ      'I'
     C*
     C                   GOTO      NXTPHY
     C*
     C                   ENDIF
     C*
     C* IF THE SERVICE DATE IS WITHIN THE CONTRACT DATES MOVE DISCOUNT CODE
     C*
     C                   MOVE      S4DISC        XXCODE
     C* FLAG FOR WITHIN CONTRACT FOR NON-HOSPITAL
     C                   MOVE      'Y'           XXINCP                         IN CONTRACT NON
     C*
     C* GET THE DISCOUNT TYPE FROM THS DISCOUNT CODE TABLE
     C*
     C     BDKEY         SETGT     PBD01
     C     NXTBD1        TAG
     C                   READP     PBD01                                  60
     C*
     C*
     C     *IN60         IFEQ      '0'
     C     BDCODE        IFNE      XXCODE
     C     BDSTAT        OREQ      'I'
     C*
     C                   GOTO      NXTBD1
     C*
     C                   ELSE                                                   ELSE BDCODE/BDSTAT
     C*
     C                   MOVE      BDTYPE        XXTYPE            2
     C*
     C* CALL THE DISCOUNT PROGRAM TO DISCOUNT THIS LINE ITEM
     C*
     C                   MOVEL     'N96XXR'      XXPGM
     C                   MOVE      XXTYPE        XDTYPE
     C                   MOVE      XXCO#         QCO#              3
     C                   MOVE      XXSUB#        QSUB#             5
     C                   MOVE      XXFND         QFND              3
     C                   MOVE      XXFYR         QFYR              3
     C                   MOVE      XXCASE        QCASE             7
     C                   MOVE      XXFEIN        QFEIN             9
     C                   MOVE      XXMOD         QMOD              3
     C                   MOVE      XXCODE        QCODE             3
     C*
     C                   Z-ADD     S4BGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QCEDT             8
     C*
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QFDAT             8
     C*
     C                   MOVE      CADSEQ        QDSEQ             5
     C                   MOVE      XXSYS         QSYS              1
     C                   MOVE      *BLANKS       QFL1              1
     C                   MOVE      *BLANKS       QFL2              1
     C                   MOVE      *BLANKS       QFL3              1
     C                   MOVE      *BLANKS       QFL4              1
     C                   MOVE      *BLANKS       QFL5              1
     C                   MOVEL     XXNWCD        QDS1             10
     C                   MOVE      *BLANKS       QDS2             10
     C                   MOVE      *BLANKS       QDS3             10
     C                   MOVE      *BLANKS       QDS4             10
     C                   MOVE      *BLANKS       QDS5             10
     C                   MOVE      ' '           QRTCD             1
     C*
     C     CANDIS        IFNE      0
     C     CACHRG        SUB       CAMRA         XXWRK9            9 2
     C                   SUB       CANDIS        XXWRK9
     C                   SUB       CAOTHR        XXWRK9
     C                   SUB       CAOTH2        XXWRK9
     C     XXWRK9        IFNE      CAPAID
     C                   Z-ADD     0             XXNDIS
     C                   ENDIF                                                  WRK9 <> PAID
     C                   ENDIF                                                  NDIS <> 0
     C     XXNDIS        IFEQ      0
     C***** Beg Add *** 03/18/15 *******************************************
     C     XXPGM         ANDNE     'N96DXR'
     C***** End Add *** 03/18/15 *******************************************
     C                   CALL      XXPGM
     C                   PARM                    QCO#
     C                   PARM                    QSUB#
     C                   PARM                    QFND
     C                   PARM                    QFYR
     C                   PARM                    QCASE
     C                   PARM                    QFEIN
     C                   PARM                    QMOD
     C                   PARM                    QFDAT
     C                   PARM                    QDSEQ
     C                   PARM                    QCODE
     C                   PARM                    QCEDT
     C                   PARM                    QSYS
     C                   PARM                    QFL1
     C                   PARM                    QFL2
     C                   PARM                    QFL3
     C                   PARM                    QFL4
     C                   PARM                    QFL5
     C                   PARM                    QDS1
     C                   PARM                    QDS2
     C                   PARM                    QDS3
     C                   PARM                    QDS4
     C                   PARM                    QDS5
     C                   PARM                    QRTCD
     C                   ENDIF                                                  NDIS = 0
     C*
     C                   END                                                    END BDCODE/BDSTAT
     C                   END                                                    END *IN60 EQ '0'
     C   86S4FSFL        IFEQ      'Y'
     C*
     C* Write error message only if it does not already exist
     C*
     C                   MOVEL(P)  'NW00001'     KKMSG#
     C*
     C     MGKY2         CHAIN     PMG03                              99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C* Find the next available message sequence number
     C*
     C                   MOVE      *HIVAL        KKMSEQ
     C*
     C     MGKY1         SETGT     PMG02
     C     MGKY          READPE(N) PMG02                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   ADD       1             MGMSEQ
     C*
     C                   ELSE
     C*
     C                   Z-ADD     1             MGMSEQ
     C*
     C                   ENDIF                                                  *IN99=OFF
     C*
     C                   Z-ADD     CACO#         MGCO#
     C                   Z-ADD     CASUB#        MGSUB#
     C                   Z-ADD     CAFND         MGFND
     C                   Z-ADD     CAFYR         MGFYR
     C                   Z-ADD     CACASE        MGCASE
     C                   Z-ADD     CADCS#        MGDCS#
     C                   Z-ADD     CAEOB#        MGEOB#
     C                   Z-ADD     CADSEQ        MGDSEQ
     C                   MOVEL(P)  'N'           MGSRC
     C                   MOVEL     *BLANKS       MGCSI
     C                   MOVEL(P)  'NW00001'     MGMSG#
     C                   Z-ADD     CARCN         MGRCN
     C                   MOVE      'A'           MGSTAT
     C                   MOVE      Q1USER        MGUSER
     C                   MOVE      Q1PGM         MGPGM
     C                   MOVE      Q1PGM         MGIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        MGTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MGDT
     C*
     C                   MOVE      Q1USER        MGIUSR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MGDT
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MGIDT
     C*
     C                   Z-ADD     MGTIME        MGITIM
     C*
     C                   WRITE     PMG01
     C*
     C                   ENDIF
     C*
     C* UPDATE FDCAP
     C*
     C     CAKY4         CHAIN     PCA02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  '300'         CACSCD
     C                   Z-ADD     *ZEROS        CAMRA
     C                   Z-ADD     *ZEROS        CAOTHR
     C                   Z-ADD     *ZEROS        CAOTH2
     C                   Z-ADD     *ZEROS        CAPAID
     C                   Z-ADD     *ZEROS        CANDIS
     C*
     C                   UPDATE    PCA02
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ELSE                                                   ELSE *IN60 EQ '0'
     C*
     C                   GOTO      NXTDET
     C*
     C                   END                                                    END *IN60 EQ '0'
     C*
     C*
     C                   END                                                    END CAPRTY EQ '1'
     C*
     C                   END                                                    END CAPRTY EQ '4'
     C*
     C                   ENDIF                                                  EOB# = EOB#
     C*
     C                   END                                                    END CSOVRD NE 'Y'
     C*
     C                   END                                                    END *IN60 EQ '0'
     C*
     C* READ THE NEXT DETAIL CHARGE RECORD
     C*
     C     NXTDET        TAG
     C*
     C* CHECK THE 'WITHIN CONTRACT' FLAGS.
     C*
     C                   MOVE      *BLANK        XXINCN            1
     C     XXINCH        IFEQ      'Y'
     C     XXINCP        OREQ      'Y'
     C                   MOVE      'Y'           XXINCN
     C                   ENDIF
     C*
     C* IF CLIENT IS SUMMIT (010) AND HAS A NETWORK CONTRACT
     C* PROCEED.
     C*
     C     XXCLT#        IFEQ      '010'
     C     EONTWK        ANDNE     *BLANKS
     C*
     C                   MOVE      '0'           XXTEMP            1
     C                   MOVEL(P)  '398'         KKEOB
     C     DEKY3         CHAIN     PDE03                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      '1'           XXTEMP
     C                   ELSE
     C                   MOVEL(P)  '396'         KKEOB
     C     DEKY3         CHAIN     PDE03                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      '1'           XXTEMP
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C* IF THERE IS NO 398 OR 396 EOB NOTE,
     C* AND THE DATE OF SERVICE IS WITHIN THE CONTRACT PERIOD,
     C* CREATE A 398 EOB NOTE.
     C*
     C     XXTEMP        IFEQ      '0'
     C     XXINCN        ANDEQ     'Y'
     C* GET NEXT AVAILABLE
     C                   Z-ADD     *HIVAL        KKSEQ#
     C     DEKY1         SETGT     PDE02
     C     DEKY2         READPE    PDE02                                  99
     C     *IN99         IFEQ      *OFF
     C     DESEQ#        ADD       1             XXSEQ#
     C                   ELSE
     C                   Z-ADD     1             XXSEQ#
     C                   ENDIF
     C                   CLEAR                   PDE01
     C                   MOVE      CACO#         DECO#
     C                   MOVE      CASUB#        DESUB#
     C                   MOVE      CAFND         DEFND
     C                   MOVE      CAFYR         DEFYR
     C                   MOVE      CACASE        DECASE
     C                   MOVE      CASS#         DESS#
     C                   MOVE      CAACDT        DEACDT
     C                   MOVE      CAEOB#        DEEOB#
     C                   MOVE      CADSEQ        DEDSEQ
     C                   Z-ADD     XXSEQ#        DESEQ#
     C*
     C**** Beg Chg *** 07/18/23 **************************************
     C****               MOVE      '398'         DEEOB
     C                   MOVEL     '398'         DEEOBCD
     C**** End Chg *** 07/18/23 **************************************
     C                   MOVEL(P)  XXNOT1        DENOT1
     C                   MOVE      *BLANKS       DENOT2
     C                   MOVE      'A'           DESTAT
     C                   MOVE      'N'           DEESRC
     C                   Z-ADD     CARCN         DERCN
     C                   MOVE      Q1USER        DEUSER
     C                   MOVE      Q1PGM         DEPGM
     C                   MOVE      Q1USER        DEIUSR
     C                   MOVE      Q1PGM         DEIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DEDT
     C                   MOVEL     SRDTO         DEIDT
     C                   Z-ADD     DSTIME        DETIME
     C                   Z-ADD     DSTIME        DEITIM
     C                   WRITE     PDE01
     C                   ENDIF                                                  XXTEMP = '0'
A    C* IF THERE IS A 398 OR 396 EOB NOTE, CHECK FOR AN ADJUDICATION
     C* NOTE.  IF THERE ARE ANY, DELETE THEM ALL.
     C*
     C                   MOVEL(P)  'A'           KKESRC
     C     DEKY4         CHAIN     PDE04                              99
     C     *IN99         DOWEQ     *OFF
     C                   DELETE    PDE04
     C     DEKY4         READE     PDE04                                  99
     C                   ENDDO
     C*
     C                   ENDIF                                                  CLTID = '010'
     C*
     C  N86CAKY1         READE     PCA01                                  61
     C   86CAKY2         READE(N)  PCA02                                  61
     C*
     C                   END                                                     END *IN61 DOWEQ '0'
     C*
     C     EOKY          CHAIN     PEU01                              99
     C*
     C     EODSTY        IFEQ      'DS'
     C*
     C     EUICN         IFEQ      'IP'
     C     EUICN         OREQ      'IR'
     C     EUICN         OREQ      'IS'
     C     EUICN         OREQ      'IM'
     C*
     C                   MOVE      EORRN         QQRRN
     C                   CALL      'FA756R'
     C                   PARM                    QQRRN             9 0
     C                   PARM      ' '           QQRTCD            1
     C                   PARM                    QQALOW            9 2
     C                   PARM      'N'           QQCALL            1
     C**** Beg Chg *** 07/18/23 **************************************
     C****               PARM      '395'         QQEOB             3
     C                   PARM      '395'         QQEOB             6
     C**** End Chg *** 07/18/23 **************************************
     C                   PARM      ' '           QQMSG             7
     C                   PARM                    QCODE
     C*
     C                   ENDIF                                                   EUICN
     C*
     C                   ENDIF                                                   EODSTY = 'DS'
     C                   END                                                     END QQF NE ZERO
     C*
     C                   END                                                     END DO 50
     C*
     C*** Beg Add *** 04/30/13 ******************************************
      /Free
          //*** Begin Chg *** 04/30/19 **********************************
          //*** If xxclt# = '114'and BDTYPE = 'DV';
          If (xxclt# = '114' Or xxclt# = '152') and BDTYPE = 'DV';
          //*** End   Chg *** 04/30/19 **********************************
             inCO#  = %editc(xxco#:'X');
             inSUB# = %editc(xxsub#:'X');
             inFND  = %editc(xxfnd:'X');
             inFYR  = %editc(xxfyr:'X');
             inCASE = %editc(xxcase:'X');
             inFEIN = %editc(xxfein:'X');
             inMOD  = %editc(xxmod:'X');
             inFDAT = QFDAT;
             inDSEQ = QDSEQ;
             inCODE = QCODE;
             inCEDT = QCEDT;
             inSYS  = QSYS;
             inFL1  = QFL1;
             inFL2  = QFL2;
             inFL3  = QFL3;
             inFL4  = QFL4;
             inFL5  = QFL5;
             inDS1  = QDS1;
             inDS2  = QDS2;
             inDS3  = QDS3;
             inDS4  = QDS4;
             inDS5  = QDS5;
             inRTCD = QRTCD;
             inEOB# = EOEOB#;
             inRCN  = EORCN ;
             outCal = *blanks;
          Callp N96DVR(inCO#:inSUB#:inFND:inFYR:inCASE:inFEIN:inMOD:inFDAT:
                       inDSEQ:inCODE:inCEDT:inSYS:inFL1:inFL2:inFL3:inFL4:
                       inFL5:inDS1:inDS2:inDS3:inDS4:inDS5:inRTCD:inEOB#:
                       inRCN:outCal);
          Endif;
      /End-Free
     C*** Beg End *** 04/30/13 ******************************************
     C*
     C* GET THE DISCOUNT TYPE FROM THS DISCOUNT CODE TABLE
     C*
     C                   MOVE      XXCO#         QCO#
     C                   MOVE      XXSUB#        QSUB#
     C                   MOVE      XXFND         QFND
     C                   MOVE      XXFYR         QFYR
     C                   MOVE      XXCASE        QCASE
     C                   MOVE      XXFEIN        QFEIN
     C                   MOVE      XXMOD         QMOD
     C*
     C                   Z-ADD     EOFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QFDAT
     C*
     C***** Beg Add *** 03/18/15 *******************************************
     C                   IF        BDTYPE = 'DN'
     C***** End Add *** 03/18/15 *******************************************
     C                   CALL      'N96DNR'                             91
     C                   PARM                    QCO#
     C                   PARM                    QSUB#
     C                   PARM                    QFND
     C                   PARM                    QFYR
     C                   PARM                    QCASE
     C                   PARM                    QFEIN
     C                   PARM                    QMOD
     C                   PARM                    QFDAT
     C                   PARM                    QDSEQ
     C                   PARM                    QCODE
     C                   PARM                    QCEDT
     C                   PARM                    QSYS
     C                   PARM                    QFL1
     C                   PARM                    QFL2
     C                   PARM                    QFL3
     C                   PARM                    QFL4
     C                   PARM                    QFL5
     C                   PARM                    QDS1
     C                   PARM                    QDS2
     C                   PARM                    QDS3
     C                   PARM                    QDS4
     C                   PARM                    QDS5
     C                   PARM                    QRTCD
     C                   PARM      EOEOB#        QEOB#             9 0
     C                   PARM      EORCN         QRCN              3 0
     C***** Beg Add *** 03/18/15 *******************************************
     C                   ENDIF
     C***** End Add *** 03/18/15 *******************************************

     C***** Beg Add *** 03/18/15 *******************************************
     C*
     C                   IF        BDTYPE = 'DX'
     C*
     C                   CALL      'N96DXR'                             91
     C                   PARM                    QCO#
     C                   PARM                    QSUB#
     C                   PARM                    QFND
     C                   PARM                    QFYR
     C                   PARM                    QCASE
     C                   PARM                    QFEIN
     C                   PARM                    QMOD
     C                   PARM                    QFDAT
     C                   PARM                    QDSEQ
     C                   PARM                    QCODE
     C                   PARM                    QCEDT
     C                   PARM                    QSYS
     C                   PARM                    QFL1
     C                   PARM                    QFL2
     C                   PARM                    QFL3
     C                   PARM                    QFL4
     C                   PARM                    QFL5
     C                   PARM                    QDS1
     C                   PARM                    QDS2
     C                   PARM                    QDS3
     C                   PARM                    QDS4
     C                   PARM                    QDS5
     C                   PARM                    QRTCD
     C                   PARM      EOEOB#        QEOB#             9 0
     C                   PARM      EORCN         QRCN              3 0
     C*
     C                   ENDIF
     C*
     C***** End Add *** 03/18/15 *******************************************
     C*
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
