     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: F4051NTX                                          *
     H*    DESCRIPTION: ADJUDICATION (08/01/91 RULES)                 *
     H*   This program is called from F0070R (Claim Adjudication)     *
     H*   and will perform the actual adjudication of the detail      *
     H*   claim records written by F0070R.                            *
     H*                                                               *
     H*  This Sub-program is intended to be used to calculate the     *
     H*  amount that is due a provider of services, by the carrier    *
     H*  of the Workers' Compensation insurance.                      *
     H*                                                               *
     H*  It should be used for any service provided on or after       *
     H*  08/01/91.                                                    *
     H*****************************************************************
     H*  CREATED: C:32 08/15/95    PROJECT: TX001                     *
     H*****************************************************************
     H*  Specialty Codes used in this program:
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*
     H*  09/25/01 C:062 PROJECT: FSMNT
     H*                 EXPAND THE ADJUDICATION CODE HOLDING FIELD.
     H*
     H*  04/08/99 C:117 Support: MC095 Seq# 17568
     H*                 Added QQCO#, QQSUB#, QQFND to the *ENTRY PARMs
     H*                 and changed all occurences of UPCO#, UPSUB#,
     H*                 UPFND to the above mentioned fields.
     H*                                                               *
     H*  09/21/98 C:79  Project: MC152                                *
     H*                 1.  Make Dates year 2000 compatible.          *
     H*                 2.  Modify to work with New Provider bills    *
     H*                 3.  Use new FDCAP fields CACHRG, CAPAID, CAMRA*
     H*
     H*  09/04/98 C:31  Project: 95MAL
     H*                 Ntwk Disc:Lesser of FS/Chr
     H*                 1. Add update of CAMRAA
     H*
     H*  08/24/95 C:32  PROJ: TX001                                   *
     H*                 1.  CHGD SERVICE CODE 'IMPLT' TO '00002'      *
     H*                 2.  ADDED SERV CODE 'OUT' AND 'IR'            *
     H*                     WHICH PAY AT .90 OF BILLED                *
     H*                 3.  DEL OUTPATIENT CODE WHICH PAID LESSER OF  *
     H*                                                               *
     H*  08/31/95 C:32  PROJ: TX001
     H*                 1.  CHG EOMB# FROM NUMBERS TO LETTERS
     H*                     AND ERROR MESSAGES FOR THE EOMBS
     H*                 2.  ADD REVENUE CODE TO FMSVL1TX SERV CODE
     H*                     RETRIEVAL
     H*
     H*  09/06/95 C:32  PROJ: TX001
     H*                 1.  CHG 'OUT' EOMB LETTER FROM F TO M
     H*
     H*  01/02/96 C:32  PROJ: TX001
     H*                 1.  MULT RADI CODES BY CONVERSION FACTOR
     H*
     H*  03/27/96 C:32  PROJ: TX005
     H*                 1.  IF SERVICE DATE > 04/01/96, DO NOT USE
     H*                     THE RADI CONV FACTOR.
     H*****************************************************************
     H*  INDICATOR USAGE:                                            **
     H*****************************************************************
     FSMUPL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FFMSVL1TX  IF   E           K DISK
     FFTABLTX   IF   E           K DISK
      *** BEG DEL *** 09/21/99 ****************************************
     F********FDCAL7  UF  E           K        DISK
      *** END DEL *** 09/21/99 ****************************************
     F*** BEG DEL *** 09/21/98 ****************************************
     F********FADCL   IF  E           K        DISK
     F*** END DEL *** 09/21/98 ****************************************
     FFDCALO    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0O)
     FFDHCL     IF   E           K DISK
     F*** BEG ADD *** 09/21/98 ****************************************
     F* Message file logicals
     F*
     FFDMGL1    UF A E           K DISK
     FFDMGL5    IF   E           K DISK
     F                                     RENAME(PMG01:PMG02)
     F*  FDCAL13 is the Primary File Read (PCA01)
     FFDCAL13   UF   E           K DISK
     F                                     RENAME(PCA01:PCA13)
     FFDDEL     IF   E           K DISK
     F                                     RENAME(PDE01:PDE02)
     FFDDEL4    UF A E           K DISK
     FFAEOL4    IF   E           K DISK
     FFAEUL     IF   E           K DISK
     F*** END ADD *** 09/21/98 ****************************************
     F*

     D*
      *** BEG DEL *** 09/21/98 ****************************************
     D******************* QQS        30  5               DETAIL SEQ #
     D******************* QQC        30  2               ERROR MESSAGES
     D******************* QQF        50  6               FROM DATES
      *** END DEL *** 09/21/98 ****************************************
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  XXCMSV                 1     11
     D  XXRVCD                 1      3  0
     D  XXHCPC                 4      8
     D  XXUNIT                 9     11  0
     D*
     D*  DATE/TIME DATA STRUCTURE.
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  FSY                    1     10
     D                                     DIM(10)                              GROUP SYS CODE
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

     I*
     I*  RENAME THE DUPLICATE FIELD NAMES.
     I*
     IPCA0O
     I              CASVCD                      OXSVCD
     I              CACPT4                      OXCPT4
     I              CACO#                       OXCO#
     I              CASUB#                      OXSUB#
     I              CAFND                       OXFND
     I              CAFYR                       OXFYR
     I              CACASE                      OXCASE
     I              CAFDAT                      OXFDAT
     I              CADSEQ                      OXDSEQ
     I              CAFEIN                      OXFEIN
     I              CAMOD                       OXMOD
     I              CATOTA                      OXTOTA
     I              CACHRG                      OXTOTC
     I              CAMRA                       OXTOTD
     I              CAPAID                      OXTOTP
     I              CAPMOD                      OXPMOD
     I              CAUSER                      OXUSER
     I              CAPGM                       OXPGM
     I              CADT                        OXDT
     I              CATIME                      OXTIME
     I              CACHK#                      OXCHK#
     I              CAEOB#                      OXEOB#
     I              CADXR                       OXDXR
     I              CAFLAG                      OXFLAG
     I              CAUNTS                      OXUNTS

     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C***CLAIM*DETAIL FILE
     C********** CAKY1     KLIST
     C**********           KFLD           QQCO#
     C**********           KFLD           QQSUB#
     C**********           KFLD           QQFND
     C**********           KFLD           QQFYR
     C**********           KFLD           QQCASE
     C**********           KFLD           XXYMD
      *** END DEL *** 09/21/98 ****************************************
     C*
     C     CAKY1A        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
     C                   KFLD                    SVFEIN
     C                   KFLD                    SVMOD
     C                   KFLD                    SVPRTY
     C                   KFLD                    SVDSEQ
     C                   KFLD                    SVEOB#
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** KFLD           SVCHK#
      *** END DEL *** 09/21/98 ****************************************
     C*
     C     CAKY1Z        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
     C                   KFLD                    HLFEIN
     C                   KFLD                    HLMOD
     C                   KFLD                    HLPRTY
     C                   KFLD                    HLDSEQ
     C                   KFLD                    HLEOB#
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** KFLD           HLCHK#
      *** END DEL *** 09/21/98 ****************************************
     C*
     C     CAKY4         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C                   KFLD                    XXSVCD
     C                   KFLD                    XXCPT4
     C                   KFLD                    XXUNTS
     C*
     C*
     C     CAKYO         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
     C                   KFLD                    CAFEIN
     C                   KFLD                    XXSVCD
     C                   KFLD                    XXCPT4
     C                   KFLD                    XXUNTS
     C*
     C*  CLAIM MASTER FILE
     C     CMKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C*
     C*  Provider Code File
     C     PDKY          KLIST
     C                   KFLD                    SVFEIN
     C                   KFLD                    SVMOD
     C*
     C*  SERV CODE FILE
     C     SVKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    XXCMSV
     C                   KFLD                    CAFDAT
     C*
     C*  CLAIM STATUS CODE TABLE FILE
     C     TCSKY         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    TABKEY
      *** BEG DEL *** 09/21/98 ****************************************
     C**********
     C***DIAGNOSIS CODE/DETAIL CHARGE FILE
     C********** DCKY      KLIST
     C**********           KFLD           QQCO#
     C**********           KFLD           QQSUB#
     C**********           KFLD           QQFND
     C**********           KFLD           QQFYR
     C**********           KFLD           QQCASE
     C**********           KFLD           CAFEIN
     C**********           KFLD           CAMOD
      *** END DEL *** 09/21/98 ****************************************
     C*
     C*  USER PARAMETER FILE
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  DUMMY KEY USED FOR RELEASE OF ALL CA RECORDS
     C*
     C     DUMMY         KLIST
     C                   KFLD                    QQCO#
     C*
     C*   KEY LIST FOR THE FUND FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C*
     C* DETAIL CHARGE AUXILIARY FILE
     C*
     C     HCKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    CADSEQ
     C*** BEG ADD *** 09/21/98 ****************************************
     C*
     C* Adjudication Message File (PMG01)
     C*
     C     MGKY1         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    XXDSEQ
     C*
     C* Adjudication Message File (PMG02)
     C*
     C     MGKY2         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    XXDSEQ
     C                   KFLD                    XXMSG#
     C                   KFLD                    XXRCN
      *
     C     CAKY1         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    XXYMD
     C*
     C     CAKY2         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C*
     C     DEKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    QQDSEQ
     C                   KFLD                    QQESRC
     C*
     C     DEKY1         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C                   KFLD                    QQDSEQ
     C*
     C     EBKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    EBKEY
     C*
     C     EOKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C*
     C     EUKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    QQEOB#
     C*** END ADD *** 09/21/98 ****************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQFYR             3 0
     C                   PARM                    QQCASE            7 0
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** PARM           QQFDT 150
     C******************** PARM           QQFDT2150
     C******************** PARM           QQS
     C******************** PARM           QQC
      *** END DEL *** 09/21/98 ****************************************
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQSS#             9 0
     C                   PARM                    QQACDT            8 0          (MMDDYY ORD)
     C                   PARM                    QQSPCD            3
      *** BEG ADD *** 09/21/98 ****************************************
     C                   PARM                    QQEOB#            9 0
      *** END ADD *** 09/21/98 ****************************************
     C*
     C*** BEG ADD *** 04/08/99 ***************************************
     C                   PARM                    QQCO#             3 0
     C                   PARM                    QQSUB#            5 0
     C                   PARM                    QQFND             3 0
     C*** END ADD *** 04/08/99 ***************************************
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
     C***************************************************************
     C* INITIALIZATION SECTION
     C                   Z-ADD     *ZEROS        S                 3 0
     C                   Z-ADD     *ZEROS        Q                 3 0
     C                   Z-ADD     *ZEROS        XXALOW            9 2
     C*** BEGIN CHG *** 09/25/01 **************************************
     C**********           Z-ADD*ZEROS    XXOVRP  32
     C                   Z-ADD     *ZEROS        XXOVRP            5 4
     C*** END   CHG *** 09/25/01 **************************************
     C*
     C*** BEGIN ADD *** 01/02/96 **************************************
     C                   Z-ADD     001.75        XXRADI            5 2           RADI Conversion
     C*** END   ADD *** 01/02/96 **************************************
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYDATE            7 0
     C**
     C*
     C     *LIKE         DEFINE    CADSEQ        HLDSEQ                         5.0
     C     *LIKE         DEFINE    CAFEIN        HLFEIN                         9.0
     C     *LIKE         DEFINE    CAMOD         HLMOD                          3.0
     C     *LIKE         DEFINE    CAPRTY        HLPRTY                         1
     C     *LIKE         DEFINE    CAEOB#        HLEOB#                         9.0
      *** BEG DEL *** 09/21/98 ****************************************
     C********** *LIKE     DEFN CACHK#    HLCHK#           7.0
     C********** *LIKE     DEFN CACHK#    SVCHK#           7.0
      *** END DEL *** 09/21/98 ****************************************
     C     *LIKE         DEFINE    CADSEQ        SVDSEQ                         5.0
     C     *LIKE         DEFINE    CAEOB#        SVEOB#                         9.0
     C     *LIKE         DEFINE    CAFEIN        SVFEIN                         9.0
     C     *LIKE         DEFINE    CAMOD         SVMOD                          3.0
     C     *LIKE         DEFINE    CAPRTY        SVPRTY                         1
     C     *LIKE         DEFINE    PDSPCD        SVSPCD
     C     *LIKE         DEFINE    CAUNTS        SVUNTS                         5.2
     C     *LIKE         DEFINE    CSKEY         TABKEY                         10
     C     *LIKE         DEFINE    CACPT4        XPCPT4                         5
     C     *LIKE         DEFINE    CAEOMB        XPEOMB                         3
     C     *LIKE         DEFINE    CAPMOD        XPPMOD                         2
     C     *LIKE         DEFINE    CATOTA        XPTOTA                         9.2
     C     *LIKE         DEFINE    CAMRA         XPTOTD                         9.2
     C     *LIKE         DEFINE    CAPAID        XPTOTP                         9.2
     C     *LIKE         DEFINE    CADIFF        XPDIFF                         9.2
     C     *LIKE         DEFINE    CABRTC        XPBRTC
     C     *LIKE         DEFINE    CACPT4        XXCPT4                         5
     C     *LIKE         DEFINE    CAFDAT        XXDATE                         6.0
     C     *LIKE         DEFINE    CASVCD        XXSVCD                         4.0
     C     *LIKE         DEFINE    CAUNTS        XXUNTS                         5.2
     C     *LIKE         DEFINE    CAFDAT        XXYMD                          6.0
     C     *LIKE         DEFINE    CATOTA        XXINVC                         9.2
     C     *LIKE         DEFINE    CATOTA        XXDIEM                         9.2
     C*** BEGIN ADD *** 08/24/95 **************************************
     C     *LIKE         DEFINE    CATOTA        XXMRA                          9.2
     C*** END   ADD *** 08/24/95 **************************************
     C*
     C     *LIKE         DEFINE    CAFEIN        SXFEIN                         9.0
     C     *LIKE         DEFINE    CAMOD         SXMOD                          3.0
     C*** BEG ADD *** 09/21/98 ****************************************
     C     *LIKE         DEFINE    MGMSG#        XXMSG#
     C     *LIKE         DEFINE    MGMSEQ        XXMSEQ
     C     *LIKE         DEFINE    MGDSEQ        XXDSEQ
     C     *LIKE         DEFINE    DEESRC        QQESRC
     C     *LIKE         DEFINE    DEDSEQ        QQDSEQ
     C     *LIKE         DEFINE    DENOT1        XXNOT1
     C     *LIKE         DEFINE    DERCN         XXRCN
     C     *LIKE         DEFINE    EOSTYP        XXOUT
     C     *LIKE         DEFINE    EUICN         XXTOS
     C*** END ADD *** 09/21/98 ****************************************
     C*
     C***************************************************************
     C*
     C     QQRTCD        CABEQ     'P'           TERM                           PRE-OPEN FILES
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** MOVEAQQFDT     QQF,1
     C******************** MOVEAQQFDT2    QQF,26
      *** END DEL *** 09/21/98 ****************************************
     C*** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      'XX00000'     XXMSG#
     C*** END ADD *** 09/21/98 ****************************************
     C*
     C** GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C*
     C** IF THE GET WAS UNSUCCESSFUL END THE PROGRAM.
     C     *IN60         IFEQ      '1'
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** MOVE '99999'   QQS,1
     C******************** MOVE '95'      QQC,1
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '95'          XXMSG#
     C                   MOVE      '99999'       XXDSEQ
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C                   GOTO      TERM
     C                   END
     C*
     C** GET THE CLAIM MASTER RECORD.
     C*
     C     CMKY          CHAIN     PCM01                              60
     C*
     C** IF THE GET WAS UNSUCCESSFUL END THE PROGRAM.
     C*
     C     *IN60         IFEQ      '1'
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** MOVE '99998'   QQS,1
     C******************** MOVE '96'      QQC,1
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '96'          XXMSG#
     C                   MOVE      '99998'       XXDSEQ
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C                   GOTO      TERM
     C                   END
     C*
      *** BEG DEL *** 09/21/98 ****************************************
      ******************
     C** ADJUDICATE FOR EACH POSSIBLE DATE ENTERED IF AFTER 08/01/91
     C******************
     C******************   DO   50        S
     C******************   MOVE QQF,S     XXDATE
     C******************   Z-ADDXXDATE    SRDTI
     C******************   CALL 'SRCVTDAT'DTCYMD
     C******************   MOVELSRDTO     XXYMD
     C******************
     C***********QQF,S**   IFNE *ZERO
     C***********XXYMD**   ANDGE910801
     C***********XXYMD**   ANDLT970801                     ADD TX007
     C***********CAKY1**   CHAINPCA01                54
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C*
     C* Find the recon number.  If this is a recon.
     C     EOKY          CHAIN     PEO01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     EORCN         XXRCN
     C*
     C                   ELSE
     C                   Z-ADD     *ZERO         XXRCN
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C     CAKY2         SETLL     PCA13
     C     CAKY2         READE     PCA13                                  54
      *
     C     *IN54         DOWEQ     '0'                                          DO FOR EACH RCD
      *
     C     CAFDAT        IFGE      910801
     C     CAFDAT        ANDLT     970801                                       ADD TX007
     C     XXRCN         ANDEQ     CARCN
      *
     C                   Z-ADD     CAFDAT        XXYMD
      *** END ADD *** 09/21/98 ****************************************
     C*
     C                   Z-ADD     CADSEQ        SVDSEQ                         SAVE FIELD VALS
     C                   MOVE      CAFEIN        SVFEIN                         FOR RECORD WE
     C                   MOVE      CAMOD         SVMOD                          ARE PROCESSING
     C                   MOVE      CAPRTY        SVPRTY
     C                   Z-ADD     CAUNTS        SVUNTS
     C                   MOVEL     CACPT4        XPCPT4
     C                   MOVE      CAPMOD        XPPMOD
     C                   Z-ADD     CAEOB#        SVEOB#
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADDCACHK#    SVCHK#
      *** END DEL *** 09/21/98 ****************************************
     C* --- Get and save the Provider's Specialty Code ----------------
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      QQSPCD        SVSPCD
     C                   END
     C*
     C                   MOVE      CAFEIN        SXFEIN                         SET PROVIDER WE
     C                   MOVE      CAMOD         SXMOD                          ARE PROCESSING
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C*+*+*+*+*+*+* Do Loop for Each Claim Detail Record +*+*+*+*+*+*+*+*+*+**
     C********** *IN54     DOWEQ'0'                        DO FOR EACH RCD
     C**********           EXCPTRCA01
      *** END DEL *** 09/21/98 ****************************************
     C*
     C* If the Service Date is greater than the system date, give err message
     C     CAFDAT        IFGT      SYDATE
      *** BEG DEL *** 09/21/98 ****************************************
     C***********CAEOB# ** IFEQ *ZEROS
     C***********CACHK# ** ANDEQ*ZEROS
     C******************** Z-ADD1         Q
     C*********  '00000'** LOKUPQQS,Q                    60
     C* N60 ************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '79'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '79'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C*** Err Msg 79: "Service Date entered is greater than today's date
     C*                (system date)."
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END                             END EQ *ZEROS
      *** END DEL *** 09/21/98 ****************************************
     C                   END                                                    END GT SYDATE
     C*
     C** GET THE CLAIM STATUS RECORD. SEE IF OVERRIDE FLAG IS 'Y'.
     C*
     C                   MOVE      *BLANK        TABKEY
     C                   MOVEL     CACSCD        TABKEY
     C     TCSKY         CHAIN     PCS01                              60
     C*
     C** A HARD ERROR WILL CAUSE THE PROGRAM TO TERMINATE ASAP.
     C*
     C     *IN60         IFEQ      '1'
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CAEOB#    IFEQ *ZEROS
     C********** CACHK#    ANDEQ*ZEROS
     C**********           Z-ADD1         Q
     C********** '00000'   LOKUPQQS,Q                    60
     C**N60*****           Z-ADD30        Q
     C**********           MOVELCADSEQ    QQS,Q
     C**********           MOVE '97'      QQC,Q            INVALID CLM STS CODE
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '97'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C*** Err Msg 97: "Invalid Claim Status Code, See Programmer"
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C                   END
     C*
     C* * * IF OVERRIDE FLAG IS NOT EQUAL TO 'Y' THEN PROCESS THIS RECORD
     C* * * ELSE DO NOT ADJUDICATE.
     C*
     C     CSOVRD        IFNE      'Y'
     C*
     C* ===== NON-OVERRIDE TYPE ======================================
     C*** CHECK FOR LEVEL BREAK (PROVIDER)
     C     SXFEIN        IFNE      CAFEIN
     C     SXMOD         ORNE      CAMOD
     C                   EXSR      NEWPRO
     C                   END
     C*
     C* ===== NON-HOSPITAL TYPE ======================================
     C     CAPRTY        IFNE      '4'
     C                   GOTO      NXTRCD
     C                   END
     C*
     C* ===== HOSPITAL TYPE ==========================================
     C                   EXSR      HOSP
     C*
     C                   ELSE                                                   OVERRIDE TYPE
     C*
     C** TEST FOR A POSSIBLE DUPLICATE CLAIM ON HOSPITAL.
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CAEOB#    IFEQ *ZERO
      *** END DEL *** 09/21/98 ****************************************
     C                   MOVE      CASVCD        XXSVCD
     C                   MOVE      CACPT4        XXCPT4
     C                   MOVE      CAUNTS        XXUNTS
     C*
     C     CAKYO         CHAIN     PCA0O                              54
     C*
     C     DUPCL3        TAG
     C*
     C     *IN54         IFEQ      '0'
     C*
     C     CADSEQ        IFNE      OXDSEQ
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CADSEQ ** OREQ OXDSEQ
     C********** CAEOB# ** ANDNEOXEOB#
     C*
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    61
     C**N61*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '03'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '03'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C*** Err Msg 03: "Possible Duplicate Claim exists for this item"
     C                   END
     C                   ELSE
     C*
     C     CAKYO         READE     PCA0O                                  54                3
     C  N54              GOTO      DUPCL3
     C                   END
     C                   END
     C*
     C* ===== OVERRIDE TYPE ==========================================
      *** BEG DEL *** 09/21/99 ****************************************
     C********** CAKY1A    CHAINPCA01                54
     C********** *IN54     IFEQ '0'
      *** END DEL *** 09/21/99 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C     CAEOB#        IFEQ      QQEOB#
      *** END ADD *** 09/21/98 ****************************************
     C*** IF OVERRIDE TYPE WE MUST STILL OUTPUT THE ALLOWED/DISALLOWED ETC.
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CAEOB#    IFEQ *ZERO
      *** END DEL *** 09/21/98 ****************************************
     C     CAMRA         ANDEQ     0
     C     CAOTHR        ANDEQ     0
     C     CAOTH2        ANDEQ     0
     C     CANDIS        ANDEQ     0
     C     CAPAID        ANDEQ     0
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CATOTA    ANDEQ0
      *** END DEL *** 09/21/98 ****************************************
     C*
      *** BEG ADD *** 09/21/98 ****************************************
     C                   Z-ADD     *ZEROS        XPTOTA
      *** END ADD *** 09/21/98 ****************************************
     C     CSOVRP        IFGT      *ZERO
     C     CSOVRP        DIV       100           XXOVRP
     C     CACHRG        MULT      XXOVRP        XPTOTA
     C                   END
     C*
     C                   Z-ADD     *ZEROS        CABRTC
     C     CACHRG        SUB       XPTOTA        CAMRA
     C                   Z-ADD     XPTOTA        CAPAID
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
      *** BEG CHG *** 09/21/99 ****************************************
     C******************** UPDATPCA01
     C                   UPDATE    PCA13
      *** END CHG *** 09/21/99 ****************************************
     C                   END
     C*
     C                   END
     C*
     C     NXTRCD        TAG
     C*
     C*
     C*+*+*+*+*+*+*+*+*+*+ Read Next Claim Detail Record +*+*+*+*+*+*+*+*+*+**
      *** BEG CHG *** 09/21/98 ****************************************
     C********** CAKY1     READEPCA01                    54  GET NEXT RCD
     C     CAKY2         READE     PCA13                                  54      GET NEXT RCD
      *** END CHG *** 09/21/98 ****************************************
     C*                                                      TO PROCESS
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C********** *IN54     IFEQ '0'                        SAVE FIELDS OF
     C**********           Z-ADDCADSEQ    SVDSEQ           CURRENT REC.
     C**********           MOVE CAFEIN    SVFEIN
     C**********           MOVE CAMOD     SVMOD
     C**********           MOVE CAPRTY    SVPRTY
     C**********           Z-ADDCAUNTS    SVUNTS
     C**********           MOVELCACPT4    XPCPT4
     C**********           MOVE CAPMOD    XPPMOD
     C**********           Z-ADDCAEOB#    SVEOB#
     C**********           Z-ADDCACHK#    SVCHK#
      *** END DEL *** 09/21/98 ****************************************
     C*
     C*----------------------------------------------------------------
     C* --- Get and save the Provider's Specialty Code ----------------
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      QQSPCD        SVSPCD
     C                   END
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   ENDDO                                                  *IN54=OFF
      *** END ADD *** 09/21/98 ****************************************
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
     C********************
     C******************** END
     C******************** END
      *** BEG DEL *** 09/21/98 ****************************************
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C*
     C** RELEASE ALL THE FORMATS FOR A SAVE RETURN.
     C*
      *** BEG DEL *** 09/21/99 ****************************************
     C********** DUMMY     CHAINPCA01                54
     C**N54*****           EXCPTRCA01
      *** END DEL *** 09/21/99 ****************************************
     C*
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     0             XXMRA
     C*** END ADD *** 09/04/98 ****************************************
     C                   RETURN
     C                   MOVE      '1'           *INLR
     *** BEG ADD *** 09/21/98 *****************************************
     *********************************************************************
     *** UPDATE FADEP WITH EOMB                                E O M B   *
     *********************************************************************
     C     EOMB          BEGSR
     C*
     C* Find the EOMB description
     C                   MOVEL(P)  XPEOMB        EBKEY
     C*
     C     EBKY          CHAIN     PEB01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL     EBDESC        XXNOT1
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       XXNOT1
     C*
     C                   ENDIF                                                  *IN60=ON
     C*
     C                   MOVE      'A'           QQESRC
     C                   MOVE      CADSEQ        QQDSEQ
     C*
     C* Determine if an EOMB already exists for this DSEQ number
     C     DEKY          CHAIN     PDE01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVE      XPEOMB        DEEOB
     C                   MOVEL(P)  XXNOT1        DENOT1
     C*
     C* Update file maintenance fields
     C                   MOVE      Q1USER        DEUSER
     C                   MOVE      Q1PGM         DEPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C*
     C                   CALL      'SRCVTDAT'    DTCYMD
     C*
     C                   MOVEL     SRDTO         DEDT
     C                   Z-ADD     DSTIME        DETIME
     C*
     C                   UPDATE    PDE01
     C*
     C                   ELSE
     C*
     C* No EOMB exists so find the next sequence number and add to file
     C     DEKY1         SETGT     PDE02
     C     DEKY1         READPE    PDE02                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C                   ADD       1             DESEQ#
     C*
     C                   ELSE
     C                   Z-ADD     1             DESEQ#
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   MOVE      CACO#         DECO#
     C                   MOVE      CASUB#        DESUB#
     C                   MOVE      CAFND         DEFND
     C                   MOVE      CAFYR         DEFYR
     C                   MOVE      CACASE        DECASE
     C                   MOVE      CASS#         DESS#
     C                   MOVE      CAACDT        DEACDT
     C                   MOVE      CAEOB#        DEEOB#
     C                   MOVE      CADSEQ        DEDSEQ
     C                   MOVE      XPEOMB        DEEOB
     C                   MOVE      XXNOT1        DENOT1
     C                   MOVE      *BLANKS       DENOT2
     C                   MOVE      'A'           DESTAT
     C                   MOVE      'A'           DEESRC
     C                   Z-ADD     XXRCN         DERCN
     C*
     C* Update file maintenance fields
     C                   MOVE      Q1USER        DEUSER
     C                   MOVE      Q1PGM         DEPGM
     C                   MOVE      Q1USER        DEIUSR
     C                   MOVE      Q1PGM         DEIPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C*
     C                   CALL      'SRCVTDAT'    DTCYMD
     C*
     C                   MOVEL     SRDTO         DEDT
     C                   MOVEL     SRDTO         DEIDT
     C                   Z-ADD     DSTIME        DETIME
     C                   Z-ADD     DSTIME        DEITIM
     C*
     C                   WRITE     PDE01
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C     EOMBE         ENDSR
     *** END ADD *** 09/21/98 *****************************************
     *** BEG ADD *** 09/21/98 *****************************************
     *********************************************************************
     *** ADJUDICATION MESSAGES SUBROUTINE                    M S G       *
     *********************************************************************
     C     MSG           BEGSR
      *
      * Load the Key fields
      *
     C                   MOVE      CADSEQ        XXDSEQ
      *
      * Is this message already in the file for this line item
      *
     C     MGKY2         CHAIN     PMG02                              60
      *
     C     *IN60         IFEQ      *ON
      *
      * Find the next available message sequence number
      *
     C     MGKY1         SETGT     PMG01
     C     MGKY1         READPE(N) PMG01                                  60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     MGMSEQ        ADD       1             XXMSEQ
      *
     C                   ELSE
      *
     C                   Z-ADD     1             XXMSEQ
      *
     C                   ENDIF                                                  *IN60 = OFF
      *
      * Write the new adjudication message to FDMGP
      *
     C                   Z-ADD     QQCO#         MGCO#
     C                   Z-ADD     QQSUB#        MGSUB#
     C                   MOVE      QQFND         MGFND
     C                   MOVE      QQFYR         MGFYR
     C                   MOVE      QQCASE        MGCASE
     C                   MOVE      *ZEROS        MGDCS#
     C                   Z-ADD     QQEOB#        MGEOB#
     C                   Z-ADD     XXDSEQ        MGDSEQ
     C                   Z-ADD     XXMSEQ        MGMSEQ
     C                   MOVE      'S'           MGSRC
     C                   MOVE      *BLANKS       MGCSI
     C                   MOVE      XXMSG#        MGMSG#
     C                   MOVE      'A'           MGSTAT
     C                   Z-ADD     XXRCN         MGRCN
      *
     C                   MOVE      Q1USER        MGUSER
     C                   MOVE      Q1PGM         MGPGM
     C                   MOVE      Q1PGM         MGIPGM
     C                   MOVE      Q1USER        MGIUSR
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSTIME        MGTIME
     C                   Z-ADD     MGTIME        MGITIM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MGDT
     C                   MOVEL     SRDTO         MGIDT
      *
     C                   WRITE     PMG01
      *
     C                   ENDIF                                                  *IN60 = OFF
      *
     C     MSGE          ENDSR
     *** END ADD *** 09/21/98 *****************************************
     *********************************************************************
     *** NEW PROVIDER SUBROUTINE (LEVEL BREAK)               N E W P R O *
     *********************************************************************
     C     NEWPRO        BEGSR
     C*
     C                   Z-ADD     CADSEQ        HLDSEQ                         SAVE FIELD VALS
     C                   MOVE      CAFEIN        HLFEIN                         FOR RECORD WE
     C                   MOVE      CAMOD         HLMOD                          ARE PROCESSING
     C                   MOVE      CAPRTY        HLPRTY
     C                   Z-ADD     CAEOB#        HLEOB#
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADDCACHK#    HLCHK#
      *** END DEL *** 09/21/98 ****************************************
     C*
     C**** NOT NEEDED *************
     C                   Z-ADD     HLDSEQ        CADSEQ                         SAVE FIELD VALS
     C                   MOVE      HLFEIN        CAFEIN                         FOR RECORD WE
     C                   MOVE      HLMOD         CAMOD                          ARE PROCESSING
     C                   MOVE      HLPRTY        CAPRTY
     C                   Z-ADD     HLEOB#        CAEOB#
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADDHLCHK#    CACHK#
      *** END DEL *** 09/21/98 ****************************************
     C*** REGET RECORD.
      *** BEG DEL *** 09/21/99 ****************************************
     C********** CAKY1Z    CHAINPCA01                54
     C**********
     C**N54*****           EXCPTRCA01
      *** END DEL *** 09/21/99 ****************************************
     C*
     C                   Z-ADD     CADSEQ        SVDSEQ
     C                   MOVE      CAFEIN        SVFEIN                            CURR REC.
     C                   MOVE      CAMOD         SVMOD
     C                   MOVE      CAPRTY        SVPRTY
     C                   Z-ADD     CAEOB#        SVEOB#
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** Z-ADDCACHK#    SVCHK#
      *** END DEL *** 09/21/98 ****************************************
     C*
     C*----------------------------------------------------------------
     C* Get and save the Provider's Specialty Code
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      QQSPCD        SVSPCD
     C                   END
     C*
     C                   MOVE      CAFEIN        SXFEIN
     C                   MOVE      CAMOD         SXMOD
     C*
     C     NEWPRE        ENDSR
     *********************************************************************
     *** HOSPITAL SUBROUTINE   SERVICE CODES                     H O S P *
     *********************************************************************
     C     HOSP          BEGSR
     C*
     C*----------------------------------------------------------------
     C* Get INPATIENT / OUTPATIENT FLAG
     C                   MOVE      *BLANKS       XXOUT
      *** BEG ADD *** 09/21/98 ****************************************
      *
     C     EOKY          CHAIN     PEO01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   MOVE      EOSTYP        XXOUT
      *
     C                   ENDIF                                                  *IN60=OFF
      *
     C     EUKY          CHAIN     PEU01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   MOVE      EUICN         XXTOS
      *
     C                   ENDIF                                                  *IN60=OFF
      *** END ADD *** 09/21/98 ****************************************
      *** BEG DEL *** 09/21/98 ****************************************
     C********** DCKY      CHAINPDC01                60
     C********** *IN60     IFEQ '0'
     C**********           SUBSTDC10A1:6  XXOUT   3
     C**********           END
      *** END DEL *** 09/21/98 ****************************************
     C*
     C*-- GET INVOICE AMOUNT FOR IMPLANTABLES -----------------------------
     C*** BEGIN CHG *** 08/24/95 **************************************
     C********** CACPT4    IFEQ 'IMPLT'
     C     CACPT4        IFEQ      '00002'
     C*** END   CHG *** 08/24/95 **************************************
     C*
     C* CHAIN TO THE DETAIL CHARGE AUXILIARY FILE TO GET INVOICE AMT
     C*
     C     HCKY          CHAIN     PHC01                              60
     C     *IN60         IFEQ      '0'
     C*
     C     HC92N2        MULT      1.10          XXINVC
     C*
     C     CACHRG        IFLE      XXINVC
     C*
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     CACHRG        XPTOTA
     C                   Z-ADD     *ZEROS        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC
     C                   ELSE
     C*
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     XXINVC        XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC
     C*
     C                   END                                                    END CACHRG
     C*
     C*
     C* Update charge with payment
     C*
     C                   EXSR      UPDAT
     C*
     C                   END                                                    END *IN60 EQ '0'
     C*
     C                   ELSE                                                   ELS CACPT4 =IMPLT
     C*--------------------------------------------------------------------
     C*
     C*** USE STANDARD (000xxxxx000) AS SVKY (I.E. 000ROOMM000)
     C*** TO RETRIEVE SERVICE CODE RECS (FOR PER DIEM RATES)
     C*** BEGIN CHG *** 08/31/95 **************************************
     C                   Z-ADD     CASVCD        XXRVCD
     C******************** Z-ADD*ZEROS    XXRVCD
     C*** END   CHG *** 08/31/95 **************************************
     C                   MOVE      CACPT4        XXHCPC
     C                   Z-ADD     *ZEROS        XXUNIT
     C*
     C     SVKY          SETGT     PSV01                                        GET SERV CODE
     C     NXTSV1        TAG
     C                   READP     PSV01                                  60     RECORD
     C     *IN60         IFEQ      '0'
     C     SVCO#         ANDEQ     CACO#
     C     SVCMSV        ANDEQ     XXCMSV
     C     SVEFDT        ANDLE     CAFDAT                                       ADD 08/24/95
     C     SVSTAT        CABEQ     'I'           NXTSV1
     C                   END
     C*
     C     *IN60         IFEQ      '1'
     C     SVCO#         ORNE      CACO#
     C     SVCMSV        ORNE      XXCMSV
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     *ZERO         XPDIFF
     C*** BEGIN CHG *** 08/31/95 *************************************
     C*****************    MOVE '217'     XPEOMB
     C*** EOMB 217: "This charge is included in the basic allowance of
     C***           another."
     C*
     C                   MOVE      'G  '         XPEOMB
     C*** EOMB G:   "INCLUDED IN GLOBAL CHARGE"
     C*** END   CHG *** 08/31/95 *************************************
     C*
     C                   EXSR      UPDAT
     C                   GOTO      HOSPE
     C                   END                                                    END *IN60=1
     C*
     C*** BEGIN ADD *** 08/24/95 **************************************
     C     CACPT4        IFEQ      'IR'
     C     CACPT4        OREQ      'OUT'
     C     CACHRG        MULT      .90           XXMRA
     C*
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     XXMRA         XPTOTA
     C     CACHRG        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC
     C*
     C                   EXSR      UPDAT
     C                   GOTO      HOSPE
     C*
     C                   END                                                    END CACPT4
     C*
     C*** END   ADD *** 08/24/95 **************************************
     C*
     C* If Code is found in the file and it has an MRA amount, pay
     C*
     C*
     C     CAUNTS        IFGT      1
     C     SVALOW        MULT(H)   CAUNTS        XXDIEM
     C                   ELSE
     C                   Z-ADD     SVALOW        XXDIEM
     C                   END
     C*
     C*
     C*** BEGIN DEL *** 08/24/95 **************************************
     C***  IF OUTPATIENT TYPE BILL, CAN PAY THE LESSER OF
     C***  CHARGED VS PER DIEM
     C*********
     C*********  XXOUT     IFEQ 'OUT'
     C*********  CACHRG    ANDLTXXDIEM
     C*** END   DEL *** 08/24/95 **************************************
     C***  BEGIN ADD *** 08/24/95 *************************************
     C***  RADIOLOGY CPT4 CODES FOR CAT SCANS AND MRI CODES
     C***    HAVE BEEN ADDED TO THE SERVICE CODE MASTER FILE
     C***    WITH THESE CODES PAY THE LESSER OF BILLED CHARGES
     C***    VS. MRA (WHICH IS FEE SCHEDULE) AMOUNT
     C***
     C                   SUBST     CACPT4:1      XXRAD7            1            RADIOLOGY CPT4
     C*** BEGIN ADD *** 01/02/96 **************************************
     C     XXRAD7        IFEQ      '7'
     C     CAFDAT        IFLT      960401                                       ADD 03/27/96
     C     XXDIEM        MULT      XXRADI        XXDIEM                         RADI CONV   sion
     C                   ENDIF                                                  ADD 03/27/96
     C                   ENDIF
     C*** END   ADD *** 01/02/96 **************************************
     C     XXRAD7        IFEQ      '7'
     C     CACHRG        ANDLT     XXDIEM
     C                   Z-ADD     CACHRG        XPTOTA
     C                   Z-ADD     *ZEROS        XPTOTD
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     XPTOTA        XPBRTC
     C*
     C                   ELSE                                                   ELSE XXRAD7
     C*
     C*
     C     CACHRG        IFLE      XXDIEM
     C     XXDIEM        SUB       CACHRG        XPDIFF
     C                   Z-ADD     *ZEROS        XPTOTD
     C                   Z-ADD     CACHRG        XPTOTA
     C                   Z-ADD     CACHRG        XPTOTP
     C                   Z-ADD     CACHRG        XPBRTC
     C*
     C                   ELSE
     C                   Z-ADD     *ZEROS        XPDIFF
     C     CACHRG        SUB       XXDIEM        XPTOTD
     C                   Z-ADD     XXDIEM        XPTOTA
     C                   Z-ADD     XXDIEM        XPTOTP
     C                   Z-ADD     XXDIEM        XPBRTC
     C*
     C                   END                                                    END CACHRG
     C                   END                                                    END XXRAD7
     C*
     C     CACPT4        IFEQ      'MISC'
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   END
     C*
     C                   EXSR      UPDAT
     C*
     C                   END                                                    END CACPT4
     C*
     C     HOSPE         ENDSR
     *********************************************************************
     *** UPDATE SUBROUTINE                                     U P D A T *
     *********************************************************************
     C     UPDAT         BEGSR
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** CHARGES EXCEED MAXIMUM ALLOWANCE FOUND IN REIMBURSEMENT MANUAL
     C*
     C     XPTOTD        IFGT      *ZERO
     C     XPEOMB        ANDEQ     *BLANK
     C     XPTOTA        ANDGT     *ZEROS
     C*** BEGIN CHG *** 08/31/95 *************************************
     C*****************    MOVE '202'     XPEOMB
     C*** EOMB 202: "Charges exceed Maximum Allowance found in reimburse manual"
     C*
     C                   MOVE      'F  '         XPEOMB
     C*** EOMB F:   "REDUCTION ACCORDING TO FEE GUIDELINES"
     C*** END   CHG *** 08/31/95 *************************************
     C*** BEGIN ADD *** 09/06/95 *************************************
     C*** IF REDUCED BY TO .90 USE 'M' INSTEAD OF 'F' FOR EOMB
     C***
     C     CACPT4        IFEQ      'IR'
     C     CACPT4        OREQ      'OUT'
     C*
     C                   MOVE      'M  '         XPEOMB
     C*** EOMB M:   "REDUCED TO FAIR AND REASONABLE"
     C                   END
     C*** END   ADD *** 09/06/95 *************************************
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     *** BEG ADD *** 09/21/98 *****************************************
     C* Call subroutine to write EOMB to FADEP
     C     XPEOMB        IFNE      *BLANKS
      *
     C                   EXSR      EOMB
      *
     C                   ENDIF                                                  XPEOMB NE BLANK
     C* If a reduction was made and no EOMB code assigned then write
     C* a message for the user to assign an EOMB code.
     C     XPTOTP        IFNE      CACHRG
     C     XPEOMB        ANDEQ     *BLANKS
      *
     C                   MOVE      'BU'          XXMSG#
     C                   EXSR      MSG
      *
     C                   ENDIF                                                  XPEOMB NE BLANK
     *** END ADD *** 09/21/98 *****************************************
     C*
     C** TEST FOR A POSSIBLE DUPLICATE CLAIM ON HOSPITAL.
     C*
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CAEOB#    IFEQ *ZERO
      *** END DEL *** 09/21/98 ****************************************
     C                   MOVE      CASVCD        XXSVCD
     C                   MOVE      CACPT4        XXCPT4
     C                   MOVE      CAUNTS        XXUNTS
     C*
     C     CAKYO         CHAIN     PCA0O                              54
     C*
     C     DUPCL2        TAG
     C*
     C     *IN54         IFEQ      '0'
     C*
     C     CADSEQ        IFNE      OXDSEQ
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CADSEQ ** OREQ OXDSEQ
     C********** CAEOB# ** ANDNEOXEOB#
     C*
     C******************** Z-ADD1         Q
     C***********'00000'** LOKUPQQS,Q                    61
     C**N61*************** Z-ADD30        Q
     C******************** MOVELCADSEQ    QQS,Q
     C******************** MOVE '03'      QQC,Q
      *** END DEL *** 09/21/98 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C                   MOVE      '03'          XXMSG#
     C                   EXSR      MSG
      *** END ADD *** 09/21/98 ****************************************
     C*** Err Msg 03: "Possible Duplicate Claim exists for this item"
     C                   END
     C                   ELSE
     C*
     C     CAKYO         READE     PCA0O                                  54
     C  N54              GOTO      DUPCL2
     C                   END
      *** BEG DEL *** 09/21/98 ****************************************
     C******************** END
     C******************** END
      *** END DEL *** 09/21/98 ****************************************
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** ONLY PROCESS THE RECORD IF NOT A OVERRIDE TYPE.
     C*
     C*
      *** BEG DEL *** 09/21/99 ****************************************
     C********** CAKY1A    CHAINPCA01                54
     C********** CSOVRD    IFNE 'Y'
      *** END DEL *** 09/21/99 ****************************************
      *** BEG ADD *** 09/21/98 ****************************************
     C     CAEOB#        IFEQ      QQEOB#
      *** END ADD *** 09/21/98 ****************************************
      *** BEG DEL *** 09/21/98 ****************************************
     C********** CACHK#    ANDEQ*ZERO
     C********** CAEOB#    ANDEQ*ZERO
      *** END DEL *** 09/21/98 ****************************************
     C                   Z-ADD     XPTOTA        CATOTA
     C                   Z-ADD     XPTOTD        CAMRA
     C                   Z-ADD     XPTOTP        CAPAID
     C                   Z-ADD     XPDIFF        CADIFF
     C                   Z-ADD     XPBRTC        CABRTC
     C                   MOVE      XPPMOD        CAPMOD
     C                   MOVE      XPEOMB        CAEOMB
     C*
     C                   MOVE      Q1USER        CAUSER
     C                   MOVE      Q1PGM         CAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   Z-ADD     DSTIME        CATIME
     C*
     C*
     C*** BEGIN ADD *** 09/04/98 **************************************
     C                   Z-ADD     XXMRA         CAMRAA
     C*** END ADD *** 09/04/98 ****************************************
      *** BEG CHG *** 09/21/99 ****************************************
     C**N54*************** UPDATPCA01
     C                   UPDATE    PCA13
      *** END CHG *** 09/21/99 ****************************************
     C                   END
     C*
     C* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
     C*
     C** CLEAR THE WORK SAVE FIELDS.
     C*
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTD
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZERO         XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   MOVE      *BLANKS       XPPMOD
     C                   MOVE      *BLANK        XPEOMB
     C*
     C     UPDAE         ENDSR
      *** BEG DEL *** 09/21/99 ****************************************
     O********* PCA01   E                RCA01
      *** END DEL *** 09/21/99 ****************************************
