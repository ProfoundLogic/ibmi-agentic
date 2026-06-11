     Hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*****************************************************************
    H* StoneRiver is a trademark of StoneRiver, Inc.                 *
     H*****************************************************************
     H*    PROGRAM: F9511RTX                                          *
     H*    DESCRIPTION: CREATE WORKFILE AND REPORT                    *
     H*                                                               *
     H*    CALLED FROM:                                               *
     H*      F9511CTX - CREATE WORKFILE                               *
     H*    CALLS TO:                                                  *
     H*      NONE                                                     *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*
     H*  06/09/20 C:295  Project : SY420
     H*                  Mask SSN
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  08/30/99 C:79 Support: 19312                                 *
     H*                Select only bills for Florida                  *
     H*                                                               *
     H*  01/06/99 C:79 PROJ:TX009                                     *
     H*                1.  Modify to work with YR 2000 dates          *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*  40   -   PRINT ONE DETAIL 2 LINE ON HOSPITAL BILLS           *
     H*  60   -   GENERAL CHAIN                                       *
     H*  61   -   GENERAL CHAIN                                       *
     H*  62   -   GENERAL CHAIN                                       *
     H*  63   -   GENERAL CHAIN                                       *
     H*  64   -   GENERAL CHAIN                                       *
     H*  65   -   OUT/MISC/IR HIT; DO NOT PRINT MORE DETAIL RECORDS   *
     H*  76   -   OUT/MISC/IR DETAIL                                  *
     H*  77   -   FSMRA DETAIL                                        *
     H*  78   -   WRITE X-DETAIL TO REPORT                            *
     H*  79   -   MOVE BATCH CONTROL HEADER RECORD TO FILE            *
     H*  80   -   NO HIT ON FMTSP - ADD NEW SEQ# RECORD BY CO         *
     H* N80   -   HIT ON FMTSP - UPDATE SEQ# RECORD BY CO             *
     H*  89   -   PHYSICIAN                                           *
     H* N89   -   HOSPITAL                                            *
     H*  90   -   PHYSICIAN (CALLED FOR BY USER)                      *
     H* N90   -   HOSPITAL (CALLED FOR BY USER)                       *
     H*  91   -   ACUTE INPATIENT                                     *
     H*  92   -   ICU/CCU ACUTE INPAIENT                              *
     H*  93   -   IMPLANTABLES UNDER ACUTE INPATIENT                  *
     H*  94   -   MRI/CT SCANS UNDER ACUTE INPATIENT                  *
     H*  95   -   MRI/CT SCANS UNDER ACUTE INPATIENT                  *
     H*  96   -   MRI/CT SCANS UNDER ACUTE INPATIENT                  *
     H*  97   -   MRI/CT SCANS UNDER ACUTE INPATIENT                  *
     H*  98   -   OUT/MISC/IR FOR REPORT                              *
     H*  99   -   NO EOMB FOR FUND WITHIN DATES                       *
     H*******************************************************************
     H*  FILES:                                                         *
     H*  FAEOL KEY LIST: CO/SUB#/FND/FYR/CASE/TRDT/FEIN/MOD/EOB#        *
     H*        OPEN QUERY SELECTION:    QQFRDT GE EOTRDT LE QQTODT      *
     H*                                 EOTAPE EQ 'Y', EASUB# EQ QQSUB# *
     H*                                 IF QQFND NE 000, QQFND EQ EOFND *
     H*******************************************************************
     FFATPPTX   O  A F   64        DISK
     FFATHPTX   O  A F   64        DISK
     FSMCOL     IF   E           K DISK
     FFMC1L     IF   E           K DISK
     FFMF1L     IF   E           K DISK
     FFDF2L     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FFAEOL4    IF   E           K DISK
     FFAE1L     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFDCALH    IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFDHCL     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FFMSML     IF   E           K DISK
     FFMDIL     IF   E           K DISK
     FFMP1L     IF   E           K DISK
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOA)
     FFMTSP     UF A E           K DISK

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
     D  SSN            S              9
      ***** End Add *** 06/09/20 *******************************************
     D*
     D                 DS
     D  EOBMED                 1      8
     D  EOB                    1      3
     D  MEDPD                  4      8
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D                 DS
     D  XXDT                   1      8  0
     D  XXMM                   1      2  0
     D  XXDD                   3      4  0
     D  XXYY                   5      8  0
     D*
     D* Year prior to current date's year; used to calc Julian date
     D                 DS
     D  ZZDT                   1      8  0
     D  ZZMM                   1      2  0
     D  ZZDD                   3      4  0
     D  ZZYY                   5      8  0
     D*
     D* Data Structure for Claim Control Number
     D                 DS
     D  CNTL                   1     13
     D  CCC                    1      3
     D  YY                     4      5
     D  JJJ                    6      8
     D  SSSSS                  9     13
     D*
     D* Diagnosis Codes Data Structure
     D                 DS
     D  DIAG                   1     20
     D                                     DIM(20)                              DIAG CODES
     D  EODIA1                 1      5
     D  EODIA2                 6     10
     D  EODIA3                11     15
     D  EODIA4                16     20
     D*
     D* TYPE OF SERVICE Data Structure
     D                 DS
     D  TPSERV                 1     20
     D  TYPE1                  7      9
     D*
     D* FILLER DATA STRUCTURE
     D                 DS                  INZ
     D  FILL24                 1     24
     D  FLB2                   1      2
     D  FLN3                   3      5  0
     D  FLN72                  6     14  2
     D  FLN72A                15     23  2
     D  FLB1                  24     24
     D*
     D*
     D* External Data Structure
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* Entry Parameters
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    QQQUE            10
     C                   PARM                    QQPROC            1
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C* Key List for the Company Master File
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C* Key List for the Company AUXILIARY Master File
     C*
     C     C1KEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C* Key List for the FUND AUXILIARY Master File
     C*
     C     F1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    EOFND
     C*
     C* Key List for the Output Routing Master File
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C* Key List for the EOMB TRANSACTION FILE
     C*
     C     EOKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    E1FND
     C                   KFLD                    E1FYR
     C                   KFLD                    E1CASE
     C                   KFLD                    E1EOB#
     C*
     C     EOKEY         KLIST
     C                   KFLD                    XTCO#             3 0
     C                   KFLD                    XTSUB#            5 0
     C                   KFLD                    XTFND             3 0
     C                   KFLD                    XTFYR             3 0
     C*
     C* Key List for the Fund Master File
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*
     C* Key List for the SUBCRIBER   File
     C*
     C     SMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C*
     C* Key List for the Case Detail Charge File
     C*
     C     CAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C* Key List for the EOMB AUXILIARY File
     C*
     C     E1KY1         KLIST
     C                   KFLD                    XTCO#             3 0
     C                   KFLD                    XTSUB#            5 0
     C                   KFLD                    XTFND             3 0
     C                   KFLD                    XTFYR             3 0
     C*
     C     E1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C* Key List for the Claim Master File
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C*
     C* Key List for the DETAIL CHARGE AUXILIARY FILE
     C*
     C     HCKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    CADSEQ
     C*
     C* Key List for the Claimant Demographics Master File
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C* Key List for the Provider Master File
     C*
     C     PDKEY         KLIST
     C                   KFLD                    EOFEIN
     C                   KFLD                    EOMOD
     C*
     C* Key List for the GROUP STATE INFORMATION FILE
     C*
     C     F2KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    EOFND
     C*
     C* Key List for the TAPE SUBMISSION SEQ# FILE
     C*
     C     TSKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C* Key List for the DIAGNOSIS CODE FILE
     C*
     C     DIKEY         KLIST
     C                   KFLD                    XXDIA             5
     C                   KFLD                    XXLOST            1
     C*
     C* Key List for the PROVIDER AUXILIARY FILE
     C*
     C     P1KEY         KLIST
     C                   KFLD                    EOFEIN
     C                   KFLD                    EOMOD
     C*
     C* Set-up and Initialize
     C*
     C                   Z-ADD     0             COUNT             5 0          ADD 02/18/97
     C                   Z-ADD     0             X                 5 0
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQSUB#        XXSUB#            5 0
     C                   MOVE      QQCO#         XTCO#             3 0
     C                   MOVE      QQSUB#        XTSUB#            5 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFRDT        XQFRDT            8 0
     C                   MOVE      QQTODT        XQTODT            8 0
     C                   Z-ADD     XQFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT            7 0
     C                   Z-ADD     XQTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT            7 0
     C                   Z-ADD     XXFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXFROM            6 0
     C                   Z-ADD     XXTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXTO              6 0
     C                   Z-ADD     0             CNTR              5 0
     C                   TIME                    DSDTTM
     C     QQPROC        IFEQ      'P'
     C                   MOVEL     'PHYSI'       XXPROC            9
     C                   MOVE      'CIAN'        XXPROC
     C                   MOVE      '1'           *IN90
     C                   ELSE
     C                   MOVEL     ' HOSPI'      XXPROC
     C                   MOVE      'TAL'         XXPROC
     C                   MOVE      '0'           *IN90
     C                   END
     C*
     C* Get the Julian Date
     C*
     C                   Z-ADD     DSDATE        XXDT
     C                   Z-ADD     12            ZZMM
     C                   Z-ADD     31            ZZDD
     C     XXYY          IFNE      0
     C     XXYY          SUB       1             ZZYY
     C                   ELSE
     C                   Z-ADD     99            ZZYY
     C                   END
     C*
     C* Call sub-program SRDDFF to Calc Julian Date
     C*
     C                   MOVE      ZZDT          SRFRDC            8 0
     C                   MOVE      XXDT          SRTODC            8 0
     C                   Z-ADD     0             SRDAYS            5 0
     C                   MOVE      ' '           SRERR             1
     C                   MOVE      ' '           SRTERM            1
     C                   CALL      'SRDDIF8'
     C                   PARM                    SRFRDC
     C                   PARM                    SRTODC
     C                   PARM                    SRDAYS
     C                   PARM                    SRERR
     C                   PARM                    SRTERM
     C*
     C*
     C*  GET NEXT AVAILABLE SEQUENCE # TO BE USED IN CONTROL RECORD
     C*
     C     TSKEY         CHAIN     PTS01                              80
     C     *IN80         IFEQ      '1'
     C                   Z-ADD     0             CNTR
     C                   ELSE
     C                   MOVE      TSSEQ#        CNTR                            LAST # USED
     C                   END
     C*
     C*  GET COMPANY NAME
     C*
     C     COKEY         CHAIN     PCO01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       XXCONM           40
     C                   ELSE
     C                   MOVEL     CONAME        XXCONM
     C                   END
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*  GET COMPANY AUXILIARY RECORD FOR AUDIT NUMBERS
     C*  IF NON EXIST - PRINT ERROR MESSAGE AND END PROGRAM
     C*
     C     C1KEY         CHAIN     PC101                              60
     C     *IN60         IFEQ      '1'
     C                   EXCEPT    ERRMS1
     C                   GOTO      TERM
     C                   END
     C*
     C*  GET SUBCRIBER NAME
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       XXSBNM           40
     C                   ELSE
     C                   MOVEL     SMNAME        XXSBNM
     C                   END
     C*
     C* Get the Fund Record
     C     FNKEY         CHAIN     PFN01                              63
     C     *IN63         IFEQ      '1'
     C                   MOVE      *BLANKS       XXFNNM           40
     C                   ELSE
     C                   MOVEL     FNNAME        XXFNNM
     C                   END
     C*
     C* Get the Routing Information and Print Routing Page
     C*
      ***** Beg Del ***** 08/26/14 *********************************************
     C**********         MOVE      '0'           XXPRT
     C**********     ORKEY         CHAIN     POR01                              61
     C**********    *IN61         IFEQ      '0'
     C**********         EXCEPT    ROUTE
     C**********         END
      ***** End Del ***** 08/26/14 *********************************************
     C*
     C                   MOVE      '0'           *IN99
     C*
     C* GET THE # OF TYPE 1A RECORDS IN THE BATCH
     C*
     C                   Z-ADD     *ZEROS        XXTP1A            5 0
     C                   READ      PE101                                  62
     C     *IN62         DOWEQ     '0'
     C*
     C     EOKY1         CHAIN     PEO01                              64
     C     *IN64         IFEQ      '0'
     C*
     C     EOTAPE        IFEQ      'Y'
     C*** BEG ADD 08/30/99 ********************************************
     C     EOAST         ANDEQ     'TX'
     C*** END ADD 08/30/99 ********************************************
     C*
     C     EOSYS         IFEQ      'P'
     C                   MOVE      '1'           *IN89
     C                   ELSE
     C     EOSYS         IFEQ      'H'
     C                   MOVE      '0'           *IN89
     C                   END                                                    EOSYS = H
     C                   END                                                    EOSYS = P
     C*
     C   89
     CAN 90XXTP1A        ADD       1             XXTP1A
     C  N89
     CANN90XXTP1A        ADD       1             XXTP1A
     C                   READ      PEO01                                  64
     C*
     C                   END                                                    EOTAPE = Y
     C                   END                                                    *IN64 = 0
     C*
     C                   READ      PE101                                  62
     C*
     C                   END                                                    DOW*IN62 = 0
     C*
     C                   MOVE      '1'           *IN79
     C                   MOVE      QQCO#         XTCO#
     C                   MOVE      QQSUB#        XTSUB#
     C                   MOVE      *LOVAL        XTFND
     C                   MOVE      *LOVAL        XTFYR
     C*
     C     E1KY1         SETLL     PE101
     C*
     C**  BEGIN READING BATCH
     C*
     C                   READ      PE101                                  62
     C*
     C     *IN62         IFEQ      '1'
     C                   EXCEPT    ERRMS2
     C                   GOTO      TERM
     C                   END                                                    *IN62 = 1
     C*
     C     *IN62         DOWEQ     '0'
     C*
     C     EOKY1         CHAIN     PEO01                              64
     C     *IN64         IFEQ      '0'
     C*
     C     EOTAPE        IFEQ      'Y'
     C*** BEG ADD 08/30/99 ********************************************
     C     EOAST         ANDEQ     'TX'
     C*** END ADD 08/30/99 ********************************************
     C*
     C     EOSYS         IFEQ      'P'
     C                   MOVE      '1'           *IN89
     C                   ELSE
     C     EOSYS         IFEQ      'H'
     C                   MOVE      '0'           *IN89
     C                   END                                                    EOSYS = H
     C                   END                                                    EOSYS = P
     C*
     C     *IN89         IFEQ      *IN90
     C                   MOVE      '1'           *IN99
     C                   END                                                    *IN89=*IN90
     C*
     C* GET THE FUND AUXILIARY RECORD
     C     F1KEY         CHAIN     PF101                              63
     C*
     C* Get the Claim Master Record
     C     CMKEY         CHAIN     PCM01                              63
     C*
     C* Get the Claimant Demographic Record
     C     CDKEY         CHAIN     PCD01                              63
     C*
     C* Get the Provider Master Record
     C     PDKEY         CHAIN     PPD01                              63
     C*
     C* Get the GROUP STATE INFORMATION
     C     F2KEY         CHAIN     PF201                              63
     C*
     C* Get the PROVIDER AUXILARY RECORD
     C     P1KEY         CHAIN     PP101                              63
     C*
     C* GET THE # OF TYPE 2 RECORDS AND FSRMA, OUT & IR TOTALS
     C*
     C                   MOVE      '0'           *IN75
     C                   MOVE      '0'           *IN76
     C                   MOVE      '0'           *IN77
     C                   Z-ADD     *ZEROS        XXTYP2            3 0
     C                   Z-ADD     *ZEROS        XXMRAC            9 2
     C                   Z-ADD     *ZEROS        XXMRAP            9 2
     C*
     C     CAKEY         CHAIN     PCA01                              64
     C     *IN64         DOWEQ     '0'
     C*
     C   89
     CAN 90XXTYP2        ADD       1             XXTYP2
     C  N89
     CANN90CACPT4        IFEQ      'FSMRA'
     C                   ADD       CATOTC        XXMRAC            9 2
     C                   ADD       CATOTP        XXMRAP            9 2
     C                   MOVE      '1'           *IN77
     C                   END                                                    FSMRA
     C  N89
     CANN90CACPT4        IFEQ      'OUT'
     C                   ADD       CATOTC        XXMRAC            9 2
     C                   ADD       CATOTP        XXMRAP            9 2
     C                   MOVE      '1'           *IN75
     C                   END                                                    OUT
     C  N89
     CANN90CACPT4        IFEQ      'IR'
     C                   ADD       CATOTC        XXMRAC            9 2
     C                   ADD       CATOTP        XXMRAP            9 2
     C                   MOVE      '1'           *IN76
     C                   END                                                    IR
     C  N89
     CANN90CACPT4        IFEQ      'MISC'
     C                   ADD       CATOTC        XXMRAC            9 2
     C                   ADD       CATOTP        XXMRAP            9 2
     C                   MOVE      '1'           *IN74
     C                   END                                                    MISC
     C*
     C     CAKEY         READE     PCA01                                  64
     C                   END                                                    DOW*IN64 = 0
     C*
     C********************  HEADER SECTION  **********************************
     C* Clear Header record Fields
     C*
     C                   EXSR      CLRH
     C*
     C* Move information to Header fields
     C*
     C* RECORD TYPE
     C                   MOVEL     '0'           RECTYP
     C* CARRIER CODE
     C*******************  MOVE F292N1    CARCDE           DEL 11/07/95
     C                   MOVE      F29N01        CARCDE                         CHG 11/07/95
     C* LOCATION CODE
     C                   MOVE      C1LOC         LOCCDE
     C* YEAR OF SUBMISSION
     C                   MOVE      XXYY          YRSUB
     C* JULIAN DATE
     C                   MOVE      SRDAYS        JULDT
     C* NUMBER OF TYPE 1A RECORDS
     C                   Z-ADD     XXTP1A        TYPE1A
     C* FILLER
     C                   MOVE      *BLANKS       FILL38           38
     C*
     C* Write the Header Record to the Work File ONCE
     C*
     C   90
     CAN 89
     CAN 79              EXCEPT    HEAD0
     C  N90
     CANN89
     CAN 79              EXCEPT    HED0
     C   90
     CAN 89
     CAN 79              MOVE      '0'           *IN79
     C  N90
     CANN89
     CAN 79              MOVE      '0'           *IN79
     C*
     C*
     C* RECORD TYPE
     C                   MOVEL     '1A'          RECTYP
     C* LAST NAME
     C                   MOVEL     CDLSN         LSNAME           20
     C* FIRST NAME
     C                   MOVEL     CDFSN         FSNAME           12
     C* MIDDLE INITIAL
     C                   MOVEL     CDMID         MID
     C                   MOVEL     MID           MID2              1
     C* SOCIAL SECURITY NUMBER
     C     CMSS#         IFNE      *BLANKS
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVE      CMSS#         SSN               9 0
      /Free
        SSN = MaskData('SSN':'CMSS#':CMSS#);
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C                   ELSE
     C                   MOVEL     '999'         SSN
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXACDT            8 0
     C                   END                                                    CMSS# = BLANK
     C* DATE OF INJURY
     C     CMACDT        IFNE      0
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         INJDT             6 0
     C                   ELSE
     C                   MOVE      XXYY          INJDT
     C                   MOVEL     '0000'        INJDT
     C                   END                                                    CMACDT NE 0
     C* DATE FIRST CONSULTED
     C                   Z-ADD     EODFC         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         CONDT             6 0
     C* ADMISSION DATE
     C                   Z-ADD     E1DT6         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         ADMDT             6 0
     C* SUBMISSION DATE
     C                   Z-ADD     E1DT9         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         SUBDT             6 0
     C* DATE BILL RECEIVED
     C                   Z-ADD     E1CRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         RECDT             6 0
     C*
     C* Write the Header Record to the Work File
     C*
     C   90
     CAN 89              EXCEPT    HEAD1A
     C  N90
     CANN89              EXCEPT    HED1A
     C* RECORD TYPE
     C                   MOVEL     '1B'          RECTYP
     C* DATE BILL RECEIVED
     C                   Z-ADD     E1CRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         RECDT             6 0
     C* DATE OF CARRIER PAYMENT
     C                   Z-ADD     E1ENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         PAYDT             6 0
     C* FILLER
     C                   MOVE      *BLANKS       FILL              1
     C* PRIMARY ICD-9
     C                   MOVE      'D'           XXLOST
     C                   MOVE      EODIA1        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDS1         ICD9              6
     C* SECONDARY ICD-9 1
     C                   MOVE      EODIA2        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDS1         ICD91             6
     C* SECONDARY ICD-9 2
     C                   MOVE      EODIA3        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDS1         ICD92             6
     C* SECONDARY ICD-9 3
     C                   MOVE      EODIA4        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDS1         ICD93             6
     C* PROVIDER LISCENCE NUMBER
     C     EOFLIC        IFEQ      *BLANKS
     C     PDFLIC        IFNE      *BLANKS
     C                   MOVEL     PDFLIC        PRLIC#            8
     C                   ELSE                                                   ELSE NE *BLANKS
     C                   MOVEL     '00000'       PRLIC#
     C                   MOVE      '0000'        PRLIC#
     C                   END                                                    END NE *BLANKS
     C                   ELSE                                                   ELSE EQ *BLANKS
     C                   MOVEL     EOFLIC        PRLIC#
     C                   END                                                    END EQ *BLANKS
     C* NUMBER OF TYPE 2 RECORDS
     C                   Z-ADD     XXTYP2        TYPE2
     C* TAX ID #
     C     EOFEIN        IFNE      0
     C                   MOVE      EOFEIN        TAXID#            9 0
     C                   END                                                    EOFEIN NE 0
     C* SERVICE FROM DATE
     C                   Z-ADD     E1DT7         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         SRVDTF            6 0
     C* SERVICE TO DATE
     C                   Z-ADD     E1DT8         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         SRVDTT            6 0
     C* TOTAL CHARGES
     C                   MOVE      EOAMTC        TTLCHR
     C* ESTIMATED AMOUNT DUE
     C     EOAMT         SUB       EONDIS        ESTDUE
     C* PAYMENT EXCEPTION CODE
     C                   MOVEL     EOEOB3        EXCPT1
     C* CLAIMANT SS#
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVE      CMSS#         SSN
      /Free
       SSN = MaskData('SSN':'CMSS#':CMSS#);
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C* INJURY DATE
     C*                    MOVE CMACDT    INJDT
     C* MEDICARE PROVIDER #
     C                   MOVE      P1MCR#        MEDPRO
     C* TYPE OF SERVICE
     C                   MOVE      E120A2        TPSERV
     C                   MOVE      TYPE1         TYPSRV
     C* FILLER
     C                   MOVE      *BLANKS       FILL3             3
     C*
     C* Write the Header Record to the Work File
     C*
     C   90
     CAN 89              EXCEPT    HEAD1B
     C  N90
     CANN89              EXCEPT    HED1B
     C*
     C* Write the Header Record to the report
     C*
     C     *INOA         IFEQ      '1'
     C     XXFND         ORNE      HLDFND
     C                   EXCEPT    HEADER
     C                   Z-ADD     XXFND         HLDFND            3 0          TO DETERMINE LVL BRK
     C                   END
     C*
     C   90
     CAN 89              EXCEPT    HEADR1
     C  N90
     CANN89              EXCEPT    HEADR1
     C*** BEGIN ADD *** 02/18/97 *********************************************
     C*  COUNTER IS USED TO DETERMIN THE NUMBER OF HEADERS THAT WERE PROCESSED
     C*  IN THE TAPE SUBMISSION.
     C*
     C                   ADD       1             COUNT
     C*** END ADD ***** 02/18/97 *********************************************
     C*
     C********************  DETAIL SECTION  **********************************
     C* Get all Detail Charge Records that are associated with the
     C* EOB Master record
     C*
     C                   MOVE      '0'           *IN65
     C                   MOVEA     '0000000'     *IN(51)
     C                   MOVEA     '00000000'    *IN(91)
     C                   MOVE      '1'           *IN40
     C                   EXSR      CLRX
     C*
     C     CAKEY         CHAIN     PCA01                              64
     C     *IN64         DOWEQ     '0'
     C*
     C* Clear Detail record Fields
     C*
     C* Get the DETAIL CHARGE AUXILARY RECORD
     C     HCKEY         CHAIN     PHC01                              63
     C*
     C     *IN65         IFEQ      '0'                                          OUT BILL
     C                   EXSR      CLRD
     C*
     C*
     C* Move information to Detail fields
     C*
     C* RECORD TYPE
     C                   MOVEL     '2'           RECTYP
     C* SERVICE FROM DATE
     C     CAFDAT        IFEQ      0
     C                   MOVEL     '0000'        SRVDTF            6 0
     C                   MOVE      XXYY          SRVDTF
     C                   ELSE
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         SRVDTF
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT
     C                   END
     C* SERVICE TO DATE
     C     CATDAT        IFEQ      0
     C                   MOVEL     '0000'        SRVDTT
     C                   MOVE      XXYY          SRVDTT
     C                   ELSE
     C                   Z-ADD     CATDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         SRVDTT
     C                   Z-ADD     CATDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT
     C                   END
     C* FEE GUIDELINE CODE
     C                   MOVEL     CACPT4        FEECDE
     C* FIRST MODIFIER
     C     CAPMOD        IFNE      *BLANKS
     C                   MOVE      CAPMOD        MOD1
     C                   END
     C* SECOND MODIFIER (NOT USED AT THIS TIME)
     C                   MOVE      *BLANKS       MOD2              2
     C* FIRST DIAG COND XREF #
     C                   MOVE      CADXR         DIAG1             1
     C* SECOND DIAG COND XREF # (NOT USED AT THIS TIME)
     C                   MOVE      *ZEROS        DIAG2             1
     C* THIRD DIAG COND XREF # (NOT USED AT THIS TIME)
     C                   MOVE      *ZEROS        DIAG3             1
     C* FOURTH DIAG COND XREF # (NOT USED AT THIS TIME)
     C                   MOVE      *ZEROS        DIAG4             1
     C* UNITS
     C     CADXR         IFLE      1999
     C     CADXR         ANDGE     100
     C                   Z-ADD     CAANMN        XXUNIT            3 0
     C                   ELSE
     C                   Z-ADD     CAUNTS        XXUNIT
     C                   END
     C                   MOVE      XXUNIT        UNITS
     C* CHARGE AMOUNT
     C                   MOVE      CATOTC        CHGAMT            9 2
     C* PAYMENT AMOUNT
     C                   MOVE      CATOTP        PAYAMT            9 2
     C* PAYMENT EXCEPTION CODE
     C                   MOVEL     CAEOMB        PAYCDE
     C* PLACE OF SERVICE
     C                   MOVE      HCPOS         PLACE
     C* FILLER
     C                   MOVE      *BLANKS       FILL13           13
     C*
     C* DIAGNOSIS ICD-9 CODES
     C* PRINCIPAL ICD-9
     C                   MOVE      'D'           XXLOST
     C                   MOVE      EODIA1        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDS1         ICD9              6
     C* OTHER DIAGNOSIS
     C                   MOVE      EODIA2        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDS1         ICD91             6
     C* OTHER DIAGNOSIS
     C                   MOVE      EODIA3        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDS1         ICD92             6
     C* OTHER DIAGNOSIS
     C                   MOVE      EODIA4        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDS1         ICD93             6
     C*
     C* ICD-9-CM PROCEDURE CODES
     C* If Procedure Code Method = '9', then use 'O' type Diag Code
     C                   Z-ADD     20            X
     C     1             SUBST     E120A1:X      XXPCM             1    65
     C     XXPCM         IFEQ      '9'
     C*
     C                   MOVE      'O'           XXLOST
     C*
     C* PRIN ICD-9-CM PROC
     C                   MOVEL     E17A01        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDS1         ICD9CM
     C****************     MOVE HC1A1     ICD9CM
     C*
     C* OTHER ICD-9-CM PROC
     C                   MOVEL     E17A02        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDS1         ICD9C1
     C****************     MOVE HC1A2     ICD9C1
     C*
     C* OTHER ICD-9-CM PROC
     C                   MOVEL     E17A03        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDS1         ICD9C2
     C****************     MOVE HC1A3     ICD9C2
     C                   ELSE                                                   ELS XXPCM 9
     C     XXPCM         IFEQ      '4'
     C                   MOVEL     E17A01        ICD9CM
     C                   MOVEL     E17A02        ICD9C1
     C                   MOVEL     E17A03        ICD9C2
     C                   END                                                    END XXPCM 4
     C                   END                                                    END XXPCM 9
     C*
     C* TAX ID #
     C                   MOVE      EOFEIN        TAXID#
     C* FILLER
     C                   MOVE      *BLANKS       FILL6             6
     C*
     C*
     C* Write DETAIL record to Work File
     C*
     C   90
     CAN 89              EXCEPT    DETAIL
     C     CACPT4        IFNE      'FSMRA'
     C  N90
     CANN89
     CAN 40              EXCEPT    DTL
     C   40              MOVE      '0'           *IN40
     C                   END
     C*
     C** IF HOSPITAL/OUT PATIENT THE SOME DETAIL SECTION CAN BE SKIPPED
     C     CACPT4        IFEQ      'OUT  '
     C                   MOVE      '1'           *IN65
     C                   MOVE      '1'           *IN98
     C                   MOVEL     'OUT  '       ITEM              5
     C   90
     CAN 89              EXCEPT    DETL
     C  N90
     CANN89              EXCEPT    DETL
     C                   END
     C     CACPT4        IFEQ      'MISC '
     C                   MOVE      '1'           *IN65
     C                   MOVE      '1'           *IN98
     C                   MOVEL     'MISC '       ITEM              5
     C   90
     CAN 89              EXCEPT    DETL
     C  N90
     CANN89              EXCEPT    DETL
     C                   END
     C     CACPT4        IFEQ      'IR   '
     C                   MOVE      '1'           *IN65
     C                   MOVE      '1'           *IN98
     C                   MOVEL     'IR   '       ITEM              5
     C   90
     CAN 89              EXCEPT    DETL
     C  N90
     CANN89              EXCEPT    DETL
     C                   END
     C** IF HOSPITAL/FSMRA SKIP OUTPUT
     C     CACPT4        IFEQ      'FSMRA'
     C                   GOTO      ENDMRA
     C                   END
     C*
     C* Move information to Detail fields
     C*
     C* RECORD TYPE
     C                   MOVEL     'X1'          RECTP1            2
     C* FILLER
     C                   MOVE      *BLANKS       FILL2             2
     C* ITEMIZED CHARGE #1
     C     CACPT4        IFEQ      'ROOMS'
     C     CACPT4        OREQ      'ROOMM'
     C                   MOVEL     'ROOM'        ITMIZ1            5
     C                   MOVE      '1'           *IN91
     C                   MOVE      '1'           *IN78
     C* UNITS
     C                   Z-ADD     CAUNTS        XXUNIT
     C                   MOVE      XXUNIT        UNITS1            3
     C* CHARGES
     C                   MOVE      CATOTC        CHARG1            9 2
     C   77              ADD       XXMRAC        CHARG1
     C   76              Z-ADD     XXMRAC        CHARG1
     C* AMOUNT PAID
     C                   MOVE      CATOTP        AMTPD1            9 2
     C   77              ADD       XXMRAP        AMTPD1
     C   76              Z-ADD     XXMRAP        AMTPD1
     C   77              MOVE      '0'           *IN77
     C* PAYMENT EXCEPTION CODE
     C                   MOVEL     CAEOMB        EXC1              1
     C*
     C                   END                                                    ROOMM OR ROOMS
     C* FILLER
     C**********           MOVE *BLANKS   FILL24 24        DEL 10/06/97
     C*
     C* ITEMIZED CHARGE #2
     C     CACPT4        IFEQ      '00001'
     C                   MOVE      '00001'       ITMIZ2            5
     C                   MOVE      '1'           *IN92
     C                   MOVE      '1'           *IN78
     C* UNITS
     C                   Z-ADD     CAUNTS        XXUNIT
     C                   MOVE      XXUNIT        UNITS2            3
     C* CHARGES
     C                   MOVE      CATOTC        CHARG2            9 2
     C   77              ADD       XXMRAC        CHARG2
     C   76              Z-ADD     XXMRAC        CHARG2
     C* AMOUNT PAID
     C                   MOVE      CATOTP        AMTPD2            9 2
     C   77              ADD       XXMRAP        AMTPD2
     C   76              Z-ADD     XXMRAP        AMTPD2
     C   77              MOVE      '0'           *IN77
     C* PAYMENT EXCEPTION CODE
     C                   MOVEL     CAEOMB        EXC2              1
     C*
     C                   END                                                    00001
     C* FILLER
     C                   MOVE      *BLANKS       FILL4             4
     C* FILLER
     C                   MOVE      *BLANKS       FILL28           28
     C*
     C* Write Detail record to REPORT
     C*
     C  N90
     CANN89
     CAN 78              EXCEPT    DETL
     C*
     C* Move information to Detail fields
     C*
     C* RECORD TYPE
     C                   MOVEL     'X2'          RECTP2            2
     C                   MOVEL     'X3'          RECTP3            2
     C                   MOVEL     'X4'          RECTP4            2
     C* ITEMIZED CHARGE #3
     C     CACPT4        IFEQ      '00002'
     C                   MOVEL     '00002'       ITMIZ3            5
     C                   MOVE      '1'           *IN93
     C                   MOVE      '1'           *IN78
     C* UNITS
     C                   Z-ADD     CAUNTS        XXUNIT
     C                   MOVE      XXUNIT        UNITS3            3
     C* CHARGES
     C                   MOVE      CATOTC        CHARG3            9 2
     C* AMOUNT PAID
     C                   MOVE      CATOTP        AMTPD3            9 2
     C* PAYMENT EXCEPTION CODE
     C                   MOVEL     CAEOMB        EXC3              1
     C*
     C                   END                                                    00002
     C* FILLER
     C**********           MOVE *BLANKS   FILL24 24        DEL 10/06/97
     C*
     C* ITEMIZED CHARGE #4, #5, #6 & #7
     C     CACPT4        IFGE      '70450'
     C     CACPT4        ANDLE     '70492'
     C     CACPT4        ORGE      '71250'
     C     CACPT4        ANDLE     '71270'
     C     CACPT4        ORGE      '72125'
     C     CACPT4        ANDLE     '72133'
     C     CACPT4        ORGE      '72192'
     C     CACPT4        ANDLE     '72194'
     C     CACPT4        ORGE      '73200'
     C     CACPT4        ANDLE     '73202'
     C     CACPT4        ORGE      '73700'
     C     CACPT4        ANDLE     '73701'
     C     CACPT4        ORGE      '74150'
     C     CACPT4        ANDLE     '74170'
     C     CACPT4        OREQ      '70336'
     C     CACPT4        ORGE      '70540'
     C     CACPT4        ANDLE     '70552'
     C     CACPT4        OREQ      '71550'
     C     CACPT4        ORGE      '72141'
     C     CACPT4        ANDLE     '72149'
     C     CACPT4        OREQ      '72196'
     C     CACPT4        ORGE      '73220'
     C     CACPT4        ANDLE     '73221'
     C     CACPT4        ORGE      '73720'
     C     CACPT4        ANDLE     '73721'
     C     CACPT4        OREQ      '74181'
     C     CACPT4        OREQ      '75552'
     C     CACPT4        OREQ      '76400'
     C  N94              MOVE      CACPT4        ITMIZ4            5
     C   94
     CANN95              MOVE      CACPT4        ITMIZ5            5
     C   94
     CAN 95
     CANN96              MOVE      CACPT4        ITMIZ6            5
     C   94
     CAN 95
     CAN 96              MOVE      CACPT4        ITMIZ7            5
     C* CPT-4 MODIFIER
     C  N94              MOVE      CAPMOD        CPT44             2
     C   94
     CANN95              MOVE      CAPMOD        CPT45             2
     C   94
     CAN 95
     CANN96              MOVE      CAPMOD        CPT46             2
     C   94
     CAN 95
     CAN 96              MOVE      CAPMOD        CPT47             2
     C* UNITS
     C                   Z-ADD     CAUNTS        XXUNIT
     C  N94              MOVE      XXUNIT        UNITS4            3
     C   94
     CANN95              MOVE      XXUNIT        UNITS5            3
     C   94
     CAN 95
     CANN96              MOVE      XXUNIT        UNITS6            3
     C   94
     CAN 95
     CAN 96              MOVE      XXUNIT        UNITS7            3
     C* CHARGES
     C  N94              MOVE      CATOTC        CHARG4            9 2
     C   94
     CANN95              MOVE      CATOTC        CHARG5            9 2
     C   94
     CAN 95
     CANN96              MOVE      CATOTC        CHARG6            9 2
     C   94
     CAN 95
     CAN 96              MOVE      CATOTC        CHARG7            9 2
     C* AMOUNT PAID
     C  N94              MOVE      CATOTP        AMTPD4            9 2
     C   94
     CANN95              MOVE      CATOTP        AMTPD5            9 2
     C   94
     CAN 95
     CANN96              MOVE      CATOTP        AMTPD6            9 2
     C   94
     CAN 95
     CAN 96              MOVE      CATOTP        AMTPD7            9 2
     C* PAYMENT EXCEPTION CODE
     C  N94              MOVEL     CAEOMB        EXC4              1
     C   94
     CANN95              MOVEL     CAEOMB        EXC5              1
     C   94
     CAN 95
     CANN96              MOVEL     CAEOMB        EXC6              1
     C   94
     CAN 95
     CAN 96              MOVEL     CAEOMB        EXC7              1
     C*
     C   94
     CAN 95
     CAN 96              MOVE      '1'           *IN97
     C   94
     CAN 95
     CANN96              MOVE      '1'           *IN96
     C   94
     CANN95              MOVE      '1'           *IN95
     C  N94              MOVE      '1'           *IN94
     C*
     C                   MOVE      '1'           *IN78
     C                   END
     C* FILLER
     C                   MOVE      *BLANKS       FILL4             4
     C* FILLER
     C                   MOVE      *BLANKS       FILL28           28
     C* FILLER
     C                   MOVE      *BLANKS       FILL33           33
     C*
     C* Write Detail record to REPORT
     C*
     C     *INOA         IFEQ      '1'
     C   90
     CAN 89              EXCEPT    HEADER
     C  N90
     CANN89              EXCEPT    HEADR1
     C                   MOVE      '0'           *INOA
     C                   END
     C   90
     CAN 89              EXCEPT    DETL
     C  N90
     CANN89
     CAN 78              EXCEPT    DETL
     C*
     C* Continue to read  detail charge file
     C*
     C     ENDMRA        TAG
     C                   END                                                    OUT BILL
     C*
     C                   MOVE      '0'           *IN78
     C*
     C     CAKEY         READE     PCA01                                  64
     C                   END                                                    *in64 end
     C*
     C* Write Detail record to WORK FILE
     C*
     C  N89              EXCEPT    DTLX1
     C  N89              EXCEPT    DTLX2
     C  N89              EXCEPT    DTLX3
     C  N89              EXCEPT    DTLX4
     C*
     C                   END                                                    EOTAPE = Y
     C                   END                                                    *in64 end
     C*
     C                   READ      PE101                                  62
     C                   END                                                    *in62 end
     C*
     C     *IN99         IFNE      '1'
     C                   EXCEPT    ERRMS2
     C                   GOTO      TERM
     C                   END
     C*
     C* UPDATE TAPE SUBMISSION SEQ# FILE WITH LAST SEQ# USED BY CO/SUB
     C                   MOVE      XXCO#         TSCO#
     C                   MOVE      CNTR          TSSEQ#                         LAST SEQ# USED
     C     *IN80         IFEQ      '1'
     C                   WRITE     PTS01
     C                   ELSE                                                   ELSE 1 *IN80
     C                   UPDATE    PTS01
     C                   END                                                    END 1 *IN80
     C*
     C*** BEGIN ADD *** 02/18/97 ****************************************
     C*  WRITES OUT TOTAL NUMBER OF EOMBS PER SUBSCRIBER
     C                   EXCEPT    CNT
     C*** END ADD ***** 02/18/97 ****************************************
     C* END OF PROGRAM
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxprt = '0';
       Chain Orkey Por01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*
     C****************************************************************
     C* CLRH - Subroutine to Clear Batch Control & Header Record Fields
     C****************************************************************
     C* FIELD DEFINITION:                                             *
     C* BATCH CONTROL RECORD:                                         *
     C* RECTYP   RECORD TYPE = 0                                      *
     C* CARCDE   CARRIER CODE                                         *
     C* LOCCDE   LOCATION CODE                                        *
     C* YRSUB    YEAR OF SUBMISSION (W/CENTURY)                       *
     C* JULDT    JULIAN DATE (DATE TAPE WAS CREATED)                  *
     C* TYPE1A   ACCUMULATION OF TYPE 1A RECORDS                      *
     C* --PHYSICIAN--                                                 *
     C* FORM#    FORM NUMBER = 01                                     *
     C* --HOSPITAL--                                                  *
     C* FORM#    FORM NUMBER = 02                                     *
     C*                                                               *
     C* TYPE 1A - DOCUMENT HEADER RECORD:                             *
     C* RECTYP   RECORD TYPE = 1A                                     *
     C* LSNAME   LAST NAME                                            *
     C* FSNAME   FIRST NAME                                           *
     C* MID      MIDDLE INITIAL                                       *
     C*                                                               *
     C* --PHYSICIAN--                                                 *
     C* SSN      SOCIAL SECURITY NUMBER                               *
     C* INJDT    DATE OF INJURY                                       *
     C* CONDT    DATE FIRST CONSULTED                                 *
     C*                                                               *
     C* --HOSPITAL--                                                  *
     C* ADMDT    ADMISSION DATE                                       *
     C* SUBDT    SUBMISSION DATE                                      *
     C* RECDT    RECIEVED DATE                                        *
     C*                                                               *
     C* TYPE 1B - DOCUMENT HEADER RECORD:                             *
     C* RECTYP   RECORD TYPE = 1B                                     *
     C*                                                               *
     C* --PHYSICIAN--                                                 *
     C* RECDT    DATE BILL RECIEVED                                   *
     C* CARPMT   DATE OF CARRIER PAYMENT                              *
     C* ICD9     PRIMARY ICD-9                                        *
     C* ICD91    SECONDARY ICD-9 1                                    *
     C* ICD92    SECONDARY ICD-9 2                                    *
     C* ICD93    SECONDARY ICD-9 3                                    *
     C* PRLIC#   PROVIDER LISCENCE NUMBER                             *
     C* TYPE2    ACCUMULATION OF TYPE 2 RECORDS                       *
     C* TAXID#   TAX ID NUMBER                                        *
     C*                                                               *
     C* --HOSPITAL--                                                  *
     C* SRVDTF   SERVICE FROM DATE                                    *
     C* SRVDTT   SERVICE TO DATE                                      *
     C* TTLCHR   TOTAL CHARGES                                        *
     C* ESTDUE   ESTIMATED AMOUNT DUE                                 *
     C* EXCPT1   PAYMENT EXCEPTION CODE                               *
     C* SSN      CLAIMANT SS#                                         *
     C* INJDT    INJURY DATE                                          *
     C* MEDPRO   MEDICARE PROVIDER #                                  *
     C* TYPSER   TYPE OF SERVICE                                      *
     C****************************************************************
     C     CLRH          BEGSR
     C*
     C                   MOVE      '0'           RECTYP            2
     C   90              MOVE      '01'          FORM#             2
     C  N90              MOVE      '02'          FORM#             2
     C                   MOVEL     '00000'       CARCDE            9
     C                   MOVE      '0000'        CARCDE
     C                   MOVE      '00'          LOCCDE            2
     C                   MOVEL     '0000'        YRSUB             4 0
     C                   MOVE      '000'         JULDT             3 0
     C                   MOVE      *ZEROS        TYPE1A            5 0
     C                   MOVE      *BLANKS       LSNAME           20
     C                   MOVE      *BLANKS       FSNAME           12
     C                   MOVE      *BLANKS       MID               9
     C                   MOVE      *BLANKS       SSN
     C                   MOVE      *ZEROS        INJDT
     C                   MOVE      *ZEROS        CONDT
     C                   MOVE      *ZEROS        RECDT
     C*************        MOVE *ZEROS    CARPAY
     C                   MOVE      '000000'      ICD9
     C                   MOVE      *BLANKS       ICD91
     C                   MOVE      *BLANKS       ICD92
     C                   MOVE      *BLANKS       ICD93
     C                   MOVE      *BLANKS       PRLIC#
     C                   MOVE      *ZEROS        TYPE2             3 0
     C                   MOVE      *BLANKS       TAXID#
     C                   MOVE      *ZEROS        ADMDT
     C                   MOVE      *ZEROS        SUBDT
     C                   MOVE      *ZEROS        SRVDTF
     C                   MOVE      *ZEROS        SRVDTT
     C                   MOVE      *ZEROS        TTLCHR            9 2
     C                   MOVE      *ZEROS        ESTDUE            9 2
     C                   MOVE      *BLANKS       EXCPT1            1
     C                   MOVE      *BLANKS       MEDPRO           10
     C                   MOVE      *BLANKS       TYPSRV            3
     C*
     C                   ENDSR
     C****************************************************************
     C* CLRD - Subroutine to Clear Detail Record's Fields            *
     C****************************************************************
     C* FIELD DEFINITION:                                             *
     C* DETAIL RECORD:                                                *
     C* RECTYP   RECORD TYPE = 2                                      *
     C*                                                               *
     C* --PHYSICIAN--                                                 *
     C* SRVDTF   SERVICE FROM DATE                                    *
     C* SRVDTT   SERVICE TO DATE                                      *
     C* FEECDE   FEE GUIDELINE CODE                                   *
     C* MOD1     FIRST MODIFIER                                       *
     C* MOD2     SECOND MODIFIER (NOT USED AT THIS TIME)              *
     C* DIAG1    FIRST DIAGNOSTIC CODE XREF#                          *
     C* DIAG2    SECOND DIAGNOSTIC CODE (NOT USED AT THIS TIME)       *
     C* DIAG3    THIRD DIAGNOSTIC CODE (NOT USED AT THIS TIME)        *
     C* DIAG4    FOURTH DIAGNOSTIC CODE (NOT USED AT THIS TIME)       *
     C* UNITS    UNITS                                                *
     C* CHGAMT   CHARGE AMOUNT                                        *
     C* PAYAMT   PAYMENT AMOUNT                                       *
     C* PAYCDE   PAYMENT EXCEPTION CODE                               *
     C* POS      PLACE OF SERVICE                                     *
     C*                                                               *
     C* --HOSPITAL--                                                  *
     C* ICD9     PRINCIPAL ICD-9                                      *
     C* ICD91    OTHER DIAGNOSIS                                      *
     C* ICD92    OTHER DIAGNOSIS                                      *
     C* ICD93    OTHER DIAGNOSIS                                      *
     C* OTHDIA   OTHER DIAGNOSIS (NOT CAPTURING THIS DATA)            *
     C* ICD9CM   PRIN ICD-9-CM PROC                                   *
     C* ICD9C1   OTHER ICD-9-CM PROC                                  *
     C* ICD9C2   OTHER ICD-9-CM PROC                                  *
     C* TAXID#   TAX ID #                                             *
     C****************************************************************
     C     CLRD          BEGSR
     C*
     C                   MOVE      '2'           RCFLG2            1
     C                   MOVE      '09'          FORMID            2
     C                   MOVE      *ZEROS        SRVDTF
     C                   MOVE      *ZEROS        SRVDTT
     C                   MOVE      '00000'       FEECDE            5
     C                   MOVE      *BLANKS       MOD1              2
     C                   MOVE      '0'           DIAG1             1
     C                   MOVE      '000'         UNITS             3
     C                   MOVEL     '0000000'     CRGAMT            9
     C                   MOVEL     '00'          CRGAMT
     C                   MOVEL     '000000'      PAYAMT
     C                   MOVE      '000'         PAYAMT
     C                   MOVE      '0'           PAYCDE            1
     C                   MOVEL     '00'          PLACE             2
     C                   MOVE      *BLANKS       ICD9              6
     C                   MOVE      *BLANKS       ICD91             6
     C                   MOVE      *BLANKS       ICD92             6
     C                   MOVE      *BLANKS       ICD93             6
     C                   MOVE      *BLANKS       OTHDIA            6
     C                   MOVE      *BLANKS       ICD9CM            6
     C                   MOVE      *BLANKS       ICD9CM            6
     C                   MOVE      *BLANKS       ICD9C1            6
     C                   MOVE      *BLANKS       ICD9C2            6
     C                   MOVE      *ZEROS        TAXID#
     C*
     C                   ENDSR
     C****************************************************************
     C* CLRX - Subroutine to Clear X(1,2,3,4) Fields                 *
     C****************************************************************
     C*
     C     CLRX          BEGSR
     C*
     C                   MOVE      *BLANKS       ITMIZ1
     C                   MOVE      *BLANKS       ITMIZ2
     C                   MOVE      *BLANKS       ITMIZ3
     C                   MOVE      *BLANKS       ITMIZ4
     C                   MOVE      *BLANKS       ITMIZ5
     C                   MOVE      *BLANKS       ITMIZ6
     C                   MOVE      *BLANKS       ITMIZ7
     C                   MOVE      '000'         UNITS1
     C                   MOVE      '000'         UNITS2
     C                   MOVE      '000'         UNITS3
     C                   MOVE      '000'         UNITS4
     C                   MOVE      '000'         UNITS5
     C                   MOVE      '000'         UNITS6
     C                   MOVE      '000'         UNITS7
     C                   MOVE      *ZEROS        CHARG1
     C                   MOVE      *ZEROS        CHARG2
     C                   MOVE      *ZEROS        CHARG3
     C                   MOVE      *ZEROS        CHARG4
     C                   MOVE      *ZEROS        CHARG5
     C                   MOVE      *ZEROS        CHARG6
     C                   MOVE      *ZEROS        CHARG7
     C                   MOVE      *ZEROS        AMTPD1
     C                   MOVE      *ZEROS        AMTPD2
     C                   MOVE      *ZEROS        AMTPD3
     C                   MOVE      *ZEROS        AMTPD4
     C                   MOVE      *ZEROS        AMTPD5
     C                   MOVE      *ZEROS        AMTPD6
     C                   MOVE      *ZEROS        AMTPD7
     C                   MOVE      *BLANKS       EXC1
     C                   MOVE      *BLANKS       EXC2
     C                   MOVE      *BLANKS       EXC3
     C                   MOVE      *BLANKS       EXC4
     C                   MOVE      *BLANKS       EXC5
     C                   MOVE      *BLANKS       EXC6
     C                   MOVE      *BLANKS       EXC7
     C                   MOVE      *BLANKS       CPT44
     C                   MOVE      *BLANKS       CPT45
     C                   MOVE      *BLANKS       CPT46
     C                   MOVE      *BLANKS       CPT47
     C*
     C                   ENDSR
     C****************************************************************
     OFATPPTX   EADD         HEAD0
     O                       RECTYP               2
     O                       FORM#                3
     O                       CARCDE              12
     O                       LOCCDE              14
     O                       YRSUB               18
     O                       JULDT               21
     O                       TYPE1A              26
     O                       FILL38              64
     O          EADD         HEAD1A
     O                       RECTYP               2
     O                       LSNAME              22
     O                       FSNAME              34
     O                       MID                 41
     O                       SSN                 52
     O                       INJDT               58
     O                       CONDT               64
     O          EADD         HEAD1B
     O                       RECTYP               2
     O                       RECDT                8
     O                       PAYDT               14
     O                       FILL                15
     O                       ICD9                21
     O                       ICD91               27
     O                       ICD92               33
     O                       ICD93               39
     O                       PRLIC#              47
     O                       TYPE2               50
     O                       TAXID#              59
     O          EADD         DETAIL
     O                       RECTYP               2
     O                       SRVDTF               7
     O                       SRVDTT              13
     O                       FILL                14
     O                       FEECDE              19
     O                       MOD1                21
     O                       MOD2                23
     O                       DIAG1               24
     O                       DIAG2               25
     O                       DIAG3               26
     O                       DIAG4               27
     O                       UNITS               30
     O                       CHGAMT              39
     O                       PAYAMT              48
     O                       PAYCDE              49
     O                       PLACE               51
     O                       FILL13              64
     OFATHPTX   EADD         HED0
     O                       RECTYP               2
     O                       FORM#                3
     O                       CARCDE              12
     O                       LOCCDE              14
     O                       YRSUB               18
     O                       JULDT               21
     O                       TYPE1A              26
     O                       FILL38              64
     O          EADD         HED1A
     O                       RECTYP               2
     O                       LSNAME              22
     O                       FSNAME              34
     O                       MID                 43
     O                       PAYDT               46
     O                       ADMDT               52
     O                       SUBDT               58
     O                       RECDT               64
     O          EADD         HED1B
     O                       RECTYP               2
     O                       SRVDTF               8
     O                       SRVDTT              14
     O                       TTLCHR              23
     O                       ESTDUE              32
     O                       EXCPT1              33
     O                       SSN                 42
     O                       INJDT               48
     O                       MEDPRO              58
     O                       TYPSRV              61
     O                       FILL3               64
     O          EADD         DTL
     O                       RECTYP               2
     O                       ICD9                 7
     O                       ICD91               13
     O                       ICD92               19
     O                       ICD93               25
     O                       OTHDIA              31
     O                       ICD9CM              37
     O                       ICD9C1              43
     O                       ICD9C2              49
     O                       TAXID#              58
     O          EADD         DTLX1
     O                       RECTP1               2
     O                       ITMIZ1               7
     O                  91   FILL2                9
     O                  91   UNITS1              12
     O                  91   CHARG1              21
     O                  91   AMTPD1              30
     O                  91   EXC1                31
     O                 N91   FILL24              31
     O                       ITMIZ2              36
     O                  92   FILL2               38
     O                  92   UNITS2              41
     O                  92   CHARG2              50
     O                  92   AMTPD2              59
     O                  92   EXC2                60
     O                  92   FILL4               64
     O                 N92   FILL28              64
     O          EADD         DTLX2
     O                       RECTP2               2
     O                       ITMIZ3               7
     O                  93   FILL2                9
     O                  93   UNITS3              12
     O                  93   CHARG3              21
     O                  93   AMTPD3              30
     O                  93   EXC3                31
     O                 N93   FILL24              31
     O                       ITMIZ4              36
     O                  94   CPT44               38
     O                  94   UNITS4              41
     O                  94   CHARG4              50
     O                  94   AMTPD4              59
     O                  94   EXC4                60
     O                  94   FILL4               64
     O                 N94   FILL28              64
     O          EADD         DTLX3
     O                       RECTP3               2
     O                       ITMIZ5               7
     O                  95   CPT45                9
     O                  95   UNITS5              12
     O                  95   CHARG5              21
     O                  95   AMTPD6              30
     O                  95   EXC5                31
     O                 N95   FILL24              31
     O                       ITMIZ6              36
     O                  96   CPT46               38
     O                  96   UNITS6              41
     O                  96   CHARG6              50
     O                  96   AMTPD6              59
     O                  96   EXC6                60
     O                  96   FILL4               64
     O                 N96   FILL28              64
     O          EADD         DTLX4
     O                       RECTP4               2
     O                       ITMIZ7               7
     O                  97   CPT47                9
     O                  97   UNITS7              12
     O                  97   CHARG7              21
     O                  97   AMTPD7              30
     O                  97   EXC7                31
     O                 N97   FILL24              31
     O                       FILL33              64
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********             XXFNNM              60
     O**********E            ROUTE          1
     O**********             ORDIS1              60
     O**********E            ROUTE          1
     O**********             ORDIS2              60
     O**********E            ROUTE          1
     O**********             ORDIS3              60
     O**********E            ROUTE          2
     O**********             ORDIS4              60
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       XXFNNM              60
     O          E            ROUTE          1
     O                       ORDIS1              60
     O          E            ROUTE          1
     O                       ORDIS2              60
     O          E            ROUTE          1
     O                       ORDIS3              60
     O          E            ROUTE          2
     O                       ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O***********            ORHOLD              61
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *********************************************
     O****************
     O*  PROMPT SELECTION
     O****************
     O          E            ROUTE          1 34
     O                                           18 'COMPANY  :'
     O                       XXCO#         Z     25
     O          E            ROUTE          1
     O                                           18 'SUBSCRIBER:'
     O                       XXSUB#        Z     27
     O          E            ROUTE          1
     O                                           18 'GROUP    :'
     O                       XXFND         Z     25
     O          E            ROUTE          1
     O                                           18 'FROM DATE:'
     O                       XXFROM              30 '  /  /  '
     O          E            ROUTE          1
     O                                           18 'TO DATE  :'
     O                       XXTO                30 '  /  /  '
     O****************
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O***                                   '(C) COPYRIGHT PROGRESSIV'
     O***                              +  0 'E DATA SOLUTIONS, INC.'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEADER         1 06
     O                       DSTIME              08 '  :  :  '
     O                       XXCONM             120
     O                       DSDATE        Y    198
     O          E            HEADER         1
     O                       Q1PGM               10
     O                       ORTITL             129
     O                                          192 'PAGE:'
     O                       PAGE          Z    198
     O          E            HEADER         1
     O                                           88 'FROM DATE:'
     O                       XXFROM              97 '  /  /  '
     O                                          108 'TO DATE:'
     O                       XXTO               117 '  /  /  '
     O          E            HEADER         1
     O                       XXPROC             102
     O          E            HEADER         1
     O                                           11 'SUBSCRIBER:'
     O                       XXSUB#        Z     18
     O                       XXSBNM              60
     O          E            HEADER         2
     O                                           06 'GROUP:'
     O                       XXFND         Z     10
     O                                           12 '-'
     O                       XXFNNM              51
     O          E            HEADER         1
     O               90                          10 'CARRIER'
     O               90                          22 'LOCATION'
     O               90                          29 'YEAR'
     O               90                          35 'DAY'
     O               90                          45 'TYPE 1A'
     O               90                          61 'CLAIMANT NAME'
     O               90                          95 'SOC SEC #'
     O               90                         109 'INJURY DATE'
     O               90                         121 'CONSULTED'
     O               90                         132 'REC DATE'
     O               90                         143 'PAY DATE'
     O               90                         165 'ICD-9 CODES'
     O               90                         185 'LICENCE #'
     O               90                         193 'TYPE 2'
     O              N90                          12 'CARRIER'
     O              N90                          23 'LOCATION'
     O              N90                          29 'YEAR'
     O              N90                          34 'DAY'
     O              N90                          43 'TYPE 1A'
     O              N90                          57 'CLAIMANT NAME'
     O              N90                          89 'PAY DATE'
     O              N90                          99 'SUB DATE'
     O              N90                         109 'REC DATE'
     O              N90                         120 'FROM DATE'
     O              N90                         129 'TO DATE'
     O              N90                         145 'TOTAL CHARGES'
     O              N90                         160 'ESTIMATED DUE'
     O              N90                         170 'PAY CODE'
     O              N90                         181 'SOC SEC #'
     O              N90                         194 'INJURY DATE'
     O          E            HEADER         2
     O               90                          51 'FROM DATE'
     O               90                          61 'TO DATE'
     O               90                          73 'FEE CODE'
     O               90                          84 'MODIFIER'
     O               90                          92 'DIAG1'
     O               90                         100 'UNITS'
     O               90                         116 'CHARGE AMOUNT'
     O               90                         133 'PAYMENT AMOUNT'
     O               90                         144 'PAY CODE'
     O               90                         154 'SERVICE'
     O              N90                          38 'PROVIDER #'
     O              N90                          47 'SERVICE'
     O              N90                          70 'DIAGNOSIS CODES'
     O              N90                          95 'ICD-9-CM CODES'
     O              N90                         108 'TAX ID#'
     O              N90                         118 'ITEMIZE'
     O              N90                         124 'CPT4'
     O              N90                         131 'UNITS'
     O              N90                         141 'CHARGE'
     O              N90                         156 'AMOUNT PAID'
     O              N90                         167 'EXCEPT'
     O          E            HEADR1         1
     O               90      CARCDE              11
     O               90      LOCCDE              19
     O               90      YRSUB               29
     O               90      JULDT               35
     O               90      TYPE1A              44
     O               90      FSNAME              60
     O               90      MID2                62
     O               90      LSNAME              83
     O               90      SSN                 95
     O               90      INJDT              107 '  /  /  '
     O               90      CONDT              120 '  /  /  '
     O               90      RECDT              132 '  /  /  '
     O               90      PAYDT              143 '  /  /  '
     O               90      ICD9               152
     O               90      ICD91              159
     O               90      ICD92              166
     O               90      ICD93              173
     O               90      PRLIC#             184
     O               90      TYPE2              192
     O              N90      CARCDE              13
     O              N90      LOCCDE              20
     O              N90      YRSUB               29
     O              N90      JULDT               34
     O              N90      TYPE1A              42
     O              N90      FSNAME              57
     O              N90      MID2                58
     O              N90      LSNAME              79
     O              N90      PAYDT               89 '  /  /  '
     O              N90      SUBDT               99 '  /  /  '
     O              N90      RECDT              109 '  /  /  '
     O              N90      SRVDTF             119 '  /  /  '
     O              N90      SRVDTT             130 '  /  /  '
     O              N90      TTLCHR        J    143
     O              N90      ESTDUE        J    158
     O              N90      PAYCDE             166
     O              N90      SSN                181
     O              N90      INJDT              192 '  /  /  '
     O          E            DETL           1
     O               90      SRVDTF              50 '  /  /  '
     O               90      SRVDTT              62 '  /  /  '
     O               90      FEECDE              72
     O               90      MOD1                81
     O               90      DIAG1               90
     O               90      UNITS               99
     O               90      CHGAMT        J    114
     O               90      PAYAMT        J    131
     O               90      PAYCDE             140
     O               90      PLACE              151
     O              N90      MEDPRO              38
     O              N90      TYPE1               45
     O              N90      ICD9                55
     O              N90      ICD91               62
     O              N90      ICD92               69
     O              N90      ICD93               76
     O              N90      ICD9CM              84
     O              N90      ICD9C1              91
     O              N90      ICD9C2              98
     O              N90      TAXID#             109
     O              N90 91   ITMIZ1             117
     O              N90 92   ITMIZ2             117
     O              N90 93   ITMIZ3             117
     O              N90 94   ITMIZ4             117
     O              N90 95   ITMIZ5             117
     O              N90 96   ITMIZ6             117
     O              N90 97   ITMIZ7             117
     O              N90 98   ITEM               117
     O              N90 94   CPT44              123
     O              N90 95   CPT45              123
     O              N90 96   CPT46              123
     O              N90 97   CPT47              123
     O              N90 91   UNITS1             130
     O              N90 92   UNITS2             130
     O              N90 93   UNITS3             130
     O              N90 94   UNITS4             130
     O              N90 95   UNITS5             130
     O              N90 96   UNITS6             130
     O              N90 97   UNITS7             130
     O              N90 91   CHARG1        J    143
     O              N90 92   CHARG2        J    143
     O              N90 93   CHARG3        J    143
     O              N90 94   CHARG4        J    143
     O              N90 95   CHARG5        J    143
     O              N90 96   CHARG6        J    143
     O              N90 97   CHARG7        J    143
     O              N90 91   AMTPD1        J    156
     O              N90 92   AMTPD2        J    156
     O              N90 93   AMTPD3        J    156
     O              N90 94   AMTPD4        J    156
     O              N90 95   AMTPD5        J    156
     O              N90 96   AMTPD6        J    156
     O              N90 97   AMTPD7        J    156
     O              N90 91   EXC1               163
     O              N90 92   EXC2               163
     O              N90 93   EXC3               163
     O              N90 94   EXC4               163
     O              N90 95   EXC5               163
     O              N90 96   EXC6               163
     O              N90 97   EXC7               163
     O*** BEGIN ADD *** 02/18/97 ***************************************
     O          E            CNT         2
     O                                           25 'TOTAL NUMBER OF RECORDS'
     O                                           35 'PER FUND:'
     O                       COUNT               49
     O*** BEGIN ADD *** 02/18/97 ***************************************
     O          E            ERRMS1      3  1
     O                                           33 '*** THERE IS NO COMPANY'
     O                                           54 'AUXILIARY RECORD FOR'
     O                                           67 'THIS COMPANY'
     O          E            ERRMS1         1
     O                                           29 'ENTER ONE, THEN'
     O                                           50 'RE-RUN THIS JOB. ***'
     O          E            ERRMS2      3  1
     O                                           33 '*** THERE ARE NO EOMB R'
     O                                           53 'ECORDS FOR THIS FUND'
     O                                           75 ' WITHIN SELECTED DATES'
