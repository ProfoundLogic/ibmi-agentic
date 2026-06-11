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
     H*    PROGRAM: F9511R                                            *
     H*    DESCRIPTION: CREATE WORKFILE AND REPORT                    *
     H*                                                               *
     H*    CALLED FROM:                                               *
     H*      F9511C - CREATE WORKFILE                                 *
     H*    CALLS TO:                                                  *
     H*      NONE                                                     *
     H*****************************************************************
     H** REVISIONS:
     H*
     H* 06/09/20 C:295 Project: SY420                                 *
     H*                -Mask SSN                                      *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  01/21/00 C:117 Project: MC010  Seq#: 21136
     H*                 Populated "To Date of Service" rather than
     H*                 filling w/ zeros.
     H*
     H*  12/29/99 C:117 Project: MC010  Seq#: 21136
     H*                 Problems with the Billed Code; going to check
     H*                 for blanks in the field.
     H*
     H*  12/03/99 C:117 Project: Y2K
     H*                 Had to change some date formats to show
     H*                 all 8 characters rather than 6.
     H*
     H*  09/02/99 C:79  Support: 19312
     H*                 Add condition to select only Texas bills
     H*                 EOAST='TX'
     H*
     H*  08/24/99 C:117 Project: FLMNT
     H*                 Sup Seq: 19357
     H*                 Chained out to FDCAP to check for detail recs
     H*                 before doing anything with the control number.
     H*                 If there are no details, then it doesn't get
     H*                 a CNTL# & that record is skipped.
     H*
     H*  06/12/99 C:117 Project: FL021 FL Tape Submission
     H*                 Added update of FAEOP; update fields added to
     H*                 FAEOP: EOCNTL & EOSBDT; mapped "Billed Code"
     H*
     H*  07/10/98 C:31  Support: 12441
     H*                 From/to service date issue
     H*                 1. Changed service date logic.
     H*
     H*  03/27/98 C:31  Project: MC145
     H*                 Yr 2000, RISC Development
     H*
     H*  12/11/97 C:53 REQ/PROJ: FTMNT # 9612
     H*                CHG XPLAN LENGTH IN DS SO IT DOES NOT GET
     H*                OVERLAYED WITH BLANKS CAUSING AN ERROR ON TAPE
     H*
     H*  04/02/96 C:20 REQ/PROJ: FL006
     H*                1. MODIFICATION TO USE THE CORRECT KEY IN
     H*                   CHAINING TO THE DETAIL CHARGE AUXILIARY
     H*                   FILE.
     H*
     H*  02/19/96 C:20 REQ/PROJ: FL006
     H*                1. IF THIS IS A STANDALONE WCMC FUIND, E1ENDT
     H*                   WOULD NOT HAVE THE PROVIDER CHECK DATE.  WE
     H*                   HAVE MODIFIED PROGRAM TO USE THE DATE THE
     H*                   EOMB WAS CREATED AND THE PROVIDER PAYMENT
     H*                   DATE.
     H*
     H*  10/13/95 C:32 REQ/PROJ: FL006
     H*                MOVED CHAIN OF FDHCL TO BEGINNING OF MAIN LOOP
     H*
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*  65   -   NO HIT ON FDHCP                                     *
     H* N65   -   HIT ON FDHCP                                        *
     H*  70   -   ACCDT IS ZEROS DO NOT PRINT                         *
     H*  71   -   DTERC IS ZEROS DO NOT PRINT                         *
     H*  72   -   DTEPD IS ZEROS DO NOT PRINT                         *
     H*  73   -   ADMDT IS ZEROS DO NOT PRINT                         *
     H*  74   -   CVFRM IS ZEROS DO NOT PRINT                         *
     H*  75   -   CVTO  IS ZEROS DO NOT PRINT                         *
     H*  76   -   FDOS2 IS ZEROS DO NOT PRINT                         *
     H*  77   -   TDOS2 IS ZEROS DO NOT PRINT                         *
     H*  80   -   NO HIT ON FMTSP - ADD NEW SEQ# RECORD BY CO         *
     H* N80   -   HIT ON FMTSP - UPDATE SEQ# RECORD BY CO             *
     H*  89   -   PHYSICIAN                                           *
     H* N89   -   HOSPITAL                                            *
     H*  90   -   PHYSICIAN (CALLED FOR BY USER)                      *
     H* N90   -   HOSPITAL (CALLED FOR BY USER)                       *
     H*  99   -   NO EOMB FOR FUND WITHIN DATES                       *
     H*******************************************************************
     H*  FILES:                                                         *
     H*  FAEOL KEY LIST: CO/SUB#/FND/FYR/CASE/TRDT/FEIN/MOD/EOB#        *
     H*        OPEN QUERY SELECTION:    QQFRDT GE EOTRDT LE QQTODT      *
     H*                                 EOTAPE EQ 'Y', EASUB# EQ QQSUB# *
     H*                                 IF QQFND NE 000, QQFND EQ EOFND *
     H*******************************************************************
     FFATPPP    O  A F  170        DISK
     FFATPPH    O  A F  208        DISK
     FSMCOL     IF   E           K DISK
     FFMC1L     IF   E           K DISK
     FFMF1L     IF   E           K DISK
     FFDF2L     IF   E           K DISK
     FSMORL     IF   E           K DISK
     F*** BEG CHG *** 06/12/99 ****************************************
     F********FAEOL   IF  E           K        DISK
     FFAEOL     UF   E           K DISK
     F*** END CHG *** 06/12/99 ****************************************
     FFAE1L     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFDCALH    IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFDHCL     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FFMSML     IF   E           K DISK
     FFMDIL     IF   E           K DISK
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOA)
     FFMTSP     UF A E           K DISK

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************
     D EOMB            S              8    DIM(18) CTDATA PERRCD(1)
     D*
     D                 DS
     D  EOBMED                 1      8
     D  EOB                    1      3
     D  MEDPD                  4      8
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D                 DS
     D  XXDT                   1      6  0
     D  XXMM                   1      2  0
     D  XXDD                   3      4  0
     D  XXYY                   5      6  0
     D                 DS
     D  XXCNT                  1      6  0
     D  XXMMDD                 1      4  0
     D  XXYR                   5      6  0
     D                 DS
     D  XXTYPE                 1     20
     D  XTYPE                 15     17
     D  XDISCH                18     19
     D  XPC                   20     20
     D                 DS
     D  XXPLAN                 1     10
     D*** BEG CHG *** 12/11/97 ****************
     D*************                           5   9 XPLAN
     D  XPLAN                  5      6
     D*** END CHG *** 12/11/97 ****************
     D*
     D* Year prior to current date's year; used to calc Julian date
     D                 DS
     D  ZZDT                   1      6  0
     D  ZZMM                   1      2  0
     D  ZZDD                   3      4  0
     D  ZZYY                   5      6  0
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
      *
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
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
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
     C*
     C* Set-up and Initialize
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQSUB#        XXSUB#            5 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFRDT        XQFRDT            8 0
     C                   MOVE      QQTODT        XQTODT            8 0
     C                   Z-ADD     XQFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT            7 0
     C                   Z-ADD     XQTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT            7 0
     C*** BEG CHG *** 12/03/99 ****************************************
     C                   Z-ADD     XXFRDT        SRDTI
     C********             CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C********             MOVELSRDTO     XXFROM  60
     C                   MOVEL     SRDTO         XXFROM            8 0
     C                   Z-ADD     XXTODT        SRDTI
     C********             CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C********             MOVELSRDTO     XXTO    60
     C                   MOVEL     SRDTO         XXTO              8 0
     C*** END CHG *** 12/03/99 ****************************************
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
     C                   MOVE      ZZDT          SRFRDC            6
     C                   MOVE      XXDT          SRTODC            6
     C                   Z-ADD     0             SRDAYS            5 0
     C                   MOVE      ' '           SRERR             1
     C                   MOVE      ' '           SRTERM            1
     C                   CALL      'SRDDIF'
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
     C********           MOVE      '0'           XXPRT
     C********     ORKEY         CHAIN     POR01                              61
     C********     *IN61         IFEQ      '0'
     C********                   EXCEPT    ROUTE
     C********           END
      ***** End Del ***** 08/26/14 *********************************************
     C*
     C                   MOVE      '0'           *IN99
     C*
     C                   READ      PEO01                                  62
     C*
     C* The following is to print heading on report for Resubmissions
     C*
     C     EOCNTL        IFNE      *BLANKS
     C     EOSBDT        ANDEQ     *BLANKS
     C                   Z-ADD     1             HLDFND
     C                   ENDIF
     C*
     C     *IN62         IFEQ      '1'
     C                   EXCEPT    ERRMS2
     C                   GOTO      TERM
     C                   END
     C*
     C     *IN62         DOWEQ     '0'
     C*** BEG ADD *** 08/24/99 ****************************************
     C     CAKEY         CHAIN     PCA01                              59
     C     *IN59         IFEQ      *OFF
     C*** END ADD *** 08/24/99 ****************************************
     C*** BEG ADD *** 09/02/99 ****************************************
     C     EOAST         IFEQ      'FL'
     C*** END ADD *** 09/02/99 ****************************************
     C*
     C     EOSYS         IFEQ      'P'
     C                   MOVE      '1'           *IN89
     C                   ELSE
     C     EOSYS         IFEQ      'H'
     C                   MOVE      '0'           *IN89
     C                   END
     C                   END
     C*
     C     *IN89         IFEQ      *IN90
     C                   MOVE      '1'           *IN99
     C                   END
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
     C* Get the GROUP STATE INFORMATION
     C     E1KEY         CHAIN     PE101                              63
     C*
     C********************  HEADER SECTION  **********************************
     C* Clear Header record Fields
     C*
     C                   EXSR      CLRH
     C*
     C* Move information to Header fields
     C*** BEG CHG *** 06/12/99 ****************************************
     C* CNTL
     C**************       MOVE C1SUBM    CCC              submitter cd
     C**************       MOVE XXYY      YY               year
     C**************       MOVE SRDAYS    JJJ              Julian date
     C**************       ADD  1         CNTR             calc seq #
     C**************       MOVE CNTR      SSSSS            seq #
     C*
     C     EORCN         IFEQ      *ZEROS
     C     EOSYS         ANDEQ     'P'
     C     EOCNTL        ANDEQ     *BLANKS
     C                   MOVE      C1SUBM        CCC                            submitter cd
     C                   MOVE      XXYY          YY                             year
     C                   MOVE      SRDAYS        JJJ                            Julian date
     C                   ADD       1             CNTR                           calc seq #
     C                   MOVE      CNTR          SSSSS                          seq #
     C                   ENDIF
     C*** END CHG *** 06/12/99 ****************************************
     C*** BEG ADD *** 06/12/99 ****************************************
     C     EORCN         IFEQ      *ZEROS
     C     EOCNTL        ANDEQ     *BLANKS
     C*
     C                   MOVEL     CNTL          EOCNTL
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         EOSBDT
     C*
     C                   UPDATE    PEO01
     C*
     C                   ELSE
     C*
     C     EOCNTL        IFNE      *BLANKS
     C     EOSBDT        ANDEQ     *BLANKS
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         EOSBDT
     C                   MOVEL     EOCNTL        CNTL
     C*
     C                   UPDATE    PEO01
     C*
     C                   ELSE
     C*
     C                   MOVE      *OFF          *IN89
     C                   EXCEPT    REO01
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END ADD *** 06/12/99 ****************************************
     C* CARR
     C                   MOVE      F25N01        CARR
     C* CARFIN
     C                   MOVE      F29N01        CARFIN                         CARFEIN
     C* CARZIP
     C                   MOVEL     F29N02        CARZIP
     C* AGNT
     C     F25N02        IFGT      *ZEROS
     C                   MOVE      F25N02        AGNT
     C* AGTFIN
     C                   MOVE      F29N03        AGTFIN                         AGENTFEIN
     C* AGTZIP
     C                   MOVEL     F29N04        AGTZIP                         AGENTZIP
     C                   END
     C* SSN
     C     CMSS#         IFNE      *BLANKS
      ***** Beg Add *** 06/09/20 *******************************************
     C***                MOVE      CMSS#         SSN
      /Free
        SSN = MaskData('SSN':'CMSS#':CMSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C                   END
     C* DOA
     C     CMACDT        IFEQ      0
     C                   MOVEL     '19'          TEMP              4 0
     C                   MOVE      XXYY          TEMP
     C                   MOVEL     TEMP          DOA
     C                   MOVE      '1'           *IN70
     C                   ELSE
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         DOA               8 0
     C*** BEG CHG *** 12/03/99 ****************************************
     C                   Z-ADD     CMACDT        SRDTI
     C********             CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C********             MOVELSRDTO     ACCDT   60
     C                   MOVEL     SRDTO         ACCDT             8 0
     C*** END CHG *** 12/03/99 ****************************************
     C                   END
     C* PHYS
     C     EOFLIC        IFEQ      *BLANKS
     C     PDFLIC        IFNE      *BLANKS
     C                   MOVE      PDFLIC        PHYS
     C                   ELSE                                                   ELSE NE *BLANKS
     C                   MOVEL     'ZZ999'       PHYS
     C                   MOVE      '9999'        PHYS
     C                   END                                                    END NE *BLANKS
     C                   ELSE                                                   ELSE EQ *BLANKS
     C                   MOVE      EOFLIC        PHYS
     C                   END                                                    END EQ *BLANKS
     C* FEIN
     C     EOFEIN        IFNE      0
     C                   MOVE      EOFEIN        FEIN                           PROVFEIN
     C                   END
     C* LNAME
     C                   MOVE      *BLANKS       FLAG
     C                   MOVE      *BLANKS       STRING
     C                   MOVEL     CDLSN         STRING
     C                   MOVEL     CDLSN         PATT1
     C                   CALL      'F9511C1'
     C                   PARM                    STRING           40
     C                   PARM                    PATT1            15
     C                   PARM                    FLAG              1
     C                   MOVEL(P)  STRING        LNAME
     C* FNAME
     C                   MOVE      *BLANKS       FLAG
     C                   MOVE      *BLANKS       STRING
     C                   MOVEL     CDFSN         STRING
     C                   MOVEL     CDFSN         PATT1
     C                   CALL      'F9511C1'
     C                   PARM                    STRING           40
     C                   PARM                    PATT1            15
     C                   PARM                    FLAG              1
     C                   MOVEL(P)  STRING        FNAME
     C* MI
     C                   MOVE      CDMID         MI
     C* DATERC
     C     E1CRDT        IFEQ      *ZERO
     C                   Z-ADD     EOIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         DATERC            8 0
     C*** BEG CHG *** 12/03/99 ****************************************
     C                   Z-ADD     EOIDT         SRDTI
     C********             CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C********             MOVELSRDTO     DTERC   60
     C                   MOVEL     SRDTO         DTERC             8 0
     C*** END CHG *** 12/03/99 ****************************************
     C                   ELSE
     C                   Z-ADD     E1CRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         DATERC            8 0
     C*** BEG CHG *** 12/03/99 ****************************************
     C                   Z-ADD     E1CRDT        SRDTI
     C********             CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C********             MOVELSRDTO     DTERC   60
     C                   MOVEL     SRDTO         DTERC             8 0
     C*** END CHG *** 12/03/99 ****************************************
     C                   END
     C* DATEPD
     C     E1ENDT        IFEQ      *ZERO
     C                   Z-ADD     EOIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         DATEPD            8 0
     C*** BEG CHG *** 12/03/99 ****************************************
     C                   Z-ADD     EOIDT         SRDTI
     C********             CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C********             MOVELSRDTO     DTEPD   60
     C                   MOVEL     SRDTO         DTEPD             8 0
     C*** END CHG *** 12/03/99 ****************************************
     C                   ELSE
     C                   Z-ADD     E1ENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         DATEPD            8 0
     C*** BEG CHG *** 12/03/99 ****************************************
     C                   Z-ADD     E1ENDT        SRDTI
     C********             CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C********             MOVELSRDTO     DTEPD   60
     C                   MOVEL     SRDTO         DTEPD             8 0
     C*** END CHG *** 12/03/99 ****************************************
     C                   END
     C* TTLPD
     C     EOAMT         SUB       EONDIS        TTLPD            11 2          TOTALPAID
     C* TYPE
     C                   MOVEL     E120A1        XXTYPE
     C                   MOVEL     XTYPE         TYPE
     C* DSCGHR
     C                   MOVEL     XDISCH        DSCGHR                         DISCGHR
     C* ADMDTE
     C     E1DT6         IFEQ      *ZERO
     C                   MOVEL     '19'          TEMP
     C                   MOVE      XXYY          TEMP
     C                   MOVEL     TEMP          ADMDTE
     C                   MOVE      '1'           *IN73
     C                   ELSE
     C                   Z-ADD     E1DT6         SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         ADMDTE            8 0
     C*** BEG CHG *** 12/03/99 ****************************************
     C                   Z-ADD     E1DT6         SRDTI
     C********             CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTYYMD
     C********             MOVELSRDTO     ADMDT   60
     C                   MOVEL     SRDTO         ADMDT             8 0
     C*** BEG CHG *** 12/03/99 ****************************************
     C                   END
     C* COVFRM
     C     E1DT7         IFEQ      *ZERO
     C                   MOVEL     '19'          TEMP
     C                   MOVE      XXYY          TEMP
     C                   MOVEL     TEMP          COVFRM
     C                   MOVE      '1'           *IN74
     C                   ELSE
     C                   Z-ADD     E1DT7         SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         COVFRM            8 0
     C*** BEG CHG *** 12/03/99 ****************************************
     C                   Z-ADD     E1DT7         SRDTI
     C********             CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTYYMD
     C********             MOVELSRDTO     CVFRM   60
     C                   MOVEL     SRDTO         CVFRM             8 0
     C*** END CHG *** 12/03/99 ****************************************
     C                   END
     C* COVTO
     C     E1DT8         IFEQ      *ZERO
     C                   MOVEL     '19'          TEMP
     C                   MOVE      XXYY          TEMP
     C                   MOVEL     TEMP          COVTO
     C                   MOVE      '1'           *IN75
     C                   ELSE
     C                   Z-ADD     E1DT8         SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         COVTO             8 0
     C*** BEG CHG *** 12/03/99 ****************************************
     C                   Z-ADD     E1DT8         SRDTI
     C********             CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTYYMD
     C********             MOVELSRDTO     CVTO    60
     C                   MOVEL     SRDTO         CVTO              8 0
     C*** END CHG *** 12/03/99 ****************************************
     C                   END
     C* FACFIN
     C                   MOVE      EOFEIN        FACFIN                         FACFEIN
     C* FACZIP
     C                   MOVE      EOZIP         FACZIP
     C*
     C                   Z-ADD     EOFYR         XXFYR             2 0
     C                   Z-ADD     EOCASE        XXCASE            7 0
     C                   Z-ADD     EOEOB#        XXEOB#            9 0
     C*
     C* Write the Header Record to the Work File and to report
     C*
     C  N90
     CANN89              EXCEPT    HEADR
     C   90
     CAN 89              EXCEPT    HEADER
     C     *INOA         IFEQ      '1'
     C     XXFND         ORNE      HLDFND
     C                   EXCEPT    HEAD
     C                   Z-ADD     XXFND         HLDFND            3 0          TO DETERMINE LVL BRK
     C                   END
     C   90
     CAN 89              EXCEPT    HEAD1
     C  N90
     CANN89              EXCEPT    HEAD1
     C*
     C********************  DETAIL SECTION  **********************************
     C* Get all Detail Charge Records that are associated with the
     C* EOB Master record
     C*
     C     CAKEY         CHAIN     PCA01                              64
     C     *IN64         DOWEQ     '0'
     C*
     C* Clear Detail record Fields
     C*
     C                   EXSR      CLRD
     C*** BEGIN ADD *** 10/13/95 **************************************
     C     HCKEY         CHAIN     PHC01                              65
     C*** END   ADD *** 10/13/95 **************************************
     C*
     C* Move information to Detail fields
     C*
     C*** BEGIN CHG *** 10/13/95 **************************************
     C* LOCATE
     C******************** MOVE HCPOS     LOCATE
     C  N65              MOVE      HCPOS         LOCATE
     C*** END   CHG *** 10/13/95 **************************************
     C* DIAG1
     C                   MOVE      EODIA1        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDS1         DIAG1
     C* DIAG2
     C                   MOVE      EODIA2        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDS1         DIAG2
     C* DIAG3
     C                   MOVE      EODIA3        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDS1         DIAG3
     C* DIAG4
     C                   MOVE      EODIA4        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDS1         DIAG4
     C* PROCCH
     C     CAPMOD        IFNE      *BLANKS
     C                   MOVE      CAPMOD        PROCCH                         PROCCHG
     C                   END
     C*** BEG CHG *** 06/12/99 ****************************************
     C***********          MOVELCACPT4    PROCCH
     C     CABPRC        IFEQ      *BLANKS
     C                   MOVEL     CACPT4        PROCCH
     C                   ELSE
     C*** BEG ADD *** 12/29/99 ****************************************
     C     ' '           SCAN      CABPRC                                 60
     C   60              MOVEL     CACPT4        PROCCH
     C  N60              MOVEL     CABPRC        PROCCH
     C*** END ADD *** 12/29/99 ****************************************
     C*** BEG DEL *** 12/29/99 ****************************************
     C*******              MOVELCABPRC    PROCCH
     C*** END DEL *** 12/29/99 ****************************************
     C                   ENDIF
     C*** END CHG *** 06/12/99 ****************************************
     C* PROCPD
     C     CAPMOD        IFNE      *BLANKS
     C                   MOVE      CAPMOD        PROCPD                         PROCPD
     C                   END
     C                   MOVE      '0'           *IN60
     C                   Z-ADD     1             X                 2 0
     C     *IN60         DOWEQ     '0'
     C     X             IFEQ      18
     C                   MOVE      '1'           *IN60
     C                   END
     C                   MOVE      EOMB(X)       EOBMED
     C                   ADD       1             X
     C     CAEOMB        IFEQ      EOB
     C                   MOVEL     MEDPD         PROCPD
     C                   MOVE      '1'           *IN60
     C                   END
     C                   ENDDO
     C   60              MOVEL     CACPT4        PROCPD
     C*
     C* CHRG
     C                   MOVE      CATOTC        CHRG             11 2
     C* SRVS
     C                   Z-ADD     *ZEROS        XXUNTS            2 0
     C*** For Anesthesia Codes:
     C     CACPT4        IFGE      '00100'
     C     CACPT4        ANDLE     '01999'
     C*** If Service Date is after 06/01/91
     C     CAFDAT        IFGE      910601
     C*** Determine Payee Specialty
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C*** If "RNA" Specialty, use 15    else use 10   as divisor
     C     PDSPCD        IFEQ      'RNA'
     C     CAANMN        DIV       15            XXUNTS
     C                   ELSE                                                   ELS EQ 'RNA'
     C     CAANMN        DIV       10            XXUNTS
     C                   END                                                    END EQ 'RNA'
     C                   END                                                    END 0 *IN60
     C                   ELSE                                                   ELS GE 910601
     C*** If Service Date is before 06/01/91, then just use CAANMN
     C                   Z-ADD     CAANMN        XXUNTS
     C                   END                                                    END GE 910601
     C                   ELSE                                                   ELS EQ 00100/01999
     C*** If not an Anesthesia Code, then just use CAUNTS
     C                   Z-ADD     CAUNTS        XXUNTS
     C                   END                                                    END GE 910601
     C*
     C     XXUNTS        IFEQ      0
     C                   MOVE      '01'          SRVS
     C                   ELSE
     C                   MOVE      XXUNTS        SRVS
     C                   END
     C*
     C* PAID
     C                   MOVE      CATOTP        PAID
     C* FDOS
     C     CAFDAT        IFEQ      0
     C                   MOVEL     '19'          TEMP
     C                   MOVE      XXYY          TEMP
     C                   MOVEL     TEMP          FDOS              8 0
     C                   MOVE      '1'           *IN76
     C                   ELSE
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         FDOS              8 0
     C*** BEG CHG *** 12/03/99 ****************************************
     C                   Z-ADD     CAFDAT        SRDTI
     C********             CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C********             MOVELSRDTO     FDOS2   60
     C                   MOVEL     SRDTO         FDOS2             8 0
     C*** END CHG *** 12/03/99 ****************************************
     C                   END
     C* TDOS
     C*** BEGIN DEL *** 07/10/98 **************************************
     C********   CATDAT    IFEQ 0
     C********             MOVEL'19'      TEMP
     C********             MOVE XXYY      TEMP
     C********             MOVELTEMP      TDOS    80
     C*** END DEL *** 07/10/98 ****************************************
     C*** BEGIN ADD *** 07/10/98 **************************************
     C     CATDAT        IFEQ      0
     C*** END ADD *** 07/10/98 ****************************************
     C*** BEG CHG *** 01/21/00 ****************************************
     C***********          Z-ADD0         TDOS    80
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         TDOS              8 0
     C***********          MOVE '1'       *IN77
     C*** END CHG *** 01/21/00 ****************************************
     C                   ELSE
     C                   Z-ADD     CATDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         TDOS              8 0
     C*** BEG CHG *** 12/03/99 ****************************************
     C                   Z-ADD     CATDAT        SRDTI
     C********             CALL 'SRCVTDAT'DTMDY
     C                   CALL      'SRCVTDAT'    DTMDYY
     C********             MOVELSRDTO     TDOS2   60
     C                   MOVEL     SRDTO         TDOS2             8 0
     C*** END CHG *** 12/03/99 ****************************************
     C                   END
     C* RECON
     C     CACSCD        IFEQ      C1CSC1
     C                   MOVE      'R'           RECON
     C                   ELSE
     C                   MOVE      'F'           RECON
     C                   END
     C* PLAN
     C*** BEGIN CHG *** 10/13/95 **************************************
     C********** HCKEY     CHAINPHC01                60
     C  N65              MOVE      HC10A1        XXPLAN
     C  N65              MOVEL     XPLAN         PLAN
     C*** END   CHG *** 10/13/95 **************************************
     C* REVCDE
     C                   MOVE      CASVCD        REVCDE                         REVCODE
     C* HCPCS
     C     CACPT4        IFNE      *BLANKS
     C                   MOVE      CACPT4        HCPCS
     C                   ELSE
     C                   MOVE      *ZEROS        HCPCS
     C                   END
     C* UNITS
     C                   MOVE      CAUNTS        UNITS
     C* PC
     C                   MOVE      XPC           PC
     C* PRINPR
     C     HC1A1         IFEQ      *BLANKS
     C                   MOVE      '1'           *IN50
     C                   END
     C     HC1A1         IFEQ      '1'
     C                   MOVE      E17A01        XXDIA
     C                   END
     C     HC1A1         IFEQ      '2'
     C                   MOVE      E17A02        XXDIA
     C                   END
     C     HC1A1         IFEQ      '3'
     C                   MOVE      E17A03        XXDIA
     C                   END
     C     HC1A1         IFEQ      '4'
     C                   MOVE      E17A04        XXDIA
     C                   END
     C  N50DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDS1         PRINPR                         PRINPROC
     C   60              MOVE      *ZEROS        PRINPR
     C                   MOVE      '0'           *IN50
     C* OTHRPR
     C                   MOVE      *ZEROS        OTHRPR                         OTHERPROC
     C* CHARGE
     C                   MOVE      CATOTC        CHARGE                         CHRG/REV
     C*
     C* Write Detail record to Work File and report
     C*
     C  N90
     CANN89              EXCEPT    DTL
     C   90
     CAN 89              EXCEPT    DETAIL
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C   90
     CAN 89              EXCEPT    HEAD1
     C  N90
     CANN89              EXCEPT    HEAD1
     C                   MOVE      '0'           *INOA
     C                   END
     C   90
     CAN 89              EXCEPT    DETL
     C  N90
     CANN89              EXCEPT    DETL
     C*
     C* Continue to read  detail charge file
     C*
     C     CAKEY         READE     PCA01                                  64
     C                   END                                                    *in64 end
     C*
     C********************  TOTAL  SECTION  **********************************
     C* Clear TOTAL record Fields
     C*
     C                   EXSR      CLRT
     C* TOTPD
     C     EOAMT         IFNE      *ZEROS
     C                   MOVE      EOAMT         TOTPD
     C                   END                                                    END NE *BLANKS
     C*
     C* Write TOTAL record to Report
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C   90
     CAN 89              EXCEPT    HEAD1
     C  N90
     CANN89              EXCEPT    HEAD1
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C*** BEG ADD *** 09/02/99 ****************************************
     C                   ELSE
     C                   EXCEPT    REO01
     C*
     C                   ENDIF                                                  EOAST='FL'
     C*** END ADD *** 09/02/99 ****************************************
     C*
     C*** BEG ADD *** 08/24/99 ****************************************
     C                   END                                                    *in59 end
     C*** END ADD *** 08/24/99 ****************************************
     C                   READ      PEO01                                  62
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
     C* CLRH - Subroutine to Clear Header Record's Fields            *
     C****************************************************************
     C* FIELD DEFINITION:                                             *
     C* HEADER RECORD:(PHYSICIAN & HOSPITAL)                          *
     C* CNTL     CLAIM CONTROL NUMBER - FORMAT CCCYJJJSSS             *
     C*          CCC  = SUBMTTER/CARRIER CODE                         *
     C*          YY   = LAST DIGIT OF THE YEAR OF SUBMISSION          *
     C*          JJJ  = JULIAN DATE REPRESENTING THE DAY OF SUBMISSION*
     C*          SSSSS= SEQUENCE NUMBER (0001-9999) DESIGNATING THE   *
     C*                 FIRST, SECOND, SUBSEQUENT DOCUMENTS ON THE    *
     C*                 TAPE                                          *
     C* RCFLG1   RECORD FLAG = 1                                      *
     C* FORMID   FORM ID = 09                                         *
     C* CARR     CARRIER INTERNAL AUDIT #                             *
     C* CARFIN   CARRIER FEDERAL ID #                                 *
     C* CARZIP   CARRIER LOCATION ZIP CODE                            *
     C* AGNT     SERVICING AGENT INTERNAL AUDIT #                     *
     C* AGTFIN   SERVICING AGENT FEDERAL ID #                         *
     C* AGTZIP   SERVICING AGENT ZIP CODE                             *
     C* SSN      CLAIMANT SOC SEC #                                   *
     C* DOA      DATE OF ACCIDENT (CCYYMMDD)                          *
     C* PHYS     PROVIDER FLORIDA LICENSE #                           *
     C* FEIN     PROVIDER FEIN                                        *
     C* LNAME    CLAIMANT'S LAST  NAME                                *
     C* FNAME    CLAIMANT'S FIRST NAME                                *
     C* MI       CLAIMANT'S MIDDLE INITIAL                            *
     C* DATERC   DATE CARRIER RECEIVED BILL FROM PROVIDER             *
     C* DATEPD   DATE CARRIER PAID PROVIDER                           *
     C* TTLPD    TOTAL PAID                                           *
     C*                                                               *
     C* HEADER RECORD:(HOSPITAL)                                      *
     C* FORMID   FORM ID = 90                                         *
     C* TYPE     TYPE OF BILL                                         *
     C* ADMDATE  ADMISSION DATE                                       *
     C* DISCGHR  DISCHARGE HOUR                                       *
     C* COVFROM  DATE STATEMENT COVERS FROM                           *
     C* COVTO    DATE STATEMENT COVERS TO                             *
     C* FACFEIN  FACILTIY'S FEDERAL ID NUMBER                         *
     C* FACZIP   FACILITY'S ZIP CODE                                  *
     C****************************************************************
     C     CLRH          BEGSR
     C*
     C                   MOVE      '00000000'    *IN(70)
     C                   MOVE      *BLANKS       CNTL             13
     C                   MOVE      '1'           RCFLG1            1
     C   90              MOVE      '09'          FORMID            2
     C                   MOVE      '00000'       CARR              5
     C                   MOVEL     '00000'       CARFIN            9
     C                   MOVE      '0000'        CARFIN
     C                   MOVEL     '00000'       CARZIP            9
     C                   MOVE      '0000'        CARZIP
     C                   MOVE      '00000'       AGNT              5
     C                   MOVEL     '00000'       AGTFIN            9
     C                   MOVE      '0000'        AGTFIN
     C                   MOVEL     '00000'       AGTZIP            9
     C                   MOVE      '0000'        AGTZIP
     C                   MOVE      *BLANKS       SSN               9
     C                   MOVE      '00000000'    DOA               8 0
     C                   MOVEL     'ZZ99'        PHYS              9
     C                   MOVE      '99999'       PHYS
     C                   MOVEL     '00000'       FEIN              9
     C                   MOVE      '0000'        FEIN
     C                   MOVE      *BLANKS       LNAME            30
     C                   MOVE      *BLANKS       FNAME            15
     C                   MOVE      *BLANKS       MI                1
     C                   MOVE      '00000000'    DATERC
     C                   MOVE      '00000000'    DATEPD
     C                   MOVEL     '000000'      TTLPD
     C                   MOVE      '00000'       TTLPD
     C  N90              MOVE      '90'          FORMID
     C                   MOVE      *BLANKS       TYPE              3
     C                   MOVE      '00000000'    ADMDTE            8 0
     C                   MOVE      '00'          DSCGHR            8
     C                   MOVE      '00000000'    COVFRM            8 0
     C                   MOVE      '00000000'    COVTO             8 0
     C                   MOVE      '00000'       FACFIN            9
     C                   MOVE      '0000'        FACFIN
     C                   MOVE      '00000'       FACZIP            9 0
     C                   MOVE      '0000'        FACZIP
     C*
     C                   ENDSR
     C****************************************************************
     C* CLRD - Subroutine to Clear Detail Record's Fields            *
     C****************************************************************
     C* FIELD DEFINITION:                                             *
     C* DETAIL RECORD: (PHYSICIAN & HOSPITAL)                         *
     C* CNTL     CLAIM CONTROL NUMBER - DUPLICATE OF HEADER           *
     C* RCFLG2   RECORD FLAG = 2                                      *
     C* FORMID   FORM ID = 09                                         *
     C* LOCATE   PLACE OF SERVICE                                     *
     C* DIAG1    ICD-9-CM DIAGNOSTIC CODE                             *
     C* DIAG2    ICD-9-CM DIAGNOSTIC CODE                             *
     C* DIAG3    ICD-9-CM DIAGNOSTIC CODE                             *
     C* DIAG4    ICD-9-CM DIAGNOSTIC CODE                             *
     C* PROCCH   MEDICAL PROCEDURE CODE - AS BILLED                   *
     C* PROCPA   MEDICAL PROCEDURE CODE - AS PAID                     *
     C* CHRG     PROVIDER CHARGE                                      *
     C* SRVS     NUMBER OF SERVICES (UNITS)                           *
     C* PAID     CARRIER PAYMENT                                      *
     C* FDOS     FROM DATE OF SERVICE                                 *
     C* TDOS     TO DATE OF SERVICE                                   *
     C* RECON    RECONSIDERATION FLAG                                 *
     C* PLAN     PAYMENT PLAN TYPE                                    *
     C* FILL67   BLANK OUT TO 170 POSITIONS                           *
     C*                                                               *
     C* DETAIL RECORD: (HOSPITAL)                                     *
     C* FORMID   FORM ID = 90                                         *
     C* PRINDIAG ICD-9-CM DIAGNOSTIC CODE (DIAG1 FROM PHYSICIAN)      *
     C* REVCODE  REVENUE CODE                                         *
     C* HCPCS    HCPCS PROCEDURE CODE                                 *
     C* UNITS    UNITS OF SERVICE                                     *
     C* PC       PROCEDURE CODING METHOD IN 9B/10B                    *
     C* PRINPROC PRINCIPLE PROCEDURE                                  *
     C* OTHERPROC OTHER PROCEDURE                                     *
     C* CHRG/REV CHARGE PER REVENUE CODE                              *
     C* FILL116  BLANK OUT TO 208 POSITIONS                           *
     C****************************************************************
     C     CLRD          BEGSR
     C*
     C                   MOVE      '2'           RCFLG2            1
     C   90              MOVE      '09'          FORMID            2
     C                   MOVE      *BLANKS       LOCATE            2
     C                   MOVE      *ZEROS        DIAG1             6
     C                   MOVE      *ZEROS        DIAG2             6
     C                   MOVE      *ZEROS        DIAG3             6
     C                   MOVE      *ZEROS        DIAG4             6
     C                   MOVE      '0000000'     PROCCH            7
     C                   MOVE      '0000000'     PROCPD            7
     C                   MOVEL     '000000'      CHRG
     C                   MOVE      '00000'       CHRG
     C                   MOVE      '000'         SRVS              3
     C                   MOVEL     '000000'      PAID             11
     C                   MOVE      '00000'       PAID
     C                   MOVE      '00000000'    FDOS
     C                   MOVE      '00000000'    TDOS
     C                   MOVE      *BLANKS       RECON             1
     C                   MOVE      *ZEROS        PLAN              5
     C                   MOVE      *BLANKS       FILL67           67
     C  N90              MOVE      '90'          FORMID
     C                   MOVE      *BLANKS       REVCDE            4
     C                   MOVE      *ZEROS        HCPCS             5
     C                   MOVE      *ZEROS        UNITS             7
     C                   MOVE      *BLANKS       PC                1
     C                   MOVE      *BLANKS       PRINPR            7
     C                   MOVE      *BLANKS       OTHRPR            7
     C                   MOVE      '000000'      CHARGE           11 2
     C                   MOVE      '00000'       CHARGE
     C                   MOVE      *BLANKS       FIL116          116
     C*
     C                   ENDSR
     C****************************************************************
     C* CLRT - Subroutine to Clear TOTAL  Record's Fields            *
     C****************************************************************
     H* FIELD DEFINITION: (PER MARCH 31, 1990 STATE DATA DICTIONARY)  *
     H* TOTAL  RECORD:                                                *
     H* CNTL     CLAIM CONTROL NUMBER - DUPLICATE OF HEADER           *
     H* RCFLG3   RECORD FLAG = 3                                      *
     H* FILL18   BLANK OUT                                            *
     H* TOTPD    TOTAL PAID ON EOMB (MUST EQUAL TOTAL OF PAID FIELDS) *
     H* FILL65   BLANK OUT TO 102 POSTIONS                            *
     C****************************************************************
     C*
     C     CLRT          BEGSR
     C*
     C                   MOVE      '2'           RCFLG3            1
     C                   MOVE      *BLANKS       FILL18           18
     C                   MOVE      '000000'      TOTPD            11
     C                   MOVE      '00000'       TOTPD
     C                   MOVE      *BLANKS       FILL65           65
     C*
     C                   ENDSR
     C****************************************************************
     OPEO01     E            REO01
     OFATPPP    EADD         HEADER
     O                       CNTL                13
     O                       RCFLG1              14
     O                       FORMID              16
     O                       CARR                21
     O                       CARFIN              30
     O                       CARZIP              39
     O                       AGNT                44
     O                       AGTFIN              53
     O                       AGTZIP              62
     O                       SSN                 71
     O                       DOA                 79
     O                       PHYS                88
     O                       FEIN                97
     O                       LNAME              127
     O                       FNAME              142
     O                       MI                 143
     O                       DATERC             151
     O                       DATEPD             159
     O                       TTLPD              170
     O          EADD         DETAIL
     O                       CNTL                13
     O                       RCFLG2              14
     O                       FORMID              16
     O                       LOCATE              18
     O                       DIAG1               24
     O                       DIAG2               30
     O                       DIAG3               36
     O                       DIAG4               42
     O                       PROCCH              49
     O                       PROCPD              56
     O                       CHRG                67
     O                       SRVS                70
     O                       PAID                81
     O                       FDOS                89
     O                       TDOS                97
     O                       RECON               98
     O                       PLAN               103
     O                       FILL67             170
     O*
     OFATPPH    EADD         HEADR
     O                       CNTL                13
     O                       RCFLG1              14
     O                       FORMID              16
     O                       CARR                21
     O                       CARFIN              30
     O                       CARZIP              39
     O                       AGNT                44
     O                       AGTFIN              53
     O                       AGTZIP              62
     O                       TYPE                65
     O                       SSN                 74
     O                       DOA                 82
     O                       PHYS                91
     O                       LNAME              121
     O                       FNAME              136
     O                       MI                 137
     O                       ADMDTE             145
     O                       DSCGHR             147
     O                       COVFRM             155
     O                       COVTO              163
     O                       FACFIN             172
     O                       FACZIP             181
     O                       DATERC             189
     O                       DATEPD             197
     O                       TTLPD              208
     O          EADD         DTL
     O                       CNTL                13
     O                       RCFLG2              14
     O                       FORMID              16
     O                       DIAG1               22
     O                       DIAG2               28
     O                       DIAG3               34
     O                       DIAG4               40
     O                       REVCDE              44
     O                       HCPCS               49
     O                       UNITS               56
     O                       PC                  57
     O                       PRINPR              64
     O                       OTHRPR              71
     O                       CHARGE              82
     O                       PLAN                87
     O                       FIL116             203
     O*
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
     O**********             ORHOLD              61
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
     O                       XXFROM              30 '  /  /    '
     O          E            ROUTE          1
     O                                           18 'TO DATE  :'
     O                       XXTO                30 '  /  /    '
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
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       XXCONM             120
     O                       DSDATE        Y    198
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       ORTITL             129
     O                                          192 'PAGE:'
     O                       PAGE          Z    198
     O          E            HEAD           1
     O                                           88 'FROM DATE:'
     O                       XXFROM              98 '  /  /    '
     O                                          108 'TO DATE:'
     O                       XXTO               118 '  /  /    '
     O          E            HEAD           1
     O                       XXPROC             102
     O          E            HEAD           1
     O                                           11 'SUBSCRIBER:'
     O                       XXSUB#        Z     18
     O                       XXSBNM              60
     O          E            HEAD           2
     O                                           06 'GROUP:'
     O                       XXFND         Z     10
     O                                           12 '-'
     O                       XXFNNM              51
     O                                           59 'CARR'
     O                       CARR                65
     O                                           74 'AGNT'
     O                       AGNT                80
     O          E            HEAD           1
     O               90                          12 'CONTROL #'
     O               90                          28 'SOC SEC #'
     O               90                          41 'ACC DATE'
     O               90                          56 'YR   CASE'
     O               90                          69 'EOMB#'
     O               90                          85 'PHYS LIC#'
     O               90                          99 'PROV FEIN'
     O               90                         116 'CLAIMANT NAME'
     O               90                         164 'DATE REC'
     O               90                         179 'DATE PAID'
     O               90                         196 'TOTAL PAID'
     O              N90                          13 'CONTROL #'
     O              N90                          26 'BILL TYPE'
     O              N90                          37 'SOC SEC #'
     O              N90                          47 'ACC DATE'
     O              N90                          58 'PHYS LIC#'
     O              N90                          73 'CLAIMANT NAME'
     O              N90                         124 'ADMISSION DATE'
     O              N90                         135 'DISCHARGE'
     O              N90                         146 'FROM DATE'
     O              N90                         157 'TO DATE'
     O              N90                         172 'FACILITY ID#'
     O              N90                         187 'FACILITY ZIP'
     O          E            HEAD           2
     O               90                          24 'PLACE OF SERV'
     O               90                          51 'ICD-9-CM DIAG CODES'
     O               90                          72 'MED PROC-BILL'
     O               90                          89 'MED PROC-PAID'
     O               90                         103 'PROV CHRG'
     O               90                         123 'NO OF SERVICES'
     O               90                         138 'PAID AMOUNT'
     O               90                         153 'FR DT SERV'
     O               90                         167 'TO DT SERV'
     O               90                         176 'RECON'
     O               90                         188 'PAY PLAN'
     O              N90                          31 'DATE REC'
     O              N90                          43 'DATE PAID'
     O              N90                          58 'TOTAL PAID'
     O              N90                          81 'ICD-9-CM DIAGNOSTIC'
     O              N90                          87 'CODES'
     O              N90                          97 'REVENUE'
     O              N90                         108 'PROC CODE'
     O              N90                         116 'UNITS'
     O              N90                         121 'PC'
     O              N90                         132 'PRIN PROC'
     O              N90                         144 'OTHER PROC'
     O          E            HEAD1          1
     O               90      CNTL                14
     O               90      SSN                 28
     O               90      ACCDT               41 '  /  /    '
     O               90      XXFYR               49
     O               90                          50 '-'
     O               90      XXCASE              57
     O               90      XXEOB#        Z     71
     O               90      PHYS                85
     O               90      FEIN                99
     O               90      FNAME              118
     O               90      MI                 120
     O               90      LNAME              151
     O               90      DTERC              165 '  /  /    '
     O               90      DTEPD              178 '  /  /    '
     O               90      TTLPD         J    198
     O              N90      CNTL                15
     O              N90      TYPE                23
     O              N90      SSN                 37
     O              N90N70   ACCDT               47 '  /  /    '
     O              N90      PHYS                58
     O              N90      FNAME               75
     O              N90      MI                  77
     O              N90      LNAME              108
     O              N90N73   ADMDT              121 '  /  /    '
     O              N90      DSCGHR             131
     O              N90N74   CVFRM              146 '  /  /    '
     O              N90N75   CVTO               158 '  /  /    '
     O              N90      FACFIN             171
     O              N90      FACZIP             186 '     -    '
     O          E            DETL           1
     O               90      LOCATE              18
     O               90      DIAG1               34
     O               90      DIAG2               41
     O               90      DIAG3               48
     O               90      DIAG4               55
     O               90      PROCCH              69
     O               90      PROCPD              86
     O               90      CHRG          J    105
     O               90      SRVS               117
     O               90      CATOTP        J    139
     O               90N76   FDOS2              152 '  /  /    '
     O               90      TDOS2              166 '  /  /    '
     O               90      RECON              174
     O               90      PLAN               186
     O              N90      DTERC               31 '  /  /    '
     O              N90      DTEPD               43 '  /  /    '
     O**********      N90 72   DTEPD     41
     O              N90      TTLPD         J     59
     O              N90      DIAG1               67
     O              N90      DIAG2               74
     O              N90      DIAG3               81
     O              N90      DIAG4               88
     O              N90      REVCDE              95
     O              N90      HCPCS              106
     O              N90      UNITS              117
     O              N90      PC                 121
     O              N90      PRINPR             132
     O              N90      OTHRPR             142
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
**  TABLE EOMB
22390010
22490020
22590040
22690060
22790620
23597530
24072240
24172255
24272265
24372270
24472285
24572295
24673040
24773085
24873115
24973525
25073580
25173615
