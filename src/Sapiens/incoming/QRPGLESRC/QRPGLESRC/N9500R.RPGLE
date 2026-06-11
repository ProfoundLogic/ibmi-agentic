     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE N9500R - PROCESS DETAIL CHARGES TO CORRECT DISCOUNT PGM
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
     F*** BEGIN CHG *** 02/18/98 **************************************
     F********** NMHSL   IF  E           K        DISK
     F********** NMPHL   IF  E           K        DISK
     FFMS4L2    IF   E           K DISK
     FFMS4L3    IF   E           K DISK
     F                                     RENAME(PS401:PS402)
     F*** END   CHG *** 02/18/98 **************************************
     F*
     F*** BEG CHG *** 06/09/98 ****************************************
     F********** FDCAL7  IF  E           K        DISK
     F*** BEG DEL 06/16/98 ********************************************
     F**********FDCALD  IF  E           K        DISK
     F*** END DEL *** 06/16/98 ****************************************
     F*** END CHG *** 06/09/98 ****************************************
     F*** BEG ADD *** 06/16/98 ****************************************
     FFDCAL7    IF   E           K DISK    USROPN
     FFDCALD    IF   E           K DISK    USROPN
     F                                     RENAME(PCA01:PCA02)
     F*** END ADD *** 06/16/98 ****************************************
     FFTABL     IF   E           K DISK

     D*** BEGIN CHG *** 02/02/98 **************************************
     D**********          QQF        50  8               FROM DATES
     D QQF             S              6    DIM(50)                              FROM DATES
     D*** END   CHG *** 02/02/98 **************************************
     D                 DS
     D  XXPGM                  1      8
     D  XDTYPE                 4      5

     C*
     C*  CASE DETAIL RECORDS
     C*
     C*** BEG DEL *** 06/16/98 ****************************************
     C********** CAKEY     KLIST
     C**********           KFLD           XXCO#
     C**********           KFLD           XXSUB#
     C**********           KFLD           XXFND
     C**********           KFLD           XXFYR
     C**********           KFLD           XXCASE
     C*** BEG CHG *** 06/09/98 ****************************************
     C**********           KFLD           XXYMD            DEL 06/09/98
     C**********           KFLD           XXFEIN           ADD
     C**********           KFLD           XXMOD            ADD
     C**********           KFLD           XXDCS#           ADD 06/09/98
     C*** END CHG *** 06/09/98 ****************************************
     C*** END DEL *** 06/16/98 ****************************************
     C*
     C*
     C*** BEGIN ADD *** 06/16/98 **************************************
     C*
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
     C                   KFLD                    XXDCS#
     C*** END ADD *** 06/16/98 ****************************************
     C*
     C*** BEGIN DEL *** 02/18/98 **************************************
     C*
     C*   KEY LIST FOR THE HOSPITAL PROVIDERS
     C*
     C********** HSKEY     KLIST
     C**********           KFLD           XXFEIN
     C**********           KFLD           XXMOD
     C**********           KFLD           XXNWCD
     C**********           KFLD           CAFDAT
     C*
     C*   KEY LIST FOR THE DOCTOR PROVIDERS
     C*
     C********** PHKEY     KLIST
     C**********           KFLD           XXFEIN
     C**********           KFLD           XXMOD
     C**********           KFLD           XXNWCD
     C**********           KFLD           CAFDAT
     C*** END   DEL *** 02/18/98 **************************************
     C*** BEGIN ADD *** 02/18/98 **************************************
     C*
     C*   KEY LIST FOR PROVIDER NETWORK CONTRACT FILE.
     C*
     C     S4KEY         KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C                   KFLD                    XXNWCD
     C                   KFLD                    CAFDAT
     C*** END   ADD *** 02/18/98 **************************************
     C*
     C*  BILLING CODE TABLE FILE
     C*
     C     BDKEY         KLIST
     C                   KFLD                    XXCODE            3
     C                   KFLD                    CAFDAT
     C*
     C*  CLAIM STATUS CODE TABLE FILE
     C*
     C     TCSKY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    TABKEY           10
     C*
     C     *LIKE         DEFINE    CAFDAT        XXYMD
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFUND            3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM                    QQDCS#            5
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
     C*** BEGIN CHG *** 02/02/98 **************************************
     C                   PARM                    QQFDT           150
     C                   PARM                    QQFDT2          150
     C*** END   CHG *** 02/02/98 **************************************
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
     C*** BEGIN ADD *** 06/16/98 **************************************
     C     QQRTCD        COMP      'N'                                    86
     C   86              OPEN      FDCALD
     C  N86              OPEN      FDCAL7
     C*** END ADD *** 06/16/98 ****************************************
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQSUB#        XXSUB#            5 0
     C                   MOVE      QQFUND        XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQCASE        XXCASE            7 0
     C                   MOVE      QQFEIN        XXFEIN            9 0
     C                   MOVE      QQMOD         XXMOD             3 0
     C                   MOVE      QQDCS#        XXDCS#            5 0
     C                   MOVE      QQNWCD        XXNWCD            5
     C                   MOVE      QQSYS         XXSYS             1
     C                   MOVEA     QQFDT         QQF(1)
     C                   MOVEA     QQFDT2        QQF(26)
     C*** BEGIN ADD *** 06/16/98 **************************************
     C     *IN86         IFEQ      *ON
     C                   CLEAR                   QQF
     C                   MOVE      010101        QQF(1)
     C                   ENDIF                                                  *IN86 = On
     C*** END ADD *** 06/16/98 ****************************************
     C*
     C                   DO        50            S                 3 0
     C*** BEGIN CHG *** 02/02/98 **************************************
     C**********           MOVE QQF,S     XXDATE  80
     C                   MOVE      QQF(S)        XXDATE            6 0
     C*** END   CHG *** 02/02/98 **************************************
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXYMD
     C*
     C     QQF(S)        IFNE      *ZERO
     C*** BEGIN ADD *** 06/16/98 **************************************
     C     QQF(S)        ANDNE     *BLANKS
     C*** END ADD *** 06/16/98 ****************************************
     C*
     C*  GET THE CASE DETAIL RECORDS
     C*
     C*** BEG CHG *** 06/16/98 ****************************************
     C********** CAKEY     CHAINPCA01                61
     C  N86CAKY1         CHAIN     PCA01                              61
     C   86CAKY2         CHAIN     PCA02                              61
     C*** END CHG *** 06/16/98 ****************************************
     C     *IN61         DOWEQ     '0'
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
     C     CADCS#        IFEQ      XXDCS#                                       ADD 09/13/94
     C     CAFEIN        ANDEQ     XXFEIN                                       ADD 09/13/94
     C     CAMOD         ANDEQ     XXMOD                                        ADD 09/13/94
     C*
     C*  IF A HOSPITAL PROVIDER GET THE DISCOUNT CODE FOR THIS EFF DT RANGE
     C*
     C     XXSYS         IFEQ      'H'
     C*** BEGIN CHG *** 02/18/98 **************************************
     C********** HSKEY     SETGTPHS01
     C     S4KEY         SETGT     PS401
     C     NXTHOS        TAG
     C**********           READPPHS01                    60
     C                   READP     PS401                                  60
     C     *IN60         IFEQ      '0'
     C********** HSNWCD    ANDEQXXNWCD
     C********** HSFEIN    ANDEQXXFEIN
     C********** HSFMOD    ANDEQXXMOD
     C     S4NWCD        ANDEQ     XXNWCD
     C     S4FEIN        ANDEQ     XXFEIN
     C     S4MOD         ANDEQ     XXMOD
     C*** BEGIN ADD *** 03/09/98 **************************************
     C     S4ENDT        IFEQ      0
     C                   Z-ADD     99999999      S4ENDT
     C                   ENDIF                                                  ENDT = 0
     C*** END ADD *** 03/09/98 ****************************************
     C*                                                                *********
     C* THE SERVICE DATE MUST BE BETWEEN THE PROVIDERS CONTRACT DATES
     C*
     C********** CAFDAT    IFGT HSTMDT
     C     CAFDAT        IFGT      S4ENDT
     C                   GOTO      NXTHOS
     C                   END
     C*
     C* IF THE SERVICE DATE IS WITHIN THE CONTRACT DATES MOVE DISCOUNT CODE
     C*
     C**********           MOVE HSDTCD    XXCODE
     C                   MOVE      S4DISC        XXCODE
     C*** END   CHG *** 02/18/98 **************************************
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
     C                   MOVEL     'N95XXR'      XXPGM
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
     C*** BEGIN CHG *** 02/18/98 **************************************
     C**********           Z-ADDHSCEDT    SRDTI
     C                   Z-ADD     S4BGDT        SRDTI
     C*** END   CHG *** 02/18/98 **************************************
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
     C*** BEGIN ADD *** 03/11/99 **************************************
     C     CANDIS        IFNE      0
     C     CATOTC        SUB       CATOTD        XXWRK9            9 2
     C                   SUB       CAOTHR        XXWRK9
     C                   SUB       CANDIS        XXWRK9
     C     XXWRK9        IFNE      CATOTP
     C                   Z-ADD     0             CANDIS
     C                   ENDIF                                                  WRK9 <> TOTP
     C                   ENDIF                                                  NDIS <> 0
     C     CANDIS        IFEQ      0
     C*** END ADD *** 03/11/99 ****************************************
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
     C*** BEGIN ADD *** 03/11/99 **************************************
     C                   ENDIF                                                  NDIS = 0
     C*** END ADD *** 03/11/99 ****************************************
     C*
     C                   END                                                    END BDCODE/BDSTAT
     C                   END                                                    END *IN60 EQ '0'
     C*
     C                   ELSE                                                   ELSE *IN60 EQ '0'
     C*
     C                   GOTO      NXTDET
     C*
     C                   END                                                    END *IN60 EQ '0'
     C*
     C                   ELSE                                                   ELSE XXSYS EQ 'H'
     C*
     C     XXSYS         IFEQ      'P'
     C*** BEGIN CHG *** 02/18/98 **************************************
     C********** PHKEY     SETGTPPH01
     C     S4KEY         SETGT     PS402
     C     NXTPHY        TAG
     C**********           READPPPH01                    60
     C                   READP     PS402                                  60
     C     *IN60         IFEQ      '0'
     C********** PHNWCD    ANDEQXXNWCD
     C********** PHFEIN    ANDEQXXFEIN
     C********** PHFMOD    ANDEQXXMOD
     C     S4NWCD        ANDEQ     XXNWCD
     C     S4FEIN        ANDEQ     XXFEIN
     C     S4MOD         ANDEQ     XXMOD
     C*** BEGIN ADD *** 03/09/98 **************************************
     C     S4ENDT        IFEQ      0
     C                   Z-ADD     99999999      S4ENDT
     C                   ENDIF                                                  ENDT = 0
     C*** END ADD *** 03/09/98 ****************************************
     C*                                                                *********
     C* THE SERVICE DATE MUST BE BETWEEN THE PROVIDERS CONTRACT DATES
     C*
     C********** CAFDAT    IFGT PHNXDT
     C     CAFDAT        IFGT      S4ENDT
     C                   GOTO      NXTPHY
     C                   END
     C*
     C* IF THE SERVICE DATE IS WITHIN THE CONTRACT DATES MOVE DISCOUNT CODE
     C*
     C**********           MOVE PHDTCD    XXCODE
     C                   MOVE      S4DISC        XXCODE
     C*** END   CHG *** 02/18/98 **************************************
     C*
     C* GET THE DISCOUNT TYPE FROM THS DISCOUNT CODE TABLE
     C*
     C     BDKEY         SETGT     PBD01
     C     NXTBD1        TAG
     C                   READP     PBD01                                  60
     C*
     C*** VERIFY DISCOUNT CODE AND STATUS
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
     C                   MOVEL     'N95XXR'      XXPGM
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
     C*** BEGIN CHG *** 02/18/98 **************************************
     C**********           Z-ADDPHNEDT    SRDTI
     C                   Z-ADD     S4BGDT        SRDTI
     C*** END   CHG *** 02/18/98 **************************************
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
     C*** BEGIN ADD *** 03/11/99 **************************************
     C     CANDIS        IFNE      0
     C     CATOTC        SUB       CATOTD        XXWRK9            9 2
     C                   SUB       CAOTHR        XXWRK9
     C                   SUB       CANDIS        XXWRK9
     C     XXWRK9        IFNE      CATOTP
     C                   Z-ADD     0             CANDIS
     C                   ENDIF                                                  WRK9 <> TOTP
     C                   ENDIF                                                  NDIS <> 0
     C     CANDIS        IFEQ      0
     C*** END ADD *** 03/11/99 ****************************************
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
     C*** BEGIN ADD *** 03/11/99 **************************************
     C                   ENDIF                                                  NDIS = 0
     C*** END ADD *** 03/11/99 ****************************************
     C*
     C                   END                                                    END BDCODE/BDSTAT
     C                   END                                                    END *IN60 EQ '0'
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
     C                   END                                                    END CADCS# EQ XXDCS#
     C*
     C                   END                                                    END CSOVRD NE 'Y'
     C*
     C                   END                                                    END *IN60 EQ '0'
     C*
     C* READ THE NEXT DETAIL CHARGE RECORD
     C*
     C     NXTDET        TAG
     C*** BEG CHG *** 06/16/98 ****************************************
     C********** CAKEY     READEPCA01                    61
     C  N86CAKY1         READE     PCA01                                  61
     C   86CAKY2         READE     PCA02                                  61
     C*** END CHG *** 06/16/98 ****************************************
     C*
     C                   END                                                     END *IN61 DOWEQ '0'
     C*
     C                   END                                                     END QQF NE ZERO
     C*
     C                   END                                                     END DO 50
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
